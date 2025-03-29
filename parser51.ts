/**
 * PARSER 51
 * By OxidaneDev/Ostensibly
 * 
 * Note: some ugly code ahead, goodluck
 * 
 * This file handles "pratt-parsing" for lua 5.1 programs.
 * 
 * HUGE CREDITS TO TYLERLACEBY: https://github.com/tlaceby
 * HUGE CREDITS TO FSTIRLITZ: https://github.com/fstirlitz
 */

import { TokenRecord } from "./enums.ts";

// Lexer

export enum TokenType {
    // Keywords
    And, Break, Do, Else, Elseif, End, False, For, Function, If, In, Local, Nil, Not, Or, Repeat, Return, Then, True, Until, While, Self,

    // Identifiers and literals
    Identifier, Number, StringLit,

    // Operators and punctuation
    ArithmeticOperator, // +, -, *, /, %, ^
    Hash,          // #
    Equal,         // ==
    NotEqual,      // ~=
    Less,          // <
    Greater,       // >
    LessEqual,     // <=
    GreaterEqual,  // >=
    Assign,        // =
    Dot,           // .
    Colon,         // :
    Comma,         // ,
    Semicolon,     // ;
    LeftParen,     // (
    RightParen,    // )
    LeftBrace,     // {
    RightBrace,    // }
    LeftBracket,   // [
    RightBracket,  // ]
    Concat,        // ..
    Varargs,       // ...

    // End of file
    Eof,

    // Error token for invalid inputs
    Error
}

const KEYWORDS: { [key: string]: TokenType } = {
    'and': TokenType.And, 'break': TokenType.Break, 'do': TokenType.Do,
    'else': TokenType.Else, 'elseif': TokenType.Elseif, 'end': TokenType.End,
    'false': TokenType.False, 'for': TokenType.For, 'function': TokenType.Function,
    'if': TokenType.If, 'in': TokenType.In, 'local': TokenType.Local,
    'nil': TokenType.Nil, 'not': TokenType.Not, 'or': TokenType.Or,
    'repeat': TokenType.Repeat, 'return': TokenType.Return, 'then': TokenType.Then,
    'true': TokenType.True, 'until': TokenType.Until, 'while': TokenType.While, 'self': TokenType.Self
};

interface Token {
    Value: string;
    Type: TokenType;
}

// Lexer Helpers

function CreateToken(value: string, type: TokenType): Token {
    return { Value: value, Type: type };
}

function IsAlpha(char: string): boolean {
    return /[a-zA-Z]/.test(char);
}

function IsInteger(char: string): boolean {
    return /[0-9]/.test(char);
}

function IsSkippable(char: string): boolean {
    return char === ' ' || char === '\n' || char === '\t' || char === '\r';
}

function ConsumeNumNotation(src: string[]): string {
    let num = '';
    if (src[0] === '0' && (src[1] === 'x' || src[1] === 'X')) {
        // Hexadecimal
        num += src.shift()! + src.shift()!; // 0x or 0X
        while (src.length > 0 && /[0-9a-fA-F]/.test(src[0])) {
            num += src.shift();
        }
    } else {
        // Decimal, floating-point, or scientific
        if (src[0] === '.') num += src.shift()!;
        while (src.length > 0 && IsInteger(src[0])) {
            num += src.shift();
        }
        if (src[0] === '.') {
            num += src.shift();
            while (src.length > 0 && IsInteger(src[0])) {
                num += src.shift();
            }
        }
        if (src[0] === 'e' || src[0] === 'E') {
            num += src.shift();
            const _src = src
            if (_src[0] === '-' || _src[0] === '+') num += src.shift();
            while (src.length > 0 && IsInteger(src[0])) {
                num += src.shift();
            }
        }
    }
    return num;
}

function Lex(source: string): { tokens: Token[], errors: string[] } {
    const tokens: Token[] = [];
    const errors: string[] = [];
    const src = source.split('');
    let currentLine = 1;

    while (src.length > 0) {
        const char = src[0];

        if (IsSkippable(char)) {
            if (char === '\n') currentLine++;
            src.shift();
            continue;
        }

        if (IsAlpha(char) || char === '_') {
            let ident = src.shift()!;
            while (src.length > 0 && (IsAlpha(src[0]) || IsInteger(src[0]) || src[0] === '_')) {
                ident += src.shift();
            }
            const reserved = KEYWORDS[ident];
            if (reserved !== undefined) {
                tokens.push(CreateToken(ident, reserved));
            } else {
                tokens.push(CreateToken(ident, TokenType.Identifier));
            }
            continue;
        }

        if (/[0-9]/.test(char) || (char === '.' && src[1] && /[0-9]/.test(src[1]))) {
            const num = ConsumeNumNotation(src);
            tokens.push(CreateToken(num, TokenType.Number));
            continue;
        }

        switch (char) {
            case '"':
            case "'": {
                const opener = src.shift()!;
                let str = '';
                while (src.length > 0 && src[0] !== opener) {
                    if (src[0] === '\\') {
                        src.shift();
                        if (src.length > 0) {
                            str += src.shift();
                        }
                    } else {
                        str += src.shift();
                    }
                }
                if (src.length > 0 && src[0] === opener) {
                    src.shift();
                    tokens.push(CreateToken(str, TokenType.StringLit));
                } else {
                    errors.push(`Unclosed string starting with ${opener} at line ${currentLine}`);
                    tokens.push(CreateToken(str, TokenType.Error));
                }
                break;
            }
            case '[':
                if (src[1] === '[') {
                    src.shift();
                    src.shift();
                    let longStr = '';
                    const _src = src
                    while (src.length > 0 && !(_src[0] === ']' && _src[1] === ']')) {
                        if (src[0] === '\n') currentLine++;
                        longStr += src.shift();
                    }
                    const _src2 = src
                    if (_src2.length >= 2 && _src2[0] === ']' && _src2[1] === ']') {
                        src.shift();
                        src.shift();
                        tokens.push(CreateToken(longStr, TokenType.StringLit));
                    } else {
                        errors.push(`Unclosed long bracket string at line ${currentLine}`);
                        tokens.push(CreateToken(longStr, TokenType.Error));
                    }
                } else {
                    tokens.push(CreateToken(src.shift()!, TokenType.LeftBracket));
                }
                break;

            case '-':
                if (src[1] === '-') {
                    src.shift();
                    src.shift();
                    const _src = src
                    if (_src[0] === '[' && _src[1] === '[') {
                        src.shift();
                        src.shift();
                        const _src = src
                        while (src.length > 0 && !(_src[0] === ']' && _src[1] === ']')) {
                            if (src.shift() === '\n') currentLine++;
                        }
                        if (src.length >= 2) {
                            src.shift();
                            src.shift();
                        }
                    } else {
                        while (src.length > 0 && src[0] !== '\n') {
                            src.shift();
                        }
                    }
                } else {
                    tokens.push(CreateToken(src.shift()!, TokenType.ArithmeticOperator));
                }
                break;

            case '+':
            case '*':
            case '/':
            case '%':
            case '^':
                tokens.push(CreateToken(src.shift()!, TokenType.ArithmeticOperator));
                break;

            case '=':
                if (src[1] === '=') {
                    src.shift(); src.shift();
                    tokens.push(CreateToken('==', TokenType.Equal));
                } else {
                    tokens.push(CreateToken(src.shift()!, TokenType.Assign));
                }
                break;

            case '~':
                if (src[1] === '=') {
                    src.shift(); src.shift();
                    tokens.push(CreateToken('~=', TokenType.NotEqual));
                } else {
                    errors.push(`Standalone '~' is invalid in Lua 5.1.5 at line ${currentLine}`);
                    tokens.push(CreateToken(src.shift()!, TokenType.Error));
                }
                break;

            case '<':
                if (src[1] === '=') {
                    src.shift(); src.shift();
                    tokens.push(CreateToken('<=', TokenType.LessEqual));
                } else {
                    tokens.push(CreateToken(src.shift()!, TokenType.Less));
                }
                break;

            case '>':
                if (src[1] === '=') {
                    src.shift(); src.shift();
                    tokens.push(CreateToken('>=', TokenType.GreaterEqual));
                } else {
                    tokens.push(CreateToken(src.shift()!, TokenType.Greater));
                }
                break;

            case '.':
                if (src[1] === '.' && src[2] === '.') {
                    src.shift(); src.shift(); src.shift();
                    tokens.push(CreateToken('...', TokenType.Varargs));
                } else if (src[1] === '.') {
                    src.shift(); src.shift();
                    tokens.push(CreateToken('..', TokenType.Concat));
                } else if (!src[1] || !/[0-9]/.test(src[1])) {
                    tokens.push(CreateToken(src.shift()!, TokenType.Dot));
                }
                break;

            case '#':
                tokens.push(CreateToken(src.shift()!, TokenType.Hash));
                break;

            case ':':
                tokens.push(CreateToken(src.shift()!, TokenType.Colon));
                break;

            case ',':
                tokens.push(CreateToken(src.shift()!, TokenType.Comma));
                break;

            case ';':
                tokens.push(CreateToken(src.shift()!, TokenType.Semicolon));
                break;

            case '(':
                tokens.push(CreateToken(src.shift()!, TokenType.LeftParen));
                break;

            case ')':
                tokens.push(CreateToken(src.shift()!, TokenType.RightParen));
                break;

            case '{':
                tokens.push(CreateToken(src.shift()!, TokenType.LeftBrace));
                break;

            case '}':
                tokens.push(CreateToken(src.shift()!, TokenType.RightBrace));
                break;

            case ']':
                tokens.push(CreateToken(src.shift()!, TokenType.RightBracket));
                break;

            default: {
                const char = src.shift()!;
                errors.push(`Unrecognized character '${char}' at line ${currentLine}`);
                tokens.push(CreateToken(char, TokenType.Error));
                break;
            }
        }
    }

    tokens.push(CreateToken("End of file", TokenType.Eof));
    return { tokens, errors };
}

// AST (based on luaparse.js)

type NodeType =
    | "BreakStatement"
    | "ReturnStatement"
    | "IfStatement"
    | "IfClause"
    | "ElseifClause"
    | "ElseClause"
    | "WhileStatement"
    | "DoStatement"
    | "RepeatStatement"
    | "LocalStatement"
    | "AssignmentStatement"
    | "CallStatement"
    | "FunctionDeclaration"
    | "ForNumericStatement"
    | "ForGenericStatement"
    | "Chunk"
    | "Identifier"
    | "StringLiteral"
    | "NumericLiteral"
    | "BooleanLiteral"
    | "NilLiteral"
    | "VarargLiteral"
    | "TableKey"
    | "TableKeyString"
    | "TableValue"
    | "TableConstructorExpression"
    | "LogicalExpression"
    | "BinaryExpression"
    | "UnaryExpression"
    | "MemberExpression"
    | "IndexExpression"
    | "CallExpression"
    | "TableCallExpression"
    | "StringCallExpression"

interface Stmt {
    kind: NodeType;
}

interface Expr extends Stmt { }

// Statements

interface BreakStatement extends Stmt {
    kind: "BreakStatement";
}

interface ReturnStatement extends Stmt {
    kind: "ReturnStatement";
    arguments: Expr[];
}

interface IfStatement extends Stmt {
    kind: "IfStatement";
    clauses: (IfClause | ElseifClause | ElseClause)[];
}

interface IfClause extends Stmt {
    kind: "IfClause";
    condition: Expr;
    body: Stmt[];
}

interface ElseifClause extends Stmt {
    kind: "ElseifClause";
    condition: Expr;
    body: Stmt[];
}

interface ElseClause extends Stmt {
    kind: "ElseClause";
    body: Stmt[];
}

interface WhileStatement extends Stmt {
    kind: "WhileStatement";
    condition: Expr;
    body: Stmt[];
}

interface DoStatement extends Stmt {
    kind: "DoStatement";
    body: Stmt[];
}

interface RepeatStatement extends Stmt {
    kind: "RepeatStatement";
    condition: Expr;
    body: Stmt[];
}

interface LocalStatement extends Stmt {
    kind: "LocalStatement";
    variables: Identifier[];
    init: Expr[];
}

interface AssignmentStatement extends Stmt {
    kind: "AssignmentStatement";
    variables: Expr[];
    init: Expr[];
}

interface CallStatement extends Stmt {
    kind: "CallStatement";
    expression: Expr;
}

interface FunctionDeclaration extends Stmt {
    kind: "FunctionDeclaration";
    identifier: Identifier | null;
    isLocal: boolean;
    parameters: Identifier[];
    body: Stmt[];
}

interface ForNumericStatement extends Stmt {
    kind: "ForNumericStatement";
    variable: Identifier;
    start: Expr;
    end: Expr;
    step: Expr | null;
    body: Stmt[];
}

interface ForGenericStatement extends Stmt {
    kind: "ForGenericStatement";
    variables: Identifier[];
    iterators: Expr[];
    body: Stmt[];
}

interface Chunk extends Stmt {
    kind: "Chunk";
    body: Stmt[];
}

// Expressions

interface Identifier extends Expr {
    kind: "Identifier";
    name: string;
}

interface StringLiteral extends Expr {
    kind: "StringLiteral";
    value: string;
    raw: string;
}

interface NumericLiteral extends Expr {
    kind: "NumericLiteral";
    value: number;
    raw: string;
}

interface BooleanLiteral extends Expr {
    kind: "BooleanLiteral";
    value: boolean;
    raw: string;
}

interface NilLiteral extends Expr {
    kind: "NilLiteral";
}

interface VarargLiteral extends Expr {
    kind: "VarargLiteral";
}

interface TableKey extends Expr {
    kind: "TableKey";
    key: Expr;
    value: Expr;
}

interface TableKeyString extends Expr {
    kind: "TableKeyString";
    key: Identifier;
    value: Expr;
}

interface TableValue extends Expr {
    kind: "TableValue";
    value: Expr;
}

interface TableConstructorExpression extends Expr {
    kind: "TableConstructorExpression";
    fields: (TableKey | TableKeyString | TableValue)[];
}

interface LogicalExpression extends Expr {
    kind: "LogicalExpression";
    operator: "and" | "or";
    left: Expr;
    right: Expr;
}

interface BinaryExpression extends Expr {
    kind: "BinaryExpression";
    operator: string;
    left: Expr;
    right: Expr;
}

interface UnaryExpression extends Expr {
    kind: "UnaryExpression";
    operator: string;
    argument: Expr;
}

interface MemberExpression extends Expr {
    kind: "MemberExpression";
    indexer: "." | ":";
    identifier: Identifier;
    base: Expr;
}

interface IndexExpression extends Expr {
    kind: "IndexExpression";
    base: Expr;
    index: Expr;
}

interface CallExpression extends Expr {
    kind: "CallExpression";
    base: Expr;
    arguments: Expr[];
}

interface TableCallExpression extends Expr {
    kind: "TableCallExpression";
    base: Expr;
    arguments: TableConstructorExpression;
}

interface StringCallExpression extends Expr {
    kind: "StringCallExpression";
    base: Expr;
    argument: Identifier;
}

type LiteralType = "StringLiteral" | "NumericLiteral" | "BooleanLiteral" | "NilLiteral" | "VarargLiteral";

// AST Helpers

export const ast = {
    breakStatement: (): BreakStatement => ({
        kind: "BreakStatement",
    }),

    returnStatement: (args: Expr[]): ReturnStatement => ({
        kind: "ReturnStatement",
        arguments: args,
    }),

    ifStatement: (clauses: (IfClause | ElseifClause | ElseClause)[]): IfStatement => ({
        kind: "IfStatement",
        clauses,
    }),

    ifClause: (condition: Expr, body: Stmt[]): IfClause => ({
        kind: "IfClause",
        condition,
        body,
    }),

    elseifClause: (condition: Expr, body: Stmt[]): ElseifClause => ({
        kind: "ElseifClause",
        condition,
        body,
    }),

    elseClause: (body: Stmt[]): ElseClause => ({
        kind: "ElseClause",
        body,
    }),

    whileStatement: (condition: Expr, body: Stmt[]): WhileStatement => ({
        kind: "WhileStatement",
        condition,
        body,
    }),

    doStatement: (body: Stmt[]): DoStatement => ({
        kind: "DoStatement",
        body,
    }),

    repeatStatement: (condition: Expr, body: Stmt[]): RepeatStatement => ({
        kind: "RepeatStatement",
        condition,
        body,
    }),

    localStatement: (variables: Identifier[], init: Expr[]): LocalStatement => ({
        kind: "LocalStatement",
        variables,
        init,
    }),

    assignmentStatement: (
        variables: Expr[],
        init: Expr[]
    ): AssignmentStatement => ({
        kind: "AssignmentStatement",
        variables,
        init,
    }),

    callStatement: (
        expression: Expr
    ): CallStatement => ({
        kind: "CallStatement",
        expression,
    }),

    functionStatement: (
        identifier: Identifier | null,
        parameters: Identifier[],
        isLocal: boolean,
        body: Stmt[]
    ): FunctionDeclaration => ({
        kind: "FunctionDeclaration",
        identifier,
        isLocal,
        parameters,
        body,
    }),

    forNumericStatement: (
        variable: Identifier,
        start: Expr,
        end: Expr,
        step: Expr | null,
        body: Stmt[]
    ): ForNumericStatement => ({
        kind: "ForNumericStatement",
        variable,
        start,
        end,
        step,
        body,
    }),

    forGenericStatement: (
        variables: Identifier[],
        iterators: Expr[],
        body: Stmt[]
    ): ForGenericStatement => ({
        kind: "ForGenericStatement",
        variables,
        iterators,
        body,
    }),

    chunk: (body: Stmt[]): Chunk => ({
        kind: "Chunk",
        body,
    }),

    identifier: (name: string): Identifier => ({
        kind: "Identifier",
        name,
    }),

    // deno-lint-ignore no-explicit-any
    literal: (type: LiteralType, value: any, raw: string): StringLiteral | NumericLiteral | BooleanLiteral | NilLiteral | VarargLiteral => {
        const kind = type === "StringLiteral" ? "StringLiteral"
            : type === "NumericLiteral" ? "NumericLiteral"
                : type === "BooleanLiteral" ? "BooleanLiteral"
                    : type === "NilLiteral" ? "NilLiteral"
                        : "VarargLiteral";
        return { kind, value, raw } as StringLiteral | NumericLiteral | BooleanLiteral | NilLiteral | VarargLiteral;
    },

    tableKey: (key: Expr, value: Expr): TableKey => ({
        kind: "TableKey",
        key,
        value,
    }),

    tableKeyString: (key: Identifier, value: Expr): TableKeyString => ({
        kind: "TableKeyString",
        key,
        value,
    }),

    tableValue: (value: Expr): TableValue => ({
        kind: "TableValue",
        value,
    }),

    tableConstructorExpression: (
        fields: (TableKey | TableKeyString | TableValue)[]
    ): TableConstructorExpression => ({
        kind: "TableConstructorExpression",
        fields,
    }),

    binaryExpression: (operator: string, left: Expr, right: Expr): LogicalExpression | BinaryExpression => {
        const kind = operator === "and" || operator === "or" ? "LogicalExpression" : "BinaryExpression";
        return { kind, operator, left, right } as LogicalExpression | BinaryExpression;
    },

    unaryExpression: (operator: string, argument: Expr): UnaryExpression => ({
        kind: "UnaryExpression",
        operator,
        argument,
    }),

    memberExpression: (
        base: Expr,
        indexer: "." | ":",
        identifier: Identifier
    ): MemberExpression => ({
        kind: "MemberExpression",
        indexer,
        identifier,
        base,
    }),

    indexExpression: (base: Expr, index: Expr): IndexExpression => ({
        kind: "IndexExpression",
        base,
        index,
    }),

    callExpression: (base: Expr, args: Expr[]): CallExpression => ({
        kind: "CallExpression",
        base,
        arguments: args,
    }),

    tableCallExpression: (
        base: Expr,
        args: TableConstructorExpression
    ): TableCallExpression => ({
        kind: "TableCallExpression",
        base,
        arguments: args,
    }),

    stringCallExpression: (base: Expr, argument: Identifier): StringCallExpression => ({
        kind: "StringCallExpression",
        base,
        argument,
    }),
};

// Parser (based on tylerlaceby's tutorial)

export class Parser {
    private tokens: Token[];
    private errors: string[];
    private pos: number

    constructor(SourceCode: string) {
        const { tokens, errors } = Lex(SourceCode);
        this.tokens = tokens;
        this.errors = errors;
        this.pos = 0
    }

    // helpers
    private Peek(): Token {
        if (this.pos <= this.tokens.length) return this.tokens[this.pos]
        return { Type: TokenType.Eof, Value: "End of file :)" }
    }

    private Consume(): Token {
        const Tk = this.Peek()
        this.pos++
        return Tk
    }

    private ConsumeIf(Tk: TokenType): boolean {
        if (this.Peek().Type == Tk) {
            this.Consume()
            return true
        }
        return false
    }

    private Expect(t: TokenType): Token {
        const Tk = this.Peek()
        if (Tk.Type == t) {
            return this.Consume()
        } else {
            throw new Error(`Expected token type '${TokenRecord[t]}', got '${TokenRecord[Tk.Type]}'`)
        }
    }

    private IsAtEnd(): boolean {
        return this.pos > this.tokens.length || this.tokens[this.pos].Type === TokenType.Eof
    }

    public parse(): Chunk {
        if (this.errors.length > 0) {
            console.log(this.errors)
            throw "LEXER ERRORS FOUND"
        }
        const statements = []
        while (!this.IsAtEnd()) {
            const stmt = this.ParseStatement()
            statements.push(stmt)
            if (this.Peek().Type === TokenType.Semicolon) {
                this.Consume()
            }
        }
        return ast.chunk(statements)
    }

    // Statements

    private ParseStatement(): Stmt {
        const tk = this.Peek()
        switch (tk.Type) {
            case TokenType.If:
                return this.ParseIfStatement()
            case TokenType.While:
                return this.ParseWhileStatement()
            case TokenType.Repeat:
                return this.ParseRepeatStatement()
            case TokenType.For:
                return this.ParseForStatement()
            case TokenType.Function: {
                const islocal = this.tokens[this.pos - 1] !== undefined && this.tokens[this.pos - 1].Type === TokenType.Local
                return this.ParseFunctionDef(islocal)
            }
            case TokenType.Local:
                return this.ParseLocalStatement()
            case TokenType.Return:
                return this.ParseReturnStatement()
            case TokenType.Do:
                return this.ParseDoStatement()
            case TokenType.Break:
                this.Expect(TokenType.Break)
                return ast.breakStatement() as Stmt
            default: {
                const exp = this.ParsePrefixExp()
                const nextToken = this.Peek()
                if (nextToken.Type == TokenType.Assign || nextToken.Type == TokenType.Comma) {
                    const varlist = [exp]
                    while (this.Peek().Type == TokenType.Comma) {
                        this.Consume()
                        const variable = this.ParsePrefixExp()
                        if (!(variable.kind == "Identifier" || variable.kind == "IndexExpression")) {
                            throw new Error("Expected variable")
                        }
                        varlist.push(variable)
                    }
                    this.Expect(TokenType.Assign)
                    const explist = this.ParseExpList()
                    return ast.assignmentStatement(
                        varlist, explist
                    )
                } else {
                    if (exp.kind != "CallExpression" && exp.kind != "TableCallExpression") {
                        throw new Error("Expected function call")
                    }
                    return ast.callStatement(exp)
                }
            }
        }
    }

    private ParseFunctionDef(isLocal: boolean = false): Stmt {
        if (!isLocal) {
            this.Expect(TokenType.Function);
        }
        let name: Identifier | null = null;
        if (this.Peek().Type === TokenType.Identifier) {
            name = ast.identifier(this.Consume().Value);
        }
        const params = this.ParseParamList();
        const body = this.ParseBlock([TokenType.End]) as Chunk;
        this.Expect(TokenType.End);
        return ast.functionStatement(name, params as Identifier[], isLocal, body.body);
    }

    private ParseParamList(): Expr[] {
        this.Expect(TokenType.LeftParen);
        const params: Expr[] = [];
        if (this.Peek().Type !== TokenType.RightParen) {
            do {
                if (this.Peek().Type === TokenType.Identifier) {
                    const ident = this.Consume();
                    params.push(ast.identifier(ident.Value));
                } else if (this.Peek().Type === TokenType.Varargs) {
                    this.Consume();
                    params.push(ast.literal("VarargLiteral", "...", "..."));
                    break;
                } else {
                    throw new Error("Expected identifier or '...' in parameter list");
                }
            } while (this.ConsumeIf(TokenType.Comma));
        }
        this.Expect(TokenType.RightParen);
        return params;
    }

    private ParseLocalStatement(): Stmt {
        this.Expect(TokenType.Local);

        if (this.Peek().Type === TokenType.Function) {
            this.Consume()
            return this.ParseFunctionDef(true);
        } else {
            const varlist = [];
            do {
                const ident = this.Expect(TokenType.Identifier);
                varlist.push(ast.identifier(ident.Value));
            } while (this.ConsumeIf(TokenType.Comma));

            let explist: Expr[] = [];
            if (this.ConsumeIf(TokenType.Assign)) {
                explist = this.ParseExpList();
            }

            return ast.localStatement(varlist, explist);
        }
    }

    private ParseReturnStatement(): Stmt {
        this.Expect(TokenType.Return);
        const explist = this.ParseExpListUntilTerminator();
        return ast.returnStatement(explist);
    }

    private ParseExpListUntilTerminator(): Expr[] {
        const explist = [];
        while (this.Peek().Type !== TokenType.Semicolon &&
            this.Peek().Type !== TokenType.End &&
            !this.IsAtEnd()) {
            explist.push(this.ParseExp());
            if (!this.ConsumeIf(TokenType.Comma)) {
                break;
            }
        }
        return explist;
    }

    private ParseBlock(terminators: TokenType[]): Chunk {
        const statements: Stmt[] = [];
        while (!this.IsAtEnd() && !terminators.includes(this.Peek().Type)) {
            const stmt = this.ParseStatement();
            statements.push(stmt);
            if (this.Peek().Type === TokenType.Semicolon) {
                this.Consume();
            }
        }
        return ast.chunk(statements);
    }

    private ParseIfStatement(): Stmt {
        const clauses: (IfClause | ElseifClause | ElseClause)[] = []
        this.Expect(TokenType.If)
        const condition = this.ParseExp()
        this.Expect(TokenType.Then)
        const thenBlock = this.ParseBlock([TokenType.Elseif, TokenType.Else, TokenType.End])
        clauses.push(ast.ifClause(condition, (thenBlock as Chunk).body))
        while (this.Peek().Type == TokenType.Elseif) {
            this.Consume()
            const elseIfCondition = this.ParseExp()
            this.Expect(TokenType.Then)
            const elseIfBlock = this.ParseBlock([TokenType.Elseif, TokenType.Else, TokenType.End])
            clauses.push(ast.elseifClause(elseIfCondition, (elseIfBlock as Chunk).body))
        }
        if (this.Peek().Type == TokenType.Else) {
            this.Consume()
            const elseBlock = (this.ParseBlock([TokenType.End])) as Chunk
            clauses.push(ast.elseClause(elseBlock.body))
        }
        this.Expect(TokenType.End)
        return ast.ifStatement(clauses)
    }

    private ParseWhileStatement(): Stmt {
        this.Expect(TokenType.While)
        const condition = this.ParseExp()
        this.Expect(TokenType.Do)
        const whileBlock = this.ParseBlock([TokenType.End]) as Chunk
        this.Expect(TokenType.End)
        return ast.whileStatement(condition, whileBlock.body)
    }

    private ParseRepeatStatement(): Stmt {
        this.Expect(TokenType.Repeat)
        const repeatBlock = this.ParseBlock([TokenType.Until]) as Chunk
        this.Expect(TokenType.Until)
        const repeatCondition = this.ParseExp()
        return ast.repeatStatement(repeatCondition, repeatBlock.body)
    }

    private ParseForStatement(): Stmt {
        this.Expect(TokenType.For);

        const varlist = [];
        do {
            const _ident = this.Expect(TokenType.Identifier);
            varlist.push(ast.identifier(_ident.Value));
        } while (this.ConsumeIf(TokenType.Comma));

        const nextToken = this.Peek();
        if (nextToken.Type === TokenType.Assign && varlist.length === 1) {
            this.Consume();
            const start_value = this.ParseExp();
            this.Expect(TokenType.Comma);
            const end_value = this.ParseExp();
            let step_value = null;
            if (this.ConsumeIf(TokenType.Comma)) {
                step_value = this.ParseExp();
            }
            this.Expect(TokenType.Do);
            const body = this.ParseBlock([TokenType.End]) as Chunk;
            this.Expect(TokenType.End);
            return ast.forNumericStatement(varlist[0], start_value, end_value, step_value, body.body);
        } else if (nextToken.Type === TokenType.In) {
            this.Consume();
            const explist = this.ParseExpList();
            this.Expect(TokenType.Do);
            const body = this.ParseBlock([TokenType.End]) as Chunk;
            this.Expect(TokenType.End);
            return ast.forGenericStatement(varlist, explist, body.body);
        } else {
            throw new Error("Expected '=' or 'in' after for statement");
        }
    }

    private ParseDoStatement(): Stmt {
        this.Expect(TokenType.Do)
        const body = this.ParseBlock([TokenType.End]) as Chunk
        this.Expect(TokenType.End)
        return ast.doStatement(body.body)
    }

    // Expressions

    private ParsePrefixExp(): Expr {
        let exp = this.ParseBaseExp();
        while (true) {
            const token = this.Peek();
            if (token.Type === TokenType.LeftBracket) {
                this.Consume();
                const key = this.ParseExp();
                this.Expect(TokenType.RightBracket);
                exp = ast.indexExpression(exp, key);
            } else if (token.Type === TokenType.Dot) {
                this.Consume();
                const name = this.Expect(TokenType.Identifier).Value;
                exp = ast.memberExpression(exp, ".", ast.identifier(name));
            } else if (token.Type === TokenType.LeftParen) {
                this.Consume();
                const args: Expr[] = [];
                if (this.Peek().Type !== TokenType.RightParen) {
                    do {
                        args.push(this.ParseExp());
                    } while (this.ConsumeIf(TokenType.Comma));
                }
                this.Expect(TokenType.RightParen);
                exp = ast.callExpression(exp, args);
            } else if (token.Type === TokenType.Colon) {
                this.Consume();
                const name = this.Expect(TokenType.Identifier).Value;
                const method = ast.memberExpression(exp, ":", ast.identifier(name));
                this.Expect(TokenType.LeftParen);
                const args: Expr[] = [];
                if (this.Peek().Type !== TokenType.RightParen) {
                    do {
                        args.push(this.ParseExp());
                    } while (this.ConsumeIf(TokenType.Comma));
                }
                this.Expect(TokenType.RightParen);
                exp = ast.callExpression(method, args);
            } else {
                break;
            }
        }
        return exp;
    }

    private ParseBaseExp(): Expr {
        const token = this.Peek();
        if (token.Type === TokenType.Identifier) {
            const name = this.Consume().Value;
            return ast.identifier(name);
        } else if (token.Type === TokenType.LeftParen) {
            this.Consume();
            const exp = this.ParseExp();
            this.Expect(TokenType.RightParen);
            return exp;
        } else if (token.Type === TokenType.Function) {
            this.Consume();
            const params = this.ParseParamList();
            const body = this.ParseBlock([TokenType.End]) as Chunk;
            this.Expect(TokenType.End);
            return ast.functionStatement(null, params as Identifier[], false, body.body);
        } else if (token.Type === TokenType.Number) {
            const value = this.Consume().Value;
            return ast.literal("NumericLiteral", parseFloat(value), value);
        } else if (token.Type === TokenType.StringLit) {
            const value = this.Consume().Value;
            return ast.literal("StringLiteral", value, value);
        } else if (token.Type === TokenType.True) {
            this.Consume();
            return ast.literal("BooleanLiteral", true, "true");
        } else if (token.Type === TokenType.False) {
            this.Consume();
            return ast.literal("BooleanLiteral", false, "false");
        } else if (token.Type === TokenType.Nil) {
            this.Consume();
            return ast.literal("NilLiteral", null, "nil");
        } else if (token.Type === TokenType.Varargs) {
            this.Consume();
            return ast.literal("VarargLiteral", "...", "...");
        } else if (token.Type === TokenType.LeftBrace) {
            return this.ParseTableConstructor();
        } else {
            throw new Error(`Unexpected token: ${token.Value}`);
        }
    }

    private ParseArgs(): Expr[] {
        const token = this.Peek()
        if (token.Type == TokenType.LeftParen) {
            this.Consume()
            const args = this.ParseExpList()
            this.Expect(TokenType.RightParen)
            return args
        } else if (token.Type == TokenType.LeftBrace) {
            return (this.ParseTableConstructor() as TableConstructorExpression).fields
        } else if (token.Type == TokenType.StringLit) {
            const value = this.Consume().Value
            return [ast.literal("StringLiteral", value, value)]
        } else {
            throw new Error("Expected '(', '{', or string.\n")
        }
    }

    private ParseTableConstructor(): Expr {
        this.Expect(TokenType.LeftBrace);
        const fields: (TableKey | TableKeyString | TableValue)[] = [];

        while (this.Peek().Type !== TokenType.RightBrace) {
            if (this.Peek().Type === TokenType.LeftBracket) {
                this.Consume();
                const key = this.ParseExp();
                this.Expect(TokenType.RightBracket);
                this.Expect(TokenType.Assign);
                const value = this.ParseExp();
                fields.push(ast.tableKey(key, value));
            } else if (
                this.Peek().Type === TokenType.Identifier &&
                this.tokens[this.pos + 1].Type === TokenType.Assign
            ) {
                const ident = this.Consume();
                const key = ast.identifier(ident.Value);
                this.Consume();
                const value = this.ParseExp();
                fields.push(ast.tableKeyString(key, value));
            } else {
                const value = this.ParseExp();
                fields.push(ast.tableValue(value));
            }

            if (!this.ConsumeIf(TokenType.Comma) && !this.ConsumeIf(TokenType.Semicolon)) {
                break;
            }
        }

        this.Expect(TokenType.RightBrace);
        return ast.tableConstructorExpression(fields);
    }

    private ParseExpList(): Expr[] {
        const explist = [];
        if (this.Peek().Type !== TokenType.Do) {
            explist.push(this.ParseExp());
            while (this.ConsumeIf(TokenType.Comma)) {
                explist.push(this.ParseExp());
            }
        }
        return explist;
    }

    // op precedence
    /**
     * or: 1
     * and: 2
     * concat: 3
     * binop: 4
     * additive: 5
     * multiplicative: 6
     * unop: 7
     * power: 8
     */

    private ParseExp(): Expr {
        return this.ParseOrExp()
    }

    private ParseOrExp(): Expr {
        let exp = this.ParseAndExp()
        while (this.Peek().Type == TokenType.Or) {
            this.Consume()
            const right = this.ParseAndExp()
            exp = ast.binaryExpression("or", exp, right)
        }
        return exp
    }

    private ParseAndExp(): Expr {
        let exp = this.ParseConcatExp()
        while (this.Peek().Type == TokenType.And) {
            this.Consume()
            const right = this.ParseConcatExp()
            exp = ast.binaryExpression("and", exp, right)
        }
        return exp
    }

    private ParseConcatExp(): Expr {
        let exp = this.ParseRelExp();
        while (this.Peek().Type === TokenType.Concat) {
            this.Consume();
            const right = this.ParseRelExp();
            exp = ast.binaryExpression("..", exp, right);
        }
        return exp;
    }

    private ParseRelExp(): Expr {
        let exp = this.ParseAdditiveExp()
        while (
            this.Peek().Type == TokenType.Less
            || this.Peek().Type == TokenType.Greater
            || this.Peek().Type == TokenType.GreaterEqual
            || this.Peek().Type == TokenType.LessEqual
            || this.Peek().Type == TokenType.Equal
            || this.Peek().Type == TokenType.NotEqual
        ) {
            const op = this.Consume()
            const right = this.ParseAdditiveExp()
            exp = ast.binaryExpression(op.Value, exp, right)
        }
        return exp
    }

    private ParseAdditiveExp(): Expr {
        let exp = this.ParseMultiplicativeExp()
        while (
            this.Peek().Type == TokenType.ArithmeticOperator && (
                this.Peek().Value == "+" || this.Peek().Value == "-"
            )
        ) {
            const op = this.Consume()
            const right = this.ParseMultiplicativeExp()
            exp = ast.binaryExpression(op.Value, exp, right)
        }
        return exp
    }

    private ParseMultiplicativeExp(): Expr {
        let exp = this.ParseUnopExp()
        while (
            this.Peek().Type == TokenType.ArithmeticOperator && (
                this.Peek().Value == "*" || this.Peek().Value == "/" || this.Peek().Value == "%"
            )
        ) {
            const op = this.Consume()
            const right = this.ParseUnopExp()
            exp = ast.binaryExpression(op.Value, exp, right)
        }
        return exp
    }

    // tbh, i hate this part.
    private ParseUnopExp(): Expr {
        const token = this.Peek();
        if (
            token.Type === TokenType.Hash ||
            token.Type === TokenType.Not ||
            (token.Type === TokenType.ArithmeticOperator && token.Value === "-")
        ) {
            const op = this.Consume();
            const operand = this.ParseUnopExp();
            return ast.unaryExpression(op.Value, operand);
        }
        return this.ParsePowExp();
    }

    private ParsePowExp(): Expr {
        let exp = this.ParsePrefixExp();
        while (this.Peek().Type === TokenType.ArithmeticOperator && this.Peek().Value === "^") {
            const op = this.Consume();
            const right = this.ParsePowExp();
            exp = ast.binaryExpression(op.Value, exp, right);
        }
        return exp;
    }
}

/**
 * Todo:
 *  Finish parser class
 *  Full scale tests.
 */