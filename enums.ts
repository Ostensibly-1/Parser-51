const TokenRecord = [
    "and",
    "break",
    "do",
    "else",
    "elseif",
    "end",
    "false",
    "for",
    "function",
    "if",
    "in",
    "local",
    "nil",
    "not",
    "or",
    "repeat",
    "return",
    "then",
    "true",
    "until",
    "while",

    // Identifiers and literals
    "<identifier>",
    "<number>",
    "<string>",

    // Operators and punctuation
    "<arithmetic_op>",
    "#",
    "==",
    "~=",
    "<",
    ">",
    "<=",
    ">=",
    "=",
    ".",
    ":",
    ",",
    ";",
    "(",
    ")",
    "{",
    "}",
    "[",
    "]",
    "..",
    "...",

    // End of file
    "<eof>",

    // Error token for invalid inputs
    "<error>"
];

export {
    TokenRecord
}