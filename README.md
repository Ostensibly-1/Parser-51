# Parser 51
A Lua 5.1.x Parser made in TypeScript
## Usage:
```typescript
import { Parser } from ".parser51.ts";

const src = await Deno.readTextFile("./script-to-parse.lua")
const parser = new Parser(src)
const parsed = parser.parse()
console.log(parsed)
Deno.writeTextFile("./ast.json", JSON.stringify(parsed, null, 4))
```
