# Bracketfuck
This is a standard-issue Brainfuck interpreter, with the only modification being to replace the non-bracket characters of standard brainfuck with brackets. This interpreter therefore makes the following switches:
- `+` is spelt `(`
- `-` is spelt `)`
- `.` is spelt `{`
- `,` is spelt `}`


This gives us the following instruction set:
| Instruction | Description |
| -------- | ------- |
| `>` | Move the pointer to the right. |
| `<` | Move the pointer to the left. |
| `(` | Increment memory cell at the pointer. |
| `)` | Decrement memory cell at the pointer. |
| `{` | Output the character signified by the cell at the pointer. |
| `}` | Input a character and store it in the cell at the pointer. |
| `[` | Jump past the matching `]` if the cell at the pointer is 0. |
| `]` | Jump back to the matching `]` if the cell at the pointer is non-zero. |

A full description of Brainfuck can be found [here](https://esolangs.org/wiki/Brainfuck).