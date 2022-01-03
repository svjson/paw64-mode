
# Paw64 Mode

`paw64-mode` is the beginnings of an Emacs major mode that provides font-lock(syntax highlighting), column-oriented indentation. 

-----------

This mode is in early stages of development.

## Commands

| Binding | Command                              | Description                                                                                                     |
| ------- | ------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| C-c C-b | Insert Basic Header                  | Insert minimal basic program at $0801 to start program with SYS.                                                |
| C-c C-c | Assemble current buffer using 64tass | Assemble current buffer (ie, myprogram.asm) to disk (ie, myprogram.prg) in the same folder as the source buffer |
| C-C C-x | Assemble(64tass) and run (x64)       | Assemble current buffer and launch resulting binary in VICE(x64)                                                |

## Indentation

Indentation is context sensitive, and geared towards working in three columns: label, assembly instruction, comment.
Indentation level for assembly instructions and comments are derived from indentations used in the buffer, and falls back to `paw64-indent-level` and `paw64-comment-indent-level` if buffer resolution fails.


## Dependencies
### 64tass 
[64tass](https://github.com/irmen/64tass) is a fast multi-pass assembler that uses a superset of the C64 Turbo Assembler syntax.

### Paw64
**Paw64** is a currently unreleased toolkit that features a 64tass-compatible assembler/disassembler, Commodore 64 and 6510 emulation, unit test framework and debugger. Paw64 is not required for paw64-mode.

### VICE
[VICE - The Versatile Commodore Emulator](https://vice-emu.sourceforge.io/) is an emulator for Commodore machines, available for virtually all platforms.



