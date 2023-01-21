
# Paw64 Mode

`paw64-mode` is the beginnings of an Emacs major mode that provides font-lock(syntax highlighting), column-oriented indentation and shortcuts for assembling and running programs in an emulator.

-----------

This mode is in an early stage of development, so fasten your seatbelt if you're taking it for a ride.

## Commands

| Binding   | Command                              | Description                                                                                                     |
| --------- | ------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| C-c C-b   | Insert Basic Header                  | Insert minimal basic program at $0801 to start program with SYS.                                                |
| C-c C-c   | Assemble current buffer using 64tass | Assemble current buffer (ie, myprogram.asm) to disk (ie, myprogram.prg) in the same folder as the source buffer |
| C-C C-x   | Assemble(64tass) and run (x64)       | Assemble current buffer and launch resulting binary in VICE(x64)                                                |
| C-C C-m u | Untroll/Expand macro                 | Expand macro or generative directive (such as .rept, .for or range())                                             |

## Indentation

Indentation is context sensitive, and geared towards working in three columns: label, assembly instruction, comment.
Indentation level for assembly instructions and comments are derived from indentations used in the buffer, and falls back to `paw64-indent-level` and `paw64-comment-indent-level` if buffer resolution fails.

## Other recommended packages

### company-64tass
[company-64tass](https://github.com/svjson/company-64tass) provides label and variable auto-completion using [company-mode](https://github.com/company-mode/company-mode)

### flycheck-64tass
[flycheck-64tass](https://github.com/svjson/flycheck-64tass) provides a checker for highlighting syntax and compile/assembly errors using [flycheck](https://github.com/flycheck/flycheck)

## Dependencies
### 64tass.el
[64tass.el](https://github.com/svjson/64tass.el) is a small library for interaction between emacs and 64tass

### 64tass
[64tass](https://github.com/irmen/64tass) is a fast multi-pass assembler that uses a superset of the C64 Turbo Assembler syntax.

### VICE
[VICE - The Versatile Commodore Emulator](https://vice-emu.sourceforge.io/) is an emulator for Commodore machines, available for virtually all platforms.
