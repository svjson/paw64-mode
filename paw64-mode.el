
(defconst paw64-font-lock-keywords

  (list

   ;; Preprocessor
   '("\\.[[:alpha:]]+" . font-lock-keyword-face)

   ;; Comments
   '("\\;.*" . font-lock-comment-face)

   ;; Assembly address
   '("^\\(\\*=\\)\\(\\$[0-9a-fA-F]+\\)"
     (1 font-lock-constant-face)
     (2 font-lock-function-name-face))

   ;; Constant declaration
   '("^\\(?:[[:alnum:]]\\|_\\)+" . font-lock-function-name-face)

   ;; Hi/Lo ref
   '("\\#[\\>\\<][a-zA-Z_][a-zA-Z0-9_]*" . font-lock-function-name-face)

   ;; Constant hex value
   '("=" "\\$[0-9a-fA-F]+" nil nil (0 'bold))

   ;; Opcode argument value
   '("[[:blank:]]+[a-z]\\{3\\}\\>" "\\#[\\$\\%]\\(?:[0-9a-fA-F]+\\)" nil nil (0 'bold))

   ;; Opcode argument address
   '("[[:blank:]]+[a-z]\\{3\\}\\>" "\\$\\(?:[0-9a-fA-F]+\\)" nil nil (0 'font-lock-variable-name-face))

   ;; Opcode argument label/constant
   '("[[:blank:]]+[a-z]\\{3\\}\\>" "[[:blank:]]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" nil nil (0 'font-lock-preprocessor-face))

   ;; Opcode register arg
   '("\\([a-z]\\{3\\}[[:space:]]\\).*\\,[[:space:]]?\\([xy]\\)"
     (2 'font-lock-builtin-face))

   ;; Highlight opcodes with optimized regexp for all valid 6502/6510 opcodes, including illegal/undocumented opcodes
   '("\\<\\(a\\(?:dc\\|n[cde]\\|rr\\|s[lr]\\)\\|b\\(?:c[cs]\\|eq\\|it\\|mi\\|ne\\|pl\\|rk\\|v[cs]\\)\\|c\\(?:l[cdiv]\\|mp\\|p[xy]\\)\\|d\\(?:cp\\|e[cxy]\\)\\|eor\\|i\\(?:n[cxy]\\|sb\\)\\|j\\(?:am\\|mp\\|sr\\)\\|l\\(?:ax\\|d[asxy]\\|sr\\)\\|nop\\|ora\\|p\\(?:h[ap]\\|l[ap]\\)\\|r\\(?:la\\|o[lr]\\|ra\\|t[is]\\)\\|s\\(?:ax\\|b[cx]\\|e[cdi]\\|h[asxy]\\|lo\\|re\\|t[axy]\\)\\|t\\(?:a[xy]\\|sx\\|x[as]\\|ya\\)\\)\\>" . font-lock-keyword-face)))

(define-derived-mode paw64-mode fundamental-mode
  "Paw64"
  (set (make-local-variable 'font-lock-defaults) '(paw64-font-lock-keywords)))

(provide 'paw64-mode)
