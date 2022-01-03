
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


(defcustom paw64-indent-level 16
  "Default level of indentation for assembly instructions(opcodes)"
  :type 'integer)

(defcustom paw64-comment-indent-level 30
  "Default level of comment/documentation column"
  :type 'integer)

(defconst paw64-6502-opcode-regex "\\<\\(a\\(?:dc\\|n[cde]\\|rr\\|s[lr]\\)\\|b\\(?:c[cs]\\|eq\\|it\\|mi\\|ne\\|pl\\|rk\\|v[cs]\\)\\|c\\(?:l[cdiv]\\|mp\\|p[xy]\\)\\|d\\(?:cp\\|e[cxy]\\)\\|eor\\|i\\(?:n[cxy]\\|sb\\)\\|j\\(?:am\\|mp\\|sr\\)\\|l\\(?:ax\\|d[asxy]\\|sr\\)\\|nop\\|ora\\|p\\(?:h[ap]\\|l[ap]\\)\\|r\\(?:la\\|o[lr]\\|ra\\|t[is]\\)\\|s\\(?:ax\\|b[cx]\\|e[cdi]\\|h[asxy]\\|lo\\|re\\|t[axy]\\)\\|t\\(?:a[xy]\\|sx\\|x[as]\\|ya\\)\\)\\>")

(defun paw64-resolve-instr-indent ()
  "Resolve indentation level for instruction/opcode column by looking at previous lines, otherwise use ‘paw64-indent-level’"
    (let ((col (re-search-backward paw64-6502-opcode-regex)))
      (if (integerp col)
          (save-excursion
            (goto-char col)
            (current-column))
        paw64-indent-level)))

(defun paw64-resolve-comment-indent()
  "Resolve indentation level for comment column by looking at previous lines, otherwise use ‘paw64-indent-level’"
  (let ((col (re-search-backward ";")))
      (if (integerp col)
          (save-excursion
            (goto-char col)
            (current-column))
        paw64-comment-indent-level)))

(defun paw64-indent ()
  "The ‘indent-line-function’ of paw64-mode. Delegates to either ‘paw64-indent-line’ or ‘paw64-indent-at-cursor’ depending on context."
  (interactive)
  (if (and (not (bolp))
           (looking-at ";")
           (and (= (current-column) paw64-comment-indent-level)))
      (progn
        (paw64-eat-ws)
        (if (not (bolp))
            (indent-to (+ 1 (current-column)))))
    (if (or (bolp)
            (looking-back "\n[[:blank:]]+"))
        (paw64-indent-line)
      (paw64-indent-at-cursor))))

(defun paw64-eat-ws ()
  "Removes all whitespace immediately before and after the current cursor-position"
  (save-restriction
    (save-match-data
      (progn
        (skip-chars-backward "[[:blank:]]*")
        (re-search-forward "[[:blank:]]*")
        (replace-match "" nil nil)))))

(defun paw64-indent-at-cursor ()
  "Indents the remainder of the line from the current cursor column"
  (if (looking-at "[[:blank:]]*;")
      (progn
        (paw64-eat-ws)
        (indent-to (paw64-resolve-comment-indent)))))

(defun paw64-indent-line ()
  "Indent current line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (paw64-eat-ws))
  (paw64-eat-ws)
  (if (looking-at ";")
      (indent-to (paw64-resolve-comment-indent)))
  (if (looking-at paw64-6502-opcode-regex)
      (indent-to (paw64-resolve-instr-indent))))

(define-derived-mode paw64-mode fundamental-mode
  "Paw64"
  (set (make-local-variable 'font-lock-defaults) '(paw64-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'paw64-indent))

(provide 'paw64-mode)
