;;; paw64-mode.el --- Major mode for 64tass assembly -*- lexical-binding: t -*-

;; Copyright © 2012-2023 Sven Johansson

;; Author: Sven Johansson <johansson.sven@gmail.com>
;; URL: https://github.com/svjson/paw64-mode
;; Keywords: tools, languages, 64tass, assembly, major-mode
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.2"))
;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This Major Mode provides syntax highlighting and indentation support
;; for editing 64tass  assembly source files, as well as convenience
;; shortcuts for assembling and launching programs in VICE.

;;; Code:

(require '64tass)
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'subr-x)

(defcustom paw64-indent-level 16
  "Default level of indentation for assembly instructions(opcodes)."
  :type 'integer
  :group 'languages)

(defcustom paw64-comment-indent-level 30
  "Default level of comment/documentation column."
  :type 'integer
  :group 'languages)

(defcustom paw64-insert-basic-header-comment t
  "Insert BASIC source comment before basic header."
  :type 'boolean
  :group 'languages)



(defconst paw64-6502-opcode-list '("adc" "anc" "and" "ane" "arr" "asl" "asr" "bcc" "bcs" "beq" "bit" "bmi" "bne"
                                   "bpl" "brk" "bvc" "bvs" "clc" "cld" "cli" "clv" "cmp" "cpx" "cpy" "dcp" "dec"
                                   "dex" "dey" "eor" "inc" "inx" "iny" "isb" "jam" "jmp" "jsr" "lax" "lda" "lds"
                                   "ldx" "ldy" "lsr" "nop" "ora" "pha" "php" "pla" "plp" "rla" "rol" "ror" "rra"
                                   "rti" "rts" "sax" "sbc" "sbx" "sec" "sed" "sei" "sha" "shs" "shx" "shy" "slo"
                                   "sre" "sta" "stx" "sty" "tax" "tay" "tsx" "txa" "txs" "tya"))

(defconst paw64-6502-opcode-regex (concat "\\<" (regexp-opt paw64-6502-opcode-list) "\\>"))

(defconst paw64-symbol-regex "[a-zA-Z_][a-zA-Z0-9_]+")

(defconst paw64-constant-decl-regex "^" (concat paw64-symbol-regex "\s*="))
(defconst paw64-assembly-address-regex "^\\(\\*=\\)")
(defconst paw64-instruction-regex (concat "[[:blank:]]+" paw64-6502-opcode-regex))
(defconst paw64-banner-label-regex (concat "^" paw64-symbol-regex ":"))



(defconst paw64-font-lock-keywords
  `(;; Preprocessor
    ("\\.[[:alpha:]]+" . font-lock-keyword-face)

    ;; Assembly address
    ("^\\(\\*=\\)\\(\\$[0-9a-fA-F]+\\)"
     (1 font-lock-constant-face)
     (2 font-lock-function-name-face))

    ;; Constant declaration
    (,(concat "^\\(" paw64-symbol-regex "\\)[[:blank:]]*=") 1 font-lock-function-name-face)

    ;; Label declaration
    (,(concat "^\\(" paw64-symbol-regex "\\)\\(:?\\)\\>")
     (1 font-lock-type-face)
     (2 'bold))

    ;; Hi/Lo ref
    (,(concat "\\#[\\>\\<]" paw64-symbol-regex) . font-lock-function-name-face)

    ;; Constant hex value
    ("=" "\\$[0-9a-fA-F]+" nil nil (0 'bold))

    ;; Highlight opcodes with optimized regexp for all valid 6502/6510 opcodes, including illegal/undocumented opcodes
    (,(concat "[[:blank:]]+" paw64-6502-opcode-regex) 0 font-lock-keyword-face)

    ;; Opcode argument value
    (,paw64-6502-opcode-regex "\\#[\\$\\%]\\(?:[0-9a-fA-F]+\\)" nil nil (0 'bold))

    ;; Opcode argument address
    (,paw64-6502-opcode-regex "\\$\\(?:[0-9a-fA-F]+\\)" nil nil (0 'font-lock-variable-name-face))

    ;; Opcode argument label/constant
    (,paw64-instruction-regex ,(concat "[[:blank:]]+\\(" paw64-symbol-regex "\\)") nil nil (0 'font-lock-preprocessor-face))

    ;; Opcode register arg
    (,(concat "\\([[:blank:]]+" paw64-6502-opcode-regex "\\).*\\,[[:space:]]?\\([xy]\\)")
     (2 'bold))))



(defun paw64-resolve-instr-indent ()
  "Resolve indentation level for instruction/opcode column used in file.
This operation attempts to determine indentation by looking at previous lines.
If no previous lines exist or indent level cannot be determined,
fall back to ‘paw64-indent-level’."
  (save-excursion
    (beginning-of-line)
    (let ((col (progn (re-search-backward (concat "[[:blank:]]+\\(" paw64-6502-opcode-regex "\\)") nil t)
                      (match-beginning 1))))
      (if (and (integerp col)
               (not (= col 0)))
          (progn
            (goto-char col)
            (current-column))
        paw64-indent-level))))

(defun paw64-resolve-comment-indent()
  "Resolve indentation level for comment column used in file.
This operation attemts to determine indentation by looking at previous
lines, otherwise use ‘paw64-indent-level’"
  (let ((instr-col (paw64-resolve-instr-indent)))
    (save-excursion
      ;; Find comment start that is right of instruction column
      (let ((col (re-search-backward (concat "^\\(?:[^;]\\{" (int-to-string instr-col) ",\\}\\);") (beginning-of-line) t)))
        (if (integerp col)
            (progn
              ;; Find comment start character column
              (goto-char col)
              (beginning-of-line)
              (forward-char instr-col)
              (goto-char (re-search-forward ";"))
              (backward-char)
              (current-column))
          paw64-comment-indent-level)))))

(defun paw64-previous-statement ()
  (save-excursion
    (if (bobp)
        'nothing
      (progn
        (previous-line)
        (beginning-of-line)
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^\s*$" line)
            (paw64-previous-statement))

           ((string-match paw64-instruction-regex line)
            'instruction)

           ((string-match paw64-assembly-address-regex line)
            'assembly-address)

           ((string-match paw64-banner-label-regex line)
            'banner-label)

           ((string-match paw64-constant-decl-regex line)
            'constant-declaration)))))))

(defun paw64-eat-ws ()
  "Remove all whitespace immediately before and after the current cursor-position."
  (save-restriction
    (save-match-data
      (progn
        (skip-chars-backward "[[:blank:]]*")
        (re-search-forward "[[:blank:]]*")
        (replace-match "" nil nil)))))

(defun paw64-indent ()
  "The ‘indent-line-function’ of paw64-mode.
Delegates to either ‘paw64-indent-line’, ‘paw64-indent-at-cursor’ or
'paw64-indent-by-previous' depending on context."
  (interactive)
  (cond
   ((and (bolp) (eolp))
    (paw64-indent-by-previous))

   ((bolp)
    (paw64-indent-line))

   (t
    (paw64-indent-at-cursor))))

(defun paw64-indent-at-cursor ()
  "Indent the remainder of the line from the current cursor column."
  (let* ((instr-col (paw64-resolve-instr-indent))
         (comment-col (paw64-resolve-comment-indent)))
    (cond
     ((and
       (looking-back "^[[:blank:]]*")
       (eolp))
      (paw64-eat-ws))

     ((and
       (looking-back "^[[:blank:]]*")
       (looking-at (concat "[[:blank:]]*" paw64-6502-opcode-regex)))
      (progn
        (paw64-eat-ws)
        (indent-to (paw64-resolve-instr-indent))))

     ((looking-back (concat "^" paw64-6502-opcode-regex))
      (save-excursion
        (beginning-of-line)
        (indent-to (paw64-resolve-instr-indent))))

     ((looking-back "^\\(?:[[:alnum:]]\\|_\\)+[[:blank:]]*")
      (progn
        (paw64-eat-ws)
        (indent-to (paw64-resolve-instr-indent))))

     ((and (looking-at ";") (= (current-column) comment-col))
      (progn
        (paw64-eat-ws)
        (indent-to (+ (if (bolp) 0 1) (current-column)))))

     ((or (eolp)
          (and (looking-at "[[:blank:]]*;")
               (> (current-column) instr-col)))
      (progn
        (paw64-eat-ws)
        (indent-to comment-col))))))

(defun paw64-indent-by-previous ()
  "Indent line according to previous lines."
  (when (member (paw64-previous-statement)
                '(instruction assembly-address banner-label))
    (indent-to  (paw64-resolve-instr-indent))))

(defun paw64-indent-line ()
  "Indent current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (paw64-eat-ws))
  (paw64-eat-ws)
  (cond

   ((looking-at ";")
    (indent-to (paw64-resolve-comment-indent)))

   ((or (looking-at paw64-6502-opcode-regex)
        (and (bolp) (eolp)))
    (indent-to (paw64-resolve-instr-indent)))))

(defun paw64-post-command-hook ()
  "Post-command hook for immediate formatting."
  (let ((this-cmd (symbol-name this-command)))
    (when (member this-cmd '("self-insert-command"
                             "delete-backward-char"
                             "delete-forward-char"
                             "company-complete-selection"
                             "yank"
                             "kill-region"))
      (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (instr-pos (string-match paw64-6502-opcode-regex line))
             (col (- (point) (line-beginning-position))))
        (when (and instr-pos
                   (< col instr-pos))
          (save-excursion
            (goto-char (+ (line-beginning-position) instr-pos))
            (paw64-indent-at-cursor)))))))

(defun paw64-indent-for-tab (&optional arg)
  "Handler for TAB key. ARG is optional."
  (interactive)
  (if (and (bolp) (eolp))
      (indent-to (paw64-resolve-instr-indent))
    (indent-for-tab-command arg)))



(defun paw64-to-decimal-string (input)
  "Convert an INPUT hex string to decimal string.
A string representation of hexadecimal number will be converted a string
containing its corresponding integer value."
  (let ((hex (string-match "\\$[[:digit:]]+" input))
        (dec (string-match "[[:digit:]]+" input)))
    (if (= dec 0)
        input
      (number-to-string (string-to-number (substring input 1) 16)))))

(defun paw64-insert-basic-header ()
  "Insert a basic program to bootstrap machine language program at cursor."
  (interactive)
  (let* ((input (string-trim (read-string "Enter program start address ($0810): ")))
         (raw-addr (if (string-empty-p input)
                       "$0810"
                     input))
         (prog-addr (paw64-to-decimal-string raw-addr)))
    (beginning-of-line)
    (insert "*=$0801")
    (newline)
    (when paw64-insert-basic-header-comment
      (progn
        (insert (concat "; 10 SYS " prog-addr))
        (newline)))
    (insert ".byte $0c, $08, $0a, $00, $9e, $20")
    (newline)
    (insert (concat ".byte " (let ((nums ()))
                               (dotimes (i (length prog-addr))
                                 (push (concat "$3" (substring prog-addr i (+ i 1))) nums))
                               (dotimes (i (- 7 (length prog-addr)))
                                 (push "$00" nums))
                               (string-join (nreverse nums) ", "))))
    (newline)
    (newline)
    (insert (concat "*=" raw-addr ))
    (newline)
    (paw64-indent-for-tab)))



(defun paw64-list--extract-lines (list-content line-start line-end)
  "Extract lines from 64tass listing in LIST-CONTENT.
Extracts corresponding source lines between LINE-START and LINE-END
from 64tass Assembly listing output."
  (interactive)
  (let ((lines-regex (--> (number-sequence line-start line-end)
                          (cl-map 'list 'int-to-string it)
                          (string-join it "\\|")
                          (concat "^\\(" it "\\)[[:blank:]]"))))
    (with-temp-buffer
      (insert list-content)
      (beginning-of-buffer)
      (re-search-forward lines-regex)
      (beginning-of-line)
      (let* ((pos-start (point))
             (pos-last-line-number (progn
                                     (end-of-buffer)
                                     (re-search-backward lines-regex)
                                     (end-of-line)
                                     (point)))
             (pos-end (progn
                        (if (re-search-forward "\n[[:digit:]]+[[:blank:]]" (point-max) t)
                            (progn
                              (beginning-of-line)
                              (point))
                          (progn
                            pos-last-line-number)))))
        (--> (buffer-substring-no-properties pos-start pos-end)
             (split-string it "\n")
             (seq-filter (lambda (l) (not (string= l ""))) it))))))

(defun paw64-list--parse-line (line)
  "Parse a single LINE of 64tass Assembly listing.
Returns an alist with column names"
  (save-match-data
    (let* ((columns (split-string line "\t"))
           (column-value (lambda (n)
                           (if (string= "" (nth n columns))
                               nil
                             (nth n columns))))
           (address (funcall column-value 1)))
      `((line-num . ,(funcall column-value 0))
        (address . ,(when address (substring address 1)))
        (bin . ,(funcall column-value 2))
        (monitor . ,(funcall column-value 3))
        (source . ,(when (> (length columns) 4)
                     (funcall column-value (- (length columns) 1))))))))

(defun paw64-list--parse-lines (lines)
  "Parse a list of lines in LINES in 64tass Assembly Listing format."
  (--> lines
       (cl-map 'list 'paw64-list--parse-line it)
       (cl-reduce (lambda (result line)
                    (append result (list line)))
                  it
                  :initial-value '())))

(defun paw64-list--stringify-line (line)
  "Construct a source string out of LINE alist."
  (let-alist line
    (cond
     ((or (and .source (not .monitor))
          (and .bin (not .source) (not .monitor)))
      (concat (s-repeat 16 " ")
              ".byte "
              (string-join (cl-map 'list
                                   (lambda (val) (concat "$" val))
                                   (split-string .bin " "))
                           ", ")))
     (.monitor
      (concat (s-repeat 16 " ") .monitor))

     (.source
      .source))))

(defun paw64-list--stringify-lines (lines)
  "Construct source strings out of list of LINES."
  (cl-reduce (lambda (result line)
               (concat result
                       (paw64-list--stringify-line line)
                       "\n"))
             lines
             :initial-value ""))

(defun paw64-unroll-macro ()
  "Unroll a macro or generative statement."
  (interactive)
  (when (region-active-p)
    (let* ((line-start (line-number-at-pos (region-beginning)))
           (line-end (line-number-at-pos (region-end)))
           (tmp-file (64tass-create-temp-file buffer-file-name))
           (unrolled-text (-> tmp-file
                              64tass-export-list
                              f-read-text
                              (paw64-list--extract-lines line-start line-end)
                              (paw64-list--parse-lines)
                              (paw64-list--stringify-lines))))
      (delete-region (region-beginning) (region-end))
      (insert unrolled-text)
      (delete-directory (file-name-directory tmp-file) t))))



(defun paw64-compile-and-run-64tass ()
  "Assemble current buffer using and run the resulting binary in VICE/x64.
This uses ‘64tass-compile’ from `64tass.el' to produce a PRG file named after
the buffer and then launching VICE/x64 with it."
  (interactive)
  (when (= 0 (64tass-compile))
    (call-process "x64" nil 0 nil (64tass-target-name))))



(defvar paw64-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

(defvar paw64-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") '64tass-compile)
    (define-key map (kbd "C-c C-x") 'paw64-compile-and-run-64tass)
    (define-key map (kbd "C-c C-b") 'paw64-insert-basic-header)
    (define-key map (kbd "C-c C-m u") 'paw64-unroll-macro)
    (define-key map (kbd "TAB") 'paw64-indent-for-tab)

    map))

;;;###autoload
(define-derived-mode paw64-mode
  fundamental-mode
  "Paw64"
  "Major mode for 6502/6510 assembly with 64tass and/or paw64."
  (set-syntax-table (make-syntax-table paw64-mode-syntax-table))
  (set (make-local-variable 'font-lock-defaults) '(paw64-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'paw64-indent)

  (add-hook 'post-command-hook #'paw64-post-command-hook nil t)
  (electric-indent-local-mode -1))


(provide 'paw64-mode)
;;; paw64-mode.el ends here
