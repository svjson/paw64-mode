(require 'paw64-mode)

(defun insert-and-indent (text)
  (interactive)
  (save-excursion
    (insert text))
  (indent-according-to-mode))

(defun current-line= (text)
  (string= text
           (current-line-contents)))

(defun with-indent (num text)
  (concat (make-string num ?\s) text))

(defun trailing-ws (num text)
  (concat text (make-string num ?\s)))

(defun current-line-contents ()
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun goto-comment-start ()
  (interactive)
  (beginning-of-line)
  (goto-char (- (search-forward ";") 1)))

(ert-deftest paw64--indent-instr--on-first-line ()
  (with-temp-buffer
    (paw64-mode)
    (indent-and-insert "lda #$01")
    (should (string= (with-indent 16 "lda #$01")
                     (current-line-contents)))
    (beginning-of-line)
    (indent-according-to-mode)
    (should (string= (with-indent 16 "lda #$01")
                     (current-line-contents)))))

(ert-deftest paw64--indent-instr--from-bol-on-line-with-instr ()
  (with-temp-buffer
    (paw64-mode)
    (indent-and-insert "lda #$01")
    (beginning-of-line)
    (indent-according-to-mode)
    (should (string= (with-indent 16 "lda #$01")
                     (current-line-contents)))))

(ert-deftest paw64--indent-instr--use-indent-level-from-prev-line ()
  (with-temp-buffer
    (paw64-mode)
    (insert (with-indent 8 "lda #$ff"))
    (newline)
    (indent-and-insert "sta #$0400")
    (should (current-line= (with-indent 8 "sta #$0400")))))

(ert-deftest paw64--indent-at-comment--on-first-line ()
  (with-temp-buffer
    (paw64-mode)
    (insert ";; This is a comment")
    (beginning-of-line)
    (indent-according-to-mode)
    (should (current-line= (with-indent 30 ";; This is a comment")))))

(ert-deftest paw64--indent-comment--from-bol-on-line-with-comment ()
  (with-temp-buffer
    (paw64-mode)
    (insert (with-indent 30 ";; This is a comment"))
    (beginning-of-line)
    (indent-according-to-mode)
    (should (current-line= (with-indent 30 ";; This is a comment")))))

(ert-deftest paw64--indent-after-instr ()
  (with-temp-buffer
    (paw64-mode)
    (insert (with-indent 16 "lda #$00"))
    (end-of-line)
    (indent-according-to-mode)
    (should (current-line= (trailing-ws 6 (with-indent 16 "lda #$00"))))))

(ert-deftest paw64--indent-comment-after-instr ()
  (with-temp-buffer
    (paw64-mode)
    (insert (with-indent 16 "lda #$00 ;; Load that zero, man!"))
    (beginning-of-line)
    (goto-comment-start)
    (indent-according-to-mode)
    (should (current-line= (concat (with-indent 16 "lda #$00") (with-indent 6 ";; Load that zero, man!"))))
    (indent-according-to-mode)
    (should (current-line= (with-indent 16 "lda #$00 ;; Load that zero, man!")))))

(ert-deftest paw64--indent-at-label ()
  (with-temp-buffer
    (paw64-mode)
    (insert "my_label")
    (beginning-of-line)
    (should (current-line= "my_label"))))

(ert-deftest paw64--indent-after-label--empty-line-after ()
  (with-temp-buffer
    (paw64-mode)
    (insert "my_label")
    (end-of-line)
    (indent-according-to-mode)
    (should (current-line= (trailing-ws 8 "my_label")))))


