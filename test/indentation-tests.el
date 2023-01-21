;;; indentation-tests.el --- Indentation/editing tests for paw64-mode -*- lexical-binding: t -*-

;; Copyright © 2012-2023 Sven Johansson

;; Author: Sven Johansson <johansson.sven@gmail.com>

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

;; Basic sanity tests using ERT

;;; Code:

(require 'paw64-mode)

(defun insert-and-indent (text)
  "Insert TEXT and indent according to mode."
  (interactive)
  (save-excursion
    (insert text))
  (indent-according-to-mode))

(defun current-line= (text)
  "Equality test for current line and TEXT."
  (string= text
           (current-line-contents)))

(defun with-indent (num text)
  "Add indentation of NUM to TEXT."
  (concat (make-string num ?\s) text))

(defun trailing-ws (num text)
  "Add trailing ws of NUM to end of TEXT."
  (concat text (make-string num ?\s)))

(defun current-line-contents ()
  "Get contents of current line."
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun goto-comment-start ()
  "Move cursor to start of next comment."
  (interactive)
  (beginning-of-line)
  (goto-char (- (search-forward ";") 1)))

(ert-deftest paw64--indent-instr--on-first-line ()
  (with-temp-buffer
    (paw64-mode)
    (insert-and-indent "lda #$01")
    (should (string= (with-indent 16 "lda #$01")
                     (current-line-contents)))
    (beginning-of-line)
    (indent-according-to-mode)
    (should (string= (with-indent 16 "lda #$01")
                     (current-line-contents)))))

(ert-deftest paw64--indent-instr--from-bol-on-line-with-instr ()
  (with-temp-buffer
    (paw64-mode)
    (insert-and-indent "lda #$01")
    (beginning-of-line)
    (indent-according-to-mode)
    (should (string= (with-indent 16 "lda #$01")
                     (current-line-contents)))))

(ert-deftest paw64--indent-instr--use-indent-level-from-prev-line ()
  (with-temp-buffer
    (paw64-mode)
    (insert (with-indent 8 "lda #$ff"))
    (newline)
    (insert-and-indent "sta #$0400")
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

;;; indentation-tests.el ends here
