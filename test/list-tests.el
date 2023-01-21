;;; list-tests.el --- List/source extraction test suite -*- lexical-binding: t -*-

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

(defvar list-content--test-data1
  "27	.0841	8d fb 07	sta $07fb	                sta $07fb
28	.0844	a9 80		lda #$80	                lda #$80
29	.0846	8d fc 07	sta $07fc	                sta $07fc
30	.0849	a9 84		lda #$84	                lda #$84
31	.084b	8d fd 07	sta $07fd	                sta $07fd
34	.084e	a9 80		lda #$80	                lda #$80 + i*$10
35	.0850	8d 00 d0	sta $d000	                sta $d000 + i*2
")

(defvar list-content--test-data2
  "133	.0927					sprite_y_step:
134	>0927	00 08 10 18 20 28 30 38		                .byte $00 + range(0, $60, $08)
	>092f	40 48 50 58
136	.0933					sin_table_y:
")

(defvar list-content--test-data2-lines '("133	.0927					sprite_y_step:"
                                         "134	>0927	00 08 10 18 20 28 30 38		                .byte $00 + range(0, $60, $08)"
                                         "	>092f	40 48 50 58"
                                         "136	.0933					sin_table_y:"))

(defvar list-content--line-4 "4	>0807	32 30 38 30 00 00 00		.byte $32, $30, $38, $30, $00, $00, $00")
(defvar list-content--line-4-parsed `((line-num . ,"4")
                                      (address . ,"0807")
                                      (bin . ,"32 30 38 30 00 00 00")
                                      (monitor)
                                      (source . ,".byte $32, $30, $38, $30, $00, $00, $00")))

(defvar list-content--line-29 "29	.0846	8d fc 07	sta $07fc	                sta $07fc")
(defvar list-content--line-29-parsed `((line-num . ,"29")
                                       (address . ,"0846")
                                       (bin . ,"8d fc 07")
                                       (monitor . ,"sta $07fc")
                                       (source . ,"                sta $07fc")))

(defvar list-content--line-133 "133	.0927					sprite_y_step:")
(defvar list-content--line-133-parsed `((line-num . ,"133")
                                        (address . ,"0927")
                                        (bin . ,nil)
                                        (monitor . ,nil)
                                        (source . ,"sprite_y_step:")))

(defvar list-content--line-134-ext "	>092f	40 48 50 58")
(defvar list-content--line-134-ext-parsed `((line-num . nil)
                                            (addr . "092f")
                                            (bin . "40 48 50 58")
                                            (monitor . nil)
                                            (source . nil)))


(ert-deftest paw64-list--extract-lines--instruction--single-line ()
  (should (equal (paw64-list--extract-lines list-content--test-data1 29 29)
                 '("29	.0846	8d fc 07	sta $07fc	                sta $07fc"))))

(ert-deftest paw64-list--extract-lines--generated-bytes--single-source-line ()
  (should (equal (paw64-list--extract-lines list-content--test-data2 134 134)
                 '("134	>0927	00 08 10 18 20 28 30 38		                .byte $00 + range(0, $60, $08)"
                   "\t>092f	40 48 50 58"))))

(ert-deftest paw64-list--extract-lines--range ()
  (should (equal (paw64-list--extract-lines list-content--test-data1 30 31)
                 '("30	.0849	a9 84		lda #$84	                lda #$84"
                   "31	.084b	8d fd 07	sta $07fd	                sta $07fd"))))

(ert-deftest paw64-list--extract-lines--range--last-line ()
  (should (equal (paw64-list--extract-lines list-content--test-data1 31 35)
                 '("31	.084b	8d fd 07	sta $07fd	                sta $07fd"
                   "34	.084e	a9 80		lda #$80	                lda #$80 + i*$10"
                   "35	.0850	8d 00 d0	sta $d000	                sta $d000 + i*2"))))

(ert-deftest paw64-list--parse-line--processor-instruction ()
  (should (equal (paw64-list--parse-line list-content--line-29)
                 list-content--line-29-parsed)))

(ert-deftest paw64-list--parse-line--byte-directive ()
  (should (equal (paw64-list--parse-line list-content--line-4)
                 list-content--line-4-parsed)))

(ert-deftest paw64-list--parse-line--label-only ()
  (should (equal (paw64-list--parse-line list-content--line-133)
                 list-content--line-133-parsed)))

(ert-deftest paw64-list--parse-lines--generated-and-labels ()
  (should (equal (paw64-list--parse-lines list-content--test-data2-lines)
                 `(((line-num . "133")
                    (address . "0927")
                    (bin . nil)
                    (monitor . nil)
                    (source . "sprite_y_step:"))

                   ((line-num . "134")
                    (address . "0927")
                    (bin . "00 08 10 18 20 28 30 38")
                    (monitor . nil)
                    (source . "                .byte $00 + range(0, $60, $08)"))

                   ((line-num . nil)
                    (address . "092f")
                    (bin . "40 48 50 58")
                    (monitor . nil)
                    (source . nil))

                   ((line-num . "136")
                    (address . "0933")
                    (bin . nil)
                    (monitor . nil)
                    (source . "sin_table_y:"))))))


(ert-deftest paw64-list--stringify-line--processor-instruction ()
  (should (equal (paw64-list--stringify-line list-content--line-29-parsed)
                 "                sta $07fc")))

(ert-deftest paw64-list--stringify-line--byte-directive ()
  (should (equal (paw64-list--stringify-line list-content--line-4-parsed)
                 "                .byte $32, $30, $38, $30, $00, $00, $00")))

(ert-deftest paw64-list--stringify-line--generated-bytes-only ()
  (should (equal (paw64-list--stringify-line list-content--line-134-ext-parsed)
                 "                .byte $40, $48, $50, $58")))

;;; list-tests.el ends here
