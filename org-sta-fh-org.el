;;; org-sta-fh-org.el --- org mode interface -*- lexical-binding: t -*-

;; Copyright (c) 2022 Simon Dobson <simoninireland@gmail.com>

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Version: 0.1.1
;; Homepage: https://github.com/simoninireland/org-sta-fh
;; Package-Requires: ((emacs "27.2") (org "8.0"))

;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface to org mode.

;;; Code:

;; ---------- TYpe and sanity check function ----------
;; (At the moment these are very tied to St Andrews standards.)

(defun org-sta-fh--student-identifier? (student)
  "Check whether STUDENT has the form of a student identifier.

The St Andrews student identifier is a 9-digit matriculation number,
represented as a string as it can contain leading zeros (for very
long-standing students, anyway)."
  (string-match-p (rx (seq bol (= 9 digit) eol)) student))

(defun org-sta-fh--grade? (grade)
  "Check that GRADE is a valid student grade.

St Andrews grades lie between 0 and 20 inclusive in units of
0.5."
  (and (>= grade 0)                                   ; grade range
       (<= grade 20)
       (= (mod (* grade 10) 5) 0)                     ; only whole or half-points
       (= (- grade (* 0.5 (round (/ grade 0.5)))))))  ; only 1dp


;; ---------- Extractor helper functions ----------




;; ---------- Extract feedback records ----------

(defun org-sta-fh--extract-feedback (tree feedback)
  "Extract feedback and grades from an org tree TREE into FEEDBACK."
  (org-element-map tree '(headline paragraph)))




(save-excursion
  (set-buffer "test-feedback.org")
  (goto-char (point-min))
  (let ((tree (org-element-parse-buffer)))
    tree))


(provide 'org-sta-fh-org)
;;; org-sta-fh-org.el ends here
