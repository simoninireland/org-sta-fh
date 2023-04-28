;;; org-sta-fh-ids.el --- student identifiers and grades -*- lexical-binding: t -*-

;; Copyright (c) 2022--2023 Simon Dobson <simoninireland@gmail.com>

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

;; Functions to find and identify sturdent identifiers and grades.
;;
;; At the moment these match the St Andrews standards.
;;
;; The only constraint is that we can differentiate grades from
;; student identifiers. More precisely, if we recognise an identifier,
;; it definitely /isn't/ a grade. It could be the case, however, that
;; an identifier matches the grade regexp.

;;; Code:

;; ---------- Regexps ----------

(defvar org-sta-fh--student-identifier-regexp
  (rx (seq bol (= 9 digit) eol))
  "Regexp matching a student identifier.

The St Andrews student identifier is a 9-digit matriculation number,
represented as a string as it can contain leading zeros (for very
long-standing students, anyway).")

(defvar org-sta-fh--grade-regexp
  (rx (seq bol (one-or-more digit) (opt (seq "." digit)) eol))
  "Regexp matching a grade.

The St Andrews grades are simply real numbers with at most one decimal place.")


;; ---------- Validity checkers ----------

(defun org-sta-fh--valid-student-identifier? (student)
  "Check whether STUDENT has the form of a valid student identifier."
  (string-match-p org-sta-fh--student-identifier-regexp student))

(defun org-sta-fh--valid-grade? (grade)
  "Check that GRADE is a valid grade.

Return the grade (as a string) if it is valid, nil if not. St Andrews grades
are numbers that lie between 0 and 20 inclusive in units of 0.5."
  (let ((g (if (numberp grade)
	       grade
	     (if (string-match-p org-sta-fh--grade-regexp grade)
		 (string-to-number grade)
	       nil))))
    (cond ((null g)
	   nil)
	  ((and (>= g 0)			      ; grade range
		(<= g 20)
		(= (mod (* g 10) 5) 0)                ; only whole or half-points
		(= (- g (* 0.5 (round (/ g 0.5))))))  ; only 1dp
	   (number-to-string g)))))


;; ---------- Directory parser----------

(defun org-sta-fh--filter-submissions (ss)
  "Filter the list of possible submissions in SS.

The elements of SSS will generally be file or directory names.
They are retained if their stem names (without extensions)
are valid student identifiers according to
`org-sta-fh--valid-student-identifier?'."
  (-filter (lambda (s)
	     (org-sta-fh--valid-student-identifier? (f-base s)))
	   (-map #'f-base ss)))

(defun org-sta-fh--submissions-in-dir (dir)
  "Return a list of all student identifiers in DIR.

The entries in the tree are taken as all files or directories whose
stem names (without extensions) are valid student identifiers
according to `org-sta-fh--valid-student-identifier?'.

Sub-directories are not traversed recursively."
  (org-sta-fh--filter-submissions (directory-files dir)))


(provide 'org-sta-fh-ids)
;;; org-sta-fh-ids.el ends here
