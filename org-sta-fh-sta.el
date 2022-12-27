;;; org-sta-fh-sta.el --- St Andrews student ids and grades -*- lexical-binding: t -*-

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

;; Definitions of St Andrews student identifiers (matirculaion
;; numbers), grades, file structures, and helper functions.

;; The only constraint is that we can differentiate grades from
;; student identifiers. More precisely, if we recognise an identifier,
;; it definitely /isn't/ a grade. It could be the case, however, that
;; an identifier matches the grade regexp.

;;; Code:

;; ---------- Type and sanity check function ----------

;; 'Tric numbers regexp
(rx-define org-sta-fh--matriculation-number
  (= 9 digit))

(defvar org-sta-fh--student-identifier-regexp
  (rx bol org-sta-fh--matriculation-number eol)
  "St Andrews student identifiers.

These are simply matriculation numbers.")

(defvar org-sta-fh--student-identifier-file-name-regexp
  (rx (seq bol
	   org-sta-fh--matriculation-number
	   "." (one-or-more alphanumeric)
	   eol))
  "St Andrews student-identifier-derived file names.

These are identifiers with an arbitrary extension.")

(defvar org-sta-fh--grade-regexp
  (rx (seq bol (one-or-more digit) (opt (seq "." digit)) eol))
  "St Andrews grades regexp.

A grade is a one- or two-digit number with an optional single
decimal place.")

(defun org-sta-fh--valid-student-identifier? (student)
  "Check whether STUDENT has the form of a valid student identifier."
  (string-match-p org-sta-fh--student-identifier-regexp student))

(defun org-sta-fh--valid-student-identifier-file-name? (fn)
  "Check whether FN has the form of a valid student identifier-derived file name."
  (string-match-p org-sta-fh--student-identifier-file-name-regexp fn))

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


;; ---------- Cohort extraction ----------

(defun org-sta-fh--student-identifiers-from-directory (dir)
  "Parse the filenames in DIR that are valid student identifiers."
  (diretory-files dir nil )


  )

(provide 'org-sta-fh-sta)
;;; org-sta-fh-sta.el ends here
