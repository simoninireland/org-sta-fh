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
;; The only constraint is that we can differentiate grades from
;; student identifiers. More precisely, if we recognise an identifier,
;; it definitely /isn't/ a grade. It could be the case, however, that
;; an identifier matches the grade regexp.

(defun org-sta-fh--student-identifier? (student)
  "Check whether STUDENT has the form of a student identifier.

The St Andrews student identifier is a 9-digit matriculation number,
represented as a string as it can contain leading zeros (for very
long-standing students, anyway)."
  (string-match-p (rx (seq bol (= 9 digit) eol)) student))

(defun org-sta-fh--valid-grade? (grade)
  "Check that GRADE is a valid grade.

Return the grade if it is valid, nil if not. St Andrews grades
are numbers that lie between 0 and 20 inclusive in units of 0.5."
  (let ((g (if (numberp grade)
	       grade
	     (string-to-number grade))))
    (if (and (>= g 0)				   ; grade range
	     (<= g 20)
	     (= (mod (* g 10) 5) 0)                ; only whole or half-points
	     (= (- g (* 0.5 (round (/ g 0.5))))))  ; only 1dp
	g)))


;; ---------- Extractor helper functions ----------

(defun org-sta-fh--split-headline (h)
  "Split H as a headline.

Returns a cons cell consisting of a list of students and
their grade, which may be nil if no grade is provided.
No checks are made on the validity of the student identifiers
or grades: this is done in `org-sta-fh--parse-headline'.."
  (let* ((es (s-split (rx (one-or-more (or " " ","))) h t))
	 (n (length es)))
    (cond ((= n 0)
	   nil)
	  ((= n 1)                                  ; no students or grades
	   (cons es nil))                           ; one student, no grade
	  ((org-sta-fh--student-identifier? (car (last es)))
	   (cons es nil))                           ; student(s) with no grade
	  (t
	   (cons (butlast es) (car (last es)))))))  ; students(s) with grade

(defun org-sta-fh--parse-headline (h)
  "Parse H as a headline and check result for validity.

Returns a cons cell containing a list of (valid) students
and either a valid grade or nil if none was provided."
  (let* ((sgs (org-sta-fh--split-headline h))
	 (students (car-safe sgs))
	 (grade (cdr-safe sgs)))
    (if (null sgs)
	nil
      (progn
	;; check all student identifiers are valid
	(mapcar #'(lambda (student)
		    (if (not (org-sta-fh--student-identifier? student))
			(error (format "Bad student identifier '%s'" student))))
		students)

	(if (null grade)
	    (cons students nil)
	  ;; convert grade to number and check it;s valid
	  (let ((g (org-sta-fh--valid-grade? grade)))
	    (if (null g)
		(error (format "Invalid grade '%s'" grade))
	      (cons students g))))))))


;; ---------- Extract feedback records ----------

(defun org-sta-fh--extract-feedback (tree feedback)
  "Extract feedback and grades from an org tree TREE into FEEDBACK."
  (org-element-map tree '(headline paragraph)))


(provide 'org-sta-fh-org)
;;; org-sta-fh-org.el ends here
