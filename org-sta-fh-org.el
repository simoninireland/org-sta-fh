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

;; ---------- Type and sanity check function ----------
;; (At the moment these are very tied to St Andrews standards.)
;; The only constraint is that we can differentiate grades from
;; student identifiers. More precisely, if we recognise an identifier,
;; it definitely /isn't/ a grade. It could be the case, however, that
;; an identifier matches the grade regexp.

(defvar org-sta-sh--student-identifier-regexp
  (rx (seq bol (= 9 digit) eol))
  "Regexp matching a student identifier.

The St Andrews student identifier is a 9-digit matriculation number,
represented as a string as it can contain leading zeros (for very
long-standing students, anyway).")

(defvar org-sta-sh--grade-regexp
  (rx (seq bol (one-or-more digit (opt (seq "." (one-or-more digit)))) eol))
  "Regexp matching a grade.

The St Andrews grades are simply real numbers.")

(defun org-sta-fh--valid-student-identifier? (student)
  "Check whether STUDENT has the form of a valid student identifier."
  (string-match-p org-sta-sh--student-identifier-regexp student))

(defun org-sta-fh--valid-grade? (grade)
  "Check that GRADE is a valid grade.

Return the grade if it is valid, nil if not. St Andrews grades
are numbers that lie between 0 and 20 inclusive in units of 0.5."
  (let ((g (if (numberp grade)
	       grade
	     (if (string-match-p org-sta-fh--grade-regexp grade)
		 (string-to-number grade)
	       nil))))
    (if (null g)
	nil
      (if (and (>= g 0)				     ; grade range
	       (<= g 20)
	       (= (mod (* g 10) 5) 0)                ; only whole or half-points
	       (= (- g (* 0.5 (round (/ g 0.5))))))  ; only 1dp
	  g))))


;; ---------- Parser helper functions ----------

(defun org-sta-fh--split-headline (h)
  "Split H as a headline.

The headline is split on space and comma boundaries"
  (s-split (rx (one-or-more (or " " ","))) h t))

(defun org-sta-fh--parse-headline (h)
  "Parse H as a headline and check result for validity.

Returns a cons cell containing a list of (valid) students
and a (valid) grade."
  (let* ((es (org-sta-fh--split-headline h)))
    (if (< (length es) 2)
	(error "Need at least one student and one grade in headline")
      (let ((students (butlast es))
	    (grade (car (last es))))
	;; check all student identifiers are valid
	(mapcar (lambda (student)
		  (if (not (org-sta-fh--student-identifier? student))
		      (error (format "Invalid student identifier '%s'" student))))
		students)

	;; check grade is valid
	(let ((g (org-sta-fh--valid-grade? grade)))
	    (if (null g)
		(error (format "Invalid grade '%s'" grade))
	      (cons students g)))))))

(defun org-sta-fh--headline-tagged-for-feedback (headline)
  "Test whether the given HEADLINE is tagged for feedback.

Feedback headlines are tagged 'STUDENT' or 'GROUP' (case-insensitive).
Return the symbol ':student', ':group', or nil."
  (let ((tags (org-element-property :tags headline)))
    (dolist (t tags)
      (let ((tag (s-downcase t)))
	(cond ((s-equals? tag "student")
	       :student)
	      ((s-equals? tag "group")
	       :group)
	      (t
	       nil))))))


;; ---------- Org interface ----------

(defun org-sta-fh--find-headline-at-point (tree)
  "Find the element in TREE corresponding to point.

This assumes that TREE is the parser tree corresponding to
thew current buffer. Return nil if point is not at a headline."
  (let ((place (org-element-property :begin (org-element-at-point))))
    (org-element-map tree '(headline)
      (lambda (e)
	(if (equal (org-element-property :begin e) place)
	    e))
      nil t)))

(defun org-sta-fh--parse-student (tree)
  "Parse a single student's feedback and grade from TREE."
  (let* ((sg (org-sta-fh--parse-headline (car (org-element-property :title tree))))
	 (student (if (> (length (car sg)) 1)
		      (error "Expected only a single student")
		    (caar sg)))
	 (grade (cdr sg)))
    ;; create the feedback struture
    (princ (format "Student %s grade %f" student grade))
    (org-sta-fh--add-student student grade)

    ;; extract the content
    (dolist (e (org-element-contents tree))
      (if (and (eq (org-element-type 'headline))
	       (let ((tag (s-downcase (car-safe (org-element-property :tags tree)))))))))
    ))

(defun org-sta-fh--parse-tree (tree)
  "Construct the feedback defined in TREE."
  (pcase (org-sta-fh--headline-tagged-for-feedback tree)
    (:student (org-sta-fh--parse-student tree))
    (:group (org-sta-fh--parse-group tree))
    (_ (error "Tree not tagged for feedback"))))


;; ---------- Extract feedback records ----------

(defun org-sta-fh--extract-feedback (tree feedback)
  "Extract feedback and grades from an org tree TREE into FEEDBACK."
  (org-element-map tree '(headline)))


(provide 'org-sta-fh-org)
;;; org-sta-fh-org.el ends here
