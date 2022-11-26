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

(defun org-sta-fh--valid-student-identifier? (student)
  "Check whether STUDENT has the form of a valid student identifier."
  (string-match-p org-sta-sh--student-identifier-regexp student))

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


;; ---------- Parser helper functions ----------

(defun org-sta-fh--split-headline (h)
  "Split H as a headline.

The headline is split on space and comma boundaries"
  (s-split (rx (one-or-more (or " " ","))) h t))

(defun org-sta-fh--parse-headline (h)
  "Parse H as a headline and check result for validity.

Returns a list containing a list of (valid) students
and a (valid) grade."
  (let* ((es (org-sta-fh--split-headline (org-element-property :raw-value h))))
    (if (< (length es) 2)
	(error "Need at least one student and one grade in headline")
      (let ((students (butlast es))
	    (grade (car (last es))))
	;; check all student identifiers are valid
	(mapcar (lambda (student)
		  (if (not (org-sta-fh--valid-student-identifier? student))
		      (error (format "Invalid student identifier '%s'" student))))
		students)

	;; check grade is valid
	(let ((g (org-sta-fh--valid-grade? grade)))
	    (if (null g)
		(error (format "Invalid grade '%s'" grade))

	      ;; if everything is in order, return a list of students and their grade
	      (append students (list g))))))))

(defun org-sta-fh--feedback-for-student? (ssg)
  "Test whether the parsed headline SSG refers to a single student."
  (equal (length ssg) 2))


;; ---------- Org interface ----------

(defun org-sta-fh--find-headline-at-point (tree)
  "Find the element in TREE corresponding to point.

This assumes that TREE is the parse tree corresponding to
the current buffer. Return nil if point is not at a headline."
  (let ((place (org-element-property :begin (org-element-at-point))))
    (org-element-map tree '(headline)
      (lambda (e)
	(if (= (org-element-property :begin e) place)
	    e))
      nil t)))

(defun org-sta-fh--export-as-string ()
  "Export the string corresponding to the current sub-tree in the current buffer.

We tweak the export parameters to avoid adding headings such as author
and table of contents, and trim whitespace."
  (save-excursion
    (let* ((org-export-with-title nil)
	   (org-export-with-author nil)
	   (org-export-with-toc nil)
	   (buf (org-ascii-export-as-ascii nil t))
	   (feedback (with-current-buffer buf
		       (s-trim (buffer-string)))))
      (kill-buffer buf)
      feedback)))

(defun org-sta-fh--parse-student (student grade tree)
  "Parse feedback for a single STUDENT and GRADE from TREE.

Because this is feedback for a single student we simply export the
entire sub-tree as feedback, without looking inside for any other
structure."
  (princ (format "Student %s grade %s" student grade))
  (org-sta-fh--add-student student grade)

  ;; extract the content, writing it into the student's feedback buffer
  (let ((feedback (org-sta-fh--export-as-string)))
    (princ feedback)
    (org-sta-fh--add-feedback student feedback)))

(defun org-sta-fh--parse-subtree (tree)
  "Construct the feedback defined in TREE.

Point should be placed at the start of the tree's headline."
  (let ((ssg (org-sta-fh--parse-headline tree)))
    (cond ((org-sta-fh--feedback-for-student? ssg)
	   (org-sta-fh--parse-student (car ssg) (cadr ssg) tree))
	  (t
	   (error "Not a single student's feedback")))))

(defun org-sta-fh--parse-tree (tree)
  "Extract feedback and grades from the org TREE.

Feedback is taken from headlines at one level below that
of TREE, all of which must have correctly-parsing headlines."
  (let ((outermost (org-element-property :level tree)))
    (org-element-map tree '(headline)
      (lambda (e)
	(when (= (org-element-property :level e) (1+ outermost))
	  ;; headline is for feedback, parse the sub-tree
	  (goto-char (org-element-property :begin e))
	  (org-sta-fh--parse-subtree e))))))


(provide 'org-sta-fh-org)
;;; org-sta-fh-org.el ends here
