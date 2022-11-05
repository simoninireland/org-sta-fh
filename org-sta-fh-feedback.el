;;; org-sta-fh-feedback.el --- feedback data structure -*- lexical-binding: t -*-

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

;; A feedback data structure.
;;
;; We store feedback in a hash table for speed compared to alists.
;; The hash table is keyed by student identifier; each value is a
;; cons cell consisting of the feedback and the grade.

;;; Code:

;; ---------- Construction ----------

(defun org-sta-fh--make-feedback ()
  "Make a feedback structure."
  (make-hash-table :size 100))

(defun org-sta-fh--feedback? (feedback)
  "Test FEEDBACK is a feedback structure.

At the moment this just tests it's a hash table."
  (hash-table-p feedback))


;; ---------- Formatting utilities ----------

(defun org-sta-fh--ensure-trailing-blank-line (s)
  "Ensure S ends with a trailing blank line."
  (concat (s-trim-right s) "\n\n"))


;; ---------- Adding and retrieving feedback and grades ----------

(defun org-sta-fh--set-feedback (student text feedback)
  "Set TEXT as the feedback for STUDENT in FEEDBACK.

If there is already feedback for this student, TEXT is appended to it."
  (let ((fb (gethash student feedback)))
    (if fb
	;; existing feedback or grade, append to it
	(let* ((old-text (car fb))
	       (grade (cdr fb))
	       (new-text (if (null old-text)
			     text
			   (org-sta-fh--ensure-trailing-blank-line (concat old-text text)))))
	  (puthash student (cons new-text grade) feedback))

      ;; no existing feedback, save this as the text with a nil grade
      (puthash student (cons (org-sta-fh--ensure-trailing-blank-line text) nil) feedback))))

(defun org-sta-fh--set-grade (student grade feedback)
  "Set GRADE as the grade for STUDENT in FEEDBACK.

If there is already feedback for this student, it is preserved."
  (let ((fb (gethash student feedback)))
    (if fb
	;;, existing feedback or grade, replace it
	(let* ((old-text (car fb)))
	  (puthash student (cons old-text grade) feedback))

      ;; no existing feedback, save this as the grade with empty feedback
      (puthash student (cons nil grade) feedback))))

(defun org-sta-fh--get-feedback (student feedback)
  "Return the feedback for STUDENT in FEEDBACK.

The feedback may be nil if none has been set.Any trailing
whitespace is trimmer"
  (let ((fb (gethash student feedback)))
    (if (null fb)
	nil
      (if (null (car fb))
	  nil
	(s-trim-right (car fb))))))

(defun org-sta-fh--get-grade (student feedback)
  "Return the grade for STUDENT in FEEDBACK.

The grade may be nil if none has been set."
  (let ((fb (gethash student feedback)))
    (if (null fb)
	nil
      (cdr fb))))


;; ---------- Consistency checks ----------

(defun org-sta-fh--students-without-feedback (feedback)
  "Returns a list of students without feedback in FEEDBACK."
  (let ((without ()))
    (maphash #'(lambda (student fb)
		 (if (null (car fb))
		     (push student without)))
	     feedback)
    without))

(defun org-sta-fh--students-without-grades (feedback)
  "Returns a list of students without grades in FEEDBACK."
  (let ((without ()))
    (maphash #'(lambda (student fb)
		 (if (null (cdr fb))
		     (push student without)))
	     feedback)
    without))

(defun org-sta-fh--check-feedback-and-grades (feedback)
  "Do consistency checks on FEEDBACK.

This checks that all students have both a grade and some feedback.
It rewturns t if everything has been completed. It raises an
error if there is missing data."
  (let ((wfs (org-sta-fh--students-without-feedback feedback)))
    (if (> (length wfs) 0)
	(error "Students without feedback: %s" (s-join ", " wfs))))
  (let ((wgs (org-sta-fh--students-without-grades feedback)))
    (if (> (length wgs) 0)
	(error "Students without grades: %s" (s-join ", " wgs))))
  t)



;; ---------- Export ----------

(defun org-sta-fh--feedback-file-name (student)
  "Return the filename used to store feedback for STUDENT."
  (concat (student ".txt")))

(defun org-sta-fh--grades-file-name (feedback)
  "Return the filename used to store grades from FEEDBACK.

At present this is always 'grades.csv'"
  "grades.csv")

(defun org-sta-fh--export-feedback (student feedback)
  "Export STUDENT's feedback as held in FEEDBACK.

The filename is determined by `org-sta-fh--feedback-file-name'."
  (let ((text (org-sta-fh--get-feedback student feedback))
	(fn (org-sta-fh--feedback-file-name student)))
    (with-temp-buffer
      (insert text)
      (write-file fn))))

(defun org-sta-fh--export-all-feedback (feedback)
  "Export all feddback from FEEDBACK."
  (maphash #'(lambda (student fb)
	       (org-sta-fh--export-feedback student feedback))
	   feedback))

(defun org-sta-fh--export-grades (feedback)
  "Export all grades from FEEDBACK.

The filename is determined by `org-sta-fh--grades-file-name'."
  (let ((fn (org-sta-fh--grades-file-name feedback)))
    (with-temp-buffer
      (maphash #'(lambda (student fb)
		   (let ((grade (cdr fb)))
		     (insert (concat student "," grade ",,\n"))))
	       feedback)
      (write-file fn))))

(defun org-sta-fh--export (feedback)
  "Export all grades and feedback in FEEDBACK."
  (org-sta-fh--export-grades feedback)
  (org-sta-fh--export-all-feedback feedback))


(provide 'org-sta-fh-feedback)
;;; org-sta-fh-feedback.el ends here
