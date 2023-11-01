;;; org-sta-fh-feedback.el --- Feedback data structure -*- lexical-binding: t -*-

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

;; A feedback data structure.
;;
;; We store feedback in a hash table for speed compared to alists.
;; The hash table is keyed by student identifier; each value is a
;; cons cell consisting of a buffer for the feedback and the grade.

;;; Code:

;; ---------- Data structures ----------
;; :test 'equal because student identifiers are strings

(defvar org-sta-fh--feedback
  (make-hash-table :size 10 :test 'equal)
  "Hash table mapping student identifier to a buffer containing feedback.")

(defvar org-sta-fh--grades
  (make-hash-table :size 100 :test 'equal)
  "Hash table mapping student identifier to grade.")


;; ---------- Process management ----------

(defun org-sta-fh--cleanup-buffers ()
  "Clean up all the feedback buffers."
  (maphash (lambda (_ buf)
	     (kill-buffer buf))
	   org-sta-fh--feedback))

(defun org-sta-fh--start-grading ()
  "Start grading."
  (clrhash org-sta-fh--feedback)
  (clrhash org-sta-fh--grades))

(defun org-sta-fh--end-grading (dir)
  "Finish grading and export the grades file to DIR."
  (org-sta-fh--export-all-feedback dir)
  (org-sta-fh--export-grades dir))


;; ---------- Adding and retrieving feedback and grades ----------

(defun org-sta-fh--add-student (student grade)
  "Create a feedback record for a STUDENT with the given GRADE.

The feedback is left empty."
  ;; check whether student already exists
  (if (gethash student org-sta-fh--feedback)
      (error (format "Student %s already exists" student)))

  ;; create a new feedback buffer
  (let ((buf (generate-new-buffer (format "%s.txt" student))))
    (puthash student buf org-sta-fh--feedback))

  ;; create a grade entry
  (puthash student grade org-sta-fh--grades))

(defun org-sta-fh--get-feedback (student)
  "Return the feedback buffer for STUDENT."
  (gethash student org-sta-fh--feedback))

(defun org-sta-fh--get-grade (student)
  "Return the grade for STUDENT."
  (gethash student org-sta-fh--grades))

(defun org-sta-fh--add-feedback (student feedback)
  "Add FEEDBACK for STUDENT."
  (let ((buf (org-sta-fh--get-feedback student)))
    (save-excursion
      (with-current-buffer buf
	(goto-char (point-max))
	(if (> (point) 0)
	    (insert "\n\n"))
	(insert feedback)))))


;; ---------- Export ----------

(defun org-sta-fh--feedback-file-name (student dir)
  "Return the filename used to store feedback for STUDENT in DIR."
  (f-join dir (concat student ".txt")))

(defun org-sta-fh--grades-file-name (dir)
  "Return the filename used to store grades in DIR.

At present this is always 'grades.csv'"
  (f-join dir "grades.csv"))

(defun org-sta-fh--export-feedback (student dir)
  "Export STUDENT's feedback to directory DIR.

The filename is determined by `org-sta-fh--feedback-file-name'."
  (let ((buf (org-sta-fh--get-feedback student))
	(fn (org-sta-fh--feedback-file-name student dir)))
    (save-excursion
      (with-current-buffer buf
	(write-file fn)))))

(defun org-sta-fh--export-all-feedback (dir)
  "Export all feedback."
  (maphash (lambda (student _)
	     (org-sta-fh--export-feedback student dir))
	   org-sta-fh--feedback))

(defun org-sta-fh--export-grades (dir)
  "Export all grades to DIR.

The filename is determined by `org-sta-fh--grades-file-name'."
  (let ((fn (org-sta-fh--grades-file-name dir)))
    (with-temp-buffer
      (maphash (lambda (student grade)
		 (insert (concat student "," grade ",,\n")))
	       org-sta-fh--grades)
      (write-file fn))))


(provide 'org-sta-fh-feedback)
;;; org-sta-fh-feedback.el ends here
