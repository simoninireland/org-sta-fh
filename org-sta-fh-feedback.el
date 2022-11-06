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
;; cons cell consisting of a b uffer for the feedback and the grade.

;;; Code:

;; ---------- Data structures ----------

(defvar org-sta-fh--feedback
  (make-hash-table :size 100)
  "Hash table mappoing student identifier to a buffer containing feedback.")

(defvar org-sta-fh--grades
  (make-hash-table :size 100)
  "Hash table mapping student identifier to grade.")


;; ---------- Process management ----------

(defun org-sta-fh--start-grading ()
  "Start grading."
  (clrhash org-sta-fh--feedback)
  (clrhash org-sta-fh--grades))

(defun org-sta-fh--start-feedback ()
  "Reset the feedback structure ready for a new student or group."
  (clrhash org-sta-fh--feedback))

(defun org-sta-fh--end-feedback ()
  "Export the feedback for all the students."
  (org-sta-fh--export-all-feedback))

(defun org-sta-fh--end-grading ()
  "Finish grading and export the grades file."
  (org-sta-sh--export-grades))


;; ---------- Adding and retrieving feedback and grades ----------

(defun org-sta-fh--add-student (student grade)
  "Create a feedback record for a STUDENT with the given GRADE.

The feedback is left empty."
  ;; check whether student already exists
  (if (gethash student org-sta-fh--feedback)
      (error (format "Student %s already exists")))

  ;; create a new feedback buffer
  (let ((buf (generate-new-buffer (format "%s.org" student))))
    (puthash student buf org-sta-fh--feedback))

  ;; create a grade entry
  (puthash student buf org-sta-fh--grades))

(defun org-sta-fh--get-feedback (student)
  "Return the feedback for STUDENT."
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
	(insert "\n\n")
	(insert feedback)))))


;; ---------- Export ----------

(defun org-sta-fh--feedback-file-name (student)
  "Return the filename used to store feedback for STUDENT."
  (concat (student ".txt")))

(defun org-sta-fh--grades-file-name ()
  "Return the filename used to store grades.

At present this is always 'grades.csv'"
  "grades.csv")

(defun org-sta-fh--export-feedback (student feedback)
  "Export STUDENT's feedback as held in FEEDBACK.

The filename is determined by `org-sta-fh--feedback-file-name'."
  (let* ((buf (org-sta-fh--get-feedback student))
	 (fn (org-sta-fh--feedback-file-name student)))
    (save-excursion
      (with-current-buffer buf
	(let ((export (org-ascii-export-to-ascii)))
	  (with-current-buffer export
	    (write-file fn)))))))

(defun org-sta-fh--export-all-feedback ()
  "Export all fedback."
  (maphash (lambda (student fb)
	       (org-sta-fh--export-feedback student))
	   org-sta-fh--feedback))

(defun org-sta-fh--export-grades ()
  "Export all grades.

The filename is determined by `org-sta-fh--grades-file-name'."
  (let ((fn (org-sta-fh--grades-file-name feedback)))
    (with-temp-buffer
      (maphash (lambda (student grade)
		 (insert (concat student "," grade ",,\n")))
	       org-sta-fh--grades)
      (write-file fn))))


(provide 'org-sta-fh-feedback)
;;; org-sta-fh-feedback.el ends here
