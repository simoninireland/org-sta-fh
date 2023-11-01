;;; org-sta-fh.el --- Org-based bulk upload of feedback -*- lexical-binding: t -*-

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

;; A small feedback helper library to automate uploading student
;; feedback maintained in an org mode document to MMS.

;;; Code:

;; ---------- Dependencies ----------

(require 's)
(require 'f)
(require 'dash)
(require 'org)

(require 'org-sta-fh-feedback)
(require 'org-sta-fh-ids)
(require 'org-sta-fh-org)


;; ---------- Public interface ----------

(defun org-sta-fh-export-feedback-tree (dir)
  "Export the grades and feedback files to directory DIR for the org tree at point."
  (interactive "DFeedback directory: ")
  (let* ((tree (org-element-parse-buffer))
	 (h (org-sta-fh--find-headline-at-point tree)))
    (save-mark-and-excursion
      (unwind-protect
	  (progn
	    (org-sta-fh--start-grading)
	    (org-sta-fh--parse-tree h)
	    (org-sta-fh--end-grading dir))

	;; delete any buffers we created
	(org-sta-fh--cleanup-buffers)))))

(defun org-sta-fh-build-feedback-tree (title dir)
  "Build a feedback tree with header TITLE from the submissions in DIR."
  (interactive "sTitle: \nDSubmissions directory: ")
  (let* ((tree (org-element-parse-buffer)))
    (save-excursion
      (org-sta-fh--insert-feedback-tree title ids 1))))


(provide 'org-sta-fh)
;;; org-sta-fh.el ends here
