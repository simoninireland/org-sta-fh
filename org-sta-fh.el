;;; org-sta-fh.el --- org-based bulk upload of feedback -*- lexical-binding: t -*-

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

;; A small feedback helper library to automate uploading student
;; feedback maintained in an org mode document to MMS.

;;; Code:

;; ---------- Dependencies ----------

(require 's)
(require 'org)

(require 'org-sta-fh-feedback)
(require 'org-sta-fh-org)


;; ---------- Public interface ----------

(defun org-sta-fh-complete (feedback)
  "Check the all the grading is done in FEEDBACK.

This /doesn't/ check that we've graded all the students we should,
but /does/ check that all students have a grade and feedback."
  (org-sta-fh--check-feedback-and-grades feedback))

(defun org-sta-fh-export-at-point ()
  "Locate the tree at point and export it as feedback and grades.")


(provide 'org-sta-fh)
;;; org-sta-fh.el ends here
