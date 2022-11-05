;;; test-org.el --- Test interface to org documents -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Keywords: hypermedia, attachments

;; This file is NOT part of GNU Emacs.

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

;; Tests os the interface to org documents.

;;; Code:

(require 'ert)


;; ---------- Tests ----------

(ert-deftest matriculation-numbers ()
  "Test we can recognise tric numbers as student identifiers."
  (should (org-sta-fh--student-identifier? "123456789"))
  (should (org-sta-fh--student-identifier? "012345678"))
  (should (not (org-sta-fh--student-identifier? "1234567")))
  (should (not (org-sta-fh--student-identifier? "1234567890")))
  (should (not (org-sta-fh--student-identifier? "1234 567")))
  (should (not (org-sta-fh--student-identifier? "123456789 ")))
  (should (not (org-sta-fh--student-identifier? "sd80"))))

(ert-deftest grades ()
  "Test we can parse grades on the 20-point scale."
  (should (org-sta-fh--grade? 0))
  (should (org-sta-fh--grade? 20))
  (should (org-sta-fh--grade? 10))
  (should (org-sta-fh--grade? 10.5))
  (should-not (org-sta-fh--grade? 21))
  (should-not (org-sta-fh--grade? 20.5))
  (should-not (org-sta-fh--grade? 10.1))
  (should-not (org-sta-fh--grade? 10.51))
  (should-not (org-sta-fh--grade? -1)))
