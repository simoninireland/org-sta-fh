;;; test-org.el --- Test interface to org documents -*- lexical-binding: t -*-

;; Copyright (c) 2022--2023 Simon Dobson <simoninireland@gmail.com>

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
  (should (org-sta-fh--valid-student-identifier? "123456789"))
  (should (org-sta-fh--valid-student-identifier? "012345678"))
  (should (not (org-sta-fh--valid-student-identifier? "1234567")))
  (should (not (org-sta-fh--valid-student-identifier? "1234567890")))
  (should (not (org-sta-fh--valid-student-identifier? "1234 567")))
  (should (not (org-sta-fh--valid-student-identifier? "123456789 ")))
  (should (not (org-sta-fh--valid-student-identifier? "sd80"))))

(ert-deftest differentiate-students-grades ()
  "Test we can differentiate a student identifier from a grade.
(We don't guarantee that the other way doesn't work.)"
  (should (org-sta-fh--valid-student-identifier? "123456789"))
  (should-not (org-sta-fh--valid-student-identifier? "10"))
  (should-not (org-sta-fh--valid-student-identifier? "10.5")))

(ert-deftest valid-grades ()
  "Test we can identify valid grades on the 20-point scale."
  (should (org-sta-fh--valid-grade? 0))
  (should (org-sta-fh--valid-grade? 20))
  (should (org-sta-fh--valid-grade? "20"))      ; we allow strings as well
  (should (org-sta-fh--valid-grade? 10))
  (should (org-sta-fh--valid-grade? 10.5))
  (should (org-sta-fh--valid-grade? "10.5"))
  (should-not (org-sta-fh--valid-grade? 21))
  (should-not (org-sta-fh--valid-grade? "21"))
  (should-not (org-sta-fh--valid-grade? 20.5))
  (should-not (org-sta-fh--valid-grade? 10.1))
  (should-not (org-sta-fh--valid-grade? 10.51))
  (should-not (org-sta-fh--valid-grade? -1)))

(ert-deftest header ()
  "Test we can parse headers."
  (let ((sgs (org-sta-fh--parse-headline "123456789")))
    (should (and (equal (car sgs) (list "123456789"))
		 (null (cdr sgs)))))
  (should-error (org-sta-fh--parse-headline "10.5"))
  (let ((sgs (org-sta-fh--parse-headline "123456789 10.5")))
    (should (and (equal (car sgs) (list "123456789"))
		 (= (cdr sgs) 10.5))))
  (let ((sgs (org-sta-fh--parse-headline "123456789 234567890")))
    (should (and (equal (car sgs) (list "123456789" "234567890"))
		 (null (cdr sgs)))))
  (let ((sgs (org-sta-fh--parse-headline "123456789 234567890 10.5")))
    (should (and (equal (car sgs) (list "123456789" "234567890"))
		 (= (cdr sgs) 10.5)))))

(ert-deftest submissions ()
  "Test we can extract submission file patterns."
  (should (equal (org-sta-fh--filter-submissions (list "123456789"))
		 (list "123456789")))
  (should (equal (org-sta-fh--filter-submissions (list))
		 nil))
  (should (equal (org-sta-fh--filter-submissions (list "123456789" "234567890"))
		 (list "123456789" "234567890")))
  (should (equal (org-sta-fh--filter-submissions (list "1234567890" "234567890"))
		 (list "234567890")))
  (should (equal (org-sta-fh--filter-submissions (list "123456789" "2345678900"))
		 (list "123456789")))
  (should (equal (org-sta-fh--filter-submissions (list "123456789.zip" "234567890"))
		 (list "123456789" "234567890")))
  (should (equal (org-sta-fh--filter-submissions (list "123456789.zip" "234567890.txt"))
		 (list "123456789" "234567890"))))
