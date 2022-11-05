;;; test-feedback.el --- Test feedback data structure -*- lexical-binding: t -*-

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

;; Tests of the feedback data structure.

;;; Code:

(require 'ert)


;; ---------- Test environment ----------

(defmacro with-feedback (&rest body)
  "Run BODY with a variable 'feedback' holding a feedback structure."
  (declare (indent defun))
  `(let ((feedback (org-sta-fh--make-feedback)))
     ,@body))


;; ---------- Tests ----------

(ert-deftest feedback-structure-creation ()
  "Test we can create a feedback structure."
  (with-feedback
    (should (org-sta-fh--feedback? feedback))))

(ert-deftest feedback-initially-blank ()
  "Test that feedback for a student is initially empty"
  (let ((student "012345678"))
    (with-feedback
      (should (null (org-sta-fh--get-feedback student feedback))))))

(ert-deftest add-new-feedback ()
  "Test we can set feedback and not affect a null grade."
  (let ((student "012345678"))
    (with-feedback
      (org-sta-fh--set-feedback student "Good work!" feedback)
      (should (string-equal (org-sta-fh--get-feedback student feedback)
			    "Good work!"))
      (should (null (org-sta-fh--get-grade student feedback))))))

(ert-deftest add-new-grade ()
  "Test we can set a grade and not affect null feedback."
  (let ((student "012345678"))
    (with-feedback
      (org-sta-fh--set-grade student 17 feedback)
      (should (eq (org-sta-fh--get-grade student feedback)
		  17))
      (should (null (org-sta-fh--get-feedback student feedback))))))

(ert-deftest add-feedback-then-grade ()
  "Test we can set a grade and not affect existing feedback."
  (let ((student "012345678"))
    (with-feedback
      (org-sta-fh--set-feedback student "Good work!" feedback)
      (org-sta-fh--set-grade student 17 feedback)
      (should (eq (org-sta-fh--get-grade student feedback)
		  17))
      (should (string-equal (org-sta-fh--get-feedback student feedback)
			    "Good work!")))))

(ert-deftest add-grade-then-feedback ()
  "Test we can set feedback and not affect an existing grade."
  (with-feedback
    (let ((student "012345678"))
      (org-sta-fh--set-grade student 17 feedback)
      (org-sta-fh--set-feedback student "Good work!" feedback)
      (should (eq (org-sta-fh--get-grade student feedback)
		  17))
      (should (string-equal (org-sta-fh--get-feedback student feedback)
			    "Good work!")))))

(ert-deftest update-feedback ()
  "Test we can update feedback and not affect an existing grade."
  (let ((student "012345678"))
    (with-feedback
      (org-sta-fh--set-feedback student "Good work!" feedback)
      (org-sta-fh--set-grade student 17 feedback)
      (org-sta-fh--set-feedback student "Kind of" feedback)
      (should (eq (org-sta-fh--get-grade student feedback)
		  17))
      (should (string-equal (org-sta-fh--get-feedback student feedback)
			    "Good work!\n\nKind of")))))

(ert-deftest update-grades ()
  "Test we can update grade and not affect existing feedback."
  (let ((student "012345678"))
    (with-feedback
      (org-sta-fh--set-feedback student "Good work!" feedback)
      (org-sta-fh--set-grade student 1 feedback)
      (org-sta-fh--set-grade student 17 feedback)
      (should (eq (org-sta-fh--get-grade student feedback)
		  17))
      (should (string-equal (org-sta-fh--get-feedback student feedback)
			    "Good work!")))))

(ert-deftest two-students ()
  "Test we can maintain two students without an issue"
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-grade s2 18 feedback)
      (org-sta-fh--set-feedback s2 "You rock!" feedback)
      (should (eq (org-sta-fh--get-grade s1 feedback) 5))
      (should (string-equal (org-sta-fh--get-feedback s1 feedback)
			    "You suck!"))
      (should (eq (org-sta-fh--get-grade s2 feedback) 18))
      (should (string-equal (org-sta-fh--get-feedback s2 feedback)
			    "You rock!")))))

(ert-deftest students-without-feedback ()
  "Test we can detect students without feedback (when there is one)."
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-grade s2 18 feedback)
      (should (equal (org-sta-fh--students-without-feedback feedback)
		     (list s2))))))

(ert-deftest students-not-without-feedback ()
  "Test we can detect students without feedback (when there aren't any)."
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-grade s2 18 feedback)
      (org-sta-fh--set-feedback s2 "You rock!" feedback)
      (should (eq (length (org-sta-fh--students-without-feedback feedback)) 0)))))

(ert-deftest students-without-grades ()
  "Test we can detect students without grades (when there is one)."
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-feedback s2 "You rock!" feedback)
      (should (equal (org-sta-fh--students-without-grades feedback)
		     (list s2))))))

(ert-deftest students-not-without-grades ()
  "Test we can detect students without grades (when there aren't any)."
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-grade s2 18 feedback)
      (org-sta-fh--set-feedback s2 "You rock!" feedback)
      (should (eq (length (org-sta-fh--students-without-grades feedback)) 0)))))

(ert-deftest students-without-grade-errors ()
  "Test we raise an error for students without grades (when there is one)."
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-feedback s2 "You rock!" feedback)
      (should-error
       (org-sta-fh--check-feedback-and-grades feedback)))))

(ert-deftest students-without-feedback-errors ()
  "Test we raise an error for students without feedback(when there is one)."
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-grade s2 18 feedback)
      (should-error
       (org-sta-fh--check-feedback-and-grades feedback)))))

(ert-deftest students-without-errors ()
  "Test we don't raise an error if there shouldn't be one."
  (let ((s1 "12345")
	(s2 "56789"))
    (with-feedback
      (org-sta-fh--set-grade s1 5 feedback)
      (org-sta-fh--set-feedback s1 "You suck!" feedback)
      (org-sta-fh--set-grade s2 18 feedback)
      (org-sta-fh--set-feedback s2 "You rock!" feedback)
      (should
       (org-sta-fh--check-feedback-and-grades feedback)))))
