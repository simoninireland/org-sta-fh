org-sta-fh: A St Andrews feedback helper for org mode
=====================================================

This is a simple Emacs package to help when bulk-uploading feedback on
student assignments from a document maintained using `Emacs org
mode <https://orgmode.org>`_. It's intended specifically for use within
the `University of St Andrews <https://www.st-andrews.ac.uk>`_ with our
MMS module management system, but could be adapted to other systems.
It's also a useful exploration of how to manipulate org-mode
documents.

Use case
--------

You are grading a student assignment. Each student has uploaded a
submission, possibly individual and possibly as part of a group. Your
tasks are as follows:

- Write feedback for individual students
- Assign a grade to each student

Org mode
--------

Feedback is stored either in an org mode document or in a tree within
a larger document. The top-level heading is the assignment title, and
there may be text and notes below: these are ignored.

For each student there is a heading containing their unique student
identifier and their grade. (In St Andrews the unique identifiers are
matriculation numbers, and the grades adhere to our common 20-point
scale.) Under that heading is the feedback for the student.

The workflow is therefore to create headings for students, decide on
the feedback, and then add the grade to the heading. The results is a
tree containing sub-headings containing feedback and grades.

Export format
-------------

The tree is exported to a set of files in a directory:

- One file per student, with their student identifier as the file
  stem, containing the feedback in plain text
- One file called `grades.csv` containing the grades in CSV format,
  first column the student identifier, second column the grade

Acknowledgements
----------------

This feedback helper is inspired by envy of Michael Young's `standalone
feedback helper <https://github.com/mtorpey/FeedbackHelper>`_. It's a
work in progress to bring this sort of functionality to Emacs.
