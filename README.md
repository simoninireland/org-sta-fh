org-sta-fh: A St Andrews feedback helper for org mode
=====================================================

This is a simple Emacs package to help when bulk-uploading feedback on
student assignments from a document maintained using [Emacs org
mode](https://orgmode.org). It's intended specifically for use within
the [University of St Andrews](https://www.st-andrews.ac.uk) with our
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

Installation
------------

Clone the repo somewhere convenient, for example:

```
cd ~/.emacs.d
git clone git@github.com:simoninireland/org-sta-fh.git
```

Add the downloaded directory to your Emacs load path by adding
something like the following to your `.emacs` file:

```
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-sta-fh"))
```

You can if you wish bind the top-level user interface functions to
keys, again in `.emacs`:

```
(define-key org-mode-map (kbd "C-c e") #'org-sta-fh-export-feedback-tree)
```

(I use C-c e because it's close to the usual export keys, C-c C-e. You
can use any unused key combo you like, of course.)

Example
-------

Suppose you have the following feedback in org mode

```
* CSxxxx first practical

  It went pretty well; maybe a little more content would have helped.

** 123456789 18

   A great solution. Be careful with the indenting.

** 234567890 5.5

   Not a great solution. The indenting was the least of your problems.

```

(This is an example of a text format, not of good feedback
practices...) The feedback tree has a title and some header nodes,
which are ignored. The sub-trees immediately beneath the main tree
have a specific format, consisting of a student identifier and a
grade, both of which need to confirm to the St Andrews standards
(9-digit matriculation number, grade on the 20-point scale with at
most one decimal place).

Place the cursor in the tree headline and run
`org-sta-fh-export-feedback-tree`. This will ask for an export
directory and then produce three files there:

- 123456789.txt
- 234567890.txt
- grades.csv

The first two contain the feedback for the given students, in plain
text. The last contains the grades as a CSV spreadsheet. These can
then be bulk-uploaded directly to MMS.

To be done
----------

- Put the feedback and grades somewhere user-selected
- Handle feedback for groups of students, with group and individual
  feedback and grades
- Turn into a "proper" export backend
- Automate upload to MMS

Acknowledgements
----------------

This feedback helper is inspired by envy of Michael Young's [standalone
feedback helper](https://github.com/mtorpey/FeedbackHelper). It's a
work in progress to bring this sort of functionality to Emacs.
