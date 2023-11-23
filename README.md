# org-sta-fh: A St Andrews feedback helper for org mode

This is a simple Emacs package to help when bulk-uploading feedback
on student assignments from a document maintained using [Emacs org
mode](https://orgmode.org). It's intended specifically for use within the [University of
St Andrews](https://www.st-andrews.ac.uk) with our MMS module management system, but could be
adapted to other systems. It's also a useful exploration of how to
manipulate org-mode documents.


## Use case

You are grading a student assignment. Each student has uploaded a
submission. Your tasks are as follows:

-   Create a list of students to be graded
-   Write feedback for individual students
-   Assign a grade to each student
-   Upload the feedback and grades to MMS

`org-sta-fh` creates a headline per student for the first task,
captures feedback and grades from the next two tasks, and creates the
files needed to complete the fourth.


## Installation

Clone the repo somewhere convenient, for example:

    cd ~/.emacs.d
    git clone git@github.com:simoninireland/org-sta-fh.git

Add the downloaded directory to your Emacs load path and load the
package by adding something like the following to your `.emacs`
file:

    (add-to-list 'load-path (expand-file-name "~/.emacs.d/org-sta-fh"))
    (require 'org-sta-fh)


## Top-level functions

`org-sta-fh` defines two top-level interactive commands:

-  `org-sta-sh-build-feedback-tree`: Asks for a directory and looks
   for files or directories whose base names match the pattern for
   student identifiers ("matriculation numbers" in St Andrews
   terminology). Then creates a heading per student at point, nested
   appropriately
-  `org-sta-sh-export-feedback-tree`: Prompts for a feedback
   directory, and then creates a file for every heading beneath the
   current heading whose name matches a student identifier followed by
   a grade, plus a spreadsheet of grades. These are the files needed
   by MMS.

These commands only make sense in org mode buffers. You can bind them
to keys if you wish, presumably within `org-mode-map`.


## Example

Suppose you have the following feedback in org mode

    * CSxxxx first practical

      It went pretty well; maybe a little more content would have helped.

    ** 123456789 18

       A great solution. Be careful with the indenting.

    ** 234567890 5.5

       Not a great solution. The indenting was the least of your problems.

(This is an example of a text format, not of good feedback
practices &#x2026;) The feedback tree has a title and some header notes,
which are ignored. The sub-trees immediately beneath the main tree
have a specific format, consisting of a student identifier and a
grade, both of which need to confirm to the St Andrews standards
(9-digit matriculation number, grade on the 20-point scale with at
most one decimal place).

Place the cursor in the tree headline and run
`org-sta-fh-export-feedback-tree`. This will ask for an export
directory and then produce three files there:

-   123456789.txt
-   234567890.txt
-   grades.csv

The first two contain the feedback for the given students, in plain
text. The last contains the grades as a CSV spreadsheet. These can
then be bulk-uploaded directly to MMS.

The feedback tree can start at any depth in an org file: all that
matters is that the feedback sub-trees lie immediately beneath
it. You can include any markup in the feedback that you like,
including extra sub-headings: these will be exported to the
feedback file.


## To be done

-   Handle feedback for groups of students, with group feedback and
    grades
-   Handle cohort-wide general feedback


## Acknowledgements

This feedback helper is inspired by envy of Michael Young's
[standalone feedback helper](https://github.com/mtorpey/FeedbackHelper). It's a work in progress to bring this
sort of functionality to Emacs.
