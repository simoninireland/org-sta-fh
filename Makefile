# Copyright (c) 2022--2023 Simon Dobson <simoninireland@gmail.com>

# Author: Simon Dobson <simoninireland@gmail.com>
# Maintainer: Simon Dobson <simoninireland@gmail.com>

# This file is NOT part of GNU Emacs.
#
# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# Source files
MAIN_SOURCE_FILE = \
	org-sta-fh.el
LIBRARY_SOURCE_FILES = \
	org-sta-fh-ids.el \
	org-sta-fh-feedback.el \
	org-sta-fh-org.el
SOURCE_FILES = \
	$(MAIN_SOURCE_FILE) \
	$(LIBARRY_SOURCE_FILES)

# Unit tests
SOURCES_TESTS = \
	test/test-feedback.el \
	test/test-org.el

# Tools
EMACS = emacs
CASK = cask
RM = rm -fr

# Virtual environment
VENV = .cask

# Constructed tools
RUN_EMACS = $(CASK) exec $(EMACS) -Q -batch -L "."

# Top-level targets

# Build virtual environment
.PHONY: env
env: $(VENV)

$(VENV):
	$(CASK) install

.PHONY: test
test: env
	$(RUN_EMACS) \
	-l $(MAIN_SOURCE_FILE) \
	$(SOURCES_TESTS:%=-l %) \
	--eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"

.PHONY: lint
lint: env
	$(RUN_EMACS) \
	--eval "(progn (require 'package-lint)(package-lint-batch-and-exit))" \
	$(SOURCE_FILES)

# Clean up the build
.PHONY: clean
clean:
	$(RM) $(VENV)
