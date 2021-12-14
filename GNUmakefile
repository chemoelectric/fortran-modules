# Copyright 2021 Barry Schwartz
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# For help in writing gfortran makefiles, see
# https://aoterodelaroza.github.io/devnotes/modern-fortran-makefiles/

.DELETE_ON_ERROR:
.DEFAULT_GOAL = default

# At least Fortran 2008 is needed.
#FORTRAN_STANDARD = gnu
#FORTRAN_STANDARD = f2018
FORTRAN_STANDARD = f2008

OBJEXT = o

M4 = m4
#M4 = m4-heirloom -B10000"
M4FLAGS =
COMPILE.m4 = $(M4) $(M4FLAGS) $(XM4FLAGS)

#M4FLAGS += -DDEBUGGING=true

# How many lists can be discarded with one call?
M4FLAGS += -DLIST_DISCARDN_MAX=10

# How deep should car-cdr permutations go?
M4FLAGS += -DCADADR_MAX=4

# How many arguments should list1.. and unlist1.. go up to?
M4FLAGS += -DLISTN_MAX=20

# How many arguments should zips and unzips go up to?
M4FLAGS += -DZIP_MAX=10

FC = gfortran
FCFLAGS = -std=$(FORTRAN_STANDARD) -g -fcheck=all -Wall $(FCFLAG_WNO_TRAMPOLINES)
COMPILE.f90 = $(FC) $(FCFLAGS) $(XFCFLAGS)
FCFLAG_WNO_TRAMPOLINES = -Wno-trampolines
FCFLAG_WTRAMPOLINES = -Wtrampolines
FCFLAG_WNO_UNUSED_DUMMY_ARGUMENT = -Wno-unused-dummy-argument

%.anchor: %.f90
	$(COMPILE.f90) -c -fsyntax-only $(<) && touch $(@)

%.$(OBJEXT): %.anchor
	$(COMPILE.f90) -c $(<:.anchor=.f90) -o $(@)

.PRECIOUS: %.f90
%.f90: %.f90.m4 common-macros.m4
	$(COMPILE.m4) $(<) > $(@)

unused_variables.anchor: unused_variables.f90
	$(COMPILE.f90) $(FCFLAG_WTRAMPOLINES) $(FCFLAG_WNO_UNUSED_DUMMY_ARGUMENT) -c -fsyntax-only $(<) && touch $(@)
unused_variables.$(OBJEXT): unused_variables.anchor
	$(COMPILE.f90) $(FCFLAG_WTRAMPOLINES) $(FCFLAG_WNO_UNUSED_DUMMY_ARGUMENT) -c $(<:.anchor=.f90) -o $(@)

#
# The core modules are to be kept free of trampolines, which gfortran
# generates on AMD64 and other platforms.
#
# Avoidance of trampolines results in less code re-use, but probably
# faster code. Mainly, however, it is not for code speed but to let
# the core modules work with hardened, non-executable stacks.
#
cons_lists.anchor: cons_lists.f90
	$(COMPILE.f90) $(FCFLAG_WTRAMPOLINES) -c -fsyntax-only $(<) && touch $(@)
cons_lists.$(OBJEXT): cons_lists.anchor
	$(COMPILE.f90) $(FCFLAG_WTRAMPOLINES) -c $(<:.anchor=.f90) -o $(@)

.PHONY: all default
default: all
all: modules tests

.PHONY: modules
modules: cons_lists.$(OBJEXT)

.PHONY: tests
tests: test__cons_lists

.PHONY: check
check: tests
	./test__cons_lists

test__cons_lists: test__cons_lists.$(OBJEXT) cons_lists.$(OBJEXT) unused_variables.$(OBJEXT)
	$(COMPILE.f90) $(^) -o $(@)

cons_lists.f90: cadadr.m4

unused_variables.anchor: unused_variables.mod
unused_variables.mod:

garbage_collector.anchor: unused_variables.anchor
garbage_collector.anchor: garbage_collector.mod
garbage_collector.mod:

cons_lists.anchor: unused_variables.anchor
cons_lists.anchor: cons_types.mod
cons_lists.anchor: cons_lists.mod
cons_types.mod:
cons_lists.mod:

test__cons_lists.anchor: cons_lists.anchor
test__cons_lists.anchor: test__cons_lists.mod
test__cons_lists.mod:

.PHONY: clean maintainer-clean
clean:
	-rm -f test__cons_lists
	-rm -f *.mod
	-rm -f *.$(OBJEXT)
	-rm -f *.anchor
maintainer-clean: clean
	-rm -f cons_lists.f90 test__cons_lists.f90
