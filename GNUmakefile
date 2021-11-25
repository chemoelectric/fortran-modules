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

# At least Fortran 2008 is needed.
FORTRAN_STANDARD = gnu
#FORTRAN_STANDARD = f2018
#FORTRAN_STANDARD = f2008

OBJEXT = o

M4 = m4
ifeq ($(FORTRAN_STANDARD),gnu)
M4FLAGS = -DCALL_ABORT="call abort"
else
M4FLAGS = -DCALL_ABORT="!call abort"
endif
M4FLAGS += -DCADADR_MAX=4

FC = gfortran
FCFLAGS = -std=$(FORTRAN_STANDARD) -g -fcheck=all
FCCOMPILE = $(FC) $(FCFLAGS) $(XFCFLAGS)

%.anchor: %.f90					# Makes a module file.
	$(FCCOMPILE) -c -fsyntax-only $(<) && touch $(@)

%.$(OBJEXT): %.anchor
	$(FCCOMPILE) -c $(<:.anchor=.f90) -o $(@)

.PRECIOUS: %.f90
%.f90: %.f90.m4 common-macros.m4
	$(M4) $(M4FLAGS) $(XM4FLAGS) $(<) > $(@)

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

test__cons_lists: test__cons_lists.$(OBJEXT) cons_lists.$(OBJEXT)
	$(FCCOMPILE) $(^) -o $(@)

cons_lists.f90: cadadr.m4 GNUmakefile

cons_lists.anchor: cons_lists.mod
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
