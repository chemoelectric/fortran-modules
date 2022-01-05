# Copyright 2021, 2022 Barry Schwartz
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

# What INTEGER kind should be used to represent sizes? (Names from
# ISO_Fortran_Env and ISO_C_Binding are available.)
M4FLAGS += -DSIZE_KIND=int64

# How deep should car-cdr permutations go?
M4FLAGS += -DCADADR_MAX=4

# How many arguments should list1.. and unlist1.. go up to?
M4FLAGS += -DLISTN_MAX=20

# How many arguments should zips and unzips go up to? This number also
# is used for maps and similar procedures.
M4FLAGS += -DZIP_MAX=10

FC = gfortran

FCFLAGS = -std=$(FORTRAN_STANDARD) -g -fcheck=all -Wall -Wextra

# It is a grave error to teach that it is ‘wrong’ to test equality of
# two floating point numbers. (Awk, for instance, uses floating point
# format to store integers.)
FCFLAGS += -Wno-compare-reals

# Unused dummy arguments are often a fine thing.
FCFLAGS += -Wno-unused-dummy-argument

COMPILE.f90 = $(FC) $(FCFLAGS) $(XFCFLAGS)

# Sometimes we tolerate trampolines.
FCFLAG_WNO_TRAMPOLINES = -Wno-trampolines

# Sometimes we tolerate elimination of impure functions.
FCFLAGS_WNO_FUNCTION_ELIMINATION = -Wno-function-elimination

%.anchor: %.f90
	$(COMPILE.f90) -c -fsyntax-only $(<) && touch $(@)

%.$(OBJEXT): %.anchor
	$(COMPILE.f90) -c $(<:.anchor=.f90) -o $(@)

.PRECIOUS: %.f90
%.f90: %.f90.m4 common-macros.m4
	$(COMPILE.m4) $(<) > $(@)

MODULE_BASENAMES =
MODULE_BASENAMES += garbage_collector
MODULE_BASENAMES += boxes
MODULE_BASENAMES += cons_pairs
MODULE_BASENAMES += lsets

TEST_PROGRAM_BASENAMES =
TEST_PROGRAM_BASENAMES += test__boxes
TEST_PROGRAM_BASENAMES += test__cons_pairs

.PHONY: all default
default: all
all: modules tests

.PHONY: modules
modules: $(addsuffix .$(OBJEXT), $(MODULE_BASENAMES))

.PHONY: tests
tests: $(TEST_PROGRAM_BASENAMES)

.PHONY: check
check: check-boxes
check: check-cons_pairs

.PHONY: check-boxes
check-boxes: test__boxes
	./test__boxes

.PHONY: check-cons_pairs
check-cons_pairs: test__cons_pairs
	./test__cons_pairs

test__boxes: $(addsuffix .$(OBJEXT), test__boxes boxes garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

test__cons_pairs: $(addsuffix .$(OBJEXT), test__cons_pairs cons_pairs garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

test__cons_pairs.anchor: test__cons_pairs.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c -fsyntax-only $(<) && touch $(@)
test__cons_pairs.$(OBJEXT): test__cons_pairs.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c $(<:.anchor=.f90) -o $(@)

cons_lists.f90: cadadr.m4

garbage_collector.anchor: garbage_collector.mod
garbage_collector.mod:

boxes.anchor: garbage_collector.anchor
boxes.anchor: boxes.mod
boxes.mod:

cons_pairs.f90: cadadr.m4
cons_pairs.anchor: garbage_collector.anchor
cons_pairs.anchor: cons_pairs.mod
cons_pairs.mod:

lsets.anchor: garbage_collector.anchor
lsets.anchor: cons_pairs.mod
lsets.anchor: lsets.mod
lsets.mod:

test__boxes.anchor: boxes.anchor
test__boxes.anchor: test__boxes.mod
test__boxes.mod:

test__cons_pairs.f90: cadadr.m4
test__cons_pairs.anchor: cons_pairs.anchor
test__cons_pairs.anchor: test__cons_pairs.mod
test__cons_pairs.mod:

suffixed-all-basenames = $(addsuffix $(shell printf "%s" $(1)),$(MODULE_BASENAMES) $(TEST_PROGRAM_BASENAMES))

.PHONY: clean maintainer-clean
clean:
	-rm -f $(TEST_PROGRAM_BASENAMES)
	-rm -f *.mod
	-rm -f $(call suffixed-all-basenames, .$(OBJEXT))
	-rm -f $(call suffixed-all-basenames, .anchor)
maintainer-clean: clean
	-rm -f $(call suffixed-all-basenames, .f90)
