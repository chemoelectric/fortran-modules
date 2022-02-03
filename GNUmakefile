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

#
# What INTEGER kind should be used to represent sizes?
#
# Names from ISO_Fortran_Env and ISO_C_Binding are available.
#
# Note that the implementation of the stable sort function in
# vectars.f90 may assume SIZE_KIND (or, at least, the size of any
# possible array) is no more than 64 bits.
#
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

# If you want to do coverage analysis.
#FCFLAGS += --coverage

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
MODULE_BASENAMES += vectars
MODULE_BASENAMES += sorting_and_selection

TEST_PROGRAM_BASENAMES =
TEST_PROGRAM_BASENAMES += test__boxes
TEST_PROGRAM_BASENAMES += test__cons_pairs
TEST_PROGRAM_BASENAMES += test__lsets
TEST_PROGRAM_BASENAMES += test__vectars
TEST_PROGRAM_BASENAMES += test__sorting_and_selection

EXAMPLE_PROGRAM_BASENAMES += example__knights_tour
EXAMPLE_PROGRAM_BASENAMES += example__n_queens

.PHONY: all default
default: all
all: modules tests examples

.PHONY: modules
modules: $(addsuffix .$(OBJEXT), $(MODULE_BASENAMES))

.PHONY: tests
tests: $(TEST_PROGRAM_BASENAMES)

.PHONY: examples
examples: $(EXAMPLE_PROGRAM_BASENAMES)

.PHONY: check
check: check-boxes
check: check-cons_pairs
check: check-lsets
check: check-vectars
check: check-sorting_and_selection

.PHONY: check-boxes
check-boxes: test__boxes
	./test__boxes

.PHONY: check-cons_pairs
check-cons_pairs: test__cons_pairs
	./test__cons_pairs

.PHONY: check-lsets
check-lsets: test__lsets
	./test__lsets

.PHONY: check-vectars
check-vectars: test__vectars
	./test__vectars

.PHONY: check-sorting_and_selection
check-sorting_and_selection: test__sorting_and_selection
	./test__sorting_and_selection

test__boxes: $(addsuffix .$(OBJEXT), test__boxes boxes garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

test__cons_pairs: $(addsuffix .$(OBJEXT), test__cons_pairs cons_pairs garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

test__lsets: $(addsuffix .$(OBJEXT), test__lsets lsets cons_pairs garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

test__vectars: $(addsuffix .$(OBJEXT), test__vectars vectars cons_pairs garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

test__sorting_and_selection: $(addsuffix .$(OBJEXT), test__sorting_and_selection sorting_and_selection \
														vectars cons_pairs garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

example__knights_tour: $(addsuffix .$(OBJEXT), example__knights_tour cons_pairs lsets \
													sorting_and_selection vectars garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

example__n_queens: $(addsuffix .$(OBJEXT), example__n_queens cons_pairs garbage_collector)
	$(COMPILE.f90) $(^) -o $(@)

lsets.anchor: lsets.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) -c -fsyntax-only $(<) && touch $(@)
lsets.$(OBJEXT): lsets.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) -c $(<:.anchor=.f90) -o $(@)

test__cons_pairs.anchor: test__cons_pairs.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c -fsyntax-only $(<) && touch $(@)
test__cons_pairs.$(OBJEXT): test__cons_pairs.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c $(<:.anchor=.f90) -o $(@)

test__lsets.anchor: test__lsets.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c -fsyntax-only $(<) && touch $(@)
test__lsets.$(OBJEXT): test__lsets.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c $(<:.anchor=.f90) -o $(@)

test__vectars.anchor: test__vectars.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c -fsyntax-only $(<) && touch $(@)
test__vectars.$(OBJEXT): test__vectars.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c $(<:.anchor=.f90) -o $(@)

test__sorting_and_selection.anchor: test__sorting_and_selection.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c -fsyntax-only $(<) && touch $(@)
test__sorting_and_selection.$(OBJEXT): test__sorting_and_selection.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c $(<:.anchor=.f90) -o $(@)

example__knights_tour.anchor: example__knights_tour.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c -fsyntax-only $(<) && touch $(@)
example__knights_tour.$(OBJEXT): example__knights_tour.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c $(<:.anchor=.f90) -o $(@)

example__n_queens.anchor: example__n_queens.f90
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c -fsyntax-only $(<) && touch $(@)
example__n_queens.$(OBJEXT): example__n_queens.anchor
	$(COMPILE.f90) $(FCFLAG_WNO_TRAMPOLINES) $(FCFLAGS_WNO_FUNCTION_ELIMINATION) -c $(<:.anchor=.f90) -o $(@)

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
lsets.anchor: cons_pairs.anchor
lsets.anchor: lsets.mod
lsets.mod:

vectars.anchor: garbage_collector.anchor
vectars.anchor: cons_pairs.anchor
vectars.anchor: vectars.mod
vectars.mod:

sorting_and_selection.anchor: garbage_collector.anchor
sorting_and_selection.anchor: cons_pairs.anchor
sorting_and_selection.anchor: vectars.anchor
sorting_and_selection.anchor: sorting_and_selection.mod
sorting_and_selection.mod:

test__boxes.anchor: boxes.anchor
test__boxes.anchor: test__boxes.mod
test__boxes.mod:

test__cons_pairs.f90: cadadr.m4
test__cons_pairs.anchor: garbage_collector.anchor
test__cons_pairs.anchor: cons_pairs.anchor
test__cons_pairs.anchor: test__cons_pairs.mod
test__cons_pairs.mod:

test__lsets.f90: lstsort.m4
test__lsets.anchor: garbage_collector.anchor
test__lsets.anchor: cons_pairs.anchor
test__lsets.anchor: lsets.anchor
test__lsets.anchor: test__lsets.mod
test__lsets.mod:

test__vectars.anchor: garbage_collector.anchor
test__vectars.anchor: cons_pairs.anchor
test__vectars.anchor: vectars.anchor
test__vectars.anchor: test__vectars.mod
test__vectars.mod:

test__sorting_and_selection.f90: lstsort.m4
test__sorting_and_selection.anchor: garbage_collector.anchor
test__sorting_and_selection.anchor: cons_pairs.anchor
test__sorting_and_selection.anchor: vectars.anchor
test__sorting_and_selection.anchor: sorting_and_selection.anchor
test__sorting_and_selection.anchor: test__sorting_and_selection.mod
test__sorting_and_selection.mod:

example__knights_tour.anchor: garbage_collector.anchor
example__knights_tour.anchor: cons_pairs.anchor
example__knights_tour.anchor: lsets.anchor
example__knights_tour.anchor: vectars.anchor
example__knights_tour.anchor: sorting_and_selection.anchor
example__knights_tour.anchor: example__knights_tour.mod
example__knights_tour.mod:

example__n_queens.anchor: garbage_collector.anchor
example__n_queens.anchor: cons_pairs.anchor
example__n_queens.anchor: example__n_queens.mod
example__n_queens.mod:

suffixed-all-basenames-but-examples = $(addsuffix $(shell printf "%s" $(1)), \
	$(MODULE_BASENAMES) $(TEST_PROGRAM_BASENAMES))

suffixed-all-basenames = $(addsuffix $(shell printf "%s" $(1)), \
	$(MODULE_BASENAMES) $(TEST_PROGRAM_BASENAMES) $(EXAMPLE_PROGRAM_BASENAMES))

.PHONY: coverage
coverage:
	lcov --capture --directory . --output-file coverage.info
	genhtml coverage.info --output-directory coverage-html

.PHONY: coverage-clean
coverage-clean:
	-rm -f *.gcda *.gcno coverage.info
	-rm -f -R coverage-html

.PHONY: clean maintainer-clean
clean: coverage-clean
	-rm -f $(TEST_PROGRAM_BASENAMES)
	-rm -f $(EXAMPLE_PROGRAM_BASENAMES)
	-rm -f *.mod
	-rm -f $(call suffixed-all-basenames, .$(OBJEXT))
	-rm -f $(call suffixed-all-basenames, .anchor)
maintainer-clean: clean
	-rm -f $(call suffixed-all-basenames-but-examples, .f90)
