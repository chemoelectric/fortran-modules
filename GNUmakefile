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

.PHONY: clean veryclean
clean:
	-rm -f test__cons_lists
	-rm -f *.mod
	-rm -f *.$(OBJEXT)
	-rm -f *.anchor
veryclean: clean
	-rm -f cons_lists.f90
