.DELETE_ON_ERROR:

# At least Fortran 2008 is needed.
FORTRAN_STANDARD = gnu
#FORTRAN_STANDARD = f2018
#FORTRAN_STANDARD = f2008

M4 = m4
ifeq ($(FORTRAN_STANDARD),gnu)
M4FLAGS = -DCALL_ABORT="call abort"
else
M4FLAGS = -DCALL_ABORT="!call abort"
endif
M4FLAGS += -DCADADR_MAX=4

FC = gfortran
FCFLAGS = -std=$(FORTRAN_STANDARD) -g -fcheck=all

%.o: %.f90
	$(FC) $(FCFLAGS) $(XFCFLAGS) -c $(<) -o $(@)

%: %.m4 common-macros.m4
	$(M4) $(M4FLAGS) $(XM4FLAGS) $(<) > $(@)

.PHONY: all default
default: all
all : cons_lists_demo

cons_lists_demo: cons_lists_demo.o cons_lists.o
	$(FC) $(FCFLAGS) $(^) -o $(@)

cons_lists.f90: cadadr.m4 GNUmakefile
cons_lists_demo.o: cons_lists.o

.PHONY: clean veryclean
clean:
	-rm -f cons_lists_demo
	-rm -f *.mod
	-rm -f *.o
veryclean: clean
	-rm -f cons_lists.f90
