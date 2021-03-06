! -*- F90 -*- include(`common-macros.m4')
!
! Copyright 2022 Barry Schwartz
!
! Permission is hereby granted, free of charge, to any person
! obtaining a copy of this software and associated documentation files
! (the "Software"), to deal in the Software without restriction,
! including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software,
! and to permit persons to whom the Software is furnished to do so,
! subject to the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
! BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
! ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!

dnl
dnl
dnl I have tried to keep this file compatible with "heirloom" m4
dnl implementations (for example, by using ASCII), and also compatible
dnl with the POSIX specification for m4.
dnl
dnl However, with an "heirloom" m4 you might have to increase buffer
dnl size with the -B option.
dnl
dnl
divert(-1)

m4_define([m4_declare_iteration_variables],[dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: vec[]k[]_root
])dnl
m4_forloop([_k],[1],$1,[dnl
    type(vectar_range_t) :: vecr[]_k
])dnl
m4_forloop([_k],[1],$1,[dnl
    class(vectar_data_t), pointer :: data[]_k
])dnl
    integer(sz) :: min_length
])

m4_define([m4_initialize_iteration],[dnl
m4_forloop([_k],[1],$1,[dnl
    vec[]_k[]_root = vec[]_k
])dnl
m4_forloop([_k],[1],$1,[dnl
    vecr[]_k = vec[]_k
])dnl
m4_set_min_length($1)dnl
m4_forloop([_k],[1],$1,[dnl
    data[]_k => vectar_data_ptr (vecr[]_k)
])dnl
])

m4_define([m4_set_min_length],[dnl
m4_if($1,[1],[dnl
    min_length = vecr1%length()
],[dnl
    min_length = min (vecr1%length()[]m4_forloop([_k],[2],$1,[, m4_if(m4_eval(_k % 3),[1],[&
         &            ])vecr[]_k%length()]))
])dnl
])

dnl
dnl  m4_discard_vec_roots(n) might help ensure that garbage collection
dnl  mutator roots are not discarded by an optimizer.
dnl
m4_define([m4_discard_vec_roots],[dnl
m4_forloop([_k],[1],$1,[dnl
    call vec[]_k[]_root%discard
])dnl
])

divert[]dnl

module vectars
  !
  ! Vectors (single-dimensional arrays) in the fashion of Scheme
  ! Request for Implementation 133 (SRFI-133).
  ! https://srfi.schemers.org/srfi-133/srfi-133.html
  !
  ! An important difference from SRFI-133 is that we use `ranges' --
  ! with inclusive end index -- in place of optional `start' and `end'
  ! parameters (with exclusive end index). Inclusive end indices are
  ! more Fortranish; also, this approach reduces the number of
  ! different implementations of procedures needed. (FUTURE PROJECT:
  ! objects with Icon-style indexing.)
  !
  ! The name `vectar' (short for `vector array') is used instead of
  ! `vector', to avoid confusion with Gibbs vectors.
  !
  ! Conversions between vectars and strings are not included in this
  ! module, despite that they are included in SRFI-133.
  !

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none
  private

  ! The type for a vectar.
  public :: vectar_t

  !
  ! vectar_range_t: the type for a `range' of a vectar.
  !
  ! Vectar ranges exist to solve the problem of how to handle the
  ! `[start [end]]' parameters of SRFI-133, without multiplying the
  ! implementations of generic functions.
  !
  ! They are not meant to take the place of `slices', `views', etc.,
  ! although they do let many procedures effectively have `[start
  ! [end]]' parameters that they do not have in SRFI-133.
  !
  public :: vectar_range_t

  ! Tests for the vectar_t type.
  public :: is_vectar          ! Is the given object a vectar_t?
  public :: is_not_vectar      ! Is the given object *not* a vectar_t?

  ! Do two objects refer to the same vectar in the garbage collector's
  ! heap?
  public :: vectar_t_eq

  ! Convert an object to a vectar, if possible.
  public :: vectar_t_cast
  public :: operator(.tovectar.)

  ! Convert an object to a vectar range, if possible.
  public :: vectar_range_t_cast
  public :: operator(.tovecrange.)

  ! Generic function: make a vectar from elements passed as arguments.
  public :: vectar

  ! Implementations of vectar.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: vectar[]n
])dnl

  ! Generic function: make a vectar of one value repeated (or with
  ! unspecified values).
  public :: make_vectar

  ! Implementations of make_vectar.
  public :: make_vectar_unspecified_fill_size_kind
  public :: make_vectar_unspecified_fill_int
  public :: make_vectar_fill_size_kind
  public :: make_vectar_fill_int

  ! Copy all or part of a vectar, to a new vectar.
  public :: vectar_copy

  ! Copy all or part of a vectar, to a new vectar, but with the order
  ! of elements reversed.
  public :: vectar_reverse_copy

  ! Generic function: return a new vector, appending the contents of
  ! vectars or vectar ranges to each other. This function does both
  ! what `vector-append' and `vector-append-subvectors' do in
  ! SRFI-133.
  public :: vectar_append

  ! Implementations of vectar_append.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: vectar_append[]n
])dnl

  ! Return a new vector, appending the contents of the vectars and
  ! vectar ranges given by a list. (In SRFI-133, `vector-concatenate'
  ! cannot append subvectors, the way our function here can.)
  public :: vectar_concatenate

  ! Return the length of a vectar as an INTEGER([SIZE_KIND]); if the
  ! vectar is a vectar_range_t, the length of the range is returned,
  ! instead.
  public :: vectar_length

  ! Is a vectar empty? That is, is its length equal to zero? If the
  ! vectar is a vectar_range_t, the length of the range is queried,
  ! instead.
  public :: vectar_is_empty

  ! Generic functions: return a vectar element, or an element from a
  ! vectar range.
  public :: vectar_ref0         ! Indices run 0, 1, 2, ...
  public :: vectar_ref1         ! Indices run 1, 2, 3, ...
  public :: vectar_refn         ! Indices run n, n+1, n+2, ...

  ! Implementations of the vectar_refX functions.
  public :: vectar_ref0_size_kind
  public :: vectar_ref1_size_kind
  public :: vectar_refn_size_kind
  public :: vectar_ref0_int
  public :: vectar_ref1_int
  public :: vectar_refn_int

  ! Generic subroutines: set a vectar element, or an element of a
  ! vectar range.
  public :: vectar_set0         ! Indices run 0, 1, 2, ...
  public :: vectar_set1         ! Indices run 1, 2, 3, ...
  public :: vectar_setn         ! Indices run n, n+1, n+2, ...

  ! Implementations of the vectar_setX subroutines.
  public :: vectar_set0_size_kind
  public :: vectar_set1_size_kind
  public :: vectar_setn_size_kind
  public :: vectar_set0_int
  public :: vectar_set1_int
  public :: vectar_setn_int

  ! Generic subroutines: swap vectar elements, or elements of a vectar
  ! range.
  public :: vectar_swap0        ! Indices run 0, 1, 2, ...
  public :: vectar_swap1        ! Indices run 1, 2, 3, ...
  public :: vectar_swapn        ! Indices run n, n+1, n+2, ...

  ! Implementations of the vectar_swapX subroutines.
  public :: vectar_swap0_size_kind
  public :: vectar_swap1_size_kind
  public :: vectar_swapn_size_kind
  public :: vectar_swap0_int
  public :: vectar_swap1_int
  public :: vectar_swapn_int

  ! Some subroutines that (like the vectar sets and vectar swaps)
  ! alter the contents of a vectar. These accept vectar ranges, as
  ! well. (The `x' in the names is to remind us that the corresponding
  ! SRFI-133 procedures have a `!' in their names. We have not used
  ! `x' in the names of vectar set and swap subroutines, however.)
  public :: vectar_fillx        ! Fill with a repeated value.
  public :: vectar_reversex     ! Reversal in place.
  public :: vectar_copyx0       ! Generic function: copy data,
                                ! zero-based indexing. Source and
                                ! destination are allowed to overlap.
  public :: vectar_copyx1       ! Generic function: copy data,
                                ! one-based indexing. Source and
                                ! destination are allowed to overlap.
  public :: vectar_copyxn       ! Generic function: copy data,
                                ! n-based indexing. Source and
                                ! destination are allowed to overlap.
  public :: vectar_reverse_copyx0 ! Generic function: copy data,
                                  ! zero-based indexing. Source and
                                  ! destination are allowed to
                                  ! overlap.
  public :: vectar_reverse_copyx1 ! Generic function: copy data,
                                  ! one-based indexing. Source and
                                  ! destination are allowed to
                                  ! overlap.
  public :: vectar_reverse_copyxn ! Generic function: copy data,
                                  ! n-based indexing. Source and
                                  ! destination are allowed to
                                  ! overlap.

  ! Implementations of vectar_copyx0.
  public :: vectar_copyx0_size_kind
  public :: vectar_copyx0_int

  ! Implementations of vectar_copyx1.
  public :: vectar_copyx1_size_kind
  public :: vectar_copyx1_int

  ! Implementations of vectar_copyxn.
  public :: vectar_copyxn_size_kind
  public :: vectar_copyxn_int

  ! Implementations of vectar_reverse_copyx0.
  public :: vectar_reverse_copyx0_size_kind
  public :: vectar_reverse_copyx0_int

  ! Implementations of vectar_reverse_copyx1.
  public :: vectar_reverse_copyx1_size_kind
  public :: vectar_reverse_copyx1_int

  ! Implementations of vectar_reverse_copyxn.
  public :: vectar_reverse_copyxn_size_kind
  public :: vectar_reverse_copyxn_int

  ! Vector equality. These accept vectar ranges and so are, in that
  ! respect, more general than their SRFI-133 equivalents.
  public :: vectar_equal        ! A generic function.

  ! Implementations of vectar_equal.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: vectar_equal[]n
])dnl

  ! Generic functions for per-element mapping. These accept vectar
  ! ranges and so are, in that respect, more general than their
  ! SRFI-133 equivalents.
  public :: vectar_map          ! Create a new vectar with mapped
                                ! values.
  public :: vectar_mapx         ! Map the elements in place.

  ! Implementations of vectar_map.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_map[]n[]_subr
])dnl

  ! Implementations of vectar_mapx.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_mapx[]n[]_subr
])dnl

  ! Generic side effects iterator. This accepts vectar ranges.
  public :: vectar_for_each

  ! Implementations of vectar_for_each.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_for_each[]n[]_subr
])dnl

  ! Generic function. See SRFI-133. This accepts vectar ranges.
  public :: vectar_cumulate

  ! Implementations of vectar_cumulate.
  public :: vectar_cumulate_subr

  ! Generic function: count how many times the predicate is
  ! satisfied. This accepts vectar ranges.
  public :: vectar_count

  ! Implementations of vectar_count.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_count[]n
])dnl

  ! Generic function: left-to-right fold. This accepts vectar ranges.
  public :: vectar_fold

  ! Implementations of vectar_fold.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_fold[]n[]_subr
])dnl

  ! Generic function: right-to-left fold. This accepts vectar ranges.
  public :: vectar_fold_right

  ! Implementations of vectar_fold_right.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_fold_right[]n[]_subr
])dnl

  ! Generic subroutine: create a new vectar by unfolding
  ! left-to-right.
  public :: vectar_unfold

  ! Implementations of vectar_unfold.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: vectar_unfold[]n[]_subr_size_kind
  public :: vectar_unfold[]n[]_subr_int
])dnl

  ! Generic subroutine: unfold left-to-right into an existing vectar
  ! or vectar range.
  public :: vectar_unfoldx

  ! Generic subroutine: create a new vectar by unfolding
  ! right-to-left.
  public :: vectar_unfold_right

  ! Implementations of vectar_unfold.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: vectar_unfold_right[]n[]_subr_size_kind
  public :: vectar_unfold_right[]n[]_subr_int
])dnl

  ! Generic subroutine: unfold right-to-left into an existing vectar
  ! or vectar range.
  public :: vectar_unfold_rightx

  ! Implementations of vectar_unfoldx.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: vectar_unfold_rightx[]n[]_subr
])dnl

  !
  ! Generic functions for finding the first index at which a predicate
  ! is satisfied for the elements of vectars or vectar ranges. These
  ! functions are designed so they always return a negative number on
  ! failure to satisfy the predicate, and also so the negative number
  ! is specifically -1, if the index base is non-negative.  This
  ! seemed a convenient convention.
  !
  ! Note that `vector-index' in SRFI-133 returns #f instead of an
  ! integer.
  !
  public :: vectar_index0     ! Return the 0-based index where a
                              ! predicate is first satisfied, or -1 if
                              ! it is never satisfied.
  public :: vectar_index1     ! Return the 1-based index where a
                              ! predicate is first satisfied, or -1 if
                              ! it is never satisfied.
  public :: vectar_indexn     ! Return the n-based index where a
                              ! predicate is first satisfied, or min
                              ! (-1, n - 1) if it is never satisfied.

  ! Implementations of the vectar index functions.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_index0_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_index1_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_indexn_[]n
])dnl

  !
  ! Generic functions: right-to-left analogs of the vectar_index0,
  ! vectar_index1, and vectar_indexn functions.
  !
  ! SRFI-133 requires (in conforming code) that the vectors passed to
  ! vector-index-right all have the same length. We relax that
  ! requirement and, instead, say that the search begins at each
  ! range's start index, plus the minimum range length, minus one.
  !
  public :: vectar_index_right0 ! Return the 0-based index where a
                                ! predicate is first satisfied, or -1
                                ! if it is never satisfied.
  public :: vectar_index_right1 ! Return the 1-based index where a
                                ! predicate is first satisfied, or -1
                                ! if it is never satisfied.
  public :: vectar_index_rightn ! Return the n-based index where a
                                ! predicate is first satisfied, or min
                                ! (-1, n - 1) if it is never
                                ! satisfied.

  ! Implementations of the right-to-left vectar index functions.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_index_right0_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_index_right1_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_index_rightn_[]n
])dnl

  ! Generic functions: equivalents to the corresponding
  ! `vectar_index...' functions, except that they search for the first
  ! time the predicate is *not* satisfied.
  public :: vectar_skip0
  public :: vectar_skip1
  public :: vectar_skipn
  public :: vectar_skip_right0
  public :: vectar_skip_right1
  public :: vectar_skip_rightn

  ! Implementations of the `vectar_skip...' functions.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_skip0_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_skip1_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_skipn_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_skip_right0_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_skip_right1_[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_skip_rightn_[]n
])dnl

  !
  ! Functions to do binary searches on vectars and vectar ranges.
  !
  ! These functions are designed so they always return a negative
  ! number on failure to satisfy the predicate, and also so the
  ! negative number is specifically -1, if the index base is
  ! non-negative. This seemed a convenient convention.
  !
  ! (Note that the `cmp' procedure argument comes last. This is the
  ! argument order specified in SRFI-133.)
  !
  public :: vectar_binary_search0 ! Return the 0-based index of a
                                  ! match, or -1 if there is no match.
  public :: vectar_binary_search1 ! Return the 1-based index of a
                                  ! match, or -1 if there is no match.
  public :: vectar_binary_searchn ! Return the n-based index of a
                                  ! match, or min (-1, n - 1) if there
                                  ! is no match.

  ! Vectar versions of the cons_pairs module's `some', `some_map,
  ! `every', and `every_map' functions.
  public :: vectar_some      ! Generic function: applies a predicate
                             ! across lists, returning .true. if the
                             ! predicate returns .true. on any
                             ! application.
  public :: vectar_some_map  ! Generic function: applies a mapping
                             ! procedure across lists, returning the
                             ! result of the mapping the first time it
                             ! comes out as a value other than .false.
  public :: vectar_every     ! Generic function: applies a predicate
                             ! across lists, returning .true. if the
                             ! predicate returns .true. on every
                             ! application.
  public :: vectar_every_map ! Generic function: applies a mapping
                             ! procedure across lists, returning the
                             ! result of the last mapping, if no
                             ! application of the procedure returns
                             ! .false.

  ! Implementations of `vectar_some' et al.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_some[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_some_map[]n[]_subr
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_every[]n
])dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_every_map[]n[]_subr
])dnl

  !
  ! Partitioning. See SRFI-133, `vector-partition'.
  !
  public :: do_vectar_partition ! Subroutine.
  public :: vectar_partition    ! Function returning a length-2 list.

  ! Vectar-list conversions.
  public :: vectar_to_list
  public :: reverse_vectar_to_list
  public :: list_to_vectar
  public :: reverse_list_to_vectar

  ! Type-unbound generic versions of the `range' type-bound
  ! procedures.
  public :: vectar_range0
  public :: vectar_range1
  public :: vectar_rangen
  public :: range0              ! Synonym for vectar_range0.
  public :: range1              ! Synonym for vectar_range1.
  public :: rangen              ! Synonym for vectar_rangen.

  ! Implementations of the `range' functions.
  public :: vectar_range0_size_kind
  public :: vectar_range0_int
  public :: vectar_range1_size_kind
  public :: vectar_range1_int
  public :: vectar_rangen_size_kind
  public :: vectar_rangen_int

!!!-------------------------------------------------------------------
!!!
!!! NOT FOR GENERAL PROGRAMMING.
!!!

  !
  ! For use in writing extensions to this module. Not recommended for
  ! general programming.
  !
  public :: vectar_data_ptr
  public :: vectar_data_t
  public :: vectar_element_t

!!!-------------------------------------------------------------------

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

  ! Private conversion to `size_kind'.
  interface operator(.sz.)
     module procedure int2sz
  end interface operator(.sz.)

  type :: vectar_element_t
     class(*), allocatable :: element
  end type vectar_element_t

  type :: vectar_data_t
     integer(sz) :: length
     type(vectar_element_t), allocatable :: array(:) ! Zero-based.
  end type vectar_data_t

  type, extends (collectible_t) :: vectar_t
   contains
     procedure, pass :: get_branch => vectar_t_get_branch
     procedure, pass :: assign => vectar_t_assign
     generic :: assignment(=) => assign

     procedure, pass :: range0_size_kind => vectar_t_range0_size_kind
     procedure, pass :: range0_int => vectar_t_range0_int
     generic :: range0 => range0_size_kind
     generic :: range0 => range0_int

     procedure, pass :: range1_size_kind => vectar_t_range1_size_kind
     procedure, pass :: range1_int => vectar_t_range1_int
     generic :: range1 => range1_size_kind
     generic :: range1 => range1_int

     procedure, pass :: rangen_size_kind => vectar_t_rangen_size_kind
     procedure, pass :: rangen_int => vectar_t_rangen_int
     generic :: rangen => rangen_size_kind
     generic :: rangen => rangen_int
  end type vectar_t

  type, extends (vectar_t) :: vectar_range_t
     integer(sz), private :: index_
     integer(sz), private :: length_
   contains
     procedure, pass :: assign => vectar_range_t_assign

     procedure, pass :: vec => vectar_range_t_vec ! Cast to vectar_t.

     procedure, pass :: istart0 => vectar_range_t_istart0
     procedure, pass :: iend0 => vectar_range_t_iend0

     procedure, pass :: istart1 => vectar_range_t_istart1
     procedure, pass :: iend1 => vectar_range_t_iend1

     procedure, pass :: istartn_size_kind => vectar_range_t_istartn_size_kind
     procedure, pass :: istartn_int => vectar_range_t_istartn_int
     generic :: istartn => istartn_size_kind
     generic :: istartn => istartn_int

     procedure, pass :: iendn_size_kind => vectar_range_t_iendn_size_kind
     procedure, pass :: iendn_int => vectar_range_t_iendn_int
     generic :: iendn => iendn_size_kind
     generic :: iendn => iendn_int

     procedure, pass :: length => vectar_range_t_length
  end type vectar_range_t

  interface operator(.tovectar.)
     module procedure vectar_t_cast
  end interface operator(.tovectar.)

  interface operator(.tovecrange.)
     module procedure vectar_range_t_cast
  end interface operator(.tovecrange.)

!!!-------------------------------------------------------------------

  interface vectar_range0
     module procedure vectar_range0_size_kind
     module procedure vectar_range0_int
  end interface vectar_range0

  interface range0
     module procedure vectar_range0_size_kind
     module procedure vectar_range0_int
  end interface range0

  interface vectar_range1
     module procedure vectar_range1_size_kind
     module procedure vectar_range1_int
  end interface vectar_range1

  interface range1
     module procedure vectar_range1_size_kind
     module procedure vectar_range1_int
  end interface range1

  interface vectar_rangen
     module procedure vectar_rangen_size_kind
     module procedure vectar_rangen_int
  end interface vectar_rangen

  interface rangen
     module procedure vectar_rangen_size_kind
     module procedure vectar_rangen_int
  end interface rangen

  interface vectar
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure vectar[]n
])dnl
  end interface vectar

  interface make_vectar
     module procedure make_vectar_unspecified_fill_size_kind
     module procedure make_vectar_unspecified_fill_int
     module procedure make_vectar_fill_size_kind
     module procedure make_vectar_fill_int
  end interface make_vectar

  interface vectar_ref0
     module procedure vectar_ref0_size_kind
     module procedure vectar_ref0_int
  end interface vectar_ref0

  interface vectar_ref1
     module procedure vectar_ref1_size_kind
     module procedure vectar_ref1_int
  end interface vectar_ref1

  interface vectar_refn
     module procedure vectar_refn_size_kind
     module procedure vectar_refn_int
  end interface vectar_refn

  interface vectar_set0
     module procedure vectar_set0_size_kind
     module procedure vectar_set0_int
  end interface vectar_set0

  interface vectar_set1
     module procedure vectar_set1_size_kind
     module procedure vectar_set1_int
  end interface vectar_set1

  interface vectar_setn
     module procedure vectar_setn_size_kind
     module procedure vectar_setn_int
  end interface vectar_setn

  interface vectar_swap0
     module procedure vectar_swap0_size_kind
     module procedure vectar_swap0_int
  end interface vectar_swap0

  interface vectar_swap1
     module procedure vectar_swap1_size_kind
     module procedure vectar_swap1_int
  end interface vectar_swap1

  interface vectar_swapn
     module procedure vectar_swapn_size_kind
     module procedure vectar_swapn_int
  end interface vectar_swapn

  interface vectar_copyx0
     module procedure vectar_copyx0_size_kind
     module procedure vectar_copyx0_int
  end interface vectar_copyx0

  interface vectar_copyx1
     module procedure vectar_copyx1_size_kind
     module procedure vectar_copyx1_int
  end interface vectar_copyx1

  interface vectar_copyxn
     module procedure vectar_copyxn_size_kind
     module procedure vectar_copyxn_int
  end interface vectar_copyxn

  interface vectar_reverse_copyx0
     module procedure vectar_reverse_copyx0_size_kind
     module procedure vectar_reverse_copyx0_int
  end interface vectar_reverse_copyx0

  interface vectar_reverse_copyx1
     module procedure vectar_reverse_copyx1_size_kind
     module procedure vectar_reverse_copyx1_int
  end interface vectar_reverse_copyx1

  interface vectar_reverse_copyxn
     module procedure vectar_reverse_copyxn_size_kind
     module procedure vectar_reverse_copyxn_int
  end interface vectar_reverse_copyxn

  interface vectar_append
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure vectar_append[]n
])dnl
  end interface vectar_append

  interface vectar_map
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_map[]n[]_subr
])dnl
  end interface vectar_map

  interface vectar_mapx
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_mapx[]n[]_subr
])dnl
  end interface vectar_mapx

  interface vectar_for_each
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_for_each[]n[]_subr
])dnl
  end interface vectar_for_each

  interface vectar_cumulate
     module procedure vectar_cumulate_subr
  end interface vectar_cumulate

  interface vectar_count
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_count[]n
])dnl
  end interface vectar_count

  interface vectar_fold
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_fold[]n[]_subr
])dnl
  end interface vectar_fold

  interface vectar_fold_right
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_fold_right[]n[]_subr
])dnl
  end interface vectar_fold_right

  interface vectar_unfold
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure vectar_unfold[]n[]_subr_size_kind
     module procedure vectar_unfold[]n[]_subr_int
])dnl
  end interface vectar_unfold

  interface vectar_unfoldx
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure vectar_unfoldx[]n[]_subr
])dnl
  end interface vectar_unfoldx

  interface vectar_unfold_right
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure vectar_unfold_right[]n[]_subr_size_kind
     module procedure vectar_unfold_right[]n[]_subr_int
])dnl
  end interface vectar_unfold_right

  interface vectar_unfold_rightx
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure vectar_unfold_rightx[]n[]_subr
])dnl
  end interface vectar_unfold_rightx

  interface vectar_equal
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure vectar_equal[]n
])dnl
  end interface vectar_equal

  interface vectar_index0
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_index0_[]n
])dnl
  end interface vectar_index0

  interface vectar_index1
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_index1_[]n
])dnl
  end interface vectar_index1

  interface vectar_indexn
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_indexn_[]n
])dnl
  end interface vectar_indexn

  interface vectar_index_right0
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_index_right0_[]n
])dnl
  end interface vectar_index_right0

  interface vectar_index_right1
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_index_right1_[]n
])dnl
  end interface vectar_index_right1

  interface vectar_index_rightn
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_index_rightn_[]n
])dnl
  end interface vectar_index_rightn

  interface vectar_skip0
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_skip0_[]n
])dnl
  end interface vectar_skip0

  interface vectar_skip1
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_skip1_[]n
])dnl
  end interface vectar_skip1

  interface vectar_skipn
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_skipn_[]n
])dnl
  end interface vectar_skipn

  interface vectar_skip_right0
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_skip_right0_[]n
])dnl
  end interface vectar_skip_right0

  interface vectar_skip_right1
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_skip_right1_[]n
])dnl
  end interface vectar_skip_right1

  interface vectar_skip_rightn
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_skip_rightn_[]n
])dnl
  end interface vectar_skip_rightn
  
  interface vectar_some
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_some[]n
])dnl
  end interface vectar_some

  interface vectar_some_map
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_some_map[]n[]_subr
])dnl
  end interface vectar_some_map

  interface vectar_every
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_every[]n
])dnl
  end interface vectar_every

  interface vectar_every_map
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure vectar_every_map[]n[]_subr
])dnl
  end interface vectar_every_map

!!!-------------------------------------------------------------------
!!!
!!! Types for predicates.
!!!

  public :: vectar_predicate1_t ! A predicate taking 1 argument.
m4_forloop([n],[2],ZIP_MAX,[dnl
  public :: vectar_predicate[]n[]_t ! A predicate taking n arguments.
])dnl

  abstract interface
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive function vectar_predicate[]n[]_t (x1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          &                                  ])x[]k])) result (bool)
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: [x]k
])dnl
       logical :: bool
     end function vectar_predicate[]n[]_t
])dnl
  end interface

!!!-------------------------------------------------------------------
!!!
!!! The type of a function resembling C's `strcmp'; for use in
!!! SRFI-133-style binary searches.
!!!
!!! Only the sign and zeroness of the result are significant.
!!!

  public :: vectar_cmp_func_t

  abstract interface
     recursive function vectar_cmp_func_t (x1, x2) result (sign)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       integer :: sign
     end function vectar_cmp_func_t
  end interface

!!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!
!!! Types for the per-element-mapping argument to a map procedure.
!!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_map[]n[]_subr_t
])dnl

  abstract interface
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive subroutine vectar_map[]n[]_subr_t (input1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          &                                   ])input[]k]), output)
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: input[]k
])dnl
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map[]n[]_subr_t
])dnl
  end interface

!!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!
!!! Types for the side-effects subroutine to a for-each procedure.
!!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_side_effects[]n[]_t
])dnl

  abstract interface
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive subroutine vectar_side_effects[]n[]_t (input1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          &                                       ])input[]k]))
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: input[]k
])dnl
     end subroutine vectar_side_effects[]n[]_t
])dnl
  end interface

!!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!
!!! Types for the kons subroutine in folds, vectar_cumulate, etc.
!!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: vectar_kons[]n[]_subr_t
])dnl

  abstract interface
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive subroutine vectar_kons[]n[]_subr_t (state, val1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          &                                    ])val[]k]), kons_result)
       class(*), intent(in) :: state
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: val[]k
])dnl
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons[]n[]_subr_t
])dnl
  end interface

!!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!
!!! Types for the f subroutine in unfolds.
!!!

m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: vectar_unfold[]n[]_f_subr_t
])dnl

  abstract interface
     recursive subroutine vectar_unfold0_f_subr_t (index, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold0_f_subr_t
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive subroutine vectar_unfold[]n[]_f_subr_t (index, seed1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          &                                        ])seed[]k]), element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
m4_forloop([k],[1],n,[dnl
       class(*), allocatable, intent(inout) :: seed[]k
])dnl
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold[]n[]_f_subr_t
])dnl
  end interface

!!!-------------------------------------------------------------------

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

!!!-------------------------------------------------------------------

contains

!!!-------------------------------------------------------------------

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module vectars error: ", a)') msg
    error stop
  end subroutine error_abort_1

  subroutine strange_error
    call error_abort ("a strange error, possibly use of an object already garbage-collected")
  end subroutine strange_error

  elemental function unspecified () result (unspecified_value)
    use, intrinsic :: ieee_arithmetic, only: ieee_value
    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan
    real :: unspecified_value

    unspecified_value = ieee_value (1.0, ieee_quiet_nan)
  end function unspecified

  elemental function int2sz (i) result (j)
    integer, intent(in) :: i
    integer(sz) :: j

    j = i
  end function int2sz

  function vectar_data_ptr (vec) result (data_ptr)
    class(*), intent(in) :: vec
    class(vectar_data_t), pointer :: data_ptr

    select type (v => .autoval. vec)
    class is (vectar_t)
       if (associated (v%heap_element)) then
          select type (data => v%heap_element%data)
          class is (vectar_data_t)
             data_ptr => data
          class default
             call strange_error
          end select
       else
          call error_abort ("vectar_t not properly allocated")
       end if
    class default
       call error_abort ("expected a vectar_t")
    end select
  end function vectar_data_ptr

!!!-------------------------------------------------------------------

  subroutine vectar_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(vectar_t), intent(in) :: this
    integer(sz), intent(in) :: branch_number
    logical, intent(out) :: branch_number_out_of_range
    class(*), allocatable, intent(out) :: branch

    class(*), pointer :: data

    branch_number_out_of_range = .true.
    if (associated (this%heap_element)) then
       if (0_sz <= branch_number) then
          data => this%heap_element%data
          select type (data)
          class is (vectar_data_t)
             if (branch_number - 1_sz <= ubound (data%array, 1, sz)) then
                branch = data%array(branch_number - 1_sz)%element
                branch_number_out_of_range = .false.
             end if
          end select
       end if
    end if
  end subroutine vectar_t_get_branch

!!!-------------------------------------------------------------------

  function vectar_range0_size_kind (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (.tovectar. vec, istart, iend)
  end function vectar_range0_size_kind

  function vectar_range0_int (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_int (.tovectar. vec, istart, iend)
  end function vectar_range0_int

  function vectar_range1_size_kind (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range1_size_kind (.tovectar. vec, istart, iend)
  end function vectar_range1_size_kind

  function vectar_range1_int (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range1_int (.tovectar. vec, istart, iend)
  end function vectar_range1_int

  function vectar_rangen_size_kind (vec, n, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_rangen_size_kind (.tovectar. vec, n, istart, iend)
  end function vectar_rangen_size_kind

  function vectar_rangen_int (vec, n, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer, intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_rangen_int (.tovectar. vec, n, istart, iend)
  end function vectar_rangen_int

  function vectar_t_range0_size_kind (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    !
    ! NOTES:
    !
    !   * Any value of iend less than istart is legal, and indicates a
    !     range of length zero.
    !
    !   * If iend is less than istart, then istart may hold any value.
    !

    integer(sz) :: len

    range = vec
    range%index_ = istart

    len = vectar_length (vec)

    if (iend < istart) then
       range%length_ = 0
    else
       if (istart < 0_sz .or. len <= iend) then
          call error_abort ("vectar_t range indices are out of range")
       end if
       range%length_ = (iend - istart) + 1
    end if
  end function vectar_t_range0_size_kind

  function vectar_t_range0_int (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, .sz. istart, .sz. iend)
  end function vectar_t_range0_int

  function vectar_t_range1_size_kind (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, istart - 1, iend - 1)
  end function vectar_t_range1_size_kind

  function vectar_t_range1_int (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, (.sz. istart) - 1, (.sz. iend) - 1)
  end function vectar_t_range1_int

  function vectar_t_rangen_size_kind (vec, n, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer(sz), intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, istart - n, iend - n)
  end function vectar_t_rangen_size_kind

  function vectar_t_rangen_int (vec, n, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer, intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, (.sz. istart) - (.sz. n), (.sz. iend) - (.sz. n))
  end function vectar_t_rangen_int

  function vectar_range_t_vec (range) result (vec)
    class(vectar_range_t), intent(in) :: range
    type(vectar_t) :: vec

    vec = .tovectar. range
  end function vectar_range_t_vec

  function vectar_range_t_istart0 (range) result (istart0)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: istart0

    istart0 = range%index_
  end function vectar_range_t_istart0
     
  function vectar_range_t_iend0 (range) result (iend0)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: iend0

    iend0 = range%index_ + (range%length_ - 1)
  end function vectar_range_t_iend0

  function vectar_range_t_istart1 (range) result (istart1)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: istart1

    istart1 = range%index_ + 1
  end function vectar_range_t_istart1
     
  function vectar_range_t_iend1 (range) result (iend1)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: iend1

    iend1 = range%index_ + range%length_
  end function vectar_range_t_iend1

  function vectar_range_t_istartn_size_kind (range, n) result (istartn)
    class(vectar_range_t), intent(in) :: range
    integer(sz), intent(in) :: n
    integer(sz) :: istartn

    istartn = range%index_ + n
  end function vectar_range_t_istartn_size_kind
     
  function vectar_range_t_iendn_size_kind (range, n) result (iendn)
    class(vectar_range_t), intent(in) :: range
    integer(sz), intent(in) :: n
    integer(sz) :: iendn

    iendn = range%index_ + (range%length_ - 1) + n
  end function vectar_range_t_iendn_size_kind

  function vectar_range_t_istartn_int (range, n) result (istartn)
    class(vectar_range_t), intent(in) :: range
    integer, intent(in) :: n
    integer(sz) :: istartn

    istartn = range%index_ + (.sz. n)
  end function vectar_range_t_istartn_int
     
  function vectar_range_t_iendn_int (range, n) result (iendn)
    class(vectar_range_t), intent(in) :: range
    integer, intent(in) :: n
    integer(sz) :: iendn

    iendn = range%index_ + (range%length_ - 1) + (.sz. n)
  end function vectar_range_t_iendn_int

  function vectar_range_t_length (range) result (len)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: len

    len = range%length_
  end function vectar_range_t_length

  recursive subroutine vectar_range_t_assign (dst, src)
    class(vectar_range_t), intent(inout) :: dst
    class(*), intent(in) :: src

    class(vectar_data_t), pointer :: data

    select type (src1 => .autoval. src)
    type is (vectar_range_t)
       call vectar_t_assign (dst, .tovectar. src1)
       dst%index_ = src1%index_
       dst%length_ = src1%length_
    type is (vectar_t)
       ! Set the entire vector as the range.
       call vectar_t_assign (dst, .tovectar. src1)
       data => vectar_data_ptr (src1)
       dst%length_ = data%length
       dst%index_ = 0_sz
    class default
       call error_abort ("assignment to vectar_range_t of an incompatible object")
    end select
  end subroutine vectar_range_t_assign

  recursive function vectar_range_t_cast (obj) result (range)
    class(*), intent(in) :: obj
    type(vectar_range_t) :: range

    class(vectar_data_t), pointer :: data

    select type (src => .autoval. obj)
    type is (vectar_range_t)
       call vectar_t_assign (range, .tovectar. src)
       range%index_ = src%index_
       range%length_ = src%length_
    type is (vectar_t)
       call vectar_t_assign (range, .tovectar. src)
       data => vectar_data_ptr (src)
       range%length_ = data%length
       range%index_ = 0_sz
    class default
       call error_abort ("vectar_range_t_cast of an incompatible object")
    end select
  end function vectar_range_t_cast

!!!-------------------------------------------------------------------

  recursive subroutine vectar_t_assign (dst, src)
    class(vectar_t), intent(inout) :: dst
    class(*), intent(in) :: src

    select type (obj => .autoval. src)
    type is (vectar_t)
       dst%heap_element => obj%heap_element
    class default
       call error_abort ("assignment to vectar_t of an incompatible object")
    end select
  end subroutine vectar_t_assign

  recursive function vectar_t_cast (obj) result (vec)
    class(*), intent(in) :: obj
    type(vectar_t) :: vec

    select type (object => .autoval. obj)
    class is (vectar_t)
       vec%heap_element => object%heap_element
    class default
       call error_abort ("vectar_t_cast of an incompatible object")
    end select
  end function vectar_t_cast

  pure function is_vectar (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (vectar_t)
       bool = .true.
    class default
       bool = .false.
    end select
  end function is_vectar

  pure function is_not_vectar (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (vectar_t)
       bool = .false.
    class default
       bool = .true.
    end select
  end function is_not_vectar

  recursive function vectar_t_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    logical :: bool

    type(vectar_t) :: o1, o2

    o1 = obj1
    o2 = obj2

    if (associated (o1%heap_element)) then
       if (associated (o2%heap_element)) then
          bool = associated (o1%heap_element, o2%heap_element)
       else
          bool = .false.
       end if
    else
       bool = .not. associated (o2%heap_element)
    end if
  end function vectar_t_eq

!!!-------------------------------------------------------------------

  function vectar0 () result (vec)
    type(vectar_t) :: vec

    vec = make_vectar (0_sz)
  end function vectar0

m4_forloop([n],[1],LISTN_MAX,[dnl
  function vectar[]n (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &            ])obj[]k])) result (vec)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = m4_eval(n)_sz
    allocate (data%array(0_sz:m4_eval(n - 1)_sz))
m4_forloop([k],[1],n,[dnl
    data%array(m4_eval(k - 1)_sz) = vectar_element_t (.autoval. obj[]k)
])dnl
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar[]n

])
dnl
  function make_vectar_unspecified_fill_size_kind (size) result (vec)
    integer(sz), intent(in) :: size
    type(vectar_t) :: vec

    vec = make_vectar_fill_size_kind (size, unspecified ())
  end function make_vectar_unspecified_fill_size_kind

  function make_vectar_unspecified_fill_int (size) result (vec)
    integer, intent(in) :: size
    type(vectar_t) :: vec

    vec = make_vectar_fill_size_kind (.sz. size, unspecified ())
  end function make_vectar_unspecified_fill_int

  function make_vectar_fill_size_kind (size, fill) result (vec)
    integer(sz), intent(in) :: size
    class(*), intent(in) :: fill
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    if (size < 0_sz) then
       call error_abort ("vectar size must be at least zero")
    else
       allocate (data)
       data%length = size
       if (0_sz < size) then
          allocate (data%array(0_sz:size - 1_sz), source = vectar_element_t (.autoval. fill))
       else
          allocate (data%array(0_sz:0_sz))
       end if
       allocate (new_element)
       new_element%data => data
       call heap_insert (new_element)
       vec%heap_element => new_element
    end if
  end function make_vectar_fill_size_kind

  function make_vectar_fill_int (size, fill) result (vec)
    integer, intent(in) :: size
    class(*), intent(in) :: fill
    type(vectar_t) :: vec

    vec = make_vectar_fill_size_kind (.sz. size, fill)
  end function make_vectar_fill_int

!!!-------------------------------------------------------------------

  function vectar_length (vec) result (len)
    class(*), intent(in) :: vec
    integer(sz) :: len

    select type (v => .autoval. vec)
    type is (vectar_t)
       block
         class(vectar_data_t), pointer :: data
         data => vectar_data_ptr (v)
         len = data%length
       end block
    class is (vectar_range_t)
       len = v%length()
    class default
       call error_abort ("vectar_length of an incompatible object")
    end select
  end function vectar_length

  function vectar_is_empty (vec) result (bool)
    class(*), intent(in) :: vec
    logical :: bool

    bool = (vectar_length (vec) == 0_sz)
  end function vectar_is_empty

!!!-------------------------------------------------------------------

  function vectar_ref0_size_kind (vec, i) result (element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    class(vectar_data_t), pointer :: data

    select type (v => .autoval. vec)
    type is (vectar_t)
       data => vectar_data_ptr (v)
       if (i < 0_sz .or. data%length <= i) then
          call error_abort ("vectar_t index out of range")
       end if
       element = data%array(i)%element
    class is (vectar_range_t)
       data => vectar_data_ptr (v)
       if (i < 0_sz .or. v%length() <= i) then
          call error_abort ("vectar_range_t index out of range")
       end if
       element = data%array(v%istart0() + i)%element
    class default
       call error_abort ("vectar_ref0 of an incompatible object")
    end select
  end function vectar_ref0_size_kind

  function vectar_ref1_size_kind (vec, i) result (element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = vectar_ref0_size_kind (vec, i - 1)
  end function vectar_ref1_size_kind

  function vectar_refn_size_kind (vec, n, i) result (element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = vectar_ref0_size_kind (vec, i - n)
  end function vectar_refn_size_kind

  function vectar_ref0_int (vec, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), allocatable :: element

    element = vectar_ref0_size_kind (vec, .sz. i)
  end function vectar_ref0_int

  function vectar_ref1_int (vec, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), allocatable :: element

    element = vectar_ref1_size_kind (vec, .sz. i)
  end function vectar_ref1_int

  function vectar_refn_int (vec, n, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), allocatable :: element

    element = vectar_refn_size_kind (vec, .sz. n, .sz. i)
  end function vectar_refn_int

!!!-------------------------------------------------------------------

  subroutine vectar_set0_size_kind (vec, i, element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), intent(in) :: element

    class(vectar_data_t), pointer :: data

    select type (v => .autoval. vec)
    type is (vectar_t)
       data => vectar_data_ptr (v)
       if (i < 0_sz .or. data%length <= i) then
          call error_abort ("vectar_t index out of range")
       end if
       data%array(i)%element = element
    class is (vectar_range_t)
       data => vectar_data_ptr (v)
       if (i < 0_sz .or. v%length() <= i) then
          call error_abort ("vectar_range_t index out of range")
       end if
       data%array(v%istart0() + i)%element = element
    class default
       call error_abort ("vectar_set0 of an incompatible object")
    end select
  end subroutine vectar_set0_size_kind

  subroutine vectar_set1_size_kind (vec, i, element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), intent(in) :: element

    call vectar_set0_size_kind (vec, i - 1, element)
  end subroutine vectar_set1_size_kind

  subroutine vectar_setn_size_kind (vec, n, i, element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i
    class(*), intent(in) :: element

    call vectar_set0_size_kind (vec, i - n, element)
  end subroutine vectar_setn_size_kind

  subroutine vectar_set0_int (vec, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), intent(in) :: element

    call vectar_set0_size_kind (vec, .sz. i, element)
  end subroutine vectar_set0_int

  subroutine vectar_set1_int (vec, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), intent(in) :: element

    call vectar_set1_size_kind (vec, .sz. i, element)
  end subroutine vectar_set1_int

  subroutine vectar_setn_int (vec, n, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), intent(in) :: element

    call vectar_setn_size_kind (vec, .sz. n, .sz. i, element)
  end subroutine vectar_setn_int

!!!-------------------------------------------------------------------

  subroutine vectar_swap0_size_kind (vec, i, j)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i, j

    class(vectar_data_t), pointer :: data
    class(*), allocatable :: tmp
    integer(sz) :: i1, j1

    select type (v => .autoval. vec)
    type is (vectar_t)
       data => vectar_data_ptr (v)
       if (i < 0_sz .or. j < 0_sz .or. data%length <= min (i, j)) then
          call error_abort ("vectar_t index out of range")
       end if
       if (i /= j) then
          tmp = data%array(i)%element
          data%array(i)%element = data%array(j)%element
          data%array(j)%element = tmp
       end if
    class is (vectar_range_t)
       data => vectar_data_ptr (vec)
       if (i < 0_sz .or. j < 0_sz .or. v%length() <= min (i, j)) then
          call error_abort ("vectar_range_t index out of range")
       end if
       if (i /= j) then
          i1 = v%istart0() + i
          j1 = v%istart0() + j
          tmp = data%array(i1)%element
          data%array(i1)%element = data%array(j1)%element
          data%array(j1)%element = tmp
       end if
    class default
       call error_abort ("vectar_swap0 of an incompatible object")
    end select
  end subroutine vectar_swap0_size_kind

  subroutine vectar_swap1_size_kind (vec, i, j)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i, j

    call vectar_swap0_size_kind (vec, i - 1, j - 1)
  end subroutine vectar_swap1_size_kind

  subroutine vectar_swapn_size_kind (vec, n, i, j)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i, j

    call vectar_swap0_size_kind (vec, i - n, j - n)
  end subroutine vectar_swapn_size_kind

  subroutine vectar_swap0_int (vec, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: i, j

    call vectar_swap0_size_kind (vec, .sz. i, .sz. j)
  end subroutine vectar_swap0_int

  subroutine vectar_swap1_int (vec, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: i, j

    call vectar_swap1_size_kind (vec, .sz. i, .sz. j)
  end subroutine vectar_swap1_int

  subroutine vectar_swapn_int (vec, n, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i, j

    call vectar_swapn_size_kind (vec, .sz. n, .sz. i, .sz. j)
  end subroutine vectar_swapn_int

!!!-------------------------------------------------------------------

  subroutine vectar_fillx (vec, fill)
    class(*), intent(in) :: vec
    class(*), intent(in) :: fill

    type(vectar_range_t) :: range
    class(vectar_data_t), pointer :: data
    integer(sz) :: i

    range = vec
    data => vectar_data_ptr (range)
    do i = range%istart0(), range%iend0()
       data%array(i)%element = fill
    end do
  end subroutine vectar_fillx

  subroutine vectar_reversex (vec)
    class(*), intent(in) :: vec

    type(vectar_range_t) :: range
    class(vectar_data_t), pointer :: data
    integer(sz) :: i, j
    class(*), allocatable :: tmp

    range = vec
    data => vectar_data_ptr (range)
    i = range%istart0()
    j = range%iend0()
    do while (i < j)
       tmp = data%array(i)%element
       data%array(i)%element = data%array(j)%element
       data%array(j)%element = tmp
       i = i + 1
       j = j - 1
    end do
  end subroutine vectar_reversex

!!!-------------------------------------------------------------------

  subroutine vectar_copyx0_size_kind (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: i
    class(*), intent(in) :: src

    type(vectar_range_t) :: src_range
    class(vectar_data_t), pointer :: src_data, dst_data
    integer(sz) :: copy_len
    integer(sz) :: j, k

    src_range = src

    src_data => vectar_data_ptr (src_range)
    dst_data => vectar_data_ptr (dst)

    if (i < 0_sz .or. dst_data%length <= i) then
       call error_abort ("vectar_copyx0 destination index is out of range")
    end if

    copy_len = src_range%length()

    if (dst_data%length - i < copy_len) then
       call error_abort ("vectar_copyx0 destination is shorter than the source")
    end if

    if (i <= src_range%istart0()) then
       j = src_range%istart0()
       k = i
       do while (k < i + copy_len)
          dst_data%array(k)%element = src_data%array(j)%element
          j = j + 1
          k = k + 1
       end do
    else
       j = src_range%iend0()
       k = i + copy_len
       do while (i < k)
          k = k - 1
          dst_data%array(k)%element = src_data%array(j)%element
          j = j - 1
       end do
    end if
  end subroutine vectar_copyx0_size_kind

  subroutine vectar_copyx0_int (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, .sz. i, src)
  end subroutine vectar_copyx0_int

  subroutine vectar_copyx1_size_kind (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, i - 1_sz, src)
  end subroutine vectar_copyx1_size_kind

  subroutine vectar_copyx1_int (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, (.sz. i) - 1_sz, src)
  end subroutine vectar_copyx1_int

  subroutine vectar_copyxn_size_kind (dst, n, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: n
    integer(sz) :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, i - n, src)
  end subroutine vectar_copyxn_size_kind

  subroutine vectar_copyxn_int (dst, n, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: n
    integer :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, (.sz. i) - (.sz. n), src)
  end subroutine vectar_copyxn_int

!!!-------------------------------------------------------------------

  subroutine vectar_reverse_copyx0_size_kind (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: i
    class(*), intent(in) :: src

    type(vectar_range_t) :: src_range
    class(vectar_data_t), pointer :: src_data, dst_data
    integer(sz) :: n_full
    integer(sz) :: n_copy
    integer(sz) :: n_reverse

    src_range = src

    src_data => vectar_data_ptr (src_range)
    dst_data => vectar_data_ptr (dst)

    if (i < 0_sz .or. dst_data%length <= i) then
       call error_abort ("vectar_reverse_copyx0 destination index is out of range")
    end if

    n_full = src_range%length()

    if (dst_data%length - i < n_full) then
       call error_abort ("vectar_reverse_copyx0 destination is shorter than the source")
    end if

    if (.not. associated (src_data, dst_data)) then
       ! The source and destination are in different vectors and so do
       ! not overlap. No special handling is needed.
       call reverse_copy_full
    else if (i + n_full < src_range%istart0()) then
       ! The destination is entirely left of the source. No special
       ! handling is needed.
       call reverse_copy_full
    else if (src_range%iend0() < i) then
       ! The destination is entirely right of the source. No special
       ! handling is needed.
       call reverse_copy_full
    else if (i < src_range%istart0()) then
       ! There is overlap. Reverse-copy some of the data, and reverse
       ! the rest of it in place.
       n_copy = src_range%istart0() - i
       n_reverse = n_full - n_copy
       call reverse_copy (i, src_range%istart0(), n_copy)
       call reverse_in_place (src_range%istart0(), n_reverse)
    else if (i == src_range%istart0()) then
       ! The overlap is total. Simply reverse in place.
       call reverse_in_place (i, n_full)
    else
       ! There is overlap. Reverse-copy some of the data, and reverse
       ! the rest of it in place.
       n_copy = i - src_range%istart0()
       n_reverse = n_full - n_copy
       call reverse_copy (src_range%iend0() + 1_sz, src_range%istart0(), n_copy)
       call reverse_in_place (i, n_reverse)
    end if

  contains

    subroutine reverse_copy_full
      call reverse_copy (i, src_range%istart0(), n_full)
    end subroutine reverse_copy_full

    subroutine reverse_copy (idst, isrc, len)
      integer(sz), intent(in) :: idst
      integer(sz), intent(in) :: isrc
      integer(sz), intent(in) :: len

      integer(sz) :: j, k
      integer(sz) :: count

      j = isrc + len - 1_sz
      k = idst
      do count = 1, len
         dst_data%array(k)%element = src_data%array(j)%element
         j = j - 1
         k = k + 1
      end do
    end subroutine reverse_copy

    subroutine reverse_in_place (istart, len)
      integer(sz), intent(in) :: istart
      integer(sz), intent(in) :: len

      integer(sz) :: j, k
      class(*), allocatable :: tmp

      j = istart + len - 1_sz
      k = istart
      do while (k < j)
         tmp = src_data%array(k)%element
         dst_data%array(k)%element = src_data%array(j)%element
         src_data%array(j)%element = tmp
         j = j - 1
         k = k + 1
      end do
    end subroutine reverse_in_place

  end subroutine vectar_reverse_copyx0_size_kind

  subroutine vectar_reverse_copyx0_int (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: i
    class(*), intent(in) :: src

    call vectar_reverse_copyx0_size_kind (dst, .sz. i, src)
  end subroutine vectar_reverse_copyx0_int

  subroutine vectar_reverse_copyx1_size_kind (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: i
    class(*), intent(in) :: src

    call vectar_reverse_copyx0_size_kind (dst, i - 1_sz, src)
  end subroutine vectar_reverse_copyx1_size_kind

  subroutine vectar_reverse_copyx1_int (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: i
    class(*), intent(in) :: src

    call vectar_reverse_copyx0_size_kind (dst, (.sz. i) - 1_sz, src)
  end subroutine vectar_reverse_copyx1_int

  subroutine vectar_reverse_copyxn_size_kind (dst, n, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: n
    integer(sz) :: i
    class(*), intent(in) :: src

    call vectar_reverse_copyx0_size_kind (dst, i - n, src)
  end subroutine vectar_reverse_copyxn_size_kind

  subroutine vectar_reverse_copyxn_int (dst, n, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: n
    integer :: i
    class(*), intent(in) :: src

    call vectar_reverse_copyx0_size_kind (dst, (.sz. i) - (.sz. n), src)
  end subroutine vectar_reverse_copyxn_int

!!!-------------------------------------------------------------------

  function vectar_to_list (vec) result (lst)
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    type(vectar_range_t) :: range
    class(vectar_data_t), pointer :: data
    integer(sz) :: i

    range = vec
    data => vectar_data_ptr (range)
    lst = nil
    do i = range%iend0(), range%istart0(), -1
       lst = data%array(i)%element ** lst
    end do
  end function vectar_to_list

  function reverse_vectar_to_list (vec) result (lst)
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    type(vectar_range_t) :: range
    class(vectar_data_t), pointer :: data
    integer(sz) :: i

    range = vec
    data => vectar_data_ptr (range)
    lst = nil
    do i = range%istart0(), range%iend0()
       lst = data%array(i)%element ** lst
    end do
  end function reverse_vectar_to_list

  function list_to_vectar (lst) result (vec)
    class(*), intent(in) :: lst
    type(vectar_t) :: vec

    integer(sz) :: n
    integer(sz) :: i
    type(cons_t) :: p
    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    n = length (.autoval. lst)
    allocate (data)
    data%length = n
    if (0_sz < n) then
       allocate (data%array(0_sz:(n - 1_sz)))
       i = 0
       p = .autoval. lst
       do while (is_pair (p))
          data%array(i) = vectar_element_t (car (p))
          i = i + 1
          p = cdr (p)
       end do
    else
       allocate (data%array(0_sz:0_sz))
    end if
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function list_to_vectar

  function reverse_list_to_vectar (lst) result (vec)
    class(*), intent(in) :: lst
    type(vectar_t) :: vec

    integer(sz) :: n
    integer(sz) :: i
    type(cons_t) :: p
    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    n = length (.autoval. lst)
    allocate (data)
    data%length = n
    if (0_sz < n) then
       allocate (data%array(0_sz:(n - 1_sz)))
       i = n - 1
       p = .autoval. lst
       do while (is_pair (p))
          data%array(i) = vectar_element_t (car (p))
          i = i - 1
          p = cdr (p)
       end do
    else
       allocate (data%array(0_sz:0_sz))
    end if
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function reverse_list_to_vectar

!!!-------------------------------------------------------------------

  function vectar_copy (vec) result (vec_copy)

    !
    ! DESIGN NOTE: I had considered supporting SRFI-43-style vector
    ! copy, wherein it is possible to extend the length of the
    ! destination vector. Then, however, I remembered I was using
    ! vectar_range_t instead of `start' and `end' parameters. It is
    ! not legal for the end of a vectar_range_t to extend past the end
    ! of the source vector. Therefore SRFI-43-style vector copy is not
    ! possible.
    !

    class(*), intent(in) :: vec
    type(vectar_t) :: vec_copy

    type(vectar_range_t) :: range
    class(vectar_data_t), pointer :: src, dst
    integer(sz) :: istart, iend, size, i

    range = vec

    istart = range%istart0()
    iend = range%iend0()
    size = (iend - istart) + 1_sz

    vec_copy = make_vectar (size)

    if (0_sz < size) then
       src => vectar_data_ptr (range)
       dst => vectar_data_ptr (vec_copy)

       do i = 0_sz, size - 1_sz
          dst%array(i) = src%array(istart + i)
       end do
    end if
  end function vectar_copy

  function vectar_reverse_copy (vec) result (vec_copy)
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_copy

    type(vectar_range_t) :: range
    class(vectar_data_t), pointer :: src, dst
    integer(sz) :: istart, iend, size, i

    range = vec

    istart = range%istart0()
    iend = range%iend0()

    size = (iend - istart) + 1_sz

    vec_copy = make_vectar (size)

    if (0_sz < size) then
       src => vectar_data_ptr (range)
       dst => vectar_data_ptr (vec_copy)

       do i = 0_sz, size - 1_sz
          dst%array(i) = src%array(iend - i)
       end do
    end if
  end function vectar_reverse_copy

  function vectar_append0 () result (vec_a)
    type(vectar_t) :: vec_a

    vec_a = vectar ()
  end function vectar_append0

m4_forloop([n],[1],ZIP_MAX,[dnl
  function vectar_append[]n (vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                   ])vec[]k])) result (vec_a)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    type(vectar_t) :: vec_a

m4_forloop([k],[1],n,[dnl
    type(vectar_range_t) :: vecr[]k
])dnl
m4_forloop([k],[1],n,[dnl
    class(vectar_data_t), pointer :: src[]k
])dnl
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
m4_forloop([k],[1],n,[dnl
    vecr[]k = vec[]k
    src[]k => vectar_data_ptr (vecr[]k)
    len_vec_a = len_vec_a + vecr[]k%length()
])dnl

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
m4_forloop([k],[1],n,[dnl
    do i = vecr[]k%istart0(), vecr[]k%iend0()
       dst%array(j) = src[]k%array(i)
       j = j + 1_sz
    end do
])dnl

  end function vectar_append[]n
])dnl
dnl
  function vectar_concatenate (vectars) result (vec_c)
    class(*), intent(in) :: vectars
    type(vectar_t) :: vec_c

    integer(sz) :: num_vectars

    num_vectars = length (vectars)

    select case (num_vectars)
    case (0_sz)
       vec_c = vectar ()
m4_forloop([n],[1],ZIP_MAX,[dnl
    case (n[]_sz)
       block
m4_forloop([k],[1],n,[dnl
         class(*), allocatable :: vec[]k
])dnl
         call unlist (vectars, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
              &       ])vec[]k]))
         vec_c = vectar_append (vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
              &                 ])vec[]k]))
       end block
])dnl
    case default
       vec_c = vectar_concatenate_any_length (vectars)
    end select
  end function vectar_concatenate

  function vectar_concatenate_any_length (vectars) result (vec_c)
    class(*), intent(in) :: vectars
    type(vectar_t) :: vec_c

    integer(sz) :: num_vectars
    type(cons_t) :: p
    type(vectar_range_t) :: vecr
    integer(sz) :: len_vec_c
    integer(sz) :: i, j
    class(vectar_data_t), pointer :: src
    type(vectar_data_t), pointer :: dst

    num_vectars = length (vectars)

    ! Compute the length of the result vectar.
    len_vec_c = 0_sz
    p = vectars
    do i = 1_sz, num_vectars
       vecr = car (p)
       len_vec_c = len_vec_c + vecr%length()
       p = cdr (p)
    end do

    vec_c = make_vectar (len_vec_c)

    ! Copy the data.
    vec_c = make_vectar (len_vec_c)
    dst => vectar_data_ptr (vec_c)
    j = 0;
    p = vectars
    do while (is_pair (p))
       vecr = car (p)
       src => vectar_data_ptr (vecr)
       do i = vecr%istart0(), vecr%iend0()
          dst%array(j)%element = src%array(i)%element
          j = j + 1
       end do
       p = cdr (p)
    end do
  end function vectar_concatenate_any_length

!!!-------------------------------------------------------------------

  recursive function vectar_equal0 (pred) result (bool)
    procedure(vectar_predicate2_t) :: pred
    logical :: bool

    bool = .true.
  end function vectar_equal0

  recursive function vectar_equal1 (pred, vec1) result (bool)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    logical :: bool

    type(vectar_range_t) :: vecr1

    vecr1 = vec1               ! Check the type of vec1.

    bool = .true.
  end function vectar_equal1

m4_forloop([n],[2],ZIP_MAX,[dnl
  recursive function vectar_equal[]n (equal, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                            ])vec[]k])) result (bool)
    procedure(vectar_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    logical :: bool

m4_forloop([k],[1],n,[dnl
    type(vectar_range_t) :: vecr[]k
])dnl
m4_forloop([k],[1],n,[dnl
    class(vectar_data_t), pointer :: data[]k
])dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: vec[]k[]_root
])dnl
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i0_[]k
])dnl
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_forloop([k],[1],n,[dnl
    vecr[]k = vec[]k
])dnl

m4_forloop([k],[1],n,[dnl
    data[]k => vectar_data_ptr (.val. vec[]k[]_root)
    i0_[]k = vecr[]k%istart0()
])dnl

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
m4_forloop([k],[3],n,[dnl
    else if (vecr[]k%length() /= vecr1_length) then
       bool = .false.
])dnl
    else
       bool = .true.
m4_forloop([k],[1],m4_eval(n - 1),[dnl
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data[]k%array(i0_[]k + i)%element, data[]m4_eval(k + 1)%array(i0_[]m4_eval(k + 1) + i)%element)
             i = i + 1
          end do
       end if
])dnl
    end if

m4_discard_vec_roots(n)dnl
  end function vectar_equal[]n

])dnl
dnl

!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_map[]n[]_subr (subr, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                               ])vec[]k])) result (vec_m)
    procedure(vectar_map[]n[]_subr_t) :: subr
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    type(vectar_t) :: vec_m

m4_declare_iteration_variables(n)dnl
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl
    class(*), allocatable :: result_value

m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       call subr (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
         &        ])data[]k%array(i[]k)%element]), &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

m4_discard_vec_roots(n)dnl
  end function vectar_map[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive subroutine vectar_mapx[]n[]_subr (subr, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                                  ])vec[]k]))
    procedure(vectar_map[]n[]_subr_t) :: subr
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl

m4_declare_iteration_variables(n)dnl
    class(*), allocatable :: result_element
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl

    ! Protect against garbage collections instigated by subr.
m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl

    do i = 0_sz, min_length - 1_sz
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       call subr (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
         &        ])data[]k%array(i[]k)%element]), &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

m4_discard_vec_roots(n)dnl
  end subroutine vectar_mapx[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive subroutine vectar_for_each[]n[]_subr (subr, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                                      ])vec[]k]))
    procedure(vectar_side_effects[]n[]_t) :: subr
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl

m4_declare_iteration_variables(n)dnl
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl

    ! Protect against garbage collections instigated by subr.
m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl

    do i = 0_sz, min_length - 1_sz
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       call subr (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
         &        ])data[]k%array(i[]k)%element]))
    end do

m4_discard_vec_roots(n)dnl
  end subroutine vectar_for_each[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

  recursive function vectar_cumulate_subr (subr, knil, vec) result (vec_c)
    procedure(vectar_kons1_subr_t) :: subr
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_c

    type(gcroot_t) :: vec_root
    type(gcroot_t) :: vec_c_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(vectar_data_t), pointer :: result_data
    class(*), allocatable :: seed
    class(*), allocatable :: new_seed
    integer(sz) :: i

    ! Protect against garbage collections instigated by subr.
    vec_root = vec

    vecr = vec

    vec_c_root = make_vectar (vecr%length())

    result_data => vectar_data_ptr (vec_c_root)
    data => vectar_data_ptr (vecr)

    seed = knil
    do i = 0_sz, vecr%length() - 1_sz
       call subr (seed, data%array(vecr%istart0() + i)%element, new_seed)
       result_data%array(i)%element = new_seed
       seed = new_seed
    end do

    vec_c = .val. vec_c_root

    call vec_root%discard
  end function vectar_cumulate_subr

!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_count[]n (pred, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                            ])vec[]k])) result (count)
    procedure(vectar_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    integer(sz) :: count

m4_declare_iteration_variables(n)dnl
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl

    ! Protect against garbage collections instigated by subr.
m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       if (pred (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
            &        ])data[]k%array(i[]k)%element]))) then
          count = count + 1
       end if
    end do

m4_discard_vec_roots(n)dnl
  end function vectar_count[]n

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_fold[]n[]_subr (kons, knil, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                                ])vec[]k])) result (vec_f)
    procedure(vectar_kons[]n[]_subr_t) :: kons
    class(*), intent(in) :: knil
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    class(*), allocatable :: vec_f

m4_declare_iteration_variables(n)dnl
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl

m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl

    state = knil
    do i = 0_sz, min_length - 1_sz
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       call kons (.val. state, data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
            &     ])data[]k%array(i[]k)%element]), next_state)
       state = next_state
    end do
    vec_f = .val. state

m4_discard_vec_roots(n)dnl
  end function vectar_fold[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_fold_right[]n[]_subr (kons, knil, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                                      ])vec[]k])) result (vec_f)
    procedure(vectar_kons[]n[]_subr_t) :: kons
    class(*), intent(in) :: knil
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    class(*), allocatable :: vec_f

m4_declare_iteration_variables(n)dnl
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl

m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl

    state = knil
    do i = 0_sz, min_length - 1_sz
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + (min_length - 1_sz - i)
])dnl
       call kons (.val. state, data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
            &     ])data[]k%array(i[]k)%element]), next_state)
       state = next_state
    end do
    vec_f = .val. state

m4_discard_vec_roots(n)dnl
  end function vectar_fold_right[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

m4_define([m4_vectar_unfold_procedures],[dnl
  recursive subroutine vectar_unfold[]$1[]x0_subr (f, vec)
    procedure(vectar_unfold0_f_subr_t) :: f
    class(*), intent(in) :: vec

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    do index = $2
       call f (index, element)
       data%array(i0 + index)%element = element
    end do

    call vec_root%discard
  end subroutine vectar_unfold[]$1[]x0_subr

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive subroutine vectar_unfold[]$1[]x[]n[]_subr (f, vec, &
       initial_seed1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
       ])initial_seed[]k]))
    procedure(vectar_unfold[]n[]_f_subr_t) :: f
    class(*), intent(in) :: vec
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: initial_seed[]k
])dnl

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: seed[]k
])dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: seed[]k[]_root
])dnl
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
m4_forloop([k],[1],n,[dnl
    seed[]k[]_root = initial_seed[]k
])dnl
    do index = $2
m4_forloop([k],[1],n,[dnl
       seed[]k = .val. seed[]k[]_root
])dnl
       call f (index, seed1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
            &  ])seed[]k]), element)
       data%array(i0 + index)%element = element
m4_forloop([k],[1],n,[dnl
       seed[]k[]_root = seed[]k
])dnl
    end do

    call vec_root%discard
  end subroutine vectar_unfold[]$1[]x[]n[]_subr

])dnl
dnl
  recursive function vectar_unfold[]$1[]0_subr_size_kind (f, length) result (vec)
    procedure(vectar_unfold0_f_subr_t) :: f
    integer(sz), intent(in) :: length
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold[]$1[]x0_subr (f, vec)
  end function vectar_unfold[]$1[]0_subr_size_kind

  recursive function vectar_unfold[]$1[]0_subr_int (f, length) result (vec)
    procedure(vectar_unfold0_f_subr_t) :: f
    integer, intent(in) :: length
    type(vectar_t) :: vec

    vec = vectar_unfold[]$1[]0_subr_size_kind (f, .sz. length)
  end function vectar_unfold[]$1[]0_subr_int

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_unfold[]$1[][]n[]_subr_size_kind (f, length, &
       initial_seed1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
       ])initial_seed[]k])) result (vec)
    procedure(vectar_unfold[]n[]_f_subr_t) :: f
    integer(sz), intent(in) :: length
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: initial_seed[]k
])dnl
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold[]$1[]x[]n[]_subr (f, vec, &
         initial_seed1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
         ])initial_seed[]k]))
  end function vectar_unfold[]$1[][]n[]_subr_size_kind

  recursive function vectar_unfold[]$1[][]n[]_subr_int (f, length, &
       initial_seed1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
       ])initial_seed[]k])) result (vec)
    procedure(vectar_unfold[]n[]_f_subr_t) :: f
    integer, intent(in) :: length
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: initial_seed[]k
])dnl
    type(vectar_t) :: vec

    vec = vectar_unfold[]$1[][]n[]_subr_size_kind (f, .sz. length, &
         initial_seed1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
         ])initial_seed[]k]))
  end function vectar_unfold[]$1[][]n[]_subr_int

])dnl
])dnl
dnl
m4_vectar_unfold_procedures([],[0_sz, vecr%length() - 1_sz])dnl
m4_vectar_unfold_procedures([_right],[vecr%length() - 1_sz, 0_sz, -1_sz])dnl
dnl
!!!-------------------------------------------------------------------

m4_define([m4_vectar_indexing_left_to_right],[dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_$1n_[]n (pred, [n], vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                              ])vec[]k])) result (index)
    procedure(vectar_predicate[]n[]_t) :: pred
    integer(sz), intent(in) :: [n]
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    integer(sz) :: index

m4_declare_iteration_variables(n)dnl
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl
    logical :: requirement_is_satisfied

m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       requirement_is_satisfied = &
            $2pred (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
            &     ])data[]k%array(i[]k)%element]))
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = ([n] - 1_sz) + i
    else
       index = min (-1_sz, [n] - 1_sz)
    end if

m4_discard_vec_roots(n)dnl
  end function vectar_$1n_[]n

  recursive function vectar_$1[]0_[]n (pred, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                              ])vec[]k])) result (index)
    procedure(vectar_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    integer(sz) :: index

    index = vectar_$1n_[]n (pred, 0_sz, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
         &                   ])vec[]k]))
  end function vectar_$1[]0_[]n

  recursive function vectar_$1[]1_[]n (pred, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                              ])vec[]k])) result (index)
    procedure(vectar_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    integer(sz) :: index

    index = vectar_$1n_[]n (pred, 1_sz, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
         &                   ])vec[]k]))
  end function vectar_$1[]1_[]n

])dnl
])dnl
dnl
m4_vectar_indexing_left_to_right([index],[])dnl
m4_vectar_indexing_left_to_right([skip],[.not. ])dnl
dnl
!!!-------------------------------------------------------------------

m4_define([m4_vectar_indexing_right_to_left],[dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_$1_rightn_[]n (pred, [n], vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                              ])vec[]k])) result (index)
    procedure(vectar_predicate[]n[]_t) :: pred
    integer(sz), intent(in) :: [n]
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    integer(sz) :: index

m4_declare_iteration_variables(n)dnl
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl
    logical :: requirement_is_satisfied

m4_forloop([k],[1],n,[dnl
    vec[]k[]_root = vec[]k
])dnl

m4_initialize_iteration(n)dnl
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       requirement_is_satisfied = &
            $2pred (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
            &     ])data[]k%array(i[]k)%element]))
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = ([n] + 1_sz) + i
    else
       index = min (-1_sz, [n] - 1_sz)
    end if

m4_discard_vec_roots(n)dnl
  end function vectar_$1_rightn_[]n

  recursive function vectar_$1_right0_[]n (pred, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                              ])vec[]k])) result (index)
    procedure(vectar_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    integer(sz) :: index

    index = vectar_$1_rightn_[]n (pred, 0_sz, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
         &                         ])vec[]k]))
  end function vectar_$1_right0_[]n

  recursive function vectar_$1_right1_[]n (pred, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                              ])vec[]k])) result (index)
    procedure(vectar_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    integer(sz) :: index

    index = vectar_$1_rightn_[]n (pred, 1_sz, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
         &                         ])vec[]k]))
  end function vectar_$1_right1_[]n

])dnl
])dnl
dnl
m4_vectar_indexing_right_to_left([index],[])dnl
m4_vectar_indexing_right_to_left([skip],[.not. ])dnl
dnl
!!!-------------------------------------------------------------------

  recursive function vectar_binary_search0 (vec, x, cmp) result (index)
    class(*), intent(in) :: vec
    class(*), intent(in) :: x
    procedure(vectar_cmp_func_t) :: cmp
    integer(sz) :: index

    index = vectar_binary_searchn (vec, 0_sz, x, cmp)
  end function vectar_binary_search0

  recursive function vectar_binary_search1 (vec, x, cmp) result (index)
    class(*), intent(in) :: vec
    class(*), intent(in) :: x
    procedure(vectar_cmp_func_t) :: cmp
    integer(sz) :: index

    index = vectar_binary_searchn (vec, 1_sz, x, cmp)
  end function vectar_binary_search1

  recursive function vectar_binary_searchn (vec, n, x, cmp) result (index)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    class(*), intent(in) :: x
    procedure(vectar_cmp_func_t) :: cmp
    integer(sz) :: index

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: len, i0, i
    integer(sz) :: ileft, imiddle, iright
    integer :: sign

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    len = vecr%length()
    data => vectar_data_ptr (vecr)
    ileft = 0_sz
    iright = len - 1_sz
    index = min (-1_sz, [n] - 1_sz)
    do while (ileft <= iright .and. index < [n])
       imiddle = ileft + ((iright - ileft) / 2_sz)
       i = i0 + imiddle
       sign = cmp (data%array(i)%element, x)
       if (sign < 0) then
          ileft = imiddle + 1
       else if (0 < sign) then
          iright = imiddle - 1
       else
          index = imiddle + [n]
       end if
    end do

    call vec_root%discard
  end function vectar_binary_searchn

!!!-------------------------------------------------------------------

m4_define([m4_vectar_some_or_every],[dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_$1[]n (pred, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                           ])vec[]k])) result (bool)
    procedure(vectar_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    logical :: bool

m4_declare_iteration_variables(n)dnl
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl

m4_initialize_iteration(n)dnl

    i = 0_sz
    bool = $2.true.
    do while ($2[]bool .and. i < min_length)
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       bool = pred (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
            &     ])data[]k%array(i[]k)%element]))
       i = i + 1
    end do

m4_discard_vec_roots(n)dnl
  end function vectar_$1[]n

])dnl
])dnl
dnl
m4_vectar_some_or_every([some],[.not. ])dnl
m4_vectar_some_or_every([every],[])dnl
dnl
!!!-------------------------------------------------------------------

m4_define([m4_vectar_some_map_or_every_map],[dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function vectar_$1_map[]n[]_subr (subr, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                           ])vec[]k])) result (retval)
    procedure(vectar_map[]n[]_subr_t) :: subr
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    class(*), allocatable :: retval

m4_declare_iteration_variables(n)dnl
    integer(sz) :: i
m4_forloop([k],[1],n,[dnl
    integer(sz) :: i[]k
])dnl
    class(*), allocatable :: subr_result
    logical :: short_circuited

m4_initialize_iteration(n)dnl

    subr_result = $2.true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
m4_forloop([k],[1],n,[dnl
       i[]k = vecr[]k%istart0() + i
])dnl
       call subr (data1%array(i1)%element[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 2),[1],[&
            &     ])data[]k%array(i[]k)%element]), subr_result)
       short_circuited = $2[]is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

m4_discard_vec_roots(n)dnl
  end function vectar_$1_map[]n[]_subr

])dnl
])dnl
dnl
m4_vectar_some_map_or_every_map([some],[.not. ])dnl
m4_vectar_some_map_or_every_map([every],[])dnl
dnl
!!!-------------------------------------------------------------------

  recursive subroutine do_vectar_partition (pred, vec, vec_out, num_satisfied)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec
    type(vectar_t), intent(inout) :: vec_out
    integer(sz), intent(inout) :: num_satisfied

    !
    ! Our implementation tries to minimize the number of predicate
    ! tests done. It allocates an array of 
    !

    integer, parameter :: bool = selected_int_kind (0)
    integer(bool), parameter :: false = 0_bool
    integer(bool), parameter :: true = 1_bool

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: len
    integer(sz) :: len_minus_one
    integer(bool), allocatable :: satisfied(:)
    integer(sz) :: i0, i, j, k
    integer(sz) :: satisfied_count
    type(vectar_t) :: partitioned_vectar
    class(vectar_data_t), pointer :: partitioned_vectar_data

    vec_root = vec

    vecr = vec
    if (vecr%length() == 0) then
       vec_out = make_vectar (0_sz)
       num_satisfied = 0_sz
    else
       data => vectar_data_ptr (vecr)
       i0 = vecr%istart0()
       len = vecr%length()
       len_minus_one = len - 1_sz

       allocate (satisfied(0_sz:len_minus_one), source = false)

       ! Mark which entries satisfy the predicate, and also count how
       ! many do.
       satisfied_count = 0_sz
       do i = 0_sz, len_minus_one
          if (pred (data%array(i0 + i)%element)) then
             satisfied(i) = true
             satisfied_count = satisfied_count + 1_sz
          end if
       end do

       ! Create and fill the output vector.
       partitioned_vectar = make_vectar (len)
       partitioned_vectar_data => vectar_data_ptr (partitioned_vectar)
       j = 0_sz
       k = satisfied_count
       do i = 0_sz, len_minus_one
          if (satisfied(i) == true) then
             partitioned_vectar_data%array(j) = data%array(i0 + i)
             j = j + 1
          else
             partitioned_vectar_data%array(k) = data%array(i0 + i)
             k = k + 1
          end if
       end do

       deallocate (satisfied)

       vec_out = partitioned_vectar
       num_satisfied = satisfied_count
    end if

    call vec_root%discard
  end subroutine do_vectar_partition

  recursive function vectar_partition (pred, vec) result (lst)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    type(vectar_t) :: vec_out
    integer(sz) :: num_satisfied

    call do_vectar_partition (pred, vec, vec_out, num_satisfied)
    lst = list (vec_out, num_satisfied)
  end function vectar_partition

!!!-------------------------------------------------------------------

end module vectars
