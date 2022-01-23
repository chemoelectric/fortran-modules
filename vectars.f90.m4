! -*- F90 -*- include(`common-macros.m4')m4_include([cadadr.m4])
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
  ! Sorting routines *are* included, inspired by SRFI-132
  ! (https://srfi.schemers.org/srfi-132/srfi-132.html). See also
  ! SRFI-95 (https://srfi.schemers.org/srfi-95/srfi-95.html).
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

  !
  ! Generic functions to do binary searches on vectars and vectar
  ! ranges, using an ordering predicate <
  !
  ! These are not based on anything in SRFI-133.
  !
  ! See H. Bottenbruch, `Structure and use of ALGOL 60', Journal of
  ! the ACM, Volume 9, Issue 2, April 1962,
  ! pp.161-221. https://doi.org/10.1145/321119.321120
  !
  ! The general algorithm is described on pages 214 and 215.
  !
  ! See also
  ! https://en.wikipedia.org/w/index.php?title=Binary_search_algorithm&oldid=1062988272#Alternative_procedure
  !
  ! If no equality predicate is provided, we make the assumption that
  ! if neither a < b nor b < a is true, then a == b (for the purpose
  ! of the search).
  !
  ! These functions are designed so they always return a negative
  ! number on failure to satisfy the predicate, and also so the
  ! negative number is specifically -1, if the index base is
  ! non-negative. This seemed a convenient convention.
  !
  ! (Note that the predicate arguments come first, unlike the `cmp'
  ! argument in vectar_binary_search0, vectar_binary_search1, or
  ! vectar_binary_searchn, which comes last.)
  !
  ! Using a `strcmp'-like function rather than an ordering predicate
  ! and equality test is clearly a good idea in many cases (such as
  ! sorting strings). But I wanted to have some fun, so I included
  ! these searches as well.
  !
  public :: vectar_bottenbruch_search0 ! Return the 0-based index
                                       ! where a predicate is first
                                       ! satisfied, or -1 if it is
                                       ! never satisfied.
  public :: vectar_bottenbruch_search1 ! Return the 1-based index of a
                                       ! match, or -1 if there is no
                                       ! match.
  public :: vectar_bottenbruch_searchn ! Return the n-based index of ax
                                       ! match, or min (-1, n - 1) if
                                       ! there is no match.

  ! Implementations of the Bottenbruch searches.
  public :: vectar_bottenbruch_search0_without_equality
  public :: vectar_bottenbruch_search1_without_equality
  public :: vectar_bottenbruch_searchn_without_equality
  public :: vectar_bottenbruch_search0_with_equality
  public :: vectar_bottenbruch_search1_with_equality
  public :: vectar_bottenbruch_searchn_with_equality

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! SHUFFLING.
!!
!! Shuffling is not included in SRFI-133.
!!

  ! Subroutine that creates a new vectar, containing the same elements
  ! as a given vectar or vectar range, but shuffled.
  public :: vectar_shuffle

  ! Subroutine that shuffles a vectar or vectar range in place.
  public :: vectar_shufflex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! SORTING.
!!
!! Sorting is not included in SRFI-133. These procedures are based
!! instead on SRFI-132.
!!

  public :: vectar_is_sorted

  ! Stable merges.
  public :: vectar_merge
  public :: vectar_mergex

  public :: vectar_stable_sortx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Some unit tests. Not for use in a program other than for testing.
  public :: vectar_stable_mergesort_unit_tests

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

  ! The maximum size of a stack used by the stable sort. It should be
  ! good enough if bit_size(1_sz) <= 64. See in particular
  ! http://envisage-project.eu/proving-android-java-and-python-sorting-algorithm-is-broken-and-how-to-fix-it/
  !
  ! From the Timsort code:
  !
  !    /* The maximum number of entries in a MergeState's 
  !     * pending-runs stack.
  !     * This is enough to sort arrays of size up to about
  !     *     32 * phi ** MAX_MERGE_PENDING
  !     * where phi ~= 1.618.  85 is ridiculously large enough, 
  !     * good for an array with 2**64 elements.
  !     */
  !
  ! Presumably one likes to keep it a fixed size and as small as is
  ! reasonable.
  !
  integer, parameter :: run_stack_size = 85


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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  
  interface vectar_bottenbruch_search0
     module procedure vectar_bottenbruch_search0_without_equality
     module procedure vectar_bottenbruch_search0_with_equality
  end interface vectar_bottenbruch_search0

  interface vectar_bottenbruch_search1
     module procedure vectar_bottenbruch_search1_without_equality
     module procedure vectar_bottenbruch_search1_with_equality
  end interface vectar_bottenbruch_search1

  interface vectar_bottenbruch_searchn
     module procedure vectar_bottenbruch_searchn_without_equality
     module procedure vectar_bottenbruch_searchn_with_equality
  end interface vectar_bottenbruch_searchn

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  function int_less_than (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = .false.
    select type (x)
    type is (integer)
       select type (y)
       type is (integer)
          bool = (x < y)
       end select
    end select
  end function int_less_than

  function int_equal (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = .false.
    select type (x)
    type is (integer)
       select type (y)
       type is (integer)
          bool = (x == y)
       end select
    end select
  end function int_equal

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar0 () result (vec)
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 0_sz
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
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
          allocate (data%array(0_sz:(size - 1_sz)), source = vectar_element_t (.autoval. fill))
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    end if
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function reverse_list_to_vectar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    case (0)
       vec_c = vectar ()
m4_forloop([n],[1],ZIP_MAX,[dnl
    case (n)
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
       block
         type(cons_t) :: p
         type(cons_t) :: vecs_reversed
         type(vectar_range_t) :: vecr
         integer(sz) :: len_vec_c
         integer(sz) :: i, j
         class(vectar_data_t), pointer :: src, dst

         vecs_reversed = nil
         len_vec_c = 0_sz
         p = vectars
         do i = 1_sz, num_vectars
            vecr = car (p)
            vecs_reversed = vecr ** vecs_reversed
            len_vec_c = len_vec_c + vecr%length()
            p = cdr (p)
         end do

         vec_c = make_vectar (len_vec_c)

         dst => vectar_data_ptr (vec_c)

         j = len_vec_c
         p = vecs_reversed
         do while (is_pair (p))
            vecr = car (p)
            src => vectar_data_ptr (vecr)
            do i = vecr%iend0(), vecr%istart0(), -1
               j = j - 1
               dst%array(j) = src%array(i)
            end do
            p = cdr (p)
         end do
       end block
    end select
  end function vectar_concatenate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_bottenbruch_search0_without_equality (less_than, vec, x) result (index)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    class(*), intent(in) :: x
    integer(sz) :: index

    index = vectar_bottenbruch_searchn_without_equality (less_than, vec, 0_sz, x)
  end function vectar_bottenbruch_search0_without_equality

  recursive function vectar_bottenbruch_search1_without_equality (less_than, vec, x) result (index)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    class(*), intent(in) :: x
    integer(sz) :: index

    index = vectar_bottenbruch_searchn_without_equality (less_than, vec, 1_sz, x)
  end function vectar_bottenbruch_search1_without_equality

  recursive function vectar_bottenbruch_searchn_without_equality (less_than, vec, n, x) result (index)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    class(*), intent(in) :: x
    integer(sz) :: index

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: istart0, iend0, i

    index = min (-1_sz, [n] - 1_sz)

    vecr = vec
    istart0 = vecr%istart0()
    iend0 = vecr%iend0()

    if (istart0 <= iend0) then
       vec_root = vec

       data => vectar_data_ptr (vecr)
       i = bottenbruch_search (less_than, data, istart0, iend0, x)
       if (.not. less_than (data%array(i)%element, x)) then
          if (.not. less_than (x, data%array(i)%element)) then
             index = i - istart0 + [n]
          end if
       end if

       call vec_root%discard
    end if
  end function vectar_bottenbruch_searchn_without_equality

  recursive function vectar_bottenbruch_search0_with_equality (less_than, equal, vec, x) result (index)
    procedure(vectar_predicate2_t) :: less_than
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec
    class(*), intent(in) :: x
    integer(sz) :: index

    index = vectar_bottenbruch_searchn_with_equality (less_than, equal, vec, 0_sz, x)
  end function vectar_bottenbruch_search0_with_equality

  recursive function vectar_bottenbruch_search1_with_equality (less_than, equal, vec, x) result (index)
    procedure(vectar_predicate2_t) :: less_than
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec
    class(*), intent(in) :: x
    integer(sz) :: index

    index = vectar_bottenbruch_searchn_with_equality (less_than, equal, vec, 1_sz, x)
  end function vectar_bottenbruch_search1_with_equality

  recursive function vectar_bottenbruch_searchn_with_equality (less_than, equal, vec, n, x) result (index)
    procedure(vectar_predicate2_t) :: less_than
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    class(*), intent(in) :: x
    integer(sz) :: index

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: istart0, iend0, i

    index = min (-1_sz, [n] - 1_sz)

    vecr = vec
    istart0 = vecr%istart0()
    iend0 = vecr%iend0()

    if (istart0 <= iend0) then
       vec_root = vec

       data => vectar_data_ptr (vecr)
       i = bottenbruch_search (less_than, data, istart0, iend0, x)
       if (equal (x, data%array(i)%element)) then
          index = i - istart0 + [n]
       end if

       call vec_root%discard
    end if
  end function vectar_bottenbruch_searchn_with_equality

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    call vec_root%discard

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar_shuffle (vec) result (vec_shuffled)
    use, intrinsic :: iso_fortran_env, only: real64
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_shuffled

    !
    ! Fisher-Yates shuffle.
    !
    ! See
    ! https://en.wikipedia.org/w/index.php?title=Fisher%E2%80%93Yates_shuffle&oldid=1063206771#The_%22inside-out%22_algorithm
    !

    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(vectar_data_t), pointer :: data_shuffled
    integer(sz) :: i0, len
    real(real64) :: randnum
    integer(sz) :: i, j

    vecr = vec
    data => vectar_data_ptr (vecr)
    i0 = vecr%istart0()
    len = vecr%length()

    vec_shuffled = make_vectar (len)
    data_shuffled => vectar_data_ptr (vec_shuffled)

    do i = 0_sz, len - 1
       call random_number (randnum)
       j = int (randnum * (i + 1), kind = sz)
       if (j /= i) then
          data_shuffled%array(i) = data_shuffled%array(j)
       end if
       data_shuffled%array(j) = data%array(i0 + i)
    end do
  end function vectar_shuffle

  subroutine vectar_shufflex (vec)
    use, intrinsic :: iso_fortran_env, only: real64
    class(*), intent(in) :: vec

    !
    ! Fisher-Yates shuffle.
    !
    ! See
    ! https://en.wikipedia.org/w/index.php?title=Fisher%E2%80%93Yates_shuffle&oldid=1063206771#The_modern_algorithm
    !

    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: i0, len
    real(real64) :: randnum
    integer(sz) :: i, j
    class(*), allocatable :: tmp

    vecr = vec
    data => vectar_data_ptr (vecr)
    i0 = vecr%istart0()
    len = vecr%length()
    do i = 0_sz, len - 2
       call random_number (randnum)
       j = i + int (randnum * (len - i), kind = sz)
       tmp = data%array(i0 + i)%element
       data%array(i0 + i)%element = data%array(i0 + j)%element
       data%array(i0 + j)%element = tmp
    end do
  end subroutine vectar_shufflex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_is_sorted (less_than, vec) result (bool)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    logical :: bool

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data

    vec_root = vec

    vecr = vec
    data => vectar_data_ptr (vecr)
    bool = data_is_sorted (less_than, data, vecr%istart0(), vecr%iend0())

    call vec_root%discard
  end function vectar_is_sorted

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_merge (less_than, vec1, vec2) result (vec_m)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    type(vectar_t) :: vec_m

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2

    vecr1 = vec1
    vecr2 = vec2
    vec_m = make_vectar (vecr1%length() + vecr2%length())
    call vectar_mergex (less_than, vec_m, vecr1, vecr2)
  end function vectar_merge

  recursive subroutine vectar_mergex (less_than, vec_m, vec1, vec2)

    !
    ! It is assumed that vec_m does not overlap either of vec1 or
    ! vec2. This assumption is carried over from SRFI-132.
    !

    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec_m
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2

    type(gcroot_t) :: vec_m_root, vec1_root, vec2_root
    type(vectar_range_t) :: vecr_m, vecr1, vecr2
    class(vectar_data_t), pointer :: data_m, data1, data2
    integer(sz) :: i0_m, i0_1, i0_2
    integer(sz) :: len_m, len1, len2
    integer(sz) :: i, j, k

    vec_m_root = vec_m
    vec1_root = vec1
    vec2_root = vec2

    vecr_m = vec_m
    i0_m = vecr_m%istart0()
    len_m = vecr_m%length()

    vecr1 = vec1
    i0_1 = vecr1%istart0()
    len1 = vecr1%length()

    vecr2 = vec2
    i0_2 = vecr2%istart0()
    len2 = vecr2%length()

    if (len_m /= len1 + len2) then
       call error_abort ("vectar_mergex arguments have incompatible lengths")
    end if

    data_m => vectar_data_ptr (vecr_m)
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    j = 0_sz
    k = 0_sz
    i = 0_sz
    do while (i < len_m)
       if (j == len1) then
          ! The rest of the result is from vecr2.
          do while (i < len_m)
             data_m%array(i0_m + i) = data2%array(i0_2 + k)
             k = k + 1
             i = i + 1
          end do
       else if (k == len2) then
          ! The rest of the result is from vecr1
          do while (i < len_m)
             data_m%array(i0_m + i) = data1%array(i0_1 + j)
             j = j + 1
             i = i + 1
          end do
       else if (.not. less_than (data2%array(i0_2 + k)%element, data1%array(i0_1 + j)%element)) then
          data_m%array(i0_m + i) = data1%array(i0_1 + j)
          j = j + 1
          i = i + 1
       else
          data_m%array(i0_m + i) = data2%array(i0_2 + k)
          k = k + 1
          i = i + 1
       end if
    end do

    call vec_m_root%discard
    call vec1_root%discard
    call vec2_root%discard
  end subroutine vectar_mergex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine vectar_stable_sortx (less_than, vec)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec

    type(gcroot_t) :: vec_root, workspace_root
    type(vectar_range_t) :: vecr, workspace_range
    class(vectar_data_t), pointer :: data, workspace
    integer(sz) :: idatastart, idataend

    vec_root = vec

    vecr = vec
    data => vectar_data_ptr (vecr)

    idatastart = vecr%istart0()
    idataend = vecr%iend0()

    workspace_root = make_vectar (((idataend - idatastart) + 1) / 2)
    workspace_range = workspace_root
    workspace => vectar_data_ptr (workspace_range)

    call stable_mergesort (less_than, data, idatastart, idataend, workspace)

    call workspace_root%discard
    call vec_root%discard
  end subroutine vectar_stable_sortx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function data_is_sorted (less_than, data, ileft, iright) result (bool)
    !
    ! Is data[ileft .. iright] free of any descending sequences?
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright
    logical::bool

    integer(sz) :: i

    bool = .true.
    i = ileft
    do while (bool .and. i < iright)
       bool = .not. less_than (data%array(i + 1)%element, data%array(i)%element)
       i = i + 1
    end do
  end function data_is_sorted

  recursive function bottenbruch_search (less_than, data, ileft, iright, x) result (index)
    !
    ! Do a search on the data whose first element is at ileft and
    ! whose last element is at iright. Return `index' such that:
    !
    !    * if x is less than the element at ileft, then index = ileft;
    !
    !    * otherwise, x is greater than or equal to the element at
    !      index (and therefore to every element to the left of
    !      index), and less than everything to the right of index.
    !
    ! References:
    !
    !    * H. Bottenbruch, `Structure and use of ALGOL 60', Journal of
    !      the ACM, Volume 9, Issue 2, April 1962,
    !      pp.161-221. https://doi.org/10.1145/321119.321120
    !      The general algorithm is described on pages 214 and 215.
    !
    !    * https://en.wikipedia.org/w/index.php?title=Binary_search_algorithm&oldid=1062988272#Alternative_procedure
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright
    class(*), intent(in) :: x
    integer(sz) :: index

    integer(sz) :: i, j, k
    integer(sz) :: k_minus_j

    j = ileft
    k = iright
    do while (k /= j)
       ! Set i := ceil ((j + k) / 2).
       k_minus_j = k - j
       i = j + ishft (k_minus_j, -1) + ibits (k_minus_j, 0, 1)
       if (less_than (x, data%array(i)%element)) then
          k = i - 1
       else
          j = i
       end if
    end do
    index = j
  end function bottenbruch_search

  subroutine unit_test__bottenbruch_search__test1
    integer, parameter :: number_of_vectars = 100
    integer(sz), parameter :: veclen = 1024_sz
    integer, parameter :: modulus = 64

    type(gcroot_t) :: vec
    integer :: vectar_number
    integer(sz) :: i
    real :: randnum
    integer :: x
    class(vectar_data_t), pointer :: data

    vec = make_vectar (veclen)
    do vectar_number = 1, number_of_vectars
       do i = 0_sz, veclen - 1
          call random_number (randnum)
          call vectar_set0 (vec, i, int (randnum * (modulus)))
       end do
       vec = list_to_vectar (list_sortx (int_less_than, vectar_to_list (vec)))
       data => vectar_data_ptr (vec)
       do x = -1, modulus
          i = bottenbruch_search (int_less_than, data, 0_sz, veclen - 1, x)
          if (i /= 0) then
             if (int_less_than (x, vectar_ref0 (vec, i))) then
                ! Unless i is 0, x must be greater than or equal
                ! to the element at i (and therefore also to every
                ! element to the left of i).
                call error_abort ("unit_test__bottenbruch_search__test1 0010 failed")
             end if
          end if
          if (i /= veclen - 1) then
             if (.not. int_less_than (x, vectar_ref0 (vec, i + 1))) then
                ! x must be less than the element at i+1 (and
                ! therefore also to everything else to the right of
                ! i).
                call error_abort ("unit_test__bottenbruch_search__test1 0020 failed")
             end if
          end if
       end do
    end do
  end subroutine unit_test__bottenbruch_search__test1

  recursive function bottenbruch_search2 (less_than, data, ileft, iright, x) result (index)
    !
    ! Do a search on the data whose first element is at ileft and
    ! whose last element is at iright. Return `index' such that:
    !
    !    * if x is greater than the element at iright, then index =
    !      iright;
    !
    !    * otherwise, x is greater than every element to the left of
    !      index, and less than or equal to the elements at index and
    !      to the right of index.
    !
    ! References:
    !
    !    * H. Bottenbruch, `Structure and use of ALGOL 60', Journal of
    !      the ACM, Volume 9, Issue 2, April 1962,
    !      pp.161-221. https://doi.org/10.1145/321119.321120
    !      The general algorithm is described on pages 214 and 215.
    !
    !    * https://en.wikipedia.org/w/index.php?title=Binary_search_algorithm&oldid=1062988272#Alternative_procedure
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright
    class(*), intent(in) :: x
    integer(sz) :: index

    integer(sz) :: i, j, k
    integer(sz) :: k_minus_j

    j = ileft
    k = iright
    do while (k /= j)
       ! Set i := floor ((j + k) / 2).
       k_minus_j = k - j
       i = j + ishft (k_minus_j, -1)
       if (less_than (data%array(i)%element, x)) then
          ! x is greater than the element at i.
          j = i + 1
       else
          k = i
       end if
    end do
    index = j
  end function bottenbruch_search2

  subroutine unit_test__bottenbruch_search2__test1
    integer, parameter :: number_of_vectars = 100
    integer(sz), parameter :: veclen = 1024_sz
    integer, parameter :: modulus = 64

    type(gcroot_t) :: vec
    integer :: vectar_number
    integer(sz) :: i
    real :: randnum
    integer :: x
    class(vectar_data_t), pointer :: data

    vec = make_vectar (veclen)
    do vectar_number = 1, number_of_vectars
       do i = 0_sz, veclen - 1
          call random_number (randnum)
          call vectar_set0 (vec, i, int (randnum * (modulus)))
       end do
       vec = list_to_vectar (list_sortx (int_less_than, vectar_to_list (vec)))
       data => vectar_data_ptr (vec)
       do x = -1, modulus
          i = bottenbruch_search2 (int_less_than, data, 0_sz, veclen - 1, x)
          if (i /= veclen - 1) then
             if (int_less_than (vectar_ref0 (vec, i), x)) then
                ! Unless i is veclen - 1, x must be less than or equal
                ! to the element at i (and therefore also to every
                ! element to the right of i).
                call error_abort ("unit_test__bottenbruch_search__test1 0010 failed")
             end if
          end if
          if (i /= 0) then
             if (.not. int_less_than (vectar_ref0 (vec, i - 1), x)) then
                ! x must be greater than the element at i-1 (and
                ! therefore greater than everything to the left of i).
                call error_abort ("unit_test__bottenbruch_search__test1 0020 failed")
             end if
          end if
       end do
    end do
  end subroutine unit_test__bottenbruch_search2__test1

  recursive subroutine stable_binary_insertion_sort (less_than, data, ileft, ipresorted, iright)
    !
    ! Sort the data whose first element is at ileft and whose last
    ! element is at iright, presuming everything from ileft to
    ! ipresorted (inclusively) is already sorted.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: ipresorted
    integer(sz), intent(in) :: iright

    integer(sz) :: i
    integer(sz) :: j
    class(vectar_element_t), pointer :: p_ileft
    class(vectar_element_t), pointer :: p_i
    class(vectar_element_t), allocatable :: elem_i

    p_ileft => data%array(ileft)
    do i = ipresorted + 1, iright
       p_i => data%array(i)
       j = bottenbruch_search (less_than, data, ileft, i - 1, p_i%element)
       if (j == ileft) then
          if (less_than (p_i%element, p_ileft%element)) then
             call insert_at_j
          else
             call insert_after_j
          end if
       else
          call insert_after_j
       end if
    end do

  contains

    subroutine insert_at_j
      elem_i = p_i
      call move_elements_right (j, i - 1)
      data%array(j) = elem_i
    end subroutine insert_at_j

    subroutine insert_after_j
      if (j + 1 /= i) then
         elem_i = p_i
         call move_elements_right (j + 1, i - 1)
         data%array(j + 1) = elem_i
      end if
    end subroutine insert_after_j

    subroutine move_elements_right (ifirst, ilast)
      integer(sz), intent(in) :: ifirst
      integer(sz), intent(in) :: ilast

      integer(sz) :: k

      do k = ilast, ifirst, -1_sz
         data%array(k + 1) = data%array(k)
      end do
    end subroutine move_elements_right

  end subroutine stable_binary_insertion_sort

  subroutine reverse_in_place (data, ileft, iright)
    !
    ! Reverse the data whose first element is at ileft and whose last
    ! element is at iright.
    !
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright

    class(vectar_element_t), allocatable :: tmp
    integer(sz) :: i, j

    i = ileft
    j = iright
    do while (i < j)
       tmp = data%array(i)
       data%array(i) = data%array(j)
       data%array(j) = tmp
       i = i + 1
       j = j - 1
    end do
  end subroutine reverse_in_place

  recursive subroutine gather_an_increasing_run (less_than, data, ileft, iend, iright)
    !
    ! Set iright so from ileft to iright, inclusive, there is a
    ! (non-strictly) increasing run of elements. Elements may be
    ! reversed to make it so.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iend
    integer(sz), intent(out) :: iright

    integer(sz) :: itrial
    logical :: done

    if (ileft == iend) then
       ! A final run of one.
       iright = ileft
    else
       iright = ileft + 1
       if (less_than (data%array(iright)%element, data%array(ileft)%element)) then
          ! The sequence is strictly decreasing. Reverse it and so get a
          ! strictly increasing sequence. (Reversing a non-strictly
          ! decreasing sequence would be an unstable sort.)
          done = .false.
          do while (.not. done)
             if (iright == iend) then
                ! This is the final run.
                done = .true.
             else
                itrial = iright + 1
                if (.not. less_than (data%array(itrial)%element, data%array(iright)%element)) then
                   ! The next element would be equal or increasing.
                   done = .true.
                else
                   ! Keep going.
                   iright = itrial
                end if
             end if
          end do
          ! Make the sequence increasing.
          call reverse_in_place (data, ileft, iright)
       else
          ! The sequence is increasing.
          done = .false.
          do while (.not. done)
             if (iright == iend) then
                ! This is the final run.
                done = .true.
             else
                itrial = iright + 1
                if (less_than (data%array(itrial)%element, data%array(iright)%element)) then
                   ! The next element would be decreasing.
                   done = .true.
                else
                   ! Keep going.
                   iright = itrial
                end if
             end if
          end do
       end if
    end if
  end subroutine gather_an_increasing_run

  recursive subroutine gather_an_adequately_long_increasing_run (less_than, data, ileft, iend, min_length, iright)
    !
    ! Set iright so from ileft to iright, inclusively, there is a
    ! (non-strictly) increasing run of elements, either of the given
    ! min_length or at end of the data. Elements may be sorted to make
    ! it so.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iend
    integer(sz), intent(in) :: min_length
    integer(sz), intent(out) :: iright

    integer(sz) :: ipresorted

    call gather_an_increasing_run (less_than, data, ileft, iend, ipresorted)
    if (ipresorted == iend .or. min_length - 1 <= ipresorted - ileft) then
       iright = ipresorted
    else
       iright = min (iend, ileft + (min_length - 1))
       call stable_binary_insertion_sort (less_than, data, ileft, ipresorted, iright)
    end if
  end subroutine gather_an_adequately_long_increasing_run

  recursive subroutine merge_going_leftwards (less_than, data2, itarget, irunstart2, irunend2, &
       &                                      data1, irunstart1, irunend1)
    !
    ! Currently this is just a straightforward stable merge, filling
    ! empty space at the left of data2, with data1 as the run having
    ! priority in the merge.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data2
    integer(sz), intent(in) :: itarget
    integer(sz), intent(in) :: irunstart2
    integer(sz), intent(in) :: irunend2
    class(vectar_data_t), pointer, intent(in) :: data1
    integer(sz), intent(in) :: irunstart1
    integer(sz), intent(in) :: irunend1

    integer(sz) :: i, i1, i2
    integer(sz) :: last1, last2

    i = itarget
    last1 = irunend1 + 1
    last2 = irunend2 + 1
    i1 = irunstart1
    i2 = irunstart2
    do while (i /= last2)
       if (i1 == last1) then
          ! The rest of the merger is from data2 and already is in
          ! place.
          if (i /= i2) then
             call error_abort ("internal error in merge_going_leftwards")
          end if
          i = last2
       else if (i2 == last2) then
          ! Copy the remainder of data1.
          do while (i1 /= last1)
             data2%array(i) = data1%array(i1)
             i = i + 1
             i1 = i1 + 1
          end do
       else if (less_than (data2%array(i2)%element, data1%array(i1)%element)) then
          ! The element from data2 is strictly less than the element
          ! from data1, and so belongs in front. Move the element in
          ! data2 leftwards.
          data2%array(i) = data2%array(i2)
          i = i + 1
          i2 = i2 + 1
       else
          ! Copy an element from data1.
          data2%array(i) = data1%array(i1)
          i = i + 1
          i1 = i1 + 1
       end if
    end do
  end subroutine merge_going_leftwards

  recursive subroutine merge_going_rightwards (less_than, data1, irunstart1, irunend1, itarget, &
       &                                       data2, irunstart2, irunend2)
    !
    ! Currently this is just a straightforward stable merge, filling
    ! empty space at the right of data1, with data1 as the run having
    ! priority in the merge.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data1
    integer(sz), intent(in) :: irunstart1
    integer(sz), intent(in) :: irunend1
    integer(sz), intent(in) :: itarget
    class(vectar_data_t), pointer, intent(in) :: data2
    integer(sz), intent(in) :: irunstart2
    integer(sz), intent(in) :: irunend2

    integer(sz) :: i, i1, i2
    integer(sz) :: last1, last2

    i = itarget
    last1 = irunstart1 - 1
    last2 = irunstart2 - 1
    i1 = irunend1
    i2 = irunend2
    do while (i /= last1)
       if (i2 == last2) then
          ! The rest of the merger is from data1 and already is in
          ! place.
          if (i /= i1) then
             call error_abort ("internal error in merge_going_rightwards")
          end if
          i = last1
       else if (i1 == last1) then
          ! Copy the remainder of data2
          do while (i2 /= last2)
             data1%array(i) = data2%array(i2)
             i = i - 1
             i2 = i2 - 1
          end do
       else if (less_than (data2%array(i2)%element, data1%array(i1)%element)) then
          ! The element from data2 is strictly less than the element
          ! from data1, and so belongs in front. Move the element in
          ! data1 rightwards.
          data1%array(i) = data1%array(i1)
          i = i - 1
          i1 = i1 - 1
       else
          ! Copy an element from data2.
          data1%array(i) = data2%array(i2)
          i = i - 1
          i2 = i2 - 1
       end if
    end do
  end subroutine merge_going_rightwards

  recursive subroutine merge_two_runs (less_than, data, i, j, k, workspace)
    !
    ! Merge sorted data[i .. j-1] and sorted data[j .. k], giving
    ! sorted data[i .. k].
    !
    ! `workspace' is a vectar of length at least
    !
    !     floor ((k - i + 1) / 2)
    !
    ! It has to be protected from garbage collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i
    integer(sz), intent(in) :: j
    integer(sz), intent(in) :: k
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz) :: i1, k1
    integer(sz) :: u

    ! Find some elements on the left that definitely will not need to
    ! be moved, because they are less than or equal to the first
    ! element of the right side of the merge.
    i1 = bottenbruch_search (less_than, data, i, j - 1, data%array(j)%element)

    ! Find some elements on the right that definitely will not need to
    ! be moved, because they are greater than or equal to the last
    ! element of the left side of the merge.
    k1 = bottenbruch_search2 (less_than, data, j, k, data%array(j - 1)%element)
    if (k1 /= k) then
       k1 = k1 - 1
    end if

    if (j - i1 < k1 - j) then
       ! The left side is shorter than or equal in length to the right
       ! side. Copy the left side to workspace, then merge leftwards.
       do u = i1, j - 1
          workspace%array(u - i1) = data%array(u)
       end do
       call merge_going_leftwards (less_than, data, i1, j, k1, workspace, 0_sz, j - i1 - 1)
    else
       ! The left side is longer than the right side side. Copy the
       ! right side to workspace, then merge rightwards.
       do u = j, k1
          workspace%array(u - j) = data%array(u)
       end do
       call merge_going_rightwards (less_than, data, i1, j - 1, k1, workspace, 0_sz, k1 - j)
    end if
  end subroutine merge_two_runs

  recursive subroutine restore_run_stack_invariant (less_than, data, run_stack, stack_count, workspace)
    !
    ! Merge run_stack contents until the invariant is met.
    !
    ! The stack invariant is taken from the corrected later versions
    ! of Timsort. See
    ! envisage-project.eu/proving-android-java-and-python-sorting-algorithm-is-broken-and-how-to-fix-it/
    !
    ! Note that the run_stack indexing starts at position 0. All other
    ! entries are the end indices of runs; we put a fake end index in
    ! position 0. The stack_size counter does not count the fake stack
    ! entry.
    !
    ! `workspace' is a vectar of length at least half the length of
    ! the data (rounded down). It has to be protected from garbage
    ! collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(inout) :: run_stack(0:run_stack_size)
    integer, intent(inout) :: stack_count
    class(vectar_data_t), pointer, intent(in) :: workspace

    logical :: the_invariant_is_established
    integer :: n

    n = stack_count
    the_invariant_is_established = .false.
    do while (.not. the_invariant_is_established .and. 2 <= n)
       if (stack_needs_reduction_after_comparing_three_entries (n)) then
          if (runlen (n - 2) < runlen (n)) then
             call merge_two_runs (less_than, data, &
                  &               run_stack (n - 3) + 1, run_stack (n - 2) + 1, run_stack (n - 1), &
                  &               workspace)
             run_stack (n - 2) = run_stack (n - 1)
             run_stack (n - 1) = run_stack (n)
             n = n - 1
          else
             call merge_two_runs (less_than, data, &
                  &               run_stack (n - 2) + 1, run_stack (n - 1) + 1, run_stack (n), &
                  &               workspace)
             run_stack (n - 1) = run_stack (n)
             n = n - 1
          end if
       else if (runlen (n - 1) <= runlen (n)) then
          call merge_two_runs (less_than, data, &
               &               run_stack (n - 2) + 1, run_stack (n - 1) + 1, run_stack (n), &
               &               workspace)
          run_stack (n - 1) = run_stack (n)
          n = n - 1
       else
          the_invariant_is_established = .true.
       end if
    end do
    stack_count = n

  contains

    function stack_needs_reduction_after_comparing_three_entries (n) result (bool)
      integer, intent(in) :: n
      logical :: bool

      if (4 <= n) then
         bool = (runlen (n - 2) <= runlen (n - 1) + runlen (n)) &
              &    .or. (runlen (n - 3) <= runlen (n - 2) + runlen (n - 1))
      else if (3 <= n) then
         bool = (runlen (n - 2) <= runlen (n - 1) + runlen (n))
      else
         bool = .false.
      end if
    end function stack_needs_reduction_after_comparing_three_entries

    function runlen (i) result (len)
      integer, intent(in) :: i
      integer(sz) :: len

      len = run_stack(i) - run_stack(i - 1)
    end function runlen

  end subroutine restore_run_stack_invariant

  recursive subroutine unit_test__restore_run_stack_invariant__test1
    type(gcroot_t) :: vec, workspace_vec
    type(vectar_data_t), pointer :: data, workspace
    integer(sz) :: run_stack(0:run_stack_size)
    integer :: stack_count

    workspace_vec = make_vectar (2000, 1)
    workspace => vectar_data_ptr (workspace_vec)

    run_stack(0) = -1_sz

    ! Test 3 <= n, (runlen (n - 2) <= runlen (n - 1) + runlen (n)),
    ! with runlen(n - 2) <= runlen(n). This should reduce the stack
    ! height by 1, by merging the 2nd and 3rd entries.
    vec = list_to_vectar (append (iota (11, 0, 2), iota (11, 1, 2), iota (39, 22)))
    data => vectar_data_ptr (vec)
    run_stack(1) = 10_sz
    run_stack(2) = 30_sz
    run_stack(3) = 60_sz
    stack_count = 3
    call restore_run_stack_invariant (int_less_than, data, run_stack, stack_count, workspace)
    if (stack_count /= 2) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0010 failed")
    end if
    if (run_stack(0) /= -1_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0020 failed")
    end if
    if (run_stack(1) /= 30_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0030 failed")
    end if
    if (run_stack(2) /= 60_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0040 failed")
    end if
    if (.not. vectar_equal (int_equal, vec, list_to_vectar (iota (61)))) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0050 failed")
    end if

    ! Test 4 <= n, (runlen (n - 2) <= runlen (n - 1) + runlen (n)),
    ! with runlen(n - 2) <= runlen(n). This should reduce the stack
    ! height by 1, by merging the 2nd and 3rd entries. (FIXME: write a
    ! stronger test of correct merge.)
    vec = list_to_vectar (iota (1045, 1))
    data => vectar_data_ptr (vec)
    run_stack(1) = 1000_sz
    run_stack(2) = 1010_sz
    run_stack(3) = 1030_sz
    run_stack(4) = 1045_sz
    stack_count = 4
    call restore_run_stack_invariant (int_less_than, data, run_stack, stack_count, workspace)
    if (stack_count /= 3) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1010 failed")
    end if
    if (run_stack(0) /= -1_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1020 failed")
    end if
    if (run_stack(1) /= 1000_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1040 failed")
    end if
    if (run_stack(2) /= 1030_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1040 failed")
    end if
    if (run_stack(3) /= 1045_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1050 failed")
    end if
    if (.not. vectar_equal (int_equal, vec, list_to_vectar (iota (1045, 1)))) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1060 failed")
    end if

    ! FIXME: Add a test for 4 <= n, (runlen (n - 3) <= runlen (n - 2)
    !        + runlen (n - 1)), with runlen(n - 2) <= runlen(n). This
    !        is the extra condition that was added by Stijn de Gouw et
    !        al. (`Verifying OpenJDK’s Sort Method for Generic
    !        Collections', J Autom Reason. 2019; 62(1): 93–126. DOI:
    !        10.1007/s10817-017-9426-4

  end subroutine unit_test__restore_run_stack_invariant__test1

  recursive subroutine reduce_the_run_stack_to_depth_1 (less_than, data, run_stack, stack_count, workspace)
    !
    ! Merge run_stack contents until the run stack contains just one,
    ! big run (which is the sorted result).
    !
    ! Note that the run_stack indexing starts at position 0. All other
    ! entries are the end indices of runs; we put a fake end index in
    ! position 0. The stack_size counter does not count the fake stack
    ! entry.
    !
    ! `workspace' is a vectar of length at least half the length of
    ! the data (rounded down). It has to be protected from garbage
    ! collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(inout) :: run_stack(0:run_stack_size)
    integer, intent(inout) :: stack_count
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer :: n

    n = stack_count
    do while (n /= 1)
       if (stack_needs_merge_inside_it (n)) then
          call merge_two_runs (less_than, data, &
               &               run_stack (n - 3) + 1, run_stack (n - 2) + 1, run_stack (n - 1), &
               &               workspace)
          run_stack (n - 2) = run_stack (n - 1)
          run_stack (n - 1) = run_stack (n)
          n = n - 1
       else
          call merge_two_runs (less_than, data, &
               &               run_stack (n - 2) + 1, run_stack (n - 1) + 1, run_stack (n), &
               &               workspace)
          run_stack (n - 1) = run_stack (n)
          n = n - 1
       end if
    end do
    stack_count = n

  contains

    function stack_needs_merge_inside_it (n) result (bool)
      integer, intent(in) :: n
      logical :: bool

      if (3 <= n) then
         bool = (runlen (n - 2) < runlen (n))
      else
         bool = .false.
      end if
    end function stack_needs_merge_inside_it

    function runlen (i) result (len)
      integer, intent(in) :: i
      integer(sz) :: len

      len = run_stack(i) - run_stack(i - 1)
    end function runlen

  end subroutine reduce_the_run_stack_to_depth_1

  recursive subroutine unit_test__reduce_the_run_stack_to_depth_1__test1
    type(gcroot_t) :: vec, workspace_vec
    type(vectar_data_t), pointer :: data, workspace
    integer(sz) :: run_stack(0:run_stack_size)
    integer :: stack_count

    workspace_vec = make_vectar (2000, 1)
    workspace => vectar_data_ptr (workspace_vec)

    run_stack(0) = -1_sz

    ! Test 3 <= n, runlen(n - 2) <= runlen(n). This should reduce the
    ! stack height by 1, by merging the 2nd and 3rd entries. Then the
    ! stack height will go to 1, through another merge; however, code
    ! coverage tools can show that first merge happened.
    vec = list_to_vectar (append (iota (11, 0, 2), iota (11, 1, 2), iota (39, 22)))
    data => vectar_data_ptr (vec)
    run_stack(1) = 10_sz
    run_stack(2) = 30_sz
    run_stack(3) = 60_sz
    stack_count = 3
    call reduce_the_run_stack_to_depth_1 (int_less_than, data, run_stack, stack_count, workspace)
    if (stack_count /= 1) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0010 failed")
    end if
    if (run_stack(0) /= -1_sz) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0020 failed")
    end if
    if (run_stack(1) /= 60_sz) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0030 failed")
    end if
    if (.not. vectar_equal (int_equal, vec, list_to_vectar (iota (61)))) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0040 failed")
    end if
  end subroutine unit_test__reduce_the_run_stack_to_depth_1__test1

  function choose_minimum_run_length (data_length) result (min_run_length)
    !
    ! Minimum run length as suggested by Tim Peters.
    !
    ! See
    ! https://en.wikipedia.org/w/index.php?title=Timsort&oldid=1065277889#Minimum_run_size
    !
    !    "The final algorithm takes the six most significant bits of
    !    the size of the array, adds one if any of the remaining bits
    !    are set, and uses that result as the minrun. This algorithm
    !    works for all arrays, including those smaller than 64; for
    !    arrays of size 63 or less, this sets minrun equal to the
    !    array size and Timsort reduces to an insertion sort."
    !
    integer(sz), intent(in) :: data_length
    integer(sz) :: min_run_length

    integer :: total_bits_count
    integer :: leading_zeros_count
    integer :: significant_bits_count
    integer :: right_bits_count
    integer(sz) :: left_bits
    integer(sz) :: right_bits

    total_bits_count = bit_size (data_length)
    leading_zeros_count = leadz (data_length)
    significant_bits_count = total_bits_count - leading_zeros_count
    right_bits_count = max (0, significant_bits_count - 6)
    left_bits = ibits (data_length, right_bits_count, 6)
    right_bits = ibits (data_length, 0, right_bits_count)
    if (right_bits == 0) then
       min_run_length = left_bits
    else
       min_run_length = left_bits + 1
    end if
  end function choose_minimum_run_length

  recursive subroutine stable_mergesort (less_than, data, idatastart, idataend, workspace)
    !
    ! A adaptive natural mergesort using a run stack similar to that
    ! employed by Tim Peters in older versions of the CPython sort
    ! function.
    !
    ! Aside: I would say the name `timsort' is misleading, and has led
    ! to much confusion in the online literature; `timsort' is not a
    ! particular algorithm. Indeed, the algorithm is `simply'
    ! mergesort done iteratively, which I have myself programmed in
    ! years past -- albeit in much simpler form. The enhancements
    ! Python employs change from time to time and are very, very, VERY
    ! clever.
    !
    ! (FIXME: Switch to using the `powersort' merge strategy, from
    ! J. Ian Munro and Sebastian Wild, "Nearly-optimal mergesorts:
    ! fast, practical sorting methods that optimally adapt to existing
    ! runs". Peters cites this paper in the CPython sources, in file
    ! Objects/listsort.txt.
    !
    ! Note Peters points out C has no `leading zeros' primitive;
    ! Fortran, however, does.
    !
    ! Also interesting is that there *are* ways to compute leading
    ! zeros in O(1) time, using only standard C and de Bruijn
    ! sequences. Peters, at least as of Python version 3.11.0a4, seems
    ! unaware of such methods; the existence of such `bit twiddling'
    ! hacks is too poorly known.)
    !
    ! Note that the run_stack indexing starts at position 0. All other
    ! entries are the end indices of runs; we put a fake end index in
    ! position 0. The stack_size counter does not count the fake stack
    ! entry.
    !
    ! `workspace' is a vectar of length at least floor((idataend0 -
    ! idatastart0 + 1)/2). Both `data' and `workspace' must be rooted
    ! to protect them from garbage collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: idatastart
    integer(sz), intent(in) :: idataend
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz) :: run_stack(0:run_stack_size)
    integer :: stack_count
    integer(sz) :: iright
    integer(sz) :: data_length
    integer(sz) :: min_run_length

    data_length = (idataend - idatastart) + 1

    if (data_length <= 1) then
       ! The data is already sorted.
       continue
    else
       min_run_length = choose_minimum_run_length (data_length)

       run_stack(0) = idatastart - 1_sz
       stack_count = 0

       do while (run_stack(stack_count) /= idataend)
          call gather_an_adequately_long_increasing_run (less_than, data, &
               &                                         run_stack(stack_count) + 1, idataend, &
               &                                         min_run_length, iright)

          if (stack_count == run_stack_size) then
             call error_abort ("adaptive mergesort stack size exceeded")
          end if
          stack_count = stack_count + 1
          run_stack(stack_count) = iright

          call restore_run_stack_invariant (less_than, data, run_stack, stack_count, workspace)
       end do

       call reduce_the_run_stack_to_depth_1 (less_than, data, run_stack, stack_count, workspace)

    end if
  end subroutine stable_mergesort

  subroutine vectar_stable_mergesort_unit_tests
    call unit_test__bottenbruch_search__test1
    call unit_test__bottenbruch_search2__test1
    call unit_test__restore_run_stack_invariant__test1
    call unit_test__reduce_the_run_stack_to_depth_1__test1
  end subroutine vectar_stable_mergesort_unit_tests

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module vectars
