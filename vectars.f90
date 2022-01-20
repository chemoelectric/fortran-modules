! -*- F90 -*- 
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
  ! `start [end]' parameters of SRFI-133, without multiplying the
  ! implementations of generic functions.
  !
  ! They are not meant to take the place of `slices', `views', etc.,
  ! although they do let many procedures effectively have `start
  ! [end]' parameters that they do not have in SRFI-133.
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
  public :: vectar0
  public :: vectar1
  public :: vectar2
  public :: vectar3
  public :: vectar4
  public :: vectar5
  public :: vectar6
  public :: vectar7
  public :: vectar8
  public :: vectar9
  public :: vectar10
  public :: vectar11
  public :: vectar12
  public :: vectar13
  public :: vectar14
  public :: vectar15
  public :: vectar16
  public :: vectar17
  public :: vectar18
  public :: vectar19
  public :: vectar20

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
  public :: vectar_append0
  public :: vectar_append1
  public :: vectar_append2
  public :: vectar_append3
  public :: vectar_append4
  public :: vectar_append5
  public :: vectar_append6
  public :: vectar_append7
  public :: vectar_append8
  public :: vectar_append9
  public :: vectar_append10

  ! Return a new vector, appending the contents of the vectars and
  ! vectar ranges given by a list. (In SRFI-133, `vector-concatenate'
  ! cannot append subvectors, the way our function here can.)
  public :: vectar_concatenate

  ! Return the length of a vectar as an INTEGER(SIZE_KIND); if the
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
  public :: vectar_equal0
  public :: vectar_equal1
  public :: vectar_equal2
  public :: vectar_equal3
  public :: vectar_equal4
  public :: vectar_equal5
  public :: vectar_equal6
  public :: vectar_equal7
  public :: vectar_equal8
  public :: vectar_equal9
  public :: vectar_equal10

  ! Generic functions for per-element mapping. These accept vectar
  ! ranges and so are, in that respect, more general than their
  ! SRFI-133 equivalents.
  public :: vectar_map          ! Create a new vectar with mapped
                                ! values.
  public :: vectar_mapx         ! Map the elements in place.

  ! Implementations of vectar_map.
  public :: vectar_map1_subr
  public :: vectar_map2_subr
  public :: vectar_map3_subr
  public :: vectar_map4_subr
  public :: vectar_map5_subr
  public :: vectar_map6_subr
  public :: vectar_map7_subr
  public :: vectar_map8_subr
  public :: vectar_map9_subr
  public :: vectar_map10_subr

  ! Implementations of vectar_mapx.
  public :: vectar_mapx1_subr
  public :: vectar_mapx2_subr
  public :: vectar_mapx3_subr
  public :: vectar_mapx4_subr
  public :: vectar_mapx5_subr
  public :: vectar_mapx6_subr
  public :: vectar_mapx7_subr
  public :: vectar_mapx8_subr
  public :: vectar_mapx9_subr
  public :: vectar_mapx10_subr

  ! Generic side effects iterator. This accepts vectar ranges.
  public :: vectar_for_each

  ! Implementations of vectar_for_each.
  public :: vectar_for_each1_subr
  public :: vectar_for_each2_subr
  public :: vectar_for_each3_subr
  public :: vectar_for_each4_subr
  public :: vectar_for_each5_subr
  public :: vectar_for_each6_subr
  public :: vectar_for_each7_subr
  public :: vectar_for_each8_subr
  public :: vectar_for_each9_subr
  public :: vectar_for_each10_subr

  ! Generic function. See SRFI-133. This accepts vectar ranges.
  public :: vectar_cumulate

  ! Implementations of vectar_cumulate.
  public :: vectar_cumulate_subr

  ! Generic function: count how many times the predicate is
  ! satisfied. This accepts vectar ranges.
  public :: vectar_count

  ! Implementations of vectar_count.
  public :: vectar_count1
  public :: vectar_count2
  public :: vectar_count3
  public :: vectar_count4
  public :: vectar_count5
  public :: vectar_count6
  public :: vectar_count7
  public :: vectar_count8
  public :: vectar_count9
  public :: vectar_count10

  ! Generic function: left-to-right fold. This accepts vectar ranges.
  public :: vectar_fold

  ! Implementations of vectar_fold.
  public :: vectar_fold1_subr
  public :: vectar_fold2_subr
  public :: vectar_fold3_subr
  public :: vectar_fold4_subr
  public :: vectar_fold5_subr
  public :: vectar_fold6_subr
  public :: vectar_fold7_subr
  public :: vectar_fold8_subr
  public :: vectar_fold9_subr
  public :: vectar_fold10_subr

  ! Generic function: right-to-left fold. This accepts vectar ranges.
  public :: vectar_fold_right

  ! Implementations of vectar_fold_right.
  public :: vectar_fold_right1_subr
  public :: vectar_fold_right2_subr
  public :: vectar_fold_right3_subr
  public :: vectar_fold_right4_subr
  public :: vectar_fold_right5_subr
  public :: vectar_fold_right6_subr
  public :: vectar_fold_right7_subr
  public :: vectar_fold_right8_subr
  public :: vectar_fold_right9_subr
  public :: vectar_fold_right10_subr

  ! Generic subroutine: create a new vectar by unfolding
  ! left-to-right.
  public :: vectar_unfold

  ! Implementations of vectar_unfold.
  public :: vectar_unfold0_subr_size_kind
  public :: vectar_unfold0_subr_int
  public :: vectar_unfold1_subr_size_kind
  public :: vectar_unfold1_subr_int
  public :: vectar_unfold2_subr_size_kind
  public :: vectar_unfold2_subr_int
  public :: vectar_unfold3_subr_size_kind
  public :: vectar_unfold3_subr_int
  public :: vectar_unfold4_subr_size_kind
  public :: vectar_unfold4_subr_int
  public :: vectar_unfold5_subr_size_kind
  public :: vectar_unfold5_subr_int
  public :: vectar_unfold6_subr_size_kind
  public :: vectar_unfold6_subr_int
  public :: vectar_unfold7_subr_size_kind
  public :: vectar_unfold7_subr_int
  public :: vectar_unfold8_subr_size_kind
  public :: vectar_unfold8_subr_int
  public :: vectar_unfold9_subr_size_kind
  public :: vectar_unfold9_subr_int
  public :: vectar_unfold10_subr_size_kind
  public :: vectar_unfold10_subr_int

  ! Generic subroutine: unfold left-to-right into an existing vectar
  ! or vectar range.
  public :: vectar_unfoldx

  ! Generic subroutine: create a new vectar by unfolding
  ! right-to-left.
  public :: vectar_unfold_right

  ! Implementations of vectar_unfold.
  public :: vectar_unfold_right0_subr_size_kind
  public :: vectar_unfold_right0_subr_int
  public :: vectar_unfold_right1_subr_size_kind
  public :: vectar_unfold_right1_subr_int
  public :: vectar_unfold_right2_subr_size_kind
  public :: vectar_unfold_right2_subr_int
  public :: vectar_unfold_right3_subr_size_kind
  public :: vectar_unfold_right3_subr_int
  public :: vectar_unfold_right4_subr_size_kind
  public :: vectar_unfold_right4_subr_int
  public :: vectar_unfold_right5_subr_size_kind
  public :: vectar_unfold_right5_subr_int
  public :: vectar_unfold_right6_subr_size_kind
  public :: vectar_unfold_right6_subr_int
  public :: vectar_unfold_right7_subr_size_kind
  public :: vectar_unfold_right7_subr_int
  public :: vectar_unfold_right8_subr_size_kind
  public :: vectar_unfold_right8_subr_int
  public :: vectar_unfold_right9_subr_size_kind
  public :: vectar_unfold_right9_subr_int
  public :: vectar_unfold_right10_subr_size_kind
  public :: vectar_unfold_right10_subr_int

  ! Generic subroutine: unfold right-to-left into an existing vectar
  ! or vectar range.
  public :: vectar_unfold_rightx

  ! Implementations of vectar_unfoldx.
  public :: vectar_unfold_rightx0_subr
  public :: vectar_unfold_rightx1_subr
  public :: vectar_unfold_rightx2_subr
  public :: vectar_unfold_rightx3_subr
  public :: vectar_unfold_rightx4_subr
  public :: vectar_unfold_rightx5_subr
  public :: vectar_unfold_rightx6_subr
  public :: vectar_unfold_rightx7_subr
  public :: vectar_unfold_rightx8_subr
  public :: vectar_unfold_rightx9_subr
  public :: vectar_unfold_rightx10_subr

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
  public :: vectar_index0_1
  public :: vectar_index0_2
  public :: vectar_index0_3
  public :: vectar_index0_4
  public :: vectar_index0_5
  public :: vectar_index0_6
  public :: vectar_index0_7
  public :: vectar_index0_8
  public :: vectar_index0_9
  public :: vectar_index0_10
  public :: vectar_index1_1
  public :: vectar_index1_2
  public :: vectar_index1_3
  public :: vectar_index1_4
  public :: vectar_index1_5
  public :: vectar_index1_6
  public :: vectar_index1_7
  public :: vectar_index1_8
  public :: vectar_index1_9
  public :: vectar_index1_10
  public :: vectar_indexn_1
  public :: vectar_indexn_2
  public :: vectar_indexn_3
  public :: vectar_indexn_4
  public :: vectar_indexn_5
  public :: vectar_indexn_6
  public :: vectar_indexn_7
  public :: vectar_indexn_8
  public :: vectar_indexn_9
  public :: vectar_indexn_10

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
  public :: vectar_index_right0_1
  public :: vectar_index_right0_2
  public :: vectar_index_right0_3
  public :: vectar_index_right0_4
  public :: vectar_index_right0_5
  public :: vectar_index_right0_6
  public :: vectar_index_right0_7
  public :: vectar_index_right0_8
  public :: vectar_index_right0_9
  public :: vectar_index_right0_10
  public :: vectar_index_right1_1
  public :: vectar_index_right1_2
  public :: vectar_index_right1_3
  public :: vectar_index_right1_4
  public :: vectar_index_right1_5
  public :: vectar_index_right1_6
  public :: vectar_index_right1_7
  public :: vectar_index_right1_8
  public :: vectar_index_right1_9
  public :: vectar_index_right1_10
  public :: vectar_index_rightn_1
  public :: vectar_index_rightn_2
  public :: vectar_index_rightn_3
  public :: vectar_index_rightn_4
  public :: vectar_index_rightn_5
  public :: vectar_index_rightn_6
  public :: vectar_index_rightn_7
  public :: vectar_index_rightn_8
  public :: vectar_index_rightn_9
  public :: vectar_index_rightn_10

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
  public :: vectar_skip0_1
  public :: vectar_skip0_2
  public :: vectar_skip0_3
  public :: vectar_skip0_4
  public :: vectar_skip0_5
  public :: vectar_skip0_6
  public :: vectar_skip0_7
  public :: vectar_skip0_8
  public :: vectar_skip0_9
  public :: vectar_skip0_10
  public :: vectar_skip1_1
  public :: vectar_skip1_2
  public :: vectar_skip1_3
  public :: vectar_skip1_4
  public :: vectar_skip1_5
  public :: vectar_skip1_6
  public :: vectar_skip1_7
  public :: vectar_skip1_8
  public :: vectar_skip1_9
  public :: vectar_skip1_10
  public :: vectar_skipn_1
  public :: vectar_skipn_2
  public :: vectar_skipn_3
  public :: vectar_skipn_4
  public :: vectar_skipn_5
  public :: vectar_skipn_6
  public :: vectar_skipn_7
  public :: vectar_skipn_8
  public :: vectar_skipn_9
  public :: vectar_skipn_10
  public :: vectar_skip_right0_1
  public :: vectar_skip_right0_2
  public :: vectar_skip_right0_3
  public :: vectar_skip_right0_4
  public :: vectar_skip_right0_5
  public :: vectar_skip_right0_6
  public :: vectar_skip_right0_7
  public :: vectar_skip_right0_8
  public :: vectar_skip_right0_9
  public :: vectar_skip_right0_10
  public :: vectar_skip_right1_1
  public :: vectar_skip_right1_2
  public :: vectar_skip_right1_3
  public :: vectar_skip_right1_4
  public :: vectar_skip_right1_5
  public :: vectar_skip_right1_6
  public :: vectar_skip_right1_7
  public :: vectar_skip_right1_8
  public :: vectar_skip_right1_9
  public :: vectar_skip_right1_10
  public :: vectar_skip_rightn_1
  public :: vectar_skip_rightn_2
  public :: vectar_skip_rightn_3
  public :: vectar_skip_rightn_4
  public :: vectar_skip_rightn_5
  public :: vectar_skip_rightn_6
  public :: vectar_skip_rightn_7
  public :: vectar_skip_rightn_8
  public :: vectar_skip_rightn_9
  public :: vectar_skip_rightn_10

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
  public :: vectar_some1
  public :: vectar_some2
  public :: vectar_some3
  public :: vectar_some4
  public :: vectar_some5
  public :: vectar_some6
  public :: vectar_some7
  public :: vectar_some8
  public :: vectar_some9
  public :: vectar_some10
  public :: vectar_some_map1_subr
  public :: vectar_some_map2_subr
  public :: vectar_some_map3_subr
  public :: vectar_some_map4_subr
  public :: vectar_some_map5_subr
  public :: vectar_some_map6_subr
  public :: vectar_some_map7_subr
  public :: vectar_some_map8_subr
  public :: vectar_some_map9_subr
  public :: vectar_some_map10_subr
  public :: vectar_every1
  public :: vectar_every2
  public :: vectar_every3
  public :: vectar_every4
  public :: vectar_every5
  public :: vectar_every6
  public :: vectar_every7
  public :: vectar_every8
  public :: vectar_every9
  public :: vectar_every10
  public :: vectar_every_map1_subr
  public :: vectar_every_map2_subr
  public :: vectar_every_map3_subr
  public :: vectar_every_map4_subr
  public :: vectar_every_map5_subr
  public :: vectar_every_map6_subr
  public :: vectar_every_map7_subr
  public :: vectar_every_map8_subr
  public :: vectar_every_map9_subr
  public :: vectar_every_map10_subr

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
     module procedure vectar0
     module procedure vectar1
     module procedure vectar2
     module procedure vectar3
     module procedure vectar4
     module procedure vectar5
     module procedure vectar6
     module procedure vectar7
     module procedure vectar8
     module procedure vectar9
     module procedure vectar10
     module procedure vectar11
     module procedure vectar12
     module procedure vectar13
     module procedure vectar14
     module procedure vectar15
     module procedure vectar16
     module procedure vectar17
     module procedure vectar18
     module procedure vectar19
     module procedure vectar20
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
     module procedure vectar_append0
     module procedure vectar_append1
     module procedure vectar_append2
     module procedure vectar_append3
     module procedure vectar_append4
     module procedure vectar_append5
     module procedure vectar_append6
     module procedure vectar_append7
     module procedure vectar_append8
     module procedure vectar_append9
     module procedure vectar_append10
  end interface vectar_append

  interface vectar_map
     module procedure vectar_map1_subr
     module procedure vectar_map2_subr
     module procedure vectar_map3_subr
     module procedure vectar_map4_subr
     module procedure vectar_map5_subr
     module procedure vectar_map6_subr
     module procedure vectar_map7_subr
     module procedure vectar_map8_subr
     module procedure vectar_map9_subr
     module procedure vectar_map10_subr
  end interface vectar_map

  interface vectar_mapx
     module procedure vectar_mapx1_subr
     module procedure vectar_mapx2_subr
     module procedure vectar_mapx3_subr
     module procedure vectar_mapx4_subr
     module procedure vectar_mapx5_subr
     module procedure vectar_mapx6_subr
     module procedure vectar_mapx7_subr
     module procedure vectar_mapx8_subr
     module procedure vectar_mapx9_subr
     module procedure vectar_mapx10_subr
  end interface vectar_mapx

  interface vectar_for_each
     module procedure vectar_for_each1_subr
     module procedure vectar_for_each2_subr
     module procedure vectar_for_each3_subr
     module procedure vectar_for_each4_subr
     module procedure vectar_for_each5_subr
     module procedure vectar_for_each6_subr
     module procedure vectar_for_each7_subr
     module procedure vectar_for_each8_subr
     module procedure vectar_for_each9_subr
     module procedure vectar_for_each10_subr
  end interface vectar_for_each

  interface vectar_cumulate
     module procedure vectar_cumulate_subr
  end interface vectar_cumulate

  interface vectar_count
     module procedure vectar_count1
     module procedure vectar_count2
     module procedure vectar_count3
     module procedure vectar_count4
     module procedure vectar_count5
     module procedure vectar_count6
     module procedure vectar_count7
     module procedure vectar_count8
     module procedure vectar_count9
     module procedure vectar_count10
  end interface vectar_count

  interface vectar_fold
     module procedure vectar_fold1_subr
     module procedure vectar_fold2_subr
     module procedure vectar_fold3_subr
     module procedure vectar_fold4_subr
     module procedure vectar_fold5_subr
     module procedure vectar_fold6_subr
     module procedure vectar_fold7_subr
     module procedure vectar_fold8_subr
     module procedure vectar_fold9_subr
     module procedure vectar_fold10_subr
  end interface vectar_fold

  interface vectar_fold_right
     module procedure vectar_fold_right1_subr
     module procedure vectar_fold_right2_subr
     module procedure vectar_fold_right3_subr
     module procedure vectar_fold_right4_subr
     module procedure vectar_fold_right5_subr
     module procedure vectar_fold_right6_subr
     module procedure vectar_fold_right7_subr
     module procedure vectar_fold_right8_subr
     module procedure vectar_fold_right9_subr
     module procedure vectar_fold_right10_subr
  end interface vectar_fold_right

  interface vectar_unfold
     module procedure vectar_unfold0_subr_size_kind
     module procedure vectar_unfold0_subr_int
     module procedure vectar_unfold1_subr_size_kind
     module procedure vectar_unfold1_subr_int
     module procedure vectar_unfold2_subr_size_kind
     module procedure vectar_unfold2_subr_int
     module procedure vectar_unfold3_subr_size_kind
     module procedure vectar_unfold3_subr_int
     module procedure vectar_unfold4_subr_size_kind
     module procedure vectar_unfold4_subr_int
     module procedure vectar_unfold5_subr_size_kind
     module procedure vectar_unfold5_subr_int
     module procedure vectar_unfold6_subr_size_kind
     module procedure vectar_unfold6_subr_int
     module procedure vectar_unfold7_subr_size_kind
     module procedure vectar_unfold7_subr_int
     module procedure vectar_unfold8_subr_size_kind
     module procedure vectar_unfold8_subr_int
     module procedure vectar_unfold9_subr_size_kind
     module procedure vectar_unfold9_subr_int
     module procedure vectar_unfold10_subr_size_kind
     module procedure vectar_unfold10_subr_int
  end interface vectar_unfold

  interface vectar_unfoldx
     module procedure vectar_unfoldx0_subr
     module procedure vectar_unfoldx1_subr
     module procedure vectar_unfoldx2_subr
     module procedure vectar_unfoldx3_subr
     module procedure vectar_unfoldx4_subr
     module procedure vectar_unfoldx5_subr
     module procedure vectar_unfoldx6_subr
     module procedure vectar_unfoldx7_subr
     module procedure vectar_unfoldx8_subr
     module procedure vectar_unfoldx9_subr
     module procedure vectar_unfoldx10_subr
  end interface vectar_unfoldx

  interface vectar_unfold_right
     module procedure vectar_unfold_right0_subr_size_kind
     module procedure vectar_unfold_right0_subr_int
     module procedure vectar_unfold_right1_subr_size_kind
     module procedure vectar_unfold_right1_subr_int
     module procedure vectar_unfold_right2_subr_size_kind
     module procedure vectar_unfold_right2_subr_int
     module procedure vectar_unfold_right3_subr_size_kind
     module procedure vectar_unfold_right3_subr_int
     module procedure vectar_unfold_right4_subr_size_kind
     module procedure vectar_unfold_right4_subr_int
     module procedure vectar_unfold_right5_subr_size_kind
     module procedure vectar_unfold_right5_subr_int
     module procedure vectar_unfold_right6_subr_size_kind
     module procedure vectar_unfold_right6_subr_int
     module procedure vectar_unfold_right7_subr_size_kind
     module procedure vectar_unfold_right7_subr_int
     module procedure vectar_unfold_right8_subr_size_kind
     module procedure vectar_unfold_right8_subr_int
     module procedure vectar_unfold_right9_subr_size_kind
     module procedure vectar_unfold_right9_subr_int
     module procedure vectar_unfold_right10_subr_size_kind
     module procedure vectar_unfold_right10_subr_int
  end interface vectar_unfold_right

  interface vectar_unfold_rightx
     module procedure vectar_unfold_rightx0_subr
     module procedure vectar_unfold_rightx1_subr
     module procedure vectar_unfold_rightx2_subr
     module procedure vectar_unfold_rightx3_subr
     module procedure vectar_unfold_rightx4_subr
     module procedure vectar_unfold_rightx5_subr
     module procedure vectar_unfold_rightx6_subr
     module procedure vectar_unfold_rightx7_subr
     module procedure vectar_unfold_rightx8_subr
     module procedure vectar_unfold_rightx9_subr
     module procedure vectar_unfold_rightx10_subr
  end interface vectar_unfold_rightx

  interface vectar_equal
     module procedure vectar_equal0
     module procedure vectar_equal1
     module procedure vectar_equal2
     module procedure vectar_equal3
     module procedure vectar_equal4
     module procedure vectar_equal5
     module procedure vectar_equal6
     module procedure vectar_equal7
     module procedure vectar_equal8
     module procedure vectar_equal9
     module procedure vectar_equal10
  end interface vectar_equal

  interface vectar_index0
     module procedure vectar_index0_1
     module procedure vectar_index0_2
     module procedure vectar_index0_3
     module procedure vectar_index0_4
     module procedure vectar_index0_5
     module procedure vectar_index0_6
     module procedure vectar_index0_7
     module procedure vectar_index0_8
     module procedure vectar_index0_9
     module procedure vectar_index0_10
  end interface vectar_index0

  interface vectar_index1
     module procedure vectar_index1_1
     module procedure vectar_index1_2
     module procedure vectar_index1_3
     module procedure vectar_index1_4
     module procedure vectar_index1_5
     module procedure vectar_index1_6
     module procedure vectar_index1_7
     module procedure vectar_index1_8
     module procedure vectar_index1_9
     module procedure vectar_index1_10
  end interface vectar_index1

  interface vectar_indexn
     module procedure vectar_indexn_1
     module procedure vectar_indexn_2
     module procedure vectar_indexn_3
     module procedure vectar_indexn_4
     module procedure vectar_indexn_5
     module procedure vectar_indexn_6
     module procedure vectar_indexn_7
     module procedure vectar_indexn_8
     module procedure vectar_indexn_9
     module procedure vectar_indexn_10
  end interface vectar_indexn

  interface vectar_index_right0
     module procedure vectar_index_right0_1
     module procedure vectar_index_right0_2
     module procedure vectar_index_right0_3
     module procedure vectar_index_right0_4
     module procedure vectar_index_right0_5
     module procedure vectar_index_right0_6
     module procedure vectar_index_right0_7
     module procedure vectar_index_right0_8
     module procedure vectar_index_right0_9
     module procedure vectar_index_right0_10
  end interface vectar_index_right0

  interface vectar_index_right1
     module procedure vectar_index_right1_1
     module procedure vectar_index_right1_2
     module procedure vectar_index_right1_3
     module procedure vectar_index_right1_4
     module procedure vectar_index_right1_5
     module procedure vectar_index_right1_6
     module procedure vectar_index_right1_7
     module procedure vectar_index_right1_8
     module procedure vectar_index_right1_9
     module procedure vectar_index_right1_10
  end interface vectar_index_right1

  interface vectar_index_rightn
     module procedure vectar_index_rightn_1
     module procedure vectar_index_rightn_2
     module procedure vectar_index_rightn_3
     module procedure vectar_index_rightn_4
     module procedure vectar_index_rightn_5
     module procedure vectar_index_rightn_6
     module procedure vectar_index_rightn_7
     module procedure vectar_index_rightn_8
     module procedure vectar_index_rightn_9
     module procedure vectar_index_rightn_10
  end interface vectar_index_rightn

  interface vectar_skip0
     module procedure vectar_skip0_1
     module procedure vectar_skip0_2
     module procedure vectar_skip0_3
     module procedure vectar_skip0_4
     module procedure vectar_skip0_5
     module procedure vectar_skip0_6
     module procedure vectar_skip0_7
     module procedure vectar_skip0_8
     module procedure vectar_skip0_9
     module procedure vectar_skip0_10
  end interface vectar_skip0

  interface vectar_skip1
     module procedure vectar_skip1_1
     module procedure vectar_skip1_2
     module procedure vectar_skip1_3
     module procedure vectar_skip1_4
     module procedure vectar_skip1_5
     module procedure vectar_skip1_6
     module procedure vectar_skip1_7
     module procedure vectar_skip1_8
     module procedure vectar_skip1_9
     module procedure vectar_skip1_10
  end interface vectar_skip1

  interface vectar_skipn
     module procedure vectar_skipn_1
     module procedure vectar_skipn_2
     module procedure vectar_skipn_3
     module procedure vectar_skipn_4
     module procedure vectar_skipn_5
     module procedure vectar_skipn_6
     module procedure vectar_skipn_7
     module procedure vectar_skipn_8
     module procedure vectar_skipn_9
     module procedure vectar_skipn_10
  end interface vectar_skipn

  interface vectar_skip_right0
     module procedure vectar_skip_right0_1
     module procedure vectar_skip_right0_2
     module procedure vectar_skip_right0_3
     module procedure vectar_skip_right0_4
     module procedure vectar_skip_right0_5
     module procedure vectar_skip_right0_6
     module procedure vectar_skip_right0_7
     module procedure vectar_skip_right0_8
     module procedure vectar_skip_right0_9
     module procedure vectar_skip_right0_10
  end interface vectar_skip_right0

  interface vectar_skip_right1
     module procedure vectar_skip_right1_1
     module procedure vectar_skip_right1_2
     module procedure vectar_skip_right1_3
     module procedure vectar_skip_right1_4
     module procedure vectar_skip_right1_5
     module procedure vectar_skip_right1_6
     module procedure vectar_skip_right1_7
     module procedure vectar_skip_right1_8
     module procedure vectar_skip_right1_9
     module procedure vectar_skip_right1_10
  end interface vectar_skip_right1

  interface vectar_skip_rightn
     module procedure vectar_skip_rightn_1
     module procedure vectar_skip_rightn_2
     module procedure vectar_skip_rightn_3
     module procedure vectar_skip_rightn_4
     module procedure vectar_skip_rightn_5
     module procedure vectar_skip_rightn_6
     module procedure vectar_skip_rightn_7
     module procedure vectar_skip_rightn_8
     module procedure vectar_skip_rightn_9
     module procedure vectar_skip_rightn_10
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
     module procedure vectar_some1
     module procedure vectar_some2
     module procedure vectar_some3
     module procedure vectar_some4
     module procedure vectar_some5
     module procedure vectar_some6
     module procedure vectar_some7
     module procedure vectar_some8
     module procedure vectar_some9
     module procedure vectar_some10
  end interface vectar_some

  interface vectar_some_map
     module procedure vectar_some_map1_subr
     module procedure vectar_some_map2_subr
     module procedure vectar_some_map3_subr
     module procedure vectar_some_map4_subr
     module procedure vectar_some_map5_subr
     module procedure vectar_some_map6_subr
     module procedure vectar_some_map7_subr
     module procedure vectar_some_map8_subr
     module procedure vectar_some_map9_subr
     module procedure vectar_some_map10_subr
  end interface vectar_some_map

  interface vectar_every
     module procedure vectar_every1
     module procedure vectar_every2
     module procedure vectar_every3
     module procedure vectar_every4
     module procedure vectar_every5
     module procedure vectar_every6
     module procedure vectar_every7
     module procedure vectar_every8
     module procedure vectar_every9
     module procedure vectar_every10
  end interface vectar_every

  interface vectar_every_map
     module procedure vectar_every_map1_subr
     module procedure vectar_every_map2_subr
     module procedure vectar_every_map3_subr
     module procedure vectar_every_map4_subr
     module procedure vectar_every_map5_subr
     module procedure vectar_every_map6_subr
     module procedure vectar_every_map7_subr
     module procedure vectar_every_map8_subr
     module procedure vectar_every_map9_subr
     module procedure vectar_every_map10_subr
  end interface vectar_every_map

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Types for predicates.
!!!

  public :: vectar_predicate1_t ! A predicate taking 1 argument.
  public :: vectar_predicate2_t ! A predicate taking 2 arguments.
  public :: vectar_predicate3_t ! A predicate taking 3 arguments.
  public :: vectar_predicate4_t ! A predicate taking 4 arguments.
  public :: vectar_predicate5_t ! A predicate taking 5 arguments.
  public :: vectar_predicate6_t ! A predicate taking 6 arguments.
  public :: vectar_predicate7_t ! A predicate taking 7 arguments.
  public :: vectar_predicate8_t ! A predicate taking 8 arguments.
  public :: vectar_predicate9_t ! A predicate taking 9 arguments.
  public :: vectar_predicate10_t ! A predicate taking 10 arguments.

  abstract interface
     recursive function vectar_predicate1_t (x1) result (bool)
       class(*), intent(in) :: x1
       logical :: bool
     end function vectar_predicate1_t
     recursive function vectar_predicate2_t (x1, x2) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       logical :: bool
     end function vectar_predicate2_t
     recursive function vectar_predicate3_t (x1, x2, x3) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       logical :: bool
     end function vectar_predicate3_t
     recursive function vectar_predicate4_t (x1, x2, x3, x4) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       logical :: bool
     end function vectar_predicate4_t
     recursive function vectar_predicate5_t (x1, x2, x3, x4, x5) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       logical :: bool
     end function vectar_predicate5_t
     recursive function vectar_predicate6_t (x1, x2, x3, x4, x5, &
          &                                  x6) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       logical :: bool
     end function vectar_predicate6_t
     recursive function vectar_predicate7_t (x1, x2, x3, x4, x5, &
          &                                  x6, x7) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       logical :: bool
     end function vectar_predicate7_t
     recursive function vectar_predicate8_t (x1, x2, x3, x4, x5, &
          &                                  x6, x7, x8) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       class(*), intent(in) :: x8
       logical :: bool
     end function vectar_predicate8_t
     recursive function vectar_predicate9_t (x1, x2, x3, x4, x5, &
          &                                  x6, x7, x8, x9) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       class(*), intent(in) :: x8
       class(*), intent(in) :: x9
       logical :: bool
     end function vectar_predicate9_t
     recursive function vectar_predicate10_t (x1, x2, x3, x4, x5, &
          &                                  x6, x7, x8, x9, x10) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       class(*), intent(in) :: x8
       class(*), intent(in) :: x9
       class(*), intent(in) :: x10
       logical :: bool
     end function vectar_predicate10_t
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

  public :: vectar_map1_subr_t
  public :: vectar_map2_subr_t
  public :: vectar_map3_subr_t
  public :: vectar_map4_subr_t
  public :: vectar_map5_subr_t
  public :: vectar_map6_subr_t
  public :: vectar_map7_subr_t
  public :: vectar_map8_subr_t
  public :: vectar_map9_subr_t
  public :: vectar_map10_subr_t

  abstract interface
     recursive subroutine vectar_map1_subr_t (input1, output)
       class(*), intent(in) :: input1
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map1_subr_t
     recursive subroutine vectar_map2_subr_t (input1, input2, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map2_subr_t
     recursive subroutine vectar_map3_subr_t (input1, input2, input3, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map3_subr_t
     recursive subroutine vectar_map4_subr_t (input1, input2, input3, input4, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map4_subr_t
     recursive subroutine vectar_map5_subr_t (input1, input2, input3, input4, input5, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map5_subr_t
     recursive subroutine vectar_map6_subr_t (input1, input2, input3, input4, input5, &
          &                                   input6, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map6_subr_t
     recursive subroutine vectar_map7_subr_t (input1, input2, input3, input4, input5, &
          &                                   input6, input7, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map7_subr_t
     recursive subroutine vectar_map8_subr_t (input1, input2, input3, input4, input5, &
          &                                   input6, input7, input8, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map8_subr_t
     recursive subroutine vectar_map9_subr_t (input1, input2, input3, input4, input5, &
          &                                   input6, input7, input8, input9, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), intent(in) :: input9
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map9_subr_t
     recursive subroutine vectar_map10_subr_t (input1, input2, input3, input4, input5, &
          &                                   input6, input7, input8, input9, input10, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), intent(in) :: input9
       class(*), intent(in) :: input10
       class(*), allocatable, intent(out) :: output
     end subroutine vectar_map10_subr_t
  end interface

!!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!
!!! Types for the side-effects subroutine to a for-each procedure.
!!!

  public :: vectar_side_effects1_t
  public :: vectar_side_effects2_t
  public :: vectar_side_effects3_t
  public :: vectar_side_effects4_t
  public :: vectar_side_effects5_t
  public :: vectar_side_effects6_t
  public :: vectar_side_effects7_t
  public :: vectar_side_effects8_t
  public :: vectar_side_effects9_t
  public :: vectar_side_effects10_t

  abstract interface
     recursive subroutine vectar_side_effects1_t (input1)
       class(*), intent(in) :: input1
     end subroutine vectar_side_effects1_t
     recursive subroutine vectar_side_effects2_t (input1, input2)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
     end subroutine vectar_side_effects2_t
     recursive subroutine vectar_side_effects3_t (input1, input2, input3)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
     end subroutine vectar_side_effects3_t
     recursive subroutine vectar_side_effects4_t (input1, input2, input3, input4)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
     end subroutine vectar_side_effects4_t
     recursive subroutine vectar_side_effects5_t (input1, input2, input3, input4, input5)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
     end subroutine vectar_side_effects5_t
     recursive subroutine vectar_side_effects6_t (input1, input2, input3, input4, input5, &
          &                                       input6)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
     end subroutine vectar_side_effects6_t
     recursive subroutine vectar_side_effects7_t (input1, input2, input3, input4, input5, &
          &                                       input6, input7)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
     end subroutine vectar_side_effects7_t
     recursive subroutine vectar_side_effects8_t (input1, input2, input3, input4, input5, &
          &                                       input6, input7, input8)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
     end subroutine vectar_side_effects8_t
     recursive subroutine vectar_side_effects9_t (input1, input2, input3, input4, input5, &
          &                                       input6, input7, input8, input9)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), intent(in) :: input9
     end subroutine vectar_side_effects9_t
     recursive subroutine vectar_side_effects10_t (input1, input2, input3, input4, input5, &
          &                                       input6, input7, input8, input9, input10)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), intent(in) :: input9
       class(*), intent(in) :: input10
     end subroutine vectar_side_effects10_t
  end interface

!!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!
!!! Types for the kons subroutine in folds, vectar_cumulate, etc.
!!!

  public :: vectar_kons1_subr_t
  public :: vectar_kons2_subr_t
  public :: vectar_kons3_subr_t
  public :: vectar_kons4_subr_t
  public :: vectar_kons5_subr_t
  public :: vectar_kons6_subr_t
  public :: vectar_kons7_subr_t
  public :: vectar_kons8_subr_t
  public :: vectar_kons9_subr_t
  public :: vectar_kons10_subr_t

  abstract interface
     recursive subroutine vectar_kons1_subr_t (state, val1, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons1_subr_t
     recursive subroutine vectar_kons2_subr_t (state, val1, val2, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons2_subr_t
     recursive subroutine vectar_kons3_subr_t (state, val1, val2, val3, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons3_subr_t
     recursive subroutine vectar_kons4_subr_t (state, val1, val2, val3, val4, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), intent(in) :: val4
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons4_subr_t
     recursive subroutine vectar_kons5_subr_t (state, val1, val2, val3, val4, val5, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), intent(in) :: val4
       class(*), intent(in) :: val5
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons5_subr_t
     recursive subroutine vectar_kons6_subr_t (state, val1, val2, val3, val4, val5, &
          &                                    val6, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), intent(in) :: val4
       class(*), intent(in) :: val5
       class(*), intent(in) :: val6
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons6_subr_t
     recursive subroutine vectar_kons7_subr_t (state, val1, val2, val3, val4, val5, &
          &                                    val6, val7, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), intent(in) :: val4
       class(*), intent(in) :: val5
       class(*), intent(in) :: val6
       class(*), intent(in) :: val7
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons7_subr_t
     recursive subroutine vectar_kons8_subr_t (state, val1, val2, val3, val4, val5, &
          &                                    val6, val7, val8, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), intent(in) :: val4
       class(*), intent(in) :: val5
       class(*), intent(in) :: val6
       class(*), intent(in) :: val7
       class(*), intent(in) :: val8
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons8_subr_t
     recursive subroutine vectar_kons9_subr_t (state, val1, val2, val3, val4, val5, &
          &                                    val6, val7, val8, val9, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), intent(in) :: val4
       class(*), intent(in) :: val5
       class(*), intent(in) :: val6
       class(*), intent(in) :: val7
       class(*), intent(in) :: val8
       class(*), intent(in) :: val9
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons9_subr_t
     recursive subroutine vectar_kons10_subr_t (state, val1, val2, val3, val4, val5, &
          &                                    val6, val7, val8, val9, val10, kons_result)
       class(*), intent(in) :: state
       class(*), intent(in) :: val1
       class(*), intent(in) :: val2
       class(*), intent(in) :: val3
       class(*), intent(in) :: val4
       class(*), intent(in) :: val5
       class(*), intent(in) :: val6
       class(*), intent(in) :: val7
       class(*), intent(in) :: val8
       class(*), intent(in) :: val9
       class(*), intent(in) :: val10
       class(*), allocatable, intent(out) :: kons_result
     end subroutine vectar_kons10_subr_t
  end interface

!!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!
!!! Types for the f subroutine in unfolds.
!!!

  public :: vectar_unfold0_f_subr_t
  public :: vectar_unfold1_f_subr_t
  public :: vectar_unfold2_f_subr_t
  public :: vectar_unfold3_f_subr_t
  public :: vectar_unfold4_f_subr_t
  public :: vectar_unfold5_f_subr_t
  public :: vectar_unfold6_f_subr_t
  public :: vectar_unfold7_f_subr_t
  public :: vectar_unfold8_f_subr_t
  public :: vectar_unfold9_f_subr_t
  public :: vectar_unfold10_f_subr_t

  abstract interface
     recursive subroutine vectar_unfold0_f_subr_t (index, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold0_f_subr_t
     recursive subroutine vectar_unfold1_f_subr_t (index, seed1, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold1_f_subr_t
     recursive subroutine vectar_unfold2_f_subr_t (index, seed1, seed2, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold2_f_subr_t
     recursive subroutine vectar_unfold3_f_subr_t (index, seed1, seed2, seed3, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold3_f_subr_t
     recursive subroutine vectar_unfold4_f_subr_t (index, seed1, seed2, seed3, seed4, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       class(*), allocatable, intent(inout) :: seed4
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold4_f_subr_t
     recursive subroutine vectar_unfold5_f_subr_t (index, seed1, seed2, seed3, seed4, seed5, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       class(*), allocatable, intent(inout) :: seed4
       class(*), allocatable, intent(inout) :: seed5
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold5_f_subr_t
     recursive subroutine vectar_unfold6_f_subr_t (index, seed1, seed2, seed3, seed4, seed5, &
          &                                        seed6, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       class(*), allocatable, intent(inout) :: seed4
       class(*), allocatable, intent(inout) :: seed5
       class(*), allocatable, intent(inout) :: seed6
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold6_f_subr_t
     recursive subroutine vectar_unfold7_f_subr_t (index, seed1, seed2, seed3, seed4, seed5, &
          &                                        seed6, seed7, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       class(*), allocatable, intent(inout) :: seed4
       class(*), allocatable, intent(inout) :: seed5
       class(*), allocatable, intent(inout) :: seed6
       class(*), allocatable, intent(inout) :: seed7
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold7_f_subr_t
     recursive subroutine vectar_unfold8_f_subr_t (index, seed1, seed2, seed3, seed4, seed5, &
          &                                        seed6, seed7, seed8, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       class(*), allocatable, intent(inout) :: seed4
       class(*), allocatable, intent(inout) :: seed5
       class(*), allocatable, intent(inout) :: seed6
       class(*), allocatable, intent(inout) :: seed7
       class(*), allocatable, intent(inout) :: seed8
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold8_f_subr_t
     recursive subroutine vectar_unfold9_f_subr_t (index, seed1, seed2, seed3, seed4, seed5, &
          &                                        seed6, seed7, seed8, seed9, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       class(*), allocatable, intent(inout) :: seed4
       class(*), allocatable, intent(inout) :: seed5
       class(*), allocatable, intent(inout) :: seed6
       class(*), allocatable, intent(inout) :: seed7
       class(*), allocatable, intent(inout) :: seed8
       class(*), allocatable, intent(inout) :: seed9
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold9_f_subr_t
     recursive subroutine vectar_unfold10_f_subr_t (index, seed1, seed2, seed3, seed4, seed5, &
          &                                        seed6, seed7, seed8, seed9, seed10, element)
       import sz
       ! Zero-based index.
       integer(sz), intent(in) :: index
       ! One set of parameters for both the old and new seed values.
       class(*), allocatable, intent(inout) :: seed1
       class(*), allocatable, intent(inout) :: seed2
       class(*), allocatable, intent(inout) :: seed3
       class(*), allocatable, intent(inout) :: seed4
       class(*), allocatable, intent(inout) :: seed5
       class(*), allocatable, intent(inout) :: seed6
       class(*), allocatable, intent(inout) :: seed7
       class(*), allocatable, intent(inout) :: seed8
       class(*), allocatable, intent(inout) :: seed9
       class(*), allocatable, intent(inout) :: seed10
       ! The new element.
       class(*), allocatable, intent(out) :: element
     end subroutine vectar_unfold10_f_subr_t
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

  function vectar1 (obj1) result (vec)
    class(*), intent(in) :: obj1
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 1_sz
    allocate (data%array(0_sz:0_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar1

  function vectar2 (obj1, obj2) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 2_sz
    allocate (data%array(0_sz:1_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar2

  function vectar3 (obj1, obj2, obj3) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 3_sz
    allocate (data%array(0_sz:2_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar3

  function vectar4 (obj1, obj2, obj3, obj4) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 4_sz
    allocate (data%array(0_sz:3_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar4

  function vectar5 (obj1, obj2, obj3, obj4, obj5) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 5_sz
    allocate (data%array(0_sz:4_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar5

  function vectar6 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 6_sz
    allocate (data%array(0_sz:5_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar6

  function vectar7 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 7_sz
    allocate (data%array(0_sz:6_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar7

  function vectar8 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 8_sz
    allocate (data%array(0_sz:7_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar8

  function vectar9 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 9_sz
    allocate (data%array(0_sz:8_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar9

  function vectar10 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 10_sz
    allocate (data%array(0_sz:9_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar10

  function vectar11 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 11_sz
    allocate (data%array(0_sz:10_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar11

  function vectar12 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 12_sz
    allocate (data%array(0_sz:11_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar12

  function vectar13 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 13_sz
    allocate (data%array(0_sz:12_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar13

  function vectar14 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 14_sz
    allocate (data%array(0_sz:13_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar14

  function vectar15 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 15_sz
    allocate (data%array(0_sz:14_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar15

  function vectar16 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 16_sz
    allocate (data%array(0_sz:15_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar16

  function vectar17 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 17_sz
    allocate (data%array(0_sz:16_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar17

  function vectar18 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17, obj18) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    class(*), intent(in) :: obj18
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 18_sz
    allocate (data%array(0_sz:17_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    data%array(17_sz) = vectar_element_t (.autoval. obj18)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar18

  function vectar19 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17, obj18, obj19) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    class(*), intent(in) :: obj18
    class(*), intent(in) :: obj19
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 19_sz
    allocate (data%array(0_sz:18_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    data%array(17_sz) = vectar_element_t (.autoval. obj18)
    data%array(18_sz) = vectar_element_t (.autoval. obj19)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar19

  function vectar20 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17, obj18, obj19, obj20) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    class(*), intent(in) :: obj18
    class(*), intent(in) :: obj19
    class(*), intent(in) :: obj20
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    class(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 20_sz
    allocate (data%array(0_sz:19_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    data%array(17_sz) = vectar_element_t (.autoval. obj18)
    data%array(18_sz) = vectar_element_t (.autoval. obj19)
    data%array(19_sz) = vectar_element_t (.autoval. obj20)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar20


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

  function vectar_append1 (vec1) result (vec_a)
    class(*), intent(in) :: vec1
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do

  end function vectar_append1
  function vectar_append2 (vec1, vec2) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do

  end function vectar_append2
  function vectar_append3 (vec1, vec2, vec3) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do

  end function vectar_append3
  function vectar_append4 (vec1, vec2, vec3, vec4) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: src4
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()
    vecr4 = vec4
    src4 => vectar_data_ptr (vecr4)
    len_vec_a = len_vec_a + vecr4%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = vecr4%istart0(), vecr4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do

  end function vectar_append4
  function vectar_append5 (vec1, vec2, vec3, vec4, vec5) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: src4
    class(vectar_data_t), pointer :: src5
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()
    vecr4 = vec4
    src4 => vectar_data_ptr (vecr4)
    len_vec_a = len_vec_a + vecr4%length()
    vecr5 = vec5
    src5 => vectar_data_ptr (vecr5)
    len_vec_a = len_vec_a + vecr5%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = vecr4%istart0(), vecr4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = vecr5%istart0(), vecr5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do

  end function vectar_append5
  function vectar_append6 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: src4
    class(vectar_data_t), pointer :: src5
    class(vectar_data_t), pointer :: src6
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()
    vecr4 = vec4
    src4 => vectar_data_ptr (vecr4)
    len_vec_a = len_vec_a + vecr4%length()
    vecr5 = vec5
    src5 => vectar_data_ptr (vecr5)
    len_vec_a = len_vec_a + vecr5%length()
    vecr6 = vec6
    src6 => vectar_data_ptr (vecr6)
    len_vec_a = len_vec_a + vecr6%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = vecr4%istart0(), vecr4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = vecr5%istart0(), vecr5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = vecr6%istart0(), vecr6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do

  end function vectar_append6
  function vectar_append7 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: src4
    class(vectar_data_t), pointer :: src5
    class(vectar_data_t), pointer :: src6
    class(vectar_data_t), pointer :: src7
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()
    vecr4 = vec4
    src4 => vectar_data_ptr (vecr4)
    len_vec_a = len_vec_a + vecr4%length()
    vecr5 = vec5
    src5 => vectar_data_ptr (vecr5)
    len_vec_a = len_vec_a + vecr5%length()
    vecr6 = vec6
    src6 => vectar_data_ptr (vecr6)
    len_vec_a = len_vec_a + vecr6%length()
    vecr7 = vec7
    src7 => vectar_data_ptr (vecr7)
    len_vec_a = len_vec_a + vecr7%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = vecr4%istart0(), vecr4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = vecr5%istart0(), vecr5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = vecr6%istart0(), vecr6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = vecr7%istart0(), vecr7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do

  end function vectar_append7
  function vectar_append8 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: src4
    class(vectar_data_t), pointer :: src5
    class(vectar_data_t), pointer :: src6
    class(vectar_data_t), pointer :: src7
    class(vectar_data_t), pointer :: src8
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()
    vecr4 = vec4
    src4 => vectar_data_ptr (vecr4)
    len_vec_a = len_vec_a + vecr4%length()
    vecr5 = vec5
    src5 => vectar_data_ptr (vecr5)
    len_vec_a = len_vec_a + vecr5%length()
    vecr6 = vec6
    src6 => vectar_data_ptr (vecr6)
    len_vec_a = len_vec_a + vecr6%length()
    vecr7 = vec7
    src7 => vectar_data_ptr (vecr7)
    len_vec_a = len_vec_a + vecr7%length()
    vecr8 = vec8
    src8 => vectar_data_ptr (vecr8)
    len_vec_a = len_vec_a + vecr8%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = vecr4%istart0(), vecr4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = vecr5%istart0(), vecr5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = vecr6%istart0(), vecr6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = vecr7%istart0(), vecr7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = vecr8%istart0(), vecr8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do

  end function vectar_append8
  function vectar_append9 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: src4
    class(vectar_data_t), pointer :: src5
    class(vectar_data_t), pointer :: src6
    class(vectar_data_t), pointer :: src7
    class(vectar_data_t), pointer :: src8
    class(vectar_data_t), pointer :: src9
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()
    vecr4 = vec4
    src4 => vectar_data_ptr (vecr4)
    len_vec_a = len_vec_a + vecr4%length()
    vecr5 = vec5
    src5 => vectar_data_ptr (vecr5)
    len_vec_a = len_vec_a + vecr5%length()
    vecr6 = vec6
    src6 => vectar_data_ptr (vecr6)
    len_vec_a = len_vec_a + vecr6%length()
    vecr7 = vec7
    src7 => vectar_data_ptr (vecr7)
    len_vec_a = len_vec_a + vecr7%length()
    vecr8 = vec8
    src8 => vectar_data_ptr (vecr8)
    len_vec_a = len_vec_a + vecr8%length()
    vecr9 = vec9
    src9 => vectar_data_ptr (vecr9)
    len_vec_a = len_vec_a + vecr9%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = vecr4%istart0(), vecr4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = vecr5%istart0(), vecr5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = vecr6%istart0(), vecr6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = vecr7%istart0(), vecr7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = vecr8%istart0(), vecr8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = vecr9%istart0(), vecr9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do

  end function vectar_append9
  function vectar_append10 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: src1
    class(vectar_data_t), pointer :: src2
    class(vectar_data_t), pointer :: src3
    class(vectar_data_t), pointer :: src4
    class(vectar_data_t), pointer :: src5
    class(vectar_data_t), pointer :: src6
    class(vectar_data_t), pointer :: src7
    class(vectar_data_t), pointer :: src8
    class(vectar_data_t), pointer :: src9
    class(vectar_data_t), pointer :: src10
    class(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    vecr1 = vec1
    src1 => vectar_data_ptr (vecr1)
    len_vec_a = len_vec_a + vecr1%length()
    vecr2 = vec2
    src2 => vectar_data_ptr (vecr2)
    len_vec_a = len_vec_a + vecr2%length()
    vecr3 = vec3
    src3 => vectar_data_ptr (vecr3)
    len_vec_a = len_vec_a + vecr3%length()
    vecr4 = vec4
    src4 => vectar_data_ptr (vecr4)
    len_vec_a = len_vec_a + vecr4%length()
    vecr5 = vec5
    src5 => vectar_data_ptr (vecr5)
    len_vec_a = len_vec_a + vecr5%length()
    vecr6 = vec6
    src6 => vectar_data_ptr (vecr6)
    len_vec_a = len_vec_a + vecr6%length()
    vecr7 = vec7
    src7 => vectar_data_ptr (vecr7)
    len_vec_a = len_vec_a + vecr7%length()
    vecr8 = vec8
    src8 => vectar_data_ptr (vecr8)
    len_vec_a = len_vec_a + vecr8%length()
    vecr9 = vec9
    src9 => vectar_data_ptr (vecr9)
    len_vec_a = len_vec_a + vecr9%length()
    vecr10 = vec10
    src10 => vectar_data_ptr (vecr10)
    len_vec_a = len_vec_a + vecr10%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = vecr1%istart0(), vecr1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = vecr2%istart0(), vecr2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = vecr3%istart0(), vecr3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = vecr4%istart0(), vecr4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = vecr5%istart0(), vecr5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = vecr6%istart0(), vecr6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = vecr7%istart0(), vecr7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = vecr8%istart0(), vecr8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = vecr9%istart0(), vecr9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = vecr10%istart0(), vecr10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do

  end function vectar_append10
  function vectar_concatenate (vectars) result (vec_c)
    class(*), intent(in) :: vectars
    type(vectar_t) :: vec_c

    integer(sz) :: num_vectars

    num_vectars = length (vectars)

    select case (num_vectars)
    case (0)
       vec_c = vectar ()
    case (1)
       block
         class(*), allocatable :: vec1
         call unlist (vectars, vec1)
         vec_c = vectar_append (vec1)
       end block
    case (2)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         call unlist (vectars, vec1, vec2)
         vec_c = vectar_append (vec1, vec2)
       end block
    case (3)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         call unlist (vectars, vec1, vec2, vec3)
         vec_c = vectar_append (vec1, vec2, vec3)
       end block
    case (4)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         call unlist (vectars, vec1, vec2, vec3, vec4)
         vec_c = vectar_append (vec1, vec2, vec3, vec4)
       end block
    case (5)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5)
       end block
    case (6)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6)
       end block
    case (7)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7)
       end block
    case (8)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8)
       end block
    case (9)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9)
       end block
    case (10)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10)
       end block
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

  recursive function vectar_equal2 (equal, vec1, vec2) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2

    vecr1 = vec1
    vecr2 = vec2

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_equal2

  recursive function vectar_equal3 (equal, vec1, vec2, vec3) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_equal3

  recursive function vectar_equal4 (equal, vec1, vec2, vec3, vec4) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: i0_4
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()
    data4 => vectar_data_ptr (.val. vec4_root)
    i0_4 = vecr4%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else if (vecr4%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data3%array(i0_3 + i)%element, data4%array(i0_4 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_equal4

  recursive function vectar_equal5 (equal, vec1, vec2, vec3, vec4, vec5) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: i0_4
    integer(sz) :: i0_5
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()
    data4 => vectar_data_ptr (.val. vec4_root)
    i0_4 = vecr4%istart0()
    data5 => vectar_data_ptr (.val. vec5_root)
    i0_5 = vecr5%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else if (vecr4%length() /= vecr1_length) then
       bool = .false.
    else if (vecr5%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data3%array(i0_3 + i)%element, data4%array(i0_4 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data4%array(i0_4 + i)%element, data5%array(i0_5 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_equal5

  recursive function vectar_equal6 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: i0_4
    integer(sz) :: i0_5
    integer(sz) :: i0_6
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()
    data4 => vectar_data_ptr (.val. vec4_root)
    i0_4 = vecr4%istart0()
    data5 => vectar_data_ptr (.val. vec5_root)
    i0_5 = vecr5%istart0()
    data6 => vectar_data_ptr (.val. vec6_root)
    i0_6 = vecr6%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else if (vecr4%length() /= vecr1_length) then
       bool = .false.
    else if (vecr5%length() /= vecr1_length) then
       bool = .false.
    else if (vecr6%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data3%array(i0_3 + i)%element, data4%array(i0_4 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data4%array(i0_4 + i)%element, data5%array(i0_5 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data5%array(i0_5 + i)%element, data6%array(i0_6 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_equal6

  recursive function vectar_equal7 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: i0_4
    integer(sz) :: i0_5
    integer(sz) :: i0_6
    integer(sz) :: i0_7
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()
    data4 => vectar_data_ptr (.val. vec4_root)
    i0_4 = vecr4%istart0()
    data5 => vectar_data_ptr (.val. vec5_root)
    i0_5 = vecr5%istart0()
    data6 => vectar_data_ptr (.val. vec6_root)
    i0_6 = vecr6%istart0()
    data7 => vectar_data_ptr (.val. vec7_root)
    i0_7 = vecr7%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else if (vecr4%length() /= vecr1_length) then
       bool = .false.
    else if (vecr5%length() /= vecr1_length) then
       bool = .false.
    else if (vecr6%length() /= vecr1_length) then
       bool = .false.
    else if (vecr7%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data3%array(i0_3 + i)%element, data4%array(i0_4 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data4%array(i0_4 + i)%element, data5%array(i0_5 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data5%array(i0_5 + i)%element, data6%array(i0_6 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data6%array(i0_6 + i)%element, data7%array(i0_7 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_equal7

  recursive function vectar_equal8 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: i0_4
    integer(sz) :: i0_5
    integer(sz) :: i0_6
    integer(sz) :: i0_7
    integer(sz) :: i0_8
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()
    data4 => vectar_data_ptr (.val. vec4_root)
    i0_4 = vecr4%istart0()
    data5 => vectar_data_ptr (.val. vec5_root)
    i0_5 = vecr5%istart0()
    data6 => vectar_data_ptr (.val. vec6_root)
    i0_6 = vecr6%istart0()
    data7 => vectar_data_ptr (.val. vec7_root)
    i0_7 = vecr7%istart0()
    data8 => vectar_data_ptr (.val. vec8_root)
    i0_8 = vecr8%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else if (vecr4%length() /= vecr1_length) then
       bool = .false.
    else if (vecr5%length() /= vecr1_length) then
       bool = .false.
    else if (vecr6%length() /= vecr1_length) then
       bool = .false.
    else if (vecr7%length() /= vecr1_length) then
       bool = .false.
    else if (vecr8%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data3%array(i0_3 + i)%element, data4%array(i0_4 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data4%array(i0_4 + i)%element, data5%array(i0_5 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data5%array(i0_5 + i)%element, data6%array(i0_6 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data6%array(i0_6 + i)%element, data7%array(i0_7 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data7%array(i0_7 + i)%element, data8%array(i0_8 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_equal8

  recursive function vectar_equal9 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: i0_4
    integer(sz) :: i0_5
    integer(sz) :: i0_6
    integer(sz) :: i0_7
    integer(sz) :: i0_8
    integer(sz) :: i0_9
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()
    data4 => vectar_data_ptr (.val. vec4_root)
    i0_4 = vecr4%istart0()
    data5 => vectar_data_ptr (.val. vec5_root)
    i0_5 = vecr5%istart0()
    data6 => vectar_data_ptr (.val. vec6_root)
    i0_6 = vecr6%istart0()
    data7 => vectar_data_ptr (.val. vec7_root)
    i0_7 = vecr7%istart0()
    data8 => vectar_data_ptr (.val. vec8_root)
    i0_8 = vecr8%istart0()
    data9 => vectar_data_ptr (.val. vec9_root)
    i0_9 = vecr9%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else if (vecr4%length() /= vecr1_length) then
       bool = .false.
    else if (vecr5%length() /= vecr1_length) then
       bool = .false.
    else if (vecr6%length() /= vecr1_length) then
       bool = .false.
    else if (vecr7%length() /= vecr1_length) then
       bool = .false.
    else if (vecr8%length() /= vecr1_length) then
       bool = .false.
    else if (vecr9%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data3%array(i0_3 + i)%element, data4%array(i0_4 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data4%array(i0_4 + i)%element, data5%array(i0_5 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data5%array(i0_5 + i)%element, data6%array(i0_6 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data6%array(i0_6 + i)%element, data7%array(i0_7 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data7%array(i0_7 + i)%element, data8%array(i0_8 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data8%array(i0_8 + i)%element, data9%array(i0_9 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_equal9

  recursive function vectar_equal10 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    logical :: bool

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    integer(sz) :: i0_1
    integer(sz) :: i0_2
    integer(sz) :: i0_3
    integer(sz) :: i0_4
    integer(sz) :: i0_5
    integer(sz) :: i0_6
    integer(sz) :: i0_7
    integer(sz) :: i0_8
    integer(sz) :: i0_9
    integer(sz) :: i0_10
    integer(sz) :: vecr1_length
    integer(sz) :: i

    ! Protection from the garbage collector.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10

    data1 => vectar_data_ptr (.val. vec1_root)
    i0_1 = vecr1%istart0()
    data2 => vectar_data_ptr (.val. vec2_root)
    i0_2 = vecr2%istart0()
    data3 => vectar_data_ptr (.val. vec3_root)
    i0_3 = vecr3%istart0()
    data4 => vectar_data_ptr (.val. vec4_root)
    i0_4 = vecr4%istart0()
    data5 => vectar_data_ptr (.val. vec5_root)
    i0_5 = vecr5%istart0()
    data6 => vectar_data_ptr (.val. vec6_root)
    i0_6 = vecr6%istart0()
    data7 => vectar_data_ptr (.val. vec7_root)
    i0_7 = vecr7%istart0()
    data8 => vectar_data_ptr (.val. vec8_root)
    i0_8 = vecr8%istart0()
    data9 => vectar_data_ptr (.val. vec9_root)
    i0_9 = vecr9%istart0()
    data10 => vectar_data_ptr (.val. vec10_root)
    i0_10 = vecr10%istart0()

    vecr1_length = vecr1%length()
    if (vecr2%length() /= vecr1_length) then
       bool = .false.
    else if (vecr3%length() /= vecr1_length) then
       bool = .false.
    else if (vecr4%length() /= vecr1_length) then
       bool = .false.
    else if (vecr5%length() /= vecr1_length) then
       bool = .false.
    else if (vecr6%length() /= vecr1_length) then
       bool = .false.
    else if (vecr7%length() /= vecr1_length) then
       bool = .false.
    else if (vecr8%length() /= vecr1_length) then
       bool = .false.
    else if (vecr9%length() /= vecr1_length) then
       bool = .false.
    else if (vecr10%length() /= vecr1_length) then
       bool = .false.
    else
       bool = .true.
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data1%array(i0_1 + i)%element, data2%array(i0_2 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data2%array(i0_2 + i)%element, data3%array(i0_3 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data3%array(i0_3 + i)%element, data4%array(i0_4 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data4%array(i0_4 + i)%element, data5%array(i0_5 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data5%array(i0_5 + i)%element, data6%array(i0_6 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data6%array(i0_6 + i)%element, data7%array(i0_7 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data7%array(i0_7 + i)%element, data8%array(i0_8 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data8%array(i0_8 + i)%element, data9%array(i0_9 + i)%element)
             i = i + 1
          end do
       end if
       if (bool) then
          i = 0
          do while (bool .and. i < vecr1_length)
             bool = equal (data9%array(i0_9 + i)%element, data10%array(i0_10 + i)%element)
             i = i + 1
          end do
       end if
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_equal10


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_map1_subr (subr, vec1) result (vec_m)
    procedure(vectar_map1_subr_t) :: subr
    class(*), intent(in) :: vec1
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    class(*), allocatable :: result_value

    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       call subr (data1%array(i1)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
  end function vectar_map1_subr

  recursive function vectar_map2_subr (subr, vec1, vec2) result (vec_m)
    procedure(vectar_map2_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_map2_subr

  recursive function vectar_map3_subr (subr, vec1, vec2, vec3) result (vec_m)
    procedure(vectar_map3_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_map3_subr

  recursive function vectar_map4_subr (subr, vec1, vec2, vec3, vec4) result (vec_m)
    procedure(vectar_map4_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_map4_subr

  recursive function vectar_map5_subr (subr, vec1, vec2, vec3, vec4, vec5) result (vec_m)
    procedure(vectar_map5_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_map5_subr

  recursive function vectar_map6_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                               vec6) result (vec_m)
    procedure(vectar_map6_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_map6_subr

  recursive function vectar_map7_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                               vec6, vec7) result (vec_m)
    procedure(vectar_map7_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_map7_subr

  recursive function vectar_map8_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                               vec6, vec7, vec8) result (vec_m)
    procedure(vectar_map8_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_map8_subr

  recursive function vectar_map9_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                               vec6, vec7, vec8, vec9) result (vec_m)
    procedure(vectar_map9_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        data9%array(i9)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_map9_subr

  recursive function vectar_map10_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                               vec6, vec7, vec8, vec9, vec10) result (vec_m)
    procedure(vectar_map10_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    type(vectar_t) :: vec_m

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    type(gcroot_t) :: vec_m_root
    class(vectar_data_t), pointer :: result_data
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10
    class(*), allocatable :: result_value

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    vec_m_root = make_vectar (min_length)
    result_data => vectar_data_ptr (vec_m_root)
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        data9%array(i9)%element, data10%array(i10)%element, &
         &        result_value)
       result_data%array(i)%element = result_value
    end do

    vec_m = .val. vec_m_root

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_map10_subr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine vectar_mapx1_subr (subr, vec1)
    procedure(vectar_map1_subr_t) :: subr
    class(*), intent(in) :: vec1

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       call subr (data1%array(i1)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
  end subroutine vectar_mapx1_subr

  recursive subroutine vectar_mapx2_subr (subr, vec1, vec2)
    procedure(vectar_map2_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
  end subroutine vectar_mapx2_subr

  recursive subroutine vectar_mapx3_subr (subr, vec1, vec2, vec3)
    procedure(vectar_map3_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end subroutine vectar_mapx3_subr

  recursive subroutine vectar_mapx4_subr (subr, vec1, vec2, vec3, vec4)
    procedure(vectar_map4_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end subroutine vectar_mapx4_subr

  recursive subroutine vectar_mapx5_subr (subr, vec1, vec2, vec3, vec4, vec5)
    procedure(vectar_map5_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end subroutine vectar_mapx5_subr

  recursive subroutine vectar_mapx6_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                  vec6)
    procedure(vectar_map6_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end subroutine vectar_mapx6_subr

  recursive subroutine vectar_mapx7_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                  vec6, vec7)
    procedure(vectar_map7_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end subroutine vectar_mapx7_subr

  recursive subroutine vectar_mapx8_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                  vec6, vec7, vec8)
    procedure(vectar_map8_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end subroutine vectar_mapx8_subr

  recursive subroutine vectar_mapx9_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                  vec6, vec7, vec8, vec9)
    procedure(vectar_map9_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        data9%array(i9)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end subroutine vectar_mapx9_subr

  recursive subroutine vectar_mapx10_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                  vec6, vec7, vec8, vec9, vec10)
    procedure(vectar_map10_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    class(*), allocatable :: result_element
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        data9%array(i9)%element, data10%array(i10)%element, &
         &        result_element)
       data1%array(i1)%element = result_element
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end subroutine vectar_mapx10_subr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine vectar_for_each1_subr (subr, vec1)
    procedure(vectar_side_effects1_t) :: subr
    class(*), intent(in) :: vec1

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       call subr (data1%array(i1)%element)
    end do

    call vec1_root%discard
  end subroutine vectar_for_each1_subr

  recursive subroutine vectar_for_each2_subr (subr, vec1, vec2)
    procedure(vectar_side_effects2_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
  end subroutine vectar_for_each2_subr

  recursive subroutine vectar_for_each3_subr (subr, vec1, vec2, vec3)
    procedure(vectar_side_effects3_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end subroutine vectar_for_each3_subr

  recursive subroutine vectar_for_each4_subr (subr, vec1, vec2, vec3, vec4)
    procedure(vectar_side_effects4_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end subroutine vectar_for_each4_subr

  recursive subroutine vectar_for_each5_subr (subr, vec1, vec2, vec3, vec4, vec5)
    procedure(vectar_side_effects5_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end subroutine vectar_for_each5_subr

  recursive subroutine vectar_for_each6_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6)
    procedure(vectar_side_effects6_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end subroutine vectar_for_each6_subr

  recursive subroutine vectar_for_each7_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7)
    procedure(vectar_side_effects7_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end subroutine vectar_for_each7_subr

  recursive subroutine vectar_for_each8_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7, vec8)
    procedure(vectar_side_effects8_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end subroutine vectar_for_each8_subr

  recursive subroutine vectar_for_each9_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7, vec8, vec9)
    procedure(vectar_side_effects9_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        data9%array(i9)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end subroutine vectar_for_each9_subr

  recursive subroutine vectar_for_each10_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7, vec8, vec9, vec10)
    procedure(vectar_side_effects10_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
         &        data3%array(i3)%element, data4%array(i4)%element, &
         &        data5%array(i5)%element, data6%array(i6)%element, &
         &        data7%array(i7)%element, data8%array(i8)%element, &
         &        data9%array(i9)%element, data10%array(i10)%element)
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end subroutine vectar_for_each10_subr

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

  recursive function vectar_count1 (pred, vec1) result (count)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       if (pred (data1%array(i1)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
  end function vectar_count1

  recursive function vectar_count2 (pred, vec1, vec2) result (count)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_count2

  recursive function vectar_count3 (pred, vec1, vec2, vec3) result (count)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_count3

  recursive function vectar_count4 (pred, vec1, vec2, vec3, vec4) result (count)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element, data4%array(i4)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_count4

  recursive function vectar_count5 (pred, vec1, vec2, vec3, vec4, vec5) result (count)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element, data4%array(i4)%element, &
            &        data5%array(i5)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_count5

  recursive function vectar_count6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6) result (count)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element, data4%array(i4)%element, &
            &        data5%array(i5)%element, data6%array(i6)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_count6

  recursive function vectar_count7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7) result (count)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element, data4%array(i4)%element, &
            &        data5%array(i5)%element, data6%array(i6)%element, &
            &        data7%array(i7)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_count7

  recursive function vectar_count8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8) result (count)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element, data4%array(i4)%element, &
            &        data5%array(i5)%element, data6%array(i6)%element, &
            &        data7%array(i7)%element, data8%array(i8)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_count8

  recursive function vectar_count9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9) result (count)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element, data4%array(i4)%element, &
            &        data5%array(i5)%element, data6%array(i6)%element, &
            &        data7%array(i7)%element, data8%array(i8)%element, &
            &        data9%array(i9)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_count9

  recursive function vectar_count10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10) result (count)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: count

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10

    ! Protect against garbage collections instigated by subr.
    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    count = 0_sz
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       if (pred (data1%array(i1)%element, data2%array(i2)%element, &
            &        data3%array(i3)%element, data4%array(i4)%element, &
            &        data5%array(i5)%element, data6%array(i6)%element, &
            &        data7%array(i7)%element, data8%array(i8)%element, &
            &        data9%array(i9)%element, data10%array(i10)%element)) then
          count = count + 1
       end if
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_count10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_fold1_subr (kons, knil, vec1) result (vec_f)
    procedure(vectar_kons1_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1

    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       call kons (.val. state, data1%array(i1)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
  end function vectar_fold1_subr

  recursive function vectar_fold2_subr (kons, knil, vec1, vec2) result (vec_f)
    procedure(vectar_kons2_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2

    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_fold2_subr

  recursive function vectar_fold3_subr (kons, knil, vec1, vec2, vec3) result (vec_f)
    procedure(vectar_kons3_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_fold3_subr

  recursive function vectar_fold4_subr (kons, knil, vec1, vec2, vec3, vec4) result (vec_f)
    procedure(vectar_kons4_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_fold4_subr

  recursive function vectar_fold5_subr (kons, knil, vec1, vec2, vec3, vec4, vec5) result (vec_f)
    procedure(vectar_kons5_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_fold5_subr

  recursive function vectar_fold6_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                vec6) result (vec_f)
    procedure(vectar_kons6_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_fold6_subr

  recursive function vectar_fold7_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                vec6, vec7) result (vec_f)
    procedure(vectar_kons7_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_fold7_subr

  recursive function vectar_fold8_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                vec6, vec7, vec8) result (vec_f)
    procedure(vectar_kons8_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_fold8_subr

  recursive function vectar_fold9_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                vec6, vec7, vec8, vec9) result (vec_f)
    procedure(vectar_kons9_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_fold9_subr

  recursive function vectar_fold10_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                vec6, vec7, vec8, vec9, vec10) result (vec_f)
    procedure(vectar_kons10_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_fold10_subr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_fold_right1_subr (kons, knil, vec1) result (vec_f)
    procedure(vectar_kons1_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1

    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
  end function vectar_fold_right1_subr

  recursive function vectar_fold_right2_subr (kons, knil, vec1, vec2) result (vec_f)
    procedure(vectar_kons2_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2

    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_fold_right2_subr

  recursive function vectar_fold_right3_subr (kons, knil, vec1, vec2, vec3) result (vec_f)
    procedure(vectar_kons3_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_fold_right3_subr

  recursive function vectar_fold_right4_subr (kons, knil, vec1, vec2, vec3, vec4) result (vec_f)
    procedure(vectar_kons4_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       i4 = vecr4%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_fold_right4_subr

  recursive function vectar_fold_right5_subr (kons, knil, vec1, vec2, vec3, vec4, vec5) result (vec_f)
    procedure(vectar_kons5_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       i4 = vecr4%istart0() + (min_length - 1_sz - i)
       i5 = vecr5%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_fold_right5_subr

  recursive function vectar_fold_right6_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6) result (vec_f)
    procedure(vectar_kons6_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       i4 = vecr4%istart0() + (min_length - 1_sz - i)
       i5 = vecr5%istart0() + (min_length - 1_sz - i)
       i6 = vecr6%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_fold_right6_subr

  recursive function vectar_fold_right7_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7) result (vec_f)
    procedure(vectar_kons7_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       i4 = vecr4%istart0() + (min_length - 1_sz - i)
       i5 = vecr5%istart0() + (min_length - 1_sz - i)
       i6 = vecr6%istart0() + (min_length - 1_sz - i)
       i7 = vecr7%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_fold_right7_subr

  recursive function vectar_fold_right8_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7, vec8) result (vec_f)
    procedure(vectar_kons8_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       i4 = vecr4%istart0() + (min_length - 1_sz - i)
       i5 = vecr5%istart0() + (min_length - 1_sz - i)
       i6 = vecr6%istart0() + (min_length - 1_sz - i)
       i7 = vecr7%istart0() + (min_length - 1_sz - i)
       i8 = vecr8%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_fold_right8_subr

  recursive function vectar_fold_right9_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7, vec8, vec9) result (vec_f)
    procedure(vectar_kons9_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       i4 = vecr4%istart0() + (min_length - 1_sz - i)
       i5 = vecr5%istart0() + (min_length - 1_sz - i)
       i6 = vecr6%istart0() + (min_length - 1_sz - i)
       i7 = vecr7%istart0() + (min_length - 1_sz - i)
       i8 = vecr8%istart0() + (min_length - 1_sz - i)
       i9 = vecr9%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_fold_right9_subr

  recursive function vectar_fold_right10_subr (kons, knil, vec1, vec2, vec3, vec4, vec5, &
       &                                      vec6, vec7, vec8, vec9, vec10) result (vec_f)
    procedure(vectar_kons10_subr_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), allocatable :: vec_f

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    type(gcroot_t) :: state
    class(*), allocatable :: next_state
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    state = knil
    do i = 0_sz, min_length - 1_sz
       i1 = vecr1%istart0() + (min_length - 1_sz - i)
       i2 = vecr2%istart0() + (min_length - 1_sz - i)
       i3 = vecr3%istart0() + (min_length - 1_sz - i)
       i4 = vecr4%istart0() + (min_length - 1_sz - i)
       i5 = vecr5%istart0() + (min_length - 1_sz - i)
       i6 = vecr6%istart0() + (min_length - 1_sz - i)
       i7 = vecr7%istart0() + (min_length - 1_sz - i)
       i8 = vecr8%istart0() + (min_length - 1_sz - i)
       i9 = vecr9%istart0() + (min_length - 1_sz - i)
       i10 = vecr10%istart0() + (min_length - 1_sz - i)
       call kons (.val. state, data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element, next_state)
       state = next_state
    end do
    vec_f = .val. state

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_fold_right10_subr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine vectar_unfoldx0_subr (f, vec)
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
    do index = 0_sz, vecr%length() - 1_sz
       call f (index, element)
       data%array(i0 + index)%element = element
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx0_subr

  recursive subroutine vectar_unfoldx1_subr (f, vec, &
       initial_seed1)
    procedure(vectar_unfold1_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    type(gcroot_t) :: seed1_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       call f (index, seed1, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx1_subr

  recursive subroutine vectar_unfoldx2_subr (f, vec, &
       initial_seed1, initial_seed2)
    procedure(vectar_unfold2_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       call f (index, seed1, seed2, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx2_subr

  recursive subroutine vectar_unfoldx3_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3)
    procedure(vectar_unfold3_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       call f (index, seed1, seed2, seed3, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx3_subr

  recursive subroutine vectar_unfoldx4_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4)
    procedure(vectar_unfold4_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       call f (index, seed1, seed2, seed3, seed4, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx4_subr

  recursive subroutine vectar_unfoldx5_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5)
    procedure(vectar_unfold5_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       call f (index, seed1, seed2, seed3, seed4, seed5, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx5_subr

  recursive subroutine vectar_unfoldx6_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6)
    procedure(vectar_unfold6_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx6_subr

  recursive subroutine vectar_unfoldx7_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7)
    procedure(vectar_unfold7_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx7_subr

  recursive subroutine vectar_unfoldx8_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8)
    procedure(vectar_unfold8_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    class(*), allocatable :: seed8
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    type(gcroot_t) :: seed8_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    seed8_root = initial_seed8
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       seed8 = .val. seed8_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, seed8, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
       seed8_root = seed8
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx8_subr

  recursive subroutine vectar_unfoldx9_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, &
       initial_seed9)
    procedure(vectar_unfold9_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    class(*), allocatable :: seed8
    class(*), allocatable :: seed9
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    type(gcroot_t) :: seed8_root
    type(gcroot_t) :: seed9_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    seed8_root = initial_seed8
    seed9_root = initial_seed9
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       seed8 = .val. seed8_root
       seed9 = .val. seed9_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, seed8, seed9, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
       seed8_root = seed8
       seed9_root = seed9
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx9_subr

  recursive subroutine vectar_unfoldx10_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, &
       initial_seed9, initial_seed10)
    procedure(vectar_unfold10_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    class(*), intent(in) :: initial_seed10

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    class(*), allocatable :: seed8
    class(*), allocatable :: seed9
    class(*), allocatable :: seed10
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    type(gcroot_t) :: seed8_root
    type(gcroot_t) :: seed9_root
    type(gcroot_t) :: seed10_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    seed8_root = initial_seed8
    seed9_root = initial_seed9
    seed10_root = initial_seed10
    do index = 0_sz, vecr%length() - 1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       seed8 = .val. seed8_root
       seed9 = .val. seed9_root
       seed10 = .val. seed10_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, seed8, seed9, seed10, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
       seed8_root = seed8
       seed9_root = seed9
       seed10_root = seed10
    end do

    call vec_root%discard
  end subroutine vectar_unfoldx10_subr

  recursive function vectar_unfold0_subr_size_kind (f, length) result (vec)
    procedure(vectar_unfold0_f_subr_t) :: f
    integer(sz), intent(in) :: length
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx0_subr (f, vec)
  end function vectar_unfold0_subr_size_kind

  recursive function vectar_unfold0_subr_int (f, length) result (vec)
    procedure(vectar_unfold0_f_subr_t) :: f
    integer, intent(in) :: length
    type(vectar_t) :: vec

    vec = vectar_unfold0_subr_size_kind (f, .sz. length)
  end function vectar_unfold0_subr_int

  recursive function vectar_unfold1_subr_size_kind (f, length, &
       initial_seed1) result (vec)
    procedure(vectar_unfold1_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx1_subr (f, vec, &
         initial_seed1)
  end function vectar_unfold1_subr_size_kind

  recursive function vectar_unfold1_subr_int (f, length, &
       initial_seed1) result (vec)
    procedure(vectar_unfold1_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    type(vectar_t) :: vec

    vec = vectar_unfold1_subr_size_kind (f, .sz. length, &
         initial_seed1)
  end function vectar_unfold1_subr_int

  recursive function vectar_unfold2_subr_size_kind (f, length, &
       initial_seed1, initial_seed2) result (vec)
    procedure(vectar_unfold2_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx2_subr (f, vec, &
         initial_seed1, initial_seed2)
  end function vectar_unfold2_subr_size_kind

  recursive function vectar_unfold2_subr_int (f, length, &
       initial_seed1, initial_seed2) result (vec)
    procedure(vectar_unfold2_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    type(vectar_t) :: vec

    vec = vectar_unfold2_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2)
  end function vectar_unfold2_subr_int

  recursive function vectar_unfold3_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3) result (vec)
    procedure(vectar_unfold3_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx3_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3)
  end function vectar_unfold3_subr_size_kind

  recursive function vectar_unfold3_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3) result (vec)
    procedure(vectar_unfold3_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    type(vectar_t) :: vec

    vec = vectar_unfold3_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3)
  end function vectar_unfold3_subr_int

  recursive function vectar_unfold4_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4) result (vec)
    procedure(vectar_unfold4_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx4_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4)
  end function vectar_unfold4_subr_size_kind

  recursive function vectar_unfold4_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4) result (vec)
    procedure(vectar_unfold4_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    type(vectar_t) :: vec

    vec = vectar_unfold4_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4)
  end function vectar_unfold4_subr_int

  recursive function vectar_unfold5_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5) result (vec)
    procedure(vectar_unfold5_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx5_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5)
  end function vectar_unfold5_subr_size_kind

  recursive function vectar_unfold5_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5) result (vec)
    procedure(vectar_unfold5_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    type(vectar_t) :: vec

    vec = vectar_unfold5_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5)
  end function vectar_unfold5_subr_int

  recursive function vectar_unfold6_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6) result (vec)
    procedure(vectar_unfold6_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx6_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6)
  end function vectar_unfold6_subr_size_kind

  recursive function vectar_unfold6_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6) result (vec)
    procedure(vectar_unfold6_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    type(vectar_t) :: vec

    vec = vectar_unfold6_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6)
  end function vectar_unfold6_subr_int

  recursive function vectar_unfold7_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7) result (vec)
    procedure(vectar_unfold7_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx7_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7)
  end function vectar_unfold7_subr_size_kind

  recursive function vectar_unfold7_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7) result (vec)
    procedure(vectar_unfold7_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    type(vectar_t) :: vec

    vec = vectar_unfold7_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7)
  end function vectar_unfold7_subr_int

  recursive function vectar_unfold8_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8) result (vec)
    procedure(vectar_unfold8_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx8_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8)
  end function vectar_unfold8_subr_size_kind

  recursive function vectar_unfold8_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8) result (vec)
    procedure(vectar_unfold8_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    type(vectar_t) :: vec

    vec = vectar_unfold8_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8)
  end function vectar_unfold8_subr_int

  recursive function vectar_unfold9_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9) result (vec)
    procedure(vectar_unfold9_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx9_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9)
  end function vectar_unfold9_subr_size_kind

  recursive function vectar_unfold9_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9) result (vec)
    procedure(vectar_unfold9_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    type(vectar_t) :: vec

    vec = vectar_unfold9_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9)
  end function vectar_unfold9_subr_int

  recursive function vectar_unfold10_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9, &
       initial_seed10) result (vec)
    procedure(vectar_unfold10_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    class(*), intent(in) :: initial_seed10
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfoldx10_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9, &
         initial_seed10)
  end function vectar_unfold10_subr_size_kind

  recursive function vectar_unfold10_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9, &
       initial_seed10) result (vec)
    procedure(vectar_unfold10_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    class(*), intent(in) :: initial_seed10
    type(vectar_t) :: vec

    vec = vectar_unfold10_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9, &
         initial_seed10)
  end function vectar_unfold10_subr_int

  recursive subroutine vectar_unfold_rightx0_subr (f, vec)
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
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       call f (index, element)
       data%array(i0 + index)%element = element
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx0_subr

  recursive subroutine vectar_unfold_rightx1_subr (f, vec, &
       initial_seed1)
    procedure(vectar_unfold1_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    type(gcroot_t) :: seed1_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       call f (index, seed1, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx1_subr

  recursive subroutine vectar_unfold_rightx2_subr (f, vec, &
       initial_seed1, initial_seed2)
    procedure(vectar_unfold2_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       call f (index, seed1, seed2, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx2_subr

  recursive subroutine vectar_unfold_rightx3_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3)
    procedure(vectar_unfold3_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       call f (index, seed1, seed2, seed3, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx3_subr

  recursive subroutine vectar_unfold_rightx4_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4)
    procedure(vectar_unfold4_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       call f (index, seed1, seed2, seed3, seed4, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx4_subr

  recursive subroutine vectar_unfold_rightx5_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5)
    procedure(vectar_unfold5_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       call f (index, seed1, seed2, seed3, seed4, seed5, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx5_subr

  recursive subroutine vectar_unfold_rightx6_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6)
    procedure(vectar_unfold6_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx6_subr

  recursive subroutine vectar_unfold_rightx7_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7)
    procedure(vectar_unfold7_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx7_subr

  recursive subroutine vectar_unfold_rightx8_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8)
    procedure(vectar_unfold8_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    class(*), allocatable :: seed8
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    type(gcroot_t) :: seed8_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    seed8_root = initial_seed8
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       seed8 = .val. seed8_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, seed8, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
       seed8_root = seed8
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx8_subr

  recursive subroutine vectar_unfold_rightx9_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, &
       initial_seed9)
    procedure(vectar_unfold9_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    class(*), allocatable :: seed8
    class(*), allocatable :: seed9
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    type(gcroot_t) :: seed8_root
    type(gcroot_t) :: seed9_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    seed8_root = initial_seed8
    seed9_root = initial_seed9
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       seed8 = .val. seed8_root
       seed9 = .val. seed9_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, seed8, seed9, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
       seed8_root = seed8
       seed9_root = seed9
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx9_subr

  recursive subroutine vectar_unfold_rightx10_subr (f, vec, &
       initial_seed1, initial_seed2, &
       initial_seed3, initial_seed4, &
       initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, &
       initial_seed9, initial_seed10)
    procedure(vectar_unfold10_f_subr_t) :: f
    class(*), intent(in) :: vec
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    class(*), intent(in) :: initial_seed10

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(*), allocatable :: seed1
    class(*), allocatable :: seed2
    class(*), allocatable :: seed3
    class(*), allocatable :: seed4
    class(*), allocatable :: seed5
    class(*), allocatable :: seed6
    class(*), allocatable :: seed7
    class(*), allocatable :: seed8
    class(*), allocatable :: seed9
    class(*), allocatable :: seed10
    type(gcroot_t) :: seed1_root
    type(gcroot_t) :: seed2_root
    type(gcroot_t) :: seed3_root
    type(gcroot_t) :: seed4_root
    type(gcroot_t) :: seed5_root
    type(gcroot_t) :: seed6_root
    type(gcroot_t) :: seed7_root
    type(gcroot_t) :: seed8_root
    type(gcroot_t) :: seed9_root
    type(gcroot_t) :: seed10_root
    class(*), allocatable :: element
    integer(sz) :: index
    integer(sz) :: i0

    vec_root = vec

    vecr = vec
    i0 = vecr%istart0()
    data => vectar_data_ptr (vecr)
    seed1_root = initial_seed1
    seed2_root = initial_seed2
    seed3_root = initial_seed3
    seed4_root = initial_seed4
    seed5_root = initial_seed5
    seed6_root = initial_seed6
    seed7_root = initial_seed7
    seed8_root = initial_seed8
    seed9_root = initial_seed9
    seed10_root = initial_seed10
    do index = vecr%length() - 1_sz, 0_sz, -1_sz
       seed1 = .val. seed1_root
       seed2 = .val. seed2_root
       seed3 = .val. seed3_root
       seed4 = .val. seed4_root
       seed5 = .val. seed5_root
       seed6 = .val. seed6_root
       seed7 = .val. seed7_root
       seed8 = .val. seed8_root
       seed9 = .val. seed9_root
       seed10 = .val. seed10_root
       call f (index, seed1, seed2, seed3, seed4, seed5, &
            &  seed6, seed7, seed8, seed9, seed10, element)
       data%array(i0 + index)%element = element
       seed1_root = seed1
       seed2_root = seed2
       seed3_root = seed3
       seed4_root = seed4
       seed5_root = seed5
       seed6_root = seed6
       seed7_root = seed7
       seed8_root = seed8
       seed9_root = seed9
       seed10_root = seed10
    end do

    call vec_root%discard
  end subroutine vectar_unfold_rightx10_subr

  recursive function vectar_unfold_right0_subr_size_kind (f, length) result (vec)
    procedure(vectar_unfold0_f_subr_t) :: f
    integer(sz), intent(in) :: length
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx0_subr (f, vec)
  end function vectar_unfold_right0_subr_size_kind

  recursive function vectar_unfold_right0_subr_int (f, length) result (vec)
    procedure(vectar_unfold0_f_subr_t) :: f
    integer, intent(in) :: length
    type(vectar_t) :: vec

    vec = vectar_unfold_right0_subr_size_kind (f, .sz. length)
  end function vectar_unfold_right0_subr_int

  recursive function vectar_unfold_right1_subr_size_kind (f, length, &
       initial_seed1) result (vec)
    procedure(vectar_unfold1_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx1_subr (f, vec, &
         initial_seed1)
  end function vectar_unfold_right1_subr_size_kind

  recursive function vectar_unfold_right1_subr_int (f, length, &
       initial_seed1) result (vec)
    procedure(vectar_unfold1_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    type(vectar_t) :: vec

    vec = vectar_unfold_right1_subr_size_kind (f, .sz. length, &
         initial_seed1)
  end function vectar_unfold_right1_subr_int

  recursive function vectar_unfold_right2_subr_size_kind (f, length, &
       initial_seed1, initial_seed2) result (vec)
    procedure(vectar_unfold2_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx2_subr (f, vec, &
         initial_seed1, initial_seed2)
  end function vectar_unfold_right2_subr_size_kind

  recursive function vectar_unfold_right2_subr_int (f, length, &
       initial_seed1, initial_seed2) result (vec)
    procedure(vectar_unfold2_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    type(vectar_t) :: vec

    vec = vectar_unfold_right2_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2)
  end function vectar_unfold_right2_subr_int

  recursive function vectar_unfold_right3_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3) result (vec)
    procedure(vectar_unfold3_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx3_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3)
  end function vectar_unfold_right3_subr_size_kind

  recursive function vectar_unfold_right3_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3) result (vec)
    procedure(vectar_unfold3_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    type(vectar_t) :: vec

    vec = vectar_unfold_right3_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3)
  end function vectar_unfold_right3_subr_int

  recursive function vectar_unfold_right4_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4) result (vec)
    procedure(vectar_unfold4_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx4_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4)
  end function vectar_unfold_right4_subr_size_kind

  recursive function vectar_unfold_right4_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4) result (vec)
    procedure(vectar_unfold4_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    type(vectar_t) :: vec

    vec = vectar_unfold_right4_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4)
  end function vectar_unfold_right4_subr_int

  recursive function vectar_unfold_right5_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5) result (vec)
    procedure(vectar_unfold5_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx5_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5)
  end function vectar_unfold_right5_subr_size_kind

  recursive function vectar_unfold_right5_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5) result (vec)
    procedure(vectar_unfold5_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    type(vectar_t) :: vec

    vec = vectar_unfold_right5_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5)
  end function vectar_unfold_right5_subr_int

  recursive function vectar_unfold_right6_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6) result (vec)
    procedure(vectar_unfold6_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx6_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6)
  end function vectar_unfold_right6_subr_size_kind

  recursive function vectar_unfold_right6_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6) result (vec)
    procedure(vectar_unfold6_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    type(vectar_t) :: vec

    vec = vectar_unfold_right6_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6)
  end function vectar_unfold_right6_subr_int

  recursive function vectar_unfold_right7_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7) result (vec)
    procedure(vectar_unfold7_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx7_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7)
  end function vectar_unfold_right7_subr_size_kind

  recursive function vectar_unfold_right7_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7) result (vec)
    procedure(vectar_unfold7_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    type(vectar_t) :: vec

    vec = vectar_unfold_right7_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7)
  end function vectar_unfold_right7_subr_int

  recursive function vectar_unfold_right8_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8) result (vec)
    procedure(vectar_unfold8_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx8_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8)
  end function vectar_unfold_right8_subr_size_kind

  recursive function vectar_unfold_right8_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8) result (vec)
    procedure(vectar_unfold8_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    type(vectar_t) :: vec

    vec = vectar_unfold_right8_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8)
  end function vectar_unfold_right8_subr_int

  recursive function vectar_unfold_right9_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9) result (vec)
    procedure(vectar_unfold9_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx9_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9)
  end function vectar_unfold_right9_subr_size_kind

  recursive function vectar_unfold_right9_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9) result (vec)
    procedure(vectar_unfold9_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    type(vectar_t) :: vec

    vec = vectar_unfold_right9_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9)
  end function vectar_unfold_right9_subr_int

  recursive function vectar_unfold_right10_subr_size_kind (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9, &
       initial_seed10) result (vec)
    procedure(vectar_unfold10_f_subr_t) :: f
    integer(sz), intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    class(*), intent(in) :: initial_seed10
    type(vectar_t) :: vec

    vec = make_vectar (length)
    call vectar_unfold_rightx10_subr (f, vec, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9, &
         initial_seed10)
  end function vectar_unfold_right10_subr_size_kind

  recursive function vectar_unfold_right10_subr_int (f, length, &
       initial_seed1, initial_seed2, initial_seed3, &
       initial_seed4, initial_seed5, initial_seed6, &
       initial_seed7, initial_seed8, initial_seed9, &
       initial_seed10) result (vec)
    procedure(vectar_unfold10_f_subr_t) :: f
    integer, intent(in) :: length
    class(*), intent(in) :: initial_seed1
    class(*), intent(in) :: initial_seed2
    class(*), intent(in) :: initial_seed3
    class(*), intent(in) :: initial_seed4
    class(*), intent(in) :: initial_seed5
    class(*), intent(in) :: initial_seed6
    class(*), intent(in) :: initial_seed7
    class(*), intent(in) :: initial_seed8
    class(*), intent(in) :: initial_seed9
    class(*), intent(in) :: initial_seed10
    type(vectar_t) :: vec

    vec = vectar_unfold_right10_subr_size_kind (f, .sz. length, &
         initial_seed1, initial_seed2, initial_seed3, &
         initial_seed4, initial_seed5, initial_seed6, &
         initial_seed7, initial_seed8, initial_seed9, &
         initial_seed10)
  end function vectar_unfold_right10_subr_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_indexn_1 (pred, n, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    logical :: requirement_is_satisfied

    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
  end function vectar_indexn_1

  recursive function vectar_index0_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_indexn_1 (pred, 0_sz, vec1)
  end function vectar_index0_1

  recursive function vectar_index1_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_indexn_1 (pred, 1_sz, vec1)
  end function vectar_index1_1

  recursive function vectar_indexn_2 (pred, n, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_indexn_2

  recursive function vectar_index0_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_indexn_2 (pred, 0_sz, vec1, vec2)
  end function vectar_index0_2

  recursive function vectar_index1_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_indexn_2 (pred, 1_sz, vec1, vec2)
  end function vectar_index1_2

  recursive function vectar_indexn_3 (pred, n, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_indexn_3

  recursive function vectar_index0_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_indexn_3 (pred, 0_sz, vec1, vec2, vec3)
  end function vectar_index0_3

  recursive function vectar_index1_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_indexn_3 (pred, 1_sz, vec1, vec2, vec3)
  end function vectar_index1_3

  recursive function vectar_indexn_4 (pred, n, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_indexn_4

  recursive function vectar_index0_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_indexn_4 (pred, 0_sz, vec1, vec2, vec3, vec4)
  end function vectar_index0_4

  recursive function vectar_index1_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_indexn_4 (pred, 1_sz, vec1, vec2, vec3, vec4)
  end function vectar_index1_4

  recursive function vectar_indexn_5 (pred, n, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_indexn_5

  recursive function vectar_index0_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_indexn_5 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_index0_5

  recursive function vectar_index1_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_indexn_5 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_index1_5

  recursive function vectar_indexn_6 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_indexn_6

  recursive function vectar_index0_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_indexn_6 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6)
  end function vectar_index0_6

  recursive function vectar_index1_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_indexn_6 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6)
  end function vectar_index1_6

  recursive function vectar_indexn_7 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_indexn_7

  recursive function vectar_index0_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_indexn_7 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7)
  end function vectar_index0_7

  recursive function vectar_index1_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_indexn_7 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7)
  end function vectar_index1_7

  recursive function vectar_indexn_8 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_indexn_8

  recursive function vectar_index0_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_indexn_8 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8)
  end function vectar_index0_8

  recursive function vectar_index1_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_indexn_8 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8)
  end function vectar_index1_8

  recursive function vectar_indexn_9 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_indexn_9

  recursive function vectar_index0_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_indexn_9 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9)
  end function vectar_index0_9

  recursive function vectar_index1_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_indexn_9 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9)
  end function vectar_index1_9

  recursive function vectar_indexn_10 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_indexn_10

  recursive function vectar_index0_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_indexn_10 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9, vec10)
  end function vectar_index0_10

  recursive function vectar_index1_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_indexn_10 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9, vec10)
  end function vectar_index1_10

  recursive function vectar_skipn_1 (pred, n, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    logical :: requirement_is_satisfied

    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
  end function vectar_skipn_1

  recursive function vectar_skip0_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_skipn_1 (pred, 0_sz, vec1)
  end function vectar_skip0_1

  recursive function vectar_skip1_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_skipn_1 (pred, 1_sz, vec1)
  end function vectar_skip1_1

  recursive function vectar_skipn_2 (pred, n, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_skipn_2

  recursive function vectar_skip0_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_skipn_2 (pred, 0_sz, vec1, vec2)
  end function vectar_skip0_2

  recursive function vectar_skip1_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_skipn_2 (pred, 1_sz, vec1, vec2)
  end function vectar_skip1_2

  recursive function vectar_skipn_3 (pred, n, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_skipn_3

  recursive function vectar_skip0_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_skipn_3 (pred, 0_sz, vec1, vec2, vec3)
  end function vectar_skip0_3

  recursive function vectar_skip1_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_skipn_3 (pred, 1_sz, vec1, vec2, vec3)
  end function vectar_skip1_3

  recursive function vectar_skipn_4 (pred, n, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_skipn_4

  recursive function vectar_skip0_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_skipn_4 (pred, 0_sz, vec1, vec2, vec3, vec4)
  end function vectar_skip0_4

  recursive function vectar_skip1_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_skipn_4 (pred, 1_sz, vec1, vec2, vec3, vec4)
  end function vectar_skip1_4

  recursive function vectar_skipn_5 (pred, n, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_skipn_5

  recursive function vectar_skip0_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_skipn_5 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_skip0_5

  recursive function vectar_skip1_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_skipn_5 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_skip1_5

  recursive function vectar_skipn_6 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_skipn_6

  recursive function vectar_skip0_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_skipn_6 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6)
  end function vectar_skip0_6

  recursive function vectar_skip1_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_skipn_6 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6)
  end function vectar_skip1_6

  recursive function vectar_skipn_7 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_skipn_7

  recursive function vectar_skip0_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_skipn_7 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7)
  end function vectar_skip0_7

  recursive function vectar_skip1_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_skipn_7 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7)
  end function vectar_skip1_7

  recursive function vectar_skipn_8 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_skipn_8

  recursive function vectar_skip0_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_skipn_8 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8)
  end function vectar_skip0_8

  recursive function vectar_skip1_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_skipn_8 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8)
  end function vectar_skip1_8

  recursive function vectar_skipn_9 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_skipn_9

  recursive function vectar_skip0_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_skipn_9 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9)
  end function vectar_skip0_9

  recursive function vectar_skip1_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_skipn_9 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9)
  end function vectar_skip1_9

  recursive function vectar_skipn_10 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)
    i = 0_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element)
       i = i + 1
    end do
    if (requirement_is_satisfied) then
       index = (n - 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_skipn_10

  recursive function vectar_skip0_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_skipn_10 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9, vec10)
  end function vectar_skip0_10

  recursive function vectar_skip1_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_skipn_10 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                   vec6, vec7, vec8, vec9, vec10)
  end function vectar_skip1_10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_index_rightn_1 (pred, n, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    logical :: requirement_is_satisfied

    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
  end function vectar_index_rightn_1

  recursive function vectar_index_right0_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_index_rightn_1 (pred, 0_sz, vec1)
  end function vectar_index_right0_1

  recursive function vectar_index_right1_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_index_rightn_1 (pred, 1_sz, vec1)
  end function vectar_index_right1_1

  recursive function vectar_index_rightn_2 (pred, n, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_index_rightn_2

  recursive function vectar_index_right0_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_index_rightn_2 (pred, 0_sz, vec1, vec2)
  end function vectar_index_right0_2

  recursive function vectar_index_right1_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_index_rightn_2 (pred, 1_sz, vec1, vec2)
  end function vectar_index_right1_2

  recursive function vectar_index_rightn_3 (pred, n, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_index_rightn_3

  recursive function vectar_index_right0_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_index_rightn_3 (pred, 0_sz, vec1, vec2, vec3)
  end function vectar_index_right0_3

  recursive function vectar_index_right1_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_index_rightn_3 (pred, 1_sz, vec1, vec2, vec3)
  end function vectar_index_right1_3

  recursive function vectar_index_rightn_4 (pred, n, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_index_rightn_4

  recursive function vectar_index_right0_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_index_rightn_4 (pred, 0_sz, vec1, vec2, vec3, vec4)
  end function vectar_index_right0_4

  recursive function vectar_index_right1_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_index_rightn_4 (pred, 1_sz, vec1, vec2, vec3, vec4)
  end function vectar_index_right1_4

  recursive function vectar_index_rightn_5 (pred, n, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_index_rightn_5

  recursive function vectar_index_right0_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_index_rightn_5 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_index_right0_5

  recursive function vectar_index_right1_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_index_rightn_5 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_index_right1_5

  recursive function vectar_index_rightn_6 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_index_rightn_6

  recursive function vectar_index_right0_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_index_rightn_6 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6)
  end function vectar_index_right0_6

  recursive function vectar_index_right1_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_index_rightn_6 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6)
  end function vectar_index_right1_6

  recursive function vectar_index_rightn_7 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_index_rightn_7

  recursive function vectar_index_right0_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_index_rightn_7 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7)
  end function vectar_index_right0_7

  recursive function vectar_index_right1_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_index_rightn_7 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7)
  end function vectar_index_right1_7

  recursive function vectar_index_rightn_8 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_index_rightn_8

  recursive function vectar_index_right0_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_index_rightn_8 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8)
  end function vectar_index_right0_8

  recursive function vectar_index_right1_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_index_rightn_8 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8)
  end function vectar_index_right1_8

  recursive function vectar_index_rightn_9 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_index_rightn_9

  recursive function vectar_index_right0_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_index_rightn_9 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9)
  end function vectar_index_right0_9

  recursive function vectar_index_right1_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_index_rightn_9 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9)
  end function vectar_index_right1_9

  recursive function vectar_index_rightn_10 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       requirement_is_satisfied = &
            pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_index_rightn_10

  recursive function vectar_index_right0_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_index_rightn_10 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9, vec10)
  end function vectar_index_right0_10

  recursive function vectar_index_right1_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_index_rightn_10 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9, vec10)
  end function vectar_index_right1_10

  recursive function vectar_skip_rightn_1 (pred, n, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    logical :: requirement_is_satisfied

    vec1_root = vec1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
  end function vectar_skip_rightn_1

  recursive function vectar_skip_right0_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_skip_rightn_1 (pred, 0_sz, vec1)
  end function vectar_skip_right0_1

  recursive function vectar_skip_right1_1 (pred, vec1) result (index)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    integer(sz) :: index

    index = vectar_skip_rightn_1 (pred, 1_sz, vec1)
  end function vectar_skip_right1_1

  recursive function vectar_skip_rightn_2 (pred, n, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_skip_rightn_2

  recursive function vectar_skip_right0_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_skip_rightn_2 (pred, 0_sz, vec1, vec2)
  end function vectar_skip_right0_2

  recursive function vectar_skip_right1_2 (pred, vec1, vec2) result (index)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    integer(sz) :: index

    index = vectar_skip_rightn_2 (pred, 1_sz, vec1, vec2)
  end function vectar_skip_right1_2

  recursive function vectar_skip_rightn_3 (pred, n, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_skip_rightn_3

  recursive function vectar_skip_right0_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_skip_rightn_3 (pred, 0_sz, vec1, vec2, vec3)
  end function vectar_skip_right0_3

  recursive function vectar_skip_right1_3 (pred, vec1, vec2, vec3) result (index)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    integer(sz) :: index

    index = vectar_skip_rightn_3 (pred, 1_sz, vec1, vec2, vec3)
  end function vectar_skip_right1_3

  recursive function vectar_skip_rightn_4 (pred, n, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_skip_rightn_4

  recursive function vectar_skip_right0_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_skip_rightn_4 (pred, 0_sz, vec1, vec2, vec3, vec4)
  end function vectar_skip_right0_4

  recursive function vectar_skip_right1_4 (pred, vec1, vec2, vec3, vec4) result (index)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    integer(sz) :: index

    index = vectar_skip_rightn_4 (pred, 1_sz, vec1, vec2, vec3, vec4)
  end function vectar_skip_right1_4

  recursive function vectar_skip_rightn_5 (pred, n, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_skip_rightn_5

  recursive function vectar_skip_right0_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_skip_rightn_5 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_skip_right0_5

  recursive function vectar_skip_right1_5 (pred, vec1, vec2, vec3, vec4, vec5) result (index)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    integer(sz) :: index

    index = vectar_skip_rightn_5 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5)
  end function vectar_skip_right1_5

  recursive function vectar_skip_rightn_6 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_skip_rightn_6

  recursive function vectar_skip_right0_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_skip_rightn_6 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6)
  end function vectar_skip_right0_6

  recursive function vectar_skip_right1_6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6) result (index)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    integer(sz) :: index

    index = vectar_skip_rightn_6 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6)
  end function vectar_skip_right1_6

  recursive function vectar_skip_rightn_7 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_skip_rightn_7

  recursive function vectar_skip_right0_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_skip_rightn_7 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7)
  end function vectar_skip_right0_7

  recursive function vectar_skip_right1_7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7) result (index)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    integer(sz) :: index

    index = vectar_skip_rightn_7 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7)
  end function vectar_skip_right1_7

  recursive function vectar_skip_rightn_8 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_skip_rightn_8

  recursive function vectar_skip_right0_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_skip_rightn_8 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8)
  end function vectar_skip_right0_8

  recursive function vectar_skip_right1_8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8) result (index)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    integer(sz) :: index

    index = vectar_skip_rightn_8 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8)
  end function vectar_skip_right1_8

  recursive function vectar_skip_rightn_9 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_skip_rightn_9

  recursive function vectar_skip_right0_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_skip_rightn_9 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9)
  end function vectar_skip_right0_9

  recursive function vectar_skip_right1_9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9) result (index)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    integer(sz) :: index

    index = vectar_skip_rightn_9 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9)
  end function vectar_skip_right1_9

  recursive function vectar_skip_rightn_10 (pred, n, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    integer(sz), intent(in) :: n
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10
    logical :: requirement_is_satisfied

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)
    i = min_length - 1_sz
    requirement_is_satisfied = .false.
    do while (.not. requirement_is_satisfied .and. 0_sz <= i)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       requirement_is_satisfied = &
            .not. pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element)
       i = i - 1
    end do
    if (requirement_is_satisfied) then
       index = (n + 1_sz) + i
    else
       index = min (-1_sz, n - 1_sz)
    end if

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_skip_rightn_10

  recursive function vectar_skip_right0_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_skip_rightn_10 (pred, 0_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9, vec10)
  end function vectar_skip_right0_10

  recursive function vectar_skip_right1_10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                              vec6, vec7, vec8, vec9, vec10) result (index)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    integer(sz) :: index

    index = vectar_skip_rightn_10 (pred, 1_sz, vec1, vec2, vec3, vec4, vec5, &
         &                         vec6, vec7, vec8, vec9, vec10)
  end function vectar_skip_right1_10

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
    index = min (-1_sz, n - 1_sz)
    do while (ileft <= iright .and. index < n)
       imiddle = ileft + ((iright - ileft) / 2_sz)
       i = i0 + imiddle
       sign = cmp (data%array(i)%element, x)
       if (sign < 0) then
          ileft = imiddle + 1
       else if (0 < sign) then
          iright = imiddle - 1
       else
          index = imiddle + n
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

    index = min (-1_sz, n - 1_sz)

    vecr = vec
    istart0 = vecr%istart0()
    iend0 = vecr%iend0()

    if (istart0 <= iend0) then
       vec_root = vec

       data => vectar_data_ptr (vecr)
       i = bottenbruch_search (less_than, data, istart0, iend0, x)
       if (.not. less_than (data%array(i)%element, x)) then
          if (.not. less_than (x, data%array(i)%element)) then
             index = i - istart0 + n
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

    index = min (-1_sz, n - 1_sz)

    vecr = vec
    istart0 = vecr%istart0()
    iend0 = vecr%iend0()

    if (istart0 <= iend0) then
       vec_root = vec

       data => vectar_data_ptr (vecr)
       i = bottenbruch_search (less_than, data, istart0, iend0, x)
       if (equal (x, data%array(i)%element)) then
          index = i - istart0 + n
       end if

       call vec_root%discard
    end if
  end function vectar_bottenbruch_searchn_with_equality

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_some1 (pred, vec1) result (bool)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       bool = pred (data1%array(i1)%element)
       i = i + 1
    end do

    call vec1_root%discard
  end function vectar_some1

  recursive function vectar_some2 (pred, vec1, vec2) result (bool)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_some2

  recursive function vectar_some3 (pred, vec1, vec2, vec3) result (bool)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_some3

  recursive function vectar_some4 (pred, vec1, vec2, vec3, vec4) result (bool)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_some4

  recursive function vectar_some5 (pred, vec1, vec2, vec3, vec4, vec5) result (bool)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_some5

  recursive function vectar_some6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6) result (bool)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_some6

  recursive function vectar_some7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7) result (bool)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_some7

  recursive function vectar_some8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8) result (bool)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_some8

  recursive function vectar_some9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9) result (bool)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_some9

  recursive function vectar_some10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9, vec10) result (bool)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    i = 0_sz
    bool = .not. .true.
    do while (.not. bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_some10

  recursive function vectar_every1 (pred, vec1) result (bool)
    procedure(vectar_predicate1_t) :: pred
    class(*), intent(in) :: vec1
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       bool = pred (data1%array(i1)%element)
       i = i + 1
    end do

    call vec1_root%discard
  end function vectar_every1

  recursive function vectar_every2 (pred, vec1, vec2) result (bool)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_every2

  recursive function vectar_every3 (pred, vec1, vec2, vec3) result (bool)
    procedure(vectar_predicate3_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_every3

  recursive function vectar_every4 (pred, vec1, vec2, vec3, vec4) result (bool)
    procedure(vectar_predicate4_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_every4

  recursive function vectar_every5 (pred, vec1, vec2, vec3, vec4, vec5) result (bool)
    procedure(vectar_predicate5_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_every5

  recursive function vectar_every6 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6) result (bool)
    procedure(vectar_predicate6_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_every6

  recursive function vectar_every7 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7) result (bool)
    procedure(vectar_predicate7_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_every7

  recursive function vectar_every8 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8) result (bool)
    procedure(vectar_predicate8_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_every8

  recursive function vectar_every9 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9) result (bool)
    procedure(vectar_predicate9_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_every9

  recursive function vectar_every10 (pred, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9, vec10) result (bool)
    procedure(vectar_predicate10_t) :: pred
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    logical :: bool

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    i = 0_sz
    bool = .true.
    do while (bool .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       bool = pred (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element)
       i = i + 1
    end do

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_every10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function vectar_some_map1_subr (subr, vec1) result (retval)
    procedure(vectar_map1_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       call subr (data1%array(i1)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
  end function vectar_some_map1_subr

  recursive function vectar_some_map2_subr (subr, vec1, vec2) result (retval)
    procedure(vectar_map2_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_some_map2_subr

  recursive function vectar_some_map3_subr (subr, vec1, vec2, vec3) result (retval)
    procedure(vectar_map3_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_some_map3_subr

  recursive function vectar_some_map4_subr (subr, vec1, vec2, vec3, vec4) result (retval)
    procedure(vectar_map4_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_some_map4_subr

  recursive function vectar_some_map5_subr (subr, vec1, vec2, vec3, vec4, vec5) result (retval)
    procedure(vectar_map5_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_some_map5_subr

  recursive function vectar_some_map6_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6) result (retval)
    procedure(vectar_map6_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_some_map6_subr

  recursive function vectar_some_map7_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7) result (retval)
    procedure(vectar_map7_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_some_map7_subr

  recursive function vectar_some_map8_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8) result (retval)
    procedure(vectar_map8_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_some_map8_subr

  recursive function vectar_some_map9_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9) result (retval)
    procedure(vectar_map9_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_some_map9_subr

  recursive function vectar_some_map10_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9, vec10) result (retval)
    procedure(vectar_map10_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    subr_result = .not. .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element, subr_result)
       short_circuited = .not. is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_some_map10_subr

  recursive function vectar_every_map1_subr (subr, vec1) result (retval)
    procedure(vectar_map1_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(vectar_range_t) :: vecr1
    class(vectar_data_t), pointer :: data1
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vecr1 = vec1
    min_length = vecr1%length()
    data1 => vectar_data_ptr (vecr1)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       call subr (data1%array(i1)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
  end function vectar_every_map1_subr

  recursive function vectar_every_map2_subr (subr, vec1, vec2) result (retval)
    procedure(vectar_map2_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vecr1 = vec1
    vecr2 = vec2
    min_length = min (vecr1%length(), vecr2%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
  end function vectar_every_map2_subr

  recursive function vectar_every_map3_subr (subr, vec1, vec2, vec3) result (retval)
    procedure(vectar_map3_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
  end function vectar_every_map3_subr

  recursive function vectar_every_map4_subr (subr, vec1, vec2, vec3, vec4) result (retval)
    procedure(vectar_map4_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
  end function vectar_every_map4_subr

  recursive function vectar_every_map5_subr (subr, vec1, vec2, vec3, vec4, vec5) result (retval)
    procedure(vectar_map5_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
  end function vectar_every_map5_subr

  recursive function vectar_every_map6_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6) result (retval)
    procedure(vectar_map6_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
  end function vectar_every_map6_subr

  recursive function vectar_every_map7_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7) result (retval)
    procedure(vectar_map7_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
  end function vectar_every_map7_subr

  recursive function vectar_every_map8_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8) result (retval)
    procedure(vectar_map8_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
  end function vectar_every_map8_subr

  recursive function vectar_every_map9_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9) result (retval)
    procedure(vectar_map9_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
  end function vectar_every_map9_subr

  recursive function vectar_every_map10_subr (subr, vec1, vec2, vec3, vec4, vec5, &
       &                           vec6, vec7, vec8, vec9, vec10) result (retval)
    procedure(vectar_map10_subr_t) :: subr
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), allocatable :: retval

    type(gcroot_t) :: vec1_root
    type(gcroot_t) :: vec2_root
    type(gcroot_t) :: vec3_root
    type(gcroot_t) :: vec4_root
    type(gcroot_t) :: vec5_root
    type(gcroot_t) :: vec6_root
    type(gcroot_t) :: vec7_root
    type(gcroot_t) :: vec8_root
    type(gcroot_t) :: vec9_root
    type(gcroot_t) :: vec10_root
    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2
    type(vectar_range_t) :: vecr3
    type(vectar_range_t) :: vecr4
    type(vectar_range_t) :: vecr5
    type(vectar_range_t) :: vecr6
    type(vectar_range_t) :: vecr7
    type(vectar_range_t) :: vecr8
    type(vectar_range_t) :: vecr9
    type(vectar_range_t) :: vecr10
    class(vectar_data_t), pointer :: data1
    class(vectar_data_t), pointer :: data2
    class(vectar_data_t), pointer :: data3
    class(vectar_data_t), pointer :: data4
    class(vectar_data_t), pointer :: data5
    class(vectar_data_t), pointer :: data6
    class(vectar_data_t), pointer :: data7
    class(vectar_data_t), pointer :: data8
    class(vectar_data_t), pointer :: data9
    class(vectar_data_t), pointer :: data10
    integer(sz) :: min_length
    integer(sz) :: i
    integer(sz) :: i1
    integer(sz) :: i2
    integer(sz) :: i3
    integer(sz) :: i4
    integer(sz) :: i5
    integer(sz) :: i6
    integer(sz) :: i7
    integer(sz) :: i8
    integer(sz) :: i9
    integer(sz) :: i10
    class(*), allocatable :: subr_result
    logical :: short_circuited

    vec1_root = vec1
    vec2_root = vec2
    vec3_root = vec3
    vec4_root = vec4
    vec5_root = vec5
    vec6_root = vec6
    vec7_root = vec7
    vec8_root = vec8
    vec9_root = vec9
    vec10_root = vec10
    vecr1 = vec1
    vecr2 = vec2
    vecr3 = vec3
    vecr4 = vec4
    vecr5 = vec5
    vecr6 = vec6
    vecr7 = vec7
    vecr8 = vec8
    vecr9 = vec9
    vecr10 = vec10
    min_length = min (vecr1%length(), vecr2%length(), vecr3%length(), &
         &            vecr4%length(), vecr5%length(), vecr6%length(), &
         &            vecr7%length(), vecr8%length(), vecr9%length(), &
         &            vecr10%length())
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)
    data3 => vectar_data_ptr (vecr3)
    data4 => vectar_data_ptr (vecr4)
    data5 => vectar_data_ptr (vecr5)
    data6 => vectar_data_ptr (vecr6)
    data7 => vectar_data_ptr (vecr7)
    data8 => vectar_data_ptr (vecr8)
    data9 => vectar_data_ptr (vecr9)
    data10 => vectar_data_ptr (vecr10)

    subr_result = .true.
    short_circuited = .false.
    i = 0_sz
    do while (.not. short_circuited .and. i < min_length)
       i1 = vecr1%istart0() + i
       i2 = vecr2%istart0() + i
       i3 = vecr3%istart0() + i
       i4 = vecr4%istart0() + i
       i5 = vecr5%istart0() + i
       i6 = vecr6%istart0() + i
       i7 = vecr7%istart0() + i
       i8 = vecr8%istart0() + i
       i9 = vecr9%istart0() + i
       i10 = vecr10%istart0() + i
       call subr (data1%array(i1)%element, data2%array(i2)%element, &
            &     data3%array(i3)%element, data4%array(i4)%element, &
            &     data5%array(i5)%element, data6%array(i6)%element, &
            &     data7%array(i7)%element, data8%array(i8)%element, &
            &     data9%array(i9)%element, data10%array(i10)%element, subr_result)
       short_circuited = is_false (subr_result)
       i = i + 1
    end do
    retval = subr_result

    call vec1_root%discard
    call vec2_root%discard
    call vec3_root%discard
    call vec4_root%discard
    call vec5_root%discard
    call vec6_root%discard
    call vec7_root%discard
    call vec8_root%discard
    call vec9_root%discard
    call vec10_root%discard
  end function vectar_every_map10_subr

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
    integer(sz) :: i0, i1
    integer(sz) :: i

    vec_root = vec

    vecr = vec
    data => vectar_data_ptr (vecr)
    i0 = vecr%istart0()
    i1 = vecr%iend0()

    bool = .true.
    i = i0
    do while (bool .and. i < i1)
       bool = .not. less_than (data%array(i + 1)%element, data%array(i)%element)
       i = i + 1
    end do

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

  recursive function bottenbruch_search (less_than, data, ileft, iright, x) result (index)
    !
    ! Do a search on the data whose first element is at ileft and
    ! whose last element is at iright. Return `index' such that:
    !
    !    * if x is less than the element at ileft, then index = ileft;
    !
    !    * otherwise, x is less than everything to the right of index
    !      and not less than everything at or to the left of index.
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

    integer(sz) :: i, j, k, k_minus_j

    j = ileft
    k = iright
    do while (k /= j)
       ! Set i := ceil (j + k).
       k_minus_j = k - j
       i = j + ishft (k_minus_j, -1) + ibits (k_minus_j, 0, 1)
       if (less_than (x, data%array(i)%element)) then
          k = i - 1
       else
          j = i
       end if
    end do
    index = j
    if (less_than (x, data%array(index)%element)) then
       if (.not. less_than (x, data%array(ileft)%element)) then
          call error_abort ("the implementation of bottenbruch_search is not correct")
       end if
    end if
  end function bottenbruch_search

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
    ! FIXME: Use a Bottenbruch search to find initial settings of i
    !        and i2.
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

    i = itarget
    i1 = irunstart1
    i2 = irunstart2
    do while (i1 <= irunend1 .and. i2 <= irunend2)
       if (i1 == irunend1) then
          ! The rest of the merger is from data2 and already is in
          ! place.
          i2 = irunend2
       else if (i2 == irunend2) then
          ! Copy the remainder of data1.
          do while (i1 <= irunend1)
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
    ! FIXME: Use a Bottenbruch search to find initial settings of i
    !        and i1.
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

    i = itarget
    i1 = irunend1
    i2 = irunend2
    do while (irunstart1 <= i1 .and. irunstart2 <= i2)
       if (i2 == irunstart2) then
          ! The rest of the merger is from data1 and already is in
          ! place.
          i1 = irunstart1
       else if (i1 == irunstart1) then
          ! Copy the remainder of data2
          do while (irunstart2 <= i2)
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
          i1 = i1 - 1
       end if
    end do
  end subroutine merge_going_rightwards

  recursive subroutine merge_two_runs (less_than, data, i, j, k, workspace)
    !
    ! Merge sorted datai .. j-1 and sorted dataj .. k, giving
    ! sorted datai .. k.
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

    integer(sz) :: u

    if (j - i < k - j) then
       ! The left side is shorter than or equal in length to the right
       ! side. Copy the left side to workspace, then merge leftwards.
       do u = 0, (j - i) - 1
          workspace%array(u) = data%array(i + u)
       end do
       call merge_going_leftwards (less_than, data, i, j, k, workspace, 0_sz, (j - i) - 2)
    else
       ! The left side is longer than the right side side. Copy the
       ! right side to workspace, then merge rightwards.
       do u = 0, k - j
          workspace%array(u) = data%array(j + u)
       end do
       call merge_going_rightwards (less_than, data, i, j - 1, k, workspace, 0_sz, (k - j) - 1)
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
    ! employed by Tim Peters. FIXME: SAY MORE.
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module vectars
