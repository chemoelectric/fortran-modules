! -*- F90 -*- include(`common-macros.m4')m4_include([cadadr.m4])
!
! Copyright 2021, 2022 Barry Schwartz
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
!
! The cons_pairs module is based in part on the SRFI-1 reference
! implementation in Scheme, which was released as follows:
!
!   "Copyright (c) 1998, 1999 by Olin Shivers. You may do as you
!    please with this code as long as you do not remove this copyright
!    notice or hold me liable for its use."
!
! The cons_pairs module is written entirely in Fortran.
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

module cons_pairs
  !
  ! CONS-pairs (Lisp-style lists and trees) in the fashion of SRFI-1.
  ! https://srfi.schemers.org/srfi-1/srfi-1.html
  !
  ! A significant differences from SRFI-1 is:
  !
  !    * The lset operations are not included in this module.
  !
  ! This core module for CONS-pairs avoids code that might cause
  ! gfortran to create trampolines. (I did not want to obey this
  ! restriction when implementing lset operations; that is why they
  ! are not included in this module.)
  !

  !
  ! Please keep in mind that the term `list' is used only loosely in
  ! Scheme. The fundamental `list' object types are actually the
  ! `null' -- in this module called `nil' -- and the CAR-CDR
  ! pair. Given the nil and pairs, one can construct practically any
  ! kind of binary tree or graph; also, the pairs can be used as
  ! efficient 2-tuples.
  !
  ! Furthermore, objects other than the nil and pairs (such as 123,
  ! "abc", 4.0, .true., etc.) can be -- and in this module will be --
  ! regarded as degenerate `dotted lists' -- as a kind of generalized
  ! nil. \footnote{The term `dotted list' is Scheme-talk for a linked
  ! list that ends in something other than a nil or back-reference. A
  ! list that ends in a nil is called ???proper??? and a list that ends in
  ! a back-reference is called ???circular???.}
  !

  use, non_intrinsic :: garbage_collector

  implicit none
  private

  ! is_false and is_not_false are useful for treating .false. the way
  ! Scheme treats #f.
  public :: is_false         ! Is a class(*) or gcroot_t object a logical .false.?
  public :: is_not_false     ! Is a class(*) or gcroot_t object not a logical .false.?

  public :: cons_t           ! The type for NIL-lists and garbage-collectible CONS-pairs.
  public :: nil              ! The canonical NIL-list (commonly written '() in Scheme).

  public :: is_pair          ! Is the object either a CONS-pair or a gcroot_t containing a CONS-pair?
  public :: is_not_pair      ! Is the object neither a CONS-pair nor a gcroot_t containing a CONS-pair?

  ! `is_nil' is equivalent to SRFI-1's `null?' procedure.
  public :: is_nil           ! Is the object either a NIL-list or a gcroot_t containing a NIL-list?
  public :: is_not_nil       ! Is the object neither a NIL-list nor a gcroot_t containing a NIL-list?

  ! `is_nil_list' is equivalent to SRFI-1's `null-list?' procedure.
  public :: is_nil_list

  public :: cons_t_cast      ! Convert an object to a CONS-pair, if possible.
  public :: operator(.tocons.) ! A synonym for cons_t_cast.
  public :: cons_t_eq        ! Are two cons_t either both NIL or pairs that share their storage?

  public :: cons             ! The fundamental CONS-pair constructor.
  public :: uncons           ! The fundamental CONS-pair deconstructor (called `car+cdr' in SRFI-1).
  public :: car              ! Get just the CAR.
  public :: cdr              ! Get just the CDR.
  public :: set_car          ! Change the CAR.
  public :: set_cdr          ! Change the CDR.

  public :: operator(**)     ! For notation such as `1 ** 2.0 ** "3" ** nil'.

  ! Permutations of car and cdr, for returning elements of a tree.
m4_forloop([n],[2],CADADR_MAX,[m4_length_n_cadadr_public_declarations(n)])dnl

  ! Return one of the first ten elements of a list.
  public :: first
  public :: second           ! NOTE: `SECOND' shadows a GNU extension.
  public :: third
  public :: fourth
  public :: fifth
  public :: sixth
  public :: seventh
  public :: eighth
  public :: ninth
  public :: tenth

  public :: list_ref0        ! Generic function: return any one of the
                             ! 0th, 1st, 2nd, etc., elements.
  public :: list_ref1        ! Generic function: Return any one of the
                             ! 1st, 2nd, 3rd, etc., elements.
  public :: list_refn        ! Generic function: Return any one of the
                             ! nth, (n+1)th, (n+2)th, etc., elements.

  ! Implementations for INTEGER([SIZE_KIND]).
  public :: list_ref0_size_kind
  public :: list_ref1_size_kind
  public :: list_refn_size_kind

  ! Implementations for INTEGER of the default kind.
  public :: list_ref0_int
  public :: list_ref1_int
  public :: list_refn_int

  ! Make or unmake a list. (SRFI-1 has `list' and `circular-list', and
  ! its `cons*' procedure is similar to our `list_with_tail'.)
  public :: list             ! Generic function: make a list from
                             ! elements.
  public :: list_with_tail   ! Generic function: make a list from
                             ! elements and a tail.
  public :: unlist           ! Generic function: extract elements from
                             ! a list.
  public :: unlist_with_tail ! Generic function: extract elements and
                             ! a tail from a list.
  public :: circular_list    ! Generic function: make a circular list
                             ! from elements.

  ! Implementations of list. These include `list0', which takes no
  ! arguments and returns a nil list.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: list[]n
])dnl

  ! Implementations of list_with_tail.
  !
  ! Note that there is no `list0_with_tail'. Why not? Because the tail
  ! might be a degenerate dotted list. Therefore a `list0_with_tail'
  ! function would have to return class(*). All the other
  ! list_with_tail implementations can -- and do -- return
  ! type(cons_t). The distinction is undesirable, so `list0_with_tail'
  ! is simply left out.
  !
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: list[]n[]_with_tail
])dnl

  ! Implementations of unlist.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: unlist[]n
])dnl

  ! Implementations of unlist_with_tail.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: unlist[]n[]_with_tail
])dnl

  ! Implementations of circular_list.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: circular_list[]n
])dnl

  ! Zipping: joining the elements of separate lists into a list of
  ! lists.
  public :: zip              ! Generic function.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: zip[]n
])dnl

  ! Unzipping: separating the elements of a list of lists into
  ! separate lists.
  public :: unzip            ! Generic function.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: unzip[]n
])dnl

  ! unzip1f is the same as unzip1, except as a function instead of a
  ! subroutine.
  public :: unzip1f

  ! SRFI-1 does not have `classify_list', although it does have
  ! procedures this module derives from it (`proper-list?',
  ! `dotted-list?', and `circular-list?).
  public :: classify_list    ! Classify a list as proper, dotted, or circular.
  public :: is_proper_list   ! A list that terminates in a NIL.
  public :: is_dotted_list   ! A list that terminates in a non-NIL.
  public :: is_circular_list ! A list that does not terminate.

  public :: length           ! The length of a proper or dotted list.
  public :: lengthc          ! The length of a proper or dotted list, or -1 for a circular list. (SRFI-1 has `length+'.)

  ! Some generic functions for breaking a list at a point specified by
  ! an integer index:
  public :: take             ! Return a freshly allocated copy of the first n elements of a list.
  public :: takex            ! Like take, but allowed to destroy its inputs. (Currently, it cannot handle circular lists.)
  public :: drop             ! Return a common tail containing all but the first n elements of a list.
  public :: take_right       ! Return a common tail containing the last n elements of a list.
  public :: drop_right       ! Return a freshly allocated copy of all but the last n elements of a list.
  public :: drop_rightx      ! Like drop_right, but allowed to destroy its inputs.
  public :: do_split_at      ! Do both take and drop, at the same time. (Subroutine version.)
  public :: do_split_atx     ! Like do_split_at, but allowed to destroy its inputs.
  public :: split_at         ! Do both take and drop, at the same time. (Function version.)
  public :: split_atx        ! Like split_at, but allowed to destroy its inputs.

  ! Implementations for INTEGER([SIZE_KIND]).
  public :: take_size_kind
  public :: takex_size_kind
  public :: drop_size_kind
  public :: take_right_size_kind
  public :: drop_right_size_kind
  public :: drop_rightx_size_kind
  public :: do_split_at_size_kind
  public :: do_split_atx_size_kind
  public :: split_at_size_kind
  public :: split_atx_size_kind

  ! Implementations for INTEGER of the default kind.
  public :: take_int
  public :: takex_int
  public :: drop_int
  public :: take_right_int
  public :: drop_right_int
  public :: drop_rightx_int
  public :: do_split_at_int
  public :: do_split_atx_int
  public :: split_at_int
  public :: split_atx_int

  public :: last_pair        ! Return the last pair of a list.
  public :: last             ! Return the last CAR of a list.

  ! Generic function: return a list of a single value, repeated, or
  ! with unspecified contents.
  public :: make_list

  ! Implementations of make_list.
  public :: make_list_unspecified_fill_size_kind
  public :: make_list_unspecified_fill_int
  public :: make_list_fill_size_kind
  public :: make_list_fill_int

  ! Return a list of values determined by a procedure.
  public :: list_tabulate_init_proc_t ! The type for the initialization procedure.
  public :: list_tabulate0   ! Indices start at 0_size_kind.
  public :: list_tabulate1   ! Indices start at 1_size_kind.
  public :: list_tabulaten   ! Indices start at n.

  abstract interface
     recursive subroutine list_tabulate_init_proc_t (i, x)
       import size_kind
       integer(size_kind), intent(in) :: i
       class(*), allocatable, intent(out) :: x
     end subroutine list_tabulate_init_proc_t
  end interface

  ! iota: return a list containing a sequence of equally spaced integers.
  public :: iota             ! `iota' = the most generic function.
  public :: iota_of_length   ! Other generics.
  public :: iota_of_length_start
  public :: iota_of_length_start_step
  public :: iota_of_length_size_kind ! Versions for INTEGER([SIZE_KIND]).
  public :: iota_of_length_start_size_kind
  public :: iota_of_length_start_step_size_kind
  public :: iota_of_length_int ! Versions for INTEGER of the default kind.
  public :: iota_of_length_start_int
  public :: iota_of_length_start_step_int

  public :: list_copy        ! Make a copy of a list.
  public :: reverse          ! Make a copy of a list, but reversed.
  public :: reversex         ! Like reverse, but allowed to destroy
                             ! its inputs.
  public :: append           ! Generic function: concatenate two
                             ! lists.
  public :: appendx          ! Generic function: like append, but
                             ! allowed to destroy all its argument
                             ! lists but the last (which becomes a
                             ! shared tail).
  public :: append_reverse   ! Concatenate the reverse of the first
                             ! list to the (unreversed) second list.
  public :: append_reversex  ! Like append_reverse, but allowed to
                             ! destroy its *first* argument (but not
                             ! the latter argument).
  public :: concatenate      ! Concatenate the lists in a list of
                             ! lists.
  public :: concatenatex     ! Like concatenate, but allowed to
                             ! destroy its inputs.

  ! Implementations of append and appendx.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: append[]n, appendx[]n
])dnl

  ! `make_circular' and `make_circularx' are not part of SRFI-1, but
  !  are related to `circular-list'.
  public :: make_circular    ! Make a copy of a list, but with the
                             ! tail connected to the head.
  public :: make_circularx   ! Like make_circular, but allowed to
                             ! destroy its inputs.

  public :: list_equal       ! Generic function: Test whether two or
                             ! more lists are `equal'. (Equivalent to
                             ! SRFI-1's `list='.)

  ! Implementations of list_equal.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: list_equal[]n
])dnl

  ! Count elements that satisfy a predicate. Counting proceeds in
  ! left-to-right order. (This is called `list_count' instead of
  ! `count' because Fortran has an intrinsic `count' function.)
  public :: list_count       ! Generic function.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: list_count[]n
])dnl

  public :: map              ! Generic function: map list elements in
                             ! an unspecified order.
  public :: map_in_order     ! Generic function: map list elements
                             ! left-to-right. (A kind of combination
                             ! of map and for_each.)
  public :: filter_map       ! Generic function: like map, but do not
                             ! save any results that satisfy `type is
                             ! (logical)' and are .false.
  public :: for_each         ! Generic function: perform side effects
                             ! on list elements, in order from left to
                             ! right.
  public :: pair_for_each    ! Generic function: like for_each, but
                             ! with the procedure applied to sublists
                             ! rather than elements.

  ! Implementations of map, taking a subroutine as the mapping
  ! procedure.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: map[]n[]_subr
])dnl

  ! Implementations of map_in_order, taking a subroutine as the
  ! mapping procedure.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: map[]n[]_in_order_subr
])dnl

  ! Implementations of filter_map, taking a subroutine as the mapping
  ! procedure.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: filter_map[]n[]_subr
])dnl

  ! Implementations of for_each.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: for_each[]n[]
])dnl

  ! Implementations of pair_for_each.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: pair_for_each[]n[]
])dnl

  public :: filter           ! Return the elements of a list that *do*
                             ! satisfy a predicate.
  public :: filterx          ! Like filter, but allowed to destroy its
                             ! input.
  public :: remove           ! Return the elements of a list that *do
                             ! not* satisfy a predicate.
  public :: removex          ! Like remove, but allowed to destroy its
                             ! input.
  public :: do_partition     ! Combines filter and remove, to make two
                             ! lists out of the elements of the input
                             ! list. (Subroutine version.)
  public :: do_partitionx    ! Like do_partition, but allowed to
                             ! destroy its input.
  public :: partition        ! Combines filter and remove, to make two
                             ! lists out of the elements of the input
                             ! list. (Function version.)
  public :: partitionx       ! Like partition, but allowed to destroy
                             ! its input.

  public :: member           ! Return the first sublist whose CAR
                             ! `equals' a given value, or return a nil
                             ! list. (SRFI-1 `member' would return #f
                             ! instead of '(), and its argument order
                             ! is different. Also note that `member'
                             ! can be used for tests other than
                             ! equalities. See SRFI-1.)
  public :: is_member        ! is_not_nil (member (...))
  public :: is_not_member    ! is_nil (member (...))

  public :: find             ! Return the first element of that
                             ! satisfies a predicate. Return
                             ! .false. if no element does.
  public :: find_tail        ! Return the first pair whose CAR
                             ! satisfies a predicate; if no pair does,
                             ! return a nil list. (SRFI-1 `find-tail'
                             ! returns #f instead of '().)

  public :: take_while       ! Return the longest initial prefixt
                             ! whose elements all satisfy a predicate.
  public :: take_whilex      ! Like take_while, but allowed to destroy
                             ! its inputs.
  public :: drop_while       ! Drop the longest initial prefix whose
                             ! elements satisfy a predicate.
  public :: do_span          ! A combination of take_while and
                             ! drop_while. See SRFI-1. (Subroutine
                             ! version.)
  public :: do_spanx         ! Like do_span, but allowed to destroy
                             ! its inputs.
  public :: do_break         ! Like do_span, but with the sense of the
                             ! predicate reversed. See SRFI-1.
  public :: do_breakx        ! Like do_break, but allowed to destroy
                             ! its inputs.
  public :: span             ! A combination of take_while and
                             ! drop_while. See SRFI-1. (Function
                             ! version.)
  public :: spanx            ! Like span, but allowed to destroy its
                             ! inputs.
  public :: break            ! Like span, but with the sense of the
                             ! predicate reversed. See SRFI-1.
  public :: breakx           ! Like break, but allowed to destroy its
                             ! inputs.

  public :: delete           ! Remove all elements that `equal' a
                             ! given value. (SRFI-1 `delete' has a
                             ! different argument order.  Also note
                             ! that `delete' can be used for tests
                             ! other than equalities. See SRFI-1.)
  public :: deletex          ! Like delete, but allowed to destroy its
                             ! inputs.

  public :: delete_duplicates ! See SRFI-1. (Note that SRFI-1
                              ! `delete_duplicates' has a different
                              ! argument order.)
  public :: delete_duplicatesx ! Like delete_duplicates, but allowed
                               ! to destroy is inputs.

  ! SRFI-1 `any' has been renamed `some' here, to avoid conflict with
  ! the Fortran intrinsic function `any'. Also it returns specifically
  ! a logical value, .true. or .false. The SRFI-1 behavior is
  ! available as `some_map' (by analogy to `filter_map').
  public :: some             ! Generic function: applies a predicate
                             ! across lists, returning .true. if the
                             ! predicate returns .true. on any
                             ! application.
  public :: some_map         ! Generic function: applies a mapping
                             ! procedure across lists, returning the
                             ! result of the mapping the first time it
                             ! comes out as a value other than .false.

  ! SRFI-1 `every' becomes Fortran `every' and `every_map', in the way
  ! `any' becomes `some' and `some_map'.
  public :: every            ! Generic function: applies a predicate
                             ! across lists, returning .true. if the
                             ! predicate returns .true. on every
                             ! application.
  public :: every_map        ! Generic function: applies a mapping
                             ! procedure across lists, returning the
                             ! result of the last mapping, if no
                             ! application of the procedure returns
                             ! .false.

  ! Implementations of `some'.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: some[]n
])dnl

  ! Implementations of `some_map'.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: some_map[]n[]_subr
])dnl

  ! Implementations of `every'.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: every[]n
])dnl

  ! Implementations of `every_map'.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: every_map[]n[]_subr
])dnl

  ! The list_index functions are designed so they always return a
  ! negative number on failure to satisfy the predicate, and so the
  ! negative number is always -1, if the index base is non-negative.
  ! This seemed a convenient convention.
  public :: list_index0      ! Generic function: return the 0-based
                             ! index where a predicate is first
                             ! satisfied, or -1 if it is never
                             ! satisfied.
  public :: list_index1      ! Generic function: return the 1-based
                             ! index where a predicate is first
                             ! satisfied, or -1 if it is never
                             ! satisfied.
  public :: list_indexn      ! Generic function: return the n-based
                             ! index where a predicate is first
                             ! satisfied, or min (-1, n - 1) if it is
                             ! never satisfied.

  ! Implementations of list_index0.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: list_index0_[]n
])dnl

  ! Implementations of list_index1.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: list_index1_[]n
])dnl

  ! Implementations of list_indexn.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: list_indexn_[]n
])dnl

!!!-------------------------------------------------------------------
!!!
!!! ASSOCIATION LISTS
!!!

  public :: assoc            ! Return the first association pair to
                             ! match the key. If there is no match,
                             ! return a nil. (SRFI-1 `assoc', by
                             ! contrast, returns #f; also its argument
                             ! order is different. We return a nil
                             ! because both pairs and nil are
                             ! type(cons_t).)
  public :: alist_cons       ! CONS an association pair onto a list.
  public :: alist_copy       ! Copy an association list, being sure to
                             ! copy each association pair, as well.
  public :: alist_delete     ! Delete all associations matching the
                             ! key. (The argument order is different
                             ! from that of SRFI-1 `alist-delete'.)
  public :: alist_deletex    ! Like alist_delete, but allowed to
                             ! destroy its input association list.

!!!-------------------------------------------------------------------
!!!
!!! FOLDS AND UNFOLDS
!!!

  public :: fold             ! Generic function: `the fundamental list
                             ! iterator.'
  public :: fold_right       ! Generic function: `the fundamental list
                             ! recursion operator.'
  public :: pair_fold        ! Generic function: like fold, but
                             ! applied to sublists instead of
                             ! elements.
  public :: pair_fold_right  ! Generic function: like fold_right, but
                             ! applied to sublists instead of
                             ! elements.
  public :: reduce           ! Generic function: A variant of
                             ! fold. See SRFI-1.
  public :: reduce_right     ! Generic function: A variant of
                             ! fold_right. See SRFI-1.

  public :: unfold           ! Generic: `The fundamental recursive
                             ! list constructor.' See SRFI-1.
  public :: unfold_with_tail_gen ! An implementation.
  public :: unfold_with_nil_tail ! Another implementation.

  public :: unfold_right     ! Generic: `The fundamental iterative
                             ! list constructor.' See SRFI-1.
  public :: unfold_right_with_tail     ! An implementation.
  public :: unfold_right_with_nil_tail ! Another implementation.

  ! Implementations of fold.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: fold[]n[]_subr
])dnl

  ! Implementations of fold_right.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: fold[]n[]_right_subr
])dnl

  ! Implementations of pair_fold.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: pair_fold[]n[]_subr
])dnl

  ! Implementations of pair_fold_right.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: pair_fold[]n[]_right_subr
])dnl

  ! Implementations of reduce.
  public :: reduce_subr

  ! Implementations of reduce_right
  public :: reduce_right_subr

!!!-------------------------------------------------------------------

  ! Types for predicates.
  public :: list_predicate1_t ! A predicate taking 1 argument.
m4_forloop([n],[2],ZIP_MAX,[dnl
  public :: list_predicate[]n[]_t ! A predicate taking n arguments.
])dnl

  abstract interface
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive function list_predicate[]n[]_t (x1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          ])x[]k])) result (bool)
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: [x]k
])dnl
       logical :: bool
     end function list_predicate[]n[]_t
])dnl
  end interface

!!!-------------------------------------------------------------------
!!
!! Types for folds, unfolds, maps, and side effects.
!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: list_kons[]n[]_subr_t
])dnl

  abstract interface
     !
     ! Types for the `kons' argument to a fold procedure.
     !
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive subroutine list_kons[]n[]_subr_t (kar1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          ])kar[]k]), kdr, kons_result)
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: kar[]k
])dnl
       class(*), intent(in) :: kdr
       class(*), allocatable, intent(out) :: kons_result
     end subroutine list_kons[]n[]_subr_t
])dnl
  end interface

m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: list_map[]n[]_subr_t
])dnl

  abstract interface
     !
     ! Types for the per-element-mapping argument to a map procedure,
     ! an unfold, etc.
     !
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive subroutine list_map[]n[]_subr_t (input1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          ])input[]k]), output)
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: input[]k
])dnl
       class(*), allocatable, intent(out) :: output
     end subroutine list_map[]n[]_subr_t
])dnl
  end interface

m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: list_side_effects[]n[]_t
])dnl

  abstract interface
     !
     ! Types for the per-element-procedure argument to a for_each
     ! procedure; for the per-sublist-procedure argument to a
     ! pair_for_each procedure; etc.
     !
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive subroutine list_side_effects[]n[]_t (input1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          ])input[]k]))
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: input[]k
])dnl
     end subroutine list_side_effects[]n[]_t
])dnl
  end interface

!!!-------------------------------------------------------------------

  type :: pair_data_t
     class(*), allocatable :: car
     class(*), allocatable :: cdr
  end type pair_data_t

  ! A cons_t is NIL if its heap_element pointer is not associated.
  type, extends (collectible_t) :: cons_t
   contains
     procedure, pass :: get_branch => cons_t_get_branch
     procedure, pass :: assign => cons_t_assign
     generic :: assignment(=) => assign
  end type cons_t

  type(cons_t), parameter :: nil = cons_t ()

  interface operator(**)
     module procedure infix_right_cons
  end interface operator(**)

  interface operator(.tocons.)
     module procedure cons_t_cast
  end interface operator(.tocons.)

  interface list_ref0
     module procedure list_ref0_size_kind
     module procedure list_ref0_int
  end interface list_ref0

  interface list_ref1
     module procedure list_ref1_size_kind
     module procedure list_ref1_int
  end interface list_ref1

  interface list_refn
     module procedure list_refn_size_kind
     module procedure list_refn_int
  end interface list_refn

  interface take
     module procedure take_size_kind
     module procedure take_int
  end interface take

  interface takex
     module procedure takex_size_kind
     module procedure takex_int
  end interface takex

  interface drop
     module procedure drop_size_kind
     module procedure drop_int
  end interface drop

  interface take_right
     module procedure take_right_size_kind
     module procedure take_right_int
  end interface take_right

  interface drop_right
     module procedure drop_right_size_kind
     module procedure drop_right_int
  end interface drop_right

  interface drop_rightx
     module procedure drop_rightx_size_kind
     module procedure drop_rightx_int
  end interface drop_rightx

  interface do_split_at
     module procedure do_split_at_size_kind
     module procedure do_split_at_int
  end interface do_split_at

  interface split_at
     module procedure split_at_size_kind
     module procedure split_at_int
  end interface split_at

  interface do_split_atx
     module procedure do_split_atx_size_kind
     module procedure do_split_atx_int
  end interface do_split_atx

  interface split_atx
     module procedure split_atx_size_kind
     module procedure split_atx_int
  end interface split_atx

  interface make_list
     module procedure make_list_unspecified_fill_size_kind
     module procedure make_list_unspecified_fill_int
     module procedure make_list_fill_size_kind
     module procedure make_list_fill_int
  end interface make_list

  interface iota
     module procedure iota_of_length_size_kind
     module procedure iota_of_length_start_size_kind
     module procedure iota_of_length_start_step_size_kind
     module procedure iota_of_length_int
     module procedure iota_of_length_start_int
     module procedure iota_of_length_start_step_int
  end interface iota

  interface iota_of_length
     module procedure iota_of_length_size_kind
     module procedure iota_of_length_int
  end interface iota_of_length

  interface iota_of_length_start
     module procedure iota_of_length_start_size_kind
     module procedure iota_of_length_start_int
  end interface iota_of_length_start

  interface iota_of_length_start_step
     module procedure iota_of_length_start_step_size_kind
     module procedure iota_of_length_start_step_int
  end interface iota_of_length_start_step

  interface unfold
     module procedure unfold_with_tail_gen
     module procedure unfold_with_nil_tail
  end interface unfold

  interface unfold_right
     module procedure unfold_right_with_tail
     module procedure unfold_right_with_nil_tail
  end interface unfold_right

  interface list
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure list[]n
])dnl
  end interface list

  interface list_with_tail
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure list[]n[]_with_tail
])dnl
  end interface list_with_tail

  interface unlist
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure unlist[]n
])dnl
  end interface unlist

  interface unlist_with_tail
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure unlist[]n[]_with_tail
])dnl
  end interface unlist_with_tail

  interface circular_list
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure circular_list[]n
])dnl
  end interface circular_list

  interface zip
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure zip[]n
])dnl
  end interface zip

  interface unzip
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure unzip[]n
])dnl
  end interface unzip

  interface list_equal
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure list_equal[]n
])dnl
  end interface list_equal

  interface list_count
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure list_count[]n
])dnl
  end interface list_count

  interface append
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure append[]n
])dnl
  end interface append

  interface appendx
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure appendx[]n
])dnl
  end interface appendx

  interface map
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure map[]n[]_subr
])dnl
  end interface map

  interface map_in_order
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure map[]n[]_in_order_subr
])dnl
  end interface map_in_order

  interface filter_map
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure filter_map[]n[]_subr
])dnl
  end interface filter_map

  interface for_each
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure for_each[]n[]
])dnl
  end interface for_each

  interface pair_for_each
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure pair_for_each[]n[]
])dnl
  end interface pair_for_each

  interface some
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure some[]n
])dnl
  end interface some

  interface some_map
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure some_map[]n[]_subr
])dnl
  end interface some_map

  interface every
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure every[]n
])dnl
  end interface every

  interface every_map
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure every_map[]n[]_subr
])dnl
  end interface every_map

  interface fold
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure fold[]n[]_subr
])dnl
  end interface fold

  interface list_index0
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure list_index0_[]n
])dnl
  end interface list_index0

  interface list_index1
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure list_index1_[]n
])dnl
  end interface list_index1

  interface list_indexn
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure list_indexn_[]n
])dnl
  end interface list_indexn

  interface fold_right
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure fold[]n[]_right_subr
])dnl
  end interface fold_right

  interface pair_fold
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure pair_fold[]n[]_subr
])dnl
  end interface pair_fold

  interface pair_fold_right
m4_forloop([n],[1],ZIP_MAX,[dnl
     module procedure pair_fold[]n[]_right_subr
])dnl
  end interface pair_fold_right

  interface reduce
     module procedure reduce_subr
  end interface reduce

  interface reduce_right
     module procedure reduce_right_subr
  end interface reduce_right

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

!!!-------------------------------------------------------------------

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

contains

!!!-------------------------------------------------------------------

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module cons_pairs error: ", a)') msg
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

!!!-------------------------------------------------------------------

  subroutine cons_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(cons_t), intent(in) :: this
    integer(sz), intent(in) :: branch_number
    logical, intent(out) :: branch_number_out_of_range
    class(*), allocatable, intent(out) :: branch

    class(*), pointer :: data

    ! A NIL-list has zero branches. A pair has two branches.

    branch_number_out_of_range = .true.
    if (associated (this%heap_element)) then
       if (branch_number == 1) then
          data => this%heap_element%data
          select type (data)
          class is (pair_data_t)
             branch = data%car
             branch_number_out_of_range = .false.
          end select
       else if (branch_number == 2) then
          data => this%heap_element%data
          select type (data)
          class is (pair_data_t)
             branch = data%cdr
             branch_number_out_of_range = .false.
          end select
       end if
    end if
  end subroutine cons_t_get_branch

!!!-------------------------------------------------------------------

  function is_false (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (val => .autoval. obj)
    type is (logical)
       bool = .not. val
    class default
       bool = .false.
    end select
  end function is_false

  function is_not_false (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (val => .autoval. obj)
    type is (logical)
       bool = val
    class default
       bool = .true.
    end select
  end function is_not_false

!!!-------------------------------------------------------------------

  pure function is_pair (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .false.
    select type (obj)
    class is (cons_t)
       bool = associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
          bool = associated (val%heap_element)
       end select
    end select
  end function is_pair

  pure function is_not_pair (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .not. is_pair (obj)
  end function is_not_pair

!!!-------------------------------------------------------------------

  pure function is_nil (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .false.
    select type (obj)
    class is (cons_t)
       bool = .not. associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
          bool = .not. associated (val%heap_element)
       end select
    end select
  end function is_nil

  pure function is_not_nil (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .not. is_nil (obj)
  end function is_not_nil

  recursive function is_nil_list (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (cons_t)
       bool = .not. associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
         bool = .not. associated (val%heap_element)
      class default
         call error_abort ("is_nil_list of a gcroot_t whose value is not a cons_t")
      end select
    class default
       call error_abort ("is_nil_list of an object that is not a cons_t")
    end select
  end function is_nil_list

!!!-------------------------------------------------------------------

  recursive subroutine cons_t_assign (dst, src)
    class(cons_t), intent(inout) :: dst
    class(*), intent(in) :: src

    select type (obj => .autoval. src)
    type is (cons_t)
       dst%heap_element => obj%heap_element
    class default
       call error_abort ("assignment to cons_t of an incompatible object")
    end select
  end subroutine cons_t_assign

  recursive function cons_t_cast (obj) result (lst)
    class(*), intent(in) :: obj
    type(cons_t) :: lst

    select type (src => .autoval. obj)
    class is (cons_t)
       lst%heap_element => src%heap_element
    class default
       call error_abort ("cons_t_cast of an incompatible object")
    end select
  end function cons_t_cast

  recursive function cons_t_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    logical :: bool

    type(cons_t) :: o1, o2

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
  end function cons_t_eq

!!!-------------------------------------------------------------------

  recursive function cons (car_value, cdr_value) result (the_pair)
    class(*), intent(in) :: car_value
    class(*), intent(in) :: cdr_value
    type(cons_t) :: the_pair

    type(heap_element_t), pointer :: new_element
    type(pair_data_t), pointer :: data

    allocate (data)
    data%car = .autoval. car_value
    data%cdr = .autoval. cdr_value
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    the_pair%heap_element => new_element
  end function cons

  recursive function infix_right_cons (car_value, cdr_value) result (the_pair)
    class(*), intent(in) :: car_value
    class(cons_t), intent(in) :: cdr_value
    type(cons_t) :: the_pair

    type(heap_element_t), pointer :: new_element
    type(pair_data_t), pointer :: data

    allocate (data)
    data%car = .autoval. car_value
    data%cdr = cdr_value
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    the_pair%heap_element => new_element
  end function infix_right_cons

  recursive subroutine uncons (the_pair, car_value, cdr_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable, intent(inout) :: car_value
    class(*), allocatable, intent(inout) :: cdr_value

    class(*), allocatable :: car_val
    class(*), allocatable :: cdr_val

    select type (pair => .autoval. the_pair)
    class is (cons_t)
       if (associated (pair%heap_element)) then
          select type (data => pair%heap_element%data)
          class is (pair_data_t)
             car_val = data%car
             cdr_val = data%cdr
          class default
             call strange_error
          end select
          car_value = car_val
          cdr_value = cdr_val
       else
          call error_abort ("uncons of a nil list")
       end if
    class default
       call error_abort ("uncons of an object that is not a cons_t")
    end select
  end subroutine uncons

  recursive function car (the_pair) result (car_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable :: car_value

    select type (pair => .autoval. the_pair)
    class is (cons_t)
       if (associated (pair%heap_element)) then
          select type (data => pair%heap_element%data)
          class is (pair_data_t)
             car_value = data%car
          class default
             call strange_error
          end select
       else
          call error_abort ("car of a nil list")
       end if
    class default
       call error_abort ("car of an object that is not a cons_t")
    end select
  end function car

  recursive function cdr (the_pair) result (cdr_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable :: cdr_value

    select type (pair => .autoval. the_pair)
    class is (cons_t)
       if (associated (pair%heap_element)) then
          select type (data => pair%heap_element%data)
          class is (pair_data_t)
             cdr_value = data%cdr
          class default
             call strange_error
          end select
       else
          call error_abort ("cdr of a nil list")
       end if
    class default
       call error_abort ("cdr of an object that is not a cons_t")
    end select
  end function cdr

  recursive subroutine set_car (the_pair, car_value)
    class(*), intent(in) :: the_pair
    class(*), intent(in) :: car_value

    type(cons_t) :: pair

    pair = the_pair
    if (associated (pair%heap_element)) then
       select type (data => pair%heap_element%data)
       class is (pair_data_t)
          data%car = .autoval. car_value
       end select
    else
       call error_abort ("set_car of a nil list")
    end if
  end subroutine set_car

  recursive subroutine set_cdr (the_pair, cdr_value)
    class(*), intent(in) :: the_pair
    class(*), intent(in) :: cdr_value

    type(cons_t) :: pair

    pair = the_pair
    if (associated (pair%heap_element)) then
       select type (data => pair%heap_element%data)
       class is (pair_data_t)
          data%cdr = .autoval. cdr_value
       end select
    else
       call error_abort ("set_cdr of a nil list")
    end if
  end subroutine set_cdr

!!!-------------------------------------------------------------------

m4_forloop([n],[2],CADADR_MAX,[m4_length_n_cadadr_definitions(n)])dnl
dnl
!!!-------------------------------------------------------------------

  function first (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([1],[element])dnl
  end function first

  function second (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([2],[element])dnl
  end function second

  function third (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([3],[element])dnl
  end function third

  function fourth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([4],[element])dnl
  end function fourth

  function fifth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([5],[element])dnl
  end function fifth

  function sixth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([6],[element])dnl
  end function sixth

  function seventh (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([7],[element])dnl
  end function seventh

  function eighth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([8],[element])dnl
  end function eighth

  function ninth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([9],[element])dnl
  end function ninth

  function tenth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([10],[element])dnl
  end function tenth

!!!-------------------------------------------------------------------

  function list_ref0_size_kind (lst, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = car (drop (lst, i))
  end function list_ref0_size_kind

  function list_ref1_size_kind (lst, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = list_ref0_size_kind (lst, i - 1_sz)
  end function list_ref1_size_kind

  function list_refn_size_kind (lst, n, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = list_ref0_size_kind (lst, i - n)
  end function list_refn_size_kind

  function list_ref0_int (lst, i) result (element)
    class(*), intent(in) :: lst
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: ii

    ii = i
    element = list_ref0_size_kind (lst, ii)
  end function list_ref0_int

  function list_ref1_int (lst, i) result (element)
    class(*), intent(in) :: lst
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: ii

    ii = i
    element = list_ref1_size_kind (lst, ii)
  end function list_ref1_int

  function list_refn_int (lst, n, i) result (element)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: nn
    integer(sz) :: ii

    nn = n
    ii = i
    element = list_refn_size_kind (lst, nn, ii)
  end function list_refn_int

!!!-------------------------------------------------------------------

  function list0 () result (lst)
    type(cons_t) :: lst
    lst = nil
  end function list0

m4_forloop([n],[1],LISTN_MAX,[dnl
  function list[]n (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])obj[]k])) result (lst)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    type(cons_t) :: lst

    lst = obj[]n ** nil
dnl
m4_forloop([k],[2],n,[dnl
    lst = obj[]m4_eval(n - k + 1) ** lst
])dnl
  end function list[]n

])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  function list[]n[]_with_tail (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])obj[]k]), tail) result (lst)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj[]n, tail)
m4_forloop([k],[2],n,[dnl
    lst = obj[]m4_eval(n - k + 1) ** lst
])dnl
  end function list[]n[]_with_tail

])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  subroutine unlist[]n (lst, obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])obj[]k]))
    !
    ! This subroutine `unlists' the n elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
m4_forloop([k],[1],n,[dnl
    class(*), allocatable, intent(inout) :: obj[]k
])dnl

    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, obj1, tail)
dnl
m4_forloop([k],[2],n,[dnl
    call uncons (tail, obj[]k, tail)
])dnl
    if (is_pair (tail)) then
       call error_abort ("unlist[]n[] of a list that is too long")
    end if
  end subroutine unlist[]n

])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  subroutine unlist[]n[]_with_tail (lst, obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])obj[]k]), tail)
    !
    ! This subroutine `unlists' the leading n elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
m4_forloop([k],[1],n,[dnl
    class(*), allocatable, intent(inout) :: obj[]k
])dnl
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, obj1, tl)
dnl
m4_forloop([k],[2],n,[dnl
    call uncons (tl, obj[]k, tl)
])dnl
    tail = tl
  end subroutine unlist[]n[]_with_tail

])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  function circular_list[]n (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])obj[]k])) result (lst)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    type(cons_t) :: lst

    type(cons_t) :: last_pair

    last_pair = obj[]n ** nil
    lst = last_pair
dnl
m4_forloop([k],[2],n,[dnl
    lst = obj[]m4_eval(n - k + 1) ** lst
])dnl
    call set_cdr (last_pair, lst)
  end function circular_list[]n

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  function zip[]n (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (lst_z)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_z

m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl

    if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
       lst_z = nil
m4_if(k,n,[dnl
    else
],[dnl
    else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
dnl
m4_forloop([k],[1],n,[dnl
       call uncons (tail[]k, head[]k, tail[]k)
])dnl
       row = nil
m4_forloop([k],[1],n,[dnl
       row = head[]m4_eval(n - k + 1) ** row
])dnl
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
          continue
m4_if(k,n,[dnl
       else
],[dnl
       else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          done = .false.
          do while (.not. done)
m4_forloop([k],[1],n,[dnl
             call uncons (tail[]k, head[]k, tail[]k)
])dnl
             row = nil
m4_forloop([k],[1],n,[dnl
             row = head[]m4_eval(n - k + 1) ** row
])dnl
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
                done = .true.
m4_if(k,n,[dnl
             end if
],[dnl
             else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          end do
       end if
    end if
  end function zip[]n

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  subroutine unzip[]n (lst_zipped, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k]))
    class(*), intent(in) :: lst_zipped
m4_forloop([k],[1],n,[dnl
    type(cons_t), intent(inout) :: lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    type(cons_t) :: cursor[]k
])dnl

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
m4_forloop([k],[1],n,[dnl
       lst[]k = nil
])dnl
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip[]n, expected a cons_t")
       end select
m4_forloop([k],[1],n,[dnl
       call uncons (head_zipped, head, tl)
       lst[]k = head ** nil
       cursor[]k = lst[]k
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip[]n, expected a cons_t")
       end select
])dnl
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip[]n, expected a cons_t")
          end select
m4_forloop([k],[1],n,[dnl
m4_if(k,n,[dnl
          head = car (head_zipped)
],[dnl
          call uncons (head_zipped, head, tl)
])dnl
          new_pair = head ** nil
          call set_cdr (cursor[]k, new_pair)
          cursor[]k = new_pair
m4_if(k,n,[],[dnl
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip[]n, expected a cons_t")
          end select
])dnl
])dnl
       end do
    end if
  end subroutine unzip[]n

])dnl
dnl
  function unzip1f (lst_zipped) result (lst)
    class(*), intent(in) :: lst_zipped
    type(cons_t) :: lst

    call unzip1 (lst_zipped, lst)
  end function unzip1f

!!!-------------------------------------------------------------------

  subroutine classify_list (obj, is_dotted, is_circular)
    !
    ! An object that is not a cons_t or a nil_t is considered dotted.
    !
    ! Dotted and circular are mutually exclusive.
    !
    ! If an object is neither dotted nor circular, then it is a proper
    ! list.
    !
    class(*), intent(in) :: obj
    logical, intent(out) :: is_dotted
    logical, intent(out) :: is_circular

    ! Detect circularity by having a `lead' reference move through the
    ! list at a higher rate than a `lag' reference. In a circular
    ! list, eventually `lead' will catch up with `lag'.
    class(*), allocatable :: lead
    class(*), allocatable :: lag

    logical :: is_dot
    logical :: is_circ
    logical :: done

    lead = .autoval. obj

    lag = lead
    is_dot = .false.
    is_circ = .false.
    done = .false.
    do while (.not. done)
       if (is_not_pair (lead)) then
          is_dot = is_not_nil (lead)
          done = .true.
       else
          lead = cdr (lead)
          if (is_not_pair (lead)) then
             is_dot = is_not_nil (lead)
             done = .true.
          else
             lead = cdr (lead)
             lag = cdr (lag)
             if (is_pair (lead)) then
                if (cons_t_eq (lead, lag)) then
                   is_circ = .true.
                   done = .true.
                end if
             end if
          end if
       end if
    end do

    is_dotted = is_dot
    is_circular = is_circ
  end subroutine classify_list

  function is_proper_list (obj) result (is_proper)
    class(*), intent(in) :: obj
    logical :: is_proper

    logical :: is_dot
    logical :: is_circ

    call classify_list (obj, is_dot, is_circ)
    is_proper = (.not. is_dot) .and. (.not. is_circ)
  end function is_proper_list

  function is_dotted_list (obj) result (is_dotted)
    !
    ! Note that is_dotted_list(4), is_dotted_list("abc"), etc., return
    ! .true.
    !
    ! One consequence is that
    !
    !    .not. is_dotted_list (x)
    !
    ! is equivalent to
    !
    !    is_proper_list (x) .or. is_circular_list (x)
    !
    class(*), intent(in) :: obj
    logical :: is_dotted

    logical :: is_dot
    logical :: is_circ

    call classify_list (obj, is_dot, is_circ)
    is_dotted = is_dot
  end function is_dotted_list

  function is_circular_list (obj) result (is_circular)
    class(*), intent(in) :: obj
    logical :: is_circular

    logical :: is_dot
    logical :: is_circ

    call classify_list (obj, is_dot, is_circ)
    is_circular = is_circ
  end function is_circular_list

!!!-------------------------------------------------------------------

  function length (lst) result (len)
    !
    ! Returns zero as the length of a degenerate dotted list.
    !
    class(*), intent(in) :: lst
    integer(sz) :: len

    class(*), allocatable :: tail

    len = 0
    select type (lst1 => .autoval. lst)
    class is (cons_t)
       tail = lst1
       do while (is_pair (tail))
          len = len + 1
          tail = cdr (tail)
       end do
    end select
  end function length

  function lengthc (lst) result (len)
    !
    ! A variant of length that returns -1 if the list is circular.
    !
    class(*), intent(in) :: lst
    integer(sz) :: len

    ! Detect circularity by having a `lead' reference move through the
    ! list at a higher rate than a `lag' reference. In a circular
    ! list, eventually `lead' will catch up with `lag'.
    class(*), allocatable :: lead
    class(*), allocatable :: lag

    logical :: done

    len = 0
    lead = .autoval. lst
    lag = lead
    done = .false.
    do while (.not. done .and. is_pair (lead))
       lead = cdr (lead)
       len = len + 1
       if (is_pair (lead)) then
          lead = cdr (lead)
          lag = cdr (lag)
          len = len + 1
          select type (lead)
          class is (cons_t)
             select type (lag)
             class is (cons_t)
                if (cons_t_eq (lead, lag)) then
                   len = -1
                   done = .true.
                end if
             end select
          end select
       else
          done = .true.
       end if
    end do
  end function lengthc

!!!-------------------------------------------------------------------

  function take_size_kind (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_t

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    integer(sz) :: i

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (n <= 0) then
          lst_t = nil
       else
          if (is_not_pair (lst1)) then
             call error_abort ("positive `take' of a nil list")
          else
             call uncons (lst1, head, tail)
             cursor = head ** nil
             lst_t = cursor
             i = n - 1
             do while (0 < i .and. is_pair (tail))
                call uncons (tail, head, tail)
                new_pair = head ** nil
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                i = i - 1
             end do
          end if
       end if
    class default
       call error_abort ("`take' of an object that is not a cons_t")
    end select
  end function take_size_kind

  function take_int (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_t

    integer(sz) :: nn

    nn = n
    lst_t = take_size_kind (lst, nn)
  end function take_int

  function takex_size_kind (lst, n) result (lst_t)
    !
    ! NOTE: This implementation cannot handle circular lists. If lst
    !       may be circular, either check first and call `take'
    !       instead of `takex', if lst is circular; or else simply use
    !       `take' instead.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_t

    type(cons_t) :: lst1
    type(cons_t) :: new_last_pair

    if (n <= 0) then
       lst_t = nil
    else
       lst1 = lst
       if (is_not_pair (lst1)) then
          lst_t = nil
       else
          lst_t = lst1
          new_last_pair = drop (lst_t, n - 1)
          call set_cdr (new_last_pair, nil)
       end if
    end if
  end function takex_size_kind

  function takex_int (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_t

    integer(sz) :: nn

    nn = n
    lst_t = takex_size_kind (lst, nn)
  end function takex_int

  function drop_size_kind (lst, n) result (lst_d)
    !
    ! If lst is dotted, then the result will be dotted.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    class(*), allocatable :: lst_d

    integer(sz) :: i

    lst_d = .autoval. lst
    do i = 1_sz, n
       lst_d = cdr (lst_d)
    end do
  end function drop_size_kind

  function drop_int (lst, n) result (lst_d)
    !
    ! If lst is dotted, then the result will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: lst_d

    integer(sz) :: nn

    nn = n
    lst_d = drop_size_kind (lst, nn)
  end function drop_int

  function take_right_size_kind (lst, n) result (lst_tr)
    !
    ! lst may be dotted, in which case the result will be dotted. lst
    ! must not be circular.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    class(*), allocatable :: lst_tr

    class(*), allocatable :: p

    lst_tr = .autoval. lst
    p = drop (lst_tr, n)
    do while (is_pair (p))
       lst_tr = cdr (lst_tr)
       p = cdr (p)
    end do
  end function take_right_size_kind

  function take_right_int (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: lst_t

    integer(sz) :: nn

    nn = n
    lst_t = take_right_size_kind (lst, nn)
  end function take_right_int

  function drop_right_size_kind (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_dr

    lst_dr = take (lst, length (lst) - n)
  end function drop_right_size_kind

  function drop_right_int (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_dr

    integer(sz) :: nn

    nn = n
    lst_dr = drop_right_size_kind (lst, nn)
  end function drop_right_int

  function drop_rightx_size_kind (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_dr

    lst_dr = takex (lst, length (lst) - n)
  end function drop_rightx_size_kind

  function drop_rightx_int (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_dr

    integer(sz) :: nn

    nn = n
    lst_dr = drop_rightx_size_kind (lst, nn)
  end function drop_rightx_int

  subroutine do_split_at_size_kind (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    type(cons_t) :: lst_t
    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    integer(sz) :: i

    if (n <= 0) then
       lst_left = nil
       lst_right = .autoval. lst
    else
       select type (lst1 => .autoval. lst)
       class is (cons_t)
          if (is_nil (lst1)) then
             call error_abort ("positive split_at of a nil list")
          else
             call uncons (lst1, head, tail)
             lst_t = cons (head, tail)
             cursor = lst_t
             i = n - 1
             do while (0 < i .and. is_pair (tail))
                call uncons (tail, head, tail)
                new_pair = cons (head, tail)
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                i = i - 1
             end do
             if (i == 0) then
                call set_cdr (cursor, nil)
             else
                call error_abort ("split_at of a list that is too short")
             end if
             lst_left = lst_t
             lst_right = tail
          end if
       class default
          call error_abort ("positive split_at of an object with no pairs")
       end select
    end if
  end subroutine do_split_at_size_kind

  function split_at_size_kind (lst, n) result (retval)
    !
    ! Returns list (lst_left, lst_right).
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: retval

    type(cons_t) :: lst_left
    class(*), allocatable :: lst_right

    call do_split_at_size_kind (lst, n, lst_left, lst_right)
    retval = lst_left ** lst_right ** nil
  end function split_at_size_kind

  subroutine do_split_at_int (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    integer(sz) :: nn

    nn = n
    call do_split_at_size_kind (lst, nn, lst_left, lst_right)
  end subroutine do_split_at_int

  function split_at_int (lst, n) result (retval)
    !
    ! Returns list (lst_left, lst_right).
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: retval

    type(cons_t) :: lst_left
    class(*), allocatable :: lst_right

    call do_split_at_int (lst, n, lst_left, lst_right)
    retval = lst_left ** lst_right ** nil
  end function split_at_int

  subroutine do_split_atx_size_kind (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! lst_left will be a cons_t, but lst_right need not be.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    class(*), allocatable :: lst1

    if (n <= 0) then
       lst_left = nil
       lst_right = .autoval. lst
    else
       lst1 = .autoval. lst
       if (is_not_pair (lst1)) then
          call error_abort ("positive do_split_atx of an object with no pairs")
       else
          lst_left = lst1
          lst1 = drop (lst_left, n - 1)
          lst_right = cdr (lst1)
          call set_cdr (lst1, nil)
       end if
    end if
  end subroutine do_split_atx_size_kind

  function split_atx_size_kind (lst, n) result (retval)
    !
    ! Returns list (lst_left, lst_right).
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: retval

    type(cons_t) :: lst_left
    class(*), allocatable :: lst_right

    call do_split_atx_size_kind (lst, n, lst_left, lst_right)
    retval = lst_left ** lst_right ** nil
  end function split_atx_size_kind

  subroutine do_split_atx_int (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    integer(sz) :: nn

    nn = n
    call do_split_atx_size_kind (lst, nn, lst_left, lst_right)
  end subroutine do_split_atx_int

  function split_atx_int (lst, n) result (retval)
    !
    ! Returns list (lst_left, lst_right).
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: retval

    type(cons_t) :: lst_left
    class(*), allocatable :: lst_right

    call do_split_atx_int (lst, n, lst_left, lst_right)
    retval = lst_left ** lst_right ** nil
  end function split_atx_int

!!!-------------------------------------------------------------------

  function last_pair (lst) result (the_last_pair)
    class(*), intent(in) :: lst
    type(cons_t) :: the_last_pair

    class(*), allocatable :: tail

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       the_last_pair = lst1
       if (is_pair (the_last_pair)) then
          tail = cdr (the_last_pair)
          do while (is_pair (tail))
             the_last_pair = tail
             tail = cdr (the_last_pair)
          end do
       else
          call error_abort ("last_pair of a nil list")
       end if
    class default
       call error_abort ("last_pair of an object with no pairs")
    end select
  end function last_pair

  function last (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element

    element = car (last_pair (lst))
  end function last

!!!-------------------------------------------------------------------

  function make_list_unspecified_fill_size_kind (length) result (lst)
    integer(sz), intent(in) :: length
    type(cons_t) :: lst

    lst = make_list_fill_size_kind (length, unspecified ())
  end function make_list_unspecified_fill_size_kind

  function make_list_unspecified_fill_int (length) result (lst)
    integer, intent(in) :: length
    type(cons_t) :: lst

    lst = make_list_fill_int (length, unspecified ())
  end function make_list_unspecified_fill_int

  function make_list_fill_size_kind (length, fill_value) result (lst)
    integer(sz), intent(in) :: length
    class(*), intent(in) :: fill_value
    type(cons_t) :: lst

    integer(sz) :: i

    lst = nil
    do i = 1_sz, length
       lst = fill_value ** lst
    end do
  end function make_list_fill_size_kind

  function make_list_fill_int (length, fill_value) result (lst)
    integer, intent(in) :: length
    class(*), intent(in) :: fill_value
    type(cons_t) :: lst

    integer(sz) :: len

    len = length
    lst = make_list_fill_size_kind (len, fill_value)
  end function make_list_fill_int

!!!-------------------------------------------------------------------

  function list_tabulate0 (length, init_subr) result (lst)
    integer(sz), intent(in) :: length
    procedure(list_tabulate_init_proc_t) :: init_subr
    type(cons_t) :: lst

    lst = list_tabulaten (length, 0_sz, init_subr)
  end function list_tabulate0

  function list_tabulate1 (length, init_subr) result (lst)
    integer(sz), intent(in) :: length
    procedure(list_tabulate_init_proc_t) :: init_subr
    type(cons_t) :: lst

    lst = list_tabulaten (length, 1_sz, init_subr)
  end function list_tabulate1

  function list_tabulaten (length, n, init_subr) result (lst)
    !
    ! NOTE: The order in which calls to init_subr are made is to be
    !       considered unspecified.
    !
    integer(sz), intent(in) :: length
    integer(sz), intent(in) :: n
    procedure(list_tabulate_init_proc_t) :: init_subr
    type(cons_t) :: lst

    integer(sz) :: i
    class(*), allocatable :: x

    ! Use a gcroot_t, to protect against any garbage collections done
    ! by init_subr.
    type(gcroot_t) :: lst1

    ! In this implementation: work backwards, from greater indices to
    ! lesser ones, so there will be no need to reverse the list.
    lst1 = nil
    do i = length - 1_sz, 0_sz, -1_sz
       call init_subr (n + i, x)
       lst1 = cons (x, lst1)
    end do
    lst = lst1
  end function list_tabulaten

!!!-------------------------------------------------------------------

  function list_copy (lst) result (lst_c)
    !
    ! Dotted lists will be copied, but the current implementation
    ! cannot copy a circular list.
    !
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_c

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair

    tail = .autoval. lst
    if (is_not_pair (tail)) then
       lst_c = tail
    else
       call uncons (tail, head, tail)
       cursor = head ** nil
       lst_c = cursor
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          new_pair = head ** nil
          call set_cdr (cursor, new_pair)
          cursor = new_pair
       end do
       if (is_not_nil (tail)) then
          call set_cdr (cursor, tail)
       end if
    end if
  end function list_copy

  function reverse (lst) result (lst_r)
    !
    ! The final CDR of any dotted list (including any non-cons_t
    ! object) is dropped.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    class(*), allocatable :: tail
    
    lst_r = nil
    tail = .autoval. lst
    do while (is_pair (tail))
       lst_r = car (tail) ** lst_r
       tail = cdr (tail)
    end do
  end function reverse

  function reversex (lst) result (lst_r)
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (is_pair (lst1)) then
          lst_r = lst1
          call reverse_in_place (lst_r)
       else
          lst_r = nil
       end if
    class default
       lst_r = nil
    end select
  end function reversex

  subroutine reverse_in_place (lst)
    type(cons_t), intent(inout) :: lst

    type(cons_t) :: lst_r
    type(cons_t) :: tail

    lst_r = nil
    do while (is_pair (lst))
       select type (tmp => cdr (lst))
       class is (cons_t)
          tail = tmp
       class default
          call error_abort ("list reversal of an object that is not a pair")
       end select
       call set_cdr (lst, lst_r)
       lst_r = lst
       lst = tail
    end do
    lst = lst_r
  end subroutine reverse_in_place

  function append0 () result (lst_a)
    class(*), allocatable :: lst_a

    lst_a = nil
  end function append0

  function append1 (lst1) result (lst_a)
    class(*), intent(in) :: lst1
    class(*), allocatable :: lst_a

    lst_a = lst1
  end function append1

  function append2 (lst1, lst2) result (lst_a)
    !
    ! The tail of the result is shared with lst2. The CAR elements of
    ! lst1 are copied; the last CDR of lst1 is dropped.
    !
    ! The result need not be a cons_t.
    !
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), allocatable :: lst_a

    class(*), allocatable :: lst1a
    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    type(cons_t) :: new_lst

    lst1a = .autoval. lst1
    if (is_not_pair (lst1a)) then
       lst_a = .autoval. lst2
    else
       call uncons (lst1a, head, tail)
       new_lst = head ** nil
       cursor = new_lst
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          new_pair = head ** nil
          call set_cdr (cursor, new_pair)
          cursor = new_pair
       end do
       if (is_not_nil (tail)) then
          call set_cdr (cursor, tail)
       end if
       call set_cdr (cursor, .autoval. lst2)
       lst_a = new_lst
    end if
  end function append2

m4_forloop([n],[3],ZIP_MAX,[dnl
  function append[]n (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (lst_a)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: lst_a

    lst_a = append2 (lst[]m4_eval(n - 1), lst[]n)
m4_forloop([k],[1],m4_eval(n - 2),[dnl
    lst_a = append2 (lst[]m4_eval(n - k - 1), lst_a)
])dnl
  end function append[]n

])dnl
dnl
  function appendx0 () result (lst_a)
    class(*), allocatable :: lst_a

    lst_a = nil
  end function appendx0

  function appendx1 (lst1) result (lst_a)
    class(*), intent(in) :: lst1
    class(*), allocatable :: lst_a

    lst_a = lst1
  end function appendx1

  function appendx2 (lst1, lst2) result (lst_a)
    !
    ! appendx2 is *not* allowed to destroy lst2, and in fact includes
    ! it in the result as a shared tail.
    !
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_a

    class(*), allocatable :: lst1a

    lst1a = .autoval. lst1
    if (is_not_pair (lst1a)) then
       lst_a = .autoval. lst2
    else
       lst_a = lst1a
       call set_cdr (last_pair (lst_a), .autoval. lst2)
    end if
  end function appendx2

m4_forloop([n],[3],ZIP_MAX,[dnl
  function appendx[]n (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (lst_a)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: lst_a

    lst_a = appendx2 (lst[]m4_eval(n - 1), lst[]n)
m4_forloop([k],[1],m4_eval(n - 2),[dnl
    lst_a = appendx2 (lst[]m4_eval(n - k - 1), lst_a)
])dnl
  end function appendx[]n

])dnl
dnl
  function append_reverse (lst1, lst2) result (lst_ar)
    !
    ! The tail of the result is shared with lst2. The CAR elements of
    ! lst1 are copied; the last CDR of the reverse of lst1 is dropped.
    !
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_ar

    class(*), allocatable :: head
    class(*), allocatable :: tail

    lst_ar = .autoval. lst2
    tail = .autoval. lst1
    do while (is_pair (tail))
       call uncons (tail, head, tail)
       lst_ar = cons (head, lst_ar)
    end do
  end function append_reverse

  function append_reversex (lst1, lst2) result (lst_ar)
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_ar

    lst_ar = appendx (reversex (lst1), lst2)
  end function append_reversex

  function concatenate (lists) result (lst_concat)
    !
    ! If `lists' is empty, then the result is a nil list.
    !
    class(*), intent(in) :: lists
    class(*), allocatable :: lst_concat

    class(*), allocatable :: lists1
    type(cons_t) :: lists_r
    class(*), allocatable :: head, tail

    lists1 = .autoval. lists
    if (is_not_pair (lists1)) then
       lst_concat = nil
    else
       lists_r = reverse (lists1)
       lst_concat = car (lists_r)
       tail = cdr (lists_r)
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          lst_concat = append (head, lst_concat)
       end do
    end if
  end function concatenate

  function concatenatex (lists) result (lst_concat)
    !
    ! If `lists' is empty, then the result is a nil list.
    !
    ! This implementation calls reversex (thus, perhaps contrarily to
    ! the programmer's expectations, destroying `lists'), and it uses
    ! appendx to do the concatenations.
    !
    class(*), intent(in) :: lists
    class(*), allocatable :: lst_concat

    class(*), allocatable :: lists1
    type(cons_t) :: lists_r
    class(*), allocatable :: head, tail

    lists1 = .autoval. lists
    if (is_not_pair (lists1)) then
       lst_concat = nil
    else
       lists_r = reversex (lists1)
       lst_concat = car (lists_r)
       tail = cdr (lists_r)
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          lst_concat = appendx (head, lst_concat)
       end do
    end if
  end function concatenatex

!!!-------------------------------------------------------------------

  function make_circular (lst) result (clst)
    !
    ! Make a fully circular list with the same CARs as lst.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: clst

    type(cons_t) :: the_last_pair

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (is_pair (lst1)) then
          clst = reverse (lst1)
          the_last_pair = clst
          call reverse_in_place (clst)
          call set_cdr (the_last_pair, clst)
       else
          call error_abort ("make_circular of a nil list")
       end if
    class default
       call error_abort ("make_circular of an object with no pairs")
    end select
  end function make_circular

  function make_circularx (lst) result (clst)
    !
    ! Connect the tail of lst to its head, destructively.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: clst

    type(cons_t) :: the_last_pair

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       the_last_pair = last_pair (lst1)
       call set_cdr (the_last_pair, lst1)
       clst = lst1
    class default
       call error_abort ("make_circularx of an object with no pairs")
    end select
  end function make_circularx

!!!-------------------------------------------------------------------

  function iota_of_length_size_kind (length) result (lst)
    integer(sz), intent(in) :: length
    type(cons_t) :: lst

    lst = iota_of_length_start_step_size_kind (length, 0_sz, 1_sz)
  end function iota_of_length_size_kind

  function iota_of_length_start_size_kind (length, start) result (lst)
    integer(sz), intent(in) :: length, start
    type(cons_t) :: lst

    lst = iota_of_length_start_step_size_kind (length, start, 1_sz)
  end function iota_of_length_start_size_kind

  function iota_of_length_start_step_size_kind (length, start, step) result (lst)
    integer(sz), intent(in) :: length, start, step
    type(cons_t) :: lst

    integer(sz) :: i, n

    if (length < 0_sz) then
       call error_abort ("iota with negative length")
    else if (length == 0_sz) then
       lst = nil
    else
       ! Go through the sequence backwards, so we will not have to
       ! reverse the resulting list.
       n = start + ((length - 1_sz) * step)
       lst = nil
       do i = 1_sz, length
          lst = cons (n, lst)
          n = n - step
       end do
    end if
  end function iota_of_length_start_step_size_kind

  function iota_of_length_int (length) result (lst)
    integer, intent(in) :: length
    type(cons_t) :: lst

    lst = iota_of_length_start_step (length, 0, 1)
  end function iota_of_length_int

  function iota_of_length_start_int (length, start) result (lst)
    integer, intent(in) :: length, start
    type(cons_t) :: lst

    lst = iota_of_length_start_step (length, start, 1)
  end function iota_of_length_start_int

  function iota_of_length_start_step_int (length, start, step) result (lst)
    integer, intent(in) :: length, start, step
    type(cons_t) :: lst

    integer :: i, n

    if (length < 0) then
       call error_abort ("iota with negative length")
    else if (length == 0) then
       lst = nil
    else
       ! Go through the sequence backwards, so we will not have to
       ! reverse the resulting list.
       n = start + ((length - 1) * step)
       lst = nil
       do i = 1, length
          lst = cons (n, lst)
          n = n - step
       end do
    end if
  end function iota_of_length_start_step_int

!!!-------------------------------------------------------------------

  recursive function list_equal0 (pred) result (bool)
    procedure(list_predicate2_t) :: pred
    logical :: bool

    bool = .true.
  end function list_equal0

  recursive function list_equal1 (pred, lst1) result (bool)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst1
    logical :: bool

    bool = .true.
  end function list_equal1

  recursive function list_equal2_unrooted (pred, lst1, lst2) result (bool)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    logical :: bool

    type(cons_t) :: p, q
    class(*), allocatable :: p_hd, q_hd
    class(*), allocatable :: p_tl, q_tl
    logical :: done

    p = lst1
    q = lst2
    done = .false.
    do while (.not. done)
       if (is_not_pair (p)) then
          ! lst1 comes to an end.
          bool = is_not_pair (q) ! Does lst2 also come to an end?
          done = .true.
       else if (is_not_pair (q)) then
          ! lst2 comes to an end (even though lst1 does not).
          bool = .false.
          done = .true.
!!!
!!! Do not do the short circuit test for equality, for reasons
!!! explained in a `Post-finalization note' in
!!! https://srfi.schemers.org/srfi-133/srfi-133.html
!!!
!!! Specifically: a NaN is unequal to itself, and therefore a shared
!!! tail containing NaN should be unequal to itself.
!!!
!!!    else if (cons_t_eq (p, q)) then
!!!       ! The two lists share a tail.
!!!       bool = .true.
!!!       done = .true.
!!!
       else
          call uncons (p, p_hd, p_tl)
          call uncons (q, q_hd, q_tl)
          if (.not. pred (p_hd, q_hd)) then
             ! The predicate failed for some elements of lst1 and lst2
             ! respectively.
             bool = .false.
             done = .true.
          else
             p = cons_t_cast (p_tl)
             q = cons_t_cast (q_tl)
          end if
       end if
    end do
  end function list_equal2_unrooted

  recursive function list_equal2 (pred, lst1, lst2) result (bool)
    !
    ! An equivalent to SRFI-1 `(list= pred lst1 lst2)'.
    !
    ! In the call
    !
    !    list_equal2 (pred, lst1, lst2)
    !
    ! pred is applied with an element of lst1 as its first argument
    ! and an element of lst2 as its second argument.
    !
    ! The pred function must be some kind of `equality' test, and not
    ! just any predicate (such as a `less than' test); in particular,
    !
    !    pred (x, y)
    !
    ! must return .true. if x and y are the same object. (Therefore
    ! shared tails always are `equal'.)
    !
    ! The current implementation does not handle circular lists.
    !
    ! WARNING: It is an error to call this procedure if either lst1 or
    !          lst2 is a dotted list.
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    logical :: bool

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root

    lst1_root = lst1
    lst2_root = lst2

    bool = list_equal2_unrooted (pred, lst1, lst2)

    call lst1_root%discard
    call lst2_root%discard
  end function list_equal2

m4_forloop([n],[3],ZIP_MAX,[dnl
  recursive function list_equal[]n (pred, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (bool)
    procedure(list_predicate2_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    logical :: bool

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

    bool = list_equal2_unrooted (pred, lst1, lst2)
m4_forloop([k],[3],n,[dnl
    if (bool) bool = list_equal2_unrooted (pred, lst[]m4_eval(k - 1), lst[]k)
])dnl

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function list_equal[]n

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function list_count[]n (pred, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (total)
    procedure(list_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    integer(sz) :: total

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
    logical :: done

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl

    total = 0
    done = .false.
    do while (.not. done)
       if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
          done = .true.
m4_if(k,n,[dnl
       else
],[dnl
       else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          if (pred (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]))) then
             total = total + 1
          end if
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function list_count[]n

])dnl
dnl
!!!-------------------------------------------------------------------
!!
!! map
!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function map[]n[]_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (lst_m)
    procedure(list_map[]n[]_subr_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_m

    lst_m = map[]n[]_in_order_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k]))
  end function map[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------
!!
!! map_in_order
!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function map[]n[]_in_order_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (lst_m)
    procedure(list_map[]n[]_subr_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_m

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
m4_forloop([k],[1],n,[dnl
       lst_m = nil
m4_if(k,n,[dnl
    else
],[dnl
    else if (is_not_pair (lst[]m4_eval(k + 1))) then
])dnl
])dnl
m4_forloop([k],[1],n,[dnl
       lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
       tail[]k = .autoval. lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
       call uncons (tail[]k, head[]k, tail[]k)
])dnl
       call proc (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
            ])head[]k]), proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
          lst_m = cursor
m4_if(k,n,[dnl
       else
],[dnl
       else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          retval = cursor
          done = .false.
          do while (.not. done)
m4_forloop([k],[1],n,[dnl
             call uncons (tail[]k, head[]k, tail[]k)
])dnl
             call proc (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
                  ])head[]k]), proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
                done = .true.
m4_if(k,n,[dnl
             end if
],[dnl
             else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          end do
          lst_m = retval
       end if

m4_forloop([k],[1],n,[dnl
       call lst[]k[]_root%discard
])dnl
    end if
  end function map[]n[]_in_order_subr

])dnl
dnl
!!!-------------------------------------------------------------------
!!
!! filter_map. These are implemented in terms of private `filter map
!! in order' functions, which *perhaps* one day will be made public.
!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function filter_map[]n[]_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (lst_m)
    procedure(list_map[]n[]_subr_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_m

    lst_m = filter_map[]n[]_in_order_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k]))
  end function filter_map[]n[]_subr

])dnl
dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function filter_map[]n[]_in_order_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (lst_m)
    procedure(list_map[]n[]_subr_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_m

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
    class(*), allocatable :: proc_result1
    class(*), allocatable :: proc_result2
    logical :: all_done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl

    call skip_falses (proc_result1)
    if (all_done) then
       lst_m = nil
    else
       retval = proc_result1 ! Protect proc_result1 from garbage collections.
       call skip_falses (proc_result2)
       if (all_done) then
          lst_m = proc_result1 ** nil
          call retval%discard
       else
          cursor = proc_result2 ** nil
          retval = proc_result1 ** cursor
          call skip_falses (proc_result2)
          do while (.not. all_done)
             new_pair = proc_result2 ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             call skip_falses (proc_result2)
          end do
          lst_m = retval
       end if
    end if

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl

  contains

    recursive subroutine skip_falses (proc_result)
      class(*), allocatable, intent(out) :: proc_result

      logical :: all_skipped

      all_skipped = .false.
      do while (.not. all_skipped)
         if (is_not_pair (tail1)) then
            all_done = .true.
            all_skipped = .true.
m4_forloop([k],[2],n,[dnl
         else if (is_not_pair (tail[]k)) then
            all_done = .true.
            all_skipped = .true.
])dnl
         else
            all_done = .false.
m4_forloop([k],[1],n,[dnl
            call uncons (tail[]k, head[]k, tail[]k)
])dnl
            call proc (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
                 ])head[]k]), proc_result)
            select type (proc_result)
            type is (logical)
               all_skipped = proc_result
            class default
               all_skipped = .true.
            end select
         end if
      end do
    end subroutine skip_falses

  end function filter_map[]n[]_in_order_subr

])dnl
dnl
!!!-------------------------------------------------------------------
!!
!! for_each
!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive subroutine for_each[]n (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k]))
    procedure(list_side_effects[]n[]_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
    logical :: done

    ! Protect the input lists against garbage collections instigated
    ! by proc.
m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl
    done = .false.
    do while (.not. done)
       if (is_not_pair (tail1)) then
          done = .true.
m4_forloop([k],[2],n,[dnl
       else if (is_not_pair (tail[]k)) then
          done = .true.
])dnl
       else
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          call proc (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]))
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end subroutine for_each[]n

])dnl
!!!-------------------------------------------------------------------
!!
!! pair_for_each
!!

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive subroutine pair_for_each[]n (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k]))
    procedure(list_side_effects[]n[]_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: tail[]k
])dnl
    logical :: done

    ! Protect the input lists against garbage collections instigated
    ! by proc.
m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl
    done = .false.
    do while (.not. done)
       if (is_not_pair (tail1)) then
          done = .true.
m4_forloop([k],[2],n,[dnl
       else if (is_not_pair (tail[]k)) then
          done = .true.
])dnl
       else
          call proc (tail1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])tail[]k]))
m4_forloop([k],[1],n,[dnl
          tail[]k = cdr (tail[]k)
])dnl
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end subroutine pair_for_each[]n

])dnl
!!!-------------------------------------------------------------------
!!
!! Filtering and partitioning.
!!

  recursive subroutine drop_falses (pred, lst, first_true)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: first_true

    class(*), allocatable :: p
    logical :: all_dropped

    p = lst
    all_dropped = .false.
    do while (.not. all_dropped)
       if (is_not_pair (p)) then
          first_true = p
          all_dropped = .true.
       else
          if (pred (car (p))) then
             first_true = p
             all_dropped = .true.
          else
             p = cdr (p)
          end if
       end if
    end do
  end subroutine drop_falses

  recursive subroutine drop_trues (pred, lst, first_false)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: first_false

    class(*), allocatable :: p
    logical :: all_dropped

    p = lst
    all_dropped = .false.
    do while (.not. all_dropped)
       if (is_not_pair (p)) then
          first_false = p
          all_dropped = .true.
       else
          if (pred (car (p))) then
             p = cdr (p)
          else
             first_false = p
             all_dropped = .true.
          end if
       end if
    end do
  end subroutine drop_trues

  recursive subroutine take_falses_destructively (pred, lst, last_false, first_true)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: last_false
    class(*), allocatable, intent(out) :: first_true

    class(*), allocatable :: p
    logical :: all_taken

    last_false = lst
    p = cdr (lst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_true = p
          all_taken = .true.
       else
          if (pred (car (p))) then
             first_true = p
             all_taken = .true.
          else
             last_false = p
             p = cdr (p)
          end if
       end if
    end do
  end subroutine take_falses_destructively

  recursive subroutine take_trues_destructively (pred, lst, last_true, first_false)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: last_true
    class(*), allocatable, intent(out) :: first_false

    class(*), allocatable :: p
    logical :: all_taken

    last_true = lst
    p = cdr (lst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_false = p
          all_taken = .true.
       else
          if (pred (car (p))) then
             last_true = p
             p = cdr (p)
          else
             first_false = p
             all_taken = .true.
          end if
       end if
    end do
  end subroutine take_trues_destructively

  recursive subroutine take_falses_nondestructively (pred, lst, falses, last_false, first_true)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: falses
    class(*), allocatable, intent(out) :: last_false
    class(*), allocatable, intent(out) :: first_true

    class(*), allocatable :: p
    type(cons_t) :: new_pair
    logical :: all_taken

    p = cdr (lst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_true = p
          all_taken = .true.
       else
          if (pred (car (p))) then
             first_true = p
             all_taken = .true.
          else
             p = cdr (p)
          end if
       end if
    end do

    if (is_not_pair (first_true)) then
       ! lst ends on a run of falses. Set `falses' to the run of
       ! falses, for use as a shared tail. (There is no need to set
       ! `last_pair'.)
       falses = lst
    else
       ! Copy the run of falses, saving a reference to its last
       ! pair.
       falses = car (lst) ** nil
       last_false = falses
       p = cdr (lst)
       do while (.not. cons_t_eq (p, first_true))
          new_pair = car (p) ** nil
          call set_cdr (last_false, new_pair)
          last_false = new_pair
          p = cdr (p)
       end do
    end if
  end subroutine take_falses_nondestructively

  recursive subroutine take_trues_nondestructively (pred, lst, trues, last_true, first_false)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: trues
    class(*), allocatable, intent(out) :: last_true
    class(*), allocatable, intent(out) :: first_false

    class(*), allocatable :: p
    type(cons_t) :: new_pair
    logical :: all_taken

    p = cdr (lst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_false = p
          all_taken = .true.
       else
          if (pred (car (p))) then
             p = cdr (p)
          else
             first_false = p
             all_taken = .true.
          end if
       end if
    end do

    if (is_not_pair (first_false)) then
       ! lst ends on a run of trues. Set `trues' to the run of
       ! trues, for use as a shared tail. (There is no need to set
       ! `last_pair'.)
       trues = lst
    else
       ! Copy the run of trues, saving a reference to its last pair.
       trues = car (lst) ** nil
       last_true = trues
       p = cdr (lst)
       do while (.not. cons_t_eq (p, first_false))
          new_pair = car (p) ** nil
          call set_cdr (last_true, new_pair)
          last_true = new_pair
          p = cdr (p)
       end do
    end if
  end subroutine take_trues_nondestructively

  recursive function filterx (pred, lst) result (lst_f)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_f

    class(*), allocatable :: first_true
    class(*), allocatable :: last_true
    class(*), allocatable :: first_false
    type(gcroot_t) :: retval
    logical :: done

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       lst_f = .autoval. lst
    else
       retval = lst            ! Protect lst from garbage collections.
       call drop_falses (pred, .autoval. lst, first_true)
       if (is_not_pair (first_true)) then
          ! There are no trues and there is no shared tail.
          lst_f = nil
          call retval%discard
       else
          retval = first_true
          call take_trues_destructively (pred, first_true, last_true, first_false)
          if (is_not_pair (first_false)) then
             ! The entire result is a tail of the input list.
             lst_f = .val. retval
          else
             done = .false.
             do while (.not. done)
                call drop_falses (pred, first_false, first_true)
                if (is_not_pair (first_true)) then
                   ! The tail of the original is a run of
                   ! falses. Remove it, and then the filtering is
                   ! done.
                   call set_cdr (last_true, nil)
                   done = .true.
                else
                   ! Leave out the run of falses, destructively.
                   call set_cdr (last_true, first_true)
                   ! Get the next run of trues.
                   call take_trues_destructively (pred, first_true, last_true, first_false)
                   if (is_not_pair (first_false)) then
                      ! The tail of the original is a run of
                      ! trues. The filtering is done.
                      done = .true.
                   end if
                end if
             end do
             lst_f = .val. retval
          end if
       end if
    end if
  end function filterx

  recursive function removex (pred, lst) result (lst_r)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_r

    class(*), allocatable :: first_false
    class(*), allocatable :: last_false
    class(*), allocatable :: first_true
    type(gcroot_t) :: retval
    logical :: done

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       lst_r = .autoval. lst
    else
       retval = lst            ! Protect lst from garbage collections.
       call drop_trues (pred, .autoval. lst, first_false)
       if (is_not_pair (first_false)) then
          ! There are no trues and there is no shared tail.
          lst_r = nil
          call retval%discard
       else
          retval = first_false
          call take_falses_destructively (pred, first_false, last_false, first_true)
          if (is_not_pair (first_true)) then
             ! The entire result is a tail of the input list.
             lst_r = .val. retval
          else
             done = .false.
             do while (.not. done)
                call drop_trues (pred, first_true, first_false)
                if (is_not_pair (first_false)) then
                   ! The tail of the original is a run of
                   ! falses. Remove it, and then the filtering is
                   ! done.
                   call set_cdr (last_false, nil)
                   done = .true.
                else
                   ! Leave out the run of falses, destructively.
                   call set_cdr (last_false, first_false)
                   ! Get the next run of trues.
                   call take_falses_destructively (pred, first_false, last_false, first_true)
                   if (is_not_pair (first_true)) then
                      ! The tail of the original is a run of
                      ! trues. The filtering is done.
                      done = .true.
                   end if
                end if
             end do
             lst_r = .val. retval
          end if
       end if
    end if
  end function removex

  recursive function filter (pred, lst) result (lst_f)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_f

    class(*), allocatable :: first_true
    class(*), allocatable :: last_true1, last_true2
    class(*), allocatable :: trues
    class(*), allocatable :: first_false
    type(gcroot_t) :: lst_root
    type(gcroot_t) :: retval
    logical :: done

    ! Protect the input from garbage collections instigated by pred.
    lst_root = lst

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       lst_f = .autoval. lst
    else
       call drop_falses (pred, .autoval. lst, first_true)
       if (is_not_pair (first_true)) then
          ! There are no trues and there is no shared tail.
          lst_f = nil
       else
          call take_trues_nondestructively (pred, first_true, trues, last_true1, first_false)
          if (is_not_pair (first_false)) then
             ! The entire result is a tail of the input list. Share
             ! it.
             lst_f = trues
          else
             retval = trues
             done = .false.
             do while (.not. done)
                call drop_falses (pred, first_false, first_true)
                if (is_not_pair (first_true)) then
                   ! The tail of the original is a run of
                   ! falses. Leave it out. Filtering is done.
                   done = .true.
                else
                   ! Leave out the run of falses. Get the next run of
                   ! trues.
                   call take_trues_nondestructively (pred, first_true, trues, last_true2, first_false)
                   call set_cdr (last_true1, trues)
                   if (is_not_pair (first_false)) then
                      ! The tail of the original is a run of
                      ! trues. Keep it as a shared tail. Filtering is
                      ! done.
                      done = .true.
                   else
                      last_true1 = last_true2
                   end if
                end if
             end do
             lst_f = .val. retval
          end if
       end if
    end if

    call lst_root%discard
  end function filter

  recursive function remove (pred, lst) result (lst_r)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_r

    class(*), allocatable :: first_false
    class(*), allocatable :: last_false1, last_false2
    class(*), allocatable :: first_true
    class(*), allocatable :: falses
    type(gcroot_t) :: lst_root
    type(gcroot_t) :: retval
    logical :: done

    ! Protect the input from garbage collections instigated by pred.
    lst_root = lst

    first_true = nil
    first_false = nil

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       lst_r = .autoval. lst
    else
       call drop_trues (pred, .autoval. lst, first_false)
       if (is_not_pair (first_false)) then
          ! There are no falses and there is no shared tail.
          lst_r = nil
       else
          call take_falses_nondestructively (pred, first_false, falses, last_false1, first_true)
          if (is_not_pair (first_true)) then
             ! The entire result is a tail of the input list. Share
             ! it.
             lst_r = falses
          else
             retval = falses
             done = .false.
             do while (.not. done)
                call drop_trues (pred, first_true, first_false)
                if (is_not_pair (first_false)) then
                   ! The tail of the original is a run of
                   ! falses. Leave it out. Filtering is done.
                   done = .true.
                else
                   ! Leave out the run of falses. Get the next run of
                   ! falses.
                   call take_falses_nondestructively (pred, first_false, falses, last_false2, first_true)
                   call set_cdr (last_false1, falses)
                   if (is_not_pair (first_true)) then
                      ! The tail of the original is a run of
                      ! falses. Keep it as a shared tail. Filtering is
                      ! done.
                      done = .true.
                   else
                      last_false1 = last_false2
                   end if
                end if
             end do
             lst_r = .val. retval
          end if
       end if
    end if

    call lst_root%discard
  end function remove

  recursive subroutine do_partitionx (pred, lst, lst_f, lst_r)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: lst_f ! The `filter' list.
    class(*), allocatable, intent(inout) :: lst_r ! The `remove' list.

    class(*), allocatable :: first_true
    class(*), allocatable :: last_true1, last_true2
    class(*), allocatable :: first_false
    class(*), allocatable :: last_false1, last_false2
    type(gcroot_t) :: p
    type(gcroot_t) :: retval_f
    type(gcroot_t) :: retval_r
    logical :: done
    logical :: next_is_true

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object to one or the other output list, arbitrarily. It will
       ! serve as a kind of shared tail.
       lst_f = .autoval. lst
       lst_r = nil
    else
       p = lst
       next_is_true = pred (car (p))

       retval_f = nil
       retval_r = nil
       done = .false.
       do while (.not. done)
          if (next_is_true) then
             call take_trues_destructively (pred, .val. p, last_true2, first_false)
             call attach_p_to_retval_f
             if (is_not_pair (first_false)) then
                ! lst ends on a run of trues. The run of trues will be
                ! a shared tail. The falses get terminated with a nil.
                call terminate_retval_r
                done = .true.
             else
                last_true1 = last_true2
                p = first_false
                next_is_true = .false.
             end if
          else
             call take_falses_destructively (pred, .val. p, last_false2, first_true)
             call attach_p_to_retval_r
             if (is_not_pair (first_true)) then
                ! lst ends on a run of falses. The run of falses will
                ! be a shared tail. The trues get terminated with a
                ! nil.
                call terminate_retval_f
                done = .true.
             else
                last_false1 = last_false2
                p = first_true
                next_is_true = .true.
             end if
          end if
       end do
       lst_f = .val. retval_f
       lst_r = .val. retval_r
    end if

  contains

    subroutine attach_p_to_retval_f
      if (is_nil (retval_f)) then
         ! Start a new list.
         retval_f = p
      else
         ! Extend an existing list.
         call set_cdr (last_true1, p)
      end if
    end subroutine attach_p_to_retval_f

    subroutine attach_p_to_retval_r
      if (is_nil (retval_r)) then
         ! Start a new list.
         retval_r = p
      else
         ! Extend an existing list.
         call set_cdr (last_false1, p)
      end if
    end subroutine attach_p_to_retval_r

    subroutine terminate_retval_f
      if (is_not_nil (retval_f)) then
         ! Terminate the list of trues.
         call set_cdr (last_true1, nil)
      end if
    end subroutine terminate_retval_f

    subroutine terminate_retval_r
      if (is_not_nil (retval_r)) then
         call set_cdr (last_false1, nil)
      end if
    end subroutine terminate_retval_r

  end subroutine do_partitionx

  recursive function partitionx (pred, lst) result (retval)
    !
    ! Returns list2 (lst_f, lst_r).
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: retval

    class(*), allocatable :: lst_f ! The `filter' list.
    class(*), allocatable :: lst_r ! The `remove' list.

    call do_partitionx (pred, lst, lst_f, lst_r)
    retval = lst_f ** lst_r ** nil
  end function partitionx

  recursive subroutine do_partition (pred, lst, lst_f, lst_r)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: lst_f ! The `filter' list.
    class(*), allocatable, intent(inout) :: lst_r ! The `remove' list.

    class(*), allocatable :: first_true
    class(*), allocatable :: last_true1, last_true2
    class(*), allocatable :: first_false
    class(*), allocatable :: last_false1, last_false2
    class(*), allocatable :: trues
    class(*), allocatable :: falses
    type(gcroot_t) :: lst_root
    type(gcroot_t) :: p
    type(gcroot_t) :: retval_f
    type(gcroot_t) :: retval_r
    logical :: done
    logical :: next_is_true

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object to one or the other output list, arbitrarily. It will
       ! serve as a kind of shared tail.
       lst_f = .autoval. lst
       lst_r = nil
    else
       lst_root = lst ! Protect the input list against garbage
                      ! collections by pred.

       p = lst
       next_is_true = pred (car (p))

       retval_f = nil
       retval_r = nil
       done = .false.
       do while (.not. done)
          if (next_is_true) then
             call take_trues_nondestructively (pred, .val. p, trues, last_true2, first_false)
             call attach_trues_to_retval_f
             if (is_not_pair (first_false)) then
                ! lst ends on a run of trues. The run of trues will be
                ! a shared tail. (Unlike in do_partitionx, the falses
                ! are already terminated with a nil.)
                done = .true.
             else
                last_true1 = last_true2
                p = first_false
                next_is_true = .false.
             end if
          else
             call take_falses_nondestructively (pred, .val. p, falses, last_false2, first_true)
             call attach_falses_to_retval_r
             if (is_not_pair (first_true)) then
                ! lst ends on a run of falses. The run of falses will
                ! be a shared tail. (Unlike in do_partitionx, the
                ! trues are already terminated with a nil.)
                done = .true.
             else
                last_false1 = last_false2
                p = first_true
                next_is_true = .true.
             end if
          end if
       end do
       lst_f = .val. retval_f
       lst_r = .val. retval_r

       call lst_root%discard
    end if

  contains

    subroutine attach_trues_to_retval_f
      if (is_nil (retval_f)) then
         ! Start a new list.
         retval_f = trues
      else
         ! Extend an existing list.
         call set_cdr (last_true1, trues)
      end if
    end subroutine attach_trues_to_retval_f

    subroutine attach_falses_to_retval_r
      if (is_nil (retval_r)) then
         ! Start a new list.
         retval_r = falses
      else
         ! Extend an existing list.
         call set_cdr (last_false1, falses)
      end if
    end subroutine attach_falses_to_retval_r

  end subroutine do_partition

  recursive function partition (pred, lst) result (retval)
    !
    ! Returns list2 (lst_f, lst_r).
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: retval

    class(*), allocatable :: lst_f ! The `filter' list.
    class(*), allocatable :: lst_r ! The `remove' list.

    call do_partition (pred, lst, lst_f, lst_r)
    retval = lst_f ** lst_r ** nil
  end function partition

  recursive function take_whilex (pred, lst) result (lst_tw)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: lst_tw

    type(gcroot_t) :: lst_root
    class(*), allocatable :: last_true
    class(*), allocatable :: first_false

    lst_root = lst
    if (is_nil_list (lst_root)) then
       lst_tw = nil
    else if (.not. pred (car (lst_root))) then
       lst_tw = nil
    else
       call take_trues_destructively (pred, .val. lst_root, last_true, first_false)
       call set_cdr (last_true, nil)
       lst_tw = lst_root
    end if
  end function take_whilex

  recursive function take_while (pred, lst) result (lst_tw)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: lst_tw

    type(gcroot_t) :: lst_root
    class(*), allocatable :: trues
    class(*), allocatable :: last_true
    class(*), allocatable :: first_false

    lst_root = lst
    if (is_nil_list (lst_root)) then
       lst_tw = nil
    else if (.not. pred (car (lst_root))) then
       lst_tw = nil
    else
       call take_trues_nondestructively (pred, .val. lst_root, trues, last_true, first_false)
       lst_tw = trues
    end if
  end function take_while

  recursive subroutine do_spanx (pred, lst, lst1, lst2)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t), intent(out) :: lst1
    type(cons_t), intent(out) :: lst2

    if (is_nil_list (lst)) then
       lst1 = nil
       lst2 = nil
    else if (.not. pred (car (lst))) then
       lst1 = nil
       lst2 = lst
    else
       block
         type(gcroot_t) :: lst_root
         class(*), allocatable :: last_true
         class(*), allocatable :: first_false

         lst_root = lst
         call take_trues_destructively (pred, .val. lst_root, last_true, first_false)
         call set_cdr (last_true, nil)
         lst1 = lst_root
         lst2 = first_false
       end block
    end if
  end subroutine do_spanx

  recursive function spanx (pred, lst) result (retval)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: retval

    type(cons_t) :: lst1
    type(cons_t) :: lst2

    call do_spanx (pred, lst, lst1, lst2)
    retval = lst1 ** lst2 ** nil
  end function spanx

  recursive subroutine do_span (pred, lst, lst1, lst2)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t), intent(out) :: lst1
    type(cons_t), intent(out) :: lst2

    if (is_nil_list (lst)) then
       lst1 = nil
       lst2 = nil
    else if (.not. pred (car (lst))) then
       lst1 = nil
       lst2 = lst
    else
       block
         type(gcroot_t) :: lst_root
         class(*), allocatable :: trues
         class(*), allocatable :: last_true
         class(*), allocatable :: first_false

         lst_root = lst
         call take_trues_nondestructively (pred, .val. lst_root, trues, last_true, first_false)
         lst1 = trues
         lst2 = first_false
       end block
    end if
  end subroutine do_span

  recursive function span (pred, lst) result (retval)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: retval

    type(cons_t) :: lst1
    type(cons_t) :: lst2

    call do_span (pred, lst, lst1, lst2)
    retval = lst1 ** lst2 ** nil
  end function span

  recursive subroutine do_breakx (pred, lst, lst1, lst2)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t), intent(out) :: lst1
    type(cons_t), intent(out) :: lst2

    if (is_nil_list (lst)) then
       lst1 = nil
       lst2 = nil
    else if (pred (car (lst))) then
       lst1 = lst
       lst2 = nil
    else
       block
         type(gcroot_t) :: lst_root
         class(*), allocatable :: last_false
         class(*), allocatable :: first_true

         lst_root = lst
         call take_falses_destructively (pred, .val. lst_root, last_false, first_true)
         call set_cdr (last_false, nil)
         lst1 = lst_root
         lst2 = first_true
       end block
    end if
  end subroutine do_breakx

  recursive function breakx (pred, lst) result (retval)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: retval

    type(cons_t) :: lst1
    type(cons_t) :: lst2

    call do_breakx (pred, lst, lst1, lst2)
    retval = lst1 ** lst2 ** nil
  end function breakx

  recursive subroutine do_break (pred, lst, lst1, lst2)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t), intent(out) :: lst1
    type(cons_t), intent(out) :: lst2

    if (is_nil_list (lst)) then
       lst1 = nil
       lst2 = nil
    else if (pred (car (lst))) then
       lst1 = lst
       lst2 = nil
    else
       block
         type(gcroot_t) :: lst_root
         class(*), allocatable :: falses
         class(*), allocatable :: last_false
         class(*), allocatable :: first_true

         lst_root = lst
         call take_falses_nondestructively (pred, .val. lst_root, falses, last_false, first_true)
         lst1 = falses
         lst2 = first_true
       end block
    end if
  end subroutine do_break

  recursive function break (pred, lst) result (retval)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: retval

    type(cons_t) :: lst1
    type(cons_t) :: lst2

    call do_break (pred, lst, lst1, lst2)
    retval = lst1 ** lst2 ** nil
  end function break

!!!-------------------------------------------------------------------

  recursive function is_member (pred, x, lst) result (bool)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    logical :: bool

    bool = is_not_nil (member (pred, x, lst))
  end function is_member

  recursive function is_not_member (pred, x, lst) result (bool)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    logical :: bool

    bool = is_nil (member (pred, x, lst))
  end function is_not_member

  recursive function member (pred, x, lst) result (sublst)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    type(cons_t) :: sublst

    class(*), allocatable :: p
    logical :: done

    type(gcroot_t) :: x_root
    type(gcroot_t) :: lst_root

    x_root = x
    lst_root = lst

    p = .autoval. lst
    done = .false.
    do while (.not. done)
       if (is_not_pair (p)) then
          sublst = nil
          done = .true.
       else if (pred (x, car (p))) then
          sublst = p
          done = .true.
       else
          p = cdr (p)
       end if
    end do

    call x_root%discard
    call lst_root%discard
  end function member

  recursive function find (pred, lst) result (obj)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: obj

    type(cons_t) :: tail

    tail = find_tail (pred, lst)
    if (is_pair (tail)) then
       obj = car (tail)
    else
       obj = .false.
    end if
  end function find

  recursive function find_tail (pred, lst) result (lst_ft)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ft

    type(gcroot_t) :: lst_root
    class(*), allocatable :: p
    logical :: done

    lst_root = lst

    p = .autoval. lst
    done = .false.
    do while (.not. done)
       if (is_nil_list (p)) then
          lst_ft = nil        ! SRFI-1 `find-tail' returns #f instead.
          done = .true.
       else if (pred (car (p))) then
          lst_ft = p
          done = .true.
       else
          p = cdr (p)
       end if
    end do

    call lst_root%discard
  end function find_tail

  recursive function drop_while (pred, lst) result (lst_dw)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: lst_dw

    type(gcroot_t) :: p
    logical :: done

    p = lst
    done = .false.
    do while (.not. done)
       if (is_nil_list (p)) then
          lst_dw = nil
          done = .true.
       else if (pred (car (p))) then
          p = cdr (p)
       else
          lst_dw = p
          done = .true.
       end if
    end do
  end function drop_while

!!!-------------------------------------------------------------------

  recursive subroutine drop_equals_trues (pred, x, lst, first_false)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: first_false

    class(*), allocatable :: p
    logical :: all_dropped

    p = lst
    all_dropped = .false.
    do while (.not. all_dropped)
       if (is_not_pair (p)) then
          first_false = p
          all_dropped = .true.
       else
          if (pred (x, car (p))) then
             p = cdr (p)
          else
             first_false = p
             all_dropped = .true.
          end if
       end if
    end do
  end subroutine drop_equals_trues

  recursive subroutine take_equals_falses_destructively (pred, x, lst, last_false, first_true)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: last_false
    class(*), allocatable, intent(out) :: first_true

    class(*), allocatable :: p
    logical :: all_taken

    last_false = lst
    p = cdr (lst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_true = p
          all_taken = .true.
       else
          if (pred (x, car (p))) then
             first_true = p
             all_taken = .true.
          else
             last_false = p
             p = cdr (p)
          end if
       end if
    end do
  end subroutine take_equals_falses_destructively

  recursive subroutine take_equals_falses_nondestructively (pred, x, lst, falses, last_false, first_true)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: falses
    class(*), allocatable, intent(out) :: last_false
    class(*), allocatable, intent(out) :: first_true

    class(*), allocatable :: p
    type(cons_t) :: new_pair
    logical :: all_taken

    p = cdr (lst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_true = p
          all_taken = .true.
       else
          if (pred (x, car (p))) then
             first_true = p
             all_taken = .true.
          else
             p = cdr (p)
          end if
       end if
    end do

    if (is_not_pair (first_true)) then
       ! lst ends on a run of falses. Set `falses' to the run of
       ! falses, for use as a shared tail. (There is no need to set
       ! `last_pair'.)
       falses = lst
    else
       ! Copy the run of falses, saving a reference to its last
       ! pair.
       falses = car (lst) ** nil
       last_false = falses
       p = cdr (lst)
       do while (.not. cons_t_eq (p, first_true))
          new_pair = car (p) ** nil
          call set_cdr (last_false, new_pair)
          last_false = new_pair
          p = cdr (p)
       end do
    end if
  end subroutine take_equals_falses_nondestructively

  recursive function deletex (pred, x, lst) result (lst_d)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_d

    class(*), allocatable :: first_false
    class(*), allocatable :: last_false
    class(*), allocatable :: first_true
    class(*), allocatable :: xx
    type(gcroot_t) :: retval
    logical :: done

    type(gcroot_t) :: x_root

    ! Protect x against garbage collections instigated by
    ! pred.
    x_root = x

    xx = .autoval. x

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       lst_d = .autoval. lst
    else
       retval = lst            ! Protect lst from garbage collections.
       call drop_equals_trues (pred, xx, .autoval. lst, first_false)
       if (is_not_pair (first_false)) then
          ! There are no trues and there is no shared tail.
          lst_d = nil
          call retval%discard
       else
          retval = first_false
          call take_equals_falses_destructively (pred, xx, first_false, last_false, first_true)
          if (is_not_pair (first_true)) then
             ! The entire result is a tail of the input list.
             lst_d = .val. retval
          else
             done = .false.
             do while (.not. done)
                call drop_equals_trues (pred, xx, first_true, first_false)
                if (is_not_pair (first_false)) then
                   ! The tail of the original is a run of
                   ! falses. Remove it, and then the filtering is
                   ! done.
                   call set_cdr (last_false, nil)
                   done = .true.
                else
                   ! Leave out the run of falses, destructively.
                   call set_cdr (last_false, first_false)
                   ! Get the next run of trues.
                   call take_equals_falses_destructively (pred, xx, first_false, last_false, first_true)
                   if (is_not_pair (first_true)) then
                      ! The tail of the original is a run of
                      ! trues. The filtering is done.
                      done = .true.
                   end if
                end if
             end do
             lst_d = .val. retval
          end if
       end if
    end if

    call x_root%discard
  end function deletex

  recursive function delete (pred, x, lst) result (lst_d)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_d

    class(*), allocatable :: first_false
    class(*), allocatable :: last_false1, last_false2
    class(*), allocatable :: first_true
    class(*), allocatable :: falses
    class(*), allocatable :: xx
    type(gcroot_t) :: retval
    logical :: done

    type(gcroot_t) :: x_root
    type(gcroot_t) :: lst_root

    ! Protect x and lst against garbage collections instigated by
    ! pred.
    x_root = x
    lst_root = lst

    xx = .autoval. x
    first_true = nil
    first_false = nil

    if (is_not_pair (lst)) then
       ! lst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       lst_d = .autoval. lst
    else
       call drop_equals_trues (pred, xx, .autoval. lst, first_false)
       if (is_not_pair (first_false)) then
          ! There are no falses and there is no shared tail.
          lst_d = nil
       else
          call take_equals_falses_nondestructively (pred, xx, first_false, falses, last_false1, first_true)
          if (is_not_pair (first_true)) then
             ! The entire result is a tail of the input list. Share
             ! it.
             lst_d = falses
          else
             retval = falses
             done = .false.
             do while (.not. done)
                call drop_equals_trues (pred, xx, first_true, first_false)
                if (is_not_pair (first_false)) then
                   ! The tail of the original is a run of
                   ! falses. Leave it out. Filtering is done.
                   done = .true.
                else
                   ! Leave out the run of falses. Get the next run of
                   ! falses.
                   call take_equals_falses_nondestructively (pred, xx, first_false, falses, last_false2, first_true)
                   call set_cdr (last_false1, falses)
                   if (is_not_pair (first_true)) then
                      ! The tail of the original is a run of
                      ! falses. Keep it as a shared tail. Filtering is
                      ! done.
                      done = .true.
                   else
                      last_false1 = last_false2
                   end if
                end if
             end do
             lst_d = .val. retval
          end if
       end if
    end if

    call x_root%discard
    call lst_root%discard
  end function delete

!!!-------------------------------------------------------------------
dnl
m4_define([m4_delete_duplicates],[dnl
  recursive function $1 (pred, lst) result (lst_dd)
    !
    ! This implementation, based on the SRFI-1 reference
    ! implementation, is non-tail recursive and uses up stack
    ! space. However, this is not a major problem: the function is
    ! specified to be O(n**2) and so is unsuitable for long lists,
    ! anyway.
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: lst_dd

    type(gcroot_t) :: lst_root

    lst_root = lst
    lst_dd = recursion (lst_root)

  contains

    recursive function recursion (lst) result (lst_dd)
      type(gcroot_t), intent(in) :: lst
      type(cons_t) :: lst_dd

      class(*), allocatable :: x
      class(*), allocatable :: tail
      type(gcroot_t) :: deletion_result
      type(gcroot_t) :: new_tail

      if (is_nil_list (lst)) then
         lst_dd = lst
      else
         call uncons (lst, x, tail)
         deletion_result = $2 (pred, x, tail)
         new_tail = recursion (deletion_result)
         if (cons_t_eq (.tocons. tail, .tocons. new_tail)) then
            lst_dd = lst
         else
            lst_dd = cons (x, new_tail)
         end if
      end if
    end function recursion

  end function $1
])dnl

m4_delete_duplicates([delete_duplicatesx],[deletex])
m4_delete_duplicates([delete_duplicates],[delete])
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function some[]n (pred, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (bool)
    procedure(list_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    logical :: bool

    logical :: done
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k[] = .autoval. lst[]k
])dnl
    bool = .false.
    done = .false.
    do while (.not. done)
       if (is_nil_list (tail1)) then
          done = .true.
m4_forloop([k],[2],n,[dnl
       else if (is_nil_list (tail[]k)) then
          done = .true.
])dnl
       else
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          if (pred (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]))) then
             bool = .true.
             done = .true.
          end if
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function some[]n

])dnl
dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function some_map[]n[]_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (retval)
    procedure(list_map[]n[]_subr_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: retval

    logical :: done
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl
    retval = .false.
    done = .false.
    do while (.not. done)
       if (is_nil_list (tail1)) then
          done = .true.
m4_forloop([k],[2],n,[dnl
       else if (is_nil_list (tail[]k)) then
          done = .true.
])dnl
       else
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          call proc (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]), retval)
          if (is_not_false (retval)) then
             done = .true.
          else
          end if
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function some_map[]n[]_subr

])dnl
dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function every[]n (pred, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (bool)
    procedure(list_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    logical :: bool

    logical :: done
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl
    bool = .true.
    done = .false.
    do while (.not. done)
       if (is_nil_list (tail1)) then
          done = .true.
m4_forloop([k],[2],n,[dnl
       else if (is_nil_list (tail[]k)) then
          done = .true.
])dnl
       else
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          if (.not. pred (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]))) then
             bool = .false.
             done = .true.
          end if
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function every[]n

])dnl
dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function every_map[]n[]_subr (proc, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (retval)
    procedure(list_map[]n[]_subr_t) :: proc
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: retval

    logical :: done
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl
    retval = .true.
    done = .false.
    do while (.not. done)
       if (is_nil_list (tail1)) then
          done = .true.
m4_forloop([k],[2],n,[dnl
       else if (is_nil_list (tail[]k)) then
          done = .true.
])dnl
       else
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          call proc (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]), retval)
          if (is_false (retval)) then
             retval = .false.   ! Make it .false. of the default kind.
             done = .true.
          end if
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function every_map[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function list_indexn_[]n (pred, [n], lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (index)
    procedure(list_predicate[]n[]_t) :: pred
    integer(sz), intent(in) :: [n]
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    integer(sz) :: index

    integer(sz) :: i
    logical :: done
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl
    index = min (-1_sz, [n] - 1)
    i = [n]
    done = .false.
    do while (.not. done)
       if (is_nil_list (tail1)) then
          done = .true.
m4_forloop([k],[2],n,[dnl
       else if (is_nil_list (tail[]k)) then
          done = .true.
])dnl
       else
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          if (pred (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]))) then
             index = i
             done = .true.
          else
             i = i + 1
          end if
       end if
    end do

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function list_indexn_[]n

])dnl
dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function list_index0_[]n (pred, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (index)
    procedure(list_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    integer(sz) :: index

    index = list_indexn_[]n (pred, 0_sz, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
         ])lst[]k]))
  end function list_index0_[]n

])dnl
dnl
m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function list_index1_[]n (pred, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (index)
    procedure(list_predicate[]n[]_t) :: pred
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    integer(sz) :: index

    index = list_indexn_[]n (pred, 1_sz, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
         ])lst[]k]))
  end function list_index1_[]n

])dnl
dnl
!!!-------------------------------------------------------------------

  recursive function assoc (pred, key, alst) result (retval)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    type(cons_t) :: retval

    type(gcroot_t) :: alst_root
    class(*), allocatable :: p
    logical :: done

    alst_root = alst

    p = .autoval. alst
    done = .false.
    do while (.not. done)
       if (is_nil_list (p)) then
          retval = nil          ! SRFI-1 `assoc' returns #f instead.
          done = .true.
       else if (pred (key, caar (p))) then
          retval = car (p)
          done = .true.
       else
          p = cdr (p)
       end if
    end do

    call alst_root%discard
  end function assoc

  function alist_cons (key, val, alst) result (longer_alst)
    class(*), intent(in) :: key
    class(*), intent(in) :: val
    class(*), intent(in) :: alst
    type(cons_t) :: longer_alst

    longer_alst = cons (cons (key, val), alst)
  end function alist_cons

  function alist_copy (alst) result (alst_c)
    class(*), intent(in) :: alst
    class(*), allocatable :: alst_c

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: key
    class(*), allocatable :: val
    type(cons_t) :: cursor
    type(cons_t) :: new_pair

    tail = .autoval. alst
    if (is_not_pair (tail)) then
       alst_c = tail
    else
       call uncons (tail, head, tail)
       cursor = head ** nil
       alst_c = cursor
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          call uncons (head, key, val)
          new_pair = cons (key, val) ** nil
          call set_cdr (cursor, new_pair)
          cursor = new_pair
       end do
       if (is_not_nil (tail)) then
          call set_cdr (cursor, tail)
       end if
    end if
  end function alist_copy

!!!-------------------------------------------------------------------

  recursive subroutine drop_key_equals_trues (pred, key, alst, first_false)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    class(*), allocatable, intent(out) :: first_false

    class(*), allocatable :: p
    logical :: all_dropped

    p = alst
    all_dropped = .false.
    do while (.not. all_dropped)
       if (is_not_pair (p)) then
          first_false = p
          all_dropped = .true.
       else
          if (pred (key, caar (p))) then
             p = cdr (p)
          else
             first_false = p
             all_dropped = .true.
          end if
       end if
    end do
  end subroutine drop_key_equals_trues

  recursive subroutine take_key_equals_falses_destructively (pred, key, alst, last_false, first_true)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    class(*), allocatable, intent(out) :: last_false
    class(*), allocatable, intent(out) :: first_true

    class(*), allocatable :: p
    logical :: all_taken

    last_false = alst
    p = cdr (alst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_true = p
          all_taken = .true.
       else
          if (pred (key, caar (p))) then
             first_true = p
             all_taken = .true.
          else
             last_false = p
             p = cdr (p)
          end if
       end if
    end do
  end subroutine take_key_equals_falses_destructively

  recursive subroutine take_key_equals_falses_nondestructively (pred, key, alst, falses, last_false, first_true)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    class(*), allocatable, intent(out) :: falses
    class(*), allocatable, intent(out) :: last_false
    class(*), allocatable, intent(out) :: first_true

    class(*), allocatable :: p
    type(cons_t) :: new_pair
    logical :: all_taken

    p = cdr (alst)
    all_taken = .false.
    do while (.not. all_taken)
       if (is_not_pair (p)) then
          first_true = p
          all_taken = .true.
       else
          if (pred (key, caar (p))) then
             first_true = p
             all_taken = .true.
          else
             p = cdr (p)
          end if
       end if
    end do

    if (is_not_pair (first_true)) then
       ! alst ends on a run of falses. Set `falses' to the run of
       ! falses, for use as a shared tail. (There is no need to set
       ! `last_pair'.)
       falses = alst
    else
       ! Copy the run of falses, saving a reference to its last
       ! pair.
       falses = car (alst) ** nil
       last_false = falses
       p = cdr (alst)
       do while (.not. cons_t_eq (p, first_true))
          new_pair = car (p) ** nil
          call set_cdr (last_false, new_pair)
          last_false = new_pair
          p = cdr (p)
       end do
    end if
  end subroutine take_key_equals_falses_nondestructively

  recursive function alist_deletex (pred, key, alst) result (alst_d)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    class(*), allocatable :: alst_d

    class(*), allocatable :: first_false
    class(*), allocatable :: last_false
    class(*), allocatable :: first_true
    class(*), allocatable :: kk
    type(gcroot_t) :: retval
    logical :: done

    type(gcroot_t) :: key_root

    ! Protect key against garbage collections instigated by pred.
    key_root = key

    kk = .autoval. key

    if (is_not_pair (alst)) then
       ! alst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       alst_d = .autoval. alst
    else
       retval = alst          ! Protect alst from garbage collections.
       call drop_key_equals_trues (pred, kk, .autoval. alst, first_false)
       if (is_not_pair (first_false)) then
          ! There are no trues and there is no shared tail.
          alst_d = nil
          call retval%discard
       else
          retval = first_false
          call take_key_equals_falses_destructively (pred, kk, first_false, last_false, first_true)
          if (is_not_pair (first_true)) then
             ! The entire result is a tail of the input list.
             alst_d = .val. retval
          else
             done = .false.
             do while (.not. done)
                call drop_key_equals_trues (pred, kk, first_true, first_false)
                if (is_not_pair (first_false)) then
                   ! The tail of the original is a run of
                   ! falses. Remove it, and then the filtering is
                   ! done.
                   call set_cdr (last_false, nil)
                   done = .true.
                else
                   ! Leave out the run of falses, destructively.
                   call set_cdr (last_false, first_false)
                   ! Get the next run of trues.
                   call take_key_equals_falses_destructively (pred, kk, first_false, last_false, first_true)
                   if (is_not_pair (first_true)) then
                      ! The tail of the original is a run of
                      ! trues. The filtering is done.
                      done = .true.
                   end if
                end if
             end do
             alst_d = .val. retval
          end if
       end if
    end if

    call key_root%discard
  end function alist_deletex

  recursive function alist_delete (pred, key, alst) result (alst_d)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    class(*), allocatable :: alst_d

    class(*), allocatable :: first_false
    class(*), allocatable :: last_false1, last_false2
    class(*), allocatable :: first_true
    class(*), allocatable :: falses
    class(*), allocatable :: kk
    type(gcroot_t) :: retval
    logical :: done

    type(gcroot_t) :: key_root
    type(gcroot_t) :: alst_root

    ! Protect key and alst against garbage collections instigated by
    ! pred.
    key_root = key
    alst_root = alst

    kk = .autoval. key
    first_true = nil
    first_false = nil

    if (is_not_pair (alst)) then
       ! alst is empty, but possibly dotted. Copy the terminating
       ! object (so it is a kind of shared tail).
       alst_d = .autoval. alst
    else
       call drop_key_equals_trues (pred, kk, .autoval. alst, first_false)
       if (is_not_pair (first_false)) then
          ! There are no falses and there is no shared tail.
          alst_d = nil
       else
          call take_key_equals_falses_nondestructively (pred, kk, first_false, falses, last_false1, first_true)
          if (is_not_pair (first_true)) then
             ! The entire result is a tail of the input list. Share
             ! it.
             alst_d = falses
          else
             retval = falses
             done = .false.
             do while (.not. done)
                call drop_key_equals_trues (pred, kk, first_true, first_false)
                if (is_not_pair (first_false)) then
                   ! The tail of the original is a run of
                   ! falses. Leave it out. Filtering is done.
                   done = .true.
                else
                   ! Leave out the run of falses. Get the next run of
                   ! falses.
                   call take_key_equals_falses_nondestructively (pred, kk, first_false, falses, last_false2, first_true)
                   call set_cdr (last_false1, falses)
                   if (is_not_pair (first_true)) then
                      ! The tail of the original is a run of
                      ! falses. Keep it as a shared tail. Filtering is
                      ! done.
                      done = .true.
                   else
                      last_false1 = last_false2
                   end if
                end if
             end do
             alst_d = .val. retval
          end if
       end if
    end if

    call key_root%discard
    call alst_root%discard
  end function alist_delete

!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function fold[]n[]_subr (kons, knil, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (retval)
    procedure(list_kons[]n[]_subr_t) :: kons
    class(*), intent(in) :: knil
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: retval

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl
    type(gcroot_t) :: retval_root

m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
    class(*), allocatable :: new_retval
    logical :: done

    ! Protect against garbage collections performed by kons.
m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

    retval = knil
m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl
    done = .false.
    do while (.not. done)
       if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
          done = .true.
m4_if(k,n,[dnl
       else
],[dnl
       else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
m4_forloop([k],[1],n,[dnl
          call uncons (tail[]k, head[]k, tail[]k)
])dnl
          retval_root = retval
          call kons (head1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
               ])head[]k]), retval, new_retval)
          retval = new_retval
       end if
    end do

    call retval_root%discard
m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function fold[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function fold[]n[]_right_subr (kons, knil, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (retval)
    !
    ! WARNING: This implementation is recursive and uses O(n) stack
    !          space. If you need to do something like this
    !          iteratively, you can use `fold' on the reverse of lst.
    !
    !          A recursive implementation tends to be faster, at least
    !          in functional languages:
    !
    !             * the list need not be reversed,
    !
    !             * on most hardware, the stack puts values near each
    !               other in memory.
    !
    !          In any case, a recursive implementation illustrates
    !          the fundamental meaning of the operation.
    !
    procedure(list_kons[]n[]_subr_t) :: kons
    class(*), intent(in) :: knil
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: retval

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

    retval = &
         recursion (.autoval. lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
         ]).autoval. lst[]k]))

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl

  contains

    recursive function recursion (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (retval)
m4_forloop([k],[1],n,[dnl
      class(*), intent(in) :: lst[]k
])dnl
      class(*), allocatable :: retval

      type(gcroot_t) :: recursion_result

      if (is_not_pair (lst1)) then
         retval = knil
m4_forloop([k],[2],n,[dnl
      else if (is_not_pair (lst[]k)) then
         retval = knil
])dnl
      else
         recursion_result = &
              recursion (cdr (lst1)[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
              ])cdr (lst[]k)]))
         call kons (car (lst1)[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
              ])car (lst[]k)]), .val. recursion_result, retval)
      end if
    end function recursion

  end function fold[]n[]_right_subr
])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function pair_fold[]n[]_subr (kons, knil, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (retval)
    procedure(list_kons[]n[]_subr_t) :: kons
    class(*), intent(in) :: knil
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: retval

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl
    type(gcroot_t) :: retval_root
m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: tail[]k, new_tail[]k
])dnl
    class(*), allocatable :: new_retval
    logical :: done

    ! Protect against garbage collections performed by kons.
m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

    retval = knil
m4_forloop([k],[1],n,[dnl
    tail[]k = lst[]k[]_root
])dnl
    done = .false.
    do while (.not. done)
       if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
          done = .true.
m4_if(k,n,[dnl
       else
],[dnl
       else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
m4_forloop([k],[1],n,[dnl
          new_tail[]k = cdr (tail[]k)
])dnl
          retval_root = retval
          call kons (.val. tail1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
               ]).val. tail[]k]), retval, new_retval)
          retval = new_retval
m4_forloop([k],[1],n,[dnl
          tail[]k = new_tail[]k
])dnl
       end if
    end do

    call retval_root%discard
m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl
  end function pair_fold[]n[]_subr

])dnl
dnl
!!!-------------------------------------------------------------------

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function pair_fold[]n[]_right_subr (kons, knil, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       ])lst[]k])) result (retval)
    !
    ! WARNING: This implementation is recursive and uses O(n) stack
    !          space.
    !
    procedure(list_kons[]n[]_subr_t) :: kons
    class(*), intent(in) :: knil
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    class(*), allocatable :: retval

m4_forloop([k],[1],n,[dnl
    type(gcroot_t) :: lst[]k[]_root
])dnl

m4_forloop([k],[1],n,[dnl
    lst[]k[]_root = lst[]k
])dnl

    retval = &
         recursion (.autoval. lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
         ]).autoval. lst[]k]))

m4_forloop([k],[1],n,[dnl
    call lst[]k[]_root%discard
])dnl

  contains

    recursive function recursion (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
         ])lst[]k])) result (retval)
m4_forloop([k],[1],n,[dnl
      class(*), intent(in) :: lst[]k
])dnl
      class(*), allocatable :: retval

      type(gcroot_t) :: recursion_result

      if (is_not_pair (lst1)) then
         retval = knil
m4_forloop([k],[2],n,[dnl
      else if (is_not_pair (lst[]k)) then
         retval = knil
])dnl
      else
         recursion_result = &
              recursion (cdr (lst1)[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
              ])cdr (lst[]k)]))
         call kons (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 3),[1],[&
              ])lst[]k]), .val. recursion_result, retval)
      end if
    end function recursion

  end function pair_fold[]n[]_right_subr

])dnl
dnl
!!!-------------------------------------------------------------------

  recursive function reduce_subr (kons, right_identity, lst) result (retval)
    procedure(list_kons1_subr_t) :: kons
    class(*), intent(in) :: right_identity
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    class(*), allocatable :: head, tail

    tail = .autoval. lst
    if (is_pair (tail)) then
       call uncons (tail, head, tail)
       retval = fold (kons, head, tail)
    else
       retval = .autoval. right_identity
    end if
  end function reduce_subr

!!!-------------------------------------------------------------------

  recursive function reduce_right_subr (kons, right_identity, lst) result (retval)
    procedure(list_kons1_subr_t) :: kons
    class(*), intent(in) :: right_identity
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    class(*), allocatable :: head, tail

    tail = .autoval. lst
    if (is_pair (tail)) then
       call uncons (tail, head, tail)
       block
         type(gcroot_t) :: lst_root
         lst_root = lst ! Protect against garbage collections performed by kons.
         retval = recursion (head, tail)
         call lst_root%discard
       end block
    else
       retval = .autoval. right_identity
    end if

  contains

    recursive function recursion (head, lst1) result (retval)
      class(*), intent(in) :: head, lst1
      class(*), allocatable :: retval

      class(*), allocatable :: hd, tl
      type(gcroot_t) :: recursion_result

      if (is_pair (lst1)) then
         call uncons (lst1, hd, tl)
         recursion_result = recursion (hd, tl)
         call kons (head, .val. recursion_result, retval)
      else
         retval = head
      end if
    end function recursion

  end function reduce_right_subr

!!!-----------------------------------------------------------------

  recursive function unfold_with_tail_gen (pred, f, g, seed, tail_gen) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    procedure(list_map1_subr_t) :: tail_gen
    class(*), allocatable :: lst

    lst = recursion (.autoval. seed)

  contains

    recursive function recursion (seed) result (retval)
      class(*), intent(in) :: seed
      class(*), allocatable :: retval

      class(*), allocatable :: elem
      class(*), allocatable :: sd
      type(gcroot_t) :: new_element
      type(gcroot_t) :: old_seed
      type(gcroot_t) :: new_seed
      type(gcroot_t) :: recursion_result

      old_seed = seed
      if (pred (.val. old_seed)) then
         call tail_gen (.val. old_seed, retval)
      else
         call f (.val. old_seed, elem)
         new_element = elem
         call g (.val. old_seed, sd)
         new_seed = sd
         recursion_result = recursion (.val. new_seed)
         retval = cons (.val. new_element, recursion_result)
      end if
    end function recursion

  end function unfold_with_tail_gen

  recursive function unfold_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst

    lst = recursion (.autoval. seed)

  contains

    recursive function recursion (seed) result (retval)
      class(*), intent(in) :: seed
      class(*), allocatable :: retval

      class(*), allocatable :: elem
      class(*), allocatable :: sd
      type(gcroot_t) :: new_element
      type(gcroot_t) :: old_seed
      type(gcroot_t) :: new_seed
      type(gcroot_t) :: recursion_result

      old_seed = seed
      if (pred (.val. old_seed)) then
         retval = nil
      else
         call f (.val. old_seed, elem)
         new_element = elem
         call g (.val. old_seed, sd)
         new_seed = sd
         recursion_result = recursion (.val. new_seed)
         retval = cons (.val. new_element, recursion_result)
      end if
    end function recursion

  end function unfold_with_nil_tail

  recursive function unfold_right_with_tail (pred, f, g, seed, tail) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    class(*), intent(in) :: tail
    class(*), allocatable :: lst

    class(*), allocatable :: elem
    class(*), allocatable :: sd
    type(gcroot_t) :: new_element
    type(gcroot_t) :: current_seed
    type(gcroot_t) :: retval

    retval = tail
    current_seed = seed
    do while (.not. pred (.val. current_seed))
       call f (.val. current_seed, elem)
       new_element = elem
       retval = cons (.val. new_element, .val. retval)
       call g (.val. current_seed, sd)
       current_seed = sd
    end do
    lst = .val. retval
  end function unfold_right_with_tail

  recursive function unfold_right_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst

    lst = unfold_right_with_tail (pred, f, g, seed, nil)
  end function unfold_right_with_nil_tail

!!!-----------------------------------------------------------------

end module cons_pairs
