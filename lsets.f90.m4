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
! The lsets module is based in part on the SRFI-1 reference
! implementation in Scheme, which was released as follows:
!
!   "Copyright (c) 1998, 1999 by Olin Shivers. You may do as you
!    please with this code as long as you do not remove this copyright
!    notice or hold me liable for its use."
!
! The lsets module is written entirely in Fortran.
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

module lsets
  !
  ! Lsets (sets implemented as CONS-pair lists) in the fashion of
  ! SRFI-1.
  ! https://srfi.schemers.org/srfi-1/srfi-1.html
  !
  ! The code here does in Fortran what the reference implementation of
  ! SRFI-1 lsets does in Scheme.
  !

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none
  private

  ! A generic function for adding elements to a set.
  public :: lset_adjoin

  ! Generic functions for set operations returning a logical value.
  public :: lset_subset         ! The transitive <= operation on sets.
  public :: lset_equal          ! The transitive = operation on sets.

  ! Generic functions for set operations returning a set.
  public :: lset_union          ! Return the union of sets.
  public :: lset_unionx         ! Union that can alter its inputs.
  public :: lset_intersection   ! Return the intersection of sets.
  public :: lset_intersectionx  ! Intersection that can alter its inputs.
  public :: lset_difference     ! Return the difference of sets.
  public :: lset_differencex    ! Difference that can alter its inputs.
  public :: lset_xor            ! Return the exclusive OR of sets.
  public :: lset_xorx           ! XOR that can alter its inputs.

  ! lset_diff_and_intersection and lset_diff_and_intersectionx return
  ! the equivalent of
  !
  !    list (lset_difference (equal, lst1, lst2, ...),   &
  !          lset_intersection (equal, lst1,             &
  !                             lset_union (equal, lst2, ...)))
  !
  ! But they are more efficient at it.
  !
  public :: lset_diff_and_intersection  ! Not allowed to alter its inputs.
  public :: lset_diff_and_intersectionx ! Allowed to alter its inputs.

  ! Functions that take a list of lists as an argument. They resemble
  ! using Scheme's `apply' procedure; for example,
  !
  !    apply_lset_union (equal, list_of_lists)
  !
  ! is a Fortran equivalent to the Scheme code
  !
  !    (apply lset-union equal list-of-lists))
  !
  public :: apply_lset_subset        ! The transitive <= operation on sets.
  public :: apply_lset_equal         ! The transitive = operation on sets.
  public :: apply_lset_union         ! Return the union of the sets.
  public :: apply_lset_unionx        ! Union that can alter its inputs.
  public :: apply_lset_intersection  ! Return the intersection of the sets.
  public :: apply_lset_intersectionx ! Intersection that can alter its inputs.
  public :: apply_lset_difference    ! Return the difference of the sets.
  public :: apply_lset_differencex   ! Difference that can alter its inputs.
  public :: apply_lset_xor           ! Return the exclusive OR of the sets.
  public :: apply_lset_xorx          ! Exclusive OR that can alter its inputs.
  public :: apply_lset_diff_and_intersection  ! See lset_diff_and_intersection.
  public :: apply_lset_diff_and_intersectionx ! See lset_diff_and_intersectionx.

  ! Implementations of lset_adjoin.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_adjoin[]n
])dnl

  ! Implementations of lset_subset.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_subset[]n
])dnl

  ! Implementations of lset_equal.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_equal[]n
])dnl

  ! Implementations of lset_union.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_union[]n
])dnl

  ! Implementations of lset_unionx.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_unionx[]n
])dnl

  ! Implementations of lset_intersection.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: lset_intersection[]n
])dnl

  ! Implementations of lset_intersectionx.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: lset_intersectionx[]n
])dnl

  ! Implementations of lset_difference.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: lset_difference[]n
])dnl

  ! Implementations of lset_differencex.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: lset_differencex[]n
])dnl

  ! Implementations of lset_xor.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_xor[]n
])dnl

  ! Implementations of lset_xorx.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_xorx[]n
])dnl

  ! Implementations of lset_diff_and_intersection.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: lset_diff_and_intersection[]n
])dnl

  ! Implementations of lset_diff_and_intersectionx.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: lset_diff_and_intersectionx[]n
])dnl

  interface lset_adjoin
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_adjoin[]n
])dnl
  end interface lset_adjoin

  interface lset_subset
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_subset[]n
])dnl
  end interface lset_subset

  interface lset_equal
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_equal[]n
])dnl
  end interface lset_equal

  interface lset_union
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_union[]n
])dnl
  end interface lset_union

  interface lset_unionx
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_unionx[]n
])dnl
  end interface lset_unionx

  interface lset_intersection
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure lset_intersection[]n
])dnl
  end interface lset_intersection

  interface lset_intersectionx
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure lset_intersectionx[]n
])dnl
  end interface lset_intersectionx

  interface lset_difference
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure lset_difference[]n
])dnl
  end interface lset_difference

  interface lset_differencex
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure lset_differencex[]n
])dnl
  end interface lset_differencex

  interface lset_xor
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_xor[]n
])dnl
  end interface lset_xor

  interface lset_xorx
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_xorx[]n
])dnl
  end interface lset_xorx

  interface lset_diff_and_intersection
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure lset_diff_and_intersection[]n
])dnl
  end interface lset_diff_and_intersection

  interface lset_diff_and_intersectionx
m4_forloop([n],[1],LISTN_MAX,[dnl
     module procedure lset_diff_and_intersectionx[]n
])dnl
  end interface lset_diff_and_intersectionx

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_adjoin0 (equal, lst) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    type(cons_t) :: lst_out

    lst_out = lst
  end function lset_adjoin0

m4_forloop([n],[1],LISTN_MAX,[dnl
  recursive function lset_adjoin[]n (equal, lst, element1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                            ])element[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: element[]k
])dnl
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &                           ])element[]k])))

  contains

    recursive subroutine kons (element, lst, lst_out)
      class(*), intent(in) :: element
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      if (is_nil (member (equal, element, lst))) then
         lst_out = cons (element, lst)
      else
         lst_out = lst
      end if
    end subroutine kons

  end function lset_adjoin[]n

])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_union0 (equal) result (lst_out)
    procedure(list_predicate2_t) :: equal
    type(cons_t) :: lst_out

    lst_out = nil
  end function lset_union0

m4_forloop([n],[1],LISTN_MAX,[dnl
  recursive function lset_union[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                          ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &       ])lst[]k]))
    lst_out = apply_lset_union (equal, lists)
  end function lset_union[]n

])dnl
dnl
  recursive function apply_lset_union (equal, lists) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    type(cons_t) :: lst_out

    lst_out = reduce (make_union, nil, lists)

  contains

    recursive subroutine make_union (lst1, lst2, lst_out)
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      class(*), allocatable, intent(out) :: lst_out

      if (is_nil (lst1)) then
         lst_out = lst2
      else if (is_nil (lst2)) then
         lst_out = lst1
      else if (cons_t_eq (lst1, lst2)) then
         lst_out = lst2
      else
         lst_out = fold (kons, lst2, lst1)
      end if
    end subroutine make_union

    recursive subroutine kons (element, lst, lst_out)
      class(*), intent(in) :: element
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      if (is_nil (member (equal, element, lst))) then
         lst_out = cons (element, lst)
      else
         lst_out = lst
      end if
    end subroutine kons

  end function apply_lset_union

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_unionx0 (equal) result (lst_out)
    procedure(list_predicate2_t) :: equal
    type(cons_t) :: lst_out

    lst_out = nil
  end function lset_unionx0

m4_forloop([n],[1],LISTN_MAX,[dnl
  recursive function lset_unionx[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                            ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx[]n

])dnl
dnl
  recursive function apply_lset_unionx (equal, lists) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    type(cons_t) :: lst_out

    lst_out = reduce (make_unionx, nil, lists)

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      class(*), allocatable, intent(out) :: lst_out

      if (is_nil (lst1)) then
         ! The union of a null set and a set is the latter.
         lst_out = lst2
      else if (is_nil (lst2)) then
         ! The union of a set and a null set is the former.
         lst_out = lst1
      else if (cons_t_eq (lst1, lst2)) then
         ! The union of a set with itself is itself.
         lst_out = lst2
      else
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      if (is_nil (member (equal, car (pair), lst))) then
         call set_cdr (pair, lst)
         lst_out = pair
      else
         lst_out = lst
      end if
    end subroutine kons

  end function apply_lset_unionx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_intersection1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_intersection1

  recursive function lset_intersectionx1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_intersectionx1

m4_forloop([n],[2],LISTN_MAX,[dnl
  recursive function lset_intersection[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                 ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection[]n

  recursive function lset_intersectionx[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                 ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx[]n

])dnl
dnl
m4_define([m4_apply_lset_intersection],[dnl
  recursive function $1 (equal, lists) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    type(cons_t) :: lst_out

    class(*), allocatable :: x ! x is used by the nested procedures.
    type(cons_t) :: lst1
    type(cons_t) :: the_rest

    lst1 = car (lists)
    the_rest = cdr (lists)

    ! Remove any references to lst1.
    the_rest = delete (cons_t_eq, lst1, the_rest)

    if (some (is_nil_list, the_rest)) then
       ! The intersection of a set with a null set is a null set.
       lst_out = nil
    else if (is_nil_list (the_rest)) then
       lst_out = lst1
    else
       lst_out = $2 (is_in_every_list, lst1)
    end if

  contains

    recursive function is_in_every_list (x_value) result (bool)
      class(*), intent(in) :: x_value
      logical :: bool

      x = x_value
      bool = every (x_is_in, the_rest)
    end function is_in_every_list

    recursive function x_is_in (lst) result (bool)
      class(*), intent(in) :: lst
      logical :: bool

      bool = is_not_nil (member (equal, x, lst))
    end function x_is_in

  end function $1
])dnl
dnl
m4_apply_lset_intersection([apply_lset_intersection],[filter])
m4_apply_lset_intersection([apply_lset_intersectionx],[filterx])
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_difference1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_difference1

  recursive function lset_differencex1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_differencex1

m4_forloop([n],[2],LISTN_MAX,[dnl
  recursive function lset_difference[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                               ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference[]n

  recursive function lset_differencex[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                               ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex[]n

])dnl
dnl
m4_define([m4_apply_lset_difference],[dnl
  recursive function $1 (equal, lists) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    type(cons_t) :: lst_out

    type(cons_t) :: lst1
    type(cons_t) :: the_rest
    class(*), allocatable :: x ! x is used by the nested procedures.

    lst1 = car (lists)
    the_rest = cdr (lists)
    the_rest = remove (is_not_pair, the_rest) ! Ignore null sets.
    if (is_nil_list (the_rest)) then
       lst_out = lst1
    else if (is_not_nil (member (cons_t_eq, lst1, the_rest))) then
       ! The difference of a set and itself is a null set.
       lst_out = nil
    else
       lst_out = $2 (is_missing_from_every_list, lst1)
    end if

  contains

    recursive function is_missing_from_every_list (x_value) result (bool)
      class(*), intent(in) :: x_value
      logical :: bool

      x = x_value
      bool = every (x_is_not_in, the_rest)
    end function is_missing_from_every_list

    recursive function x_is_not_in (lst) result (bool)
      class(*), intent(in) :: lst
      logical :: bool

      bool = is_nil (member (equal, x, lst))
    end function x_is_not_in

  end function $1

])dnl
dnl
m4_apply_lset_difference([apply_lset_difference],[filter])
m4_apply_lset_difference([apply_lset_differencex],[filterx])
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_diff_and_intersection1 (equal, lst1) result (diff_and_xsect)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: diff_and_xsect

    diff_and_xsect = list (lst1, nil)
  end function lset_diff_and_intersection1

  recursive function lset_diff_and_intersectionx1 (equal, lst1) result (diff_and_xsect)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: diff_and_xsect

    diff_and_xsect = list (lst1, nil)
  end function lset_diff_and_intersectionx1

m4_forloop([n],[2],LISTN_MAX,[dnl
  recursive function lset_diff_and_intersection[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                          ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection[]n

  recursive function lset_diff_and_intersectionx[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                           ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx[]n

])dnl
dnl
m4_define([m4_apply_lset_diff_and_intersection],[dnl
  recursive function $1 (equal, lists) result (diff_and_xsect)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    type(cons_t) :: diff_and_xsect

    type(cons_t) :: lst1
    type(cons_t) :: the_rest
    class(*), allocatable :: x ! x is used by the nested procedures.

    lst1 = car (lists)
    the_rest = cdr (lists)
    if (every (is_nil_list, lists)) then
       diff_and_xsect = list (lst1, nil)
    else if (is_not_nil (member (cons_t_eq, lst1, the_rest))) then
       ! Difference and intersection of a set with a set containing
       ! it.
       diff_and_xsect = list (nil, lst1)
    else
       diff_and_xsect = $2 (is_not_in_any_list, lst1)
    end if

  contains

    recursive function is_not_in_any_list (x_value) result (bool)
      class(*), intent(in) :: x_value
      logical :: bool

      x = x_value
      bool = .not. some (x_is_in, the_rest)
    end function is_not_in_any_list

    recursive function x_is_in (lst) result (bool)
      class(*), intent(in) :: lst
      logical :: bool

      bool = is_not_nil (member (equal, x, lst))
    end function x_is_in

  end function $1

])dnl
dnl
m4_apply_lset_diff_and_intersection([apply_lset_diff_and_intersection],[partition])
m4_apply_lset_diff_and_intersection([apply_lset_diff_and_intersectionx],[partitionx])
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_xor0 (equal) result (lst_out)
    procedure(list_predicate2_t) :: equal
    type(cons_t) :: lst_out

    lst_out = nil
  end function lset_xor0

m4_forloop([n],[1],LISTN_MAX,[dnl
  recursive function lset_xor[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                         ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor[]n

])dnl

  recursive function apply_lset_xor (equal, lists) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    type(cons_t) :: lst_out

    type(cons_t) :: a_xsect_b ! a_xsect_b is used by the nested
                              ! procedures.

    lst_out = reduce (xor, nil, lists)

  contains

    recursive subroutine xor (a, b, a_xor_b)
      class(*), intent(in) :: a
      class(*), intent(in) :: b
      class(*), allocatable, intent(out) :: a_xor_b

      type(cons_t) :: diff_and_xsect
      type(cons_t) :: a_minus_b

      diff_and_xsect = lset_diff_and_intersection (equal, a, b)
      a_minus_b = first (diff_and_xsect)
      if (is_nil (a_minus_b)) then
         a_xor_b = lset_difference (equal, b, a)
      else
         a_xsect_b = second (diff_and_xsect)
         if (is_nil (a_xsect_b)) then
            a_xor_b = append (b, a)
         else
            a_xor_b = fold (kons, a_minus_b, b)
         end if
      end if
    end subroutine xor

    recursive subroutine kons (x, lst, lst_out)
      class(*), intent(in) :: x
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      if (is_not_nil (member (equal, x, a_xsect_b))) then
         lst_out = lst
      else
         lst_out = cons (x, lst)
      end if
    end subroutine kons

  end function apply_lset_xor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_xorx0 (equal) result (lst_out)
    procedure(list_predicate2_t) :: equal
    type(cons_t) :: lst_out

    lst_out = nil
  end function lset_xorx0

m4_forloop([n],[1],LISTN_MAX,[dnl
  recursive function lset_xorx[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                          ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx[]n

])dnl

  recursive function apply_lset_xorx (equal, lists) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    type(cons_t) :: lst_out

    type(cons_t) :: a_xsect_b ! a_xsect_b is used by the nested
                              ! procedures.

    lst_out = reduce (xorx, nil, lists)

  contains

    recursive subroutine xorx (a, b, a_xorx_b)
      class(*), intent(in) :: a
      class(*), intent(in) :: b
      class(*), allocatable, intent(out) :: a_xorx_b

      type(cons_t) :: diff_and_xsect
      type(cons_t) :: a_minus_b

      diff_and_xsect = lset_diff_and_intersectionx (equal, a, b)
      a_minus_b = first (diff_and_xsect)
      if (is_nil (a_minus_b)) then
         a_xorx_b = lset_differencex (equal, b, a)
      else
         a_xsect_b = second (diff_and_xsect)
         if (is_nil (a_xsect_b)) then
            a_xorx_b = appendx (b, a)
         else
            a_xorx_b = pair_fold (kons, a_minus_b, b)
         end if
      end if
    end subroutine xorx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      if (is_not_nil (member (equal, car (pair), a_xsect_b))) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

  end function apply_lset_xorx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_subset0 (equal) result (subset)
    procedure(list_predicate2_t) :: equal
    logical :: subset

    subset = .true.
  end function lset_subset0

  recursive function lset_subset1 (equal, lst1) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    logical :: subset

    subset = .true.
  end function lset_subset1

  recursive function lset_subset2 (equal, lst1, lst2) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    logical :: subset

    if (cons_t_eq (lst2, lst1)) then
       subset = .true.
    else if (lset_subset__ (equal, lst1, lst2)) then
       subset = .true.
    else
       subset = .false.
    end if
  end function lset_subset2

m4_forloop([n],[3],LISTN_MAX,[dnl
  recursive function lset_subset[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                           ])lst[]k])) result (subset)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    subset = apply_lset_subset (equal, lists)
  end function lset_subset[]n

])dnl
dnl
  recursive function apply_lset_subset (equal, lists) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    logical :: subset

    type(cons_t) :: lst1
    type(cons_t) :: lst2
    type(cons_t) :: the_rest
    logical :: done

    if (is_nil_list (lists)) then
       subset = .true.
    else
       lst1 = car (lists)
       the_rest = cdr (lists)
       done = .false.
       do while (.not. done)
          if (is_nil_list (the_rest)) then
             subset = .true.
             done = .true.
          else
             lst2 = car (the_rest)
             the_rest = cdr (the_rest)
             if (cons_t_eq (lst2, lst1)) then
                lst1 = lst2
             else if (lset_subset__ (equal, lst1, lst2)) then
                lst1 = lst2
             else
                subset = .false.
                done = .true.
             end if
          end if
       end do
    end if
  end function apply_lset_subset

  recursive function lset_equal0 (equal) result (eq)
    procedure(list_predicate2_t) :: equal
    logical :: eq

    eq = .true.
  end function lset_equal0

  recursive function lset_equal1 (equal, lst1) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    logical :: eq

    eq = .true.
  end function lset_equal1

  recursive function lset_equal2 (equal, lst1, lst2) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    logical :: eq

    if (cons_t_eq (lst1, lst2)) then
       eq = .true.
    else if (.not. lset_subset__ (equal, lst1, lst2)) then
       eq = .false.
    else if (.not. lset_subset__ (flip_equal, lst2, lst1)) then
       eq = .false.
    else
       eq = .true.
    end if

  contains

    recursive function flip_equal (x, y) result (bool)
      !
      ! `equal' with its arguments reversed.
      !
      ! This is needed so `equal' is always run with the element from
      ! lst1 first and the element from lst2 second, and (if there are
      ! more lists) so on like that.
      !
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      bool = equal (y, x)
    end function flip_equal

  end function lset_equal2

m4_forloop([n],[3],LISTN_MAX,[dnl
  recursive function lset_equal[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                          ])lst[]k])) result (eq)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    eq = apply_lset_equal (equal, lists)
  end function lset_equal[]n

])dnl
dnl
  recursive function apply_lset_equal (equal, lists) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lists
    logical :: eq

    type(cons_t) :: lst1
    type(cons_t) :: lst2
    type(cons_t) :: the_rest
    logical :: done

    if (is_nil_list (lists)) then
       eq = .true.
    else
       lst1 = car (lists)
       the_rest = cdr (lists)
       done = .false.
       do while (.not. done)
          if (is_nil_list (the_rest)) then
             eq = .true.
             done = .true.
          else
             lst2 = car (the_rest)
             the_rest = cdr (the_rest)
             if (cons_t_eq (lst1, lst2)) then
                lst1 = lst2
             else if (.not. lset_subset__ (equal, lst1, lst2)) then
                eq = .false.
                done = .true.
             else if (.not. lset_subset__ (flip_equal, lst2, lst1)) then
                eq = .false.
                done = .true.
             else
                lst1 = lst2
             end if
          end if
       end do
    end if

  contains

    recursive function flip_equal (x, y) result (bool)
      !
      ! `equal' with its arguments reversed.
      !
      ! This is needed so `equal' is always run with the element from
      ! lst1 first and the element from lst2 second, and (if there are
      ! more lists) so on like that.
      !
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      bool = equal (y, x)
    end function flip_equal

  end function apply_lset_equal

  recursive function lset_subset__ (equal, lst1, lst2) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    logical :: subset

    type(cons_t) :: p

    subset = .true.
    p = lst1
    do while (subset .and. is_pair (p))
       subset = is_not_nil (member (equal, car (p), lst2))
       p = cdr (p)
    end do
  end function lset_subset__

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module lsets
