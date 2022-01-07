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
! The lsets module is based in part on the SRFI-1 reference
! implementation in Scheme, which was released as follows:
!
!   "Copyright (c) 1998, 1999 by Olin Shivers. You may do as you
!    please with this code as long as you do not remove this copyright
!    notice or hold me liable for its use."
!
! The lsets module is written entirely in Fortran.
!

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
  public :: lset_adjoin0
  public :: lset_adjoin1
  public :: lset_adjoin2
  public :: lset_adjoin3
  public :: lset_adjoin4
  public :: lset_adjoin5
  public :: lset_adjoin6
  public :: lset_adjoin7
  public :: lset_adjoin8
  public :: lset_adjoin9
  public :: lset_adjoin10
  public :: lset_adjoin11
  public :: lset_adjoin12
  public :: lset_adjoin13
  public :: lset_adjoin14
  public :: lset_adjoin15
  public :: lset_adjoin16
  public :: lset_adjoin17
  public :: lset_adjoin18
  public :: lset_adjoin19
  public :: lset_adjoin20

  ! Implementations of lset_subset.
  public :: lset_subset0
  public :: lset_subset1
  public :: lset_subset2
  public :: lset_subset3
  public :: lset_subset4
  public :: lset_subset5
  public :: lset_subset6
  public :: lset_subset7
  public :: lset_subset8
  public :: lset_subset9
  public :: lset_subset10
  public :: lset_subset11
  public :: lset_subset12
  public :: lset_subset13
  public :: lset_subset14
  public :: lset_subset15
  public :: lset_subset16
  public :: lset_subset17
  public :: lset_subset18
  public :: lset_subset19
  public :: lset_subset20

  ! Implementations of lset_equal.
  public :: lset_equal0
  public :: lset_equal1
  public :: lset_equal2
  public :: lset_equal3
  public :: lset_equal4
  public :: lset_equal5
  public :: lset_equal6
  public :: lset_equal7
  public :: lset_equal8
  public :: lset_equal9
  public :: lset_equal10
  public :: lset_equal11
  public :: lset_equal12
  public :: lset_equal13
  public :: lset_equal14
  public :: lset_equal15
  public :: lset_equal16
  public :: lset_equal17
  public :: lset_equal18
  public :: lset_equal19
  public :: lset_equal20

  ! Implementations of lset_union.
  public :: lset_union0
  public :: lset_union1
  public :: lset_union2
  public :: lset_union3
  public :: lset_union4
  public :: lset_union5
  public :: lset_union6
  public :: lset_union7
  public :: lset_union8
  public :: lset_union9
  public :: lset_union10
  public :: lset_union11
  public :: lset_union12
  public :: lset_union13
  public :: lset_union14
  public :: lset_union15
  public :: lset_union16
  public :: lset_union17
  public :: lset_union18
  public :: lset_union19
  public :: lset_union20

  ! Implementations of lset_unionx.
  public :: lset_unionx0
  public :: lset_unionx1
  public :: lset_unionx2
  public :: lset_unionx3
  public :: lset_unionx4
  public :: lset_unionx5
  public :: lset_unionx6
  public :: lset_unionx7
  public :: lset_unionx8
  public :: lset_unionx9
  public :: lset_unionx10
  public :: lset_unionx11
  public :: lset_unionx12
  public :: lset_unionx13
  public :: lset_unionx14
  public :: lset_unionx15
  public :: lset_unionx16
  public :: lset_unionx17
  public :: lset_unionx18
  public :: lset_unionx19
  public :: lset_unionx20

  ! Implementations of lset_intersection.
  public :: lset_intersection1
  public :: lset_intersection2
  public :: lset_intersection3
  public :: lset_intersection4
  public :: lset_intersection5
  public :: lset_intersection6
  public :: lset_intersection7
  public :: lset_intersection8
  public :: lset_intersection9
  public :: lset_intersection10
  public :: lset_intersection11
  public :: lset_intersection12
  public :: lset_intersection13
  public :: lset_intersection14
  public :: lset_intersection15
  public :: lset_intersection16
  public :: lset_intersection17
  public :: lset_intersection18
  public :: lset_intersection19
  public :: lset_intersection20

  ! Implementations of lset_intersectionx.
  public :: lset_intersectionx1
  public :: lset_intersectionx2
  public :: lset_intersectionx3
  public :: lset_intersectionx4
  public :: lset_intersectionx5
  public :: lset_intersectionx6
  public :: lset_intersectionx7
  public :: lset_intersectionx8
  public :: lset_intersectionx9
  public :: lset_intersectionx10
  public :: lset_intersectionx11
  public :: lset_intersectionx12
  public :: lset_intersectionx13
  public :: lset_intersectionx14
  public :: lset_intersectionx15
  public :: lset_intersectionx16
  public :: lset_intersectionx17
  public :: lset_intersectionx18
  public :: lset_intersectionx19
  public :: lset_intersectionx20

  ! Implementations of lset_difference.
  public :: lset_difference1
  public :: lset_difference2
  public :: lset_difference3
  public :: lset_difference4
  public :: lset_difference5
  public :: lset_difference6
  public :: lset_difference7
  public :: lset_difference8
  public :: lset_difference9
  public :: lset_difference10
  public :: lset_difference11
  public :: lset_difference12
  public :: lset_difference13
  public :: lset_difference14
  public :: lset_difference15
  public :: lset_difference16
  public :: lset_difference17
  public :: lset_difference18
  public :: lset_difference19
  public :: lset_difference20

  ! Implementations of lset_differencex.
  public :: lset_differencex1
  public :: lset_differencex2
  public :: lset_differencex3
  public :: lset_differencex4
  public :: lset_differencex5
  public :: lset_differencex6
  public :: lset_differencex7
  public :: lset_differencex8
  public :: lset_differencex9
  public :: lset_differencex10
  public :: lset_differencex11
  public :: lset_differencex12
  public :: lset_differencex13
  public :: lset_differencex14
  public :: lset_differencex15
  public :: lset_differencex16
  public :: lset_differencex17
  public :: lset_differencex18
  public :: lset_differencex19
  public :: lset_differencex20

  ! Implementations of lset_xor.
  public :: lset_xor0
  public :: lset_xor1
  public :: lset_xor2
  public :: lset_xor3
  public :: lset_xor4
  public :: lset_xor5
  public :: lset_xor6
  public :: lset_xor7
  public :: lset_xor8
  public :: lset_xor9
  public :: lset_xor10
  public :: lset_xor11
  public :: lset_xor12
  public :: lset_xor13
  public :: lset_xor14
  public :: lset_xor15
  public :: lset_xor16
  public :: lset_xor17
  public :: lset_xor18
  public :: lset_xor19
  public :: lset_xor20

  ! Implementations of lset_xorx.
  public :: lset_xorx0
  public :: lset_xorx1
  public :: lset_xorx2
  public :: lset_xorx3
  public :: lset_xorx4
  public :: lset_xorx5
  public :: lset_xorx6
  public :: lset_xorx7
  public :: lset_xorx8
  public :: lset_xorx9
  public :: lset_xorx10
  public :: lset_xorx11
  public :: lset_xorx12
  public :: lset_xorx13
  public :: lset_xorx14
  public :: lset_xorx15
  public :: lset_xorx16
  public :: lset_xorx17
  public :: lset_xorx18
  public :: lset_xorx19
  public :: lset_xorx20

  ! Implementations of lset_diff_and_intersection.
  public :: lset_diff_and_intersection1
  public :: lset_diff_and_intersection2
  public :: lset_diff_and_intersection3
  public :: lset_diff_and_intersection4
  public :: lset_diff_and_intersection5
  public :: lset_diff_and_intersection6
  public :: lset_diff_and_intersection7
  public :: lset_diff_and_intersection8
  public :: lset_diff_and_intersection9
  public :: lset_diff_and_intersection10
  public :: lset_diff_and_intersection11
  public :: lset_diff_and_intersection12
  public :: lset_diff_and_intersection13
  public :: lset_diff_and_intersection14
  public :: lset_diff_and_intersection15
  public :: lset_diff_and_intersection16
  public :: lset_diff_and_intersection17
  public :: lset_diff_and_intersection18
  public :: lset_diff_and_intersection19
  public :: lset_diff_and_intersection20

  ! Implementations of lset_diff_and_intersectionx.
  public :: lset_diff_and_intersectionx1
  public :: lset_diff_and_intersectionx2
  public :: lset_diff_and_intersectionx3
  public :: lset_diff_and_intersectionx4
  public :: lset_diff_and_intersectionx5
  public :: lset_diff_and_intersectionx6
  public :: lset_diff_and_intersectionx7
  public :: lset_diff_and_intersectionx8
  public :: lset_diff_and_intersectionx9
  public :: lset_diff_and_intersectionx10
  public :: lset_diff_and_intersectionx11
  public :: lset_diff_and_intersectionx12
  public :: lset_diff_and_intersectionx13
  public :: lset_diff_and_intersectionx14
  public :: lset_diff_and_intersectionx15
  public :: lset_diff_and_intersectionx16
  public :: lset_diff_and_intersectionx17
  public :: lset_diff_and_intersectionx18
  public :: lset_diff_and_intersectionx19
  public :: lset_diff_and_intersectionx20

  interface lset_adjoin
     module procedure lset_adjoin0
     module procedure lset_adjoin1
     module procedure lset_adjoin2
     module procedure lset_adjoin3
     module procedure lset_adjoin4
     module procedure lset_adjoin5
     module procedure lset_adjoin6
     module procedure lset_adjoin7
     module procedure lset_adjoin8
     module procedure lset_adjoin9
     module procedure lset_adjoin10
     module procedure lset_adjoin11
     module procedure lset_adjoin12
     module procedure lset_adjoin13
     module procedure lset_adjoin14
     module procedure lset_adjoin15
     module procedure lset_adjoin16
     module procedure lset_adjoin17
     module procedure lset_adjoin18
     module procedure lset_adjoin19
     module procedure lset_adjoin20
  end interface lset_adjoin

  interface lset_subset
     module procedure lset_subset0
     module procedure lset_subset1
     module procedure lset_subset2
     module procedure lset_subset3
     module procedure lset_subset4
     module procedure lset_subset5
     module procedure lset_subset6
     module procedure lset_subset7
     module procedure lset_subset8
     module procedure lset_subset9
     module procedure lset_subset10
     module procedure lset_subset11
     module procedure lset_subset12
     module procedure lset_subset13
     module procedure lset_subset14
     module procedure lset_subset15
     module procedure lset_subset16
     module procedure lset_subset17
     module procedure lset_subset18
     module procedure lset_subset19
     module procedure lset_subset20
  end interface lset_subset

  interface lset_equal
     module procedure lset_equal0
     module procedure lset_equal1
     module procedure lset_equal2
     module procedure lset_equal3
     module procedure lset_equal4
     module procedure lset_equal5
     module procedure lset_equal6
     module procedure lset_equal7
     module procedure lset_equal8
     module procedure lset_equal9
     module procedure lset_equal10
     module procedure lset_equal11
     module procedure lset_equal12
     module procedure lset_equal13
     module procedure lset_equal14
     module procedure lset_equal15
     module procedure lset_equal16
     module procedure lset_equal17
     module procedure lset_equal18
     module procedure lset_equal19
     module procedure lset_equal20
  end interface lset_equal

  interface lset_union
     module procedure lset_union0
     module procedure lset_union1
     module procedure lset_union2
     module procedure lset_union3
     module procedure lset_union4
     module procedure lset_union5
     module procedure lset_union6
     module procedure lset_union7
     module procedure lset_union8
     module procedure lset_union9
     module procedure lset_union10
     module procedure lset_union11
     module procedure lset_union12
     module procedure lset_union13
     module procedure lset_union14
     module procedure lset_union15
     module procedure lset_union16
     module procedure lset_union17
     module procedure lset_union18
     module procedure lset_union19
     module procedure lset_union20
  end interface lset_union

  interface lset_unionx
     module procedure lset_unionx0
     module procedure lset_unionx1
     module procedure lset_unionx2
     module procedure lset_unionx3
     module procedure lset_unionx4
     module procedure lset_unionx5
     module procedure lset_unionx6
     module procedure lset_unionx7
     module procedure lset_unionx8
     module procedure lset_unionx9
     module procedure lset_unionx10
     module procedure lset_unionx11
     module procedure lset_unionx12
     module procedure lset_unionx13
     module procedure lset_unionx14
     module procedure lset_unionx15
     module procedure lset_unionx16
     module procedure lset_unionx17
     module procedure lset_unionx18
     module procedure lset_unionx19
     module procedure lset_unionx20
  end interface lset_unionx

  interface lset_intersection
     module procedure lset_intersection1
     module procedure lset_intersection2
     module procedure lset_intersection3
     module procedure lset_intersection4
     module procedure lset_intersection5
     module procedure lset_intersection6
     module procedure lset_intersection7
     module procedure lset_intersection8
     module procedure lset_intersection9
     module procedure lset_intersection10
     module procedure lset_intersection11
     module procedure lset_intersection12
     module procedure lset_intersection13
     module procedure lset_intersection14
     module procedure lset_intersection15
     module procedure lset_intersection16
     module procedure lset_intersection17
     module procedure lset_intersection18
     module procedure lset_intersection19
     module procedure lset_intersection20
  end interface lset_intersection

  interface lset_intersectionx
     module procedure lset_intersectionx1
     module procedure lset_intersectionx2
     module procedure lset_intersectionx3
     module procedure lset_intersectionx4
     module procedure lset_intersectionx5
     module procedure lset_intersectionx6
     module procedure lset_intersectionx7
     module procedure lset_intersectionx8
     module procedure lset_intersectionx9
     module procedure lset_intersectionx10
     module procedure lset_intersectionx11
     module procedure lset_intersectionx12
     module procedure lset_intersectionx13
     module procedure lset_intersectionx14
     module procedure lset_intersectionx15
     module procedure lset_intersectionx16
     module procedure lset_intersectionx17
     module procedure lset_intersectionx18
     module procedure lset_intersectionx19
     module procedure lset_intersectionx20
  end interface lset_intersectionx

  interface lset_difference
     module procedure lset_difference1
     module procedure lset_difference2
     module procedure lset_difference3
     module procedure lset_difference4
     module procedure lset_difference5
     module procedure lset_difference6
     module procedure lset_difference7
     module procedure lset_difference8
     module procedure lset_difference9
     module procedure lset_difference10
     module procedure lset_difference11
     module procedure lset_difference12
     module procedure lset_difference13
     module procedure lset_difference14
     module procedure lset_difference15
     module procedure lset_difference16
     module procedure lset_difference17
     module procedure lset_difference18
     module procedure lset_difference19
     module procedure lset_difference20
  end interface lset_difference

  interface lset_differencex
     module procedure lset_differencex1
     module procedure lset_differencex2
     module procedure lset_differencex3
     module procedure lset_differencex4
     module procedure lset_differencex5
     module procedure lset_differencex6
     module procedure lset_differencex7
     module procedure lset_differencex8
     module procedure lset_differencex9
     module procedure lset_differencex10
     module procedure lset_differencex11
     module procedure lset_differencex12
     module procedure lset_differencex13
     module procedure lset_differencex14
     module procedure lset_differencex15
     module procedure lset_differencex16
     module procedure lset_differencex17
     module procedure lset_differencex18
     module procedure lset_differencex19
     module procedure lset_differencex20
  end interface lset_differencex

  interface lset_xor
     module procedure lset_xor0
     module procedure lset_xor1
     module procedure lset_xor2
     module procedure lset_xor3
     module procedure lset_xor4
     module procedure lset_xor5
     module procedure lset_xor6
     module procedure lset_xor7
     module procedure lset_xor8
     module procedure lset_xor9
     module procedure lset_xor10
     module procedure lset_xor11
     module procedure lset_xor12
     module procedure lset_xor13
     module procedure lset_xor14
     module procedure lset_xor15
     module procedure lset_xor16
     module procedure lset_xor17
     module procedure lset_xor18
     module procedure lset_xor19
     module procedure lset_xor20
  end interface lset_xor

  interface lset_xorx
     module procedure lset_xorx0
     module procedure lset_xorx1
     module procedure lset_xorx2
     module procedure lset_xorx3
     module procedure lset_xorx4
     module procedure lset_xorx5
     module procedure lset_xorx6
     module procedure lset_xorx7
     module procedure lset_xorx8
     module procedure lset_xorx9
     module procedure lset_xorx10
     module procedure lset_xorx11
     module procedure lset_xorx12
     module procedure lset_xorx13
     module procedure lset_xorx14
     module procedure lset_xorx15
     module procedure lset_xorx16
     module procedure lset_xorx17
     module procedure lset_xorx18
     module procedure lset_xorx19
     module procedure lset_xorx20
  end interface lset_xorx

  interface lset_diff_and_intersection
     module procedure lset_diff_and_intersection1
     module procedure lset_diff_and_intersection2
     module procedure lset_diff_and_intersection3
     module procedure lset_diff_and_intersection4
     module procedure lset_diff_and_intersection5
     module procedure lset_diff_and_intersection6
     module procedure lset_diff_and_intersection7
     module procedure lset_diff_and_intersection8
     module procedure lset_diff_and_intersection9
     module procedure lset_diff_and_intersection10
     module procedure lset_diff_and_intersection11
     module procedure lset_diff_and_intersection12
     module procedure lset_diff_and_intersection13
     module procedure lset_diff_and_intersection14
     module procedure lset_diff_and_intersection15
     module procedure lset_diff_and_intersection16
     module procedure lset_diff_and_intersection17
     module procedure lset_diff_and_intersection18
     module procedure lset_diff_and_intersection19
     module procedure lset_diff_and_intersection20
  end interface lset_diff_and_intersection

  interface lset_diff_and_intersectionx
     module procedure lset_diff_and_intersectionx1
     module procedure lset_diff_and_intersectionx2
     module procedure lset_diff_and_intersectionx3
     module procedure lset_diff_and_intersectionx4
     module procedure lset_diff_and_intersectionx5
     module procedure lset_diff_and_intersectionx6
     module procedure lset_diff_and_intersectionx7
     module procedure lset_diff_and_intersectionx8
     module procedure lset_diff_and_intersectionx9
     module procedure lset_diff_and_intersectionx10
     module procedure lset_diff_and_intersectionx11
     module procedure lset_diff_and_intersectionx12
     module procedure lset_diff_and_intersectionx13
     module procedure lset_diff_and_intersectionx14
     module procedure lset_diff_and_intersectionx15
     module procedure lset_diff_and_intersectionx16
     module procedure lset_diff_and_intersectionx17
     module procedure lset_diff_and_intersectionx18
     module procedure lset_diff_and_intersectionx19
     module procedure lset_diff_and_intersectionx20
  end interface lset_diff_and_intersectionx

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_adjoin0 (equal, lst) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    type(cons_t) :: lst_out

    lst_out = lst
  end function lset_adjoin0

  recursive function lset_adjoin1 (equal, lst, element1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1))

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

  end function lset_adjoin1

  recursive function lset_adjoin2 (equal, lst, element1, element2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2))

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

  end function lset_adjoin2

  recursive function lset_adjoin3 (equal, lst, element1, element2, element3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3))

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

  end function lset_adjoin3

  recursive function lset_adjoin4 (equal, lst, element1, element2, element3, element4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4))

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

  end function lset_adjoin4

  recursive function lset_adjoin5 (equal, lst, element1, element2, element3, element4, &
       &                            element5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5))

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

  end function lset_adjoin5

  recursive function lset_adjoin6 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6))

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

  end function lset_adjoin6

  recursive function lset_adjoin7 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7))

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

  end function lset_adjoin7

  recursive function lset_adjoin8 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8))

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

  end function lset_adjoin8

  recursive function lset_adjoin9 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9))

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

  end function lset_adjoin9

  recursive function lset_adjoin10 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10))

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

  end function lset_adjoin10

  recursive function lset_adjoin11 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11))

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

  end function lset_adjoin11

  recursive function lset_adjoin12 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12))

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

  end function lset_adjoin12

  recursive function lset_adjoin13 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13))

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

  end function lset_adjoin13

  recursive function lset_adjoin14 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13, element14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    class(*), intent(in) :: element14
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13, element14))

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

  end function lset_adjoin14

  recursive function lset_adjoin15 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13, element14, element15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    class(*), intent(in) :: element14
    class(*), intent(in) :: element15
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13, element14, element15))

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

  end function lset_adjoin15

  recursive function lset_adjoin16 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13, element14, element15, element16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    class(*), intent(in) :: element14
    class(*), intent(in) :: element15
    class(*), intent(in) :: element16
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13, element14, element15, element16))

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

  end function lset_adjoin16

  recursive function lset_adjoin17 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13, element14, element15, element16, &
       &                            element17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    class(*), intent(in) :: element14
    class(*), intent(in) :: element15
    class(*), intent(in) :: element16
    class(*), intent(in) :: element17
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13, element14, element15, element16, &
         &                           element17))

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

  end function lset_adjoin17

  recursive function lset_adjoin18 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13, element14, element15, element16, &
       &                            element17, element18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    class(*), intent(in) :: element14
    class(*), intent(in) :: element15
    class(*), intent(in) :: element16
    class(*), intent(in) :: element17
    class(*), intent(in) :: element18
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13, element14, element15, element16, &
         &                           element17, element18))

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

  end function lset_adjoin18

  recursive function lset_adjoin19 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13, element14, element15, element16, &
       &                            element17, element18, element19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    class(*), intent(in) :: element14
    class(*), intent(in) :: element15
    class(*), intent(in) :: element16
    class(*), intent(in) :: element17
    class(*), intent(in) :: element18
    class(*), intent(in) :: element19
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13, element14, element15, element16, &
         &                           element17, element18, element19))

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

  end function lset_adjoin19

  recursive function lset_adjoin20 (equal, lst, element1, element2, element3, element4, &
       &                            element5, element6, element7, element8, &
       &                            element9, element10, element11, element12, &
       &                            element13, element14, element15, element16, &
       &                            element17, element18, element19, element20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst
    class(*), intent(in) :: element1
    class(*), intent(in) :: element2
    class(*), intent(in) :: element3
    class(*), intent(in) :: element4
    class(*), intent(in) :: element5
    class(*), intent(in) :: element6
    class(*), intent(in) :: element7
    class(*), intent(in) :: element8
    class(*), intent(in) :: element9
    class(*), intent(in) :: element10
    class(*), intent(in) :: element11
    class(*), intent(in) :: element12
    class(*), intent(in) :: element13
    class(*), intent(in) :: element14
    class(*), intent(in) :: element15
    class(*), intent(in) :: element16
    class(*), intent(in) :: element17
    class(*), intent(in) :: element18
    class(*), intent(in) :: element19
    class(*), intent(in) :: element20
    type(cons_t) :: lst_out

    lst_out = fold (kons, lst, list (element1, element2, element3, element4, &
         &                           element5, element6, element7, element8, &
         &                           element9, element10, element11, element12, &
         &                           element13, element14, element15, element16, &
         &                           element17, element18, element19, element20))

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

  end function lset_adjoin20

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_union0 (equal) result (lst_out)
    procedure(list_predicate2_t) :: equal
    type(cons_t) :: lst_out

    lst_out = nil
  end function lset_union0

  recursive function lset_union1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union1

  recursive function lset_union2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union2

  recursive function lset_union3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union3

  recursive function lset_union4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union4

  recursive function lset_union5 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union5

  recursive function lset_union6 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union6

  recursive function lset_union7 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union7

  recursive function lset_union8 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union8

  recursive function lset_union9 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union9

  recursive function lset_union10 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union10

  recursive function lset_union11 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union11

  recursive function lset_union12 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union12

  recursive function lset_union13 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union13

  recursive function lset_union14 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13, lst14)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union14

  recursive function lset_union15 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13, lst14, lst15)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union15

  recursive function lset_union16 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13, lst14, lst15, lst16)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union16

  recursive function lset_union17 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13, lst14, lst15, lst16, &
         &       lst17)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union17

  recursive function lset_union18 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13, lst14, lst15, lst16, &
         &       lst17, lst18)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union18

  recursive function lset_union19 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13, lst14, lst15, lst16, &
         &       lst17, lst18, lst19)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union19

  recursive function lset_union20 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &       lst5, lst6, lst7, lst8, &
         &       lst9, lst10, lst11, lst12, &
         &       lst13, lst14, lst15, lst16, &
         &       lst17, lst18, lst19, lst20)
    lst_out = apply_lset_union (equal, lists)
  end function lset_union20

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

  recursive function lset_unionx1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx1

  recursive function lset_unionx2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx2

  recursive function lset_unionx3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx3

  recursive function lset_unionx4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx4

  recursive function lset_unionx5 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx5

  recursive function lset_unionx6 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx6

  recursive function lset_unionx7 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx7

  recursive function lset_unionx8 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx8

  recursive function lset_unionx9 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx9

  recursive function lset_unionx10 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx10

  recursive function lset_unionx11 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx11

  recursive function lset_unionx12 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx12

  recursive function lset_unionx13 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx13

  recursive function lset_unionx14 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx14

  recursive function lset_unionx15 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx15

  recursive function lset_unionx16 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx16

  recursive function lset_unionx17 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13, lst14, lst15, lst16, &
       &                            lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx17

  recursive function lset_unionx18 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13, lst14, lst15, lst16, &
       &                            lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx18

  recursive function lset_unionx19 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13, lst14, lst15, lst16, &
       &                            lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx19

  recursive function lset_unionx20 (equal, lst1, lst2, lst3, lst4, &
       &                            lst5, lst6, lst7, lst8, &
       &                            lst9, lst10, lst11, lst12, &
       &                            lst13, lst14, lst15, lst16, &
       &                            lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_unionx (equal, lists)
  end function lset_unionx20

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

  recursive function lset_intersection2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection2

  recursive function lset_intersectionx2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx2

  recursive function lset_intersection3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection3

  recursive function lset_intersectionx3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx3

  recursive function lset_intersection4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection4

  recursive function lset_intersectionx4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx4

  recursive function lset_intersection5 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection5

  recursive function lset_intersectionx5 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx5

  recursive function lset_intersection6 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection6

  recursive function lset_intersectionx6 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx6

  recursive function lset_intersection7 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection7

  recursive function lset_intersectionx7 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx7

  recursive function lset_intersection8 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection8

  recursive function lset_intersectionx8 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx8

  recursive function lset_intersection9 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection9

  recursive function lset_intersectionx9 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx9

  recursive function lset_intersection10 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection10

  recursive function lset_intersectionx10 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx10

  recursive function lset_intersection11 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection11

  recursive function lset_intersectionx11 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx11

  recursive function lset_intersection12 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection12

  recursive function lset_intersectionx12 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx12

  recursive function lset_intersection13 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection13

  recursive function lset_intersectionx13 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx13

  recursive function lset_intersection14 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection14

  recursive function lset_intersectionx14 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx14

  recursive function lset_intersection15 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection15

  recursive function lset_intersectionx15 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx15

  recursive function lset_intersection16 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection16

  recursive function lset_intersectionx16 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx16

  recursive function lset_intersection17 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection17

  recursive function lset_intersectionx17 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx17

  recursive function lset_intersection18 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection18

  recursive function lset_intersectionx18 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx18

  recursive function lset_intersection19 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection19

  recursive function lset_intersectionx19 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx19

  recursive function lset_intersection20 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_intersection (equal, lists)
  end function lset_intersection20

  recursive function lset_intersectionx20 (equal, lst1, lst2, lst3, lst4, &
       &                                 lst5, lst6, lst7, lst8, &
       &                                 lst9, lst10, lst11, lst12, &
       &                                 lst13, lst14, lst15, lst16, &
       &                                 lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_intersectionx (equal, lists)
  end function lset_intersectionx20

  recursive function apply_lset_intersection (equal, lists) result (lst_out)
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
       lst_out = filter (is_in_every_list, lst1)
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

  end function apply_lset_intersection

  recursive function apply_lset_intersectionx (equal, lists) result (lst_out)
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
       lst_out = filterx (is_in_every_list, lst1)
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

  end function apply_lset_intersectionx

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

  recursive function lset_difference2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference2

  recursive function lset_differencex2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex2

  recursive function lset_difference3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference3

  recursive function lset_differencex3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex3

  recursive function lset_difference4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference4

  recursive function lset_differencex4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex4

  recursive function lset_difference5 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference5

  recursive function lset_differencex5 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex5

  recursive function lset_difference6 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference6

  recursive function lset_differencex6 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex6

  recursive function lset_difference7 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference7

  recursive function lset_differencex7 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex7

  recursive function lset_difference8 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference8

  recursive function lset_differencex8 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex8

  recursive function lset_difference9 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference9

  recursive function lset_differencex9 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex9

  recursive function lset_difference10 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference10

  recursive function lset_differencex10 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex10

  recursive function lset_difference11 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference11

  recursive function lset_differencex11 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex11

  recursive function lset_difference12 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference12

  recursive function lset_differencex12 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex12

  recursive function lset_difference13 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference13

  recursive function lset_differencex13 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex13

  recursive function lset_difference14 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference14

  recursive function lset_differencex14 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex14

  recursive function lset_difference15 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference15

  recursive function lset_differencex15 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex15

  recursive function lset_difference16 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference16

  recursive function lset_differencex16 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex16

  recursive function lset_difference17 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference17

  recursive function lset_differencex17 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex17

  recursive function lset_difference18 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference18

  recursive function lset_differencex18 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex18

  recursive function lset_difference19 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference19

  recursive function lset_differencex19 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex19

  recursive function lset_difference20 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_difference (equal, lists)
  end function lset_difference20

  recursive function lset_differencex20 (equal, lst1, lst2, lst3, lst4, &
       &                               lst5, lst6, lst7, lst8, &
       &                               lst9, lst10, lst11, lst12, &
       &                               lst13, lst14, lst15, lst16, &
       &                               lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_differencex (equal, lists)
  end function lset_differencex20

  recursive function apply_lset_difference (equal, lists) result (lst_out)
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
       lst_out = filter (is_missing_from_every_list, lst1)
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

  end function apply_lset_difference


  recursive function apply_lset_differencex (equal, lists) result (lst_out)
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
       lst_out = filterx (is_missing_from_every_list, lst1)
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

  end function apply_lset_differencex


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

  recursive function lset_diff_and_intersection2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection2

  recursive function lset_diff_and_intersectionx2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx2

  recursive function lset_diff_and_intersection3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection3

  recursive function lset_diff_and_intersectionx3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx3

  recursive function lset_diff_and_intersection4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection4

  recursive function lset_diff_and_intersectionx4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx4

  recursive function lset_diff_and_intersection5 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection5

  recursive function lset_diff_and_intersectionx5 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx5

  recursive function lset_diff_and_intersection6 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection6

  recursive function lset_diff_and_intersectionx6 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx6

  recursive function lset_diff_and_intersection7 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection7

  recursive function lset_diff_and_intersectionx7 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx7

  recursive function lset_diff_and_intersection8 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection8

  recursive function lset_diff_and_intersectionx8 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx8

  recursive function lset_diff_and_intersection9 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection9

  recursive function lset_diff_and_intersectionx9 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx9

  recursive function lset_diff_and_intersection10 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection10

  recursive function lset_diff_and_intersectionx10 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx10

  recursive function lset_diff_and_intersection11 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection11

  recursive function lset_diff_and_intersectionx11 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx11

  recursive function lset_diff_and_intersection12 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection12

  recursive function lset_diff_and_intersectionx12 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx12

  recursive function lset_diff_and_intersection13 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection13

  recursive function lset_diff_and_intersectionx13 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx13

  recursive function lset_diff_and_intersection14 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection14

  recursive function lset_diff_and_intersectionx14 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx14

  recursive function lset_diff_and_intersection15 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection15

  recursive function lset_diff_and_intersectionx15 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx15

  recursive function lset_diff_and_intersection16 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection16

  recursive function lset_diff_and_intersectionx16 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx16

  recursive function lset_diff_and_intersection17 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13, lst14, lst15, lst16, &
       &                                          lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection17

  recursive function lset_diff_and_intersectionx17 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13, lst14, lst15, lst16, &
       &                                           lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx17

  recursive function lset_diff_and_intersection18 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13, lst14, lst15, lst16, &
       &                                          lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection18

  recursive function lset_diff_and_intersectionx18 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13, lst14, lst15, lst16, &
       &                                           lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx18

  recursive function lset_diff_and_intersection19 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13, lst14, lst15, lst16, &
       &                                          lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection19

  recursive function lset_diff_and_intersectionx19 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13, lst14, lst15, lst16, &
       &                                           lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx19

  recursive function lset_diff_and_intersection20 (equal, lst1, lst2, lst3, lst4, &
       &                                          lst5, lst6, lst7, lst8, &
       &                                          lst9, lst10, lst11, lst12, &
       &                                          lst13, lst14, lst15, lst16, &
       &                                          lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_diff_and_intersection (equal, lists)
  end function lset_diff_and_intersection20

  recursive function lset_diff_and_intersectionx20 (equal, lst1, lst2, lst3, lst4, &
       &                                           lst5, lst6, lst7, lst8, &
       &                                           lst9, lst10, lst11, lst12, &
       &                                           lst13, lst14, lst15, lst16, &
       &                                           lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_diff_and_intersectionx (equal, lists)
  end function lset_diff_and_intersectionx20

  recursive function apply_lset_diff_and_intersection (equal, lists) result (diff_and_xsect)
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
       diff_and_xsect = partition (is_not_in_any_list, lst1)
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

  end function apply_lset_diff_and_intersection


  recursive function apply_lset_diff_and_intersectionx (equal, lists) result (diff_and_xsect)
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
       diff_and_xsect = partitionx (is_not_in_any_list, lst1)
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

  end function apply_lset_diff_and_intersectionx


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_xor0 (equal) result (lst_out)
    procedure(list_predicate2_t) :: equal
    type(cons_t) :: lst_out

    lst_out = nil
  end function lset_xor0

  recursive function lset_xor1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor1

  recursive function lset_xor2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor2

  recursive function lset_xor3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor3

  recursive function lset_xor4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor4

  recursive function lset_xor5 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor5

  recursive function lset_xor6 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor6

  recursive function lset_xor7 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor7

  recursive function lset_xor8 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor8

  recursive function lset_xor9 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor9

  recursive function lset_xor10 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor10

  recursive function lset_xor11 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor11

  recursive function lset_xor12 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor12

  recursive function lset_xor13 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor13

  recursive function lset_xor14 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor14

  recursive function lset_xor15 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor15

  recursive function lset_xor16 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor16

  recursive function lset_xor17 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13, lst14, lst15, lst16, &
       &                         lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor17

  recursive function lset_xor18 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13, lst14, lst15, lst16, &
       &                         lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor18

  recursive function lset_xor19 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13, lst14, lst15, lst16, &
       &                         lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor19

  recursive function lset_xor20 (equal, lst1, lst2, lst3, lst4, &
       &                         lst5, lst6, lst7, lst8, &
       &                         lst9, lst10, lst11, lst12, &
       &                         lst13, lst14, lst15, lst16, &
       &                         lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_xor (equal, lists)
  end function lset_xor20


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

  recursive function lset_xorx1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx1

  recursive function lset_xorx2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx2

  recursive function lset_xorx3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx3

  recursive function lset_xorx4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx4

  recursive function lset_xorx5 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx5

  recursive function lset_xorx6 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx6

  recursive function lset_xorx7 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx7

  recursive function lset_xorx8 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx8

  recursive function lset_xorx9 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx9

  recursive function lset_xorx10 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx10

  recursive function lset_xorx11 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx11

  recursive function lset_xorx12 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx12

  recursive function lset_xorx13 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx13

  recursive function lset_xorx14 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx14

  recursive function lset_xorx15 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx15

  recursive function lset_xorx16 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx16

  recursive function lset_xorx17 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx17

  recursive function lset_xorx18 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx18

  recursive function lset_xorx19 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18, lst19) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx19

  recursive function lset_xorx20 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18, lst19, lst20) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    type(cons_t) :: lst_out

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    lst_out = apply_lset_xorx (equal, lists)
  end function lset_xorx20


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

  recursive function lset_subset3 (equal, lst1, lst2, lst3) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset3

  recursive function lset_subset4 (equal, lst1, lst2, lst3, lst4) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset4

  recursive function lset_subset5 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset5

  recursive function lset_subset6 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset6

  recursive function lset_subset7 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset7

  recursive function lset_subset8 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset8

  recursive function lset_subset9 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset9

  recursive function lset_subset10 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset10

  recursive function lset_subset11 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset11

  recursive function lset_subset12 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset12

  recursive function lset_subset13 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset13

  recursive function lset_subset14 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13, lst14) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset14

  recursive function lset_subset15 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13, lst14, lst15) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset15

  recursive function lset_subset16 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13, lst14, lst15, lst16) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset16

  recursive function lset_subset17 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13, lst14, lst15, lst16, &
       &                           lst17) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset17

  recursive function lset_subset18 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13, lst14, lst15, lst16, &
       &                           lst17, lst18) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset18

  recursive function lset_subset19 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13, lst14, lst15, lst16, &
       &                           lst17, lst18, lst19) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset19

  recursive function lset_subset20 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10, lst11, lst12, &
       &                           lst13, lst14, lst15, lst16, &
       &                           lst17, lst18, lst19, lst20) result (subset)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    logical :: subset

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    subset = apply_lset_subset (equal, lists)
  end function lset_subset20

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

  recursive function lset_equal3 (equal, lst1, lst2, lst3) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal3

  recursive function lset_equal4 (equal, lst1, lst2, lst3, lst4) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal4

  recursive function lset_equal5 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal5

  recursive function lset_equal6 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal6

  recursive function lset_equal7 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal7

  recursive function lset_equal8 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal8

  recursive function lset_equal9 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal9

  recursive function lset_equal10 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal10

  recursive function lset_equal11 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal11

  recursive function lset_equal12 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal12

  recursive function lset_equal13 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal13

  recursive function lset_equal14 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal14

  recursive function lset_equal15 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal15

  recursive function lset_equal16 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal16

  recursive function lset_equal17 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal17

  recursive function lset_equal18 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal18

  recursive function lset_equal19 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18, lst19) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal19

  recursive function lset_equal20 (equal, lst1, lst2, lst3, lst4, &
       &                          lst5, lst6, lst7, lst8, &
       &                          lst9, lst10, lst11, lst12, &
       &                          lst13, lst14, lst15, lst16, &
       &                          lst17, lst18, lst19, lst20) result (eq)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    class(*), intent(in) :: lst11
    class(*), intent(in) :: lst12
    class(*), intent(in) :: lst13
    class(*), intent(in) :: lst14
    class(*), intent(in) :: lst15
    class(*), intent(in) :: lst16
    class(*), intent(in) :: lst17
    class(*), intent(in) :: lst18
    class(*), intent(in) :: lst19
    class(*), intent(in) :: lst20
    logical :: eq

    type(cons_t) :: lists

    lists = list (lst1, lst2, lst3, lst4, &
         &        lst5, lst6, lst7, lst8, &
         &        lst9, lst10, lst11, lst12, &
         &        lst13, lst14, lst15, lst16, &
         &        lst17, lst18, lst19, lst20)
    eq = apply_lset_equal (equal, lists)
  end function lset_equal20

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
