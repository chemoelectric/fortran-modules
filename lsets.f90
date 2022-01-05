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

module lsets
  !
  ! Lsets (sets implemented as CONS-pair lists) in the fashion of
  ! SRFI-1.
  ! https://srfi.schemers.org/srfi-1/srfi-1.html
  !
  ! The code here does in Fortran what the reference implementation of
  ! SRFI-1 lsets does in Scheme.
  !

  !
  ! NOTE: Unless you know what you are doing, you should use
  !       `type(gcroot_t)' from module `garbage_collector' to hold
  !       values of type `cons_t'. Otherwise the garbage collector
  !       might collect your work unexpectedly.
  !

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none
  private

  ! Generic functions.
  public :: lset_adjoin         ! Add elements to a set.
  public :: lset_union          ! Return the union of two sets.
  public :: lset_unionx         ! Union that can alter its inputs.

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

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

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
  end interface lset_adjoin

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
  end interface lset_unionx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  interface error_abort
!!$     module procedure error_abort_1
!!$  end interface error_abort

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  subroutine error_abort_1 (msg)
!!$    use iso_fortran_env, only : error_unit
!!$    character(*), intent(in) :: msg
!!$    write (error_unit, '()')
!!$    write (error_unit, '("module lsets error: ", a)') msg
!!$    error stop
!!$  end subroutine error_abort_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    lst_out = reduce (make_union, nil, list (lst1))

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

  end function lset_union1

  recursive function lset_union2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    lst_out = reduce (make_union, nil, list (lst1, lst2))

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

  end function lset_union2

  recursive function lset_union3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3))

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

  end function lset_union3

  recursive function lset_union4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3, lst4))

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

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5))

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

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6))

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

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7))

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

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7, lst8))

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

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7, lst8, &
         &            lst9))

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

    lst_out = reduce (make_union, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7, lst8, &
         &            lst9, lst10))

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

  end function lset_union10

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

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx1

  recursive function lset_unionx2 (equal, lst1, lst2) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_out

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx2

  recursive function lset_unionx3 (equal, lst1, lst2, lst3) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_out

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx3

  recursive function lset_unionx4 (equal, lst1, lst2, lst3, lst4) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_out

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3, lst4))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx4

  recursive function lset_unionx5 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_out

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx5

  recursive function lset_unionx6 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_out

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx6

  recursive function lset_unionx7 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_out

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx7

  recursive function lset_unionx8 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8) result (lst_out)
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

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7, lst8))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx8

  recursive function lset_unionx9 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9) result (lst_out)
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

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7, lst8, &
         &            lst9))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx9

  recursive function lset_unionx10 (equal, lst1, lst2, lst3, lst4, &
       &                           lst5, lst6, lst7, lst8, &
       &                           lst9, lst10) result (lst_out)
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

    ! This variable is used by the nested procedures.
    class(*), allocatable :: element

    lst_out = reduce (make_unionx, nil, list (lst1, lst2, lst3, lst4, &
         &            lst5, lst6, lst7, lst8, &
         &            lst9, lst10))

  contains

    recursive subroutine make_unionx (lst1, lst2, lst_out)
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
         lst_out = pair_fold (kons, lst2, lst1)
      end if
    end subroutine make_unionx

    recursive subroutine kons (pair, lst, lst_out)
      class(*), intent(in) :: pair
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: lst_out

      element = car (pair)
      if (some (equals_element, lst)) then
         lst_out = lst
      else
         call set_cdr (pair, lst)
         lst_out = pair
      end if
    end subroutine kons

    recursive function equals_element (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      bool = equal (x, element)
    end function equals_element

  end function lset_unionx10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module lsets
