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
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: lset_adjoin[]n
])dnl

  ! Implementations of lset_union.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: lset_union[]n
])dnl

  ! Implementations of lset_unionx.
m4_forloop([n],[0],ZIP_MAX,[dnl
  public :: lset_unionx[]n
])dnl

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

  interface lset_adjoin
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure lset_adjoin[]n
])dnl
  end interface lset_adjoin

  interface lset_union
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure lset_union[]n
])dnl
  end interface lset_union

  interface lset_unionx
m4_forloop([n],[0],ZIP_MAX,[dnl
     module procedure lset_unionx[]n
])dnl
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

m4_forloop([n],[1],ZIP_MAX,[dnl
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

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function lset_union[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                          ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    lst_out = reduce (make_union, nil, list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &            ])lst[]k])))

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

  end function lset_union[]n

])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_unionx0 (equal) result (lst_out)
    procedure(list_predicate2_t) :: equal
    type(cons_t) :: lst_out

    lst_out = nil
  end function lset_unionx0

m4_forloop([n],[1],ZIP_MAX,[dnl
  recursive function lset_unionx[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                           ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    lst_out = reduce (make_unionx, nil, list (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &            ])lst[]k])))

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

      if (is_nil (member (equal, car (pair), lst))) then
         call set_cdr (pair, lst)
         lst_out = pair
      else
         lst_out = lst
      end if
    end subroutine kons

  end function lset_unionx[]n

])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module lsets
