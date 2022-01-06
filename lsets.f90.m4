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

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none
  private

  ! Generic functions.
  public :: lset_adjoin         ! Add elements to a set.
  public :: lset_union          ! Return the union of sets.
  public :: lset_unionx         ! Union that can alter its inputs.
  public :: lset_intersection   ! Return the intersection of sets.

  ! Implementations of lset_adjoin.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: lset_adjoin[]n
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

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

  interface lset_adjoin
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure lset_adjoin[]n
])dnl
  end interface lset_adjoin

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

m4_forloop([n],[1],LISTN_MAX,[dnl
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

  end function lset_unionx[]n

])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_intersection1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_intersection1

m4_forloop([n],[2],LISTN_MAX,[dnl
  recursive function lset_intersection[]n (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                  ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists
    class(*), allocatable :: x ! x is used by the nested procedures.

    lists = list (lst2[]m4_forloop([k],[3],n,[, m4_if(m4_eval(k % 4),[1],[&
         &       ])lst[]k]))
    lists = delete (cons_t_eq, lst1, lists) ! Remove any references to lst1.
    if (some (is_nil_list, lists)) then
       ! The intersection of a set with a null set is a null set.
       lst_out = nil
    else
       lst_out = filter (is_in_every_list, lst1)
    end if

  contains

    recursive function is_in_every_list (x_value) result (bool)
      class(*), intent(in) :: x_value
      logical :: bool

      x = x_value
      bool = every (x_is_in, lists)
    end function is_in_every_list

    recursive function x_is_in (lst) result (bool)
      class(*), intent(in) :: lst
      logical :: bool

      bool = is_not_nil (member (equal, x, lst))
    end function x_is_in

  end function lset_intersection[]n

])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module lsets
