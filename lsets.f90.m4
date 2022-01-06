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

!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME: I am repeating A LOT OF code that actually can be shared
!!!!!! FIXME:    by implementations for different numbers of elements.
!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME
!!!!!! FIXME

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

  ! Functions that take a list of lists as an argument. They resemble
  ! using Scheme's `apply' procedure; for example,
  !
  !    apply_lset_union (equal, list_of_lists)
  !
  ! is a Fortran equivalent to the Scheme code
  !
  !    (apply lset-union equal list-of-lists))
  !
  public :: apply_lset_union    ! Return the union of the sets.
  public :: apply_lset_unionx   ! Union that can alter its inputs.
  public :: apply_lset_xor      ! Return the exclusive OR of the sets.

  ! Generic functions, taking their arguments as the sets to operate
  ! upon.
  public :: lset_union          ! Return the union of sets.
  public :: lset_unionx         ! Union that can alter its inputs.
  public :: lset_intersection   ! Return the intersection of sets.
  public :: lset_intersectionx  ! Intersection that can alter its
                                ! inputs.
  public :: lset_difference     ! Return the difference of sets.
  public :: lset_differencex    ! Difference that can alter its
                                ! inputs.
  public :: lset_xor            ! Return the exclusive OR of sets.
  !public :: lset_xorx           ! XOR that can alter its inputs.

  ! lset_diff_and_intersection and lset_diff_and_intersectionx return
  ! the equivalent of
  !
  !    list (lset_difference (equal, lst1, lst2, ...),   &
  !          lset_intersection (equal, lst1,             &
  !                             lset_union (equal, lst2, ...)))
  !
  ! But they are more efficient at it.
  !
  public :: lset_diff_and_intersection  ! Not allowed to alter its
                                        ! inputs.
  public :: lset_diff_and_intersectionx ! Allowed to alter its inputs.

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

!!$  ! Implementations of lset_xorx.
!!$m4_forloop([n],[1],LISTN_MAX,[dnl
!!$  public :: lset_xorx[]n
!!$])dnl

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

!!$  interface lset_xorx
!!$m4_forloop([n],[1],LISTN_MAX,[dnl
!!$     module procedure lset_xorx[]n
!!$])dnl
!!$  end interface lset_xorx

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

m4_define([m4_lset_intersection],[dnl
  recursive function $1[]$2 (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                  ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists
    class(*), allocatable :: x ! x is used by the nested procedures.

    lists = list (lst2[]m4_forloop([k],[3],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lists = delete (cons_t_eq, lst1, lists) ! Remove any references to lst1.
    if (some (is_nil_list, lists)) then
       ! The intersection of a set with a null set is a null set.
       lst_out = nil
    else
       lst_out = $3 (is_in_every_list, lst1)
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

  end function $1[]$2

])dnl
dnl
  recursive function lset_intersection1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_intersection1

m4_forloop([n],[2],LISTN_MAX,[m4_lset_intersection([lset_intersection],n,[filter])])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_intersectionx1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_intersectionx1

m4_forloop([n],[2],LISTN_MAX,[m4_lset_intersection([lset_intersectionx],n,[filterx])])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

m4_define([m4_lset_difference],[dnl
  recursive function $1[]$2 (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                  ])lst[]k])) result (lst_out)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_out

    type(cons_t) :: lists
    class(*), allocatable :: x ! x is used by the nested procedures.

    lists = list (lst2[]m4_forloop([k],[3],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    lists = remove (is_not_pair, lists) ! Ignore null sets.
    if (is_not_nil (member (cons_t_eq, lst1, lists))) then
       ! The difference of a set and itself is a null set.
       lst_out = nil
    else
       lst_out = $3 (is_missing_from_every_list, lst1)
    end if

  contains

    recursive function is_missing_from_every_list (x_value) result (bool)
      class(*), intent(in) :: x_value
      logical :: bool

      x = x_value
      bool = every (x_is_not_in, lists)
    end function is_missing_from_every_list

    recursive function x_is_not_in (lst) result (bool)
      class(*), intent(in) :: lst
      logical :: bool

      bool = is_nil (member (equal, x, lst))
    end function x_is_not_in

  end function $1[]$2

])dnl
dnl
  recursive function lset_difference1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_difference1

m4_forloop([n],[2],LISTN_MAX,[m4_lset_difference([lset_difference],n,[filter])])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_differencex1 (equal, lst1) result (lst_out)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_out

    lst_out = lst1
  end function lset_differencex1

m4_forloop([n],[2],LISTN_MAX,[m4_lset_difference([lset_differencex],n,[filterx])])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

m4_define([m4_lset_diff_and_intersection],[dnl
  recursive function $1[]$2 (equal, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
       &                                  ])lst[]k])) result (diff_and_xsect)
    procedure(list_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: diff_and_xsect

    type(cons_t) :: lists
    class(*), allocatable :: x ! x is used by the nested procedures.

    lists = list (lst2[]m4_forloop([k],[3],n,[, m4_if(m4_eval(k % 4),[1],[&
         &        ])lst[]k]))
    if (is_not_nil (member (cons_t_eq, lst1, lists))) then
       ! Difference and intersection of a set with a set containing
       ! it.
       diff_and_xsect = list (nil, lst1)
    else
       diff_and_xsect = $3 (is_not_in_any_list, lst1)
    end if

  contains

    recursive function is_not_in_any_list (x_value) result (bool)
      class(*), intent(in) :: x_value
      logical :: bool

      x = x_value
      bool = .not. some (x_is_in, lists)
    end function is_not_in_any_list

    recursive function x_is_in (lst) result (bool)
      class(*), intent(in) :: lst
      logical :: bool

      bool = is_not_nil (member (equal, x, lst))
    end function x_is_in

  end function $1[]$2

])dnl
dnl
  recursive function lset_diff_and_intersection1 (equal, lst1) result (diff_and_xsect)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: diff_and_xsect

    diff_and_xsect = list (lst1, nil)
  end function lset_diff_and_intersection1

m4_forloop([n],[2],LISTN_MAX,[m4_lset_diff_and_intersection([lset_diff_and_intersection],n,[partition])])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lset_diff_and_intersectionx1 (equal, lst1) result (diff_and_xsect)
    procedure(list_predicate2_t) :: equal
    class(*), intent(in) :: lst1
    type(cons_t) :: diff_and_xsect

    diff_and_xsect = list (lst1, nil)
  end function lset_diff_and_intersectionx1

m4_forloop([n],[2],LISTN_MAX,[m4_lset_diff_and_intersection([lset_diff_and_intersectionx],n,[partitionx])])dnl
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

end module lsets
