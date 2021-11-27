! -*- F90 -*- include(`common-macros.m4')
!
! Copyright 2021 Barry Schwartz
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

module test__cons_lists
  use cons_lists
  implicit none
  private

  public :: run_tests

  interface operator(.eqi.)
     module procedure integer_eq
  end interface operator(.eqi.)

  interface operator(.eqr.)
     module procedure real_eq
  end interface operator(.eqr.)

contains

  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("test__cons_lists error: ", a)') msg
    CALL_ABORT
  end subroutine error_abort

  subroutine check (boolean, msg)
    logical, intent(in) :: boolean
    character(*), intent(in) :: msg
    if (.not. boolean) call error_abort (msg)
  end subroutine check

  function integer_cast (obj) result (int)
    class(*), intent(in) :: obj
    integer :: int
    select type (obj)
    type is (integer)
       int = obj
    end select
  end function integer_cast

  function real_cast (obj) result (int)
    class(*), intent(in) :: obj
    real :: int
    select type (obj)
    type is (real)
       int = obj
    end select
  end function real_cast

  function integer_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = integer_cast (obj1) == integer_cast (obj2)
  end function integer_eq

  function real_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = real_cast (obj1) == real_cast (obj2)
  end function real_eq

  function cosine_wrapper (x) result (y)
    class(cons_t), intent(in) :: x
    type(cons_t) :: y
    y = list1 (cos (real_cast (first (x))))
  end function cosine_wrapper

  function division_wrapper (x) result (y)
    class(cons_t), intent(in) :: x
    type(cons_t) :: y
    integer :: dividend, divisor
    integer :: quotient, remainder
    dividend = integer_cast (first (x))
    divisor = integer_cast (second (x))
    quotient = dividend / divisor
    remainder = mod (dividend, divisor)
    y = list2 (quotient, remainder)
  end function division_wrapper

  subroutine test_is_nil_list
    call check (.not. is_nil_list ('abc'), ".not. is_nil_list ('abc') failed")
    call check (is_nil_list (nil_list), "is_nil_list (nil_list) failed")
    call check (.not. is_nil_list (iota (15)), ".not. is_nil_list (iota (15)) failed")
    call check (.not. is_nil_list (cons ('a', 'b')), ".not. is_nil_list (cons ('a', 'b')) failed")
  end subroutine test_is_nil_list

  subroutine test_is_cons_pair
    call check (.not. is_cons_pair ('abc'), ".not. is_cons_pair ('abc') failed")
    call check (.not. is_cons_pair (nil_list), ".not. is_cons_pair (nil_list) failed")
    call check (is_cons_pair (iota (15)), "is_cons_pair (iota (15)) failed")
    call check (is_cons_pair (cons ('a', 'b')), "is_cons_pair (cons ('a', 'b')) failed")
  end subroutine test_is_cons_pair

  subroutine test_list_is_nil
    call check (list_is_nil (nil_list), "list_is_nil (nil_list) failed")
    call check (.not. list_is_nil (iota (15)), ".not. list_is_nil (iota (15)) failed")
    call check (.not. list_is_nil (cons('a', 'b')), ".not. list_is_nil (cons('a', 'b')) failed")
  end subroutine test_list_is_nil

  subroutine test_list_is_pair
    call check (.not. list_is_pair (nil_list), ".not. list_is_pair (nil_list) failed")
    call check (list_is_pair (iota (15)), "list_is_pair (iota (15)) failed")
    call check (list_is_pair (cons('a', 'b')), "list_is_pair (cons('a', 'b')) failed")
  end subroutine test_list_is_pair

  subroutine test_cons_t_eq
    type(cons_t) :: pair1, pair2
    pair1 = 1 ** nil_list
    pair2 = cons (1, 2)
    call check (cons_t_eq (nil_list, nil_list), "cons_t_eq (nil_list, nil_list) failed")
    call check (.not. cons_t_eq (nil_list, 1 ** nil_list), ".not. cons_t_eq (nil_list, 1 ** nil_list) failed")
    call check (.not. cons_t_eq (1 ** nil_list, nil_list), ".not. cons_t_eq (1 ** nil_list, nil_list) failed")
    call check (.not. cons_t_eq (1 ** nil_list, 1 ** nil_list), ".not. cons_t_eq (1 ** nil_list, 1 ** nil_list) failed")
    call check (.not. cons_t_eq (cons (1, 2), cons (1, 2)), ".not. cons_t_eq (cons (1, 2), cons (1, 2)) failed")
    call check (cons_t_eq (pair1, pair1), "cons_t_eq (pair1, pair1) failed")
    call check (cons_t_eq (pair2, pair2), "cons_t_eq (pair2, pair2) failed")
    call check (.not. cons_t_eq (pair1, pair2), ".not. cons_t_eq (pair1, pair2) failed")
    call check (.not. cons_t_eq (pair2, pair1), ".not. cons_t_eq (pair2, pair1) failed")
  end subroutine test_cons_t_eq

  subroutine test_uncons_car_cdr
    !
    ! FIXME: For uncons, add tests that you can output to the same
    !        variables as were used for inputs.
    !
    class(*), allocatable :: x, y
    type(cons_t) :: pair
    pair = cons (5, 15.0)
    call uncons (pair, x, y)
    call check (x .eqi. 5, "x .eqi. 5 failed (for uncons)")
    call check (y .eqr. 15.0, "y .eqr. 15.0 failed (for uncons)")
    call check (car (pair) .eqi. 5, "car (pair) .eqi. 5 failed")
    call check (cdr (pair) .eqr. 15.0, "cdr (pair) .eqr. 15.0 failed")
  end subroutine test_uncons_car_cdr

  subroutine test_list_cons
    type(cons_t) :: lst1, lst2
    lst1 = list_cons (cons (1, 2), list_cons (3.0, nil_list))
    lst2 = cons (1, 2) ** 3.0 ** nil_list
    call check (caar (lst1) .eqi. 1, "caar (lst1) .eqi. 1 failed (for list_cons)")
    call check (cdar (lst1) .eqi. 2, "cdar (lst1) .eqi. 1 failed (for list_cons)")
    call check (cadr (lst1) .eqr. 3.0, "cadr (lst1) .eqr. 3.0 failed (for list_cons)")
    call check (caar (lst2) .eqi. 1, "caar (lst2) .eqi. 1 failed (for operator(**))")
    call check (cdar (lst2) .eqi. 2, "cdar (lst2) .eqi. 1 failed (for operator(**))")
    call check (cadr (lst2) .eqr. 3.0, "cadr (lst2) .eqr. 3.0 failed (for operator(**))")
  end subroutine test_list_cons

  subroutine test_set_car_and_set_cdr
    type(cons_t) :: pair
    pair = cons (1.0, 2)
    call set_car (pair, 1)
    call set_cdr (pair, 2.0)
    call check (car (pair) .eqi. 1, "car (pair) .eqi. 1 failed (for set_car)")
    call check (cdr (pair) .eqr. 2.0, "cdr (pair) .eqr. 2.0 failed (for set_cdr)")
  end subroutine test_set_car_and_set_cdr

  subroutine test_list_length
    call check (list_length (nil_list) == 0, "list_length (nil_list) == 0 failed")
    call check (list_length (1 ** nil_list) == 1, "list_length (1 ** nil_list) == 1 failed")
    call check (list_length (iota (20)) == 20, "list_length (iota (20)) == 20 failed")
    call check (list_length (make_list (200, 'a')) == 200, "list_length (make_list (200, 'a')) == 200 failed")
    call check (list_length (0.0 ** cons (1, 2)) == 2, "list_length (0.0 ** cons (1, 2)) == 2 failed")
    call check (list_length (cons (1, 2)) == 1, "list_length (cons (1, 2)) == 1 failed")
    !
    ! A degenerate case.
    !
    call check (list_length ('abc') == 0, "list_length ('abc') == 0 failed")
  end subroutine test_list_length

  subroutine test_is_proper_list
    call check (.not. is_proper_list (4), ".not. is_proper_list (4) failed")
    call check (is_proper_list (nil_list), "is_proper_list (nil_list) failed")
    call check (is_proper_list (1 ** 2 ** 3 ** nil_list), "is_proper_list (1 ** 2 ** 3 ** nil_list) failed")
    call check (.not. is_proper_list (circular_list (1 ** 2 ** 3 ** nil_list)), &
         "is_proper_list (circular_list (1 ** 2 ** 3 ** nil_list)) failed")
    call check (.not. is_proper_list (cons (1, 2)), ".not. is_proper_list (cons (1, 2)) failed")
    call check (.not. is_proper_list ('a' ** 3.0 ** cons (1, 2)), ".not. is_proper_list ('a' ** 3.0 ** cons (1, 2)) failed")
  end subroutine test_is_proper_list

  subroutine test_is_dotted_list
    call check (is_dotted_list (4), "is_dotted_list (4) failed")
    call check (.not. is_dotted_list (nil_list), ".not. is_dotted_list (nil_list) failed")
    call check (.not. is_dotted_list (1 ** 2 ** 3 ** nil_list), ".not. is_dotted_list (1 ** 2 ** 3 ** nil_list) failed")
    call check (.not. is_dotted_list (circular_list (1 ** 2 ** 3 ** nil_list)), &
         "is_dotted_list (circular_list (1 ** 2 ** 3 ** nil_list)) failed")
    call check (is_dotted_list (cons (1, 2)), "is_dotted_list (cons (1, 2)) failed")
    call check (is_dotted_list ('a' ** 3.0 ** cons (1, 2)), "is_dotted_list ('a' ** 3.0 ** cons (1, 2)) failed")
  end subroutine test_is_dotted_list

  subroutine test_is_circular_list
    call check (.not. is_circular_list (4), ".not. is_circular_list (4) failed")
    call check (.not. is_circular_list (nil_list), ".not. is_circular_list (nil_list) failed")
    call check (.not. is_circular_list (1 ** 2 ** 3 ** nil_list), ".not. is_circular_list (1 ** 2 ** 3 ** nil_list) failed")
    call check (is_circular_list (circular_list (1 ** 2 ** 3 ** nil_list)), &
         "is_circular_list (circular_list (1 ** 2 ** 3 ** nil_list)) failed")
    call check (is_circular_list (1.0 ** 2.0 ** circular_list (1 ** 2 ** 3 ** nil_list)), &
         "is_circular_list (1.0 ** 2.0 ** circular_list (1 ** 2 ** 3 ** nil_list)) failed")
  end subroutine test_is_circular_list

  subroutine test_car_cadr_caddr_cadddr
    type(cons_t) :: lst
    lst = iota (15, 1)
    call check (car (lst) .eqi. 1, "car (lst) .eqi. 1 failed")
    call check (cadr (lst) .eqi. 2, "cadr (lst) .eqi. 2 failed")
    call check (caddr (lst) .eqi. 3, "caddr (lst) .eqi. 3 failed")
    call check (cadddr (lst) .eqi. 4, "cadddr (lst) .eqi. 4 failed")
  end subroutine test_car_cadr_caddr_cadddr

  subroutine test_first_second_etc
    type(cons_t) :: lst
    lst = iota (15, 1)
    call check (first (lst) .eqi. 1, "first (lst) .eqi. 1 failed")
    call check (second (lst) .eqi. 2, "second (lst) .eqi. 2 failed")
    call check (third (lst) .eqi. 3, "third (lst) .eqi. 3 failed")
    call check (fourth (lst) .eqi. 4, "fourth (lst) .eqi. 4 failed")
    call check (fifth (lst) .eqi. 5, "fifth (lst) .eqi. 5 failed")
    call check (sixth (lst) .eqi. 6, "sixth (lst) .eqi. 6 failed")
    call check (seventh (lst) .eqi. 7, "seventh (lst) .eqi. 7 failed")
    call check (eighth (lst) .eqi. 8, "eighth (lst) .eqi. 8 failed")
    call check (ninth (lst) .eqi. 9, "ninth (lst) .eqi. 9 failed")
    call check (tenth (lst) .eqi. 10, "tenth (lst) .eqi. 10 failed")
  end subroutine test_first_second_etc

  subroutine test_list_ref0
    type(cons_t) :: lst
    integer :: i
    lst = iota (15, 1)
    do i = 0, 14
       call check (list_ref0 (lst, i) .eqi. (i + 1), "list_ref0 (lst, i) .eqi. (i + 1) failed")
    end do
  end subroutine test_list_ref0

  subroutine test_list_ref1
    type(cons_t) :: lst
    integer :: i
    lst = iota (15, 1)
    do i = 1, 15
       call check (list_ref1 (lst, i) .eqi. i, "list_ref1 (lst, i) .eqi. i failed")
    end do
  end subroutine test_list_ref1

  subroutine test_list_refn
    type(cons_t) :: lst
    integer :: i
    lst = iota (15, 1)
    do i = 2, 16
       call check (list_refn (lst, i, 2) .eqi. (i - 1), "list_refn (lst, i, 2) .eqi. (i - 1) failed")
    end do
    do i = -1, 13
       call check (list_refn (lst, i, -1) .eqi. (i + 2), "list_refn (lst, i, -1) .eqi. (i + 2) failed")
    end do
  end subroutine test_list_refn

  subroutine test_list_last
    type(cons_t) :: lst
    integer :: i
    lst = iota (15, 1)
    call check (list_last (lst) .eqi. 15, "list_last (lst) .eqi. 15 failed")
  end subroutine test_list_last

  subroutine test_list_last_pair
    type(cons_t) :: lst
    integer :: i
    lst = iota (15, 1)
    call check (car (list_last_pair (lst)) .eqi. 15, "car (list_last_pair (lst)) .eqi. 15 failed")
    call check (is_nil_list (cdr (list_last_pair (lst))), "is_nil_list (cdr (list_last_pair (lst))) failed")
  end subroutine test_list_last_pair

  subroutine test_make_list
    type(cons_t) :: lst
    integer :: i
    call check (is_nil_list (make_list (0, 'abc')), "is_nil_list (make_list (0, 'abc')) failed")
    lst = make_list (30, 5.0)
    call check (list_length (lst) == 30, "list_length (lst) == 30 failed (for make_list)")
    do i = 1, 30
       call check (list_ref1 (lst, i) .eqr. 5.0, "list_ref1 (lst, i) .eqr. 5.0 failed (for make_list)")
    end do
  end subroutine test_make_list

  subroutine test_iota
    type(cons_t) :: lst
    integer :: i
    lst = iota (100)
    do i = 0, 99
       call check (list_ref0 (lst, i) .eqi. i, "list_ref0 (lst, i) .eqi. i failed (for iota (100))")
    end do
    lst = iota (100, 1)
    do i = 1, 100
       call check (list_ref1 (lst, i) .eqi. i, "list_ref1 (lst, i) .eqi. i failed (for iota (100, 1))")
    end do
    lst = iota (100, 0, 20)
    do i = 0, 99
       call check (list_ref0 (lst, i) .eqi. (20 * i), "list_ref0 (lst, i) .eqi. (20 * i) failed (for iota (100, 0, 20))")
    end do
  end subroutine test_iota

  subroutine test_circular_list
    type(cons_t) :: lst
    integer :: i
    lst = circular_list (0 ** 1 ** 2 ** 3 ** nil_list)
    do i = 0, 99
       call check (list_ref0 (lst, i) .eqi. mod (i, 4), "list_ref0 (lst, i) .eqi. mod (i, 4) failed")
    end do
    !
    ! Now break the circle.
    call set_cdr (cons_t_cast (cdddr (lst)), nil_list)
    call check (list_length (lst) == 4, "list_length (lst) == 4 failed (for circular_list)")
    do i = 0, 3
       call check (list_ref0 (lst, i) .eqi. i, "list_ref0 (lst, i) .eqi. i failed (for circular_list)")
    end do
  end subroutine test_circular_list

  subroutine test_list_unlist
    type(cons_t) :: lst1, lst2, lst3, lst4
    class(*), allocatable :: x, y, z, tail
    lst1 = list1 (123)
    call check (list_length (lst1) == 1, "list_length (lst1) == 1 failed (for list1)")
    call check (first (lst1) .eqi. 123, "first (lst1) .eqi. 123 failed (for list1)")
    call unlist1 (lst1, x)
    call check (x .eqi. 123, "x .eqi. 123 failed (for list1)")
    call unlist1_with_tail (lst1, x, tail)
    call check ((x .eqi. 123) .and. (is_nil_list (tail)), "(x .eqi. 123) .and. (is_nil_list (tail)) failed (for list1)")
    lst2 = list2 (123, 456)
    call check (list_length (lst2) == 2, "list_length (lst2) == 2 failed (for list2)")
    call check (first (lst2) .eqi. 123, "first (lst2) .eqi. 123 failed (for list2)")
    call check (second (lst2) .eqi. 456, "second (lst2) .eqi. 456 failed (for list2)")
    call unlist2 (lst2, x, y)
    call check ((x .eqi. 123) .and. (y .eqi. 456), "(x .eqi. 123) .and. (y .eqi. 456) failed (for list2)")
    call unlist2_with_tail (lst2, x, y, tail)
    call check ((x .eqi. 123) .and. (y .eqi. 456) .and. (is_nil_list (tail)), &
         "(x .eqi. 123) .and. (y .eqi. 456) .and. (is_nil_list (tail)) failed (for list2)")
    lst3 = list3 (123, 456, 789)
    call check (list_length (lst3) == 3, "list_length (lst3) == 3 failed (for list3)")
    call check (first (lst3) .eqi. 123, "first (lst3) .eqi. 123 failed (for list3)")
    call check (second (lst3) .eqi. 456, "second (lst3) .eqi. 456 failed (for list3)")
    call unlist3 (lst3, x, y, z)
    call check ((x .eqi. 123) .and. (y .eqi. 456) .and. (z .eqi. 789), &
         "(x .eqi. 123) .and. (y .eqi. 456) .and. (z .eqi. 789) failed (for list3)")
    call unlist3_with_tail (lst3, x, y, z, tail)
    call check ((x .eqi. 123) .and. (y .eqi. 456) .and. (z .eqi. 789) .and. (is_nil_list (tail)), &
         "(x .eqi. 123) .and. (y .eqi. 456) .and. (z .eqi. 789) .and. (is_nil_list (tail)) failed (for list3)")
    call unlist2_with_tail (lst3, x, y, tail)
    call check ((x .eqi. 123) .and. (y .eqi. 456) .and. (car (tail) .eqi. 789) .and. (is_nil_list (cdr (tail))), &
         "(x .eqi. 123) .and. (y .eqi. 456) .and. (car (tail) .eqi. 789) .and. (is_nil_list (cdr (tail))) failed (for list3)")
    !
    ! Now check a case with a dotted list.
    !
    lst4 = cons (123, cons (456, 789))
    call unlist2 (lst4, x, y)
    call check ((x .eqi. 123) .and. (y .eqi. 456), "(x .eqi. 123) .and. (y .eqi. 456) failed (for unlist2 of a dotted list)")
    call unlist2_with_tail (lst4, x, y, tail)
    call check ((x .eqi. 123) .and. (y .eqi. 456) .and. (tail .eqi. 789), &
         "(x .eqi. 123) .and. (y .eqi. 456) .and. (tail .eqi. 789) failed (for unlist2_with_tail of a dotted list)")
  end subroutine test_list_unlist

  subroutine test_list_reverse
    type(cons_t) :: lst1, lst2
    integer :: i
    lst1 = iota (15, 1)
    lst2 = list_reverse (lst1)
    call set_car (lst1, nil_list)
    call set_cdr (lst1, nil_list)
    call check (list_length (lst1) == 1, "list_length (lst1) == 1 failed (for list_reverse)")
    call check (is_nil_list (car (lst1)), "is_nil_list (car (lst1)) failed (for list_reverse)")
    call check (list_length (lst2) == 15, "list_length (lst2) == 15 failed (for list_reverse)")
    do i = 1, 15
       call check (list_ref1 (lst2, i) .eqi. (16 - i), "list_ref1 (lst2, i) .eqi. (16 - i) failed (for list_reverse)")
    end do
    !
    ! Test a degenerate case.
    !
    call check (is_nil_list (list_reverse (.true.)), "is_nil_list (list_reverse (.true.)) failed")
  end subroutine test_list_reverse

  subroutine test_list_reverse_in_place
    type(cons_t) :: lst
    integer :: i
    lst = iota (15, 1)
    call list_reverse_in_place (lst)
    call check (list_length (lst) == 15, "list_length (lst) == 15 failed (for list_reverse_in_place)")
    do i = 1, 15
       call check (list_ref1 (lst, i) .eqi. (16 - i), "list_ref1 (lst, i) .eqi. (16 - i) failed (for list_reverse_in_place)")
    end do
  end subroutine test_list_reverse_in_place

  subroutine test_list_copy
    type(cons_t) :: lst1, lst2, lst3, lst4
    integer :: i
    lst1 = iota (15, 1)
    lst2 = cons_t_cast (list_copy (lst1))
    call set_car (lst1, nil_list)
    call set_cdr (lst1, nil_list)
    call check (list_length (lst1) == 1, "list_length (lst1) == 1 failed (for list_copy)")
    call check (is_nil_list (car (lst1)), "is_nil_list (car (lst1)) failed (for list_copy)")
    call check (list_length (lst2) == 15, "list_length (lst2) == 15 failed (for list_copy)")
    do i = 1, 15
       call check (list_ref1 (lst2, i) .eqi. i, "list_ref1 (lst2, i) .eqi. i failed (for list_copy)")
    end do
    call check (is_nil_list (list_copy (nil_list)), "is_nil_list (list_copy (nil_list)) failed")
    !
    ! Now do a dotted list.
    !
    lst3 = cons (1, 2)
    lst4 = cons_t_cast (list_copy (lst3))
    call check (car (lst4) .eqi. 1, "car (lst4) .eqi. 1 failed (for list_copy)")
    call check (cdr (lst4) .eqi. 2, "cdr (lst4) .eqi. 2 failed (for list_copy)")
    !
    ! Now do a degenerate dotted list.
    !
    call check (list_copy (123.0) .eqr. 123.0, "list_copy (123.0) .eqr. 123.0 failed")
  end subroutine test_list_copy

  subroutine test_list_take
    type(cons_t) :: lst1, lst2, lst3, lst4
    integer :: i
    call check (is_nil_list (list_take (nil_list, 0)), "is_nil_list (list_take (nil_list, 0)) failed")
    lst1 = iota (15, 1)
    call check (is_nil_list (list_take (lst1, 0)), "is_nil_list (list_take (lst1, 0)) failed")
    lst2 = list_take (lst1, 10)
    call check (list_length (lst2) == 10, "list_length (lst2) == 10 failed (for list_take)")
    do i = 1, 10
       call check (list_ref1 (lst2, i) .eqi. i, "list_ref1 (lst2, i) .eqi. i failed (for list_take)")
    end do
    lst3 = list_take (lst1, 15)
    call check (list_length (lst3) == 15, "list_length (lst3) == 15 failed (for list_take)")
    do i = 1, 15
       call check (list_ref1 (lst3, i) .eqi. i, "list_ref1 (lst3, i) .eqi. i failed (for list_take)")
    end do
    lst4 = list_take (circular_list (iota (4)), 100)
    call check (list_length (lst4) == 100, "list_length (lst4) == 100 failed (for list_take)")
    do i = 0, 99
       call check (list_ref0 (lst4, i) .eqi. mod (i, 4), "list_ref0 (lst4, i) .eqi. mod (i, 4) failed (for list_take)")
    end do
    !
    ! Let us check a degenerate case.
    !
    call check (is_nil_list (list_take (123, 0)), "is_nil_list (list_take (123, 0)) failed")
  end subroutine test_list_take

  subroutine test_list_drop
    type(cons_t) :: lst1, lst2, lst3, lst4, lst6, lst7
    class(*), allocatable :: obj5
    integer :: i
    call check (is_nil_list (list_drop (nil_list, 0)), "is_nil_list (list_drop (nil_list, 0)) failed")
    lst1 = iota (15, 1)
    call check (is_nil_list (list_drop (lst1, 15)), "is_nil_list (list_drop (lst1, 15)) failed")
    lst2 = cons_t_cast (list_drop (lst1, 0))
    call check (list_length (lst2) == 15, "list_length (lst2) == 15 failed (for list_drop)")
    do i = 1, 15
       call check (list_ref1 (lst2, i) .eqi. i, "list_ref1 (lst2, i) .eqi. i failed (for list_drop)")
    end do
    lst3 = cons_t_cast (list_drop (lst1, 10))
    call check (list_length (lst3) == 5, "list_length (lst3) == 5 failed (for list_drop)")
    do i = 1, 5
       call check (list_ref1 (lst3, i) .eqi. (10 + i), "list_ref1 (lst3, i) .eqi. (10 + i) failed (for list_drop)")
    end do
    lst4 = cons_t_cast (list_drop (1 ** 2 ** cons (3, 4), 2))
    call check (car (lst4) .eqi. 3, "car (lst4) .eqi. 3 failed (for list_drop)")
    call check (cdr (lst4) .eqi. 4, "cdr (lst4) .eqi. 4 failed (for list_drop)")
    obj5 = list_drop (1 ** 2 ** cons (3, 4), 3)
    call check (obj5 .eqi. 4, "obj5 .eqi. 4 failed (for list_drop)")
    lst6 = cons_t_cast (list_drop (circular_list (2 ** 3 ** 0 ** 1 ** nil_list), 2))
    call check (is_circular_list (lst6), "is_circular_list (lst6) failed (for list_drop)")
    do i = 0, 99
       call check (list_ref0 (lst6, i) .eqi. mod (i, 4), "list_ref0 (lst6, i) .eqi. mod (i, 4) failed (for list_drop)")
    end do
    lst7 = cons_t_cast (list_drop (circular_list (2 ** 3 ** 0 ** 1 ** nil_list), (1000 * 4) + 2))
    call check (is_circular_list (lst7), "is_circular_list (lst7) failed (for list_drop)")
    do i = 0, 99
       call check (list_ref0 (lst7, i) .eqi. mod (i, 4), "list_ref0 (lst7, i) .eqi. mod (i, 4) failed (for list_drop)")
    end do
    !
    ! Let us check a degenerate case.
    !
    call check (list_drop (123, 0) .eqi. 123, "list_drop (123, 0) .eqi. 123 failed")
  end subroutine test_list_drop

  subroutine test_list_take_right
    type(cons_t) :: lst1, lst2, lst3
    integer :: i
    call check (is_nil_list (list_take_right (nil_list, 0)), "is_nil_list (list_take_right (nil_list, 0)) failed")
    lst1 = iota (15, 1)
    call check (is_nil_list (list_take_right (lst1, 0)), "is_nil_list (list_take_right (lst1, 0)) failed")
    lst2 = cons_t_cast (list_take_right (lst1, 10))
    call check (list_length (lst2) == 10, "list_length (lst2) == 10 failed (for list_take_right)")
    do i = 1, 10
       call check (list_ref1 (lst2, i) .eqi. (5 + i), "list_ref1 (lst2, i) .eqi. (5 + i) failed (for list_drop)")
    end do
    lst3 = cons_t_cast (list_take_right (lst1, 15))
    call check (list_length (lst3) == 15, "list_length (lst3) == 15 failed (for list_take_right)")
    do i = 1, 15
       call check (list_ref1 (lst3, i) .eqi. i, "list_ref1 (lst3, i) .eqi. i failed (for list_drop)")
    end do
    call check (list_take_right (1 ** cons (1, 2), 0) .eqi. 2, "list_take_right (1 ** cons (1, 2), 0) .eqi. 2 failed")
    call check (list_take_right (cons (1, 2), 0) .eqi. 2, "list_take_right (cons (1, 2), 0) .eqi. 2 failed")
    !
    ! Let us check a degenerate case.
    !
    call check (list_take_right (123, 0) .eqi. 123, "list_take_right (123, 0) .eqi. 123 failed")
  end subroutine test_list_take_right

  subroutine test_list_drop_right
    type(cons_t) :: lst1, lst2, lst3, lst4
    integer :: i
    call check (is_nil_list (list_drop_right (nil_list, 0)), "is_nil_list (list_drop_right (nil_list, 0)) failed")
    lst1 = iota (15, 1)
    call check (is_nil_list (list_drop_right (lst1, 15)), "is_nil_list (list_drop_right (lst1, 15)) failed")
    lst2 = cons_t_cast (list_drop_right (lst1, 10))
    call check (list_length (lst2) == 5, "list_length (lst2) == 5 failed (for list_drop_right)")
    do i = 1, 5
       call check (list_ref1 (lst2, i) .eqi. i, "list_ref1 (lst2, i) .eqi. i failed (for list_drop_right)")
    end do
    lst3 = list_drop_right (lst1, 0)
    call check (list_length (lst3) == 15, "list_length (lst3) == 15 failed (for list_drop_right)")
    do i = 1, 15
       call check (list_ref1 (lst3, i) .eqi. i, "list_ref1 (lst3, i) .eqi. i failed (for list_drop_right)")
    end do
    lst4 = list_drop_right (0.0 ** cons (1, 2), 1)
    call check (list_length (lst4) == 1, "list_length (lst4) == 1 failed (for list_drop_right)")
    call check (car (lst4) .eqr. 0.0, "car (lst4) .eqr. 0.0 (for list_drop_right)")
    call check (is_nil_list (list_drop_right (cons (1, 2), 1)), "is_nil_list (list_drop_right (cons (1, 2), 1)) failed")
    !
    ! Let us check a degenerate case.
    !
    call check (is_nil_list (list_drop_right (123, 0)), "is_nil_list (list_drop_right (123, 0)) failed")
  end subroutine test_list_drop_right

  subroutine test_list_split
    !
    ! FIXME: For list_split, add tests that you can output to the same
    !        variables as were used for inputs.
    !
    type(cons_t) :: left
    class(*), allocatable :: right
    integer :: i
    call list_split (nil_list, 0, left, right)
    call check (is_nil_list (left), "check0010 failed for list_split")
    call check (is_nil_list (right), "check0020 failed for list_split")
    call list_split (1 ** 2 ** 3 ** nil_list, 0, left, right)
    call check (is_nil_list (left), "check0030 failed for list_split")
    call check (list_length (cons_t_cast (right)) == 3, "check0040 failed for list_split")
    do i = 1, 3
       call check (list_ref1 (cons_t_cast (right), i) .eqi. i, "check0050 failed for list_split")
    end do
    call list_split (1 ** 2 ** 3 ** nil_list, 1, left, right)
    call check (list_length (left) == 1, "check0060 failed for list_split")
    do i = 1, 1
       call check (list_ref1 (left, i) .eqi. i, "check0070 failed for list_split")
    end do
    call check (list_length (cons_t_cast (right)) == 2, "check0080 failed for list_split")
    do i = 1, 2
       call check (list_ref1 (cons_t_cast (right), i) .eqi. (1 + i), "check0090 failed for list_split")
    end do
    call list_split (1 ** 2 ** 3 ** nil_list, 2, left, right)
    call check (list_length (left) == 2, "check0100 failed for list_split")
    do i = 1, 2
       call check (list_ref1 (left, i) .eqi. i, "check0110 failed for list_split")
    end do
    call check (list_length (cons_t_cast (right)) == 1, "check0120 failed for list_split")
    do i = 1, 1
       call check (list_ref1 (cons_t_cast (right), i) .eqi. (2 + i), "check0130 failed for list_split")
    end do
    call list_split (1 ** 2 ** 3 ** nil_list, 3, left, right)
    call check (list_length (left) == 3, "check0140 failed for list_split")
    do i = 1, 3
       call check (list_ref1 (left, i) .eqi. i, "check0150 failed for list_split")
    end do
    call check (is_nil_list (right), "check0160 failed for list_split")
    call list_split (cons (1, 2), 1, left, right)
    call check (list_length (left) == 1, "check0170 failed for list_split")
    call check (car (left) .eqi. 1, "check0180 failed for list_split")
    call check (right .eqi. 2, "check0190 failed for list_split")
    call list_split (circular_list (0 ** 1 ** 2 ** 3 ** nil_list), 6, left, right)
    call check (list_length (left) == 6, "check0200 failed for list_split")
    do i = 0, 5
       call check (list_ref0 (left, i) .eqi. mod (i, 4), "check0210 failed for list_split")
    end do
    call check (is_circular_list (right), "check0220 failed for list_split")
    do i = 0, 99
       call check (list_ref0 (cons_t_cast (right), i) .eqi. mod (2 + i, 4), "check0230 failed for list_split")
    end do
    !
    ! Let us check a degenerate case.
    !
    call list_split (123, 0, left, right)
    call check (is_nil_list (left), "check0240 failed for list_split")
    call check (right .eqi. 123, "check0250 failed for list_split")
  end subroutine test_list_split

  subroutine test_list_append
    type(cons_t) :: lst1, lst2
    integer :: i
    lst1 = cons_t_cast (list_append (nil_list, 1 ** 2 ** 3 ** nil_list))
    call check (list_length (lst1) == 3, "list_length (lst1) == 3 failed (for list_append)")
    do i = 1, 3
       call check (list_ref1 (lst1, i) .eqi. i, "list_ref1 (lst1, i) .eqi. i (for list_append)")
    end do
    lst2 = cons_t_cast (list_append (1 ** 2 ** 3 ** nil_list, 4 ** 5 ** 6 ** nil_list))
    call check (list_length (lst2) == 6, "list_length (lst2) == 6 failed (for list_append)")
    do i = 1, 6
       call check (list_ref1 (lst2, i) .eqi. i, "list_ref1 (lst2, i) .eqi. i (for list_append)")
    end do
    !
    ! Let us check a degenerate case.
    !
    call check (list_append (5, 6) .eqi. 6, "list_append (5, 6) .eqi. 6 failed")
  end subroutine test_list_append

  subroutine test_list_append_reverse
    type(cons_t) :: lst
    integer :: i
    lst = cons_t_cast (list_append_reverse (3 ** 2 ** 1 ** nil_list, 4 ** 5 ** 6 ** nil_list))
    call check (list_length (lst) == 6, "list_length (lst) == 6 failed (for list_append_reverse)")
    do i = 1, 6
       call check (list_ref1 (lst, i) .eqi. i, "list_ref1 (lst, i) .eqi. i (for list_append_reverse)")
    end do
    !
    ! Let us check a degenerate case.
    !
    call check (list_append_reverse (5, 6) .eqi. 6, "list_append_reverse (5, 6) .eqi. 6 failed")
  end subroutine test_list_append_reverse

  subroutine test_list_append_in_place
    !
    ! FIXME: Add a test to check for clobbered arguments.
    !
    type(cons_t) :: lst
    integer :: i
    lst = 1 ** 2 ** 3 ** nil_list
    call list_append_in_place (lst, 4 ** 5 ** 6 ** nil_list)
    call check (list_length (lst) == 6, "list_length (lst) == 6 failed (for list_append_in_place)")
    do i = 1, 6
       call check (list_ref1 (lst, i) .eqi. i, "list_ref1 (lst, i) .eqi. i (for list_append_in_place)")
    end do
  end subroutine test_list_append_in_place

  subroutine test_list_append_reverse_in_place
    !
    ! FIXME: Add a test to check for clobbered arguments.
    !
    type(cons_t) :: lst
    integer :: i
    lst = 3 ** 2 ** 1 ** nil_list
    call list_append_reverse_in_place (lst, 4 ** 5 ** 6 ** nil_list)
    call check (list_length (lst) == 6, "list_length (lst) == 6 failed (for list_append_reverse_in_place)")
    do i = 1, 6
       call check (list_ref1 (lst, i) .eqi. i, "list_ref1 (lst, i) .eqi. i (for list_append_reverse_in_place)")
    end do
  end subroutine test_list_append_reverse_in_place

  subroutine test_list_concatenate
    type(cons_t) :: lst1, lst2
    integer :: i
    call check (is_nil_list (list_concatenate (nil_list)), "is_nil_list (list_concatenate (nil_list)) failed")
    lst1 = cons_t_cast (list_concatenate ((1 ** 2 ** 3 ** nil_list) ** nil_list))
    call check (list_length (lst1) == 3, "list_length (lst1) == 3 failed (for list_concatenate)")
    do i = 1, 3
       call check (list_ref1 (lst1, i) .eqi. i, "list_ref1 (lst1, i) .eqi. i (for list_concatenate)")
    end do
    lst2 = cons_t_cast (list_concatenate &
         ((1 ** 2 ** 3 ** nil_list) &
         ** (4 ** 5 ** nil_list) &
         ** (6 ** nil_list) &
         ** (7 ** 8 ** 9 ** nil_list) &
         ** (10 ** nil_list) &
         ** nil_list))
    call check (list_length (lst2) == 10, "list_length (lst2) == 10 failed (for list_concatenate)")
    do i = 1, 10
       call check (list_ref1 (lst2, i) .eqi. i, "list_ref1 (lst2, i) .eqi. i (for list_concatenate)")
    end do
  end subroutine test_list_concatenate

  subroutine test_list_zip
    type(cons_t) :: lst1, lst2
    call check (is_nil_list (list_zip (nil_list)), "is_nil_list (list_zip (nil_list)) failed")
    call check (is_nil_list (list_zip (((1 ** nil_list) ** nil_list ** (2 ** 3 ** nil_list) ** nil_list))), &
         "is_nil_list (list_zip (((1 ** nil_list) ** nil_list ** (2 ** 3 ** nil_list) ** nil_list))) failed")
    call check (list_length (list_zip (((123 ** nil_list) ** nil_list))) == 1, &
         "list_length (list_zip (((123 ** nil_list) ** nil_list))) == 1 failed")
    call check (caar (list_zip (((123 ** nil_list) ** nil_list))) .eqi. 123, &
         "caar (list_zip (((123 ** nil_list) ** nil_list))) .eqi. 123 failed")
    call check (is_nil_list (cdar (list_zip (((123 ** nil_list) ** nil_list)))), &
         "is_nil_list (cdar (list_zip (((123 ** nil_list) ** nil_list)))) failed")
    lst1 = (123 ** 456 ** 789 ** nil_list) &
         ** (1.0 ** 2.0 ** 3.0 ** 4.0 ** nil_list) &
         ** (5.0 ** 6.0 ** 7.0 ** nil_list) &
         ** nil_list
    call check (caar (lst1) .eqi. 123, "caar (lst1) .eqi. 123 failed (for list_zip)")
    call check (cadar (lst1) .eqi. 456, "cadar (lst1) .eqi. 456 failed (for list_zip)")
    call check (caddar (lst1) .eqi. 789, "caddar (lst1) .eqi. 789 failed (for list_zip)")
    call check (caar (cdr (lst1)) .eqr. 1.0, "caar (cdr (lst1)) .eqr. 1.0 failed (for list_zip)")
    call check (cadar (cdr (lst1)) .eqr. 2.0, "cadar (cdr (lst1)) .eqr. 2.0 failed (for list_zip)")
    call check (caddar (cdr (lst1)) .eqr. 3.0, "caddar (cdr (lst1)) .eqr. 3.0 failed (for list_zip)")
    call check (cadddr (cadr (lst1)) .eqr. 4.0, "caddd (cadr (lst1)) .eqr. 4.0 failed (for list_zip)")
    call check (caar (cddr (lst1)) .eqr. 5.0, "caar (cddr (lst1)) .eqr. 5.0 failed (for list_zip)")
    call check (cadar (cddr (lst1)) .eqr. 6.0, "cadar (cddr (lst1)) .eqr. 6.0 failed (for list_zip)")
    call check (caddar (cddr (lst1)) .eqr. 7.0, "caddar (cddr (lst1)) .eqr. 7.0 failed (for list_zip)")
    lst2 = list_zip (lst1)
    call check (list_length (lst2) == 3, "list_length (lst2) == 3 failed (for list_zip)")
    call check (list_length (car (lst2)) == 3, "list_length (car (lst2)) == 3 failed (for list_zip)")
    call check (list_length (cadr (lst2)) == 3, "list_length (cadr (lst2)) == 3 failed (for list_zip)")
    call check (list_length (caddr (lst2)) == 3, "list_length (caddr (lst2)) == 3 failed (for list_zip)")
    call check (first (first (lst2)) .eqi. 123, "first (first (lst2)) .eqi. 123 failed (for list_zip)")
    call check (second (first (lst2)) .eqr. 1.0, "second (first (lst2)) .eqr. 1.0 failed (for list_zip)")
    call check (third (first (lst2)) .eqr. 5.0, "third (first (lst2)) .eqr. 5.0 failed (for list_zip)")
    call check (first (second (lst2)) .eqi. 456, "first (second (lst2)) .eqi. 456 failed (for list_zip)")
    call check (second (second (lst2)) .eqr. 2.0, "second (second (lst2)) .eqr. 2.0 failed (for list_zip)")
    call check (third (second (lst2)) .eqr. 6.0, "third (second (lst2)) .eqr. 6.0 failed (for list_zip)")
    call check (first (third (lst2)) .eqi. 789, "first (third (lst2)) .eqi. 789 failed (for list_zip)")
    call check (second (third (lst2)) .eqr. 3.0, "second (third (lst2)) .eqr. 3.0 failed (for list_zip)")
    call check (third (third (lst2)) .eqr. 7.0, "third (third (lst2)) .eqr. 7.0 failed (for list_zip)")
  end subroutine test_list_zip

  subroutine test_list_zip1
    type(cons_t) :: lst1, lst_z
    call check (is_nil_list (list_zip1 (nil_list)), "is_nil_list (list_zip1 (nil_list)) failed")
    lst1 = 1.0 ** 2 ** 3 ** nil_list
    lst_z = list_zip1 (lst1)
    call check (list_length (lst_z) == 3, "list_length (lst_z) == 3 failed (for list_zip1)")
    call check (list_length (car (lst_z)) == 1, "list_length (car (lst_z)) == 1 failed (for list_zip1)")
    call check (list_length (cadr (lst_z)) == 1, "list_length (cadr (lst_z)) == 1 failed (for list_zip1)")
    call check (list_length (caddr (lst_z)) == 1, "list_length (caddr (lst_z)) == 1 failed (for list_zip1)")
    call check (caar (lst_z) .eqr. 1.0, "caar (lst_z) .eqr. 1.0 failed (for list_zip1)")
    call check (caadr (lst_z) .eqi. 2, "caadr (lst_z) .eqi. 2 failed (for list_zip1)")
    call check (caaddr (lst_z) .eqi. 3, "caaddr (lst_z) .eqi. 3 failed (for list_zip1)")
  end subroutine test_list_zip1

  subroutine test_list_zip3
    type(cons_t) :: lst1, lst2, lst3, lst_z
    call check (is_nil_list (list_zip3 (1.0 ** nil_list, nil_list, 2 ** 3 ** nil_list)), &
         "is_nil_list (list_zip3 (1.0 ** nil_list, nil_list, 2 ** 3 ** nil_list)) failed")
    lst1 = 1.0 ** 2 ** 3 ** nil_list
    lst2 = circular_list (4 ** nil_list)
    lst3 = 5 ** 6 ** nil_list
    lst_z = list_zip3 (lst1, lst2, lst3)
    call check (list_length (lst_z) == 2, "list_length (lst_z) == 2 failed (for list_zip3)")
    call check (list_length (car (lst_z)) == 3, "list_length (car (lst_z)) == 3 failed (for list_zip3)")
    call check (list_length (cadr (lst_z)) == 3, "list_length (cadr (lst_z)) == 3 failed (for list_zip3)")
    call check (caar (lst_z) .eqr. 1.0, "caar (lst_z) .eqr. 1.0 failed (for list_zip3)")
    call check (cadar (lst_z) .eqi. 4, "cadar (lst_z) .eqi. 4 failed (for list_zip3)")
    call check (caddar (lst_z) .eqi. 5, "caddar (lst_z) .eqi. 5 failed (for list_zip3)")
    call check (caar (cdr (lst_z)) .eqi. 2, "caar (cdr (lst_z)) .eqi. 2 failed (for list_zip3)")
    call check (cadar (cdr (lst_z)) .eqi. 4, "cadar (cdr (lst_z)) .eqi. 4 failed (for list_zip3)")
    call check (caddar (cdr (lst_z)) .eqi. 6, "caddar (cdr (lst_z)) .eqi. 6 failed (for list_zip3)")
  end subroutine test_list_zip3

  subroutine test_list_unzip
    type(cons_t) :: lst1, lst2, lst3, lst4
    type(cons_t) :: lists1, lists2, lists3, lists4
    lst1 = list1 (1.0) ** list2 (2.0, 2.1) ** list1 (3.0) ** nil_list
    lists1 = list_unzip (lst1, 1)
    call check (list_length (lists1) == 1, "list_length (lists1) == 1 failed (for list_unzip)")
    call check (list_length (car (lists1)) == 3, "list_length (car (lists1)) == 3 failed (for list_unzip)")
    call check (car (car (lists1)) .eqr. 1.0, "car (car (lists1)) .eqr. 1.0 failed (for list_unzip)")
    call check (cadr (car (lists1)) .eqr. 2.0, "cadr (car (lists1)) .eqr. 2.0 failed (for list_unzip)")
    call check (caddr (car (lists1)) .eqr. 3.0, "caddr (car (lists1)) .eqr. 3.0 failed (for list_unzip)")
    lst2 = list3 (1, 2, 3) ** list2 (4, 5) ** list4 (6, 7, 8, 9) ** nil_list
    lists2 = list_unzip (lst2, 2)
    call check (list_length (lists2) == 2, "list_length (lists2) == 2 failed (for list_unzip)")
    call check (list_length (car (lists2)) == 3, "list_length (car (lists2)) == 3 failed (for list_unzip)")
    call check (list_length (cadr (lists2)) == 3, "list_length (cadr (lists2)) == 3 failed (for list_unzip)")
    call check (car (car (lists2)) .eqi. 1, "car (car (lists2)) .eqi. 1 failed (for list_unzip)")
    call check (cadr (car (lists2)) .eqi. 4, "cadr (car (lists2)) .eqi. 4 failed (for list_unzip)")
    call check (caddr (car (lists2)) .eqi. 6, "caddr (car (lists2)) .eqi. 6 failed (for list_unzip)")
    call check (car (cadr (lists2)) .eqi. 2, "car (cadr (lists2)) .eqi. 2 failed (for list_unzip)")
    call check (cadr (cadr (lists2)) .eqi. 5, "cadr (cadr (lists2)) .eqi. 5 failed (for list_unzip)")
    call check (caddr (cadr (lists2)) .eqi. 7, "caddr (cadr (lists2)) .eqi. 7 failed (for list_unzip)")
    lst3 = list3 (1, 2, 3) ** list3 (4, 5, 6) ** (7 ** circular_list (list1 (8))) ** nil_list
    lists3 = list_unzip (lst3, 3)
    call check (list_length (lists3) == 3, "list_length (lists3) == 3 failed (for list_unzip)")
    call check (list_length (car (lists3)) == 3, "list_length (car (lists3)) == 3 failed (for list_unzip)")
    call check (list_length (cadr (lists3)) == 3, "list_length (cadr (lists3)) == 3 failed (for list_unzip)")
    call check (list_length (caddr (lists3)) == 3, "list_length (caddr (lists3)) == 3 failed (for list_unzip)")
    call check (car (car (lists3)) .eqi. 1, "car (car (lists3)) .eqi. 1 failed (for list_unzip)")
    call check (cadr (car (lists3)) .eqi. 4, "cadr (car (lists3)) .eqi. 4 failed (for list_unzip)")
    call check (caddr (car (lists3)) .eqi. 7, "caddr (car (lists3)) .eqi. 7 failed (for list_unzip)")
    call check (car (cadr (lists3)) .eqi. 2, "car (cadr (lists3)) .eqi. 2 failed (for list_unzip)")
    call check (cadr (cadr (lists3)) .eqi. 5, "cadr (cadr (lists3)) .eqi. 5 failed (for list_unzip)")
    call check (caddr (cadr (lists3)) .eqi. 8, "caddr (cadr (lists3)) .eqi. 8 failed (for list_unzip)")
    call check (car (caddr (lists3)) .eqi. 3, "car (caddr (lists3)) .eqi. 3 failed (for list_unzip)")
    call check (cadr (caddr (lists3)) .eqi. 6, "cadr (caddr (lists3)) .eqi. 6 failed (for list_unzip)")
    call check (caddr (caddr (lists3)) .eqi. 8, "caddr (caddr (lists3)) .eqi. 8 failed (for list_unzip)")
    lst4 = nil_list
    lists4 = list_unzip (lst4, 4)
    call check (list_length (lists4) == 4, "list_length (lists4) == 4 failed (for list_unzip)")
    call check (is_nil_list (car (lists4)), "is_nil_list (car (lists4)) failed (for list_unzip)")
    call check (is_nil_list (cadr (lists4)), "is_nil_list (cadr (lists4)) failed (for list_unzip)")
    call check (is_nil_list (caddr (lists4)), "is_nil_list (caddr (lists4)) failed (for list_unzip)")
    call check (is_nil_list (cadddr (lists4)), "is_nil_list (cadddr (lists4)) failed (for list_unzip)")
  end subroutine test_list_unzip

  subroutine test_list_unzip1
    type(cons_t) :: lst_zipped, lst1
    lst_zipped = list1 (1.0) ** list2 (2.0, 2.1) ** list1 (3.0) ** nil_list
    call list_unzip1 (lst_zipped, lst1)
    call check (list_length (lst1) == 3, "list_length (lst1) == 3 failed (for list_unzip1)")
    call check (car (lst1) .eqr. 1.0, "car (lst1) .eqr. 1.0 failed (for list_unzip1)")
    call check (cadr (lst1) .eqr. 2.0, "cadr (lst1) .eqr. 2.0 failed (for list_unzip1)")
    call check (caddr (lst1) .eqr. 3.0, "caddr (lst1) .eqr. 3.0 failed (for list_unzip1)")
  end subroutine test_list_unzip1

  subroutine test_list_unzip2
    type(cons_t) :: lst_zipped, lst1, lst2
    lst_zipped = list3 (1, 2, 3) ** list2 (4, 5) ** list4 (6, 7, 8, 9) ** nil_list
    call list_unzip2 (lst_zipped, lst1, lst2)
    call check (list_length (lst1) == 3, "list_length (lst1) == 3 failed (for list_unzip2)")
    call check (list_length (lst2) == 3, "list_length (lst2) == 3 failed (for list_unzip2)")
    call check (car (lst1) .eqi. 1, "car (lst1) .eqi. 1 failed (for list_unzip2)")
    call check (cadr (lst1) .eqi. 4, "cadr (lst1) .eqi. 4 failed (for list_unzip2)")
    call check (caddr (lst1) .eqi. 6, "caddr (lst1) .eqi. 6 failed (for list_unzip2)")
    call check (car (lst2) .eqi. 2, "car (lst2) .eqi. 2 failed (for list_unzip2)")
    call check (cadr (lst2) .eqi. 5, "cadr (lst2) .eqi. 5 failed (for list_unzip2)")
    call check (caddr (lst2) .eqi. 7, "caddr (lst2) .eqi. 7 failed (for list_unzip2)")
  end subroutine test_list_unzip2

  subroutine test_list_unzip3
    type(cons_t) :: lst_zipped, lst1, lst2, lst3
    lst_zipped = list3 (1, 2, 3) ** list3 (4, 5, 6) ** (7 ** circular_list (list1 (8))) ** nil_list
    call list_unzip3 (lst_zipped, lst1, lst2, lst3)
    call check (list_length (lst1) == 3, "list_length (lst1) == 3 failed (for list_unzip3)")
    call check (list_length (lst2) == 3, "list_length (lst2) == 3 failed (for list_unzip3)")
    call check (list_length (lst3) == 3, "list_length (lst3) == 3 failed (for list_unzip3)")
    call check (car (lst1) .eqi. 1, "car (lst1) .eqi. 1 failed (for list_unzip3)")
    call check (cadr (lst1) .eqi. 4, "cadr (lst1) .eqi. 4 failed (for list_unzip3)")
    call check (caddr (lst1) .eqi. 7, "caddr (lst1) .eqi. 7 failed (for list_unzip3)")
    call check (car (lst2) .eqi. 2, "car (lst2) .eqi. 2 failed (for list_unzip3)")
    call check (cadr (lst2) .eqi. 5, "cadr (lst2) .eqi. 5 failed (for list_unzip3)")
    call check (caddr (lst2) .eqi. 8, "caddr (lst2) .eqi. 8 failed (for list_unzip3)")
    call check (car (lst3) .eqi. 3, "car (lst3) .eqi. 3 failed (for list_unzip3)")
    call check (cadr (lst3) .eqi. 6, "cadr (lst3) .eqi. 6 failed (for list_unzip3)")
    call check (caddr (lst3) .eqi. 8, "caddr (lst3) .eqi. 8 failed (for list_unzip3)")
  end subroutine test_list_unzip3

  subroutine test_list_unzip4
    type(cons_t) :: lst_zipped, lst1, lst2, lst3, lst4
    lst_zipped = nil_list
    call list_unzip4 (lst_zipped, lst1, lst2, lst3, lst4)
    call check (is_nil_list (lst1), "is_nil_list (lst1) failed (for list_unzip4)")
    call check (is_nil_list (lst2), "is_nil_list (lst2) failed (for list_unzip4)")
    call check (is_nil_list (lst3), "is_nil_list (lst3) failed (for list_unzip4)")
    call check (is_nil_list (lst4), "is_nil_list (lst4) failed (for list_unzip4)")
  end subroutine test_list_unzip4

  subroutine test_list_map
    type(cons_t) :: lst_x, lst_y, inputs, outputs
    procedure(list_mapfunc_t), pointer :: cos_w => cosine_wrapper
    integer :: i
    class(*), allocatable :: head, tail
    !
    ! Try computing a list of cosines.
    !
    lst_x = acos (0.00) ** acos (0.25) ** acos (0.50) ** nil_list
    inputs = list_zip1 (lst_x)
    outputs = list_map (cosine_wrapper, inputs)
    call check (list_is_pair (outputs), "check0010 failed (for list_map)")
    call list_unzip1 (outputs, lst_y)
    call check (list_length (lst_y) == 3, "check0020 failed (for list_map)")
    call check (abs (0.00 - real_cast (first (lst_y))) < 0.0001, "check0030 failed (for list_map)")
    call check (abs (0.25 - real_cast (second (lst_y))) < 0.0001, "check0040 failed (for list_map)")
    call check (abs (0.50 - real_cast (third (lst_y))) < 0.0001, "check0050 failed (for list_map)")
    outputs = list_map (cos_w, inputs) ! Use a procedure pointer (cos_w).
    call check (list_is_pair (outputs), "check0060 failed (for list_map)")
    call list_unzip1 (outputs, lst_y)
    call check (list_length (lst_y) == 3, "check0070 failed (for list_map)")
    call check (abs (0.00 - real_cast (first (lst_y))) < 0.0001, "check0080 failed (for list_map)")
    call check (abs (0.25 - real_cast (second (lst_y))) < 0.0001, "check0090 failed (for list_map)")
    call check (abs (0.50 - real_cast (third (lst_y))) < 0.0001, "check0100 failed (for list_map)")
    !
    ! Try computing a list of results of euclidean division of
    ! positive integers by 2. (Aside: this would be a great example
    ! for lazy lists.)
    inputs = list_zip2 (iota (100), circular_list (2 ** nil_list))
    call check (list_length (inputs) == 100, "check0110 failed (for list_map)")
    outputs = list_map (division_wrapper, inputs)
    call check (list_length (outputs) == 100, "check0120 failed (for list_map)")
    tail = outputs
    do i = 0, 99
       call uncons (tail, head, tail)
       call check (first (head) .eqi. i / 2, "check0130 failed (for list_map)")
       call check (second (head) .eqi. mod (i, 2), "check0140 failed (for list_map)")
    end do
  end subroutine test_list_map

  subroutine run_tests
    !
    ! FIXME: Add a test for list_classify that checks it doesn't
    !        clobber its arguments.
    !
    call test_is_nil_list
    call test_is_cons_pair
    call test_list_is_nil
    call test_list_is_pair
    call test_cons_t_eq
    call test_uncons_car_cdr
    call test_list_cons
    call test_set_car_and_set_cdr
    call test_list_length
    call test_car_cadr_caddr_cadddr
    call test_first_second_etc
    call test_list_ref0
    call test_list_ref1
    call test_list_refn
    call test_list_last
    call test_list_last_pair
    call test_make_list
    call test_is_proper_list
    call test_is_dotted_list
    call test_is_circular_list
    call test_iota
    call test_circular_list
    call test_list_unlist
    call test_list_reverse
    call test_list_reverse_in_place
    call test_list_copy
    call test_list_take
    call test_list_drop
    call test_list_take_right
    call test_list_drop_right
    call test_list_split
    call test_list_append
    call test_list_append_reverse
    call test_list_append_in_place
    call test_list_append_reverse_in_place
    call test_list_concatenate
    call test_list_zip
    call test_list_zip1
    call test_list_zip3
    call test_list_unzip
    call test_list_unzip1
    call test_list_unzip2
    call test_list_unzip3
    call test_list_unzip4
    call test_list_map
  end subroutine run_tests

end module test__cons_lists

program main
  use test__cons_lists
  implicit none
  call run_tests
end program main
