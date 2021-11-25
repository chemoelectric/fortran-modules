! -*- F90 -*- 
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
    write (error_unit, '("test__cons_lists error: ", a)') msg
    call abort
  end subroutine error_abort

  subroutine check (boolean, msg)
    logical, intent(in) :: boolean
    character(*), intent(in) :: msg
    if (.not. boolean) call error_abort (msg)
  end subroutine check

  function assume_integer (obj) result (int)
    class(*), intent(in) :: obj
    integer :: int
    select type (obj)
    type is (integer)
       int = obj
    end select
  end function assume_integer

  function assume_real (obj) result (int)
    class(*), intent(in) :: obj
    real :: int
    select type (obj)
    type is (real)
       int = obj
    end select
  end function assume_real

  function integer_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = assume_integer (obj1) == assume_integer (obj2)
  end function integer_eq

  function real_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = assume_real (obj1) == assume_real (obj2)
  end function real_eq

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

  subroutine test_uncons_car_cdr
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
  end subroutine test_list_length

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
    call set_cdr (assume_list (cdddr (lst)), nil_list)
    call check (list_length (lst) == 4, "list_length (lst) == 4 failed (for circular_list)")
    do i = 0, 3
       call check (list_ref0 (lst, i) .eqi. i, "list_ref0 (lst, i) .eqi. i failed (for circular_list)")
    end do
  end subroutine test_circular_list

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
    lst2 = list_copy (lst1)
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
    lst3 = cons (1, 2)
    lst4 = list_copy (lst3)
    call check (car (lst4) .eqi. 1, "car (lst4) .eqi. 1 failed (for list_copy)")
    call check (cdr (lst4) .eqi. 2, "cdr (lst4) .eqi. 2 failed (for list_copy)")
  end subroutine test_list_copy

  subroutine run_tests
    call test_is_nil_list
    call test_is_cons_pair
    call test_list_is_nil
    call test_list_is_pair
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
    call test_iota
    call test_circular_list
    call test_list_reverse
    call test_list_reverse_in_place
    call test_list_copy
  end subroutine run_tests

end module test__cons_lists

program main
  use test__cons_lists
  implicit none
  call run_tests
end program main
