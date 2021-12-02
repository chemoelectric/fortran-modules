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

!!!
!!! WARNING: gfortran will generate trampolines for some kinds of
!!!          tests; these tests may not work on a hardened system.
!!!

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
    error stop
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

  function integer_lt (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = integer_cast (obj1) < integer_cast (obj2)
  end function integer_lt

  function real_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = real_cast (obj1) == real_cast (obj2)
  end function real_eq

  function integer_list_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = list_equals (integer_eq, obj1, obj2)
  end function integer_list_eq

  function cosine_func (x)
    class(*), intent(in) :: x
    class(*), pointer :: cosine_func
    allocate (cosine_func, source = cos (real_cast (x)))
  end function cosine_func

  subroutine cosine_subr (x)
    class(*), intent(inout), allocatable :: x
    x = cos (real_cast (x))
  end subroutine cosine_subr

  function is_positive_integer (x) result (bool)
    class(*), intent(in) :: x
    logical :: bool
    bool = .false.
    select type (x)
    type is (integer)
       bool = (1 <= x)
    end select
  end function is_positive_integer

  function is_not_positive_integer (x) result (bool)
    class(*), intent(in) :: x
    logical :: bool
    bool = .not. is_positive_integer (x)
  end function is_not_positive_integer
  
  function passthru_func (x)
    class(*), intent(in) :: x
    class(*), pointer :: passthru_func
    allocate (passthru_func, source = x)
  end function passthru_func

  subroutine passthru_subr (x)
    class(*), intent(inout), allocatable :: x
    continue
  end subroutine passthru_subr

  subroutine test_is_nil_or_pair
    call check (.not. is_nil_or_pair ('abc'), ".not. is_nil_or_pair ('abc') failed")
    call check (is_nil_or_pair (nil_list), "is_nil_or_pair (nil_list) failed")
    call check (is_nil_or_pair (iota (15)), "is_nil_or_pair (iota (15)) failed")
    call check (is_nil_or_pair (cons ('a', 'b')), "is_nil_or_pair (cons ('a', 'b')) failed")
  end subroutine test_is_nil_or_pair

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

  subroutine test_list_length_plus
    call check (list_length_plus (nil_list) == 0, "list_length_plus (nil_list) == 0 failed")
    call check (list_length_plus (1 ** nil_list) == 1, "list_length_plus (1 ** nil_list) == 1 failed")
    call check (list_length_plus (iota (20)) == 20, "list_length_plus (iota (20)) == 20 failed")
    call check (list_length_plus (make_list (200, 'a')) == 200, "list_length_plus (make_list (200, 'a')) == 200 failed")
    call check (list_length_plus (0.0 ** cons (1, 2)) == 2, "list_length_plus (0.0 ** cons (1, 2)) == 2 failed")
    call check (list_length_plus (cons (1, 2)) == 1, "list_length_plus (cons (1, 2)) == 1 failed")
    !
    ! A degenerate case.
    !
    call check (list_length_plus ('abc') == 0, "list_length_plus ('abc') == 0 failed")
    !
    ! FIXME: Test circular cases.
    !
    call check (list_length_plus (circular_list (iota (10))) == -1, &
         "list_length_plus (circular_list (iota (10))) == -1 failed")
    call check (list_length_plus (list_append (iota (10), circular_list (iota (10)))) == -1, &
         "list_length_plus (list_append (iota (10), circular_list (iota (10)))) == -1 failed")
  end subroutine test_list_length_plus

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
    call check (is_nil_list (list_append (nil_list, nil_list)), "is_nil_list (list_append (nil_list, nil_list)) failed")
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
    call check (is_nil_list (list_append_reverse (nil_list, nil_list)), &
         "is_nil_list (list_append_reverse (nil_list, nil_list)) failed")
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
    type(cons_t) :: lst1, lst2
    integer :: i
    lst1 = 1 ** 2 ** 3 ** nil_list
    call list_append_in_place (lst1, 4 ** 5 ** 6 ** nil_list)
    call check (list_length (lst1) == 6, "list_length (lst1) == 6 failed (for list_append_in_place)")
    do i = 1, 6
       call check (list_ref1 (lst1, i) .eqi. i, "list_ref1 (lst1, i) .eqi. i (for list_append_in_place)")
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

  subroutine test_list_zip1
    type(cons_t) :: lst1, lst1_z
    type(cons_t) :: lst2, lst2_z
    call check (is_nil_list (list_zip1 (nil_list)), "is_nil_list (list_zip1 (nil_list)) failed")
    lst1 = 1.0 ** 2 ** 3 ** nil_list
    lst1_z = list_zip1 (lst1)
    call check (list_length (lst1_z) == 3, "list_length (lst1_z) == 3 failed (for list_zip1)")
    call check (list_length (car (lst1_z)) == 1, "list_length (car (lst1_z)) == 1 failed (for list_zip1)")
    call check (list_length (cadr (lst1_z)) == 1, "list_length (cadr (lst1_z)) == 1 failed (for list_zip1)")
    call check (list_length (caddr (lst1_z)) == 1, "list_length (caddr (lst1_z)) == 1 failed (for list_zip1)")
    call check (caar (lst1_z) .eqr. 1.0, "caar (lst1_z) .eqr. 1.0 failed (for list_zip1)")
    call check (caadr (lst1_z) .eqi. 2, "caadr (lst1_z) .eqi. 2 failed (for list_zip1)")
    call check (caaddr (lst1_z) .eqi. 3, "caaddr (lst1_z) .eqi. 3 failed (for list_zip1)")
    lst2 = 123 ** nil_list
    lst2_z = list_zip1 (lst2)
    call check (list_length (lst2_z) == 1, "list_length (lst2_z) == 1 failed (for list_zip1)")
    call check (list_length (car (lst2_z)) == 1, "list_length (car (lst2_z)) == 1 failed (for list_zip1)")
    call check (caar (lst2_z) .eqi. 123, "caar (lst2_z) .eqi. 123 failed (for list_zip1)")
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

  subroutine test_list_unzip1
    type(cons_t) :: lst_zipped, lst1, lst2, lst3
    lst_zipped = list1 (1.0) ** list2 (2.0, 2.1) ** list1 (3.0) ** nil_list
    call list_unzip1 (lst_zipped, lst1)
    call check (list_length (lst1) == 3, "list_length (lst1) == 3 failed (for list_unzip1)")
    call check (car (lst1) .eqr. 1.0, "car (lst1) .eqr. 1.0 failed (for list_unzip1)")
    call check (cadr (lst1) .eqr. 2.0, "cadr (lst1) .eqr. 2.0 failed (for list_unzip1)")
    call check (caddr (lst1) .eqr. 3.0, "caddr (lst1) .eqr. 3.0 failed (for list_unzip1)")
    call list_unzip1 (nil_list, lst2)
    call check (is_nil_list (lst2), "is_nil_list (lst2) failed (for list_unzip1)")
    call list_unzip1 (1234, lst3)
    call check (is_nil_list (lst3), "is_nil_list (lst3) failed (for list_unzip1)")
  end subroutine test_list_unzip1

  subroutine test_list_unzip2
    type(cons_t) :: lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6
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
    call list_unzip2 (nil_list, lst3, lst4)
    call check (is_nil_list (lst3), "is_nil_list (lst3) failed (for list_unzip2)")
    call check (is_nil_list (lst4), "is_nil_list (lst4) failed (for list_unzip2)")
    call list_unzip2 ('abc', lst5, lst6)
    call check (is_nil_list (lst5), "is_nil_list (lst5) failed (for list_unzip2)")
    call check (is_nil_list (lst6), "is_nil_list (lst6) failed (for list_unzip2)")
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
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8
    call list_unzip4 (nil_list, lst1, lst2, lst3, lst4)
    call check (is_nil_list (lst1), "is_nil_list (lst1) failed (for list_unzip4)")
    call check (is_nil_list (lst2), "is_nil_list (lst2) failed (for list_unzip4)")
    call check (is_nil_list (lst3), "is_nil_list (lst3) failed (for list_unzip4)")
    call check (is_nil_list (lst4), "is_nil_list (lst4) failed (for list_unzip4)")
    call list_unzip4 (.true., lst5, lst6, lst7, lst8)
    call check (is_nil_list (lst5), "is_nil_list (lst5) failed (for list_unzip4)")
    call check (is_nil_list (lst6), "is_nil_list (lst6) failed (for list_unzip4)")
    call check (is_nil_list (lst7), "is_nil_list (lst7) failed (for list_unzip4)")
    call check (is_nil_list (lst8), "is_nil_list (lst8) failed (for list_unzip4)")
  end subroutine test_list_unzip4

  subroutine test_list_unzip1f
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6
    class(*), allocatable :: head, tail
    integer :: i
    lst1 = list_zip1 (iota (100, 1))
    call check (list_length (lst1) == 100, "list_length (lst1) == 100 failed (for list_unzip1f)")
    tail = lst1
    do i = 1, 100
       call uncons (tail, head, tail)
       call check (list_length (head) == 1, "list_length (head) == 1 failed (for list_unzip1f)")
       call check (car (head) .eqi. i, "car (head) .eqi. i failed (for list_unzip1f)")
    end do
    lst2 = list_unzip1f (lst1)
    call check (list_length (lst2) == 100, "list_length (lst2) == 100 failed (for list_unzip1f)")
    tail = lst2
    do i = 1, 100
       call uncons (tail, head, tail)
       call check (head .eqi. i, "head .eqi. i failed (for list_unzip1f)")
    end do
    lst3 = list_zip1 (nil_list)
    call check (is_nil_list (lst3), "is_nil_list (lst3) failed (for list_unzip1f)")
    lst4 = list_unzip1f (nil_list)
    call check (is_nil_list (lst4), "is_nil_list (lst4) failed (for list_unzip1f)")
    lst5 = list_zip1 (123)
    call check (is_nil_list (lst5), "is_nil_list (lst5) failed (for list_unzip1f)")
    lst6 = list_unzip1f (456)
    call check (is_nil_list (lst6), "is_nil_list (lst6) failed (for list_unzip1f)")
  end subroutine test_list_unzip1f

  subroutine test_list_foreach
    type(cons_t) :: lst1
    integer, dimension(1 : 100) :: arr1
    integer :: i
    lst1 = iota(100, 1)
    call list_foreach (side_effector, lst1)
    do i = 1, 100
       call check (arr1(i) == i, "arr1(i) == i failed (for list_foreach)")
    end do
  contains
    subroutine side_effector (x)
      !
      ! gfortran will generate a trampoline for this procedure.
      !
      class(*), intent(in) :: x
      integer :: i
      i = integer_cast (x)
      arr1(i) = i
    end subroutine side_effector
  end subroutine test_list_foreach

  subroutine test_list_pair_foreach
    type(cons_t) :: lst1
    type(cons_t), dimension(1 : 100) :: arr1
    integer :: i
    lst1 = iota(100, 1)
    !
    ! Convert the linked list to an array of length-1 lists.
    !
    call list_pair_foreach (side_effector, lst1)
    do i = 1, 100
       call check (car (arr1(i)) .eqi. i, "car (arr1(i)) .eqi. i failed (for list_pair_foreach)")
       call check (is_nil_list (cdr (arr1(i))), "is_nil_list (cdr (arr1(i))) failed (for list_pair_foreach)")
    end do
  contains
    subroutine side_effector (x)
      !
      ! gfortran will generate a trampoline for this procedure.
      !
      class(*), intent(in) :: x
      integer :: i
      type(cons_t) :: pair
      i = integer_cast (car (x))
      pair = cons_t_cast (x)
      call set_cdr (pair, nil_list)
      arr1(i) = pair
    end subroutine side_effector
  end subroutine test_list_pair_foreach

  subroutine test_list_map_elements
    type(cons_t) :: lst1, lst2, lst3, lst4
    integer :: i
    real :: x, y
    lst1 = acos (0.25) ** acos (0.50) ** acos (0.75) ** nil_list
    lst2 = cons_t_cast (list_map (cosine_func, lst1))
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001 failed (for list_map_elements)")
       call check (abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001 failed (for list_map_elements)")
    end do
    call check (is_nil_list (list_map (cosine_func, nil_list)), &
         "is_nil_list (list_map (cosine_func, nil_list)) failed")
    call check (list_map (cosine_func, 123) .eqi. 123, &
         "list_map (cosine_func, 123) .eqi. 123 failed")
    lst3 = acos (0.25) ** acos (0.50) ** cons (acos (0.75), 123)
    lst4 = cons_t_cast (list_map (cosine_func, lst3))
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001 failed (for list_map_elements)")
       call check (abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001 failed (for list_map_elements)")
    end do
    call check (cdr (list_last_pair (lst4)) .eqi. 123, &
         "cdr (list_last_pair (lst4)) .eqi. 123 failed (for list_map_elements)")
  end subroutine test_list_map_elements

  subroutine test_list_map_elements_in_place
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6
    class(*), allocatable :: obj1, obj2
    integer :: i
    real :: x, y
    lst1 = acos (0.25) ** acos (0.50) ** acos (0.75) ** nil_list
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001 failed (for list_map_elements_in_place)")
    end do
    lst2 = lst1
    call list_map_in_place (cosine_func, lst2)
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst1, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst1, i)) - y) < 0.0001 failed (for list_map_elements_in_place)")
       call check (abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001 failed (for list_map_elements_in_place)")
    end do
    !
    lst3 = acos (0.25) ** acos (0.50) ** cons (acos (0.75), 123)
    call check (cdr (list_last_pair (lst3)) .eqi. 123, &
         "cdr (list_last_pair (lst3)) .eqi. 123 failed (for list_map_elements_in_place)")
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001 failed (for list_map_elements_in_place)")
    end do
    lst4 = lst3
    call list_map_in_place (cosine_func, lst4)
    call check (cdr (list_last_pair (lst3)) .eqi. 123, &
         "cdr (list_last_pair (lst3)) .eqi. 123 failed (for list_map_elements_in_place)")
    call check (cdr (list_last_pair (lst4)) .eqi. 123, &
         "cdr (list_last_pair (lst4)) .eqi. 123 failed (for list_map_elements_in_place)")
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst3, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst3, i)) - y) < 0.0001 failed (for list_map_elements_in_place)")
       call check (abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001 failed (for list_map_elements_in_place)")
    end do
    !
    lst5 = nil_list
    lst6 = lst5
    call list_map_in_place (cosine_func, lst6)
    call check (is_nil_list (lst5), "is_nil_list (lst5) failed (for list_map_elements_in_place)")
    call check (is_nil_list (lst6), "is_nil_list (lst6) failed (for list_map_elements_in_place)")
    !
    obj1 = 123
    obj2 = obj1
    call list_map_in_place (cosine_func, obj2)
    call check (obj1 .eqi. 123, "obj1 .eqi. 123 failed (for list_map_elements_in_place)")
    call check (obj2 .eqi. 123, "obj2 .eqi. 123 failed (for list_map_elements_in_place)")
  end subroutine test_list_map_elements_in_place

  subroutine test_list_append_map_elements
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6
    class(*), allocatable :: obj1, obj2
    integer :: i
    lst1 = list_append_map (passthru_func, list3 (list2 (1, 2), list1(3), list2 (4, 5)))
    call check (list_length (lst1) == 5, "list_length (lst1) == 5 failed (for list_append_map_elements)")
    do i = 1, 5
       call check (list_ref1 (lst1, i) .eqi. i, "list_ref1 (lst1) == i failed (for list_append_map_elements)")
    end do
    lst2 = list_append_map (passthru_func, make_list (5, nil_list))
    call check (list_length (lst2) == 0, "list_length (lst2) == 0 failed (for list_append_map_elements)")
    lst3 = list_append_map (passthru_func, nil_list)
    call check (list_length (lst3) == 0, "list_length (lst3) == 0 failed (for list_append_map_elements)")
    lst4 = list_append_map (passthru_func, list2 (1, 2) ** cons (list1(3), 4.0))
    call check (list_length (lst4) == 3, "list_length (lst4) == 3 failed (for list_append_map_elements)")
    call check (car (lst4) .eqi. 1, "car (lst4) .eqi. 1 failed (for list_append_map_elements)")
    call check (cadr (lst4) .eqi. 2, "cadr (lst4) .eqi. 2 failed (for list_append_map_elements)")
    call check (caddr (lst4) .eqi. 3, "caddr (lst4) .eqi. 3 failed (for list_append_map_elements)")
    lst5 = list_append_map (passthru_func, 'abc')
    call check (list_length (lst5) == 0, "list_length (lst5) == 0 failed (for list_append_map_elements)")
    lst6 = list_append_map (passthru_func, make_list (100, nil_list))
    call check (list_length (lst6) == 0, "list_length (lst6) == 0 failed (for list_append_map_elements)")
  end subroutine test_list_append_map_elements

  subroutine test_list_modify_elements
    type(cons_t) :: lst1, lst2, lst3, lst4
    integer :: i
    real :: x, y
    lst1 = acos (0.25) ** acos (0.50) ** acos (0.75) ** nil_list
    lst2 = cons_t_cast (list_map (cosine_subr, lst1))
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001 failed (for list_modify_elements as list_map)")
       call check (abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001 failed (for list_modify_elements as list_map)")
    end do
    call check (is_nil_list (list_map (cosine_subr, nil_list)), &
         "is_nil_list (list_map (cosine_subr, nil_list)) failed")
    call check (list_map (cosine_subr, 123) .eqi. 123, &
         "list_map (cosine_subr, 123) .eqi. 123 failed")
    lst3 = acos (0.25) ** acos (0.50) ** cons (acos (0.75), 123)
    lst4 = cons_t_cast (list_map (cosine_subr, lst3))
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001 failed (for list_modify_elements as list_map)")
       call check (abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001 failed (for list_modify_elements as list_map)")
    end do
    call check (cdr (list_last_pair (lst4)) .eqi. 123, &
         "cdr (list_last_pair (lst4)) .eqi. 123 failed (for list_modify_elements as list_map)")
  end subroutine test_list_modify_elements

  subroutine test_list_modify_elements_in_place
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6
    class(*), allocatable :: obj1, obj2
    integer :: i
    real :: x, y
    lst1 = acos (0.25) ** acos (0.50) ** acos (0.75) ** nil_list
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst1, i)) - x) < 0.0001 failed (for list_modify_elements_in_place)")
    end do
    lst2 = lst1
    call list_map_in_place (cosine_subr, lst2)
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst1, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst1, i)) - y) < 0.0001 failed (for list_modify_elements_in_place)")
       call check (abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst2, i)) - y) < 0.0001 failed (for list_modify_elements_in_place)")
    end do
    !
    lst3 = acos (0.25) ** acos (0.50) ** cons (acos (0.75), 123)
    call check (cdr (list_last_pair (lst3)) .eqi. 123, &
         "cdr (list_last_pair (lst3)) .eqi. 123 failed (for list_modify_elements_in_place)")
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001, &
            "abs (real_cast (list_ref1 (lst3, i)) - x) < 0.0001 failed (for list_modify_elements_in_place)")
    end do
    lst4 = lst3
    call list_map_in_place (cosine_subr, lst4)
    call check (cdr (list_last_pair (lst3)) .eqi. 123, &
         "cdr (list_last_pair (lst3)) .eqi. 123 failed (for list_modify_elements_in_place)")
    call check (cdr (list_last_pair (lst4)) .eqi. 123, &
         "cdr (list_last_pair (lst4)) .eqi. 123 failed (for list_modify_elements_in_place)")
    do i = 1, 3
       y = i * 0.25
       x = acos (y)
       call check (abs (real_cast (list_ref1 (lst3, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst3, i)) - y) < 0.0001 failed (for list_modify_elements_in_place)")
       call check (abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001, &
            "abs (real_cast (list_ref1 (lst4, i)) - y) < 0.0001 failed (for list_modify_elements_in_place)")
    end do
    !
    lst5 = nil_list
    lst6 = lst5
    call list_map_in_place (cosine_subr, lst6)
    call check (is_nil_list (lst5), "is_nil_list (lst5) failed (for list_modify_elements_in_place)")
    call check (is_nil_list (lst6), "is_nil_list (lst6) failed (for list_modify_elements_in_place)")
    !
    obj1 = 123
    obj2 = obj1
    call list_map_in_place (cosine_subr, obj2)
    call check (obj1 .eqi. 123, "obj1 .eqi. 123 failed (for list_modify_elements_in_place)")
    call check (obj2 .eqi. 123, "obj2 .eqi. 123 failed (for list_modify_elements_in_place)")
  end subroutine test_list_modify_elements_in_place

  subroutine test_list_append_modify_elements
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6
    class(*), allocatable :: obj1, obj2
    integer :: i
    lst1 = list_append_map (passthru_subr, list3 (list2 (1, 2), list1(3), list2 (4, 5)))
    call check (list_length (lst1) == 5, "list_length (lst1) == 5 failed (for list_append_modify_elements)")
    do i = 1, 5
       call check (list_ref1 (lst1, i) .eqi. i, "list_ref1 (lst1) == i failed (for list_append_modify_elements)")
    end do
    lst2 = list_append_map (passthru_subr, make_list (5, nil_list))
    call check (list_length (lst2) == 0, "list_length (lst2) == 0 failed (for list_append_modify_elements)")
    lst3 = list_append_map (passthru_subr, nil_list)
    call check (list_length (lst3) == 0, "list_length (lst3) == 0 failed (for list_append_modify_elements)")
    lst4 = list_append_map (passthru_subr, list2 (1, 2) ** cons (list1(3), 4.0))
    call check (list_length (lst4) == 3, "list_length (lst4) == 3 failed (for list_append_modify_elements)")
    call check (car (lst4) .eqi. 1, "car (lst4) .eqi. 1 failed (for list_append_modify_elements)")
    call check (cadr (lst4) .eqi. 2, "cadr (lst4) .eqi. 2 failed (for list_append_modify_elements)")
    call check (caddr (lst4) .eqi. 3, "caddr (lst4) .eqi. 3 failed (for list_append_modify_elements)")
    lst5 = list_append_map (passthru_subr, 'abc')
    call check (list_length (lst5) == 0, "list_length (lst5) == 0 failed (for list_append_modify_elements)")
    lst6 = list_append_map (passthru_subr, make_list (100, nil_list))
    call check (list_length (lst6) == 0, "list_length (lst6) == 0 failed (for list_append_modify_elements)")
  end subroutine test_list_append_modify_elements

  subroutine test_list_find
    type(cons_t) :: lst1, lst2, lst3, lst4
    class(*), allocatable :: pseudo_lst5
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5
    logical :: match_found1,  match_found2, match_found3, match_found4, match_found5
    lst1 = 1.0 ** (-1) ** 17 ** 5 ** 7 ** nil_list
    call list_find (is_positive_integer, lst1, match_found1, obj1)
    call check (match_found1, "match_found1 failed (for list_find)")
    call check (obj1 .eqi. 17, "obj1 .eqi. 17 failed (for list_find)")
    lst2 = 1.0 ** (-1) ** 17 ** 5 ** cons (7, 1234.0)
    call list_find (is_positive_integer, lst2, match_found1, obj1)
    call check (match_found1, "match_found1 failed (for list_find)")
    call check (obj1 .eqi. 17, "obj1 .eqi. 17 failed (for list_find)")
    lst3 = 1.0 ** (-1) ** (-17) ** 'abc' ** (-7) ** nil_list
    obj3 = 1024
    call list_find (is_positive_integer, lst3, match_found3, obj3)
    call check (.not. match_found3, ".not. match_found3 failed (for list_find)")
    call check (obj3 .eqi. 1024, "obj3 .eqi. 1024 failed (for list_find)")
    lst4 = nil_list
    obj4 = 1024
    call list_find (is_positive_integer, lst4, match_found4, obj4)
    call check (.not. match_found4, ".not. match_found4 failed (for list_find)")
    call check (obj4 .eqi. 1024, "obj4 .eqi. 1024 failed (for list_find)")
    pseudo_lst5 = 'abc'
    obj5 = 1024
    call list_find (is_positive_integer, pseudo_lst5, match_found5, obj5)
    call check (.not. match_found5, ".not. match_found5 failed (for list_find)")
    call check (obj5 .eqi. 1024, "obj5 .eqi. 1025 failed (for list_find)")
  end subroutine test_list_find

  subroutine test_list_find_tail
    type(cons_t) :: lst1, lst2, lst3, lst4
    class(*), allocatable :: pseudo_lst5
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5
    logical :: match_found1,  match_found2, match_found3, match_found4, match_found5
    lst1 = 1.0 ** (-1) ** 17 ** 5 ** 7 ** nil_list
    call list_find_tail (is_positive_integer, lst1, match_found1, obj1)
    call check (match_found1, "match_found1 failed (for list_find_tail)")
    call check (list_length (obj1) == 3, "list_length (obj1) == 3 failed (for list_find_tail)")
    call check (first (obj1) .eqi. 17, "first (obj1) .eqi. 17 failed (for list_find_tail)")
    call check (second (obj1) .eqi. 5, "second (obj1) .eqi. 5 failed (for list_find_tail)")
    call check (third (obj1) .eqi. 7, "third (obj1) .eqi. 7 failed (for list_find_tail)")
    call check (is_nil_list (cdddr (obj1)), "is_nil_list (cdddr (obj1)) failed (for list_find_tail)")
    lst2 = 1.0 ** (-1) ** 17 ** 5 ** cons (7, 1234.0)
    call list_find_tail (is_positive_integer, lst2, match_found2, obj2)
    call check (match_found2, "match_found2 failed (for list_find_tail)")
    call check (list_length (obj2) == 3, "list_length (obj2) == 3 failed (for list_find_tail)")
    call check (first (obj2) .eqi. 17, "first (obj2) .eqi. 17 failed (for list_find_tail)")
    call check (second (obj2) .eqi. 5, "second (obj2) .eqi. 5 failed (for list_find_tail)")
    call check (third (obj2) .eqi. 7, "third (obj2) .eqi. 7 failed (for list_find_tail)")
    call check (cdddr (obj2) .eqr. 1234.0, "cdddr (obj2) .eqr. 1234.0 failed (for list_find_tail)")
    lst3 = 1.0 ** (-1) ** (-17) ** 'abc' ** (-7) ** nil_list
    obj3 = 1024
    call list_find_tail (is_positive_integer, lst3, match_found3, obj3)
    call check (.not. match_found3, ".not. match_found3 failed (for list_find_tail)")
    call check (obj3 .eqi. 1024, "obj3 .eqi. 1024 failed (for list_find_tail)")
    lst4 = nil_list
    obj4 = 1024
    call list_find_tail (is_positive_integer, lst4, match_found4, obj4)
    call check (.not. match_found4, ".not. match_found4 failed (for list_find_tail)")
    call check (obj4 .eqi. 1024, "obj4 .eqi. 1024 failed (for list_find_tail)")
    pseudo_lst5 = 'abc'
    obj5 = 1024
    call list_find_tail (is_positive_integer, pseudo_lst5, match_found5, obj5)
    call check (.not. match_found5, ".not. match_found5 failed (for list_find_tail)")
    call check (obj5 .eqi. 1024, "obj5 .eqi. 1025 failed (for list_find_tail)")
  end subroutine test_list_find_tail

  subroutine test_list_take_while
    type(cons_t) :: lst1, lst2, lst3
    type(cons_t) :: obj1, obj2, obj3
    lst1 = 1.0 ** (-1) ** 17 ** 5 ** 7 ** nil_list
    obj1 = list_take_while (is_not_positive_integer, lst1)
    call check (list_length (obj1) == 2, "list_length (obj1) == 2 failed (for list_take_while)")
    call check (first (obj1) .eqr. 1.0, "first (obj1) .eqr. 1.0 failed (for list_take_while)")
    call check (second (obj1) .eqi. (-1), "second (obj1) .eqi. (-1) failed (for list_take_while)")
    call check (is_nil_list (cddr (obj1)), "is_nil_list (cddr (obj1)) failed (for list_take_while)")
    lst2 = 1.0 ** (-1) ** 17 ** 5 ** cons (7, 1234.0)
    obj2 = list_take_while (is_not_positive_integer, lst2)
    call check (list_length (obj2) == 2, "list_length (obj2) == 2 failed (for list_take_while)")
    call check (first (obj2) .eqr. 1.0, "first (obj2) .eqr. 1.0 failed (for list_take_while)")
    call check (second (obj2) .eqi. (-1), "second (obj2) .eqi. (-1) failed (for list_take_while)")
    call check (is_nil_list (cddr (obj2)), "is_nil_list (cddr (obj2)) failed (for list_take_while)")
    lst3 = 1 ** (-1) ** (-17) ** (-4321) ** (-7) ** nil_list
    obj3 = list_take_while (is_not_positive_integer, lst3)
    call check (is_nil_list (obj3), "is_nil_list (obj3) failed (for list_take_while)")
    call check (is_nil_list (list_take_while (is_not_positive_integer, nil_list)), &
         "is_nil_list (list_take_while (is_not_positive_integer, nil_list)) failed (for list_take_while)")
    call check (is_nil_list (list_take_while (is_not_positive_integer, 1234)), &
         "is_nil_list (list_take_while (is_not_positive_integer, 1234)) failed (for list_take_while)")
  end subroutine test_list_take_while

  subroutine test_list_drop_while
    type(cons_t) :: lst1, lst2, lst3
    class(*), allocatable :: obj1, obj2, obj3
    lst1 = 1.0 ** (-1) ** 17 ** 5 ** 7 ** nil_list
    obj1 = list_drop_while (is_not_positive_integer, lst1)
    call check (list_length (obj1) == 3, "list_length (obj1) == 3 failed (for list_drop_while)")
    call check (first (obj1) .eqi. 17, "first (obj1) .eqi. 17 failed (for list_drop_while)")
    call check (second (obj1) .eqi. 5, "second (obj1) .eqi. 5 failed (for list_drop_while)")
    call check (third (obj1) .eqi. 7, "third (obj1) .eqi. 7 failed (for list_drop_while)")
    call check (is_nil_list (cdddr (obj1)), "is_nil_list (cdddr (obj1)) failed (for list_drop_while)")
    lst2 = 1.0 ** (-1) ** 17 ** 5 ** cons (7, 1234.0)
    obj2 = list_drop_while (is_not_positive_integer, lst2)
    call check (list_length (obj2) == 3, "list_length (obj2) == 3 failed (for list_drop_while)")
    call check (first (obj2) .eqi. 17, "first (obj2) .eqi. 17 failed (for list_drop_while)")
    call check (second (obj2) .eqi. 5, "second (obj2) .eqi. 5 failed (for list_drop_while)")
    call check (third (obj2) .eqi. 7, "third (obj2) .eqi. 7 failed (for list_drop_while)")
    call check (cdddr (obj2) .eqr. 1234.0, "cdddr (obj2) .eqr. 1234.0 failed")
    lst3 = 1.0 ** (-1) ** (-17) ** 'abc' ** (-7) ** nil_list
    obj3 = list_drop_while (is_not_positive_integer, lst3)
    call check (is_nil_list (obj3), "is_nil_list (obj3) failed (for list_drop_while)")
    call check (is_nil_list (list_drop_while (is_not_positive_integer, nil_list)), &
         "is_nil_list (list_drop_while (is_not_positive_integer, nil_list)) failed (for list_drop_while)")
    call check (list_drop_while (is_not_positive_integer, 1234) .eqi. 1234, &
         "list_drop_while (is_not_positive_integer, 1234) .eqi. 1234 failed (for list_drop_while)")
  end subroutine test_list_drop_while

  subroutine test_list_span
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5
    class(*), allocatable :: pseudo_lst6
    type(cons_t) :: obj1a, obj2a, obj3a, obj4a, obj5a, obj6a
    class(*), allocatable :: obj1b, obj2b, obj3b, obj4b, obj5b, obj6b
    lst1 = 1.0 ** (-1) ** 17 ** 5 ** 7 ** nil_list
    call list_span (is_not_positive_integer, lst1, obj1a, obj1b)
    call check (list_length (obj1a) == 2, "list_length (obj1a) == 2 failed (for list_span)")
    call check (first (obj1a) .eqr. 1.0, "first (obj1a) .eqr. 1.0 failed (for list_span)")
    call check (second (obj1a) .eqi. (-1), "second (obj1a) .eqi. (-1) failed (for list_span)")
    call check (is_nil_list (cddr (obj1a)), "is_nil_list (cddr (obj1a)) failed (for list_span)")
    call check (list_length (obj1b) == 3, "list_length (obj1b) == 3 failed (for list_span)")
    call check (first (obj1b) .eqi. 17, "first (obj1b) .eqi. 17 failed (for list_span)")
    call check (second (obj1b) .eqi. 5, "second (obj1b) .eqi. 5 failed (for list_span)")
    call check (third (obj1b) .eqi. 7, "third (obj1b) .eqi. 7 failed (for list_span)")
    call check (is_nil_list (cdddr (obj1b)), "is_nil_list (cdddr (obj1b)) failed (for list_span)")
    lst2 = 1.0 ** (-1) ** 17 ** 5 ** cons (7, 1234.0)
    call list_span (is_not_positive_integer, lst2, obj2a, obj2b)
    call check (list_length (obj2a) == 2, "list_length (obj2a) == 2 failed (for list_span)")
    call check (first (obj2a) .eqr. 1.0, "first (obj2a) .eqr. 1.0 failed (for list_span)")
    call check (second (obj2a) .eqi. (-1), "second (obj2a) .eqi. (-1) failed (for list_span)")
    call check (is_nil_list (cddr (obj2a)), "is_nil_list (cddr (obj2a)) failed (for list_span)")
    lst3 = 1 ** (-1) ** (-17) ** (-4321) ** (-7) ** nil_list
    call list_span (is_not_positive_integer, lst3, obj3a, obj3b)
    call check (is_nil_list (obj3a), "is_nil_list (obj3a) failed (for list_span)")
    lst4 = 1.0 ** 1 ** (-17) ** (-4321) ** (-7) ** nil_list
    call list_span (is_not_positive_integer, lst4, obj4a, obj4b)
    call check (list_length (obj4a) == 1, "list_length (obj4a) == 1 failed (for list_span)")
    call check (first (obj4a) .eqr. 1.0, "first (obj4a) .eqr. 1.0 failed (for list_span)")
    call check (list_length (obj4b) == 4, "list_length (obj4b) == 4  failed (for list_span)")
    call check (first (obj4b) .eqi. 1, "first (obj4b) .eqi. 1 failed (for list_span)")
    call check (second (obj4b) .eqi. (-17), "second (obj4b) .eqi. (-17) failed (for list_span)")
    lst5 = nil_list
    call list_span (is_not_positive_integer, lst5, obj5a, obj5b)
    call check (is_nil_list (obj5a), "is_nil_list (obj5a) failed (for list_span)")
    call check (is_nil_list (obj5b), "is_nil_list (obj5b) failed (for list_span)")
    pseudo_lst6 = 1234
    call list_span (is_not_positive_integer, pseudo_lst6, obj6a, obj6b)
    call check (is_nil_list (obj6a), "is_nil_list (obj6a) failed (for list_span)")
    call check (obj6b .eqi. 1234, "obj6b .eqi. 1234 failed (for list_span)")
  end subroutine test_list_span

  subroutine test_list_break
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5
    class(*), allocatable :: pseudo_lst6
    type(cons_t) :: obj1a, obj2a, obj3a, obj4a, obj5a, obj6a
    class(*), allocatable :: obj1b, obj2b, obj3b, obj4b, obj5b, obj6b
    lst1 = 1.0 ** (-1) ** 17 ** 5 ** 7 ** nil_list
    call list_break (is_positive_integer, lst1, obj1a, obj1b)
    call check (list_length (obj1a) == 2, "list_length (obj1a) == 2 failed (for list_break)")
    call check (first (obj1a) .eqr. 1.0, "first (obj1a) .eqr. 1.0 failed (for list_break)")
    call check (second (obj1a) .eqi. (-1), "second (obj1a) .eqi. (-1) failed (for list_break)")
    call check (is_nil_list (cddr (obj1a)), "is_nil_list (cddr (obj1a)) failed (for list_break)")
    call check (list_length (obj1b) == 3, "list_length (obj1b) == 3 failed (for list_break)")
    call check (first (obj1b) .eqi. 17, "first (obj1b) .eqi. 17 failed (for list_break)")
    call check (second (obj1b) .eqi. 5, "second (obj1b) .eqi. 5 failed (for list_break)")
    call check (third (obj1b) .eqi. 7, "third (obj1b) .eqi. 7 failed (for list_break)")
    call check (is_nil_list (cdddr (obj1b)), "is_nil_list (cdddr (obj1b)) failed (for list_break)")
    lst2 = 1.0 ** (-1) ** 17 ** 5 ** cons (7, 1234.0)
    call list_break (is_positive_integer, lst2, obj2a, obj2b)
    call check (list_length (obj2a) == 2, "list_length (obj2a) == 2 failed (for list_break)")
    call check (first (obj2a) .eqr. 1.0, "first (obj2a) .eqr. 1.0 failed (for list_break)")
    call check (second (obj2a) .eqi. (-1), "second (obj2a) .eqi. (-1) failed (for list_break)")
    call check (is_nil_list (cddr (obj2a)), "is_nil_list (cddr (obj2a)) failed (for list_break)")
    lst3 = 1 ** (-1) ** (-17) ** (-4321) ** (-7) ** nil_list
    call list_break (is_positive_integer, lst3, obj3a, obj3b)
    call check (is_nil_list (obj3a), "is_nil_list (obj3a) failed (for list_break)")
    lst4 = 1.0 ** 1 ** (-17) ** (-4321) ** (-7) ** nil_list
    call list_break (is_positive_integer, lst4, obj4a, obj4b)
    call check (list_length (obj4a) == 1, "list_length (obj4a) == 1 failed (for list_break)")
    call check (first (obj4a) .eqr. 1.0, "first (obj4a) .eqr. 1.0 failed (for list_break)")
    call check (list_length (obj4b) == 4, "list_length (obj4b) == 4  failed (for list_break)")
    call check (first (obj4b) .eqi. 1, "first (obj4b) .eqi. 1 failed (for list_break)")
    call check (second (obj4b) .eqi. (-17), "second (obj4b) .eqi. (-17) failed (for list_break)")
    lst5 = nil_list
    call list_break (is_positive_integer, lst5, obj5a, obj5b)
    call check (is_nil_list (obj5a), "is_nil_list (obj5a) failed (for list_break)")
    call check (is_nil_list (obj5b), "is_nil_list (obj5b) failed (for list_break)")
    pseudo_lst6 = 1234
    call list_break (is_positive_integer, pseudo_lst6, obj6a, obj6b)
    call check (is_nil_list (obj6a), "is_nil_list (obj6a) failed (for list_break)")
    call check (obj6b .eqi. 1234, "obj6b .eqi. 1234 failed (for list_break)")
  end subroutine test_list_break

  subroutine test_list_any
    call check (list_any (is_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)), &
         "list_any (is_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)) failed")
    call check (.not. list_any (is_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)), &
         ".not. list_any (is_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)) failed")
    call check (.not. list_any (is_positive_integer, nil_list), &
         ".not. list_any (is_positive_integer, nil_list) failed")
    call check (.not. list_any (is_positive_integer, 'abc'), &
         ".not. list_any (is_positive_integer, 'abc') failed")
  end subroutine test_list_any

  subroutine test_list_every
    call check (.not. list_every (is_not_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)), &
         ".not. list_every (is_not_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)) failed")
    call check (list_every (is_not_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)), &
         "list_every (is_not_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)) failed")
    call check (list_every (is_not_positive_integer, nil_list), &
         "list_every (is_not_positive_integer, nil_list) failed")
    call check (list_every (is_not_positive_integer, 'abc'), &
         "list_every (is_not_positive_integer, 'abc') failed")
  end subroutine test_list_every

  subroutine test_list_index0
    call check (list_index0 (is_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)) == 3, &
         "list_index0 (is_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)) == 3 failed")
    call check (list_index0 (is_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)) == -1, &
         "list_index0 (is_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)) == -1 failed")
    call check (list_index0 (is_positive_integer, nil_list) == -1, &
         "list_index0 (is_positive_integer, nil_list) == -1 failed")
    call check (list_index0 (is_positive_integer, 'abc') == -1, &
         "list_index0 (is_positive_integer, 'abc') == -1 failed")
  end subroutine test_list_index0

  subroutine test_list_index1
    call check (list_index1 (is_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)) == 4, &
         "list_index1 (is_positive_integer, list5 (1.0, 3.0, 'abc', 1, .true.)) == 4 failed")
    call check (list_index1 (is_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)) == 0, &
         "list_index1 (is_positive_integer, list5 (1.0, 3.0, 'abc', -1, .true.)) == 0 failed")
    call check (list_index1 (is_positive_integer, nil_list) == 0, &
         "list_index1 (is_positive_integer, nil_list) == 0 failed")
    call check (list_index1 (is_positive_integer, 'abc') == 0, &
         "list_index1 (is_positive_integer, 'abc') == 0 failed")
  end subroutine test_list_index1

  subroutine test_list_equals
    call check (list_equals (integer_eq, nil_list, nil_list), &
         "list_equals (integer_eq, nil_list, nil_list) failed")
    call check (list_equals (real_eq, nil_list, nil_list), &
         "list_equals (real_eq, nil_list, nil_list) failed")
    call check (.not. list_equals (integer_eq, nil_list, 1 ** nil_list), &
         ".not. list_equals (integer_eq, nil_list, 1 ** nil_list) failed")
    call check (.not. list_equals (real_eq, nil_list, 1.0 ** nil_list), &
         ".not. list_equals (real_eq, nil_list, 1.0 ** nil_list) failed")
    call check (.not. list_equals (integer_eq, 1 ** nil_list, nil_list), &
         ".not. list_equals (integer_eq, 1 ** nil_list, nil_list) failed")
    call check (.not. list_equals (real_eq, 1.0 ** nil_list, nil_list), &
         ".not. list_equals (real_eq, 1.0 ** nil_list, nil_list) failed")
    call check (list_equals (integer_eq, 1 ** nil_list, 1 ** nil_list), &
         "list_equals (integer_eq, 1 ** nil_list, 1 ** nil_list) failed")
    call check (list_equals (real_eq, 1.0 ** nil_list, 1.0 ** nil_list), &
         "list_equals (real_eq, 1.0 ** nil_list, 1.0 ** nil_list) failed")
    call check (.not. list_equals (integer_eq, 1 ** nil_list, 2 ** nil_list), &
         ".not. list_equals (integer_eq, 1 ** nil_list, 2 ** nil_list) failed")
    call check (.not. list_equals (real_eq, 1.0 ** nil_list, 2.0 ** nil_list), &
         ".not. list_equals (real_eq, 1.0 ** nil_list, 2.0 ** nil_list) failed")
    call check (list_equals (integer_eq, iota (100), iota (100)), &
         "list_equals (integer_eq, iota (100), iota (100)) failed")
    call check (.not. list_equals (integer_eq, iota (100), iota (120)), &
         ".not. list_equals (integer_eq, iota (100), iota (120)) failed")
    call check (list_equals (integer_eq, iota (100), list_append (iota (50), iota (50, 50))), &
         "list_equals (integer_eq, iota (100), list_append (iota (50), iota (50, 50))) failed")
    call check (.not. list_equals (integer_eq, iota (100), list_append (iota (50), iota (50, 51))), &
         ".not. list_equals (integer_eq, iota (100), list_append (iota (50), iota (50, 51))) failed")
    !
    ! Now use list_equals to test for less-than. (The name
    ! `list_equals' is really a misnomer.)
    !
    call check (list_equals (integer_lt, iota (100), iota (100, 1)), &
         "list_equals (integer_lt, iota (100), iota (100, 1)) failed")
    call check (.not. list_equals (integer_lt, iota (100), list_append (iota (99, 1), list1 (-1))), &
         ".not. list_equals (integer_lt, iota (100), list_append (iota (99, 1), list1 (-1))) failed")
    !
    ! Try a recursion.
    !
    call check (list_equals (integer_list_eq, list_zip1 (iota (100)), list_zip1 (iota (100))), &
         "list_equals (integer_list_eq, list_zip1 (iota (100)), list_zip1 (iota (100))) failed")
  end subroutine test_list_equals

  subroutine test_list_count
    call check (list_count (is_positive_integer, nil_list) == 0, "list_count (is_positive_integer, nil_list) == 0 failed")
    call check (list_count (is_positive_integer, 1234) == 0, "list_count (is_positive_integer, 1234) == 0 failed")
    call check (list_count (is_positive_integer, iota (100, -50)) == 49, &
         "list_count (is_positive_integer, iota (100, -50)) == 49 failed")
    call check (list_count (is_positive_integer, 'abc' ** 3 ** 2.0 ** (-5) ** 6 ** nil_list) == 2, &
         "list_count (is_positive_integer, 'abc' ** 3 ** 2.0 ** (-5) ** 6 ** nil_list) == 2 failed")
    call check (list_count (is_positive_integer, 'abc' ** 3 ** 2.0 ** (-5) ** cons (6, 'xyz')) == 2, &
         "list_count (is_positive_integer, 'abc' ** 3 ** 2.0 ** (-5) ** cons (6, 'xyz')) == 2 failed")
  end subroutine test_list_count

  subroutine test_list_filter
    !
    ! FIXME: More test cases are needed. I have already had bugs slip
    !        past these.
    !
    type(cons_t) :: lst1a, lst1b
    type(cons_t) :: lst2a, lst2b
    call check (list_equals (integer_eq, list_filter (is_positive_integer, nil_list), nil_list), &
         "list_equals (list_filter (is_positive_integer, nil_list), nil_list) failed")
    call check (list_filter (is_positive_integer, 1234.0) .eqr. 1234.0, &
         "list_filter (is_positive_integer, 1234.0) .eqr. 1234.0 failed")
    call check (list_equals (integer_eq, list_filter (is_positive_integer, 1 ** nil_list), 1 ** nil_list), &
         "list_equals (integer_eq, list_filter (is_positive_integer, 1 ** nil_list), 1 ** nil_list) failed")
    call check (list_equals (integer_eq, list_filter (is_positive_integer, 1 ** 2 ** nil_list), 1 ** 2 ** nil_list), &
         "list_equals (integer_eq, list_filter (is_positive_integer, 1 ** 2 ** nil_list), 1 ** 2 ** nil_list) failed")
    call check (list_equals (integer_eq, list_filter (is_positive_integer, 1 ** (-2) ** nil_list), 1 ** nil_list), &
         "list_equals (integer_eq, list_filter (is_positive_integer, 1 ** (-2) ** nil_list), 1 ** nil_list) failed")
    call check (list_equals (integer_eq, list_filter (is_positive_integer, (-1) ** 2 ** nil_list), 2 ** nil_list), &
         "list_equals (integer_eq, list_filter (is_positive_integer, (-1) ** 2 ** nil_list), 2 ** nil_list) failed")
    call check (list_equals (integer_eq, list_filter (is_positive_integer, (-1) ** (-2) ** nil_list), nil_list), &
         "list_equals (integer_eq, list_filter (is_positive_integer, (-1) ** (-2) ** nil_list), nil_list) failed")
    lst1a = (-1) ** 2 ** (-3) ** (-4) ** 5 ** 6 ** (-7) ** (-8) ** (-9) ** nil_list
    lst1b = 2 ** 5 ** 6 ** nil_list
    call check (list_equals (integer_eq, list_filter (is_positive_integer, lst1a), lst1b), &
         "list_equals (integer_eq, list_filter (is_positive_integer, lst1a), lst1b) failed")
    lst2a = cons_t_cast (list_append (lst1a, list3 (7, 8, 9)))
    lst2b = cons_t_cast (list_append (lst1b, list3 (7, 8, 9)))
    call check (list_equals (integer_eq, list_filter (is_positive_integer, lst2a), lst2b), &
         "list_equals (integer_eq, list_filter (is_positive_integer, lst2a), lst2b) failed")
    !
    ! FIXME: Maybe write some more tests for dotted lists.
    !
  end subroutine test_list_filter

  subroutine test_list_remove
    !
    ! FIXME: More test cases are needed. I have already had bugs slip
    !        past these.
    !
    type(cons_t) :: lst1a, lst1b
    type(cons_t) :: lst2a, lst2b
    call check (list_equals (integer_eq, list_remove (is_positive_integer, nil_list), nil_list), &
         "list_equals (list_remove (is_positive_integer, nil_list), nil_list) failed")
    call check (list_remove (is_positive_integer, 1234.0) .eqr. 1234.0, &
         "list_remove (is_positive_integer, 1234.0) .eqr. 1234.0 failed")
    call check (list_equals (integer_eq, list_remove (is_positive_integer, 1 ** nil_list), nil_list), &
         "list_equals (integer_eq, list_remove (is_positive_integer, 1 ** nil_list), nil_list) failed")
    call check (list_equals (integer_eq, list_remove (is_positive_integer, (-1) ** nil_list), (-1) ** nil_list), &
         "list_equals (integer_eq, list_remove (is_positive_integer, (-1) ** nil_list), (-1) ** nil_list) failed")
    call check (list_equals (integer_eq, list_remove (is_positive_integer, 1 ** 2 ** nil_list), nil_list), &
         "list_equals (integer_eq, list_remove (is_positive_integer, 1 ** 2 ** nil_list), nil_list) failed")
    call check (list_equals (integer_eq, list_remove (is_positive_integer, 1 ** (-2) ** nil_list), (-2) ** nil_list), &
         "list_equals (integer_eq, list_remove (is_positive_integer, 1 ** (-2) ** nil_list), (-2) ** nil_list) failed")
    call check (list_equals (integer_eq, list_remove (is_positive_integer, (-1) ** 2 ** nil_list), (-1) ** nil_list), &
         "list_equals (integer_eq, list_remove (is_positive_integer, (-1) ** 2 ** nil_list), (-1) ** nil_list) failed")
    call check (list_equals (integer_eq, list_remove (is_positive_integer, (-1) ** (-2) ** nil_list), &
         (-1) ** (-2) ** nil_list), &
         "list_equals (integer_eq, list_remove (is_positive_integer, (-1) ** (-2) ** nil_list), (-1) ** (-2) ** nil_list) failed")
    lst1a = (-1) ** 2 ** (-3) ** (-4) ** 5 ** 6 ** (-7) ** (-8) ** (-9) ** nil_list
    lst1b = (-1) ** (-3) ** (-4) ** (-7) ** (-8) ** (-9) ** nil_list
    call check (list_equals (integer_eq, list_remove (is_positive_integer, lst1a), lst1b), &
         "list_equals (integer_eq, list_remove (is_positive_integer, lst1a), lst1b) failed")
    lst2a = cons_t_cast (list_append (lst1a, list3 (7, 8, 9)))
    lst2b = lst1b
    call check (list_equals (integer_eq, list_remove (is_positive_integer, lst2a), lst2b), &
         "list_equals (integer_eq, list_remove (is_positive_integer, lst2a), lst2b) failed")
    !
    ! FIXME: Maybe write some more tests for dotted lists.
    !
  end subroutine test_list_remove

  subroutine test_list_partition
    !
    ! FIXME: More test cases are needed.
    !
    class(*), allocatable :: obj1_b, obj1_c
    class(*), allocatable :: obj2_b, obj2_c
    class(*), allocatable :: obj3_b, obj3_c
    class(*), allocatable :: obj4_b, obj4_c
    class(*), allocatable :: obj5_b, obj5_c
    class(*), allocatable :: obj6_b, obj6_c
    class(*), allocatable :: obj7_b, obj7_c
    class(*), allocatable :: obj99_b, obj99_c
    class(*), allocatable :: obj_b, obj_c
    type(cons_t) :: lst1a, lst1b, lst1c
    type(cons_t) :: lst2a, lst2b, lst2c
    !
    call list_partition (is_positive_integer, nil_list, obj1_b, obj1_c)
    call check (list_equals (integer_eq, obj1_b, nil_list), &
         "list_equals (integer_eq, obj1_b, nil_list) failed (for list_partition)")
    call check (list_equals (integer_eq, obj1_c, nil_list), &
         "list_equals (integer_eq, obj1_c, nil_list) failed (for list_partition)")
    !
    call list_partition (is_positive_integer, 1 ** nil_list, obj2_b, obj2_c)
    call check (list_equals (integer_eq, obj2_b, 1 ** nil_list), &
         "list_equals (integer_eq, obj2_b, 1 ** nil_list) failed (for list_partition)")
    call check (list_equals (integer_eq, obj2_c, nil_list), &
         "list_equals (integer_eq, obj2_c, nil_list) failed (for list_partition)")
    !
    call list_partition (is_positive_integer, (-1) ** nil_list, obj3_b, obj3_c)
    call check (list_equals (integer_eq, obj3_b, nil_list), &
         "list_equals (integer_eq, obj3_b, nil_list) failed (for list_partition)")
    call check (list_equals (integer_eq, obj3_c, (-1) ** nil_list), &
         "list_equals (integer_eq, obj3_c, (-1) ** nil_list) failed (for list_partition)")
    !
    call list_partition (is_positive_integer, 1 ** 2 ** nil_list, obj4_b, obj4_c)
    call check (list_equals (integer_eq, obj4_b, 1 ** 2 ** nil_list), &
         "list_equals (integer_eq, obj4_b, 1 ** 2 ** nil_list) failed (for list_partition)")
    call check (list_equals (integer_eq, obj4_c, nil_list), &
         "list_equals (integer_eq, obj4_c, nil_list) failed (for list_partition)")
    !
    call list_partition (is_positive_integer, 1 ** (-2) ** nil_list, obj5_b, obj5_c)
    call check (list_equals (integer_eq, obj5_b, 1 ** nil_list), &
         "list_equals (integer_eq, obj5_b, 1 ** nil_list) failed (for list_partition)")
    call check (list_equals (integer_eq, obj5_c, (-2) ** nil_list), &
         "list_equals (integer_eq, obj5_c, (-2) ** nil_list) failed (for list_partition)")
    !
    call list_partition (is_positive_integer, (-1) ** 2 ** nil_list, obj6_b, obj6_c)
    call check (list_equals (integer_eq, obj6_b, 2 ** nil_list), &
         "list_equals (integer_eq, obj6_b, 2 ** nil_list) failed (for list_partition)")
    call check (list_equals (integer_eq, obj6_c, (-1) ** nil_list), &
         "list_equals (integer_eq, obj6_c, (-1) ** nil_list) failed (for list_partition)")
    !
    call list_partition (is_positive_integer, (-1) ** (-2) ** nil_list, obj7_b, obj7_c)
    call check (list_equals (integer_eq, obj7_b, nil_list), &
         "list_equals (integer_eq, obj7_b, nil_list) failed (for list_partition)")
    call check (list_equals (integer_eq, obj7_c, (-1) ** (-2) ** nil_list), &
         "list_equals (integer_eq, obj7_c, (-1) ** (-2) ** nil_list) failed (for list_partition)")
    !
    lst1a = (-1) ** 2 ** (-3) ** (-4) ** 5 ** 6 ** (-7) ** (-8) ** (-9) ** nil_list
    lst1b = 2 ** 5 ** 6 ** nil_list
    lst1c = (-1) ** (-3) ** (-4) ** (-7) ** (-8) ** (-9) ** nil_list
    call list_partition (is_positive_integer, lst1a, obj_b, obj_c)
    call check (list_equals (integer_eq, obj_b, lst1b), "list_equals (integer_eq, obj_b, lst1b) failed (for list_partition)")
    call check (list_equals (integer_eq, obj_c, lst1c), "list_equals (integer_eq, obj_c, lst1c) failed (for list_partition)")
    !
    lst2a = cons_t_cast (list_append (lst1a, list3 (7, 8, 9)))
    lst2b = 2 ** 5 ** 6 ** 7 ** 8 ** 9 ** nil_list
    lst2c = lst1c
    call list_partition (is_positive_integer, lst2a, obj_b, obj_c)
    call check (list_equals (integer_eq, obj_b, lst2b), "list_equals (integer_eq, obj_b, lst2b) failed (for list_partition)")
    call check (list_equals (integer_eq, obj_c, lst2c), "list_equals (integer_eq, obj_c, lst2c) failed (for list_partition)")
    !
    ! The following is an explicitly `unspecified result', but let s
    ! test it anyway.
    !
    call list_partition (is_positive_integer, 1234.0, obj99_b, obj99_c)
    call check (obj99_b .eqr. 1234.0, "obj99_b .eqr. 1234.0 failed (for list_partition)")
    call check (list_equals (integer_eq, obj99_c, nil_list), &
         "list_equals (integer_eq, obj99_c, nil_list) failed (for list_partition)")
    !
    ! FIXME: Maybe write some more tests for dotted lists.
    !
  end subroutine test_list_partition

  subroutine test_list_delete
    !
    ! FIXME: More test cases are needed.
    !
    type(cons_t) :: lst1a, lst1b
    type(cons_t) :: lst2a, lst2b
    call check (list_equals (integer_eq, list_delete (0, integer_lt, nil_list), nil_list), &
         "list_equals (list_delete (0, integer_lt, nil_list), nil_list) failed")
    call check (list_delete (0, integer_lt, 1234.0) .eqr. 1234.0, &
         "list_delete (0, integer_lt, 1234.0) .eqr. 1234.0 failed")
    call check (list_equals (integer_eq, list_delete (0, integer_lt, 1 ** nil_list), nil_list), &
         "list_equals (integer_eq, list_delete (0, integer_lt, 1 ** nil_list), nil_list) failed")
    call check (list_equals (integer_eq, list_delete (0, integer_lt, (-1) ** nil_list), (-1) ** nil_list), &
         "list_equals (integer_eq, list_delete (0, integer_lt, (-1) ** nil_list), (-1) ** nil_list) failed")
    call check (list_equals (integer_eq, list_delete (0, integer_lt, 1 ** 2 ** nil_list), nil_list), &
         "list_equals (integer_eq, list_delete (0, integer_lt, 1 ** 2 ** nil_list), nil_list) failed")
    call check (list_equals (integer_eq, list_delete (0, integer_lt, 1 ** (-2) ** nil_list), (-2) ** nil_list), &
         "list_equals (integer_eq, list_delete (0, integer_lt, 1 ** (-2) ** nil_list), (-2) ** nil_list) failed")
    call check (list_equals (integer_eq, list_delete (0, integer_lt, (-1) ** 2 ** nil_list), (-1) ** nil_list), &
         "list_equals (integer_eq, list_delete (0, integer_lt, (-1) ** 2 ** nil_list), (-1) ** nil_list) failed")
    call check (list_equals (integer_eq, list_delete (0, integer_lt, (-1) ** (-2) ** nil_list), &
         (-1) ** (-2) ** nil_list), &
         "list_equals (integer_eq, list_delete (0, integer_lt, (-1) ** (-2) ** nil_list), (-1) ** (-2) ** nil_list) failed")
    lst1a = (-1) ** 2 ** (-3) ** (-4) ** 5 ** 6 ** (-7) ** (-8) ** (-9) ** nil_list
    lst1b = (-1) ** (-3) ** (-4) ** (-7) ** (-8) ** (-9) ** nil_list
    call check (list_equals (integer_eq, list_delete (0, integer_lt, lst1a), lst1b), &
         "list_equals (integer_eq, list_delete (0, integer_lt, lst1a), lst1b) failed")
    lst2a = cons_t_cast (list_append (lst1a, list3 (7, 8, 9)))
    lst2b = lst1b
    call check (list_equals (integer_eq, list_delete (0, integer_lt, lst2a), lst2b), &
         "list_equals (integer_eq, list_delete (0, integer_lt, lst2a), lst2b) failed")
    !
    ! FIXME: Maybe write some more tests for dotted lists.
    !
  end subroutine test_list_delete

  subroutine test_list_delete_duplicates
    !
    ! FIXME: More test cases are needed.
    !
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, nil_list), nil_list), &
         "check0010 failed (for list_delete_duplicates)")
    call check (list_delete_duplicates (integer_eq, 1234.0) .eqr. 1234.0, &
         "check0020 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** nil_list), 1 ** nil_list), &
         "check0030 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 1 ** nil_list), 1 ** nil_list), &
         "check0040 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 1 ** 1 ** nil_list), 1 ** nil_list), &
         "check0050 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 2 ** nil_list), 1 ** 2 ** nil_list), &
         "check0060 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 1 ** 2 ** nil_list), 1 ** 2 ** nil_list), &
         "check0070 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 2 ** 2 ** nil_list), 1 ** 2 ** nil_list), &
         "check0080 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 2 ** 1 ** nil_list), 1 ** 2 ** nil_list), &
         "check0090 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 1 ** 1 ** 2 ** nil_list), &
         1 ** 2 ** nil_list), &
         "check0100 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 1 ** 2 ** 2 ** nil_list), &
         1 ** 2 ** nil_list), &
         "check0110 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 1 ** 2 ** 1 ** 2 ** nil_list), &
         1 ** 2 ** nil_list), &
         "check0120 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 2 ** 1 ** 1 ** 2 ** nil_list), &
         2 ** 1 ** nil_list), &
         "check0130 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, 2 ** 1 ** 1 ** 1 ** nil_list), &
         2 ** 1 ** nil_list), &
         "check0140 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, iota (100)), iota (100)), &
         "check0150 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, &
         list_append (list_append (list_append (iota (100), iota (100)), iota (100)), iota (100))), &
         iota (100)), &
         "check0160 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, &
         list_take (circular_list (list5 (1, 2, 3, 4, 5)), 100)), &
         list5 (1, 2, 3, 4, 5)), &
         "check0170 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, &
         list_take (circular_list (list5 (1, 2, 3, 4, 5)), 101)), &
         list5 (1, 2, 3, 4, 5)), &
         "check0180 failed (for list_delete_duplicates)")
    call check (list_equals (integer_eq, list_delete_duplicates (integer_eq, &
         list_take (circular_list (list9 (1, 1, 1, 1, 2, 2, 2, 2, 1)), 101)), &
         list2 (1, 2)), &
         "check0190 failed (for list_delete_duplicates)")
    !
    ! FIXME: Maybe write some more tests for dotted lists.
    !
  end subroutine test_list_delete_duplicates

  subroutine run_tests
    !
    ! FIXME: Add tests that check various subroutines do not clobber
    ! their arguments. (list_classify, for example.)
    !
    call test_is_nil_or_pair
    call test_is_nil_list
    call test_is_cons_pair
    call test_list_is_nil
    call test_list_is_pair
    call test_cons_t_eq
    call test_uncons_car_cdr
    call test_list_cons
    call test_set_car_and_set_cdr
    call test_list_length
    call test_list_length_plus
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
    call test_list_zip1
    call test_list_zip3
    call test_list_unzip1
    call test_list_unzip2
    call test_list_unzip3
    call test_list_unzip4
    call test_list_unzip1f
    call test_list_foreach
    call test_list_pair_foreach
    call test_list_map_elements
    call test_list_map_elements_in_place
    call test_list_append_map_elements
    call test_list_modify_elements
    call test_list_modify_elements_in_place
    call test_list_append_modify_elements
    call test_list_find
    call test_list_find_tail
    call test_list_take_while
    call test_list_drop_while
    call test_list_span
    call test_list_break
    call test_list_any
    call test_list_every
    call test_list_index0
    call test_list_index1
    call test_list_equals
    call test_list_count
    call test_list_filter
    call test_list_remove
    call test_list_partition
    call test_list_delete
    call test_list_delete_duplicates
  end subroutine run_tests

end module test__cons_lists

program main
  use test__cons_lists
  implicit none
  call run_tests
end program main
