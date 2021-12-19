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

module test__cons_pairs

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

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
    write (error_unit, '("test__cons_pairs error: ", a)') msg
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
    class default
       call error_abort ("integer_cast of an incompatible object")
    end select
  end function integer_cast

  function real_cast (obj) result (r)
    class(*), intent(in) :: obj
    real :: r
    select type (obj)
    type is (real)
       r = obj
    class default
       call error_abort ("real_cast of an incompatible object")
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

  subroutine test001
    type(gcroot_t) :: cons1
    class(*), allocatable :: car1, cdr1

    call check (car (cons (123, 456)) .eqi. 123, "test001-0010 failed")
    call check (cdr (cons (123, 456)) .eqi. 456, "test001-0020 failed")

    cons1 = cons (123.0, 456.0)
    call check (car (cons1) .eqr. 123.0, "test001-0030 failed")
    call check (cdr (cons1) .eqr. 456.0, "test001-0040 failed")
    call uncons (cons1, car1, cdr1)
    call check (car1 .eqr. 123.0, "test001-0050 failed")
    call check (cdr1 .eqr. 456.0, "test001-0060 failed")

    call check (is_pair (cons (123, 456)), "test001-0070 failed")
    call check (is_pair (cons1), "test001-0080 failed")

    call check (.not. is_not_pair (cons (123, 456)), "test001-0090 failed")
    call check (.not. is_not_pair (cons1), "test001-0100 failed")

    call check (.not. pair_t_eq (cons (123, 456), cons (123, 456)), "test001-0110 failed")
    call check (pair_t_eq (cons1, cons1), "test001-0120 failed")

    call check (.not. is_nil (cons (123, 456)), "test001-0130 failed")
    call check (.not. is_nil (cons1), "test001-0140 failed")

    call check (is_not_nil (cons (123, 456)), "test001-0150 failed")
    call check (is_not_nil (cons1), "test001-0160 failed")

    call check (.not. is_nil_list (cons (123, 456)), "test001-0170 failed")
    call check (.not. is_nil_list (cons1), "test001-0180 failed")

    call check (is_nil (nil), "test001-0190 failed")
    call check (.not. is_not_nil (nil), "test001-0200 failed")
    call check (is_nil_list (nil), "test001-0210 failed")
  end subroutine test001

  subroutine run_tests
    call test001
    call collect_garbage_now
    call check (current_heap_size () == 0, "run_tests-0100 failed")
    call check (current_roots_count () == 0, "run_tests-0110 failed")
  end subroutine run_tests

end module test__cons_pairs

program main
  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: test__cons_pairs

  implicit none

  call run_tests

end program main
