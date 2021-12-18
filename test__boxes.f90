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

module test__boxes

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: boxes

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
    write (error_unit, '("test__boxes error: ", a)') msg
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

  subroutine test1
    type(gcroot_t) :: box1

    call check (unbox (box (1234)) .eqi. 1234, "test1-0005 failed")
    call check (current_heap_size () == 1, "test1-0010 failed")
    call check (current_roots_count () == 0, "test1-0020 failed")

    box1 = box (1234)
    call check (unbox (box1) .eqi. 1234, "test1-0025 failed")
    call check (current_heap_size () == 2, "test1-0030 failed")
    call check (current_roots_count () == 1, "test1-0040 failed")

    call collect_garbage_now
    call check (current_heap_size () == 1, "test1-0050 failed")
    call check (current_roots_count () == 1, "test1-0060 failed")
    call check (unbox (box1) .eqi. 1234, "test1-0070 failed")
  end subroutine test1

  subroutine test2
    type(gcroot_t) :: box1, box2

    call check (current_roots_count () == 0, "test2-0010 failed")

    call collect_garbage_now
    call check (current_heap_size () == 0, "test2-0020 failed")
    call check (current_roots_count () == 0, "test2-0025 failed")

    box1 = box (1234.0)
    call check (current_heap_size () == 1, "test2-0030 failed")
    call check (current_roots_count () == 1, "test2-0040 failed")
    call check (unbox (box1) .eqr. 1234.0, "test2-0050 failed")

    box2 = box1
    call check (current_heap_size () == 1, "test2-0060 failed")
    call check (current_roots_count () == 2, "test2-0070 failed")
    call check (unbox (box2) .eqr. 1234.0, "test2-0050 failed")
  end subroutine test2

  subroutine test3
    type(gcroot_t) :: box1, box2, box3, box4

    call check (current_roots_count () == 0, "test3-0010 failed")

    call collect_garbage_now
    call check (current_heap_size () == 0, "test3-0020 failed")
    call check (current_roots_count () == 0, "test3-0030 failed")

    box1 = box (1234.0)
    call check (current_heap_size () == 1, "test3-0040 failed")
    call check (current_roots_count () == 1, "test3-0050 failed")
    call check (unbox (box1) .eqr. 1234.0, "test3-0060 failed")

    box2 = box (box1)
    call collect_garbage_now

    box3 = box (box2)
    call collect_garbage_now
    call check (current_heap_size () == 3, "test3-0100 failed")
    call check (current_roots_count () == 3, "test3-0110 failed")
    call check (unbox (box1) .eqr. 1234.0, "test3-0200 failed")
    call check (unbox (unbox (box2)) .eqr. 1234.0, "test3-0210 failed")
    call check (unbox (unbox (unbox (box3))) .eqr. 1234.0, "test3-0220 failed")

    box4 = box (box (box (1234)))
    call check (current_heap_size () == 6, "test3-0300 failed")
    call check (current_roots_count () == 4, "test3-0310 failed")
    call check (unbox (unbox (unbox (box4))) .eqi. 1234, "test3-0320 failed")
  end subroutine test3

  subroutine test4
    type(gcroot_t) :: box1, box2

    box1 = box (1234)
    call check (unbox (box1) .eqi. 1234, "test4-0010 failed")
    call set_box (box1, 1234.0)
    call check (unbox (box1) .eqr. 1234.0, "test4-0020 failed")

    box1 = box (box (1234))
    call check (unbox (unbox (box1)) .eqi. 1234, "test4-0030 failed")
    call set_box (box1, box (5678))
    call check (unbox (unbox (box1)) .eqi. 5678, "test4-0040 failed")

    box1 = box1
    call check (unbox (unbox (box1)) .eqi. 5678, "test4-0050 failed")

    box1 = 5678.0
    call check (box1%get_value () .eqr. 5678.0, "test4-0060 failed")

    box2 = 4321
    call check (box2%get_value () .eqi. 4321, "test4-0070 failed")
    box1 = box2
    call check (box1%get_value () .eqi. 4321, "test4-0080 failed")
  end subroutine test4

  subroutine run_tests
    call test1
    call test2
    call test3
    call test4
    call collect_garbage_now
    call check (current_heap_size () == 0, "run_tests-0100 failed")
    call check (current_roots_count () == 0, "run_tests-0110 failed")
  end subroutine run_tests

end module test__boxes

program main
  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: boxes
  use, non_intrinsic :: test__boxes

  implicit none

  call run_tests

end program main
