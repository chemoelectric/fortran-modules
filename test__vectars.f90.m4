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

module test__vectars

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: vectars

  implicit none
  private

  public :: run_tests

  integer, parameter :: sz = size_kind

  type :: str_t
     character(:), allocatable :: val
   contains
     procedure, pass :: length => str_t_length
     procedure, pass :: equal => str_t_equal
     procedure, pass :: less_than => str_t_less_than
     procedure, pass :: assign => str_t_assign
     generic :: operator(==) => equal
     generic :: operator(<) => less_than
     generic :: assignment(=) => assign
     final :: str_t_finalize
  end type str_t

  interface operator(.eqi.)
     module procedure int_eq
  end interface operator(.eqi.)

  interface operator(.eqs.)
     module procedure str_t_eq
  end interface operator(.eqs.)

contains

  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("test__vectars error: ", a)') msg
    error stop
  end subroutine error_abort

  subroutine check (boolean, msg)
    logical, intent(in) :: boolean
    character(*), intent(in) :: msg
    if (.not. boolean) call error_abort (msg)
  end subroutine check

  function str_t_length (this) result (length)
    class(str_t), intent(in) :: this
    integer :: length
    length = 0
    if (allocated (this%val)) then
       length = len (this%val)
    end if
  end function str_t_length

  function str_t_equal (this, other) result (bool)
    class(str_t), intent(in) :: this
    class(str_t), intent(in) :: other
    logical :: bool
    bool = (this%val == other%val)
  end function str_t_equal

  function str_t_less_than (this, other) result (bool)
    class(str_t), intent(in) :: this
    class(str_t), intent(in) :: other
    logical :: bool
    bool = (this%val < other%val)
  end function str_t_less_than

  subroutine str_t_assign (dst, src)
    class(str_t), intent(inout) :: dst
    class(str_t), intent(in) :: src
    allocate (character(len (src%val)) :: dst%val)
    dst%val = src%val
  end subroutine str_t_assign

  subroutine str_t_finalize (this)
    type(str_t) :: this
    if (allocated (this%val)) then
       deallocate (this%val)
    end if
  end subroutine str_t_finalize

  function int_cast (obj) result (int)
    class(*), intent(in) :: obj
    integer :: int
    select type (obj)
    type is (integer)
       int = obj
    class default
       call error_abort ("int_cast of an incompatible object")
    end select
  end function int_cast

  function str_t_cast (obj) result (s)
    class(*), intent(in) :: obj
    type(str_t) :: s
    select type (obj)
    class is (str_t)
       s = obj
    class default
       call error_abort ("str_t_cast of an incompatible object")
    end select
  end function str_t_cast

  function int_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = int_cast (obj1) == int_cast (obj2)
  end function int_eq

  function int_lt (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = int_cast (obj1) < int_cast (obj2)
  end function int_lt

  function str_t_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = str_t_cast (obj1) == str_t_cast (obj2)
  end function str_t_eq

  function str_t_lt (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = str_t_cast (obj1) < str_t_cast (obj2)
  end function str_t_lt

  function int_eq_gc (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    call collect_garbage_now
    bool = int_eq (obj1, obj2)
  end function int_eq_gc

  function str_t_eq_gc (str1, str2) result (bool)
    class(*), intent(in) :: str1
    class(*), intent(in) :: str2
    logical :: bool
    call collect_garbage_now
    bool = (str_t_cast (str1) == str_t_cast (str2))
  end function str_t_eq_gc

  subroutine test0010
    type(vectar_t) :: vec
    integer(sz) :: i
    integer :: j

    vec = make_vectar (100_sz, str_t ('fill'))
    call check (vectar_length (vec) == 100_sz, "test0010-0000 failed")
    do i = 0_sz, 99_sz
       call check (vectar_ref0 (vec, i) .eqs. str_t ('fill'), "test0010-0010 failed")
       call check (vectar_ref1 (vec, i + 1) .eqs. str_t ('fill'), "test0010-0020 failed")
       call check (vectar_refn (vec, -1_sz, i - 1) .eqs. str_t ('fill'), "test0010-0030 failed")
    end do

    vec = make_vectar (100, str_t ('fill'))
    call check (vectar_length (vec) == 100_sz, "test0010-0100 failed")
    do j = 0, 99
       call check (vectar_ref0 (vec, j) .eqs. str_t ('fill'), "test0010-0110 failed")
       call check (vectar_ref1 (vec, j + 1) .eqs. str_t ('fill'), "test0010-0120 failed")
       call check (vectar_refn (vec, -1, j - 1) .eqs. str_t ('fill'), "test0010-0130 failed")
    end do

    vec = vectar ()
    call check (vectar_length (vec) == 0_sz, "test0010-0200 failed")

    vec = vectar (1, 2, 3, 4, 5)
    call check (vectar_length (vec) == 5_sz, "test0010-0300 failed")
    do j = 0, 4
       call check (vectar_ref0 (vec, j) .eqi. j + 1, "test0010-0310 failed")
    end do
    do j = 1, 5
       call check (vectar_ref1 (vec, j) .eqi. j, "test0010-0320 failed")
    end do
    do j = -1, 3
       call check (vectar_refn (vec, -1, j) .eqi. j + 2, "test0010-0330 failed")
    end do
  end subroutine test0010

  subroutine run_tests
    heap_size_limit = 0

    call test0010

    call collect_garbage_now
    call check (current_heap_size () == 0, "run_tests-0100 failed")
    call check (current_roots_count () == 0, "run_tests-0110 failed")
  end subroutine run_tests

end module test__vectars

program main
  use, non_intrinsic :: test__vectars
  implicit none
  call run_tests
end program main
