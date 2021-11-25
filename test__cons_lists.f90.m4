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

  public :: test_first_second_etc

  interface operator(.ieq.)
     module procedure integer_eq
  end interface operator(.ieq.)

contains

  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '("test__cons_lists error: ", a)') msg
    CALL_ABORT
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

  function integer_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = assume_integer (obj1) == assume_integer (obj2)
  end function integer_eq

  subroutine test_first_second_etc
    type(cons_t) :: lst
    lst = iota (15, 1)
    call check (first (lst) .ieq. 1, "first (lst) .ieq. 1 failed")
    call check (second (lst) .ieq. 2, "second (lst) .ieq. 2 failed")
    call check (third (lst) .ieq. 3, "third (lst) .ieq. 3 failed")
    call check (fourth (lst) .ieq. 4, "fourth (lst) .ieq. 4 failed")
    call check (fifth (lst) .ieq. 5, "fifth (lst) .ieq. 5 failed")
    call check (sixth (lst) .ieq. 6, "sixth (lst) .ieq. 6 failed")
    call check (seventh (lst) .ieq. 7, "seventh (lst) .ieq. 7 failed")
    call check (eighth (lst) .ieq. 8, "eighth (lst) .ieq. 8 failed")
    call check (ninth (lst) .ieq. 9, "ninth (lst) .ieq. 9 failed")
    call check (tenth (lst) .ieq. 10, "tenth (lst) .ieq. 10 failed")
  end subroutine test_first_second_etc

end module test__cons_lists

program main
  use test__cons_lists
  implicit none

  call test_first_second_etc
end program main
