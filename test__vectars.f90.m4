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

!!$  function int_lt (obj1, obj2) result (bool)
!!$    class(*), intent(in) :: obj1, obj2
!!$    logical :: bool
!!$    bool = int_cast (obj1) < int_cast (obj2)
!!$  end function int_lt

  function str_t_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = str_t_cast (obj1) == str_t_cast (obj2)
  end function str_t_eq

!!$  function str_t_lt (obj1, obj2) result (bool)
!!$    class(*), intent(in) :: obj1, obj2
!!$    logical :: bool
!!$    bool = str_t_cast (obj1) < str_t_cast (obj2)
!!$  end function str_t_lt

!!$  function int_eq_gc (obj1, obj2) result (bool)
!!$    class(*), intent(in) :: obj1, obj2
!!$    logical :: bool
!!$    call collect_garbage_now
!!$    bool = int_eq (obj1, obj2)
!!$  end function int_eq_gc

!!$  function str_t_eq_gc (str1, str2) result (bool)
!!$    class(*), intent(in) :: str1
!!$    class(*), intent(in) :: str2
!!$    logical :: bool
!!$    call collect_garbage_now
!!$    bool = (str_t_cast (str1) == str_t_cast (str2))
!!$  end function str_t_eq_gc

  subroutine test0010
    type(vectar_t) :: vec
    type(cons_t) :: lst
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

    vec = vectar (list (1, 2, 3), list (4, 5, 6), list (7, 8, 9))
    call check (vectar_length (vec) == 3, "test0010-0400 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec, 1), list (1, 2, 3)), "test0010-0410 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec, 2), list (4, 5, 6)), "test0010-0420 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec, 3), list (7, 8, 9)), "test0010-0430 failed")

    lst = list (vectar (1, 2, 3), vectar (4, 5, 6), vectar (7, 8, 9))
    call check (vectar_length (list_ref1 (lst, 1)) == 3, "test0010-1010 failed")
    call check (vectar_length (list_ref1 (lst, 2)) == 3, "test0010-1020 failed")
    call check (vectar_length (list_ref1 (lst, 3)) == 3, "test0010-1030 failed")
    call check (vectar_ref1 (list_ref1 (lst, 1), 1) .eqi. 1, "test0010-1040 failed")
    call check (vectar_ref1 (list_ref1 (lst, 1), 2) .eqi. 2, "test0010-1050 failed")
    call check (vectar_ref1 (list_ref1 (lst, 1), 3) .eqi. 3, "test0010-1060 failed")
    call check (vectar_ref1 (list_ref1 (lst, 2), 1) .eqi. 4, "test0010-1070 failed")
    call check (vectar_ref1 (list_ref1 (lst, 2), 2) .eqi. 5, "test0010-1080 failed")
    call check (vectar_ref1 (list_ref1 (lst, 2), 3) .eqi. 6, "test0010-1090 failed")
    call check (vectar_ref1 (list_ref1 (lst, 3), 1) .eqi. 7, "test0010-1100 failed")
    call check (vectar_ref1 (list_ref1 (lst, 3), 2) .eqi. 8, "test0010-1110 failed")
    call check (vectar_ref1 (list_ref1 (lst, 3), 3) .eqi. 9, "test0010-1120 failed")

    ! Test vectars with unspecified fill.
    vec = make_vectar (100_sz)
    call check (vectar_length (vec) == 100, "test0010-2010 failed")
    vec = make_vectar (100)
    call check (vectar_length (vec) == 100, "test0010-2020 failed")
  end subroutine test0010

  subroutine test0015

    ! Check that mutator roots work.

    type(gcroot_t) :: vec
    type(gcroot_t) :: lst

    vec = vectar (list (1, 2, 3), list (4, 5, 6), list (7, 8, 9))
    call collect_garbage_now
    call check (vectar_length (vec) == 3, "test0015-0400 failed")
    call collect_garbage_now
    call check (list_equal (int_eq, vectar_ref1 (vec, 1), list (1, 2, 3)), "test0015-0410 failed")
    call collect_garbage_now
    call check (list_equal (int_eq, vectar_ref1 (vec, 2), list (4, 5, 6)), "test0015-0420 failed")
    call collect_garbage_now
    call check (list_equal (int_eq, vectar_ref1 (vec, 3), list (7, 8, 9)), "test0015-0430 failed")

    lst = list (vectar (1, 2, 3), vectar (4, 5, 6), vectar (7, 8, 9))
    call collect_garbage_now
    call check (vectar_length (list_ref1 (lst, 1)) == 3, "test0015-1010 failed")
    call collect_garbage_now
    call check (vectar_length (list_ref1 (lst, 2)) == 3, "test0015-1020 failed")
    call collect_garbage_now
    call check (vectar_length (list_ref1 (lst, 3)) == 3, "test0015-1030 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 1), 1) .eqi. 1, "test0015-1040 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 1), 2) .eqi. 2, "test0015-1050 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 1), 3) .eqi. 3, "test0015-1060 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 2), 1) .eqi. 4, "test0015-1070 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 2), 2) .eqi. 5, "test0015-1080 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 2), 3) .eqi. 6, "test0015-1090 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 3), 1) .eqi. 7, "test0015-1100 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 3), 2) .eqi. 8, "test0015-1110 failed")
    call collect_garbage_now
    call check (vectar_ref1 (list_ref1 (lst, 3), 3) .eqi. 9, "test0015-1120 failed")
  end subroutine test0015

  subroutine test0020
    call check (list_equal (int_eq, vectar_to_list (vectar ()), list ()), "test0020-0010 failed")
    call check (list_equal (int_eq, vectar_to_list (vectar (1, 2, 3, 4, 5)), iota (5, 1)), "test0020-0020 failed")

    call check (list_equal (int_eq, reverse_vectar_to_list (vectar ()), list ()), "test0020-0030 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (vectar (5, 4, 3, 2, 1)), iota (5, 1)), "test0020-0040 failed")

    call check (list_equal (int_eq, vectar_to_list (list_to_vectar (nil)), list ()), "test0020-0050 failed")
    call check (list_equal (int_eq, vectar_to_list (list_to_vectar (iota (5, 1))), iota (5, 1)), "test0020-0060 failed")

    call check (list_equal (int_eq, reverse_vectar_to_list (list_to_vectar (nil)), list ()), &
         &      "test0020-0070 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (list_to_vectar (iota (5, 5, -1))), iota (5, 1)), &
         &      "test0020-0080 failed")
  end subroutine test0020

  subroutine test0030
    type(vectar_t) :: vec
    integer :: i

    vec = make_vectar (5, .false.)
    do i = 0, 4
       call vectar_set0 (vec, i, i)
    end do
    call check (list_equal (int_eq, vectar_to_list (vec), iota (5)), "test0030-0010 failed")

    vec = make_vectar (5, .false.)
    do i = 1, 5
       call vectar_set1 (vec, i, i)
    end do
    call check (list_equal (int_eq, vectar_to_list (vec), iota (5, 1)), "test0030-0020 failed")

    vec = make_vectar (5, .false.)
    do i = -1, 3
       call vectar_setn (vec, -1, i, i)
    end do
    call check (list_equal (int_eq, vectar_to_list (vec), iota (5, -1)), "test0030-0030 failed")
  end subroutine test0030

  subroutine test0040
    type(vectar_t) :: vec1, vec2, vec3, vec4

    call check (apply_vectar_equal (str_t_eq, list ()), "test0040-0010 failed")
    call check (apply_vectar_equal (str_t_eq, list (vectar ())), "test0040-0020 failed")
    call check (apply_vectar_equal (str_t_eq, list (vectar (), vectar ())), "test0040-0030 failed")
    call check (apply_vectar_equal (str_t_eq, list (vectar (), vectar (), vectar ())), "test0040-0040 failed")

    call check (vectar_equal (str_t_eq), "test0040-0110 failed")
    call check (vectar_equal (str_t_eq, vectar ()), "test0040-0120 failed")
    call check (vectar_equal (str_t_eq, vectar (), vectar ()), "test0040-0130 failed")
    call check (vectar_equal (str_t_eq, vectar (), vectar (), vectar ()), "test0040-0140 failed")

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vec1
    vec3 = list_to_vectar (iota (100, 1))
    vec4 = list_to_vectar (iota (100, 100, -1))
    call check (vectar_equal (int_eq, vec1, vec2, vec3), "test0040-0200 failed")
    call check (.not. vectar_equal (int_eq, vec1, vec2, vec3, vec4), "test0040-0210 failed")
    call check (.not. vectar_equal (int_eq, vec1, vec2, vec3, vectar ()), "test0040-0220 failed")
    call check (.not. vectar_equal (int_eq, vec1, vec2, vec3, vec4, vec2, vec3), "test0040-0230 failed")
    call check (.not. vectar_equal (int_eq, vec1, vectar (), vec2, vec3), "test0040-0240 failed")
    call check (.not. apply_vectar_equal (int_eq, list (vec1, vec2, vec3, vec4, vec2, vec3)), "test0040-0250 failed")
    call check (apply_vectar_equal (int_eq, list (vec1, vec2, vec3, vec2, vec3)), "test0040-0260 failed")
    call check (vectar_equal (int_eq, vec1, vec2, vec3, vec2, vec3), "test0040-0270 failed")
    call check (vectar_equal (int_eq, vec1), "test0040-0280 failed")

    vec1 = vectar (str_t ('a'), str_t ('b'), str_t ('c'))
    vec2 = vectar (str_t ('a'), str_t ('b'), str_t ('c'))
    vec3 = vectar (str_t ('a'), str_t ('b'), str_t ('x'))
    vec4 = vectar (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'))
    call check (apply_vectar_equal (str_t_eq, list (vec1)), "test0040-1000 failed")
    call check (apply_vectar_equal (str_t_eq, list (vec1, vec2)), "test0040-1010 failed")
    call check (vectar_equal (str_t_eq, vec1, vec2), "test0040-1020 failed")
    call check (.not. vectar_equal (str_t_eq, vec1, vec3), "test0040-1030 failed")
    call check (.not. vectar_equal (str_t_eq, vec1, vec2, vec3), "test0040-1040 failed")
    call check (.not. vectar_equal (str_t_eq, vec1, vec3, vec2), "test0040-1050 failed")
    call check (.not. vectar_equal (str_t_eq, vec3, vec2, vec1), "test0040-1060 failed")
    call check (.not. vectar_equal (str_t_eq, vec1, vec2, vec4), "test0040-1070 failed")
    call check (.not. vectar_equal (str_t_eq, vec1, vec4, vec2), "test0040-1080 failed")
    call check (.not. vectar_equal (str_t_eq, vec4, vec2, vec1), "test0040-1090 failed")
    call check (.not. vectar_equal (str_t_eq, vec3, vec4), "test0040-1100 failed")
  end subroutine test0040

  subroutine test0050
    call check (vectar_is_empty (vectar ()), "test0050-0010 failed")
    call check (.not. vectar_is_empty (vectar (123)), "test0050-0020 failed")
    call check (.not. vectar_is_empty (vectar (1, 2.0, str_t ('3'))), "test0050-0030 failed")
  end subroutine test0050

  subroutine test0060
    call check (is_vectar (vectar ()), "test0060-0010 failed")
    call check (is_vectar (vectar (123)), "test0060-0020 failed")
    call check (is_vectar (vectar (1, 2.0, str_t ('3'))), "test0060-0030 failed")

    call check (.not. is_not_vectar (vectar ()), "test0060-0110 failed")
    call check (.not. is_not_vectar (vectar (123)), "test0060-0120 failed")
    call check (.not. is_not_vectar (vectar (1, 2.0, str_t ('3'))), "test0060-0130 failed")

    call check (.not. is_vectar (list (vectar (), vectar ())), "test0060-0210 failed")
    call check (.not. is_vectar (123), "test0060-0220 failed")
    call check (.not. is_vectar (3.1415926535), "test0060-0230 failed")
    call check (.not. is_vectar (str_t ('3.1415926535')), "test0060-0240 failed")

    call check (is_not_vectar (list (vectar (), vectar ())), "test0060-0310 failed")
    call check (is_not_vectar (123), "test0060-0320 failed")
    call check (is_not_vectar (3.1415926535), "test0060-0330 failed")
    call check (is_not_vectar (str_t ('3.1415926535')), "test0060-0340 failed")
  end subroutine test0060

  subroutine test0070
    type(vectar_t) :: vec

    vec = vectar (1, 2, 3, 4)
    call vectar_swap0 (vec, 1_sz, 2_sz)
    call check (vectar_equal (int_eq, vec, vectar (1, 3, 2, 4)), "test0070-0010 failed")

    vec = vectar (1, 2, 3, 4)
    call vectar_swap1 (vec, 1_sz, 2_sz)
    call check (vectar_equal (int_eq, vec, vectar (2, 1, 3, 4)), "test0070-0020 failed")

    vec = vectar (1, 2, 3, 4)
    call vectar_swapn (vec, -10_sz, -10_sz, -7_sz)
    call check (vectar_equal (int_eq, vec, vectar (4, 2, 3, 1)), "test0070-0030 failed")


    vec = vectar (1, 2, 3, 4)
    call vectar_swap0 (vec, 1, 2)
    call check (vectar_equal (int_eq, vec, vectar (1, 3, 2, 4)), "test0070-0010 failed")

    vec = vectar (1, 2, 3, 4)
    call vectar_swap1 (vec, 1, 2)
    call check (vectar_equal (int_eq, vec, vectar (2, 1, 3, 4)), "test0070-0020 failed")

    vec = vectar (1, 2, 3, 4)
    call vectar_swapn (vec, -10, -10, -7)
    call check (vectar_equal (int_eq, vec, vectar (4, 2, 3, 1)), "test0070-0030 failed")
  end subroutine test0070

  subroutine test0080

    !
    ! Test vectar_range_t.
    !

    type(vectar_t) :: vec
    type(vectar_range_t) :: range

    vec = list_to_vectar (iota (100, 1))

    call check (list_equal (int_eq, vectar_to_list (vec%range0(9_sz, 18_sz)), iota (10, 10)), "test0080-0010 failed")
    call check (list_equal (int_eq, vectar_to_list (vec%range1(10_sz, 19_sz)), iota (10, 10)), "test0080-0020 failed")
    call check (list_equal (int_eq, vectar_to_list (vec%rangen(2_sz, 11_sz, 20_sz)), iota (10, 10)), "test0080-0030 failed")
    call check (list_equal (int_eq, vectar_to_list (vec%range1(1_sz, 100_sz)), iota (100, 1)), "test0080-0040 failed")

    call check (list_equal (int_eq, vectar_to_list (vec%range0(9, 18)), iota (10, 10)), "test0080-0110 failed")
    call check (list_equal (int_eq, vectar_to_list (vec%range1(10, 19)), iota (10, 10)), "test0080-0120 failed")
    call check (list_equal (int_eq, vectar_to_list (vec%rangen(2, 11, 20)), iota (10, 10)), "test0080-0130 failed")
    call check (list_equal (int_eq, vectar_to_list (vec%range1(1, 100)), iota (100, 1)), "test0080-0140 failed")

    call check (list_equal (int_eq, reverse_vectar_to_list (vec%range0(9_sz, 18_sz)), iota (10, 19, -1)), &
         &      "test0080-0210 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (vec%range1(10_sz, 19_sz)), iota (10, 19, -1)), &
         &      "test0080-0220 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (vec%rangen(2_sz, 11_sz, 20_sz)), iota (10, 19, -1)), &
         &      "test0080-0230 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (vec%range1(1_sz, 100_sz)), iota (100, 100, -1)), &
         &      "test0080-0240 failed")

    range = vec%range1(20, 39)
    call check (range%istart0() == 19, "test0080-0310 failed")
    call check (range%istart1() == 20, "test0080-0320 failed")
    call check (range%istartn(2_sz) == 21, "test0080-0330 failed")
    call check (range%istartn(2) == 21, "test0080-0340 failed")
    call check (range%iend0() == 38, "test0080-0350 failed")
    call check (range%iend1() == 39, "test0080-0360 failed")
    call check (range%iendn(2_sz) == 40, "test0080-0370 failed")
    call check (range%iendn(2) == 40, "test0080-0380 failed")

    vec = list_to_vectar (iota (100, 1))

    call check (list_equal (int_eq, vectar_to_list (vectar_range0 (vec, 9_sz, 18_sz)), iota (10, 10)), "test0080-1010 failed")
    call check (list_equal (int_eq, vectar_to_list (vectar_range1 (vec, 10_sz, 19_sz)), iota (10, 10)), "test0080-1020 failed")
    call check (list_equal (int_eq, vectar_to_list (vectar_rangen (vec, 2_sz, 11_sz, 20_sz)), iota (10, 10)), &
         &      "test0080-1030 failed")
    call check (list_equal (int_eq, vectar_to_list (vectar_range1 (vec, 1_sz, 100_sz)), iota (100, 1)), "test0080-1040 failed")

    call check (list_equal (int_eq, vectar_to_list (vectar_range0 (vec, 9, 18)), iota (10, 10)), "test0080-1110 failed")
    call check (list_equal (int_eq, vectar_to_list (vectar_range1 (vec, 10, 19)), iota (10, 10)), "test0080-1120 failed")
    call check (list_equal (int_eq, vectar_to_list (vectar_rangen (vec, 2, 11, 20)), iota (10, 10)), "test0080-1130 failed")
    call check (list_equal (int_eq, vectar_to_list (vectar_range1 (vec, 1, 100)), iota (100, 1)), "test0080-1140 failed")

    call check (list_equal (int_eq, reverse_vectar_to_list (vectar_range0 (vec, 9_sz, 18_sz)), iota (10, 19, -1)), &
         &      "test0080-1210 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (vectar_range1 (vec, 10_sz, 19_sz)), iota (10, 19, -1)), &
         &      "test0080-1220 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (vectar_rangen (vec, 2_sz, 11_sz, 20_sz)), iota (10, 19, -1)), &
         &      "test0080-1230 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (vectar_range1 (vec, 1_sz, 100_sz)), iota (100, 100, -1)), &
         &      "test0080-1240 failed")

    vec = list_to_vectar (iota (100, 1))

    call check (list_equal (int_eq, vectar_to_list (range0 (vec, 9_sz, 18_sz)), iota (10, 10)), "test0080-2010 failed")
    call check (list_equal (int_eq, vectar_to_list (range1 (vec, 10_sz, 19_sz)), iota (10, 10)), "test0080-2020 failed")
    call check (list_equal (int_eq, vectar_to_list (rangen (vec, 2_sz, 11_sz, 20_sz)), iota (10, 10)), "test0080-2030 failed")
    call check (list_equal (int_eq, vectar_to_list (range1 (vec, 1_sz, 100_sz)), iota (100, 1)), "test0080-2040 failed")

    call check (list_equal (int_eq, vectar_to_list (range0 (vec, 9, 18)), iota (10, 10)), "test0080-2110 failed")
    call check (list_equal (int_eq, vectar_to_list (range1 (vec, 10, 19)), iota (10, 10)), "test0080-2120 failed")
    call check (list_equal (int_eq, vectar_to_list (rangen (vec, 2, 11, 20)), iota (10, 10)), "test0080-2130 failed")
    call check (list_equal (int_eq, vectar_to_list (range1 (vec, 1, 100)), iota (100, 1)), "test0080-2140 failed")

    call check (list_equal (int_eq, reverse_vectar_to_list (range0 (vec, 9_sz, 18_sz)), iota (10, 19, -1)), &
         &      "test0080-2210 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (range1 (vec, 10_sz, 19_sz)), iota (10, 19, -1)), &
         &      "test0080-2220 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (rangen (vec, 2_sz, 11_sz, 20_sz)), iota (10, 19, -1)), &
         &      "test0080-2230 failed")
    call check (list_equal (int_eq, reverse_vectar_to_list (range1 (vec, 1_sz, 100_sz)), iota (100, 100, -1)), &
         &      "test0080-2240 failed")
  end subroutine test0080

  subroutine test0090
    type(vectar_t) :: vec1, vec2
    integer :: i

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vectar_copy (vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (100, 1))), "test0090-0010 failed")
    do i = 1, 100
       call vectar_set1 (vec1, i, 101 - i)
    end do
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (100, 100, -1))), "test0090-0020 failed")
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (100, 1))), "test0090-0030 failed")

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vectar_copy (range1 (vec1, 25, 74))
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (100, 1))), "test0090-0040 failed")
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (50, 25))), "test0090-0050 failed")

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vectar_copy (range1 (vec1, 25, -1))
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (100, 1))), "test0090-0060 failed")
    call check (vectar_equal (int_eq, vec2, list_to_vectar (nil)), "test0090-0070 failed")
  end subroutine test0090

  subroutine test0100
    type(vectar_t) :: vec1, vec2
    integer :: i

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vectar_reverse_copy (vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (100, 100, -1))), "test0100-0010 failed")
    do i = 1, 100
       call vectar_set1 (vec1, i, -i)
    end do
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (100, -1, -1))), "test0100-0020 failed")
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (100, 100, -1))), "test0100-0030 failed")

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vectar_reverse_copy (range1 (vec1, 25, 74))
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (100, 1))), "test0100-0040 failed")
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (50, 74, -1))), "test0100-0050 failed")

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vectar_reverse_copy (range1 (vec1, 25, -1))
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (100, 1))), "test0100-0060 failed")
    call check (vectar_equal (int_eq, vec2, list_to_vectar (nil)), "test0100-0070 failed")
  end subroutine test0100

  subroutine run_tests
    heap_size_limit = 0

    call test0010
    call test0015
    call test0020
    call test0030
    call test0040
    call test0050
    call test0060
    call test0070
    call test0080
    call test0090
    call test0100

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
