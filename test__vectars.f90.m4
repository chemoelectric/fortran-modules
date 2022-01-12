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
    type(gcroot_t) :: vec1, vec2, vec3, vec4

    call check (vectar_equal (str_t_eq_gc), "test0040-0110 failed")
    call check (vectar_equal (str_t_eq_gc, vectar ()), "test0040-0120 failed")
    call check (vectar_equal (str_t_eq_gc, vectar (), vectar ()), "test0040-0130 failed")
    call check (vectar_equal (str_t_eq_gc, vectar (), vectar (), vectar ()), "test0040-0140 failed")

    vec1 = list_to_vectar (iota (100, 1))
    vec2 = vec1
    vec3 = list_to_vectar (iota (100, 1))
    vec4 = list_to_vectar (iota (100, 100, -1))
    call check (vectar_equal (int_eq_gc, vec1, vec2, vec3), "test0040-0200 failed")
    call check (.not. vectar_equal (int_eq_gc, vec1, vec2, vec3, vec4), "test0040-0210 failed")
    call check (.not. vectar_equal (int_eq_gc, vec1, vec2, vec3, vectar ()), "test0040-0220 failed")
    call check (.not. vectar_equal (int_eq_gc, vec1, vec2, vec3, vec4, vec2, vec3), "test0040-0230 failed")
    call check (.not. vectar_equal (int_eq_gc, vec1, vectar (), vec2, vec3), "test0040-0240 failed")
    call check (vectar_equal (int_eq_gc, vec1, vec2, vec3, vec2, vec3), "test0040-0270 failed")
    call check (vectar_equal (int_eq_gc, vec1), "test0040-0280 failed")

    vec1 = vectar (str_t ('a'), str_t ('b'), str_t ('c'))
    vec2 = vectar (str_t ('a'), str_t ('b'), str_t ('c'))
    vec3 = vectar (str_t ('a'), str_t ('b'), str_t ('x'))
    vec4 = vectar (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'))
    call check (vectar_equal (str_t_eq_gc, vec1, vec2), "test0040-1020 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec1, vec3), "test0040-1030 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec1, vec2, vec3), "test0040-1040 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec1, vec3, vec2), "test0040-1050 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec3, vec2, vec1), "test0040-1060 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec1, vec2, vec4), "test0040-1070 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec1, vec4, vec2), "test0040-1080 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec4, vec2, vec1), "test0040-1090 failed")
    call check (.not. vectar_equal (str_t_eq_gc, vec3, vec4), "test0040-1100 failed")

    ! Test vectar ranges.
    call check (vectar_equal (int_eq_gc, range1 (vectar (1, 2, 3, 4, 5), 2, 4), &
         &                               range1 (vectar (10, 20, 30, 1, 2, 3, 4), 5, 7), &
         &                               vectar (2, 3, 4)), &
         &      "test0040-2010 failed")
    call check (.not. vectar_equal (int_eq_gc, range1 (vectar (1, 2, 3, 4, 5), 2, 5), &
         &                                     range1 (vectar (10, 20, 30, 1, 2, 3, 4), 5, 7), &
         &                                     vectar (2, 3, 4)), &
         &      "test0040-2020 failed")
    call check (.not. vectar_equal (int_eq_gc, range1 (vectar (1, 2, 3, 4, 5), 2, 4), &
         &                                     range1 (vectar (10, 20, 30, 1, 2, 3, 4), 5, 7), &
         &                                     vectar (2, 30, 4)), &
         &      "test0040-2030 failed")

  contains

    function str_t_eq_gc (obj1, obj2) result (bool)
      class(*), intent(in) :: obj1, obj2
      logical :: bool

      call collect_garbage_now

      bool = str_t_eq (obj1, obj2)
    end function str_t_eq_gc

    function int_eq_gc (obj1, obj2) result (bool)
      class(*), intent(in) :: obj1, obj2
      logical :: bool

      call collect_garbage_now

      bool = int_eq (obj1, obj2)
    end function int_eq_gc

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

  subroutine test0110
    type(cons_t) :: vectars
    integer :: n, i

    call check (vectar_equal (int_eq, vectar_append (), vectar ()), &
         &      "test0110-0010 failed")
    call check (vectar_equal (int_eq, vectar_append (vectar (1, 2)), vectar (1, 2)), &
         &      "test0110-0020 failed")
    call check (vectar_equal (int_eq, vectar_append (vectar (1, 2), vectar (3, 4)), vectar (1, 2, 3, 4)), &
         &      "test0110-0030 failed")
    call check (vectar_equal (int_eq, vectar_append (vectar (1), vectar (2), vectar (3), vectar (4)), vectar (1, 2, 3, 4)), &
         &      "test0110-0040 failed")
    call check (vectar_equal (int_eq, vectar_append (range1 (vectar (1, 2, 3, 4), 2, 3)), vectar (2, 3)), &
         &      "test0110-0050 failed")
    call check (vectar_equal (int_eq, vectar_append (range1 (vectar (1, 2, 3, 4), 1, 1), &
         &                                           range1 (vectar (1, 2, 3, 4), 2, 3)), &
         &                    vectar (1, 2, 3)), &
         &      "test0110-0060 failed")

    call check (vectar_equal (int_eq, vectar_concatenate (nil), vectar ()), &
         &      "test0110-1010 failed")
    call check (vectar_equal (int_eq, vectar_concatenate (list (vectar (1, 2))), vectar (1, 2)), &
         &      "test0110-1020 failed")
    call check (vectar_equal (int_eq, vectar_concatenate (list (vectar (1, 2), vectar (3, 4))), vectar (1, 2, 3, 4)), &
         &      "test0110-1020 failed")
    call check (vectar_equal (int_eq, vectar_concatenate (list (range1 (vectar (1, 2, 3, 4), 3, 4), &
         &                                                      range1 (vectar (1, 2, 3, 4), 1, 3))), &
         &                    vectar (3, 4, 1, 2, 3)), &
         &      "test0110-1030 failed")

    do n = 0, LISTN_MAX + 5
       vectars = nil
       do i = n, 1, -1
          vectars = vectar (i) ** vectars
       end do
       call check (vectar_equal (int_eq, vectar_concatenate (vectars), list_to_vectar (iota (n, 1))), "test0110-2030 failed")
    end do
  end subroutine test0110

  subroutine test0120
    type(vectar_t) :: vec

    vec = vectar (1, 2, 3, 4, 5)
    call vectar_fillx (vec, str_t ("X"))
    call check (vectar_equal (str_t_eq, vec, vectar (str_t ("X"), str_t ("X"),  str_t ("X"),  str_t ("X"),  str_t ("X"))), &
         &      "test0120-0010 failed")

    vec = vectar (1, 2, 3, 4, 5)
    call vectar_fillx (range1 (vec, 2, 4), str_t ("X"))
    call check (vectar_ref1 (vec, 1) .eqi. 1, "test0120-0020 failed")
    call check (vectar_ref1 (vec, 2) .eqs. str_t ("X"), "test0120-0030 failed")
    call check (vectar_ref1 (vec, 3) .eqs. str_t ("X"), "test0120-0040 failed")
    call check (vectar_ref1 (vec, 4) .eqs. str_t ("X"), "test0120-0050 failed")
    call check (vectar_ref1 (vec, 5) .eqi. 5, "test0120-0060 failed")
  end subroutine test0120

  subroutine test0130
    type(vectar_t) :: vec
    integer :: i

    vec = vectar ()
    call vectar_reversex (vec)
    call check (vectar_equal (int_eq, vec, vectar ()), "test0130-0010 failed")

    vec = vectar (123)
    call vectar_reversex (vec)
    call check (vectar_equal (int_eq, vec, vectar (123)), "test0130-0020 failed")

    vec = vectar (123, 456)
    call vectar_reversex (vec)
    call check (vectar_equal (int_eq, vec, vectar (456, 123)), "test0130-0030 failed")

    vec = vectar (123, 456, 789)
    call vectar_reversex (vec)
    call check (vectar_equal (int_eq, vec, vectar (789, 456, 123)), "test0130-0040 failed")

    vec = vectar (1, 2, 3, 4)
    call vectar_reversex (vec)
    call check (vectar_equal (int_eq, vec, vectar (4, 3, 2, 1)), "test0130-0050 failed")

    vec = vectar (1, 2, 3, 4, 5)
    call vectar_reversex (vec)
    call check (vectar_equal (int_eq, vec, vectar (5, 4, 3, 2, 1)), "test0130-0060 failed")

    vec = vectar (1, 2, 3, 4, 5)
    call vectar_reversex (range1 (vec, 1, 3))
    call check (vectar_equal (int_eq, vec, vectar (3, 2, 1, 4, 5)), "test0130-0070 failed")

    vec = vectar (1, 2, 3, 4, 5)
    call vectar_reversex (range1 (vec, 2, 4))
    call check (vectar_equal (int_eq, vec, vectar (1, 4, 3, 2, 5)), "test0130-0080 failed")

    vec = vectar (1, 2, 3, 4, 5)
    call vectar_reversex (range1 (vec, 2, 5))
    call check (vectar_equal (int_eq, vec, vectar (1, 5, 4, 3, 2)), "test0130-0090 failed")

    vec = vectar (1, 2, 3, 4, 5)
    do i = 1, 5
       call vectar_reversex (range1 (vec, i, i))
       call check (vectar_equal (int_eq, vec, vectar (1, 2, 3, 4, 5)), "test0130-0100 failed")
    end do

    vec = vectar (1, 2, 3, 4, 5)
    do i = -60, 60        ! A range of length zero may start anywhere.
       call vectar_reversex (range1 (vec, i, -huge(1)))
       call check (vectar_equal (int_eq, vec, vectar (1, 2, 3, 4, 5)), "test0130-0110 failed")
    end do
  end subroutine test0130

  subroutine test0140
    type(vectar_t) :: vec1, vec2

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx0 (vec1, 0_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 4, 5)), "test0140-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx0 (vec1, 1_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx0 (vec1, 2_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 10, 20, 30)), "test0140-0050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0060 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx0 (vec1, 0_sz, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 40, 5)), "test0140-0070 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0080 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx0 (vec1, 1_sz, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 40)), "test0140-0090 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0100 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx0 (vec1, 1_sz, range0 (vec2, 0, 2))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0110 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0120 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx1 (vec1, 1_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 4, 5)), "test0140-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx1 (vec1, 2_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx1 (vec1, 3_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 10, 20, 30)), "test0140-0050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0060 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx1 (vec1, 1_sz, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 40, 5)), "test0140-0070 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0080 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx1 (vec1, 2_sz, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 40)), "test0140-0090 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0100 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx1 (vec1, 2_sz, range0 (vec2, 0, 2))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0110 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0120 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyxn (vec1, 2_sz, 2_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 4, 5)), "test0140-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyxn (vec1, 2_sz, 3_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyxn (vec1, 2_sz, 4_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 10, 20, 30)), "test0140-0050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0060 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyxn (vec1, 2_sz, 2_sz, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 40, 5)), "test0140-0070 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0080 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyxn (vec1, 2_sz, 3_sz, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 40)), "test0140-0090 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0100 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyxn (vec1, 2_sz, 3_sz, range0 (vec2, 0, 2))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0110 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0120 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx0 (vec1, 0, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 4, 5)), "test0140-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx0 (vec1, 1, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx0 (vec1, 2, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 10, 20, 30)), "test0140-0050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0060 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx0 (vec1, 0, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 40, 5)), "test0140-0070 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0080 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx0 (vec1, 1, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 40)), "test0140-0090 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0100 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx0 (vec1, 1, range0 (vec2, 0, 2))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0110 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0120 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx1 (vec1, 1, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 4, 5)), "test0140-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx1 (vec1, 2, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyx1 (vec1, 3, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 10, 20, 30)), "test0140-0050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0060 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx1 (vec1, 1, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 40, 5)), "test0140-0070 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0080 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx1 (vec1, 2, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 40)), "test0140-0090 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0100 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyx1 (vec1, 2, range0 (vec2, 0, 2))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0110 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0120 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyxn (vec1, 2, 2, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 4, 5)), "test0140-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyxn (vec1, 2, 3, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30)
    call vectar_copyxn (vec1, 2, 4, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 10, 20, 30)), "test0140-0050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30)), "test0140-0060 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyxn (vec1, 2, 2, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (10, 20, 30, 40, 5)), "test0140-0070 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0080 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyxn (vec1, 2, 3, range0 (vec2, 0, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 40)), "test0140-0090 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0100 failed")

    vec1 = vectar (1, 2, 3, 4, 5)
    vec2 = vectar (10, 20, 30, 40, 50, 60, 70, 80)
    call vectar_copyxn (vec1, 2, 3, range0 (vec2, 0, 2))
    call check (vectar_equal (int_eq, vec1, vectar (1, 10, 20, 30, 5)), "test0140-0110 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50, 60, 70, 80)), "test0140-0120 failed")

    ! Overlap, moving data rightwards.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_copyx1 (vec1, 2, range1 (vec1, 1, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 1, 2, 3, 5, 6, 7, 8)), "test0140-0130 failed")

    ! Overlap, moving data leftwards.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_copyx1 (vec1, 2, range1 (vec1, 4, 7))
    call check (vectar_equal (int_eq, vec1, vectar (1, 4, 5, 6, 7, 6, 7, 8)), "test0140-0140 failed")

    ! Overlap, moving data where it started.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_copyx1 (vec1, 4, range1 (vec1, 4, 7))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0140-0150 failed")
  end subroutine test0140

  subroutine test0150
    type(vectar_t) :: vec1, vec2

    ! Complete overlap, the entire vector. Simply reversal in place.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_reverse_copyx1 (vec1, 1, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (8, 7, 6, 5, 4, 3, 2, 1)), "test0150-0010 failed")

    ! Complete overlap, part of the vector. Reversal in place of that
    ! part of the vector.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_reverse_copyx1 (vec1, 2, range1 (vec1, 2, 5))
    call check (vectar_equal (int_eq, vec1, vectar (1, 5, 4, 3, 2, 6, 7, 8)), "test0150-0020 failed")

    ! Overlap, moving data to the left.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_reverse_copyx1 (vec1, 2, range1 (vec1, 5, 7))
    call check (vectar_equal (int_eq, vec1, vectar (1, 7, 6, 5, 5, 6, 7, 8)), "test0150-0030 failed")

    ! Overlap, moving data to the right.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_reverse_copyx1 (vec1, 4, range1 (vec1, 2, 5))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 5, 4, 3, 2, 8)), "test0150-0040 failed")
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_reverse_copyx1 (vec1, 5, range1 (vec1, 2, 5))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 4, 3, 2)), "test0150-0050 failed")

    ! No overlap, moving data around in a single vectar.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_reverse_copyx1 (vec1, 5, range1 (vec1, 1, 4))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 4, 3, 2, 1)), "test0150-0060 failed")
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_reverse_copyx1 (vec1, 1, range1 (vec1, 5, 8))
    call check (vectar_equal (int_eq, vec1, vectar (8, 7, 6, 5, 5, 6, 7, 8)), "test0150-0070 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyx0 (vec1, 2_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 40, 30, 20, 10, 7, 8)), "test0150-1010 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyx0 (vec1, 2, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 40, 30, 20, 10, 7, 8)), "test0150-1020 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyx1 (vec1, 2_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 40, 30, 20, 10, 6, 7, 8)), "test0150-1030 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyx1 (vec1, 2, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 40, 30, 20, 10, 6, 7, 8)), "test0150-1040 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyxn (vec1, 2_sz, 2_sz, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (40, 30, 20, 10, 5, 6, 7, 8)), "test0150-1050 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyxn (vec1, 2, 2, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (40, 30, 20, 10, 5, 6, 7, 8)), "test0150-1060 failed")

    ! Copy from a range.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyx1 (vec1, 2, range1 (vec2, 2, 3))
    call check (vectar_equal (int_eq, vec1, vectar (1, 30, 20, 4, 5, 6, 7, 8)), "test0150-1070 failed")

    ! Copy length zero.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40)
    call vectar_reverse_copyx1 (vec1, 2, range1 (vec2, 1, 0))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0150-1080 failed")

    ! Copy length zero.
    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar ()
    call vectar_reverse_copyx1 (vec1, 2, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0150-1090 failed")
  end subroutine test0150

  subroutine test0160
    type(vectar_t) :: vec1, vec2, vec3, vec4

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar_map (int_negate, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0160-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (-1, -2, -3, -4, -5, -6, -7, -8)), "test0160-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar_map (int_negate, range1 (vec1, 2, 5))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0160-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (-2, -3, -4, -5)), "test0160-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40, 50)
    vec3 = vectar (100, 200, 300, 400, 500, 600)
    vec4 = vectar_map (add3, vec1, vec2, vec3)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0160-1010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50)), "test0160-1020 failed")
    call check (vectar_equal (int_eq, vec3, vectar (100, 200, 300, 400, 500, 600)), "test0160-1030 failed")
    call check (vectar_equal (int_eq, vec4, vectar (111, 222, 333, 444, 555)), "test0160-1040 failed")


    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40, 50)
    vec3 = vectar (100, 200, 300, 400, 500, 600)
    vec4 = vectar_map (add3, range1 (vec1, 2, 5), range1 (vec2, 1, 4), range1 (vec3, 3, 6))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0160-1050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50)), "test0160-1060 failed")
    call check (vectar_equal (int_eq, vec3, vectar (100, 200, 300, 400, 500, 600)), "test0160-1070 failed")
    call check (vectar_equal (int_eq, vec4, vectar (312, 423, 534, 645)), "test0160-1080 failed")

  contains

    subroutine int_negate (x, y)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: y

      call collect_garbage_now

      y = -(int_cast (x))
    end subroutine int_negate

    subroutine add3 (x, y, z, sum)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      class(*), allocatable, intent(out) :: sum

      call collect_garbage_now

      sum = int_cast (x) + int_cast (y) + int_cast (z)
    end subroutine add3

  end subroutine test0160

  subroutine test0170
    type(vectar_t) :: vec1, vec2, vec3

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_mapx (int_negate, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (-1, -2, -3, -4, -5, -6, -7, -8)), "test0170-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_mapx (int_negate, range1 (vec1, 2, 5))
    call check (vectar_equal (int_eq, vec1, vectar (1, -2, -3, -4, -5, 6, 7, 8)), "test0170-0030 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40, 50)
    vec3 = vectar (100, 200, 300, 400, 500, 600)
    call vectar_mapx (add3, vec1, vec2, vec3)
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50)), "test0170-1020 failed")
    call check (vectar_equal (int_eq, vec3, vectar (100, 200, 300, 400, 500, 600)), "test0170-1030 failed")
    call check (vectar_equal (int_eq, vec1, vectar (111, 222, 333, 444, 555, 6, 7, 8)), "test0170-1040 failed")


    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40, 50)
    vec3 = vectar (100, 200, 300, 400, 500, 600)
    call vectar_mapx (add3, range1 (vec1, 2, 5), range1 (vec2, 1, 4), range1 (vec3, 3, 6))
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50)), "test0170-1060 failed")
    call check (vectar_equal (int_eq, vec3, vectar (100, 200, 300, 400, 500, 600)), "test0170-1070 failed")
    call check (vectar_equal (int_eq, vec1, vectar (1, 312, 423, 534, 645, 6, 7, 8)), "test0170-1050 failed")

  contains

    subroutine int_negate (x, y)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: y

      call collect_garbage_now

      y = -(int_cast (x))
    end subroutine int_negate

    subroutine add3 (x, y, z, sum)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      class(*), allocatable, intent(out) :: sum

      call collect_garbage_now

      sum = int_cast (x) + int_cast (y) + int_cast (z)
    end subroutine add3

  end subroutine test0170

  subroutine test0180
    type(vectar_t) :: vec

    ! Check vectar_length applied to a range.
    vec = make_vectar (100_sz, 123)
    call check (vectar_length (range1 (vec, 10, 19)) == 10, "test0170-0010 failed")

    ! Check vectar_is_empty applied to a range.
    vec = make_vectar (100_sz, 123)
    call check (.not. vectar_is_empty (range1 (vec, 10, 19)), "test0170-0020 failed")
    call check (vectar_is_empty (range1 (vec, 10, 9)), "test0170-0030 failed")

    ! Check vectar_refX on a range.
    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call check (vectar_ref1 (range1 (vec, 3, 7), 2) .eqi. 4, "test0170-0040 failed")

    ! Check vectar_setX on a range.
    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_set1 (range1 (vec, 3, 7), 5, -200)
    call check (vectar_equal (int_eq, vec, vectar (1, 2, 3, 4, 5, 6, -200, 8)), "test0170-0050 failed")

    ! Check vectar_swapX on a range.
    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_swap1 (range1 (vec, 3, 7), 2, 4)
    call check (vectar_equal (int_eq, vec, vectar (1, 2, 3, 6, 5, 4, 7, 8)), "test0170-0060 failed")
    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    call vectar_swap1 (range1 (vec, 3, 7), 2, 2)
    call check (vectar_equal (int_eq, vec, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0170-0070 failed")
  end subroutine test0180

  subroutine test0190
    type(vectar_t) :: vec1, vec2, vec3
    integer :: accumulator

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    accumulator = 100
    call vectar_for_each (accumulate, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0190-0010 failed")
    call check (accumulator == 100 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8, "test0190-0020 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    accumulator = 100
    call vectar_for_each (accumulate, range1 (vec1, 2, 5))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0190-0030 failed")
    call check (accumulator == 100 + 2 + 3 + 4 + 5, "test0190-0040 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40, 50)
    vec3 = vectar (100, 200, 300, 400, 500, 600)
    accumulator = -100
    call vectar_for_each (accumulate3, vec1, vec2, vec3)
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0190-1010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50)), "test0190-1020 failed")
    call check (vectar_equal (int_eq, vec3, vectar (100, 200, 300, 400, 500, 600)), "test0190-1030 failed")
    call check (accumulator == -100 + 1 + 2 + 3 + 4 + 5 + 10 + 20 + 30 + 40 + 50 + 100 + 200 + 300 + 400 + 500, &
                "test0190-1040 failed")

    vec1 = vectar (1, 2, 3, 4, 5, 6, 7, 8)
    vec2 = vectar (10, 20, 30, 40, 50)
    vec3 = vectar (100, 200, 300, 400, 500, 600)
    accumulator = -100
    call vectar_for_each (accumulate3, range1 (vec1, 2, 5), range1 (vec2, 1, 4), range1 (vec3, 3, 6))
    call check (vectar_equal (int_eq, vec1, vectar (1, 2, 3, 4, 5, 6, 7, 8)), "test0190-1050 failed")
    call check (vectar_equal (int_eq, vec2, vectar (10, 20, 30, 40, 50)), "test0190-1060 failed")
    call check (vectar_equal (int_eq, vec3, vectar (100, 200, 300, 400, 500, 600)), "test0190-1070 failed")
    call check (accumulator == -100 + 2 + 3 + 4 + 5 + 10 + 20 + 30 + 40 + 300 + 400 + 500 + 600, "test0190-1080 failed")

  contains

    subroutine accumulate (x)
      class(*), intent(in) :: x

      call collect_garbage_now

      accumulator = accumulator + (int_cast (x))
    end subroutine accumulate

    subroutine accumulate3 (x, y, z)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z

      call collect_garbage_now

      accumulator = accumulator + int_cast (x) + int_cast (y) + int_cast (z)
    end subroutine accumulate3

  end subroutine test0190

  subroutine test0200
    type(vectar_t) :: vec1, vec2

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar_cumulate (add2, 0, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)), "test0200-0010 failed")
    call check (vectar_equal (int_eq, vec2, vectar (3, 4, 8, 9, 14, 23, 25, 30, 36)), "test0200-0020 failed")

    ! These results were produced with vector-cumulate on CHICKEN
    ! Scheme 5.
    vec1 = vectar (3, 1, 4, 1, 5, 9)
    vec2 = vectar_cumulate (flip_kons, nil, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (3, 1, 4, 1, 5, 9)), "test0200-0030 failed")
    call check (vectar_length (vec2) == 6, "test0200-0040 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 1), list (3)), "test0200-0050 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 2), list (1, 3)), "test0200-0060 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 3), list (4, 1, 3)), "test0200-0070 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 4), list (1, 4, 1, 3)), "test0200-0080 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 5), list (5, 1, 4, 1, 3)), "test0200-0090 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 6), list (9, 5, 1, 4, 1, 3)), "test0200-0100 failed")

    vec1 = vectar (100, 100, 3, 1, 4, 1, 5, 9, 100)
    vec2 = vectar_cumulate (flip_kons, nil, range1 (vec1, 3, 8))
    call check (vectar_equal (int_eq, vec1, vectar (100, 100, 3, 1, 4, 1, 5, 9, 100)), "test0200-1030 failed")
    call check (vectar_length (vec2) == 6, "test0200-1040 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 1), list (3)), "test0200-1050 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 2), list (1, 3)), "test0200-1060 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 3), list (4, 1, 3)), "test0200-1070 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 4), list (1, 4, 1, 3)), "test0200-1080 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 5), list (5, 1, 4, 1, 3)), "test0200-1090 failed")
    call check (list_equal (int_eq, vectar_ref1 (vec2, 6), list (9, 5, 1, 4, 1, 3)), "test0200-1100 failed")

  contains

    subroutine add2 (x, y, sum)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), allocatable, intent(out) :: sum

      call collect_garbage_now

      sum = int_cast (x) + int_cast (y)
    end subroutine add2

    subroutine flip_kons (kdr, kar, kons_value)
      class(*), intent(in) :: kdr
      class(*), intent(in) :: kar
      class(*), allocatable, intent(out) :: kons_value

      call collect_garbage_now

      kons_value = cons (kar, kdr)
    end subroutine flip_kons

  end subroutine test0200

  subroutine test0210
    type(vectar_t) :: vec1, vec2
    integer(sz) :: count

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    count = vectar_count (is_even, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)), "test0210-0010 failed")
    call check (count == 3_sz, "test0210-0020 failed")

    ! An example from SRFI-133.
    vec1 = vectar (1, 3, 6, 9)
    vec2 = vectar (2, 4, 6, 8, 10, 12)
    count = vectar_count (less_than, vec1, vec2)
    call check (vectar_equal (int_eq, vec1, vectar (1, 3, 6, 9)), "test0210-0030 failed")
    call check (vectar_equal (int_eq, vec2, vectar (2, 4, 6, 8, 10, 12)), "test0210-0040 failed")
    call check (count == 2_sz, "test0210-0050 failed")

    vec1 = vectar (100, 100, 1, 3, 6, 9)
    vec2 = vectar (100, 100, 100, 2, 4, 6, 8, 10, 12)
    count = vectar_count (less_than, range1 (vec1, 3, 6), range1 (vec2, 4, 8))
    call check (vectar_equal (int_eq, vec1, vectar (100, 100, 1, 3, 6, 9)), "test0210-0060 failed")
    call check (vectar_equal (int_eq, vec2, vectar (100, 100, 100, 2, 4, 6, 8, 10, 12)), "test0210-0070 failed")
    call check (count == 2_sz, "test0210-0080 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x, y
      logical :: bool

      call collect_garbage_now

      bool = int_lt (x, y)
    end function less_than

  end subroutine test0210

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
    call test0110
    call test0120
    call test0130
    call test0140
    call test0150
    call test0160
    call test0170
    call test0180
    call test0190
    call test0200
    call test0210

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
