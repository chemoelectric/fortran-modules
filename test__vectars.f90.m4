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

  interface operator(.eqsz.)
     module procedure size_kind_eq
  end interface operator(.eqsz.)

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

  function size_kind_cast (obj) result (int)
    class(*), intent(in) :: obj
    integer(sz) :: int
    select type (obj)
    type is (integer(sz))
       int = obj
    class default
       call error_abort ("size_kind_cast of an incompatible object")
    end select
  end function size_kind_cast

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

  function logical_cast (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool
    select type (obj)
    type is (logical)
       bool = obj
    class default
       call error_abort ("logical_cast of an incompatible object")
    end select
  end function logical_cast

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

  function int_pair_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = (int_cast (car (obj1)) == int_cast (car (obj2))) .and. &
         & (int_cast (cdr (obj1)) == int_cast (cdr (obj2)))
  end function int_pair_eq

  function size_kind_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = size_kind_cast (obj1) == size_kind_cast (obj2)
  end function size_kind_eq

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

  subroutine test0220
    type(vectar_t) :: vec1
    integer :: sum

    ! An example from SRFI-133: find the longest string's length.
    vec1 = vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &         str_t ("spaces,"), str_t ("does"), str_t ("it?"))
    call check (vectar_fold (max_strlen, 0_sz, vec1) .eqsz. 7_sz, "test0220-0010 failed")
    call check (vectar_equal (str_t_eq, vec1, &
         &                    vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &                            str_t ("spaces,"), str_t ("does"), str_t ("it?"))), &
         &      "test0220-0020 failed")

    ! An example from SRFI-133: produce a list of the vectar's
    ! elements, reversed.
    vec1 = vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &         str_t ("spaces,"), str_t ("does"), str_t ("it?"))
    call check (list_equal (str_t_eq, vectar_fold (kons_list, nil, vec1), &
         &                  reverse (list (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &                                 str_t ("spaces,"), str_t ("does"), str_t ("it?")))), &
         &      "test0220-0030 failed")
    call check (vectar_equal (str_t_eq, vec1, &
         &                    vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &                            str_t ("spaces,"), str_t ("does"), str_t ("it?"))), &
         &      "test0220-0040 failed")

    ! An example from SRFI-133: count the even numbers in a vectar.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
    call check (vectar_fold (count_even, 0_sz, vec1) .eqsz. 3_sz, "test0220-0050 failed")
    call check (vectar_equal (int_eq, vec1, vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), "test0220-0060 failed")

    ! Use three ranges of the same vector.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
    sum = int_cast (vectar_fold (add3, 100, range1 (vec1, 1, 5), range1 (vec1, 4, 7), range1 (vec1, 8, 11)))
    call check (sum == 100 + 3 + 1 + 4 + 1 + 1 + 5 + 9 + 2 + 6 + 5 + 3 + 5, "test0220-0070 failed")
    call check (vectar_equal (int_eq, vec1, vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), "test0220-0080 failed")

  contains

    subroutine max_strlen (len, str, the_greater)
      class(*), intent(in) :: len
      class(*), intent(in) :: str
      class(*), allocatable, intent(out) :: the_greater

      type(str_t) :: s
      integer(sz) :: len_s

      s = str_t_cast (str)
      len_s = s%length()
      the_greater = max (len_s, size_kind_cast (len))
    end subroutine max_strlen

    subroutine kons_list (tail, element, output)
      class(*), intent(in) :: tail
      class(*), intent(in) :: element
      class(*), allocatable, intent(out) :: output

      output = cons (element, tail)
    end subroutine kons_list

    subroutine count_even (state, number, next_state)
      class(*), intent(in) :: state
      class(*), intent(in) :: number
      class(*), allocatable, intent(out) :: next_state

      if (mod (int_cast (number), 2) == 0) then
         next_state = size_kind_cast (state) + 1_sz
      else
         next_state = size_kind_cast (state)
      end if
    end subroutine count_even

    subroutine add3 (old_sum, x, y, z, sum)
      class(*), intent(in) :: old_sum
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      class(*), allocatable, intent(out) :: sum

      sum = int_cast (old_sum) + int_cast (x) + int_cast (y) + int_cast (z)
    end subroutine add3

  end subroutine test0220

  subroutine test0230
    type(vectar_t) :: vec1
    integer :: sum

    ! Find the longest string's length (working right-to-left).
    vec1 = vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &         str_t ("spaces,"), str_t ("does"), str_t ("it?"))
    call check (vectar_fold_right (max_strlen, 0_sz, vec1) .eqsz. 7_sz, "test0230-0010 failed")
    call check (vectar_equal (str_t_eq, vec1, &
         &                    vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &                            str_t ("spaces,"), str_t ("does"), str_t ("it?"))), &
         &      "test0230-0020 failed")

    ! An example from SRFI-133: produce a list of the vectar's
    ! elements, in original order.
    vec1 = vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &         str_t ("spaces,"), str_t ("does"), str_t ("it?"))
    call check (list_equal (str_t_eq, vectar_fold_right (kons_list, nil, vec1), &
         &                  list (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &                        str_t ("spaces,"), str_t ("does"), str_t ("it?"))), &
         &      "test0230-0030 failed")
    call check (vectar_equal (str_t_eq, vec1, &
         &                    vectar (str_t ("This"), str_t ("string"), str_t ("has"), str_t ("no"), &
         &                            str_t ("spaces,"), str_t ("does"), str_t ("it?"))), &
         &      "test0230-0040 failed")

    ! An example from SRFI-133: count the even numbers in a vectar
    ! (working right-to-left).
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
    call check (vectar_fold_right (count_even, 0_sz, vec1) .eqsz. 3_sz, "test0230-0050 failed")
    call check (vectar_equal (int_eq, vec1, vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), "test0230-0060 failed")

    ! Use three ranges of the same vector (working right-to-left).
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
    sum = int_cast (vectar_fold_right (add3, 100, range1 (vec1, 1, 5), range1 (vec1, 4, 7), range1 (vec1, 8, 11)))
    call check (sum == 100 + 3 + 1 + 4 + 1 + 1 + 5 + 9 + 2 + 6 + 5 + 3 + 5, "test0230-0070 failed")
    call check (vectar_equal (int_eq, vec1, vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), "test0230-0080 failed")

  contains

    subroutine max_strlen (len, str, the_greater)
      class(*), intent(in) :: len
      class(*), intent(in) :: str
      class(*), allocatable, intent(out) :: the_greater

      type(str_t) :: s
      integer(sz) :: len_s

      s = str_t_cast (str)
      len_s = s%length()
      the_greater = max (len_s, size_kind_cast (len))
    end subroutine max_strlen

    subroutine kons_list (tail, element, output)
      class(*), intent(in) :: tail
      class(*), intent(in) :: element
      class(*), allocatable, intent(out) :: output

      output = cons (element, tail)
    end subroutine kons_list

    subroutine count_even (state, number, next_state)
      class(*), intent(in) :: state
      class(*), intent(in) :: number
      class(*), allocatable, intent(out) :: next_state

      if (mod (int_cast (number), 2) == 0) then
         next_state = size_kind_cast (state) + 1_sz
      else
         next_state = size_kind_cast (state)
      end if
    end subroutine count_even

    subroutine add3 (old_sum, x, y, z, sum)
      class(*), intent(in) :: old_sum
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      class(*), allocatable, intent(out) :: sum

      sum = int_cast (old_sum) + int_cast (x) + int_cast (y) + int_cast (z)
    end subroutine add3

  end subroutine test0230

  subroutine test0240
    type(vectar_t) :: vec
    type(gcroot_t) :: vec_root

    ! An example from SRFI-133.
    vec = vectar_unfold (decrement, 10_sz, 0)
    call check (vectar_equal (int_eq, vec, vectar (0, -1, -2, -3, -4, -5, -6, -7, -8, -9)), "test0240-0010 failed")
    vec = vectar_unfold (decrement, 10, 0)
    call check (vectar_equal (int_eq, vec, vectar (0, -1, -2, -3, -4, -5, -6, -7, -8, -9)), "test0240-0020 failed")
    vec = make_vectar (10)
    call vectar_unfoldx (decrement, vec, 0)
    call check (vectar_equal (int_eq, vec, vectar (0, -1, -2, -3, -4, -5, -6, -7, -8, -9)), "test0240-0030 failed")
    vec = make_vectar (10, 100)
    call vectar_unfoldx (decrement, range1 (vec, 2, 5), 10)
    call check (vectar_equal (int_eq, vec, vectar (100, 10, 9, 8, 7, 100, 100, 100, 100, 100)), "test0240-0040 failed")

    ! An example from SRFI-133: an `iota' implementation for vectars.
    vec = vectar_unfold (index_passthru, 5_sz)
    call check (vectar_equal (size_kind_eq, vec, vectar (0_sz, 1_sz, 2_sz, 3_sz, 4_sz)), "test0240-0110 failed")
    vec = vectar_unfold (index_passthru, 5)
    call check (vectar_equal (size_kind_eq, vec, vectar (0_sz, 1_sz, 2_sz, 3_sz, 4_sz)), "test0240-0120 failed")
    vec = make_vectar (5)
    call vectar_unfoldx (index_passthru, vec)
    call check (vectar_equal (size_kind_eq, vec, vectar (0_sz, 1_sz, 2_sz, 3_sz, 4_sz)), "test0240-0130 failed")
    vec = make_vectar (7, 100_sz)
    call vectar_unfoldx (index_passthru, range1 (vec, 2, 6))
    call check (vectar_equal (size_kind_eq, vec, vectar (100_sz, 0_sz, 1_sz, 2_sz, 3_sz, 4_sz, 100_sz)), &
         &      "test0240-0140 failed")

    ! An example from SRFI-133: copy a vectar.
    vec_root = vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
    vec = vectar_unfold (vecref, vectar_length (vec_root))
    call check (vectar_equal (int_eq, vec_root, vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), "test0240-0210 failed")
    call check (vectar_equal (int_eq, vec, vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), "test0240-0220 failed")

    vec = vectar_unfold (increment3seeds, 5, 1, 2, 3)
    call check (vectar_length (vec) == 5, "test0240-0310 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 1), vectar (1, 2, 3)), "test0240-0320 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 2), vectar (2, 3, 4)), "test0240-0330 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 3), vectar (3, 4, 5)), "test0240-0340 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 4), vectar (4, 5, 6)), "test0240-0350 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 5), vectar (5, 6, 7)), "test0240-0360 failed")

  contains

    subroutine decrement (i, x, element)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(inout) :: x
      class(*), allocatable, intent(out) :: element

      call collect_garbage_now

      element = x
      x = int_cast (x) - 1
    end subroutine decrement

    subroutine index_passthru (i, element)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(out) :: element

      call collect_garbage_now

      element = i
    end subroutine index_passthru

    subroutine vecref (i, element)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(out) :: element

      call collect_garbage_now

      element = vectar_ref0 (vec_root, i)
    end subroutine vecref

    subroutine increment3seeds (i, seed1, seed2, seed3, element)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(inout) :: seed1, seed2, seed3
      class(*), allocatable, intent(out) :: element

      element = vectar (seed1, seed2, seed3)
      seed1 = int_cast (seed1) + 1
      seed2 = int_cast (seed2) + 1
      seed3 = int_cast (seed3) + 1
    end subroutine increment3seeds

  end subroutine test0240

  subroutine test0250
    type(vectar_t) :: vec
    type(gcroot_t) :: vec_root

    ! An example from SRFI-133: construct a vector of pairs of
    ! non-negative integers that add up to 4.
    vec = vectar_unfold_right (f1, 5_sz, 0)
    call check (vectar_equal (int_pair_eq, vec, vectar (cons (0, 4), cons (1, 3), cons (2, 2), cons (3, 1), cons (4, 0))), &
         &      "test0250-0010 failed")
    vec = vectar_unfold_right (f1, 5, 0)
    call check (vectar_equal (int_pair_eq, vec, vectar (cons (0, 4), cons (1, 3), cons (2, 2), cons (3, 1), cons (4, 0))), &
         &      "test0250-0020 failed")
    vec = make_vectar (5)
    call vectar_unfold_rightx (f1, vec, 0)
    call check (vectar_equal (int_pair_eq, vec, vectar (cons (0, 4), cons (1, 3), cons (2, 2), cons (3, 1), cons (4, 0))), &
         &      "test0250-0030 failed")
    vec = make_vectar (7, cons (10, 10))
    call vectar_unfold_rightx (f1, range1 (vec, 2, 6), 0)
    call check (vectar_equal (int_pair_eq, vec, vectar (cons (10, 10), cons (0, 4), cons (1, 3), cons (2, 2), &
         &                                              cons (3, 1), cons (4, 0), cons (10, 10))), &
         &      "test0250-0040 failed")

    ! An example from SRFI-133: reverse a vectar.
    vec_root = vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
    vec = vectar_unfold_right (vecref, vectar_length (vec_root), 0)
    call check (vectar_equal (int_eq, vec_root, vectar (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), "test0250-0110 failed")
    call check (vectar_equal (int_eq, vec, vectar (5, 3, 5, 6, 2, 9, 5, 1, 4, 1, 3)), "test0250-0120 failed")

    vec = vectar_unfold_right (increment3seeds, 5, 1, 2, 3)
    call check (vectar_length (vec) == 5, "test0240-0310 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 5), vectar (1, 2, 3)), "test0240-0320 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 4), vectar (2, 3, 4)), "test0240-0330 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 3), vectar (3, 4, 5)), "test0240-0340 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 2), vectar (4, 5, 6)), "test0240-0350 failed")
    call check (vectar_equal (int_eq, vectar_ref1 (vec, 1), vectar (5, 6, 7)), "test0240-0360 failed")

  contains

    subroutine f1 (i, x, element)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(inout) :: x
      class(*), allocatable, intent(out) :: element

      call collect_garbage_now

      element = cons (int (i), x)
      x = int_cast (x) + 1
    end subroutine f1

    subroutine vecref (i, x, element)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(inout) :: x
      class(*), allocatable, intent(out) :: element

      integer :: j

      call collect_garbage_now

      j = int_cast (x)
      element = vectar_ref0 (vec_root, j)
      x = j + 1
    end subroutine vecref

    subroutine increment3seeds (i, seed1, seed2, seed3, element)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(inout) :: seed1, seed2, seed3
      class(*), allocatable, intent(out) :: element

      element = vectar (seed1, seed2, seed3)
      seed1 = int_cast (seed1) + 1
      seed2 = int_cast (seed2) + 1
      seed3 = int_cast (seed3) + 1
    end subroutine increment3seeds

  end subroutine test0250

  subroutine test0260
    type(vectar_t) :: vec1, vec2
    type(vectar_range_t) :: vecr1, vecr2

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9)
    call check (vectar_index0 (is_even, vec1) == 2_sz, "test0260-0010 failed")
    call check (vectar_index1 (is_even, vec1) == 3_sz, "test0260-0020 failed")
    call check (vectar_indexn (is_even, 10_sz, vec1) == 12_sz, "test0260-0030 failed")

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_index0 (less_than, vec1, vec2) == 1_sz, "test0260-0110 failed")
    call check (vectar_index1 (less_than, vec1, vec2) == 2_sz, "test0260-0120 failed")
    call check (vectar_indexn (less_than, -10_sz, vec1, vec2) == -9_sz, "test0260-0130 failed")

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_index0 (equal, vec1, vec2) == -1_sz, "test0260-0210 failed")
    call check (vectar_index1 (equal, vec1, vec2) == -1_sz, "test0260-0220 failed")
    call check (vectar_indexn (equal, 10_sz, vec1, vec2) == -1_sz, "test0260-0230 failed")
    call check (vectar_indexn (equal, -10_sz, vec1, vec2) == -11_sz, "test0260-0240 failed")

    ! Check (by putting the satisfying elements last) whether the
    ! predicate is tested on all the necessary elements.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 5)
    call check (vectar_index0 (equal, vec1, vec2) == 4_sz, "test0260-0310 failed")
    call check (vectar_index1 (equal, vec1, vec2) == 5_sz, "test0260-0320 failed")
    call check (vectar_indexn (equal, 10_sz, vec1, vec2) == 14_sz, "test0260-0330 failed")
    call check (vectar_indexn (equal, -10_sz, vec1, vec2) == -6_sz, "test0260-0340 failed")

    vecr1 = range1 (vectar (3, 1, 4, 1, 5, 9, 2, 5, 6), 3, 6)
    vecr2 = range1 (vectar (2, 7, 1, 8, 2), 1, 4)
    call check (vectar_index0 (less_than, vecr1, vecr2) == 1_sz, "test0260-0410 failed")
    call check (vectar_index1 (less_than, vecr1, vecr2) == 2_sz, "test0260-0420 failed")
    call check (vectar_indexn (less_than, -10_sz, vecr1, vecr2) == -9_sz, "test0260-0430 failed")

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

    function equal (x, y) result (bool)
      class(*), intent(in) :: x, y
      logical :: bool

      call collect_garbage_now

      bool = (x .eqi. y)
    end function equal

  end subroutine test0260

  subroutine test0270
    type(vectar_t) :: vec1, vec2
    type(vectar_range_t) :: vecr1, vecr2

    vec1 = vectar (3, 1, 4, 1, 5, 9)
    call check (vectar_index_right0 (is_even, vec1) == 2_sz, "test0260-0010 failed")
    call check (vectar_index_right1 (is_even, vec1) == 3_sz, "test0260-0020 failed")
    call check (vectar_index_rightn (is_even, 10_sz, vec1) == 12_sz, "test0260-0030 failed")

    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_index_right0 (less_than, vec1, vec2) == 3_sz, "test0260-0110 failed")
    call check (vectar_index_right1 (less_than, vec1, vec2) == 4_sz, "test0260-0120 failed")
    call check (vectar_index_rightn (less_than, -10_sz, vec1, vec2) == -7_sz, "test0260-0130 failed")

    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_index_right0 (equal, vec1, vec2) == -1_sz, "test0260-0210 failed")
    call check (vectar_index_right1 (equal, vec1, vec2) == -1_sz, "test0260-0220 failed")
    call check (vectar_index_rightn (equal, 10_sz, vec1, vec2) == -1_sz, "test0260-0230 failed")
    call check (vectar_index_rightn (equal, -10_sz, vec1, vec2) == -11_sz, "test0260-0240 failed")

    ! Check (by putting the satisfying elements first) whether the
    ! predicate is tested on all the necessary elements.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (3, 7, 1, 8, 2)
    call check (vectar_index_right0 (equal, vec1, vec2) == 0_sz, "test0260-0310 failed")
    call check (vectar_index_right1 (equal, vec1, vec2) == 1_sz, "test0260-0320 failed")
    call check (vectar_index_rightn (equal, 10_sz, vec1, vec2) == 10_sz, "test0260-0330 failed")
    call check (vectar_index_rightn (equal, -10_sz, vec1, vec2) == -10_sz, "test0260-0340 failed")

    vecr1 = range1 (vectar (3, 1, 4, 1, 5, 9, 2, 5, 6), 3, 6)
    vecr2 = range1 (vectar (2, 7, 1, 8, 2), 1, 4)
    call check (vectar_index_right0 (less_than, vecr1, vecr2) == 1_sz, "test0260-0410 failed")
    call check (vectar_index_right1 (less_than, vecr1, vecr2) == 2_sz, "test0260-0420 failed")
    call check (vectar_index_rightn (less_than, -10_sz, vecr1, vecr2) == -9_sz, "test0260-0430 failed")

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

    function equal (x, y) result (bool)
      class(*), intent(in) :: x, y
      logical :: bool

      call collect_garbage_now

      bool = (x .eqi. y)
    end function equal

  end subroutine test0270

  subroutine test0280
    type(vectar_t) :: vec1, vec2
    type(vectar_range_t) :: vecr1, vecr2

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9)
    call check (vectar_skip0 (is_odd, vec1) == 2_sz, "test0280-0010 failed")
    call check (vectar_skip1 (is_odd, vec1) == 3_sz, "test0280-0020 failed")
    call check (vectar_skipn (is_odd, 10_sz, vec1) == 12_sz, "test0280-0030 failed")

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_skip0 (gte, vec1, vec2) == 1_sz, "test0280-0110 failed")
    call check (vectar_skip1 (gte, vec1, vec2) == 2_sz, "test0280-0120 failed")
    call check (vectar_skipn (gte, -10_sz, vec1, vec2) == -9_sz, "test0280-0130 failed")

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_skip0 (unequal, vec1, vec2) == -1_sz, "test0280-0210 failed")
    call check (vectar_skip1 (unequal, vec1, vec2) == -1_sz, "test0280-0220 failed")
    call check (vectar_skipn (unequal, 10_sz, vec1, vec2) == -1_sz, "test0280-0230 failed")
    call check (vectar_skipn (unequal, -10_sz, vec1, vec2) == -11_sz, "test0280-0240 failed")

    ! Check (by putting the satisfying elements last) whether the
    ! predicate is tested on all the necessary elements.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 5)
    call check (vectar_skip0 (unequal, vec1, vec2) == 4_sz, "test0280-0310 failed")
    call check (vectar_skip1 (unequal, vec1, vec2) == 5_sz, "test0280-0320 failed")
    call check (vectar_skipn (unequal, 10_sz, vec1, vec2) == 14_sz, "test0280-0330 failed")
    call check (vectar_skipn (unequal, -10_sz, vec1, vec2) == -6_sz, "test0280-0340 failed")

    vecr1 = range1 (vectar (3, 1, 4, 1, 5, 9, 2, 5, 6), 3, 6)
    vecr2 = range1 (vectar (2, 7, 1, 8, 2), 1, 4)
    call check (vectar_skip0 (gte, vecr1, vecr2) == 1_sz, "test0280-0410 failed")
    call check (vectar_skip1 (gte, vecr1, vecr2) == 2_sz, "test0280-0420 failed")
    call check (vectar_skipn (gte, -10_sz, vecr1, vecr2) == -9_sz, "test0280-0430 failed")

  contains

    function is_odd (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 1)
    end function is_odd

    function gte (x, y) result (bool)
      class(*), intent(in) :: x, y
      logical :: bool

      call collect_garbage_now

      bool = .not. int_lt (x, y)
    end function gte

    function unequal (x, y) result (bool)
      class(*), intent(in) :: x, y
      logical :: bool

      call collect_garbage_now

      bool = .not. (x .eqi. y)
    end function unequal

  end subroutine test0280

  subroutine test0290
    type(vectar_t) :: vec1, vec2
    type(vectar_range_t) :: vecr1, vecr2

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9)
    call check (vectar_skip_right0 (is_odd, vec1) == 2_sz, "test0290-0010 failed")
    call check (vectar_skip_right1 (is_odd, vec1) == 3_sz, "test0290-0020 failed")
    call check (vectar_skip_rightn (is_odd, 10_sz, vec1) == 12_sz, "test0290-0030 failed")

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_skip_right0 (gte, vec1, vec2) == 3_sz, "test0290-0110 failed")
    call check (vectar_skip_right1 (gte, vec1, vec2) == 4_sz, "test0290-0120 failed")
    call check (vectar_skip_rightn (gte, -10_sz, vec1, vec2) == -7_sz, "test0290-0130 failed")

    ! An example from SRFI-133.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (2, 7, 1, 8, 2)
    call check (vectar_skip_right0 (unequal, vec1, vec2) == -1_sz, "test0290-0210 failed")
    call check (vectar_skip_right1 (unequal, vec1, vec2) == -1_sz, "test0290-0220 failed")
    call check (vectar_skip_rightn (unequal, 10_sz, vec1, vec2) == -1_sz, "test0290-0230 failed")
    call check (vectar_skip_rightn (unequal, -10_sz, vec1, vec2) == -11_sz, "test0290-0240 failed")

    ! Check (by putting the satisfying elements first) whether the
    ! predicate is tested on all the necessary elements.
    vec1 = vectar (3, 1, 4, 1, 5, 9, 2, 5, 6)
    vec2 = vectar (3, 7, 1, 8, 2)
    call check (vectar_skip_right0 (unequal, vec1, vec2) == 0_sz, "test0290-0310 failed")
    call check (vectar_skip_right1 (unequal, vec1, vec2) == 1_sz, "test0290-0320 failed")
    call check (vectar_skip_rightn (unequal, 10_sz, vec1, vec2) == 10_sz, "test0290-0330 failed")
    call check (vectar_skip_rightn (unequal, -10_sz, vec1, vec2) == -10_sz, "test0290-0340 failed")

    vecr1 = range1 (vectar (3, 1, 4, 1, 5, 9, 2, 5, 6), 3, 6)
    vecr2 = range1 (vectar (2, 7, 1, 8, 2), 1, 4)
    call check (vectar_skip_right0 (gte, vecr1, vecr2) == 1_sz, "test0290-0410 failed")
    call check (vectar_skip_right1 (gte, vecr1, vecr2) == 2_sz, "test0290-0420 failed")
    call check (vectar_skip_rightn (gte, -10_sz, vecr1, vecr2) == -9_sz, "test0290-0430 failed")

  contains

    function is_odd (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 1)
    end function is_odd

    function gte (x, y) result (bool)
      class(*), intent(in) :: x, y
      logical :: bool

      call collect_garbage_now

      bool = .not. int_lt (x, y)
    end function gte

    function unequal (x, y) result (bool)
      class(*), intent(in) :: x, y
      logical :: bool

      call collect_garbage_now

      bool = .not. (x .eqi. y)
    end function unequal

  end subroutine test0290

  subroutine test0300
    type(vectar_t) :: vec
    type(vectar_range_t) :: vecr

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_binary_search0 (vec, 4, int_cmp) == 3_sz, "test0300-0010 failed")
    call check (vectar_binary_search1 (vec, 4, int_cmp) == 4_sz, "test0300-0020 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 4, int_cmp) == -23_sz, "test0300-0030 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_binary_search0 (vec, 7, int_cmp) == 6_sz, "test0300-0040 failed")
    call check (vectar_binary_search1 (vec, 7, int_cmp) == 7_sz, "test0300-0050 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 7, int_cmp) == -20_sz, "test0300-0060 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_binary_search0 (vec, 1, int_cmp) == 0_sz, "test0300-0070 failed")
    call check (vectar_binary_search1 (vec, 1, int_cmp) == 1_sz, "test0300-0080 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 1, int_cmp) == -26_sz, "test0300-0090 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_binary_search0 (vec, 10, int_cmp) == 9_sz, "test0300-0100 failed")
    call check (vectar_binary_search1 (vec, 10, int_cmp) == 10_sz, "test0300-0110 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 10, int_cmp) == -17_sz, "test0300-0120 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_binary_search0 (vec, 0, int_cmp) == -1_sz, "test0300-0130 failed")
    call check (vectar_binary_search1 (vec, 0, int_cmp) == -1_sz, "test0300-0140 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 0, int_cmp) == -27_sz, "test0300-0150 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_binary_search0 (vec, 11, int_cmp) == -1_sz, "test0300-0130 failed")
    call check (vectar_binary_search1 (vec, 11, int_cmp) == -1_sz, "test0300-0140 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 11, int_cmp) == -27_sz, "test0300-0150 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_binary_search0 (vecr, 7, int_cmp) == 4_sz, "test0300-0160 failed")
    call check (vectar_binary_search1 (vecr, 7, int_cmp) == 5_sz, "test0300-0170 failed")
    call check (vectar_binary_searchn (vecr, 2_sz, 7, int_cmp) == 6_sz, "test0300-0180 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_binary_search0 (vecr, 3, int_cmp) == 0_sz, "test0300-0190 failed")
    call check (vectar_binary_search1 (vecr, 3, int_cmp) == 1_sz, "test0300-0200 failed")
    call check (vectar_binary_searchn (vecr, 2_sz, 3, int_cmp) == 2_sz, "test0300-0210 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_binary_search0 (vecr, 8, int_cmp) == 5_sz, "test0300-0220 failed")
    call check (vectar_binary_search1 (vecr, 8, int_cmp) == 6_sz, "test0300-0230 failed")
    call check (vectar_binary_searchn (vecr, 2_sz, 8, int_cmp) == 7_sz, "test0300-0240 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_binary_search0 (vecr, 2, int_cmp) == -1_sz, "test0300-0250 failed")
    call check (vectar_binary_search1 (vecr, 2, int_cmp) == -1_sz, "test0300-0260 failed")
    call check (vectar_binary_searchn (vecr, 2_sz, 2, int_cmp) == -1_sz, "test0300-0270 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_binary_search0 (vecr, 9, int_cmp) == -1_sz, "test0300-0280 failed")
    call check (vectar_binary_search1 (vecr, 9, int_cmp) == -1_sz, "test0300-0290 failed")
    call check (vectar_binary_searchn (vecr, 2_sz, 9, int_cmp) == -1_sz, "test0300-0300 failed")

    vec = vectar (5)
    call check (vectar_binary_search0 (vec, 5, int_cmp) == 0_sz, "test0300-1010 failed")
    call check (vectar_binary_search1 (vec, 5, int_cmp) == 1_sz, "test0300-1020 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 5, int_cmp) == -26_sz, "test0300-1030 failed")

    vec = vectar (5)
    call check (vectar_binary_search0 (vec, 4, int_cmp) == -1_sz, "test0300-1040 failed")
    call check (vectar_binary_search1 (vec, 4, int_cmp) == -1_sz, "test0300-1050 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 4, int_cmp) == -27_sz, "test0300-1060 failed")

    vec = vectar (5)
    call check (vectar_binary_search0 (vec, 6, int_cmp) == -1_sz, "test0300-1070 failed")
    call check (vectar_binary_search1 (vec, 6, int_cmp) == -1_sz, "test0300-1080 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 6, int_cmp) == -27_sz, "test0300-1090 failed")

    vec = vectar ()
    call check (vectar_binary_search0 (vec, 5, int_cmp) == -1_sz, "test0300-2010 failed")
    call check (vectar_binary_search1 (vec, 5, int_cmp) == -1_sz, "test0300-2020 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 5, int_cmp) == -27_sz, "test0300-2030 failed")

    vec = vectar ()
    call check (vectar_binary_search0 (vec, 4, int_cmp) == -1_sz, "test0300-2040 failed")
    call check (vectar_binary_search1 (vec, 4, int_cmp) == -1_sz, "test0300-2050 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 4, int_cmp) == -27_sz, "test0300-2060 failed")

    vec = vectar ()
    call check (vectar_binary_search0 (vec, 6, int_cmp) == -1_sz, "test0300-2070 failed")
    call check (vectar_binary_search1 (vec, 6, int_cmp) == -1_sz, "test0300-2080 failed")
    call check (vectar_binary_searchn (vec, -26_sz, 6, int_cmp) == -27_sz, "test0300-2090 failed")

    vec = vectar (1, 2, 2, 2, 2, 3, 4, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 8, 9, 10)
    call check (vectar_ref0 (vec, vectar_binary_search0 (vec, 6, int_cmp)) .eqi. 6, "test0310-3010 failed")
    call check (vectar_ref1 (vec, vectar_binary_search1 (vec, 6, int_cmp)) .eqi. 6, "test0310-3020 failed")
    call check (vectar_refn (vec, -26_sz, vectar_binary_searchn (vec, -26_sz, 6, int_cmp)) .eqi. 6, &
         &      "test0310-3030 failed")

    vec = vectar (6, 6, 6, 6, 6, 6)
    call check (vectar_ref0 (vec, vectar_binary_search0 (vec, 6, int_cmp)) .eqi. 6, "test0310-3040 failed")
    call check (vectar_ref1 (vec, vectar_binary_search1 (vec, 6, int_cmp)) .eqi. 6, "test0310-3050 failed")
    call check (vectar_refn (vec, -26_sz, vectar_binary_searchn (vec, -26_sz, 6, int_cmp)) .eqi. 6, &
         &      "test0310-3060 failed")

  contains

    function int_cmp (x, y) result (sign)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      integer :: sign

      call collect_garbage_now

      sign = (int_cast (x) - int_cast (y))
    end function int_cmp

  end subroutine test0300

  subroutine test0310
    type(vectar_t) :: vec
    type(vectar_range_t) :: vecr

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, vec, 4) == 3_sz, "test0310-0010 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 4) == 4_sz, "test0310-0020 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 4) == -23_sz, "test0310-0030 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, vec, 7) == 6_sz, "test0310-0040 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 7) == 7_sz, "test0310-0050 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 7) == -20_sz, "test0310-0060 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, vec, 1) == 0_sz, "test0310-0070 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 1) == 1_sz, "test0310-0080 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 1) == -26_sz, "test0310-0090 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, vec, 10) == 9_sz, "test0310-0100 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 10) == 10_sz, "test0310-0110 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 10) == -17_sz, "test0310-0120 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, vec, 0) == -1_sz, "test0310-0130 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 0) == -1_sz, "test0310-0140 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 0) == -27_sz, "test0310-0150 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, vec, 11) == -1_sz, "test0310-0130 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 11) == -1_sz, "test0310-0140 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 11) == -27_sz, "test0310-0150 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, vecr, 7) == 4_sz, "test0310-0160 failed")
    call check (vectar_bottenbruch_search1 (less_than, vecr, 7) == 5_sz, "test0310-0170 failed")
    call check (vectar_bottenbruch_searchn (less_than, vecr, 2_sz, 7) == 6_sz, "test0310-0180 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, vecr, 3) == 0_sz, "test0310-0190 failed")
    call check (vectar_bottenbruch_search1 (less_than, vecr, 3) == 1_sz, "test0310-0200 failed")
    call check (vectar_bottenbruch_searchn (less_than, vecr, 2_sz, 3) == 2_sz, "test0310-0210 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, vecr, 8) == 5_sz, "test0310-0220 failed")
    call check (vectar_bottenbruch_search1 (less_than, vecr, 8) == 6_sz, "test0310-0230 failed")
    call check (vectar_bottenbruch_searchn (less_than, vecr, 2_sz, 8) == 7_sz, "test0310-0240 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, vecr, 2) == -1_sz, "test0310-0250 failed")
    call check (vectar_bottenbruch_search1 (less_than, vecr, 2) == -1_sz, "test0310-0260 failed")
    call check (vectar_bottenbruch_searchn (less_than, vecr, 2_sz, 2) == -1_sz, "test0310-0270 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, vecr, 9) == -1_sz, "test0310-0280 failed")
    call check (vectar_bottenbruch_search1 (less_than, vecr, 9) == -1_sz, "test0310-0290 failed")
    call check (vectar_bottenbruch_searchn (less_than, vecr, 2_sz, 9) == -1_sz, "test0310-0300 failed")

    vec = vectar (5)
    call check (vectar_bottenbruch_search0 (less_than, vec, 5) == 0_sz, "test0310-1010 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 5) == 1_sz, "test0310-1020 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 5) == -26_sz, "test0310-1030 failed")

    vec = vectar (5)
    call check (vectar_bottenbruch_search0 (less_than, vec, 4) == -1_sz, "test0310-1040 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 4) == -1_sz, "test0310-1050 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 4) == -27_sz, "test0310-1060 failed")

    vec = vectar (5)
    call check (vectar_bottenbruch_search0 (less_than, vec, 6) == -1_sz, "test0310-1070 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 6) == -1_sz, "test0310-1080 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 6) == -27_sz, "test0310-1090 failed")

    vec = vectar ()
    call check (vectar_bottenbruch_search0 (less_than, vec, 5) == -1_sz, "test0310-2010 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 5) == -1_sz, "test0310-2020 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 5) == -27_sz, "test0310-2030 failed")

    vec = vectar ()
    call check (vectar_bottenbruch_search0 (less_than, vec, 4) == -1_sz, "test0310-2040 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 4) == -1_sz, "test0310-2050 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 4) == -27_sz, "test0310-2060 failed")

    vec = vectar ()
    call check (vectar_bottenbruch_search0 (less_than, vec, 6) == -1_sz, "test0310-2070 failed")
    call check (vectar_bottenbruch_search1 (less_than, vec, 6) == -1_sz, "test0310-2080 failed")
    call check (vectar_bottenbruch_searchn (less_than, vec, -26_sz, 6) == -27_sz, "test0310-2090 failed")

    vec = vectar (1, 2, 2, 2, 2, 3, 4, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 8, 9, 10)
    call check (vectar_ref0 (vec, vectar_bottenbruch_search0 (less_than, vec, 6)) .eqi. 6, "test0310-3010 failed")
    call check (vectar_ref1 (vec, vectar_bottenbruch_search1 (less_than, vec, 6)) .eqi. 6, "test0310-3020 failed")
    call check (vectar_refn (vec, -26_sz, vectar_bottenbruch_searchn (less_than, vec, -26_sz, 6)) .eqi. 6, &
         &      "test0310-3030 failed")

    vec = vectar (6, 6, 6, 6, 6, 6)
    call check (vectar_ref0 (vec, vectar_bottenbruch_search0 (less_than, vec, 6)) .eqi. 6, "test0310-3040 failed")
    call check (vectar_ref1 (vec, vectar_bottenbruch_search1 (less_than, vec, 6)) .eqi. 6, "test0310-3050 failed")
    call check (vectar_refn (vec, -26_sz, vectar_bottenbruch_searchn (less_than, vec, -26_sz, 6)) .eqi. 6, &
         &      "test0310-3060 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

  end subroutine test0310

  subroutine test0320
    type(vectar_t) :: vec
    type(vectar_range_t) :: vecr

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 4) == 3_sz, "test0320-0010 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 4) == 4_sz, "test0320-0020 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 4) == -23_sz, "test0320-0030 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 7) == 6_sz, "test0320-0040 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 7) == 7_sz, "test0320-0050 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 7) == -20_sz, "test0320-0060 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 1) == 0_sz, "test0320-0070 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 1) == 1_sz, "test0320-0080 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 1) == -26_sz, "test0320-0090 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 10) == 9_sz, "test0320-0100 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 10) == 10_sz, "test0320-0110 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 10) == -17_sz, "test0320-0120 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 0) == -1_sz, "test0320-0130 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 0) == -1_sz, "test0320-0140 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 0) == -27_sz, "test0320-0150 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 11) == -1_sz, "test0320-0130 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 11) == -1_sz, "test0320-0140 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 11) == -27_sz, "test0320-0150 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, equal, vecr, 7) == 4_sz, "test0320-0160 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vecr, 7) == 5_sz, "test0320-0170 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vecr, 2_sz, 7) == 6_sz, "test0320-0180 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, equal, vecr, 3) == 0_sz, "test0320-0190 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vecr, 3) == 1_sz, "test0320-0200 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vecr, 2_sz, 3) == 2_sz, "test0320-0210 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, equal, vecr, 8) == 5_sz, "test0320-0220 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vecr, 8) == 6_sz, "test0320-0230 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vecr, 2_sz, 8) == 7_sz, "test0320-0240 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, equal, vecr, 2) == -1_sz, "test0320-0250 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vecr, 2) == -1_sz, "test0320-0260 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vecr, 2_sz, 2) == -1_sz, "test0320-0270 failed")

    vec = vectar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    vecr = range1 (vec, 3, 8)
    call check (vectar_bottenbruch_search0 (less_than, equal, vecr, 9) == -1_sz, "test0320-0280 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vecr, 9) == -1_sz, "test0320-0290 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vecr, 2_sz, 9) == -1_sz, "test0320-0300 failed")

    vec = vectar (5)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 5) == 0_sz, "test0320-1010 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 5) == 1_sz, "test0320-1020 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 5) == -26_sz, "test0320-1030 failed")

    vec = vectar (5)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 4) == -1_sz, "test0320-1040 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 4) == -1_sz, "test0320-1050 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 4) == -27_sz, "test0320-1060 failed")

    vec = vectar (5)
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 6) == -1_sz, "test0320-1070 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 6) == -1_sz, "test0320-1080 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 6) == -27_sz, "test0320-1090 failed")

    vec = vectar ()
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 5) == -1_sz, "test0320-2010 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 5) == -1_sz, "test0320-2020 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 5) == -27_sz, "test0320-2030 failed")

    vec = vectar ()
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 4) == -1_sz, "test0320-2040 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 4) == -1_sz, "test0320-2050 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 4) == -27_sz, "test0320-2060 failed")

    vec = vectar ()
    call check (vectar_bottenbruch_search0 (less_than, equal, vec, 6) == -1_sz, "test0320-2070 failed")
    call check (vectar_bottenbruch_search1 (less_than, equal, vec, 6) == -1_sz, "test0320-2080 failed")
    call check (vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 6) == -27_sz, "test0320-2090 failed")

    vec = vectar (1, 2, 2, 2, 2, 3, 4, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 8, 9, 10)
    call check (vectar_ref0 (vec, vectar_bottenbruch_search0 (less_than, equal, vec, 6)) .eqi. 6, "test0320-3010 failed")
    call check (vectar_ref1 (vec, vectar_bottenbruch_search1 (less_than, equal, vec, 6)) .eqi. 6, "test0320-3020 failed")
    call check (vectar_refn (vec, -26_sz, vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 6)) .eqi. 6, &
         &      "test0320-3030 failed")

    vec = vectar (6, 6, 6, 6, 6, 6)
    call check (vectar_ref0 (vec, vectar_bottenbruch_search0 (less_than, equal, vec, 6)) .eqi. 6, "test0320-3040 failed")
    call check (vectar_ref1 (vec, vectar_bottenbruch_search1 (less_than, equal, vec, 6)) .eqi. 6, "test0320-3050 failed")
    call check (vectar_refn (vec, -26_sz, vectar_bottenbruch_searchn (less_than, equal, vec, -26_sz, 6)) .eqi. 6, &
         &      "test0320-3060 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

    function equal (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) == int_cast (y))
    end function equal

  end subroutine test0320

  subroutine test0330

    call check (.not. vectar_some (eq4, vectar ()), "test0330-0010 failed")
    call check (vectar_some (eq4, vectar (1, 2, 3, 4)), "test0330-0020 failed")
    call check (vectar_some (eq4, vectar (1, 2, 3, 4, 5, 6)), "test0330-0030 failed")
    call check (vectar_some (eq4, vectar (4, 2, 3, 5, 6)), "test0330-0040 failed")
    call check (.not. vectar_some (eq4, vectar (1, 2, 3, 5, 6)), "test0330-0050 failed")

    call check (.not. vectar_some (lt2, vectar (), vectar (1, 2, 4, 4)), "test0330-1010 failed")
    call check (.not. vectar_some (lt2, vectar (1, 2, 4, 4), vectar ()), "test0330-1020 failed")
    call check (vectar_some (lt2, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4)), "test0330-1030 failed")
    call check (.not. vectar_some (lt2, vectar (1, 2, 3, 4, 5), vectar (1, 2, 3, 4)), "test0330-1040 failed")

    call check (vectar_some (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 5, 4)), &
         &      "test0330-2010 failed")
    call check (.not. vectar_some (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 4, 5)), &
         &      "test0330-2020 failed")

  contains

    function eq4 (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) == 4)
    end function eq4

    function lt2 (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function lt2

    function lt3 (x, y, z) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y)) .and. (int_cast (y) < int_cast (z))
    end function lt3

  end subroutine test0330

  subroutine test0340

    call check (vectar_every (eq4, vectar ()), "test0340-0010 failed")
    call check (.not. vectar_every (eq4, vectar (1, 2, 3, 4)), "test0340-0020 failed")
    call check (vectar_every (eq4, vectar (4, 4, 4, 4)), "test0340-0030 failed")
    call check (.not. vectar_every (eq4, vectar (3, 3, 3, 3)), "test0340-0040 failed")

    call check (vectar_every (lt2, vectar (), vectar (1, 2, 4, 4)), "test0340-1010 failed")
    call check (vectar_every (lt2, vectar (1, 2, 4, 4), vectar ()), "test0340-1020 failed")
    call check (.not. vectar_every (lt2, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4)), "test0340-1030 failed")
    call check (.not. vectar_every (lt2, vectar (1, 2, 3, 4, 5), vectar (1, 2, 3, 4)), "test0340-1040 failed")
    call check (vectar_every (lt2, vectar (1, 2, 3, 4, 5), vectar (2, 3, 4, 5)), "test0340-1050 failed")

    call check (.not. vectar_every (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 5, 4)), &
         &      "test0340-2010 failed")
    call check (.not. vectar_every (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 4, 5)), &
         &      "test0340-2020 failed")
    call check (vectar_every (lt3, vectar (1, 2, 3, 4), vectar (2, 3, 4, 5, 6), vectar (3, 4, 5, 6)), &
         &      "test0340-2030 failed")

  contains

    function eq4 (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) == 4)
    end function eq4

    function lt2 (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function lt2

    function lt3 (x, y, z) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y)) .and. (int_cast (y) < int_cast (z))
    end function lt3

  end subroutine test0340

  subroutine test0350
    class(*), allocatable :: retval

    call check (is_false (vectar_some_map (eq4, vectar ())), "test0350-0010 failed")
    call check (vectar_some_map (eq4, vectar (1, 2, 3, 4)) .eqi. 4, "test0350-0020 failed")
    call check (vectar_some_map (eq4, vectar (1, 2, 3, 4, 5, 6)) .eqi. 4, "test0350-0030 failed")
    call check (vectar_some_map (eq4, vectar (4, 2, 3, 5, 6)) .eqi. 4, "test0350-0040 failed")
    call check (is_false (vectar_some_map (eq4, vectar (1, 2, 3, 5, 6))), "test0350-0050 failed")

    call check (is_false (vectar_some_map (lt2, vectar (), vectar (1, 2, 4, 4))), "test0350-1010 failed")
    call check (is_false (vectar_some_map (lt2, vectar (1, 2, 4, 4), vectar ())), "test0350-1020 failed")
    call check (is_false (vectar_some_map (lt2, vectar (1, 2, 3, 4, 5), vectar (1, 2, 3, 4))), "test0350-1030 failed")
    retval = vectar_some_map (lt2, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4))
    call check (list_equal (int_eq, retval, list (3, 4)), "test0350-1040 failed")

    call check (is_false (vectar_some_map (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 4, 5))), &
         &      "test0350-2010 failed")
    retval = vectar_some_map (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 5, 4))
    call check (list_equal (int_eq, retval, list (3, 4, 5)), "test0350-2020 failed")

  contains

    subroutine eq4 (x, retval)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if (int_cast (x) == 4) then
         retval = 4
      else
         retval = .false.
      end if
    end subroutine eq4

    subroutine lt2 (x, y, retval)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if (int_cast (x) < int_cast (y)) then
         retval = list (x, y)
      else
         retval = .false.
      end if
    end subroutine lt2

    subroutine lt3 (x, y, z, retval)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if ((int_cast (x) < int_cast (y)) .and. (int_cast (y) < int_cast (z))) then
         retval = list (x, y, z)
      else
         retval = .false.
      end if
    end subroutine lt3

  end subroutine test0350

  subroutine test0360
    class(*), allocatable :: retval

    call check (logical_cast (vectar_every_map (eq4, vectar ())), "test0360-0010 failed")
    call check (is_false (vectar_every_map (eq4, vectar (1, 2, 3, 4))), "test0360-0020 failed")
    call check (vectar_every_map (eq4, vectar (4, 4, 4, 4)) .eqi. 4, "test0360-0030 failed")
    call check (is_false (vectar_every_map (eq4, vectar (3, 3, 3, 3))), "test0360-0040 failed")

    call check (logical_cast (vectar_every_map (lt2, vectar (), vectar (1, 2, 4, 4))), "test0360-1010 failed")
    call check (logical_cast (vectar_every_map (lt2, vectar (1, 2, 4, 4), vectar ())), "test0360-1020 failed")
    call check (is_false (vectar_every_map (lt2, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4))), "test0360-1030 failed")
    call check (is_false (vectar_every_map (lt2, vectar (1, 2, 3, 4, 5), vectar (1, 2, 3, 4))), "test0360-1040 failed")
    retval = vectar_every_map (lt2, vectar (1, 2, 3, 4, 5), vectar (2, 3, 4, 5))
    call check (list_equal (int_eq, retval, list (4, 5)), "test0360-1050 failed")

    call check (is_false (vectar_every_map (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 5, 4))), &
         &      "test0360-2010 failed")
    call check (is_false (vectar_every_map (lt3, vectar (1, 2, 3, 4), vectar (1, 2, 4, 4), vectar (1, 2, 4, 5))), &
         &      "test0360-2020 failed")
    retval = vectar_every_map (lt3, vectar (1, 2, 3, 4), vectar (2, 3, 4, 5, 6), vectar (3, 4, 5, 6))
    call check (list_equal (int_eq, retval, list (4, 5, 6)), "test0360-2030 failed")

  contains

    subroutine eq4 (x, retval)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if (int_cast (x) == 4) then
         retval = 4
      else
         retval = .false.
      end if
    end subroutine eq4

    subroutine lt2 (x, y, retval)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if (int_cast (x) < int_cast (y)) then
         retval = list (x, y)
      else
         retval = .false.
      end if
    end subroutine lt2

    subroutine lt3 (x, y, z, retval)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), intent(in) :: z
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if ((int_cast (x) < int_cast (y)) .and. (int_cast (y) < int_cast (z))) then
         retval = list (x, y, z)
      else
         retval = .false.
      end if
    end subroutine lt3

  end subroutine test0360

  subroutine test0370
    type(vectar_t) :: vec
    type(vectar_t) :: vec_out
    integer(sz) :: num_satisfied
    type(cons_t) :: lst

    vec = vectar (1, 2, 3, 4, 4, 6, 1, 2, 7)
    call do_vectar_partition (is_even, vec, vec_out, num_satisfied)
    call check (vectar_equal (int_eq, vecr1 (), vectar (2, 4, 4, 6, 2)), "test0370-0010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar (1, 3, 1, 7)), "test0370-0020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar (2, 4, 4, 6, 2, 1, 3, 1, 7)), "test0370-0040 failed")

    vec = vectar (2, 4, 4, 6, 2)
    call do_vectar_partition (is_even, vec, vec_out, num_satisfied)
    call check (vectar_equal (int_eq, vecr1 (), vectar (2, 4, 4, 6, 2)), "test0370-1010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar ()), "test0370-1020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar (2, 4, 4, 6, 2)), "test0370-1040 failed")

    vec = vectar (1, 3, 1, 7)
    call do_vectar_partition (is_even, vec, vec_out, num_satisfied)
    call check (vectar_equal (int_eq, vecr1 (), vectar ()), "test0370-2010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar (1, 3, 1, 7)), "test0370-2020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar (1, 3, 1, 7)), "test0370-2040 failed")

    vec = vectar ()
    call do_vectar_partition (is_even, vec, vec_out, num_satisfied)
    call check (vectar_equal (int_eq, vecr1 (), vectar ()), "test0370-3010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar ()), "test0370-3020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar ()), "test0370-3040 failed")

    vec = vectar (1, 2, 3, 4, 4, 6, 1, 2, 7)
    lst = vectar_partition (is_even, vec)
    vec_out = .tovectar. first (lst)
    num_satisfied = size_kind_cast (second (lst))
    call check (vectar_equal (int_eq, vecr1 (), vectar (2, 4, 4, 6, 2)), "test0370-4010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar (1, 3, 1, 7)), "test0370-4020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar (2, 4, 4, 6, 2, 1, 3, 1, 7)), "test0370-4040 failed")

    vec = vectar (2, 4, 4, 6, 2)
    lst = vectar_partition (is_even, vec)
    vec_out = .tovectar. first (lst)
    num_satisfied = size_kind_cast (second (lst))
    call check (vectar_equal (int_eq, vecr1 (), vectar (2, 4, 4, 6, 2)), "test0370-5010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar ()), "test0370-5020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar (2, 4, 4, 6, 2)), "test0370-5040 failed")

    vec = vectar (1, 3, 1, 7)
    lst = vectar_partition (is_even, vec)
    vec_out = .tovectar. first (lst)
    num_satisfied = size_kind_cast (second (lst))
    call check (vectar_equal (int_eq, vecr1 (), vectar ()), "test0370-6010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar (1, 3, 1, 7)), "test0370-6020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar (1, 3, 1, 7)), "test0370-6040 failed")

    vec = vectar ()
    lst = vectar_partition (is_even, vec)
    vec_out = .tovectar. first (lst)
    num_satisfied = size_kind_cast (second (lst))
    call check (vectar_equal (int_eq, vecr1 (), vectar ()), "test0370-7010 failed")
    call check (vectar_equal (int_eq, vecr2 (), vectar ()), "test0370-7020 failed")
    call check (vectar_equal (int_eq, vec_out, vectar ()), "test0370-7040 failed")

  contains

    function vecr1 () result (vecr)
      type(vectar_range_t) :: vecr
      vecr = range1 (vec_out, 1_sz, num_satisfied)
    end function vecr1

    function vecr2 () result (vecr)
      type(vectar_range_t) :: vecr
      vecr = range1 (vec_out, num_satisfied + 1_sz, vectar_length (vec_out))
    end function vecr2

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

  end subroutine test0370

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
    call test0220
    call test0230
    call test0240
    call test0250
    call test0260
    call test0270
    call test0280
    call test0290
    call test0300
    call test0310
    call test0320
    call test0330
    call test0340
    call test0350
    call test0360
    call test0370

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
