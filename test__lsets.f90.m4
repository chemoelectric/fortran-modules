! -*- F90 -*- include(`common-macros.m4')m4_include([cadadr.m4])
!
! Copyright 2021, 2022 Barry Schwartz
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

module test__lsets

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: lsets

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

!!$  interface operator(.eqsz.)
!!$     module procedure size_kind_eq
!!$  end interface operator(.eqsz.)
!!$
!!$  interface operator(.eqr.)
!!$     module procedure real_eq
!!$  end interface operator(.eqr.)

contains

  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("test__lsets error: ", a)') msg
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

!!$  function size_kind_cast (obj) result (int)
!!$    class(*), intent(in) :: obj
!!$    integer(size_kind) :: int
!!$    select type (obj)
!!$    type is (integer(size_kind))
!!$       int = obj
!!$    class default
!!$       call error_abort ("size_kind_cast of an incompatible object")
!!$    end select
!!$  end function size_kind_cast
!!$
!!$  function real_cast (obj) result (r)
!!$    class(*), intent(in) :: obj
!!$    real :: r
!!$    select type (obj)
!!$    type is (real)
!!$       r = obj
!!$    class default
!!$       call error_abort ("real_cast of an incompatible object")
!!$    end select
!!$  end function real_cast
!!$
!!$  function logical_cast (obj) result (b)
!!$    class(*), intent(in) :: obj
!!$    logical :: b
!!$    select type (obj)
!!$    type is (logical)
!!$       b = obj
!!$    class default
!!$       call error_abort ("logical_cast of an incompatible object")
!!$    end select
!!$  end function logical_cast

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

!!$  function size_kind_eq (obj1, obj2) result (bool)
!!$    class(*), intent(in) :: obj1, obj2
!!$    logical :: bool
!!$    bool = size_kind_cast (obj1) == size_kind_cast (obj2)
!!$  end function size_kind_eq
!!$
!!$  function real_eq (obj1, obj2) result (bool)
!!$    class(*), intent(in) :: obj1, obj2
!!$    logical :: bool
!!$    bool = real_cast (obj1) == real_cast (obj2)
!!$  end function real_eq
!!$
!!$  function num_same (obj1, obj2) result (bool)
!!$    !
!!$    ! Are obj1 and obj2 either equal integers or equal reals?
!!$    !
!!$    class(*), intent(in) :: obj1, obj2
!!$    logical :: bool
!!$    bool = .false.
!!$    select type (obj1)
!!$    type is (integer)
!!$       select type (obj2)
!!$       type is (integer)
!!$          bool = obj1 == obj2
!!$       end select
!!$    type is (real)
!!$       select type (obj2)
!!$       type is (real)
!!$          bool = obj1 == obj2
!!$       end select
!!$    end select
!!$  end function num_same

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

!!$  function str_t_lt_gc (str1, str2) result (bool)
!!$    class(*), intent(in) :: str1
!!$    class(*), intent(in) :: str2
!!$    logical :: bool
!!$    call collect_garbage_now
!!$    bool = (str_t_cast (str1) < str_t_cast (str2))
!!$  end function str_t_lt_gc

  subroutine test0010
    type(cons_t) :: lst, lst1
    type(gcroot_t) :: lst1_copy

    call check (list_equal (int_eq, lset_adjoin (int_eq_gc, nil), nil), "test0010-0010 failed")
    call check (list_equal (int_eq, lset_adjoin (int_eq_gc, list (123)), list (123)), "test0010-0020 failed")
    call check (list_equal (int_eq, lset_adjoin (int_eq_gc, nil, 123), list (123)), "test0010-0030 failed")
    call check (list_equal (int_eq, lset_adjoin (int_eq_gc, list (123), 123), list (123)), "test0010-0040 failed")
    call check (list_equal (int_eq, lset_adjoin (int_eq_gc, list (1, 2), 3), list (3, 1, 2)), "test0010-0050 failed")

    lst = lset_adjoin (int_eq_gc, nil, 1, 2, 3, 4, 5, 6, 7, 8)
    call check (list_equal (int_eq, list_sort (int_lt, lst), iota (8, 1)), "test0010-0060 failed")

    lst1 = iota (4, 1)
    lst1_copy = list_copy (lst1)
    lst = lset_adjoin (int_eq_gc, lst1, 8, 7, 3, 3, 2, 1, 3, 5, 6, 6)
    call check (list_equal (int_eq, lst1, lst1_copy), "test0010-0070 failed")
    call check (list_equal (int_eq, list_sort (int_lt, lst), iota (8, 1)), "test0010-0080 failed")
  end subroutine test0010

  subroutine test0020
    type(cons_t) :: lst1, lst2, lst3, lst4
    type(gcroot_t) :: lst1_copy, lst2_copy

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_union (str_t_eq_gc, lst1, lst2)
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0020-0003 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0020-0006 failed")
    lst4 = list (str_t ('u'), str_t ('o'), str_t ('i'), str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0020-0010 failed")

    ! An example from SRFI-1
    lst1 = list (str_t ('a'), str_t ('a'), str_t ('c'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_union (str_t_eq_gc, lst1, lst2)
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0020-0013 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0020-0016 failed")
    lst4 = list (str_t ('x'), str_t ('a'), str_t ('a'), str_t ('c'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0020-0020 failed")

    ! No lists given.
    call check (is_nil (lset_union (str_t_eq_gc)), "test0020-0030 failed")

    ! One list given.
    call check (list_equal (int_eq, lset_union (str_t_eq_gc, list (1, 2, 3)), list (1, 2, 3)), "test0020-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, lset_union (str_t_eq_gc, list (1, 2, 3), nil), list (1, 2, 3)), "test0020-0040 failed")
    call check (list_equal (int_eq, lset_union (str_t_eq_gc, nil, list (1, 2, 3)), list (1, 2, 3)), "test0020-0050 failed")

    ! If two lists are the same, for efficiency their union should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = lset_union (str_t_eq_gc, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0020-0060 failed")

    ! If more than two lists are the same, for efficiency their union
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = lset_union (str_t_eq_gc, lst1, lst1, lst1, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0020-0070 failed")

    ! Try multiple lists.
    lst3 = lset_union (int_eq_gc, nil, nil, iota (100, 1), iota (50, 1), nil, iota (100, 51, 1), nil, nil)
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (150, 1)), "test0020-0080 failed")
  end subroutine test0020

  subroutine run_tests
    heap_size_limit = 0

    call test0010
    call test0020

    call collect_garbage_now
    call check (current_heap_size () == 0, "run_tests-0100 failed")
    call check (current_roots_count () == 0, "run_tests-0110 failed")
  end subroutine run_tests

end module test__lsets

program main
  use, non_intrinsic :: test__lsets
  implicit none
  call run_tests
end program main
