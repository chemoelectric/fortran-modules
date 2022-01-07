! -*- F90 -*- 
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

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
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

  subroutine test0025
    type(cons_t) :: lst1, lst2, lst3, lst4
    type(gcroot_t) :: lst1_copy, lst2_copy

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = apply_lset_union (str_t_eq_gc, list (lst1, lst2))
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0025-0003 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0025-0006 failed")
    lst4 = list (str_t ('u'), str_t ('o'), str_t ('i'), str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0025-0010 failed")

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
    lst1 = list (str_t ('a'), str_t ('a'), str_t ('c'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = apply_lset_union (str_t_eq_gc, list (lst1, lst2))
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0025-0013 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0025-0016 failed")
    lst4 = list (str_t ('x'), str_t ('a'), str_t ('a'), str_t ('c'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0025-0020 failed")

    ! No lists given.
    call check (is_nil (apply_lset_union (str_t_eq_gc, nil)), "test0025-0030 failed")

    ! One list given.
    call check (list_equal (int_eq, apply_lset_union (str_t_eq_gc, list (list (1, 2, 3))), list (1, 2, 3)), &
         "test0025-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, apply_lset_union (str_t_eq_gc, list (list (1, 2, 3), nil)), list (1, 2, 3)), &
         "test0025-0040 failed")
    call check (list_equal (int_eq, apply_lset_union (str_t_eq_gc, list (nil, list (1, 2, 3))), list (1, 2, 3)), &
         "test0025-0050 failed")

    ! If two lists are the same, for efficiency their union should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_union (str_t_eq_gc, list (lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0025-0060 failed")

    ! If more than two lists are the same, for efficiency their union
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_union (str_t_eq_gc, list (lst1, lst1, lst1, lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0025-0070 failed")

    ! Try multiple lists.
    lst3 = apply_lset_union (int_eq_gc, list (nil, nil, iota (100, 1), iota (50, 1), nil, iota (100, 51, 1), nil, nil))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (150, 1)), "test0025-0080 failed")
  end subroutine test0025

  subroutine test0030
    type(cons_t) :: lst1, lst2, lst3, lst4

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst3 = lset_unionx (str_t_eq_gc, lst1, lst2)
    lst4 = list (str_t ('u'), str_t ('o'), str_t ('i'), str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0030-0010 failed")

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
    lst1 = list (str_t ('a'), str_t ('a'), str_t ('c'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'))
    lst3 = lset_unionx (str_t_eq_gc, lst1, lst2)
    lst4 = list (str_t ('x'), str_t ('a'), str_t ('a'), str_t ('c'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0030-0030 failed")

    ! No lists given.
    call check (is_nil (lset_unionx (str_t_eq_gc)), "test0030-0030 failed")

    ! One list given.
    call check (list_equal (int_eq, lset_unionx (str_t_eq_gc, list (1, 2, 3)), list (1, 2, 3)), "test0030-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, lset_unionx (str_t_eq_gc, list (1, 2, 3), nil), list (1, 2, 3)), "test0030-0040 failed")
    call check (list_equal (int_eq, lset_unionx (str_t_eq_gc, nil, list (1, 2, 3)), list (1, 2, 3)), "test0030-0050 failed")

    ! If two lists are the same, for efficiency their union should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = lset_unionx (str_t_eq_gc, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0030-0060 failed")

    ! If more than two lists are the same, for efficiency their union
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = lset_unionx (str_t_eq_gc, lst1, lst1, lst1, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0030-0070 failed")

    ! Try multiple lists.
    lst3 = lset_unionx (int_eq_gc, nil, nil, iota (100, 1), iota (50, 1), nil, iota (100, 51, 1), nil, nil)
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (150, 1)), "test0030-0080 failed")
  end subroutine test0030

  subroutine test0035
    type(cons_t) :: lst1, lst2, lst3, lst4

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst3 = apply_lset_unionx (str_t_eq_gc, list (lst1, lst2))
    lst4 = list (str_t ('u'), str_t ('o'), str_t ('i'), str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0035-0010 failed")

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
    lst1 = list (str_t ('a'), str_t ('a'), str_t ('c'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'))
    lst3 = apply_lset_unionx (str_t_eq_gc, list (lst1, lst2))
    lst4 = list (str_t ('x'), str_t ('a'), str_t ('a'), str_t ('c'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0035-0020 failed")

    ! No lists given.
    call check (is_nil (apply_lset_unionx (str_t_eq_gc, nil)), "test0035-0030 failed")

    ! One list given.
    call check (list_equal (int_eq, apply_lset_unionx (str_t_eq_gc, list (list (1, 2, 3))), list (1, 2, 3)), &
         "test0035-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, apply_lset_unionx (str_t_eq_gc, list (list (1, 2, 3), nil)), list (1, 2, 3)), &
         "test0035-0040 failed")
    call check (list_equal (int_eq, apply_lset_unionx (str_t_eq_gc, list (nil, list (1, 2, 3))), list (1, 2, 3)), &
         "test0035-0050 failed")

    ! If two lists are the same, for efficiency their union should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_unionx (str_t_eq_gc, list (lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0035-0060 failed")

    ! If more than two lists are the same, for efficiency their union
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_unionx (str_t_eq_gc, list (lst1, lst1, lst1, lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0035-0070 failed")

    ! Try multiple lists.
    lst3 = apply_lset_unionx (int_eq_gc, list (nil, nil, iota (100, 1), iota (50, 1), nil, iota (100, 51, 1), nil, nil))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (150, 1)), "test0035-0080 failed")
  end subroutine test0035

  subroutine test0040
    type(cons_t) :: lst1, lst2, lst3, lst4
    type(gcroot_t) :: lst1_copy, lst2_copy

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_intersection (str_t_eq_gc, lst1, lst2)
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0040-0003 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0040-0006 failed")
    lst4 = list (str_t ('a'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0040-0010 failed")

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
    lst1 = list (str_t ('a'), str_t ('x'), str_t ('y'), str_t ('a'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'), str_t ('z'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_intersection (str_t_eq_gc, lst1, lst2)
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0040-0013 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0040-0016 failed")
    lst4 = list (str_t ('a'), str_t ('x'), str_t ('a'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0040-0040 failed")

    call check (list_equal (int_eq, lset_intersection (str_t_eq_gc, list (1, 2, 3)), list (1, 2, 3)), "test0040-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, lset_intersection (str_t_eq_gc, list (1, 2, 3), nil), list ()), "test0040-0040 failed")
    call check (list_equal (int_eq, lset_intersection (str_t_eq_gc, nil, list (1, 2, 3)), list ()), "test0040-0050 failed")

    ! If two lists are the same, for efficiency their intersection should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = lset_intersection (str_t_eq_gc, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0040-0060 failed")

    ! If more than two lists are the same, for efficiency their intersection
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = lset_intersection (str_t_eq_gc, lst1, lst1, lst1, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0040-0070 failed")

    ! Try multiple lists.
    lst3 = lset_intersection (int_eq_gc, iota (100, 1), iota (75, 1), iota (100, 51))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (25, 51)), "test0040-0080 failed")
  end subroutine test0040

  subroutine test0045
    type(cons_t) :: lst1, lst2, lst3, lst4
    type(gcroot_t) :: lst1_copy, lst2_copy

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = apply_lset_intersection (str_t_eq_gc, list (lst1, lst2))
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0045-0003 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0045-0006 failed")
    lst4 = list (str_t ('a'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0045-0010 failed")

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
    lst1 = list (str_t ('a'), str_t ('x'), str_t ('y'), str_t ('a'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'), str_t ('z'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = apply_lset_intersection (str_t_eq_gc, list (lst1, lst2))
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0045-0013 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0045-0016 failed")
    lst4 = list (str_t ('a'), str_t ('x'), str_t ('a'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0045-0040 failed")

    call check (list_equal (int_eq, apply_lset_intersection (str_t_eq_gc, list (list (1, 2, 3))), &
         list (1, 2, 3)), "test0045-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, apply_lset_intersection (str_t_eq_gc, list (list (1, 2, 3), nil)), &
         list ()), "test0045-0040 failed")
    call check (list_equal (int_eq, apply_lset_intersection (str_t_eq_gc, list (nil, list (1, 2, 3))), &
         list ()), "test0045-0050 failed")

    ! If two lists are the same, for efficiency their intersection should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_intersection (str_t_eq_gc, list (lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0045-0060 failed")

    ! If more than two lists are the same, for efficiency their intersection
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_intersection (str_t_eq_gc, list (lst1, lst1, lst1, lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0045-0070 failed")

    ! Try multiple lists.
    lst3 = apply_lset_intersection (int_eq_gc, list (iota (100, 1), iota (75, 1), iota (100, 51)))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (25, 51)), "test0045-0080 failed")
  end subroutine test0045

  subroutine test0050
    type(cons_t) :: lst1, lst2, lst3, lst4

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst3 = lset_intersectionx (str_t_eq_gc, lst1, lst2)
    lst4 = list (str_t ('a'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0050-0010 failed")

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
    lst1 = list (str_t ('a'), str_t ('x'), str_t ('y'), str_t ('a'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'), str_t ('z'))
    lst3 = lset_intersectionx (str_t_eq_gc, lst1, lst2)
    lst4 = list (str_t ('a'), str_t ('x'), str_t ('a'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0050-0050 failed")

    call check (list_equal (int_eq, lset_intersectionx (str_t_eq_gc, list (1, 2, 3)), list (1, 2, 3)), "test0050-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, lset_intersectionx (str_t_eq_gc, list (1, 2, 3), nil), list ()), "test0050-0050 failed")
    call check (list_equal (int_eq, lset_intersectionx (str_t_eq_gc, nil, list (1, 2, 3)), list ()), "test0050-0050 failed")

    ! If two lists are the same, for efficiency their intersection should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = lset_intersectionx (str_t_eq_gc, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0050-0060 failed")

    ! If more than two lists are the same, for efficiency their intersection
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = lset_intersectionx (str_t_eq_gc, lst1, lst1, lst1, lst1, lst1)
    call check (cons_t_eq (lst1, lst3), "test0050-0070 failed")

    ! Try multiple lists.
    lst3 = lset_intersectionx (int_eq_gc, iota (100, 1), iota (75, 1), iota (100, 51))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (25, 51)), "test0050-0080 failed")
  end subroutine test0050

  subroutine test0055
    type(cons_t) :: lst1, lst2, lst3, lst4

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst3 = apply_lset_intersectionx (str_t_eq_gc, list (lst1, lst2))
    lst4 = list (str_t ('a'), str_t ('e'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0055-0010 failed")

    ! An example from SRFI-1. Repeated elements in the first list are
    ! preserved.
    lst1 = list (str_t ('a'), str_t ('x'), str_t ('y'), str_t ('a'))
    lst2 = list (str_t ('x'), str_t ('a'), str_t ('x'), str_t ('z'))
    lst3 = apply_lset_intersectionx (str_t_eq_gc, list (lst1, lst2))
    lst4 = list (str_t ('a'), str_t ('x'), str_t ('a'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0055-0050 failed")

    call check (list_equal (int_eq, apply_lset_intersectionx (str_t_eq_gc, list (list (1, 2, 3))), &
         list (1, 2, 3)), "test0055-0030 failed")

    ! One list is nil.
    call check (list_equal (int_eq, apply_lset_intersectionx (str_t_eq_gc, list (list (1, 2, 3), nil)), &
         list ()), "test0055-0050 failed")
    call check (list_equal (int_eq, apply_lset_intersectionx (str_t_eq_gc, list (nil, list (1, 2, 3))), &
         list ()), "test0055-0050 failed")

    ! If two lists are the same, for efficiency their intersection should be
    ! the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_intersectionx (str_t_eq_gc, list (lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0055-0060 failed")

    ! If more than two lists are the same, for efficiency their intersection
    ! should be the same.
    lst1 = iota (100, 1)
    lst3 = apply_lset_intersectionx (str_t_eq_gc, list (lst1, lst1, lst1, lst1, lst1))
    call check (cons_t_eq (lst1, lst3), "test0055-0070 failed")

    ! Try multiple lists.
    lst3 = apply_lset_intersectionx (int_eq_gc, list (iota (100, 1), iota (75, 1), iota (100, 51)))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (25, 51)), "test0055-0080 failed")
  end subroutine test0055

  subroutine test0060
    type(cons_t) :: lst1, lst2, lst3, lst4
    type(gcroot_t) :: lst1_copy, lst2_copy

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_difference (str_t_eq_gc, lst1, lst2)
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0060-00010 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0060-00020 failed")
    lst4 = list (str_t ('b'), str_t ('c'), str_t ('d'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0060-0030 failed")

    ! The difference of two equal sets is a null set.
    lst1 = iota (100, 1)
    lst2 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_difference (int_eq_gc, lst1, lst2)
    call check (list_equal (int_eq, lst1, lst1_copy), "test0060-0040 failed")
    call check (list_equal (int_eq, lst2, lst2_copy), "test0060-0050 failed")
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0060-0060 failed")

    ! The difference of a set and itself is a null set.
    lst1 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst3 = lset_difference (int_eq_gc, lst1, lst1)
    call check (list_equal (int_eq, lst1, lst1_copy), "test0060-0070 failed")
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0060-0080 failed")

    ! Try multiple lists.
    lst3 = lset_difference (int_eq_gc, iota (100, 1), iota (20, 1), iota (50, 1, 2))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (40, 22, 2)), "test0060-0090 failed")

    ! Try one list.
    call check (list_equal (int_eq, lset_difference (int_eq_gc, nil), nil), "test0060-0100 failed")
    call check (list_equal (int_eq, lset_difference (int_eq_gc, list (123)), list (123)), "test0060-0110 failed")
    call check (list_equal (int_eq, lset_difference (int_eq_gc, list (1, 2, 3)), list (1, 2, 3)), "test0060-0120 failed")
  end subroutine test0060

  subroutine test0070
    type(cons_t) :: lst1, lst2, lst3, lst4

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst3 = lset_differencex (str_t_eq_gc, lst1, lst2)
    lst4 = list (str_t ('b'), str_t ('c'), str_t ('d'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0070-0030 failed")

    ! The difference of two equal sets is a null set.
    lst1 = iota (100, 1)
    lst2 = iota (100, 1)
    lst3 = lset_differencex (int_eq_gc, lst1, lst2)
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0070-0070 failed")

    ! The difference of a set and itself is a null set.
    lst1 = iota (100, 1)
    lst3 = lset_differencex (int_eq_gc, lst1, lst1)
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0070-0080 failed")

    ! Try multiple lists.
    lst3 = lset_differencex (int_eq_gc, iota (100, 1), iota (20, 1), iota (50, 1, 2))
    call check (list_equal (int_eq, list_sort (int_lt, lst3), iota (40, 22, 2)), "test0070-0090 failed")

    ! Try one list.
    call check (list_equal (int_eq, lset_differencex (int_eq_gc, nil), nil), "test0070-0100 failed")
    call check (list_equal (int_eq, lset_differencex (int_eq_gc, list (123)), list (123)), "test0070-0110 failed")
    call check (list_equal (int_eq, lset_differencex (int_eq_gc, list (1, 2, 3)), list (1, 2, 3)), "test0070-0120 failed")
  end subroutine test0070

  subroutine test0080
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6
    type(gcroot_t) :: lst1_copy, lst2_copy

    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_diff_and_intersection (str_t_eq_gc, lst1, lst2)
    call check (list_equal (str_t_eq, lst1, lst1_copy), "0080-00010 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "0080-00020 failed")
    lst4 = list (str_t ('b'), str_t ('c'), str_t ('d'))
    lst5 = list (str_t ('a'), str_t ('e'))
    call check (length (lst3) == 2, "0080-0030 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, first (lst3)), list_sort (str_t_lt, lst4)), "0080-0040 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, second (lst3)), list_sort (str_t_lt, lst5)), "0080-0050 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, first (lst3)), &
         &                  list_sort (str_t_lt, first (reference_result2 (str_t_eq, lst1, lst2)))), "0080-0060 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, second (lst3)), &
         &                  list_sort (str_t_lt, second (reference_result2 (str_t_eq, lst1, lst2)))), "0080-0070 failed")


    ! The difference of two equal sets is a null set. Their
    ! intersection is an equal set.
    lst1 = iota (100, 1)
    lst2 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_diff_and_intersection (int_eq_gc, lst1, lst2)
    call check (list_equal (int_eq, lst1, lst1_copy), "0080-0110 failed")
    call check (list_equal (int_eq, lst2, lst2_copy), "0080-0120 failed")
    lst4 = nil
    lst5 = iota (100, 1)
    call check (length (lst3) == 2, "0080-0130 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst3)), list_sort (int_lt, lst4)), "0080-0140 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst3)), list_sort (int_lt, lst5)), "0080-0150 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst3)), &
         &                  list_sort (int_lt, first (reference_result2 (int_eq, lst1, lst2)))), "0080-0160 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst3)), &
         &                  list_sort (int_lt, second (reference_result2 (int_eq, lst1, lst2)))), "0080-0170 failed")

    ! The difference of a set and itself is a null set. The
    ! intersection is itself.
    lst1 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst3 = lset_diff_and_intersection (int_eq_gc, lst1, lst1)
    call check (list_equal (int_eq, lst1, lst1_copy), "0080-0210 failed")
    call check (length (lst3) == 2, "0080-0230 failed")
    call check (list_equal (int_eq, is_nil (first (lst3))), "0080-0240 failed")
    call check (list_equal (int_eq, cons_t_eq (second (lst3), lst1)), "0080-0250 failed")

    ! Try three lists.
    lst1 = iota (100, 1)
    lst2 = iota (20, 1)
    lst3 = iota (50, 1, 2)
    lst4 = lset_diff_and_intersection (int_eq_gc, lst1, lst2, lst3)
    call check (length (lst4) == 2, "0080-0330 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst4)), iota (40, 22, 2)), "0080-0340 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst4)), &
         &                  list_sort (int_lt, lset_union (int_eq, iota (20, 1), iota (40, 21, 2)))), &
         &      "0080-0350 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst4)), &
         &                  list_sort (int_lt, first (reference_result3 (int_eq, lst1, lst2, lst3)))), &
         &      "0080-0360 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst4)), &
         &                  list_sort (int_lt, second (reference_result3 (int_eq, lst1, lst2, lst3)))), &
         &      "0080-0370 failed")

    ! Try four lists.
    lst1 = iota (100, 1)
    lst2 = iota (20, 37, 3)
    lst3 = iota (100, 51)
    lst4 = iota (50, 75)
    lst5 = lset_diff_and_intersection (int_eq_gc, lst1, lst2, lst3, lst4)
    lst6 = reference_result4 (int_eq, lst1, lst2, lst3, lst4)
    call check (length (lst5) == 2, "0080-0430 failed")
    call check (list_equal (int_eq, &
         &                  list_sort (int_lt, first (lst5)), &
         &                  list_sort (int_lt, first (lst6))), "0080-0460 failed")
    call check (list_equal (int_eq, &
         &                  list_sort (int_lt, second (lst5)), &
         &                  list_sort (int_lt, second (lst6))), "0080-0470 failed")


    ! Try a null set, alone.
    call check (list_equal (int_eq, first (lset_diff_and_intersection (int_eq_gc, nil)), nil), "0080-0500 failed")
    call check (list_equal (int_eq, second (lset_diff_and_intersection (int_eq_gc, nil)), nil), "0080-0510 failed")

    ! Try a singleton set, alone.
    call check (list_equal (int_eq, first (lset_diff_and_intersection (int_eq_gc, list (123))), list (123)), "0080-0600 failed")
    call check (list_equal (int_eq, second (lset_diff_and_intersection (int_eq_gc, list (123))), nil), "0080-0610 failed")
    call check (list_equal (int_eq, first (lset_diff_and_intersection (int_eq_gc, list (123))), &
         &                  first (reference_result1 (int_eq, list (123)))), &
         &      "0080-0620 failed")
    call check (list_equal (int_eq, second (lset_diff_and_intersection (int_eq_gc, list (123))), &
         &                  second (reference_result1 (int_eq, list (123)))), &
         &      "0080-0640 failed")

  contains

    recursive function reference_result1 (equal, lst1) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      type(cons_t) :: diff_and_xsect

      diff_and_xsect = list (lset_difference (equal, lst1), lset_intersection (equal, lst1, lset_union (equal)))
    end function reference_result1

    recursive function reference_result2 (equal, lst1, lst2) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      type(cons_t) :: diff_and_xsect

      diff_and_xsect = list (lset_difference (equal, lst1, lst2), lset_intersection (equal, lst1, lst2))
    end function reference_result2

    recursive function reference_result3 (equal, lst1, lst2, lst3) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      class(*), intent(in) :: lst3
      type(cons_t) :: diff_and_xsect

      class(*), allocatable :: union

      union = lset_union (equal, lst2, lst3)
      diff_and_xsect = list (lset_difference (equal, lst1, lst2, lst3), lset_intersection (equal, lst1, union))
    end function reference_result3

    recursive function reference_result4 (equal, lst1, lst2, lst3, lst4) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      class(*), intent(in) :: lst3
      class(*), intent(in) :: lst4
      type(cons_t) :: diff_and_xsect

      class(*), allocatable :: union

      union = lset_union (equal, lst2, lst3, lst4)
      diff_and_xsect = list (lset_difference (equal, lst1, lst2, lst3, lst4), lset_intersection (equal, lst1, union))
    end function reference_result4

  end subroutine test0080

  subroutine test0090
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6
    type(gcroot_t) :: lst1a, lst2a, lst3a, lst4a

    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1a = list_copy (lst1)
    lst2a = list_copy (lst2)
    lst3 = lset_diff_and_intersectionx (str_t_eq_gc, lst1, lst2)
    lst4 = list (str_t ('b'), str_t ('c'), str_t ('d'))
    lst5 = list (str_t ('a'), str_t ('e'))
    call check (length (lst3) == 2, "0090-0030 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, first (lst3)), list_sort (str_t_lt, lst4)), "0090-0040 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, second (lst3)), list_sort (str_t_lt, lst5)), "0090-0050 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, first (lst3)), &
         &                  list_sort (str_t_lt, first (reference_result2 (str_t_eq, lst1a, lst2a)))), "0090-0060 failed")
    call check (list_equal (str_t_eq, list_sort (str_t_lt, second (lst3)), &
         &                  list_sort (str_t_lt, second (reference_result2 (str_t_eq, lst1a, lst2a)))), "0090-0070 failed")


    ! The difference of two equal sets is a null set. Their
    ! intersection is an equal set.
    lst1 = iota (100, 1)
    lst2 = iota (100, 1)
    lst1a = list_copy (lst1)
    lst2a = list_copy (lst2)
    lst3 = lset_diff_and_intersectionx (int_eq_gc, lst1, lst2)
    lst4 = nil
    lst5 = iota (100, 1)
    call check (length (lst3) == 2, "0090-0130 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst3)), list_sort (int_lt, lst4)), "0090-0140 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst3)), list_sort (int_lt, lst5)), "0090-0150 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst3)), &
         &                  list_sort (int_lt, first (reference_result2 (int_eq, lst1a, lst2a)))), "0090-0160 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst3)), &
         &                  list_sort (int_lt, second (reference_result2 (int_eq, lst1a, lst2a)))), "0090-0170 failed")

    ! The difference of a set and itself is a null set. The
    ! intersection is itself.
    lst1 = iota (100, 1)
    lst3 = lset_diff_and_intersectionx (int_eq_gc, lst1, lst1)
    call check (length (lst3) == 2, "0090-0230 failed")
    call check (list_equal (int_eq, is_nil (first (lst3))), "0090-0240 failed")
    call check (list_equal (int_eq, cons_t_eq (second (lst3), lst1)), "0090-0250 failed")

    ! Try three lists.
    lst1 = iota (100, 1)
    lst2 = iota (20, 1)
    lst3 = iota (50, 1, 2)
    lst1a = list_copy (lst1)
    lst2a = list_copy (lst2)
    lst3a = list_copy (lst3)
    lst4 = lset_diff_and_intersectionx (int_eq_gc, lst1, lst2, lst3)
    call check (length (lst4) == 2, "0090-0330 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst4)), iota (40, 22, 2)), "0090-0340 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst4)), &
         &                  list_sort (int_lt, lset_union (int_eq, iota (20, 1), iota (40, 21, 2)))), &
         &      "0090-0350 failed")
    call check (list_equal (int_eq, list_sort (int_lt, first (lst4)), &
         &                  list_sort (int_lt, first (reference_result3 (int_eq, lst1a, lst2a, lst3a)))), &
         &      "0090-0360 failed")
    call check (list_equal (int_eq, list_sort (int_lt, second (lst4)), &
         &                  list_sort (int_lt, second (reference_result3 (int_eq, lst1a, lst2a, lst3a)))), &
         &      "0090-0370 failed")

    ! Try four lists.
    lst1 = iota (100, 1)
    lst2 = iota (20, 37, 3)
    lst3 = iota (100, 51)
    lst4 = iota (50, 75)
    lst1a = list_copy (lst1)
    lst2a = list_copy (lst2)
    lst3a = list_copy (lst3)
    lst4a = list_copy (lst4)
    lst5 = lset_diff_and_intersectionx (int_eq_gc, lst1, lst2, lst3, lst4)
    lst6 = reference_result4 (int_eq, lst1a, lst2a, lst3a, lst4a)
    call check (length (lst5) == 2, "0090-0430 failed")
    call check (list_equal (int_eq, &
         &                  list_sort (int_lt, first (lst5)), &
         &                  list_sort (int_lt, first (lst6))), "0090-0460 failed")
    call check (list_equal (int_eq, &
         &                  list_sort (int_lt, second (lst5)), &
         &                  list_sort (int_lt, second (lst6))), "0090-0470 failed")


    ! Try a null set, alone.
    call check (list_equal (int_eq, first (lset_diff_and_intersectionx (int_eq_gc, nil)), nil), "0090-0500 failed")
    call check (list_equal (int_eq, second (lset_diff_and_intersectionx (int_eq_gc, nil)), nil), "0090-0510 failed")

    ! Try a singleton set, alone.
    call check (list_equal (int_eq, first (lset_diff_and_intersectionx (int_eq_gc, list (123))), list (123)), &
         &      "0090-0600 failed")
    call check (list_equal (int_eq, second (lset_diff_and_intersectionx (int_eq_gc, list (123))), nil), &
         &      "0090-0610 failed")
    call check (list_equal (int_eq, first (lset_diff_and_intersectionx (int_eq_gc, list (123))), &
         &                  first (reference_result1 (int_eq, list (123)))), &
         &      "0090-0620 failed")
    call check (list_equal (int_eq, second (lset_diff_and_intersectionx (int_eq_gc, list (123))), &
         &                  second (reference_result1 (int_eq, list (123)))), &
         &      "0090-0640 failed")

  contains

    recursive function reference_result1 (equal, lst1) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      type(cons_t) :: diff_and_xsect

      diff_and_xsect = list (lset_difference (equal, lst1), lset_intersection (equal, lst1, lset_union (equal)))
    end function reference_result1

    recursive function reference_result2 (equal, lst1, lst2) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      type(cons_t) :: diff_and_xsect

      diff_and_xsect = list (lset_difference (equal, lst1, lst2), lset_intersection (equal, lst1, lst2))
    end function reference_result2

    recursive function reference_result3 (equal, lst1, lst2, lst3) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      class(*), intent(in) :: lst3
      type(cons_t) :: diff_and_xsect

      class(*), allocatable :: union

      union = lset_union (equal, lst2, lst3)
      diff_and_xsect = list (lset_difference (equal, lst1, lst2, lst3), lset_intersection (equal, lst1, union))
    end function reference_result3

    recursive function reference_result4 (equal, lst1, lst2, lst3, lst4) result (diff_and_xsect)
      procedure(list_predicate2_t) :: equal
      class(*), intent(in) :: lst1
      class(*), intent(in) :: lst2
      class(*), intent(in) :: lst3
      class(*), intent(in) :: lst4
      type(cons_t) :: diff_and_xsect

      class(*), allocatable :: union

      union = lset_union (equal, lst2, lst3, lst4)
      diff_and_xsect = list (lset_difference (equal, lst1, lst2, lst3, lst4), lset_intersection (equal, lst1, union))
    end function reference_result4

  end subroutine test0090

  subroutine test0100
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5
    type(gcroot_t) :: lst1_copy, lst2_copy

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_xor (str_t_eq_gc, lst1, lst2)
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0100-00010 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0100-00020 failed")
    lst4 = list (str_t ('d'), str_t ('c'), str_t ('b'), str_t ('i'), str_t ('o'), str_t ('u'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0100-0030 failed")

    ! The XOR of two equal sets is a null set.
    lst1 = iota (100, 1)
    lst2 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = lset_xor (int_eq_gc, lst1, lst2)
    call check (list_equal (int_eq, lst1, lst1_copy), "test0100-0040 failed")
    call check (list_equal (int_eq, lst2, lst2_copy), "test0100-0050 failed")
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0100-0060 failed")

    ! The XOR of a set and itself is a null set.
    lst1 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst3 = lset_xor (int_eq_gc, lst1, lst1)
    call check (list_equal (int_eq, lst1, lst1_copy), "test0100-0070 failed")
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0100-0080 failed")

    ! Try multiple lists and permutations of the arguments.
    lst1 = iota (10, 1)
    lst2 = iota (10, 6, 1)
    lst3 = iota (10, 1, 2)
    lst4 = list_sort (int_lt, lset_xor (int_eq, lst1, lst2, lst3))
    call check (list_equal (int_eq, lst4, list (2, 4, 7, 9, 12, 14, 17, 19)), "test0100-0100 failed")
    lst5 = list_sort (int_lt, lset_xor (int_eq, lst1, lst3, lst2))
    call check (list_equal (int_eq, lst4, lst5), "test0100-0110 failed")
    lst5 = list_sort (int_lt, lset_xor (int_eq, lst3, lst1, lst2))
    call check (list_equal (int_eq, lst4, lst5), "test0100-0120 failed")
    lst5 = list_sort (int_lt, lset_xor (int_eq, lst3, lst2, lst1))
    call check (list_equal (int_eq, lst4, lst5), "test0100-0130 failed")
    lst5 = list_sort (int_lt, lset_xor (int_eq, lst2, lst3, lst1))
    call check (list_equal (int_eq, lst4, lst5), "test0100-0140 failed")
    lst5 = list_sort (int_lt, lset_xor (int_eq, lst2, lst1, lst3))
    call check (list_equal (int_eq, lst4, lst5), "test0100-0150 failed")

    ! Try one list.
    call check (list_equal (int_eq, lset_xor (int_eq_gc, nil), nil), "test0100-0100 failed")
    call check (list_equal (int_eq, lset_xor (int_eq_gc, list (123)), list (123)), "test0100-0110 failed")
    call check (list_equal (int_eq, lset_xor (int_eq_gc, list (1, 2, 3)), list (1, 2, 3)), "test0100-0120 failed")
  end subroutine test0100

  subroutine test0105
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5
    type(gcroot_t) :: lst1_copy, lst2_copy

    ! An example from SRFI-1.
    lst1 = list (str_t ('a'), str_t ('b'), str_t ('c'), str_t ('d'), str_t ('e'))
    lst2 = list (str_t ('a'), str_t ('e'), str_t ('i'), str_t ('o'), str_t ('u'))
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = apply_lset_xor (str_t_eq_gc, list (lst1, lst2))
    call check (list_equal (str_t_eq, lst1, lst1_copy), "test0105-00010 failed")
    call check (list_equal (str_t_eq, lst2, lst2_copy), "test0105-00020 failed")
    lst4 = list (str_t ('d'), str_t ('c'), str_t ('b'), str_t ('i'), str_t ('o'), str_t ('u'))
    call check (list_equal (str_t_eq, list_sort (str_t_lt, lst3), list_sort (str_t_lt, lst4)), "test0105-0030 failed")

    ! The XOR of two equal sets is a null set.
    lst1 = iota (100, 1)
    lst2 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst2_copy = list_copy (lst2)
    lst3 = apply_lset_xor (int_eq_gc, list (lst1, lst2))
    call check (list_equal (int_eq, lst1, lst1_copy), "test0105-0040 failed")
    call check (list_equal (int_eq, lst2, lst2_copy), "test0105-0050 failed")
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0105-0060 failed")

    ! The XOR of a set and itself is a null set.
    lst1 = iota (100, 1)
    lst1_copy = list_copy (lst1)
    lst3 = apply_lset_xor (int_eq_gc, list (lst1, lst1))
    call check (list_equal (int_eq, lst1, lst1_copy), "test0105-0070 failed")
    lst4 = nil
    call check (list_equal (int_eq, list_sort (int_lt, lst3), list_sort (int_lt, lst4)), "test0105-0080 failed")

    ! Try multiple lists and permutations of the arguments.
    lst1 = iota (10, 1)
    lst2 = iota (10, 6, 1)
    lst3 = iota (10, 1, 2)
    lst4 = list_sort (int_lt, apply_lset_xor (int_eq, list (lst1, lst2, lst3)))
    call check (list_equal (int_eq, lst4, list (2, 4, 7, 9, 12, 14, 17, 19)), "test0105-0100 failed")
    lst5 = list_sort (int_lt, apply_lset_xor (int_eq, list (lst1, lst3, lst2)))
    call check (list_equal (int_eq, lst4, lst5), "test0105-0110 failed")
    lst5 = list_sort (int_lt, apply_lset_xor (int_eq, list (lst3, lst1, lst2)))
    call check (list_equal (int_eq, lst4, lst5), "test0105-0120 failed")
    lst5 = list_sort (int_lt, apply_lset_xor (int_eq, list (lst3, lst2, lst1)))
    call check (list_equal (int_eq, lst4, lst5), "test0105-0130 failed")
    lst5 = list_sort (int_lt, apply_lset_xor (int_eq, list (lst2, lst3, lst1)))
    call check (list_equal (int_eq, lst4, lst5), "test0105-0140 failed")
    lst5 = list_sort (int_lt, apply_lset_xor (int_eq, list (lst2, lst1, lst3)))
    call check (list_equal (int_eq, lst4, lst5), "test0105-0150 failed")

    ! Try one list.
    call check (list_equal (int_eq, apply_lset_xor (int_eq_gc, list (nil)), nil), "test0105-0100 failed")
    call check (list_equal (int_eq, apply_lset_xor (int_eq_gc, list (list (123))), list (123)), "test0105-0110 failed")
    call check (list_equal (int_eq, apply_lset_xor (int_eq_gc, list (list (1, 2, 3))), list (1, 2, 3)), "test0105-0120 failed")
  end subroutine test0105

  subroutine run_tests
    heap_size_limit = 0

    call test0010
    call test0020
    call test0025
    call test0030
    call test0035
    call test0040
    call test0045
    call test0050
    call test0055
    call test0060
    call test0070
    call test0080
    call test0090
    call test0100
    call test0105

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
