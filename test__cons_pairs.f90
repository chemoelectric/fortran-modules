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

module test__cons_pairs

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none
  private

  public :: run_tests

  integer, parameter :: sz = size_kind

  type :: str_t
     character(:), allocatable :: val
   contains
     procedure, pass :: length => str_t_length
     procedure, pass :: equal => str_t_equal
     procedure, pass :: assign => str_t_assign
     generic :: operator(==) => equal
     generic :: assignment(=) => assign
     final :: str_t_finalize
  end type str_t

  interface operator(.eqi.)
     module procedure int_eq
  end interface operator(.eqi.)

  interface operator(.eqsz.)
     module procedure size_kind_eq
  end interface operator(.eqsz.)

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
    integer(size_kind) :: int
    select type (obj)
    type is (integer(size_kind))
       int = obj
    class default
       call error_abort ("size_kind_cast of an incompatible object")
    end select
  end function size_kind_cast

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

  function logical_cast (obj) result (b)
    class(*), intent(in) :: obj
    logical :: b
    select type (obj)
    type is (logical)
       b = obj
    class default
       call error_abort ("logical_cast of an incompatible object")
    end select
  end function logical_cast

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

  function size_kind_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = size_kind_cast (obj1) == size_kind_cast (obj2)
  end function size_kind_eq

  function real_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = real_cast (obj1) == real_cast (obj2)
  end function real_eq

  function num_same (obj1, obj2) result (bool)
    !
    ! Are obj1 and obj2 either equal integers or equal reals?
    !
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = .false.
    select type (obj1)
    type is (integer)
       select type (obj2)
       type is (integer)
          bool = obj1 == obj2
       end select
    type is (real)
       select type (obj2)
       type is (real)
          bool = obj1 == obj2
       end select
    end select
  end function num_same

  subroutine test0010
    type(gcroot_t) :: cons1
    class(*), allocatable :: car1, cdr1

    call check (car (cons (123, 456)) .eqi. 123, "test0010-0010 failed")
    call check (cdr (cons (123, 456)) .eqi. 456, "test0010-0020 failed")

    cons1 = cons (123.0, 456.0)
    call check (car (cons1) .eqr. 123.0, "test0010-0030 failed")
    call check (cdr (cons1) .eqr. 456.0, "test0010-0040 failed")
    call uncons (cons1, car1, cdr1)
    call check (car1 .eqr. 123.0, "test0010-0050 failed")
    call check (cdr1 .eqr. 456.0, "test0010-0060 failed")

    call check (is_pair (cons (123, 456)), "test0010-0070 failed")
    call check (is_pair (cons1), "test0010-0080 failed")

    call check (.not. is_not_pair (cons (123, 456)), "test0010-0090 failed")
    call check (.not. is_not_pair (cons1), "test0010-0100 failed")

    call check (.not. cons_t_eq (cons (123, 456), cons (123, 456)), "test0010-0110 failed")
    call check (cons_t_eq (cons1, cons1), "test0010-0120 failed")

    call check (.not. is_nil (cons (123, 456)), "test0010-0130 failed")
    call check (.not. is_nil (cons1), "test0010-0140 failed")

    call check (is_not_nil (cons (123, 456)), "test0010-0150 failed")
    call check (is_not_nil (cons1), "test0010-0160 failed")

    call check (.not. is_nil_list (cons (123, 456)), "test0010-0170 failed")
    call check (.not. is_nil_list (cons1), "test0010-0180 failed")

    call check (is_nil (nil), "test0010-0190 failed")
    call check (.not. is_not_nil (nil), "test0010-0200 failed")
    call check (is_nil_list (nil), "test0010-0210 failed")

    call check (cons_t_eq (nil, nil), "test0010-0200 failed")
    call check (.not. cons_t_eq (nil, cons (1, nil)), "test0010-0210 failed")
    call check (.not. cons_t_eq (cons (1, nil), nil), "test0010-0210 failed")
  end subroutine test0010

  subroutine test0020
    type(gcroot_t) :: lst1

    lst1 = 1 ** 2 ** 3 ** 4 ** 5 ** 6 ** 7 ** 8 ** 9 ** 10 ** nil
    call check (first (lst1) .eqi. 1, "test0020-0010 failed")
    call check (second (lst1) .eqi. 2, "test0020-0020 failed")
    call check (third (lst1) .eqi. 3, "test0020-0030 failed")
    call check (fourth (lst1) .eqi. 4, "test0020-0040 failed")
    call check (fifth (lst1) .eqi. 5, "test0020-0050 failed")
    call check (sixth (lst1) .eqi. 6, "test0020-0060 failed")
    call check (seventh (lst1) .eqi. 7, "test0020-0070 failed")
    call check (eighth (lst1) .eqi. 8, "test0020-0080 failed")
    call check (ninth (lst1) .eqi. 9, "test0020-0090 failed")
    call check (tenth (lst1) .eqi. 10, "test0020-0100 failed")
    call check (car (lst1) .eqi. 1, "test0020-0110 failed")
    call check (cadr (lst1) .eqi. 2, "test0020-0120 failed")
    call check (caddr (lst1) .eqi. 3, "test0020-0130 failed")
    call check (cadddr (lst1) .eqi. 4, "test0020-0140 failed")
    call check (is_proper_list (lst1), "test0020-0200 failed")
    call check (.not. is_dotted_list (lst1), "test0020-0210 failed")
    call check (.not. is_circular_list (lst1), "test0020-0220 failed")

    call check (is_dotted_list (1), "test0020-0300 failed")
    call check (is_dotted_list (cons (1, 2)), "test0020-0310 failed")
    call check (is_dotted_list (1 ** 2 ** 3 ** cons (4, 5)), "test0020-0320 failed")
    call check (.not. is_proper_list (1), "test0020-0330 failed")
    call check (.not. is_proper_list (cons (1, 2)), "test0020-0340 failed")
    call check (.not. is_proper_list (1 ** 2 ** 3 ** cons (4, 5)), "test0020-0350 failed")
    call check (.not. is_circular_list (1), "test0020-0360 failed")
    call check (.not. is_circular_list (cons (1, 2)), "test0020-0370 failed")
    call check (.not. is_circular_list (1 ** 2 ** 3 ** cons (4, 5)), "test0020-0380 failed")
  end subroutine test0020

  subroutine test0030
    type(gcroot_t) :: tree
    integer :: leaf

    tree = build_tree (1, 1, 0)
    leaf = int_cast (car (tree))
    call check (leaf == 0, "test0030-1-0 failed")
    leaf = int_cast (cdr (tree))
    call check (leaf == 1, "test0030-1-1 failed")
    tree = build_tree (1, 2, 0)
    leaf = int_cast (caar (tree))
    call check (leaf == 0, "test0030-2-0 failed")
    leaf = int_cast (cdar (tree))
    call check (leaf == 1, "test0030-2-1 failed")
    leaf = int_cast (cadr (tree))
    call check (leaf == 2, "test0030-2-2 failed")
    leaf = int_cast (cddr (tree))
    call check (leaf == 3, "test0030-2-3 failed")
    tree = build_tree (1, 3, 0)
    leaf = int_cast (caaar (tree))
    call check (leaf == 0, "test0030-3-0 failed")
    leaf = int_cast (cdaar (tree))
    call check (leaf == 1, "test0030-3-1 failed")
    leaf = int_cast (cadar (tree))
    call check (leaf == 2, "test0030-3-2 failed")
    leaf = int_cast (cddar (tree))
    call check (leaf == 3, "test0030-3-3 failed")
    leaf = int_cast (caadr (tree))
    call check (leaf == 4, "test0030-3-4 failed")
    leaf = int_cast (cdadr (tree))
    call check (leaf == 5, "test0030-3-5 failed")
    leaf = int_cast (caddr (tree))
    call check (leaf == 6, "test0030-3-6 failed")
    leaf = int_cast (cdddr (tree))
    call check (leaf == 7, "test0030-3-7 failed")
    tree = build_tree (1, 4, 0)
    leaf = int_cast (caaaar (tree))
    call check (leaf == 0, "test0030-4-0 failed")
    leaf = int_cast (cdaaar (tree))
    call check (leaf == 1, "test0030-4-1 failed")
    leaf = int_cast (cadaar (tree))
    call check (leaf == 2, "test0030-4-2 failed")
    leaf = int_cast (cddaar (tree))
    call check (leaf == 3, "test0030-4-3 failed")
    leaf = int_cast (caadar (tree))
    call check (leaf == 4, "test0030-4-4 failed")
    leaf = int_cast (cdadar (tree))
    call check (leaf == 5, "test0030-4-5 failed")
    leaf = int_cast (caddar (tree))
    call check (leaf == 6, "test0030-4-6 failed")
    leaf = int_cast (cdddar (tree))
    call check (leaf == 7, "test0030-4-7 failed")
    leaf = int_cast (caaadr (tree))
    call check (leaf == 8, "test0030-4-8 failed")
    leaf = int_cast (cdaadr (tree))
    call check (leaf == 9, "test0030-4-9 failed")
    leaf = int_cast (cadadr (tree))
    call check (leaf == 10, "test0030-4-10 failed")
    leaf = int_cast (cddadr (tree))
    call check (leaf == 11, "test0030-4-11 failed")
    leaf = int_cast (caaddr (tree))
    call check (leaf == 12, "test0030-4-12 failed")
    leaf = int_cast (cdaddr (tree))
    call check (leaf == 13, "test0030-4-13 failed")
    leaf = int_cast (cadddr (tree))
    call check (leaf == 14, "test0030-4-14 failed")
    leaf = int_cast (cddddr (tree))
    call check (leaf == 15, "test0030-4-15 failed")

  contains

    recursive function build_tree (m, n, k) result (tree)
      integer, intent(in) :: m, n, k
      type(cons_t) :: tree

      if (m == n) then
         tree = cons (k, k + 1)
      else
         tree = cons (build_tree (m + 1, n, k), build_tree (m + 1, n, k + 2 ** (n - m)))
      end if
    end function build_tree

  end subroutine test0030

  subroutine test0040
    type(gcroot_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, tail

    lst = list (123)
    call check (car (lst) .eqi. 123, "test0040-0010 failed")
    call check (is_nil (cdr (lst)), "test0040-0020 failed")
    call unlist (lst, obj1)
    call check (obj1 .eqi. 123, "test0040-0030 failed")
    call unlist_with_tail (lst, obj1, tail)
    call check (obj1 .eqi. 123, "test0040-0040 failed")
    call check (is_nil (tail), "test0040-0050 failed")

    lst = list (1, 2, 3, 4, 5)
    call check (first (lst) .eqi. 1, "test0040-0110 failed")
    call check (second (lst) .eqi. 2, "test0040-0120 failed")
    call check (third (lst) .eqi. 3, "test0040-0130 failed")
    call check (fourth (lst) .eqi. 4, "test0040-0140 failed")
    call check (fifth (lst) .eqi. 5, "test0040-0150 failed")
    call check (is_nil (cddr (cdddr (lst))), "test0040-0160 failed")
    call unlist (lst, obj1, obj2, obj3, obj4, obj5)
    call check (obj1 .eqi. 1, "test0040-0210 failed")
    call check (obj2 .eqi. 2, "test0040-0220 failed")
    call check (obj3 .eqi. 3, "test0040-0230 failed")
    call check (obj4 .eqi. 4, "test0040-0240 failed")
    call check (obj5 .eqi. 5, "test0040-0250 failed")
    call unlist_with_tail (lst, obj1, obj2, obj3, obj4, obj5, tail)
    call check (obj1 .eqi. 1, "test0040-0310 failed")
    call check (obj2 .eqi. 2, "test0040-0320 failed")
    call check (obj3 .eqi. 3, "test0040-0330 failed")
    call check (obj4 .eqi. 4, "test0040-0340 failed")
    call check (obj5 .eqi. 5, "test0040-0350 failed")
    call check (is_nil (tail), "test0040-0360 failed")
    call unlist_with_tail (lst, obj1, obj2, obj3, obj4, tail)
    call check (obj1 .eqi. 1, "test0040-0410 failed")
    call check (obj2 .eqi. 2, "test0040-0420 failed")
    call check (obj3 .eqi. 3, "test0040-0430 failed")
    call check (obj4 .eqi. 4, "test0040-0440 failed")
    call check (car (tail) .eqi. 5, "test0040-0450 failed")
    call check (is_nil (cdr (tail)), "test0040-0460 failed")

    lst = list3_with_tail (1, 2, 3, list (4, 5))
    call check (first (lst) .eqi. 1, "test0040-1110 failed")
    call check (second (lst) .eqi. 2, "test0040-1120 failed")
    call check (third (lst) .eqi. 3, "test0040-1130 failed")
    call check (fourth (lst) .eqi. 4, "test0040-1140 failed")
    call check (fifth (lst) .eqi. 5, "test0040-1150 failed")
    call check (is_nil (cddr (cdddr (lst))), "test0040-1160 failed")
    call unlist (lst, obj1, obj2, obj3, obj4, obj5)
    call check (obj1 .eqi. 1, "test0040-1210 failed")
    call check (obj2 .eqi. 2, "test0040-1220 failed")
    call check (obj3 .eqi. 3, "test0040-1230 failed")
    call check (obj4 .eqi. 4, "test0040-1240 failed")
    call check (obj5 .eqi. 5, "test0040-1250 failed")
    call unlist_with_tail (lst, obj1, obj2, obj3, obj4, obj5, tail)
    call check (obj1 .eqi. 1, "test0040-1310 failed")
    call check (obj2 .eqi. 2, "test0040-1320 failed")
    call check (obj3 .eqi. 3, "test0040-1330 failed")
    call check (obj4 .eqi. 4, "test0040-1340 failed")
    call check (obj5 .eqi. 5, "test0040-1350 failed")
    call check (is_nil (tail), "test0040-1360 failed")
    call unlist_with_tail (lst, obj1, obj2, obj3, obj4, tail)
    call check (obj1 .eqi. 1, "test0040-1410 failed")
    call check (obj2 .eqi. 2, "test0040-1420 failed")
    call check (obj3 .eqi. 3, "test0040-1430 failed")
    call check (obj4 .eqi. 4, "test0040-1440 failed")
    call check (car (tail) .eqi. 5, "test0040-1450 failed")
    call check (is_nil (cdr (tail)), "test0040-1460 failed")
  end subroutine test0040

  subroutine test0050
    type(gcroot_t) :: tail

    call check (list_equal (int_eq, nil, nil), "test0050-0010 failed")
    call check (list_equal (int_eq, list (1), list (1)), "test0050-0020 failed")
    call check (list_equal (int_eq, list (1, 2), list (1, 2)), "test0050-0030 failed")
    call check (list_equal (int_eq, list (1, 2, 3, 4, 5), list (1, 2, 3, 4, 5)), "test0050-0040 failed")

    call check (.not. list_equal (int_eq, nil, list (1)), "test0050-0110 failed")
    call check (.not. list_equal (int_eq, list (1), nil), "test0050-0120 failed")
    call check (.not. list_equal (int_eq, list (1, 2, 3, 4), list (1, 2, 3, 4, 5)), "test0050-0130 failed")
    call check (.not. list_equal (int_eq, list (1, 2, 3, 4, 5), list (1, 2, 3, 4)), "test0050-0140 failed")
    call check (.not. list_equal (int_eq, list (1, 2, 3, 4, 5), list (1, 2, 3, 4, 6)), "test0050-0150 failed")

    ! Check some lists with shared tails.
    tail = list (3, 2, 1)
    call check (list_equal (int_eq, tail, tail), "test0050-0200 failed")
    call check (list_equal (int_eq, 5 ** cons (4, tail), 5 ** cons (4, tail)), "test0050-0210 failed")
    call check (.not. list_equal (int_eq, 5 ** cons (4, tail), 5 ** cons (40, tail)), "test0050-0220 failed")

    ! Check with zero list arguments.
    call check (list_equal (int_eq), "test0050-0300 failed")

    ! Check with one list argument.
    call check (list_equal (int_eq, list (1, 2, 3)), "test0050-0310 failed")

    ! Check with multiple list arguments.
    call check (list_equal (int_eq, list (1, 2, 3), list (1, 2, 3), list (1, 2, 3), list (1, 2, 3)), "test0050-0320 failed")
    call check (.not. list_equal (int_eq, list (1, 2, 3), list (1, 2, 3), list (1, 6, 3), list (1, 2, 3)), "test0050-0330 failed")
    call check (.not. list_equal (int_eq, list (1, 2, 3), list (1, 2, 3), list (1, 3), list (1, 2, 3)), "test0050-0340 failed")
  end subroutine test0050

  subroutine test0060
    type(gcroot_t) :: lst1, lst2, lst3

    lst1 = list (1)
    lst2 = list (1, 2)
    lst3 = list (1, 2, 3)

    call check (list_equal (int_eq, take (nil, 0_sz), nil), "test0060-0010 failed")
    call check (list_equal (int_eq, take (lst1, 0_sz), nil), "test0060-0020 failed")
    call check (list_equal (int_eq, take (lst3, 0_sz), nil), "test0060-0030 failed")

    call check (list_equal (int_eq, take (lst1, 1_sz), lst1), "test0060-0040 failed")
    call check (list_equal (int_eq, take (lst3, 1_sz), lst1), "test0060-0050 failed")
    call check (list_equal (int_eq, take (lst3, 2_sz), lst2), "test0060-0060 failed")
    call check (list_equal (int_eq, take (lst3, 3_sz), lst3), "test0060-0070 failed")
  end subroutine test0060

  subroutine test0065
    type(cons_t) :: lst1, lst2, lst3

    lst1 = list (1)
    lst2 = list (1, 2)
    lst3 = list (1, 2, 3)

    call check (list_equal (int_eq, take (nil, 0_sz), nil), "test0065-0010 failed")
    call check (list_equal (int_eq, take (lst1, 0_sz), nil), "test0065-0020 failed")
    call check (list_equal (int_eq, take (lst3, 0_sz), nil), "test0065-0030 failed")

    call check (list_equal (int_eq, take (lst1, 1_sz), lst1), "test0065-0040 failed")
    call check (list_equal (int_eq, take (lst3, 1_sz), lst1), "test0065-0050 failed")
    call check (list_equal (int_eq, take (lst3, 2_sz), lst2), "test0065-0060 failed")
    call check (list_equal (int_eq, take (lst3, 3_sz), lst3), "test0065-0070 failed")

    call check (list_equal (int_eq, take (lst3, 3), lst3), "test0065-0075 failed")

    call check (list_equal (int_eq, takex (nil, 0_sz), nil), "test0065-1010 failed")
    call check (list_equal (int_eq, takex (list (1), 0_sz), nil), "test0065-1020 failed")
    call check (list_equal (int_eq, takex (list (1, 2, 3), 0_sz), nil), "test0065-1030 failed")

    call check (list_equal (int_eq, takex (list (1), 1_sz), lst1), "test0065-1040 failed")
    call check (list_equal (int_eq, takex (list (1, 2, 3), 1_sz), lst1), "test0065-1050 failed")
    call check (list_equal (int_eq, takex (list (1, 2, 3), 2_sz), lst2), "test0065-1060 failed")
    call check (list_equal (int_eq, takex (list (1, 2, 3), 3_sz), lst3), "test0065-1070 failed")

    call check (list_equal (int_eq, takex (list (1, 2, 3), 3), lst3), "test0065-1075 failed")
  end subroutine test0065

  subroutine test0070

    ! iota and list_ref

    type(gcroot_t) :: lst
    integer(sz) :: n
    integer(sz) :: i

    do n = 0_sz, 1000_sz, 100_sz

       lst = iota (n)
       do i = 0_sz, n - 1_sz, 100_sz
          call check (list_ref0 (lst, i) .eqsz. i, "test0070-0010 failed")
          call check (list_ref1 (lst, i + 1) .eqsz. i, "test0070-0020 failed")
          call check (list_refn (lst, -50_sz, i - 50) .eqsz. i, "test0070-0030 failed")
       end do

       lst = iota (n, 1_sz)
       do i = 0_sz, n - 1_sz, 100_sz
          call check (list_ref0 (lst, i) .eqsz. i + 1, "test0070-0040 failed")
          call check (list_ref1 (lst, i + 1) .eqsz. i + 1, "test0070-0050 failed")
          call check (list_refn (lst, -50_sz, i - 50) .eqsz. i + 1, "test0070-0060 failed")
       end do

       lst = iota (n, 100_sz, -10_sz)
       do i = 0_sz, n - 1_sz, 100_sz
          call check (list_ref0 (lst, i) .eqsz. 100 - (10 * i), "test0070-0070 failed")
          call check (list_ref1 (lst, i + 1) .eqsz. 100 - (10 * i), "test0070-0080 failed")
          call check (list_refn (lst, -50_sz, i - 50) .eqsz. 100 - (10 * i), "test0070-0090 failed")
       end do

    end do
  end subroutine test0070

  subroutine test0075

    ! iota and list_ref

    type(gcroot_t) :: lst
    integer :: n
    integer :: i

    do n = 0, 1000, 100

       lst = iota (n)
       do i = 0, n - 1, 100
          call check (list_ref0 (lst, i) .eqi. i, "test0075-0010 failed")
          call check (list_ref1 (lst, i + 1) .eqi. i, "test0075-0020 failed")
          call check (list_refn (lst, -50, i - 50) .eqi. i, "test0075-0030 failed")
       end do

       lst = iota (n, 1)
       do i = 0, n - 1, 100
          call check (list_ref0 (lst, i) .eqi. i + 1, "test0075-0040 failed")
          call check (list_ref1 (lst, i + 1) .eqi. i + 1, "test0075-0050 failed")
          call check (list_refn (lst, -50, i - 50) .eqi. i + 1, "test0075-0060 failed")
       end do

       lst = iota (n, 100, -10)
       do i = 0, n - 1, 100
          call check (list_ref0 (lst, i) .eqi. 100 - (10 * i), "test0075-0075 failed")
          call check (list_ref1 (lst, i + 1) .eqi. 100 - (10 * i), "test0075-0080 failed")
          call check (list_refn (lst, -50, i - 50) .eqi. 100 - (10 * i), "test0075-0090 failed")
       end do

    end do
  end subroutine test0075

  subroutine test0080
    call check (list_equal (int_eq, make_list (0_sz, 5), nil), "test0080-0010 failed")
    call check (list_equal (int_eq, make_list (1_sz, 5), 5 ** nil), "test0080-0020 failed")
    call check (list_equal (int_eq, make_list (5_sz, 5), list (5, 5, 5, 5, 5)), "test0080-0030 failed")

    call check (list_equal (int_eq, make_list (0, 5), nil), "test0080-0040 failed")
    call check (list_equal (int_eq, make_list (1, 5), 5 ** nil), "test0080-0050 failed")
    call check (list_equal (int_eq, make_list (5, 5), list (5, 5, 5, 5, 5)), "test0080-0060 failed")

    call check (length (make_list (5_sz)) == 5, "test0080-0070 failed")
    call check (length (make_list (5)) == 5, "test0080-0080 failed")
    call check (length (make_list (0_sz)) == 0, "test0080-0090 failed")
    call check (length (make_list (0)) == 0, "test0080-0100 failed")
  end subroutine test0080

  subroutine test0090
    call check (list_equal (real_eq, list_tabulate0 (0_sz, make_real), nil), "test0090-0010 failed")
    call check (list_equal (real_eq, list_tabulate1 (0_sz, make_real), nil), "test0090-0020 failed")
    call check (list_equal (real_eq, list_tabulaten (0_sz, -1_sz, make_real), nil), "test0090-0030 failed")

    call check (list_equal (real_eq, list_tabulate0 (1_sz, make_real), 0.0 ** nil), "test0090-0040 failed")
    call check (list_equal (real_eq, list_tabulate1 (1_sz, make_real), 1.0 ** nil), "test0090-0050 failed")
    call check (list_equal (real_eq, list_tabulaten (1_sz, -1_sz, make_real), (-1.0) ** nil), "test0090-0060 failed")

    call check (list_equal (real_eq, list_tabulate0 (5_sz, make_real), list (0.0, 1.0, 2.0, 3.0, 4.0)), &
         "test0090-0070 failed")
    call check (list_equal (real_eq, list_tabulate1 (5_sz, make_real), list (1.0, 2.0, 3.0, 4.0, 5.0)), &
         "test0090-0080 failed")
    call check (list_equal (real_eq, list_tabulaten (5_sz, -1_sz, make_real), list (-1.0, 0.0, 1.0, 2.0, 3.0)), &
         "test0090-0090 failed")
  contains

    subroutine make_real (i, x)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(out) :: x
      x = real (i)
    end subroutine make_real

  end subroutine test0090

  subroutine test0100
    call check (list_equal (int_eq, reverse (nil), nil), "test0100-0010 failed")
    call check (list_equal (int_eq, reverse (123 ** nil), 123 ** nil), "test0100-0020 failed")
    call check (list_equal (int_eq, reverse (list (1, 2, 3, 4, 5)), list (5, 4, 3, 2, 1)), "test0100-0030 failed")

    call check (list_equal (int_eq, reversex (nil), nil), "test0100-0040 failed")
    call check (list_equal (int_eq, reversex (123 ** nil), 123 ** nil), "test0100-0050 failed")
    call check (list_equal (int_eq, reversex (list (1, 2, 3, 4, 5)), list (5, 4, 3, 2, 1)), "test0100-0060 failed")
  end subroutine test0100

  subroutine test0110
    call check (list_equal (int_eq, take (make_circular (1 ** nil), 5_sz), list (1, 1, 1, 1, 1)), &
         "test0110-0010 failed")
    call check (list_equal (int_eq, take (circular_list (1), 5_sz), list (1, 1, 1, 1, 1)), &
         "test0110-0015 failed")
    call check (list_equal (int_eq, &
         take (make_circular (1 ** 2 ** 3 ** nil), 10_sz), &
         1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** nil), &
         "test0110-0020 failed")
    call check (list_equal (int_eq, &
         take (circular_list (1, 2, 3), 10_sz), &
         1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** nil), &
         "test0110-0025 failed")

    call check (list_equal (int_eq, take (make_circularx (1 ** nil), 5_sz), list (1, 1, 1, 1, 1)), &
         "test0110-0030 failed")
    call check (list_equal (int_eq, &
         take (make_circularx (1 ** 2 ** 3 ** nil), 10_sz), &
         1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** nil), &
         "test0110-0040 failed")

    call check (is_circular_list (make_circular (1 ** nil)), "test0110-0050 failed")
    call check (is_circular_list (circular_list (1)), "test0110-0055 failed")
    call check (is_circular_list (make_circular (1 ** 2 ** 3 ** nil)), "test0110-0060 failed")
    call check (is_circular_list (circular_list (1, 2, 3)), "test0110-0065 failed")
    call check (is_circular_list (4 ** 5 ** make_circular (1 ** 2 ** 3 ** nil)), "test0110-0070 failed")
    call check (is_circular_list (4 ** 5 ** circular_list (1, 2, 3)), "test0110-0075 failed")

    call check (.not. is_proper_list (make_circular (1 ** nil)), "test0110-0080 failed")
    call check (.not. is_proper_list (circular_list (1)), "test0110-0085 failed")
    call check (.not. is_proper_list (make_circular (1 ** 2 ** 3 ** nil)), "test0110-0090 failed")
    call check (.not. is_proper_list (circular_list (1, 2, 3)), "test0110-0095 failed")
    call check (.not. is_proper_list (4 ** 5 ** make_circular (1 ** 2 ** 3 ** nil)), "test0110-0100 failed")
    call check (.not. is_proper_list (4 ** 5 ** circular_list (1, 2, 3)), "test0110-0105 failed")

    call check (.not. is_dotted_list (make_circular (1 ** nil)), "test0110-0110 failed")
    call check (.not. is_dotted_list (circular_list (1)), "test0110-0115 failed")
    call check (.not. is_dotted_list (make_circular (1 ** 2 ** 3 ** nil)), "test0110-0120 failed")
    call check (.not. is_dotted_list (circular_list (1, 2, 3)), "test0110-0125 failed")
    call check (.not. is_dotted_list (4 ** 5 ** make_circular (1 ** 2 ** 3 ** nil)), "test0110-0130 failed")
    call check (.not. is_dotted_list (4 ** 5 ** circular_list (1, 2, 3)), "test0110-0135 failed")
  end subroutine test0110

  subroutine test0120
    call check (last (1 ** nil) .eqi. 1, "test0120-0010 failed")
    call check (last (1 ** 2 ** 3 ** nil) .eqi. 3, "test0120-0020 failed")
  end subroutine test0120

  subroutine test0130
    type(gcroot_t) :: lst1, lst2

    lst1 = nil
    lst2 = list_copy (lst1)
    call check (list_equal (int_eq, lst1, lst2), "test0130-0010 failed")

    lst1 = 123 ** nil
    lst2 = list_copy (lst1)
    call check (list_equal (int_eq, lst1, lst2), "test0130-0020 failed")
    call check (is_nil (cdr (last_pair (lst2))), "test0130-0025 failed")

    lst1 = iota (100_sz, 1_sz)
    lst2 = list_copy (lst1)
    call check (list_equal (size_kind_eq, lst1, lst2), "test0130-0030 failed")
    call check (is_nil (cdr (last_pair (lst2))), "test0130-0035 failed")

    ! Test a dotted list.
    lst1 = iota (100_sz, 1_sz)
    call set_cdr (last_pair (lst1), 123)
    lst2 = list_copy (lst1)
    call check (cdr (last_pair (lst2)) .eqi. 123, "test0130-0040 failed")

    ! Test a degenerate dotted list.
    lst1 = 123
    lst2 = list_copy (lst1)
    call check ((.val. lst2) .eqi. 123, "test0130-0050 failed")
  end subroutine test0130

  subroutine test0140
    call check (length (nil) == 0_sz, "test0140-0010 failed")
    call check (length (1 ** nil) == 1_sz, "test0140-0020 failed")
    call check (length (cons (1, 2)) == 1_sz, "test0140-0025 failed")
    call check (length (iota (100_sz)) == 100_sz, "test0140-0030 failed")

    call check (lengthc (nil) == 0_sz, "test0140-0110 failed")
    call check (lengthc (1 ** nil) == 1_sz, "test0140-0120 failed")
    call check (lengthc (cons (1, 2)) == 1_sz, "test0140-0125 failed")
    call check (lengthc (iota (100_sz)) == 100_sz, "test0140-0130 failed")
    call check (lengthc (make_circular (iota (100_sz))) == -1_sz, "test0140-0140 failed")
    call check (lengthc (123 ** 456 ** make_circular (iota (100_sz))) == -1_sz, "test0140-0150 failed")
  end subroutine test0140

  subroutine test0150
    type(gcroot_t) :: lst

    call check (list_equal (int_eq, take_right (nil, 0_sz), nil), "test0150-0010 failed")
    call check (list_equal (int_eq, take_right (list (1), 0_sz), nil), "test0150-0020 failed")
    call check (list_equal (size_kind_eq, take_right (iota (100_sz), 0_sz), nil), "test0150-0030 failed")

    call check (list_equal (int_eq, take_right (list (1), 1_sz), list (1)), "test0150-0040 failed")
    call check (list_equal (size_kind_eq, take_right (iota (100_sz), 1_sz), list (99_sz)), "test0150-0050 failed")

    call check (list_equal (size_kind_eq, take_right (iota (100_sz), 5_sz), iota (5_sz, 95_sz)), "test0150-0060 failed")
    call check (list_equal (int_eq, take_right (iota (100), 5), iota (5, 95)), "test0150-0065 failed")

    call check (take_right (5, 0_sz) .eqi. 5, "test0150-0070 failed")
    call check (take_right (cons (4, 5), 0_sz) .eqi. 5, "test0150-0080 failed")
    call check (car (take_right (3 ** cons (4, 5), 1_sz)) .eqi. 4, "test0150-0090 failed")
    call check (cdr (take_right (3 ** cons (4, 5), 1_sz)) .eqi. 5, "test0150-0100 failed")

    call check (list_equal (int_eq, drop (nil, 0_sz), nil), "test0150-1010 failed")
    call check (list_equal (int_eq, drop (list (1), 1_sz), nil), "test0150-1020 failed")
    call check (list_equal (size_kind_eq, drop (iota (100_sz), 100_sz), nil), "test0150-1030 failed")

    call check (is_nil (drop (list (1), 1_sz)), "test0150-1040 failed")
    call check (list_equal (size_kind_eq, drop (iota (100_sz), 99_sz), list (99_sz)), "test0150-1050 failed")

    call check (list_equal (size_kind_eq, drop (iota (100_sz), 95_sz), iota (5_sz, 95_sz)), "test0150-1060 failed")
    call check (list_equal (int_eq, drop (iota (100), 95), iota (5, 95)), "test0150-1065 failed")

    call check (drop (5, 0_sz) .eqi. 5, "test0150-1070 failed")
    call check (drop (cons (4, 5), 1_sz) .eqi. 5, "test0150-1080 failed")
    call check (car (drop (3 ** cons (4, 5), 1_sz)) .eqi. 4, "test0150-1090 failed")
    call check (cdr (drop (3 ** cons (4, 5), 1_sz)) .eqi. 5, "test0150-1100 failed")

    lst = list (1, 2, 3)

    call check (list_equal (int_eq, take_right (lst, 0_sz), nil), "test0150-2010 failed")
    call check (list_equal (int_eq, take_right (lst, 1_sz), list (3)), "test0150-2020 failed")
    call check (list_equal (int_eq, take_right (lst, 2_sz), list (2, 3)), "test0150-2030 failed")
    call check (list_equal (int_eq, take_right (lst, 3_sz), lst), "test0150-2040 failed")

    call check (list_equal (int_eq, drop (lst, 0_sz), lst), "test0150-3010 failed")
    call check (list_equal (int_eq, drop (lst, 1_sz), list (2, 3)), "test0150-3020 failed")
    call check (list_equal (int_eq, drop (lst, 2_sz), list (3)), "test0150-3030 failed")
    call check (list_equal (int_eq, drop (lst, 3_sz), nil), "test0150-3040 failed")
  end subroutine test0150

  subroutine test0160
    call check (list_equal (int_eq, drop_right (nil, 0_sz), nil), "test0160-0010 failed")
    call check (list_equal (int_eq, drop_right (list (1), 0_sz), list (1)), "test0160-0020 failed")
    call check (list_equal (int_eq, drop_right (list (1), 1_sz), nil), "test0160-0030 failed")
    call check (list_equal (size_kind_eq, drop_right (iota (100_sz), 0_sz), iota (100_sz)), "test0160-0040 failed")
    call check (list_equal (size_kind_eq, drop_right (iota (100_sz), 100_sz), nil), "test0160-0050 failed")
    call check (list_equal (size_kind_eq, drop_right (iota (100_sz), 50_sz), iota (50_sz)), "test0160-0060 failed")
    call check (list_equal (size_kind_eq, drop_right (iota (100_sz), 25_sz), iota (75_sz)), "test0160-0070 failed")
    call check (list_equal (size_kind_eq, drop_right (iota (100_sz), 75_sz), iota (25_sz)), "test0160-0080 failed")
    call check (list_equal (int_eq, drop_right (iota (100), 75), iota (25)), "test0160-0085 failed")

    call check (list_equal (int_eq, drop_rightx (nil, 0_sz), nil), "test0160-1010 failed")
    call check (list_equal (int_eq, drop_rightx (list (1), 0_sz), list (1)), "test0160-1020 failed")
    call check (list_equal (int_eq, drop_rightx (list (1), 1_sz), nil), "test0160-1030 failed")
    call check (list_equal (size_kind_eq, drop_rightx (iota (100_sz), 0_sz), iota (100_sz)), "test0160-1040 failed")
    call check (list_equal (size_kind_eq, drop_rightx (iota (100_sz), 100_sz), nil), "test0160-1050 failed")
    call check (list_equal (size_kind_eq, drop_rightx (iota (100_sz), 50_sz), iota (50_sz)), "test0160-1060 failed")
    call check (list_equal (size_kind_eq, drop_rightx (iota (100_sz), 25_sz), iota (75_sz)), "test0160-1070 failed")
    call check (list_equal (size_kind_eq, drop_rightx (iota (100_sz), 75_sz), iota (25_sz)), "test0160-1080 failed")
    call check (list_equal (int_eq, drop_rightx (iota (100), 75), iota (25)), "test1160-0085 failed")
  end subroutine test0160

  subroutine test0170
    type(cons_t) :: lst_left
    class(*), allocatable :: lst_right

    call do_split_at (nil, 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-0010 failed")
    call check (is_nil (lst_right), "test0170-0020 failed")

    call do_split_at (list (1, 2, 3), 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-0030 failed")
    call check (list_equal (int_eq, lst_right, list (1, 2, 3)), "test0170-0040 failed")

    call do_split_at (list (1, 2, 3), 1_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1)), "test0170-0050 failed")
    call check (list_equal (int_eq, lst_right, list (2, 3)), "test0170-0060 failed")

    call do_split_at (list (1, 2, 3), 2_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2)), "test0170-0070 failed")
    call check (list_equal (int_eq, lst_right, list (3)), "test0170-0080 failed")

    call do_split_at (list (1, 2, 3), 2, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2)), "test0170-0085 failed")
    call check (list_equal (int_eq, lst_right, list (3)), "test0170-0086 failed")

    call do_split_at (list (1, 2, 3), 3_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2, 3)), "test0170-0090 failed")
    call check (is_nil (lst_right), "test0170-0100 failed")

    call do_split_at (1 ** cons (2, 3), 1_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1)), "test0170-0110 failed")
    call check (car (lst_right) .eqi. 2, "test0170-0120 failed")
    call check (cdr (lst_right) .eqi. 3, "test0170-0130 failed")

    call do_split_at (1 ** cons (2, 3), 2_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2)), "test0170-0140 failed")
    call check (lst_right .eqi. 3, "test0170-0150 failed")

    call do_split_atx (nil, 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-1010 failed")
    call check (is_nil (lst_right), "test0170-1020 failed")

    call do_split_atx (list (1, 2, 3), 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-1030 failed")
    call check (list_equal (int_eq, lst_right, list (1, 2, 3)), "test0170-1040 failed")

    call do_split_atx (list (1, 2, 3), 1_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1)), "test0170-1050 failed")
    call check (list_equal (int_eq, lst_right, list (2, 3)), "test0170-1060 failed")

    call do_split_atx (list (1, 2, 3), 2_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2)), "test0170-1070 failed")
    call check (list_equal (int_eq, lst_right, list (3)), "test0170-1080 failed")

    call do_split_atx (list (1, 2, 3), 2, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2)), "test0170-1085 failed")
    call check (list_equal (int_eq, lst_right, list (3)), "test0170-1086 failed")

    call do_split_atx (list (1, 2, 3), 3_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2, 3)), "test0170-1090 failed")
    call check (is_nil (lst_right), "test0170-1100 failed")

    call do_split_atx (1 ** cons (2, 3), 1_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1)), "test0170-1110 failed")
    call check (car (lst_right) .eqi. 2, "test0170-1120 failed")
    call check (cdr (lst_right) .eqi. 3, "test0170-1130 failed")

    call do_split_atx (1 ** cons (2, 3), 2_sz, lst_left, lst_right)
    call check (list_equal (int_eq, lst_left, list (1, 2)), "test0170-1140 failed")
    call check (lst_right .eqi. 3, "test0170-1150 failed")
  end subroutine test0170

  subroutine test0173
    type(gcroot_t) :: retval

    retval = split_at (nil, 0_sz)
    call check (is_nil (first (retval)), "test0173-0010 failed")
    call check (is_nil (second (retval)), "test0173-0020 failed")

    retval = split_at (list (1, 2, 3), 0_sz)
    call check (is_nil (first (retval)), "test0173-0030 failed")
    call check (list_equal (int_eq, second (retval), list (1, 2, 3)), "test0173-0040 failed")

    retval = split_at (list (1, 2, 3), 1_sz)
    call check (list_equal (int_eq, first (retval), list (1)), "test0173-0050 failed")
    call check (list_equal (int_eq, second (retval), list (2, 3)), "test0173-0060 failed")

    retval = split_at (list (1, 2, 3), 2_sz)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0173-0070 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0173-0080 failed")

    retval = split_at (list (1, 2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0173-0085 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0173-0086 failed")

    retval = split_at (list (1, 2, 3), 3_sz)
    call check (list_equal (int_eq, first (retval), list (1, 2, 3)), "test0173-0090 failed")
    call check (is_nil (second (retval)), "test0173-0100 failed")

    retval = split_at (1 ** cons (2, 3), 1_sz)
    call check (list_equal (int_eq, first (retval), list (1)), "test0173-0110 failed")
    call check (car (second (retval)) .eqi. 2, "test0173-0120 failed")
    call check (cdr (second (retval)) .eqi. 3, "test0173-0130 failed")

    retval = split_at (1 ** cons (2, 3), 2_sz)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0173-0140 failed")
    call check (second (retval) .eqi. 3, "test0173-0150 failed")

    retval = split_atx (nil, 0_sz)
    call check (is_nil (first (retval)), "test0173-1010 failed")
    call check (is_nil (second (retval)), "test0173-1020 failed")

    retval = split_atx (list (1, 2, 3), 0_sz)
    call check (is_nil (first (retval)), "test0173-1030 failed")
    call check (list_equal (int_eq, second (retval), list (1, 2, 3)), "test0173-1040 failed")

    retval = split_atx (list (1, 2, 3), 1_sz)
    call check (list_equal (int_eq, first (retval), list (1)), "test0173-1050 failed")
    call check (list_equal (int_eq, second (retval), list (2, 3)), "test0173-1060 failed")

    retval = split_atx (list (1, 2, 3), 2_sz)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0173-1070 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0173-1080 failed")

    retval = split_atx (list (1, 2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0173-1085 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0173-1086 failed")

    retval = split_atx (list (1, 2, 3), 3_sz)
    call check (list_equal (int_eq, first (retval), list (1, 2, 3)), "test0173-1090 failed")
    call check (is_nil (second (retval)), "test0173-1100 failed")

    retval = split_atx (1 ** cons (2, 3), 1_sz)
    call check (list_equal (int_eq, first (retval), list (1)), "test0173-1110 failed")
    call check (car (second (retval)) .eqi. 2, "test0173-1120 failed")
    call check (cdr (second (retval)) .eqi. 3, "test0173-1130 failed")

    retval = split_atx (1 ** cons (2, 3), 2_sz)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0173-1140 failed")
    call check (second (retval) .eqi. 3, "test0173-1150 failed")
  end subroutine test0173

  subroutine test0176
    type(gcroot_t) :: retval

    retval = split_at (nil, 0)
    call check (is_nil (first (retval)), "test0176-0010 failed")
    call check (is_nil (second (retval)), "test0176-0020 failed")

    retval = split_at (list (1, 2, 3), 0)
    call check (is_nil (first (retval)), "test0176-0030 failed")
    call check (list_equal (int_eq, second (retval), list (1, 2, 3)), "test0176-0040 failed")

    retval = split_at (list (1, 2, 3), 1)
    call check (list_equal (int_eq, first (retval), list (1)), "test0176-0050 failed")
    call check (list_equal (int_eq, second (retval), list (2, 3)), "test0176-0060 failed")

    retval = split_at (list (1, 2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0176-0070 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0176-0080 failed")

    retval = split_at (list (1, 2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0176-0085 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0176-0086 failed")

    retval = split_at (list (1, 2, 3), 3)
    call check (list_equal (int_eq, first (retval), list (1, 2, 3)), "test0176-0090 failed")
    call check (is_nil (second (retval)), "test0176-0100 failed")

    retval = split_at (1 ** cons (2, 3), 1)
    call check (list_equal (int_eq, first (retval), list (1)), "test0176-0110 failed")
    call check (car (second (retval)) .eqi. 2, "test0176-0120 failed")
    call check (cdr (second (retval)) .eqi. 3, "test0176-0130 failed")

    retval = split_at (1 ** cons (2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0176-0140 failed")
    call check (second (retval) .eqi. 3, "test0176-0150 failed")

    retval = split_atx (nil, 0)
    call check (is_nil (first (retval)), "test0176-1010 failed")
    call check (is_nil (second (retval)), "test0176-1020 failed")

    retval = split_atx (list (1, 2, 3), 0)
    call check (is_nil (first (retval)), "test0176-1030 failed")
    call check (list_equal (int_eq, second (retval), list (1, 2, 3)), "test0176-1040 failed")

    retval = split_atx (list (1, 2, 3), 1)
    call check (list_equal (int_eq, first (retval), list (1)), "test0176-1050 failed")
    call check (list_equal (int_eq, second (retval), list (2, 3)), "test0176-1060 failed")

    retval = split_atx (list (1, 2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0176-1070 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0176-1080 failed")

    retval = split_atx (list (1, 2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0176-1085 failed")
    call check (list_equal (int_eq, second (retval), list (3)), "test0176-1086 failed")

    retval = split_atx (list (1, 2, 3), 3)
    call check (list_equal (int_eq, first (retval), list (1, 2, 3)), "test0176-1090 failed")
    call check (is_nil (second (retval)), "test0176-1100 failed")

    retval = split_atx (1 ** cons (2, 3), 1)
    call check (list_equal (int_eq, first (retval), list (1)), "test0176-1110 failed")
    call check (car (second (retval)) .eqi. 2, "test0176-1120 failed")
    call check (cdr (second (retval)) .eqi. 3, "test0176-1130 failed")

    retval = split_atx (1 ** cons (2, 3), 2)
    call check (list_equal (int_eq, first (retval), list (1, 2)), "test0176-1140 failed")
    call check (second (retval) .eqi. 3, "test0176-1150 failed")
  end subroutine test0176

  subroutine test0180
    type(gcroot_t) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8

    lst1 = iota (10000_sz)
    lst2 = iota (10000_sz)
    lst3 = iota (10000_sz)
    lst4 = iota (10000_sz)
    lst5 = iota (10000_sz)
    lst6 = iota (10000_sz)
    lst7 = iota (10000_sz)
    lst8 = iota (10000_sz)
    call check_heap_size
  end subroutine test0180

  subroutine test0190
    call check (list_equal (int_eq, append (nil, nil), nil), "test0190-0010 failed")
    call check (append (nil, 123) .eqi. 123, "test0190-0020 failed")
    call check (list_equal (int_eq, append (nil, list (123)), list (123)), "test0190-0030 failed")
    call check (list_equal (int_eq, append (list (123), nil), list (123)), "test0190-0040 failed")
    call check (car (append (list (123), 45)) .eqi. 123, "test0190-0050 failed")
    call check (cdr (append (list (123), 45)) .eqi. 45, "test0190-0060 failed")
    call check (list_equal (size_kind_eq, &
         append (iota (75_sz, 1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0190-0070 failed")

    call check (list_equal (int_eq, appendx (nil, nil), nil), "test0190-1010 failed")
    call check (appendx (nil, 123) .eqi. 123, "test0190-1020 failed")
    call check (list_equal (int_eq, appendx (nil, list (123)), list (123)), "test0190-1030 failed")
    call check (list_equal (int_eq, appendx (list (123), nil), list (123)), "test0190-1040 failed")
    call check (car (appendx (list (123), 45)) .eqi. 123, "test0190-1050 failed")
    call check (cdr (appendx (list (123), 45)) .eqi. 45, "test0190-1060 failed")
    call check (list_equal (size_kind_eq, &
         appendx (iota (75_sz, 1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0190-1070 failed")
  end subroutine test0190

  subroutine test0195
    call check (list_equal (int_eq, append_reverse (nil, nil), nil), "test0195-0010 failed")
    call check (append_reverse (nil, 123) .eqi. 123, "test0195-0020 failed")
    call check (list_equal (int_eq, append_reverse (nil, list (123)), list (123)), "test0195-0030 failed")
    call check (list_equal (int_eq, append_reverse (list (123), nil), list (123)), "test0195-0040 failed")
    call check (car (append_reverse (list (123), 45)) .eqi. 123, "test0195-0050 failed")
    call check (cdr (append_reverse (list (123), 45)) .eqi. 45, "test0195-0060 failed")
    call check (list_equal (size_kind_eq, &
         append_reverse (iota (75_sz, 75_sz, -1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0195-0070 failed")

    call check (list_equal (int_eq, append_reversex (nil, nil), nil), "test0195-1010 failed")
    call check (append_reversex (nil, 123) .eqi. 123, "test0195-1020 failed")
    call check (list_equal (int_eq, append_reversex (nil, list (123)), list (123)), "test0195-1030 failed")
    call check (list_equal (int_eq, append_reversex (list (123), nil), list (123)), "test0195-1040 failed")
    call check (car (append_reversex (list (123), 45)) .eqi. 123, "test0195-1050 failed")
    call check (cdr (append_reversex (list (123), 45)) .eqi. 45, "test0195-1060 failed")
    call check (list_equal (size_kind_eq, &
         append_reversex (iota (75_sz, 75_sz, -1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0195-1070 failed")
  end subroutine test0195

  subroutine test0197

    ! Tests of append and appendx for different numbers of arguments.

    ! Zero arguments. Always returns a nil list.
    call check (is_nil (append ()), "test0197-0010 failed")
    call check (is_nil (appendx ()), "test0197-0020 failed")

    ! One arguments. Always simply returns the given list.
    call check (list_equal (int_eq, append (list (1, 2, 3)), list (1, 2, 3)), "test0197-0030 failed")
    call check (list_equal (int_eq, appendx (list (1, 2, 3)), list (1, 2, 3)), "test0197-0040 failed")

    ! Five arguments.
    call check (list_equal (int_eq, &
         append (iota (20, 1), iota (20, 21), iota (20, 41), iota (20, 61), iota (20, 81)), &
         iota (100, 1)), &
         "test0197-0050 failed")
    call check (list_equal (int_eq, &
         appendx (iota (20, 1), iota (20, 21), iota (20, 41), iota (20, 61), iota (20, 81)), &
         iota (100, 1)), &
         "test0197-0050 failed")
  end subroutine test0197

  subroutine test0200
    call check (is_nil (concatenate (nil)), "test0200-0010 failed")
    call check (is_nil (concatenate (123)), "test0200-0020 failed")

    call check (list_equal (int_eq, concatenate (list (123 ** nil)), 123 ** nil), "test0200-0030 failed")
    call check (list_equal (int_eq, concatenate (list (123 ** nil, nil)), 123 ** nil), "test0200-0040 failed")
    call check (list_equal (int_eq, concatenate (list (nil, 123 ** nil)), 123 ** nil), "test0200-0050 failed")

    call check (list_equal (size_kind_eq, &
         concatenate (list (iota (25_sz, 1_sz), nil, iota (50_sz, 26_sz), iota (25_sz, 76_sz), nil)), &
         iota (100_sz, 1_sz)), &
         "test0200-0060 failed")

    call check (is_nil (concatenatex (nil)), "test0200-1010 failed")
    call check (is_nil (concatenatex (123)), "test0200-1020 failed")

    call check (list_equal (int_eq, concatenatex (list (123 ** nil)), 123 ** nil), "test0200-1030 failed")
    call check (list_equal (int_eq, concatenatex (list (123 ** nil, nil)), 123 ** nil), "test0200-1040 failed")
    call check (list_equal (int_eq, concatenatex (list (nil, 123 ** nil)), 123 ** nil), "test0200-1050 failed")

    call check (list_equal (size_kind_eq, &
         concatenatex (list (iota (25_sz, 1_sz), nil, iota (50_sz, 26_sz), iota (25_sz, 76_sz), nil)), &
         iota (100_sz, 1_sz)), &
         "test0200-1060 failed")
  end subroutine test0200

  subroutine test0210
    type(gcroot_t) :: lst1, lst2, lst3, lst4, lst5
    type(gcroot_t) :: zipped1, zipped2, zipped3, zipped4, zipped5
    type(cons_t) :: unzipped1, unzipped2, unzipped3, unzipped4, unzipped5
    type(gcroot_t) :: p
    integer :: i

    lst1 = iota (300, 1)
    lst2 = iota (200, 2)
    lst3 = iota (100, 3)
    lst4 = iota (200, 4)
    lst5 = iota (300, 5)

    zipped1 = zip (lst1)
    zipped2 = zip (lst2, lst3)
    zipped3 = zip (lst2, lst3, lst5)
    zipped4 = zip (lst1, lst2, lst4, lst5)
    zipped5 = zip (lst1, lst2, lst3, lst4, lst5)

    call check (length (zipped1) == 300, "test0210-0010 failed")
    call check (length (zipped2) == 100, "test0210-0020 failed")
    call check (length (zipped3) == 100, "test0210-0030 failed")
    call check (length (zipped4) == 200, "test0210-0040 failed")
    call check (length (zipped5) == 100, "test0210-0050 failed")

    i = 1
    p = zipped1
    do while (is_pair (p))
       call check (list_equal (int_eq, car (p), list (i)), "test0210-0110 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped2
    do while (is_pair (p))
       call check (list_equal (int_eq, car (p), list (i + 1, i + 2)), "test0210-0120 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped3
    do while (is_pair (p))
       call check (list_equal (int_eq, car (p), list (i + 1, i + 2, i + 4)), "test0210-0130 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped4
    do while (is_pair (p))
       call check (list_equal (int_eq, car (p), list (i, i + 1, i + 3, i + 4)), "test0210-0140 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped5
    do while (is_pair (p))
       call check (list_equal (int_eq, car (p), list (i, i + 1, i + 2, i + 3, i + 4)), "test0210-0150 failed")
       i = i + 1
       p = cdr (p)
    end do

    call check (list_equal (int_eq, unzip1f (zipped1), lst1), "test0210-0200 failed")

    call unzip (zipped1, unzipped1)
    call check (list_equal (int_eq, unzipped1, lst1), "test0210-0210 failed")

    call unzip (zipped2, unzipped2, unzipped3)
    call check (list_equal (int_eq, unzipped2, take (lst2, 100)), "test0210-0220 failed")
    call check (list_equal (int_eq, unzipped3, lst3), "test0210-0230 failed")

    call unzip (zipped3, unzipped2, unzipped3, unzipped5)
    call check (list_equal (int_eq, unzipped2, take (lst2, 100)), "test0210-0240 failed")
    call check (list_equal (int_eq, unzipped3, lst3), "test0210-0250 failed")
    call check (list_equal (int_eq, unzipped5, take (lst5, 100)), "test0210-0260 failed")

    call unzip (zipped4, unzipped1, unzipped2, unzipped4, unzipped5)
    call check (list_equal (int_eq, unzipped1, take (lst1, 200)), "test0210-0270 failed")
    call check (list_equal (int_eq, unzipped2, lst2), "test0210-0280 failed")
    call check (list_equal (int_eq, unzipped4, lst4), "test0210-0290 failed")
    call check (list_equal (int_eq, unzipped5, take (lst5, 200)), "test0210-0300 failed")

    call unzip (zipped5, unzipped1, unzipped2, unzipped3, unzipped4, unzipped5)
    call check (list_equal (int_eq, unzipped1, take (lst1, 100)), "test0210-0310 failed")
    call check (list_equal (int_eq, unzipped2, take (lst2, 100)), "test0210-0320 failed")
    call check (list_equal (int_eq, unzipped3, lst3), "test0210-0320 failed")
    call check (list_equal (int_eq, unzipped4, take (lst4, 100)), "test0210-0340 failed")
    call check (list_equal (int_eq, unzipped5, take (lst5, 100)), "test0210-0350 failed")

    call check (is_nil (zip (nil)), "test0210-0410 failed")
    call check (is_nil (zip (nil, iota (10))), "test0210-0420 failed")
    call check (is_nil (zip (iota (10), nil, nil)), "test0210-0430 failed")
    call check (is_nil (zip (iota (10), nil, iota (10), nil)), "test0210-0440 failed")
    call check (is_nil (zip (nil, nil, nil, iota (100), nil)), "test0210-0450 failed")

    call check (is_nil (unzip1f (nil)), "test0210-0500 failed")

    call unzip (nil, unzipped1)
    call check (is_nil (unzipped1), "test0210-0510 failed")

    call unzip (nil, unzipped1, unzipped2)
    call check (is_nil (unzipped1), "test0210-0520 failed")
    call check (is_nil (unzipped2), "test0210-0530 failed")

    call unzip (nil, unzipped1, unzipped2, unzipped3)
    call check (is_nil (unzipped1), "test0210-0540 failed")
    call check (is_nil (unzipped2), "test0210-0550 failed")
    call check (is_nil (unzipped3), "test0210-0560 failed")

    call unzip (nil, unzipped1, unzipped2, unzipped3, unzipped4)
    call check (is_nil (unzipped1), "test0210-0570 failed")
    call check (is_nil (unzipped2), "test0210-0580 failed")
    call check (is_nil (unzipped3), "test0210-0590 failed")
    call check (is_nil (unzipped4), "test0210-0600 failed")

    call unzip (nil, unzipped1, unzipped2, unzipped3, unzipped4, unzipped5)
    call check (is_nil (unzipped1), "test0210-0610 failed")
    call check (is_nil (unzipped2), "test0210-0620 failed")
    call check (is_nil (unzipped3), "test0210-0630 failed")
    call check (is_nil (unzipped4), "test0210-0640 failed")
    call check (is_nil (unzipped5), "test0210-0650 failed")
  end subroutine test0210

  subroutine test0220

    call check (list_count (pred, nil) == 0, "test0220-0010 failed")
    call check (list_count (pred, iota (100)) == 2, "test0220-0020 failed")
    call check (list_count (pred, iota (100, 1)) == 1, "test0220-0030 failed")
    call check (list_count (pred, iota (100, -50)) == 3, "test0220-0040 failed")
    call check (list_count (pred, iota (100, 2)) == 0, "test0220-0050 failed")
    call check (list_count (pred, take (make_circular (list (-1, 0, 1)), 100)) == 100, "test0220-0060 failed")
    call check (list_count (pred, take (make_circular (list (-2, -1, 0, 1, 2)), 100)) == 60, "test0220-0070 failed")

    call check (list_count (pred3, iota (100), nil, iota (1000)) == 0, "test0220-1010 failed")
    call check (list_count (pred3, iota (100), iota (1000), iota (50)) == 2, "test0220-1020 failed")
    call check (list_count (pred3, list (-1, 0, 1), list (1, 1, 1), list (0, -2, -1)) == 2, "test0220-1030 failed")

  contains

    function pred (i) result (bool)
      class(*), intent(in) :: i
      logical :: bool
      integer :: ii
      ii = int_cast (i)
      bool = (-1 <= ii .and. ii <= 1)
      call collect_garbage_now  ! Test whether this messes things up.
    end function pred

    function pred3 (i, j, k) result (bool)
      class(*), intent(in) :: i, j, k
      logical :: bool
      integer :: ii, jj, kk
      ii = int_cast (i)
      jj = int_cast (j)
      kk = int_cast (k)
      bool = (-1 <= ii .and. ii <= 1)
      bool = bool .and. (-1 <= jj .and. jj <= 1)
      bool = bool .and. (-1 <= kk .and. kk <= 1)
      call collect_garbage_now  ! Test whether this messes things up.
    end function pred3

  end subroutine test0220

  subroutine test0230

    ! Tests of fold.

    type(cons_t) :: lst1, lst2, lst3
    type(gcroot_t) :: lst4

    ! Use fold to add the numbers in a list. (An example from SRFI-1.)
    call check (fold (kons_iadd, 0, iota (10, 1)) .eqi. 55, "test0230-0010 failed")

    ! Use fold to reverse a list. (An example from SRFI-1.)
    lst1 = iota (10, 1)
    lst2 = fold (kcons, nil, lst1)
    call check (list_equal (int_eq, lst1, iota (10, 1)), "test0230-0020 failed")
    call check (list_equal (int_eq, lst2, iota (10, 10, -1)), "test0230-0030 failed")

    ! Use fold to do an append-reverse. (An example from SRFI-1.)
    lst1 = iota (10, 11)
    lst2 = iota (10, 10, -1)
    lst3 = fold (kcons, lst1, lst2)
    call check (list_equal (int_eq, lst1, iota (10, 11)), "test0230-0040 failed")
    call check (list_equal (int_eq, lst2, iota (10, 10, -1)), "test0230-0050 failed")
    call check (list_equal (int_eq, lst3, iota (20, 1)), "test0230-0060 failed")

    ! Count how many strings are in a list. (Adapted from an example
    ! in SRFI-1.)
    lst1 = 1 ** 2.0 ** str_t ('3.0') ** str_t ('x') ** 4.0 ** 5_sz ** str_t ('y') ** str_t ('z') &
         ** list (str_t ('a'), str_t ('b'), str_t ('c')) ** nil
    call check (fold (kons_count_str, 0, lst1) .eqi. 4, "test0230-0070 failed")

    ! Find the length of the longest string in a list. (An example
    ! from SRFI-1.)
    lst1 = str_t ('string1') ** str_t ('str2') ** str_t ('This is the longest string.') &
         ** str_t ('a') ** str_t ('b') ** str_t ('c') ** nil
    call check (fold (kons_longer_len, 0, lst1) .eqi. len ('This is the longest string.'), "test0230-0080 failed")

    ! Return the first longest string in a list.
    lst1 = str_t ('string1') ** str_t ('str2') ** str_t ('This is the longest string.') &
         ** str_t ('a') ** str_t ('b') ** str_t ('c') ** nil
    call check (str_t_cast (fold (kons_longer_str, str_t (''), lst1)) == str_t ('This is the longest string.'), &
         "test0230-0090 failed")

    ! Try it again with a nil list.
    call check (str_t_cast (fold (kons_longer_str, str_t (''), nil)) == str_t (''), "test0230-0100 failed")

    ! A zip-reverse of three lists.
    lst1 = list (1, 2, 3, 4)
    lst2 = list (10, 20, 30, 40)
    lst3 = list (100, 200, 300, 400)
    lst4 = fold (klist, nil, lst1, lst2, lst3)
    call check (length (lst4) == 4, "test0230-0200 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 1), list (4, 40, 400)), "test0230-0210 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 2), list (3, 30, 300)), "test0230-0220 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 3), list (2, 20, 200)), "test0230-0230 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 4), list (1, 10, 100)), "test0230-0240 failed")

  contains

    recursive subroutine kons_iadd (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = int_cast (kar) + int_cast (kdr)
      call collect_garbage_now
    end subroutine kons_iadd

    recursive subroutine kcons (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (kar, kdr)
    end subroutine kcons

    recursive subroutine kons_count_str (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      select type (kar)
      class is (str_t)
         kons = int_cast (kdr) + 1
      class default
         kons = int_cast (kdr)
      end select
      call collect_garbage_now
    end subroutine kons_count_str

    recursive subroutine kons_longer_len (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      select type (kar)
      class is (str_t)
         kons = max (kar%length (), int_cast (kdr))
      class default
         kons = int_cast (kdr)
      end select
      call collect_garbage_now
    end subroutine kons_longer_len

    recursive subroutine kons_longer_str (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      type(str_t) :: s1, s2

      call collect_garbage_now
      s1 = str_t_cast (kar)
      s2 = str_t_cast (kdr)
      if (s1%length () <= s2%length ()) then
         kons = s2
      else
         kons = s1
      end if
      call collect_garbage_now
    end subroutine kons_longer_str

    recursive subroutine klist (kar1, kar2, kar3, kdr, kons)
      class(*), intent(in) :: kar1, kar2, kar3, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (list (kar1, kar2, kar3), kdr)
    end subroutine klist

  end subroutine test0230

  subroutine test0240
    type(cons_t) :: lst1, lst2, lst3, lst4

    ! Tests of fold_right.

    ! Use fold_right to copy a list. (An example from SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = fold_right (kcons, nil, lst1)
    call check (list_equal (int_eq, lst1, iota (100, 1)), "test0240-0010 failed")
    call check (list_equal (int_eq, lst2, iota (100, 1)), "test0240-0020 failed")

    ! Keep only the even elements of a list. (An example from SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = fold_right (kcons_if_even, nil, lst1)
    call check (list_equal (int_eq, lst1, iota (100, 1)), "test0240-0030 failed")
    call check (list_equal (int_eq, lst2, iota (50, 2, 2)), "test0240-0040 failed")

    ! Try it again with a nil list.
    lst2 = fold_right (kcons_if_even, nil, nil)
    call check (list_equal (int_eq, lst2, nil), "test0240-0050 failed")

    ! Try it again, but with a non-nil tail.
    lst1 = iota (95, 1)
    lst2 = fold_right (kcons_if_even, 96 ** 98 ** 100 ** nil, lst1)
    call check (list_equal (int_eq, lst1, iota (95, 1)), "test0240-0060 failed")
    call check (list_equal (int_eq, lst2, iota (50, 2, 2)), "test0240-0070 failed")

    ! A zip of three lists.
    lst1 = list (1, 2, 3, 4)
    lst2 = list (10, 20, 30, 40)
    lst3 = list (100, 200, 300, 400)
    lst4 = fold_right (klist, nil, lst1, lst2, lst3)
    call check (length (lst4) == 4, "test0230-0200 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 1), list (1, 10, 100)), "test0240-0210 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 2), list (2, 20, 200)), "test0240-0220 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 3), list (3, 30, 300)), "test0240-0230 failed")
    call check (list_equal (int_eq, list_ref1 (lst4, 4), list (4, 40, 400)), "test0240-0240 failed")

  contains

    recursive subroutine kcons (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (kar, kdr)
    end subroutine kcons

    recursive subroutine kcons_if_even (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      if (mod (int_cast (kar), 2) == 0) then
         kons = cons (kar, kdr)
      else
         kons = kdr
      end if
    end subroutine kcons_if_even

    recursive subroutine klist (kar1, kar2, kar3, kdr, kons)
      class(*), intent(in) :: kar1, kar2, kar3, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (list (kar1, kar2, kar3), kdr)
    end subroutine klist

  end subroutine test0240

  subroutine test0250

    ! Tests of pair_fold.

    type(cons_t) :: lst1, lst2, lst3, lst4, p
    integer :: i

    ! Destructively reverse a list. (An example from SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = pair_fold (ksetcdr, nil, lst1)
    call check (list_equal (int_eq, lst1, 1 ** nil), "test0250-0010 failed")
    call check (list_equal (int_eq, lst2, iota (100, 100, -1)), "test0250-0020 failed")

    ! Enumerate tails in order of ascending length.
    lst1 = iota (100, 1)
    lst2 = pair_fold (kcopy, nil, lst1)
    call check (list_equal (int_eq, lst1, iota (100, 1)), "test0250-0030 failed")
    i = 1
    p = lst2
    do while (is_pair (p))
       call check (list_equal (int_eq, car (p), iota (i, 101 - i)), "test0250-0040 failed")
       i = i + 1
       p = cdr (p)
    end do
    call check (is_nil (p), "test0250-0050 failed")

    ! Add numbers and reverse the list, as if one were using regular
    ! fold instead of pair_fold. (It seems a simple way to test
    ! multiple lists support).
    lst1 = list (1, 2, 3, 4)
    lst2 = list (10, 20, 30, 40)
    lst3 = list (100, 200, 300, 400)
    lst4 = pair_fold (kadd3cdr, nil, lst1, lst2, lst3)
    call check (list_equal (int_eq, lst4, list (444, 333, 222, 111)), "test0250-0200 failed")

  contains

    recursive subroutine ksetcdr (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      call set_cdr (kar, kdr)
      kons = kar
    end subroutine ksetcdr

    recursive subroutine kcopy (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (list_copy (kar), kdr)
    end subroutine kcopy

    recursive subroutine kadd3cdr (kar1, kar2, kar3, kdr, kons)
      class(*), intent(in) :: kar1, kar2, kar3, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (int_cast (car (kar1)) + int_cast (car (kar2)) + int_cast (car (kar3)), kdr)
    end subroutine kadd3cdr

  end subroutine test0250

  subroutine test0260

    ! Tests of pair_fold_right.

    type(cons_t) :: lst1, lst2, lst3, lst4, p
    integer :: i

    ! A wasteful destructive append. (It does many more set_cdr than
    ! are necessary.)
    lst1 = iota (100, 1)
    lst2 = iota (100, 101)
    lst3 = pair_fold_right (ksetcdr, lst2, lst1)
    call check (list_equal (int_eq, lst1, iota (200, 1)), "test0260-0010 failed")
    call check (list_equal (int_eq, lst2, iota (100, 101)), "test0260-0015 failed")
    call check (list_equal (int_eq, lst3, iota (200, 1)), "test0260-0020 failed")

    ! Enumerate tails in order of descending length. (An example from
    ! SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = pair_fold_right (kcopy, nil, lst1)
    call check (list_equal (int_eq, lst1, iota (100, 1)), "test0260-0030 failed")
    i = 1
    p = lst2
    do while (is_pair (p))
       call check (list_equal (int_eq, car (p), iota (101 - i, i)), "test0260-0040 failed")
       i = i + 1
       p = cdr (p)
    end do
    call check (is_nil (p), "test0260-0050 failed")

    ! Add numbers, as if one were using regular fold_right instead of
    ! pair_fold_right. (It seems a simple way to test multiple lists
    ! support).
    lst1 = list (1, 2, 3, 4)
    lst2 = list (10, 20, 30, 40)
    lst3 = list (100, 200, 300, 400)
    lst4 = pair_fold_right (kadd3cdr, nil, lst1, lst2, lst3)
    call check (list_equal (int_eq, lst4, list (111, 222, 333, 444)), "test0260-0200 failed")

  contains

    recursive subroutine ksetcdr (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      call set_cdr (kar, kdr)
      kons = kar
    end subroutine ksetcdr

    recursive subroutine kcopy (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (list_copy (kar), kdr)
    end subroutine kcopy

    recursive subroutine kadd3cdr (kar1, kar2, kar3, kdr, kons)
      class(*), intent(in) :: kar1, kar2, kar3, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = cons (int_cast (car (kar1)) + int_cast (car (kar2)) + int_cast (car (kar3)), kdr)
    end subroutine kadd3cdr

  end subroutine test0260

  subroutine test0270

    ! Tests of reduce.

    type(cons_t) :: lst1

    ! Find the maximum in a list of non-negative integers. (An example
    ! from SRFI-1.)
    lst1 = concatenate (list (iota (10), iota (100), iota (1000), iota (100), iota (10)))
    call check (reduce (kmax, 0, lst1) .eqi. 999, "test0270-0010 failed")

    ! Try it on a nil list.
    lst1 = nil
    call check (reduce (kmax, 0, lst1) .eqi. 0, "test0270-0020 failed")

  contains

    recursive subroutine kmax (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = max (int_cast (kar), int_cast (kdr))
      call collect_garbage_now
    end subroutine kmax

  end subroutine test0270

  subroutine test0280

    ! Tests of reduce_right.

    type(cons_t) :: lst1, lst2, lst3

    ! An implementation of `concatenate'. (An example from SRFI-1.)
    lst1 = list (1 ** 2 ** 3 ** nil, 4 ** 5 ** nil, nil, 6 ** 7 ** 8 ** 9 ** 10 ** nil, nil)
    lst2 = reduce_right (kappend, nil, lst1)
    lst3 = iota (10, 1)
    call check (list_equal (int_eq, lst2, lst3), "test0280-0010 failed")

    ! Try it on a nil list
    lst1 = nil
    lst2 = reduce_right (kappend, nil, lst1)
    lst3 = nil
    call check (list_equal (int_eq, lst2, lst3), "test0280-0020 failed")

  contains

    recursive subroutine kappend (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = append (kar, kdr)
    end subroutine kappend

  end subroutine test0280

  subroutine test0290

    ! Tests of unfold.

    type(cons_t) :: lst1, lst2
    type(gcroot_t) :: head, tail
    class(*), allocatable :: p, q

    ! List of squares 1**2,2**2,...,10**2. (An example from SRFI-1.)
    call check (list_equal (int_eq, &
         unfold (k_gt_10, square_k, increment_k, 1), &
         list (1**2, 2**2, 3**2, 4**2, 5**2, 6**2, 7**2, 8**2, 9**2, 10**2)), &
         "test0290-0010 failed")

    ! Copy a proper list. (An example from SRFI-1.)
    call check (list_equal (int_eq, unfold (is_nil, kcar, kcdr, iota (100, 1)), iota (100, 1)), "test0290-0020 failed")

    ! Copy a possibly dotted list. (An example from SRFI-1.)
    lst1 = iota (100, 1)
    call set_cdr (last_pair (lst1), 101)
    lst2 = unfold (is_not_pair, kcar, kcdr, lst1, kpassthru)
    p = lst1
    q = lst2
    do while (is_pair (p))
       call check (car (p) .eqi. car (q), "test0290-0030 failed")
       if (is_not_pair (cdr (p))) then
          call check (is_not_pair (cdr (q)), "test0290-0040 failed")
          call check (cdr (p) .eqi. cdr (q), "test0290-0050 failed")
       end if
       p = cdr (p)
       q = cdr (q)
    end do

    ! Append head onto tail. (An example from SRFI-1.)
    head = iota (100, 1)
    tail = iota (100, 10, 10)
    lst1 = unfold (is_nil, kcar, kcdr, head, ktail)
    lst2 = append (head, tail)
    call check (list_equal (int_eq, lst1, lst2), "test0290-0060 failed")

    ! The following can be done because we made head and tail gcroot_t
    ! instead of cons_t.
    call collect_garbage_now
    call check (list_equal (int_eq, &
         unfold (is_nil, kcar, kcdr, head, ktail), &
         append(head, tail)), &
         "test0290-0070 failed")

  contains

    recursive function k_gt_10 (k) result (bool)
      class(*), intent(in) :: k
      logical :: bool
      call collect_garbage_now
      bool = (int_cast (k) > 10)
    end function k_gt_10

    recursive subroutine square_k (k, k_sq)
      class(*), intent(in) :: k
      class(*), allocatable, intent(out) :: k_sq
      call collect_garbage_now
      k_sq = (int_cast (k)) ** 2
    end subroutine square_k

    recursive subroutine increment_k (k, k_incr)
      class(*), intent(in) :: k
      class(*), allocatable, intent(out) :: k_incr
      call collect_garbage_now
      k_incr = (int_cast (k)) + 1
    end subroutine increment_k

    recursive subroutine kcar (kons, kar)
      class(*), intent(in) :: kons
      class(*), allocatable, intent(out) :: kar
      call collect_garbage_now
      kar = car (kons)
    end subroutine kcar

    recursive subroutine kcdr (kons, kdr)
      class(*), intent(in) :: kons
      class(*), allocatable, intent(out) :: kdr
      call collect_garbage_now
      kdr = cdr (kons)
    end subroutine kcdr

    recursive subroutine kpassthru (x, y)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: y
      call collect_garbage_now
      y = x
    end subroutine kpassthru

    recursive subroutine ktail (x, tl)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: tl
      call collect_garbage_now
      tl = tail
    end subroutine ktail

  end subroutine test0290

  subroutine test0300

    ! Tests of unfold_right.

    ! List of squares 1**2,2**2,...,10**2. (An example from SRFI-1.)
    call check (list_equal (int_eq, &
         unfold_right (k_eq_0, square_k, decrement_k, 10), &
         list (1**2, 2**2, 3**2, 4**2, 5**2, 6**2, 7**2, 8**2, 9**2, 10**2)), &
         "test0290-0010 failed")

    ! Reverse a proper list. (An example from SRFI-1.)
    call check (list_equal (int_eq, &
         unfold_right (is_nil, kcar, kcdr, iota (100, 1)), &
         iota (100, 100, -1)), &
         "test0290-0020 failed")

    ! Append-reverse a proper list. (An example from SRFI-1.)
    call check (list_equal (int_eq, &
         unfold_right (is_nil, kcar, kcdr, iota (50, 51), iota (50, 50, -1)), &
         iota (100, 100, -1)), &
         "test0290-0030 failed")

  contains

    recursive function k_eq_0 (k) result (bool)
      class(*), intent(in) :: k
      logical :: bool
      call collect_garbage_now
      bool = (int_cast (k) == 0)
    end function k_eq_0

    recursive subroutine square_k (k, k_sq)
      class(*), intent(in) :: k
      class(*), allocatable, intent(out) :: k_sq
      call collect_garbage_now
      k_sq = (int_cast (k)) ** 2
    end subroutine square_k

    recursive subroutine decrement_k (k, k_decr)
      class(*), intent(in) :: k
      class(*), allocatable, intent(out) :: k_decr
      call collect_garbage_now
      k_decr = (int_cast (k)) - 1
    end subroutine decrement_k

    recursive subroutine kcar (kons, kar)
      class(*), intent(in) :: kons
      class(*), allocatable, intent(out) :: kar
      call collect_garbage_now
      kar = car (kons)
    end subroutine kcar

    recursive subroutine kcdr (kons, kdr)
      class(*), intent(in) :: kons
      class(*), allocatable, intent(out) :: kdr
      call collect_garbage_now
      kdr = cdr (kons)
    end subroutine kcdr

  end subroutine test0300

  subroutine test0310

    ! Tests of map/map_in_order.

    type(cons_t) :: lst1, lst2, lst3
    type(cons_t) :: lst4, lst5, lst6
    type(gcroot_t) :: rooted_lst7
    integer :: count

    ! Select the second element of each list in a list-of-lists. (An example from SRFI-1.)
    lst1 = list (list (1, 2), list (4, 5), list (7, 8))
    lst2 = map (kcadr, lst1)
    lst3 = list (2, 5, 8)
    call check (list_equal (int_eq, lst2, lst3), "test0310-0010 failed")
    ! Try it with a nil list.
    call check (list_equal (int_eq, map (kcadr, nil), nil), "test0310-0015 failed")
    ! Try it with a length-1 list.
    call check (list_equal (int_eq, map (kcadr, list (list (1, 2))), list (2)), "test0310-0017 failed")
    !
    ! Because we did not use gcroot_t, we need to reassign all the
    ! lists. (But this way is a better test.)
    !
    lst1 = list (list (1, 2), list (4, 5), list (7, 8))
    lst2 = map_in_order (kcadr, lst1)
    lst3 = list (2, 5, 8)
    call check (list_equal (int_eq, lst2, lst3), "test0310-0020 failed")
    ! Try it with a nil list.
    call check (list_equal (int_eq, map_in_order (kcadr, nil), nil), "test0310-0025 failed")
    ! Try it with a length-1 list.
    call check (list_equal (int_eq, map_in_order (kcadr, list (list (1, 2))), list (2)), "test0310-0027 failed")

    ! Raise the elements of a list to their own power. (An example from SRFI-1.)
    call check (list_equal (int_eq, &
         map (kselfpower, iota (5, 1)), &
         list (1, 4, 27, 256, 3125)), &
         "test0310-0030 failed")
    call check (list_equal (int_eq, &
         map_in_order (kselfpower, iota (5, 1)), &
         list (1, 4, 27, 256, 3125)), &
         "test0310-0040 failed")

    ! Add the elements of two lists. (An example from SRFI-1.)
    lst1 = list (1, 2, 3)
    lst2 = list (4, 5, 6)
    lst3 = map (kadd, lst1, lst2)
    call check (list_equal (int_eq, lst3, list (5, 7, 9)), "test0310-0050 failed")
    lst1 = list (1, 2, 3)
    lst2 = list (4, 5, 6)
    lst3 = map_in_order (kadd, lst1, lst2)
    call check (list_equal (int_eq, lst3, list (5, 7, 9)), "test0310-0060 failed")

    ! Add the elements of five lists.
    lst1 = iota (5, 1)
    lst2 = iota (5, 2)
    lst3 = iota (5, 3)
    lst4 = iota (5, 4)
    lst5 = iota (5, 5)
    lst6 = map (kadd5, lst1, lst2, lst3, lst4, lst5)
    rooted_lst7 = list (&
         1 + 2 + 3 + 4 + 5, &
         2 + 3 + 4 + 5 + 6, &
         3 + 4 + 5 + 6 + 7, &
         4 + 5 + 6 + 7 + 8, &
         5 + 6 + 7 + 8 + 9)
    call check (list_equal (int_eq, lst6, rooted_lst7), "test0310-0070 failed")
    lst1 = iota (5, 1)
    lst2 = iota (5, 2)
    lst3 = iota (5, 3)
    lst4 = iota (5, 4)
    lst5 = iota (5, 5)
    lst6 = map_in_order (kadd5, lst1, lst2, lst3, lst4, lst5)
    call check (list_equal (int_eq, lst6, rooted_lst7), "test0310-0080 failed")

    ! Try them with nil lists.
    lst1 = iota (5, 1)
    lst2 = iota (5, 2)
    lst3 = nil
    lst4 = iota (5, 4)
    lst5 = iota (5, 5)
    lst6 = map (kadd5, lst1, lst2, lst3, lst4, lst5)
    call check (list_equal (int_eq, lst6, nil), "test0310-0090 failed")
    lst1 = iota (5, 1)
    lst2 = iota (5, 2)
    lst3 = iota (5, 3)
    lst4 = nil
    lst5 = iota (5, 5)
    lst6 = map_in_order (kadd5, lst1, lst2, lst3, lst4, lst5)
    call check (list_equal (int_eq, lst6, nil), "test0310-0100 failed")

    ! With side effects. (An example from SRFI-1.)
    count = 0
    lst1 = str_t ('a') ** str_t ('b') ** nil
    lst2 = map (kincrcount, lst1)
    call check (list_equal (int_eq, lst2, list (1, 2)) .or. list_equal (int_eq, lst2, list (2, 1)), &
         "test0310-0110 failed")
    count = 0
    lst1 = str_t ('a') ** str_t ('b') ** nil
    lst2 = map_in_order (kincrcount, lst1)
    call check (list_equal (int_eq, lst2, list (1, 2)), "test0310-0120 failed")

  contains

    recursive subroutine kcadr (lst, elem)
      class(*), intent(in) :: lst
      class(*), allocatable, intent(out) :: elem
      call collect_garbage_now
      elem = cadr (lst)
    end subroutine kcadr

    recursive subroutine kselfpower (x, y)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: y
      call collect_garbage_now
      y = int_cast (x) ** int_cast (x)
    end subroutine kselfpower

    recursive subroutine kadd (x, y, sum)
      class(*), intent(in) :: x, y
      class(*), allocatable, intent(out) :: sum
      call collect_garbage_now
      sum = int_cast (x) + int_cast (y)
    end subroutine kadd

    recursive subroutine kadd5 (x, y, z, u, v, sum)
      class(*), intent(in) :: x, y, z, u, v
      class(*), allocatable, intent(out) :: sum
      call collect_garbage_now
      sum = int_cast (x) + int_cast (y) + int_cast (z) + int_cast (u) + int_cast (v)
    end subroutine kadd5

    recursive subroutine kincrcount (ignored, count_val)
      class(*), intent(in) :: ignored
      class(*), allocatable, intent(out) :: count_val
      call collect_garbage_now
      count = count + 1
      count_val = count
    end subroutine kincrcount

  end subroutine test0310

  subroutine test0320
    !
    ! Tests of for_each.
    !

    integer :: count
    type(gcroot_t) :: lst1, lst2, lst3

    count = 0
    call for_each (add_to_count, iota (10, 1))
    call check (count == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10, "test0320-0010 failed")

    lst1 = nil
    lst2 = nil
    lst3 = nil
    call for_each (cons_them, list (1, 2, 3), list (4, 5, 6), list (7, 8, 9))
    call check (list_equal (int_eq, lst1, list (3, 2, 1)), "test0320-0020 failed")
    call check (list_equal (int_eq, lst2, list (6, 5, 4)), "test0320-0030 failed")
    call check (list_equal (int_eq, lst3, list (9, 8, 7)), "test0320-0030 failed")

  contains

    recursive subroutine add_to_count (n)
      class(*), intent(in) :: n
      call collect_garbage_now
      count = count + int_cast (n)
    end subroutine add_to_count

    recursive subroutine cons_them (obj1, obj2, obj3)
      class(*), intent(in) :: obj1
      class(*), intent(in) :: obj2
      class(*), intent(in) :: obj3
      call collect_garbage_now
      lst1 = cons (obj1, lst1)
      lst2 = cons (obj2, lst2)
      lst3 = cons (obj3, lst3)
    end subroutine cons_them

  end subroutine test0320

  subroutine test0330
    !
    ! Tests of pair_for_each.
    !

    type(gcroot_t) :: lst1, lst2, lst3

    lst1 = nil
    lst2 = nil
    lst3 = nil
    call pair_for_each (cons_them, list (1, 2, 3), list (4, 5, 6), list (7, 8, 9))
    call check (length (lst1) == 3, "test0330-0010 failed")
    call check (length (lst2) == 3, "test0330-0020 failed")
    call check (length (lst3) == 3, "test0330-0030 failed")
    call check (list_equal (int_eq, first (lst1), list (3)), "test0330-0040 failed")
    call check (list_equal (int_eq, second (lst1), list (2, 3)), "test0330-0050 failed")
    call check (list_equal (int_eq, third (lst1), list (1, 2, 3)), "test0330-0060 failed")
    call check (list_equal (int_eq, first (lst2), list (6)), "test0330-0070 failed")
    call check (list_equal (int_eq, second (lst2), list (5, 6)), "test0330-0080 failed")
    call check (list_equal (int_eq, third (lst2), list (4, 5, 6)), "test0330-0090 failed")
    call check (list_equal (int_eq, first (lst3), list (9)), "test0330-0100 failed")
    call check (list_equal (int_eq, second (lst3), list (8, 9)), "test0330-0110 failed")
    call check (list_equal (int_eq, third (lst3), list (7, 8, 9)), "test0330-0120 failed")

  contains

    recursive subroutine cons_them (obj1, obj2, obj3)
      class(*), intent(in) :: obj1
      class(*), intent(in) :: obj2
      class(*), intent(in) :: obj3
      call collect_garbage_now
      lst1 = cons (obj1, lst1)
      lst2 = cons (obj2, lst2)
      lst3 = cons (obj3, lst3)
    end subroutine cons_them

  end subroutine test0330

  subroutine test0340
    !
    ! Tests of filter_map.
    !

    type(cons_t) :: lst1, lst2, lst3
    type(cons_t) :: lst4, lst5

    lst1 = list (1, 2, 3, 4, 5)
    lst2 = list (1, 2, 3, 4, 5)
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list (3, 6, 9, 12, 15)
    call check (list_equal (int_eq, lst4, lst5), "test0340-0010 failed")

    lst1 = list (1, 2, 3, 4, 5)
    lst2 = list (1.0, 2, 3.0, 4, 5)
    lst3 = list (1, 2, 3, 4, 5.0)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list (6, 12)
    call check (list_equal (int_eq, lst4, lst5), "test0340-0020 failed")

    lst1 = list (1, 2, 3, 4, 5)
    lst2 = list (1.0, 2, 3.0, 4, 5)
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list (6, 12, 15)
    call check (list_equal (int_eq, lst4, lst5), "test0340-0025 failed")

    lst1 = list (1, 2)
    lst2 = list (1, 2, 3, 4, 5)
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list (3, 6)
    call check (list_equal (int_eq, lst4, lst5), "test0340-0030 failed")

    lst1 = list (1, 2)
    lst2 = list (1.0, 2, 3, 4, 5)
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list (6)
    call check (list_equal (int_eq, lst4, lst5), "test0340-0040 failed")

    lst1 = list (1, 2.0)
    lst2 = list (1, 2, 3, 4, 5)
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list (3)
    call check (list_equal (int_eq, lst4, lst5), "test0340-0050 failed")

    lst1 = list (1, 2)
    lst2 = list (1)
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list (3)
    call check (list_equal (int_eq, lst4, lst5), "test0340-0060 failed")

    lst1 = list (1.0, 2)
    lst2 = list (1)
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list ()
    call check (list_equal (int_eq, lst4, lst5), "test0340-0060 failed")

    lst1 = list (1, 2)
    lst2 = list ()
    lst3 = list (1, 2, 3, 4, 5)
    lst4 = filter_map (proc1, lst1, lst2, lst3)
    lst5 = list ()
    call check (list_equal (int_eq, lst4, lst5), "test0340-0070 failed")

  contains

    recursive subroutine proc1 (obj1, obj2, obj3, retval)
      !
      ! If all the obj are integers, return their sum. Otherwise
      ! return .false.
      !
      class(*), intent(in) :: obj1
      class(*), intent(in) :: obj2
      class(*), intent(in) :: obj3
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      retval = .false.
      select type (obj1)
      type is (integer)
         select type (obj2)
         type is (integer)
            select type (obj3)
            type is (integer)
               retval = obj1 + obj2 + obj3
            end select
         end select
      end select
    end subroutine proc1

  end subroutine test0340

  subroutine test0350
    !
    ! Tests of filterx.
    !

    type(cons_t) :: lst1, lst2
    class(*), allocatable :: result1

    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    result1 = filterx (is_int, lst1)
    lst2 = list (1, 3, 6, 7, 8)
    call check (list_equal (int_eq, result1, lst2), "test0350-0010 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    result1 = filterx (is_int, lst1)
    lst2 = list (3, 6, 7, 8)
    call check (list_equal (int_eq, result1, lst2), "test0350-0020 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8.0)
    result1 = filterx (is_int, lst1)
    lst2 = list (3, 6, 7)
    call check (list_equal (int_eq, result1, lst2), "test0350-0030 failed")

    lst1 = list ()
    result1 = filterx (is_int, lst1)
    lst2 = list ()
    call check (list_equal (int_eq, result1, lst2), "test0350-0040 failed")

    lst1 = list (123)
    result1 = filterx (is_int, lst1)
    lst2 = list (123)
    call check (list_equal (int_eq, result1, lst2), "test0350-0050 failed")

    lst1 = list (123.0)
    result1 = filterx (is_int, lst1)
    lst2 = list ()
    call check (list_equal (int_eq, result1, lst2), "test0350-0060 failed")

    lst1 = iota (100, 1)
    result1 = filterx (is_int, lst1)
    lst2 = iota (100, 1)
    call check (list_equal (int_eq, result1, lst2), "test0350-0070 failed")

    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    result1 = filterx (is_int, lst1)
    lst2 = list ()
    call check (list_equal (int_eq, result1, lst2), "test0350-0080 failed")

  contains

    recursive function is_int (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (integer)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_int

  end subroutine test0350

  subroutine test0355
    !
    ! Tests of removex.
    !

    type(cons_t) :: lst1, lst2
    class(*), allocatable :: result1

    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    result1 = removex (is_real, lst1)
    lst2 = list (1, 3, 6, 7, 8)
    call check (list_equal (int_eq, result1, lst2), "test0355-0010 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    result1 = removex (is_real, lst1)
    lst2 = list (3, 6, 7, 8)
    call check (list_equal (int_eq, result1, lst2), "test0355-0020 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8.0)
    result1 = removex (is_real, lst1)
    lst2 = list (3, 6, 7)
    call check (list_equal (int_eq, result1, lst2), "test0355-0030 failed")

    lst1 = list ()
    result1 = removex (is_real, lst1)
    lst2 = list ()
    call check (list_equal (int_eq, result1, lst2), "test0355-0040 failed")

    lst1 = list (123)
    result1 = removex (is_real, lst1)
    lst2 = list (123)
    call check (list_equal (int_eq, result1, lst2), "test0355-0050 failed")

    lst1 = list (123.0)
    result1 = removex (is_real, lst1)
    lst2 = list ()
    call check (list_equal (int_eq, result1, lst2), "test0355-0060 failed")

    lst1 = iota (100, 1)
    result1 = removex (is_real, lst1)
    lst2 = iota (100, 1)
    call check (list_equal (int_eq, result1, lst2), "test0355-0070 failed")

    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    result1 = removex (is_real, lst1)
    lst2 = list ()
    call check (list_equal (int_eq, result1, lst2), "test0355-0080 failed")

  contains

    recursive function is_real (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (real)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_real

  end subroutine test0355

  subroutine test0360
    !
    ! Tests of removex.
    !

    type(cons_t) :: lst1, lst2
    class(*), allocatable :: result1

    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    result1 = removex (is_int, lst1)
    lst2 = list (2.0, 4.0, 5.0)
    call check (list_equal (real_eq, result1, lst2), "test0360-0010 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    result1 = removex (is_int, lst1)
    lst2 = list (1.0, 2.0, 4.0, 5.0)
    call check (list_equal (real_eq, result1, lst2), "test0360-0020 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8.0)
    result1 = removex (is_int, lst1)
    lst2 = list (1.0, 2.0, 4.0, 5.0, 8.0)
    call check (list_equal (real_eq, result1, lst2), "test0360-0030 failed")

    lst1 = list ()
    result1 = removex (is_int, lst1)
    lst2 = list ()
    call check (list_equal (real_eq, result1, lst2), "test0360-0040 failed")

    lst1 = list (123)
    result1 = removex (is_int, lst1)
    lst2 = list ()
    call check (list_equal (real_eq, result1, lst2), "test0360-0050 failed")

    lst1 = list (123.0)
    result1 = removex (is_int, lst1)
    lst2 = list (123.0)
    call check (list_equal (real_eq, result1, lst2), "test0360-0060 failed")

    lst1 = iota (100, 1)
    result1 = removex (is_int, lst1)
    lst2 = list ()
    call check (list_equal (real_eq, result1, lst2), "test0360-0070 failed")

    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    result1 = removex (is_int, lst1)
    lst2 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    call check (list_equal (real_eq, result1, lst2), "test0360-0080 failed")

  contains

    recursive function is_int (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (integer)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_int

  end subroutine test0360

  subroutine test0370
    !
    ! Tests of filter.
    !

    type(cons_t) :: lst1, lst2
    type(gcroot_t) :: lst1a
    class(*), allocatable :: result1

    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = list (1, 3, 6, 7, 8)
    call check (list_equal (num_same, lst1, lst1a), "test0370-0005 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0010 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = list (3, 6, 7, 8)
    call check (list_equal (num_same, lst1, lst1a), "test0370-0015 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0020 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8.0)
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = list (3, 6, 7)
    call check (list_equal (num_same, lst1, lst1a), "test0370-0025 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0030 failed")

    lst1 = list ()
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0370-0035 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0040 failed")

    lst1 = list (123)
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = list (123)
    call check (list_equal (num_same, lst1, lst1a), "test0370-0045 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0050 failed")

    lst1 = list (123.0)
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0370-0055 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0060 failed")

    lst1 = iota (100, 1)
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = iota (100, 1)
    call check (list_equal (num_same, lst1, lst1a), "test0370-0065 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0070 failed")

    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    lst1a = list_copy (lst1)
    result1 = filter (is_int, lst1)
    lst2 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0370-0075 failed")
    call check (list_equal (int_eq, result1, lst2), "test0370-0080 failed")

  contains

    recursive function is_int (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (integer)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_int

  end subroutine test0370

  subroutine test0380
    !
    ! Tests of remove.
    !

    type(cons_t) :: lst1, lst2
    type(gcroot_t) :: lst1a
    class(*), allocatable :: result1

    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = list (1, 3, 6, 7, 8)
    call check (list_equal (num_same, lst1, lst1a), "test0380-0005 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0010 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = list (3, 6, 7, 8)
    call check (list_equal (num_same, lst1, lst1a), "test0380-0015 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0020 failed")

    lst1 = list (1.0, 2.0, 3, 4.0, 5.0, 6, 7, 8.0)
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = list (3, 6, 7)
    call check (list_equal (num_same, lst1, lst1a), "test0380-0025 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0030 failed")

    lst1 = list ()
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0380-0035 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0040 failed")

    lst1 = list (123)
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = list (123)
    call check (list_equal (num_same, lst1, lst1a), "test0380-0045 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0050 failed")

    lst1 = list (123.0)
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0380-0055 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0060 failed")

    lst1 = iota (100, 1)
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = iota (100, 1)
    call check (list_equal (num_same, lst1, lst1a), "test0380-0065 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0070 failed")

    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    lst1a = list_copy (lst1)
    result1 = remove (is_real, lst1)
    lst2 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0380-0075 failed")
    call check (list_equal (int_eq, result1, lst2), "test0380-0080 failed")

  contains

    recursive function is_real (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (real)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_real

  end subroutine test0380

  subroutine test0390
    !
    ! Tests of do_partitionx.
    !

    type(cons_t) :: lst1, lst2, lst3
    class(*), allocatable :: lst_f, lst_r

    ! The list is nil.
    lst1 = list ()
    call do_partitionx (is_int, lst1, lst_f, lst_r)
    lst2 = list ()
    lst3 = list ()
    call check (list_equal (int_eq, lst_f, lst2), "test0390-0010 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0390-0020 failed")

    ! The list is degenerate.
    call do_partitionx (is_int, str_t ('abc'), lst_f, lst_r)
    call check (length (lst_f) == 0, "test0390-0030 failed")
    call check (length (lst_r) == 0, "test0390-0040 failed")
    ! One or the other of the output lists gets the degenerate
    ! `tail'. Which list gets it is unspecified.
    call check ((is_nil (lst_f) .and. is_not_nil (lst_r)) .or. (is_not_nil (lst_f) .and. is_nil (lst_r)), &
         "test0390-0045 failed")

    ! The entire list is a run of integers.
    lst1 = list (1, 2, 3, 4, 5, 6, 7, 8)
    call do_partitionx (is_int, lst1, lst_f, lst_r)
    lst2 = list (1, 2, 3, 4, 5, 6, 7, 8)
    lst3 = list ()
    call check (list_equal (int_eq, lst_f, lst2), "test0390-0050 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0390-0060 failed")

    ! The entire list is a run of reals.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    call do_partitionx (is_int, lst1, lst_f, lst_r)
    lst2 = list ()
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    call check (list_equal (int_eq, lst_f, lst2), "test0390-0070 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0390-0080 failed")

    ! All reals, followed by all integers.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6, 7, 8)
    call do_partitionx (is_int, lst1, lst_f, lst_r)
    lst2 = list (6, 7, 8)
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    call check (list_equal (int_eq, lst_f, lst2), "test0390-0090 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0390-0100 failed")

    ! All integers, followed by all reals
    lst1 = list (1, 2, 3, 4, 5, 6.0, 7.0, 8.0)
    call do_partitionx (is_int, lst1, lst_f, lst_r)
    lst2 = list (1, 2, 3, 4, 5)
    lst3 = list (6.0, 7.0, 8.0)
    call check (list_equal (int_eq, lst_f, lst2), "test0390-0110 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0390-0120 failed")

    ! A mix of reals and integers
    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    call do_partitionx (is_int, lst1, lst_f, lst_r)
    lst2 = list (1, 3, 6, 7, 8)
    lst3 = list (2.0, 4.0, 5.0)
    call check (list_equal (int_eq, lst_f, lst2), "test0390-0130 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0390-0140 failed")

    ! A mix of reals and integers
    lst1 = list (1.0, 2, 3.0, 4, 5, 6.0, 7.0, 8.0)
    call do_partitionx (is_int, lst1, lst_f, lst_r)
    lst2 = list (2, 4, 5)
    lst3 = list (1.0, 3.0, 6.0, 7.0, 8.0)
    call check (list_equal (int_eq, lst_f, lst2), "test0390-0150 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0390-0160 failed")

  contains

    recursive function is_int (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (integer)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_int

  end subroutine test0390

  subroutine test0395
    !
    ! Tests of partitionx.
    !

    type(cons_t) :: lst1, lst2, lst3
    type(gcroot_t) :: retval

    ! The list is nil.
    lst1 = list ()
    retval = partitionx (is_int, lst1)
    lst2 = list ()
    lst3 = list ()
    call check (list_equal (int_eq, first (retval), lst2), "test0395-0010 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0395-0020 failed")

    ! The list is degenerate.
    retval = partitionx (is_int, str_t ('abc'))
    call check (length (first (retval)) == 0, "test0395-0030 failed")
    call check (length (second (retval)) == 0, "test0395-0040 failed")
    ! One or the other of the output lists gets the degenerate
    ! `tail'. Which list gets it is unspecified.
    call check ((is_nil (first (retval)) .and. is_not_nil (second (retval))) &
         &        .or. (is_not_nil (first (retval)) .and. is_nil (second (retval))), &
         "test0395-0045 failed")

    ! The entire list is a run of integers.
    lst1 = list (1, 2, 3, 4, 5, 6, 7, 8)
    retval = partitionx (is_int, lst1)
    lst2 = list (1, 2, 3, 4, 5, 6, 7, 8)
    lst3 = list ()
    call check (list_equal (int_eq, first (retval), lst2), "test0395-0050 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0395-0060 failed")

    ! The entire list is a run of reals.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    retval = partitionx (is_int, lst1)
    lst2 = list ()
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    call check (list_equal (int_eq, first (retval), lst2), "test0395-0070 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0395-0080 failed")

    ! All reals, followed by all integers.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6, 7, 8)
    retval = partitionx (is_int, lst1)
    lst2 = list (6, 7, 8)
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    call check (list_equal (int_eq, first (retval), lst2), "test0395-0090 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0395-0100 failed")

    ! All integers, followed by all reals
    lst1 = list (1, 2, 3, 4, 5, 6.0, 7.0, 8.0)
    retval = partitionx (is_int, lst1)
    lst2 = list (1, 2, 3, 4, 5)
    lst3 = list (6.0, 7.0, 8.0)
    call check (list_equal (int_eq, first (retval), lst2), "test0395-0110 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0395-0120 failed")

    ! A mix of reals and integers
    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    retval = partitionx (is_int, lst1)
    lst2 = list (1, 3, 6, 7, 8)
    lst3 = list (2.0, 4.0, 5.0)
    call check (list_equal (int_eq, first (retval), lst2), "test0395-0130 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0395-0140 failed")

    ! A mix of reals and integers
    lst1 = list (1.0, 2, 3.0, 4, 5, 6.0, 7.0, 8.0)
    retval = partitionx (is_int, lst1)
    lst2 = list (2, 4, 5)
    lst3 = list (1.0, 3.0, 6.0, 7.0, 8.0)
    call check (list_equal (int_eq, first (retval), lst2), "test0395-0150 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0395-0160 failed")

  contains

    recursive function is_int (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (integer)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_int

  end subroutine test0395

  subroutine test0400
    !
    ! Tests of do_partition.
    !

    type(cons_t) :: lst1, lst2, lst3
    type(gcroot_t) :: lst1a
    class(*), allocatable :: lst_f, lst_r

    ! The list is nil.
    lst1 = list ()
    lst1a = list_copy (lst1)
    call do_partition (is_int, lst1, lst_f, lst_r)
    lst2 = list ()
    lst3 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0400-0005 failed")
    call check (list_equal (int_eq, lst_f, lst2), "test0400-0010 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0400-0020 failed")

    ! The list is degenerate.
    call do_partition (is_int, str_t ('abc'), lst_f, lst_r)
    call check (length (lst_f) == 0, "test0400-0030 failed")
    call check (length (lst_r) == 0, "test0400-0040 failed")
    ! One or the other of the output lists gets the degenerate
    ! `tail'. Which list gets it is unspecified.
    call check ((is_nil (lst_f) .and. is_not_nil (lst_r)) .or. (is_not_nil (lst_f) .and. is_nil (lst_r)), &
         "test0400-0045 failed")

    ! The entire list is a run of integers.
    lst1 = list (1, 2, 3, 4, 5, 6, 7, 8)
    lst1a = list_copy (lst1)
    call do_partition (is_int, lst1, lst_f, lst_r)
    lst2 = list (1, 2, 3, 4, 5, 6, 7, 8)
    lst3 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0400-0045 failed")
    call check (list_equal (int_eq, lst_f, lst2), "test0400-0050 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0400-0060 failed")

    ! The entire list is a run of reals.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    lst1a = list_copy (lst1)
    call do_partition (is_int, lst1, lst_f, lst_r)
    lst2 = list ()
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    call check (list_equal (num_same, lst1, lst1a), "test0400-0065 failed")
    call check (list_equal (int_eq, lst_f, lst2), "test0400-0070 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0400-0080 failed")

    ! All reals, followed by all integers.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    call do_partition (is_int, lst1, lst_f, lst_r)
    lst2 = list (6, 7, 8)
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    call check (list_equal (num_same, lst1, lst1a), "test0400-0085 failed")
    call check (list_equal (int_eq, lst_f, lst2), "test0400-0090 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0400-0100 failed")

    ! All integers, followed by all reals
    lst1 = list (1, 2, 3, 4, 5, 6.0, 7.0, 8.0)
    lst1a = list_copy (lst1)
    call do_partition (is_int, lst1, lst_f, lst_r)
    lst2 = list (1, 2, 3, 4, 5)
    lst3 = list (6.0, 7.0, 8.0)
    call check (list_equal (num_same, lst1, lst1a), "test0400-0105 failed")
    call check (list_equal (int_eq, lst_f, lst2), "test0400-0110 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0400-0120 failed")

    ! A mix of reals and integers
    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    call do_partition (is_int, lst1, lst_f, lst_r)
    lst2 = list (1, 3, 6, 7, 8)
    lst3 = list (2.0, 4.0, 5.0)
    call check (list_equal (num_same, lst1, lst1a), "test0400-0125 failed")
    call check (list_equal (int_eq, lst_f, lst2), "test0400-0130 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0400-0140 failed")

    ! A mix of reals and integers
    lst1 = list (1.0, 2, 3.0, 4, 5, 6.0, 7.0, 8.0)
    lst1a = list_copy (lst1)
    call do_partition (is_int, lst1, lst_f, lst_r)
    lst2 = list (2, 4, 5)
    lst3 = list (1.0, 3.0, 6.0, 7.0, 8.0)
    call check (list_equal (num_same, lst1, lst1a), "test0400-0145 failed")
    call check (list_equal (int_eq, lst_f, lst2), "test0400-0150 failed")
    call check (list_equal (real_eq, lst_r, lst3), "test0400-0160 failed")

  contains

    recursive function is_int (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (integer)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_int

  end subroutine test0400

  subroutine test0405
    !
    ! Tests of partition.
    !

    type(cons_t) :: lst1, lst2, lst3
    type(gcroot_t) :: lst1a
    type(gcroot_t) :: retval

    ! The list is nil.
    lst1 = list ()
    lst1a = list_copy (lst1)
    retval = partition (is_int, lst1)
    lst2 = list ()
    lst3 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0405-0005 failed")
    call check (list_equal (int_eq, first (retval), lst2), "test0405-0010 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0405-0020 failed")

    ! The list is degenerate.
    retval = partition (is_int, str_t ('abc'))
    call check (length (first (retval)) == 0, "test0405-0030 failed")
    call check (length (second (retval)) == 0, "test0405-0040 failed")
    ! One or the other of the output lists gets the degenerate
    ! `tail'. Which list gets it is unspecified.
    call check ((is_nil (first (retval)) .and. is_not_nil (second (retval))) &
         &        .or. (is_not_nil (first (retval)) .and. is_nil (second (retval))), &
         "test0405-0045 failed")

    ! The entire list is a run of integers.
    lst1 = list (1, 2, 3, 4, 5, 6, 7, 8)
    lst1a = list_copy (lst1)
    retval = partition (is_int, lst1)
    lst2 = list (1, 2, 3, 4, 5, 6, 7, 8)
    lst3 = list ()
    call check (list_equal (num_same, lst1, lst1a), "test0405-0045 failed")
    call check (list_equal (int_eq, first (retval), lst2), "test0405-0050 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0405-0060 failed")

    ! The entire list is a run of reals.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    lst1a = list_copy (lst1)
    retval = partition (is_int, lst1)
    lst2 = list ()
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    call check (list_equal (num_same, lst1, lst1a), "test0405-0065 failed")
    call check (list_equal (int_eq, first (retval), lst2), "test0405-0070 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0405-0080 failed")

    ! All reals, followed by all integers.
    lst1 = list (1.0, 2.0, 3.0, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    retval = partition (is_int, lst1)
    lst2 = list (6, 7, 8)
    lst3 = list (1.0, 2.0, 3.0, 4.0, 5.0)
    call check (list_equal (num_same, lst1, lst1a), "test0405-0085 failed")
    call check (list_equal (int_eq, first (retval), lst2), "test0405-0090 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0405-0100 failed")

    ! All integers, followed by all reals
    lst1 = list (1, 2, 3, 4, 5, 6.0, 7.0, 8.0)
    lst1a = list_copy (lst1)
    retval = partition (is_int, lst1)
    lst2 = list (1, 2, 3, 4, 5)
    lst3 = list (6.0, 7.0, 8.0)
    call check (list_equal (num_same, lst1, lst1a), "test0405-0105 failed")
    call check (list_equal (int_eq, first (retval), lst2), "test0405-0110 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0405-0120 failed")

    ! A mix of reals and integers
    lst1 = list (1, 2.0, 3, 4.0, 5.0, 6, 7, 8)
    lst1a = list_copy (lst1)
    retval = partition (is_int, lst1)
    lst2 = list (1, 3, 6, 7, 8)
    lst3 = list (2.0, 4.0, 5.0)
    call check (list_equal (num_same, lst1, lst1a), "test0405-0125 failed")
    call check (list_equal (int_eq, first (retval), lst2), "test0405-0130 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0405-0140 failed")

    ! A mix of reals and integers
    lst1 = list (1.0, 2, 3.0, 4, 5, 6.0, 7.0, 8.0)
    lst1a = list_copy (lst1)
    retval = partition (is_int, lst1)
    lst2 = list (2, 4, 5)
    lst3 = list (1.0, 3.0, 6.0, 7.0, 8.0)
    call check (list_equal (num_same, lst1, lst1a), "test0405-0145 failed")
    call check (list_equal (int_eq, first (retval), lst2), "test0405-0150 failed")
    call check (list_equal (real_eq, second (retval), lst3), "test0405-0160 failed")

  contains

    recursive function is_int (obj) result (bool)
      class(*), intent(in) :: obj
      logical :: bool

      call collect_garbage_now

      select type (obj)
      type is (integer)
         bool = .true.
      class default
         bool = .false.
      end select
    end function is_int

  end subroutine test0405

  subroutine test0410
    call check (list_equal (int_eq, member (int_eq, 1, list (1, 2, 3)), list (1, 2, 3)), "test0410-0010 failed")
    call check (list_equal (int_eq, member (int_eq, 2, list (1, 2, 3)), list (2, 3)), "test0410-0020 failed")
    call check (list_equal (int_eq, member (int_eq, 3, list (1, 2, 3)), list (3)), "test0410-0030 failed")
    call check (list_equal (int_eq, member (int_eq, 4, list (1, 2, 3)), list ()), "test0410-0040 failed")
    call check (list_equal (int_eq, member (int_eq, 1, list ()), list ()), "test0410-0050 failed")
    call check (list_equal (int_eq, member (int_eq, 1, 1), list ()), "test0410-0060 failed") ! degenerate dotted list.
  end subroutine test0410

  subroutine test0420
    call check (list_equal (int_eq, deletex (int_eq, 1, list (1, 2, 3)), list (2, 3)), "test0420-0010 failed")
    call check (list_equal (int_eq, deletex (int_eq, 2, list (1, 2, 3)), list (1, 3)), "test0420-0020 failed")
    call check (list_equal (int_eq, deletex (int_eq, 3, list (1, 2, 3)), list (1, 2)), "test0420-0030 failed")
    call check (list_equal (int_eq, deletex (int_eq, 1, list (1, 1, 1, 1, 1, 2, 3)), list (2, 3)), "test0420-0040 failed")
    call check (list_equal (int_eq, deletex (int_eq, 3, list (1, 2, 3, 3, 3, 3, 3)), list (1, 2)), "test0420-0050 failed")
    call check (list_equal (int_eq, deletex (int_eq, 1, list (1, 2, 1, 2, 1, 2, 1)), list (2, 2, 2)), "test0420-0060 failed")
    call check (list_equal (int_eq, deletex (int_eq, 1, list (1, 2, 1, 3, 1, 4, 1)), list (2, 3, 4)), "test0420-0070 failed")
    call check (list_equal (int_eq, deletex (int_eq, 5, list (1, 2, 1, 3, 1, 4, 1)), list (1, 2, 1, 3, 1, 4, 1)), &
         "test0420-0080 failed")
    call check (list_equal (int_eq, deletex (int_eq, 1, list ()), list ()), "test0420-0090 failed")
    call check (list_equal (int_eq, deletex (int_eq, 1, list (1)), list ()), "test0420-0100 failed")
    call check (list_equal (int_eq, deletex (int_eq, 2, list (1)), list (1)), "test0420-0110 failed")
    call check (deletex (int_eq, 1, 1) .eqi. 1, "test0420-0120 failed")
  end subroutine test0420

  subroutine test0430
    !
    ! FIXME: Add tests that the inputs are not destroyed.
    !
    call check (list_equal (int_eq, delete (int_eq, 1, list (1, 2, 3)), list (2, 3)), "test0430-0010 failed")
    call check (list_equal (int_eq, delete (int_eq, 2, list (1, 2, 3)), list (1, 3)), "test0430-0020 failed")
    call check (list_equal (int_eq, delete (int_eq, 3, list (1, 2, 3)), list (1, 2)), "test0430-0030 failed")
    call check (list_equal (int_eq, delete (int_eq, 1, list (1, 1, 1, 1, 1, 2, 3)), list (2, 3)), "test0430-0040 failed")
    call check (list_equal (int_eq, delete (int_eq, 3, list (1, 2, 3, 3, 3, 3, 3)), list (1, 2)), "test0430-0050 failed")
    call check (list_equal (int_eq, delete (int_eq, 1, list (1, 2, 1, 2, 1, 2, 1)), list (2, 2, 2)), "test0430-0060 failed")
    call check (list_equal (int_eq, delete (int_eq, 1, list (1, 2, 1, 3, 1, 4, 1)), list (2, 3, 4)), "test0430-0070 failed")
    call check (list_equal (int_eq, delete (int_eq, 5, list (1, 2, 1, 3, 1, 4, 1)), list (1, 2, 1, 3, 1, 4, 1)), &
         "test0430-0080 failed")
    call check (list_equal (int_eq, delete (int_eq, 1, list ()), list ()), "test0430-0090 failed")
    call check (list_equal (int_eq, delete (int_eq, 1, list (1)), list ()), "test0430-0100 failed")
    call check (list_equal (int_eq, delete (int_eq, 2, list (1)), list (1)), "test0430-0110 failed")
    call check (delete (int_eq, 1, 1) .eqi. 1, "test0430-0120 failed")
  end subroutine test0430

  subroutine test0440
    !
    ! FIXME: Test that the shared-tail mechanism works.
    !
    call check (list_equal (int_eq, delete_duplicatesx (int_eq, list (1, 2, 1, 3, 1, 4, 1)), list (1, 2, 3, 4)), &
         "test0440-0010 failed")
    call check (list_equal (int_eq, delete_duplicatesx (int_eq, list (1, 2, 1, 2, 1, 2, 1)), list (1, 2)), &
         "test0440-0020 failed")
    call check (list_equal (int_eq, delete_duplicatesx (int_eq, list ()), list ()), "test0440-0030 failed")
    call check (list_equal (int_eq, delete_duplicatesx (int_eq, list (1)), list (1)), "test0440-0040 failed")
    call check (list_equal (int_eq, delete_duplicatesx (int_eq, list (1, 1)), list (1)), "test0440-0050 failed")
    call check (list_equal (int_eq, delete_duplicatesx (int_eq, make_list (500, 1)), list (1)), "test0440-0060 failed")
  end subroutine test0440

  subroutine test0450
    !
    ! FIXME: Add tests that the inputs are not destroyed.
    !
    ! FIXME: Test that the shared-tail mechanism works.
    !
    call check (list_equal (int_eq, delete_duplicates (int_eq, list (1, 2, 1, 3, 1, 4, 1)), list (1, 2, 3, 4)), &
         "test0450-0010 failed")
    call check (list_equal (int_eq, delete_duplicates (int_eq, list (1, 2, 1, 2, 1, 2, 1)), list (1, 2)), &
         "test0450-0020 failed")
    call check (list_equal (int_eq, delete_duplicates (int_eq, list ()), list ()), "test0450-0030 failed")
    call check (list_equal (int_eq, delete_duplicates (int_eq, list (1)), list (1)), "test0450-0040 failed")
    call check (list_equal (int_eq, delete_duplicates (int_eq, list (1, 1)), list (1)), "test0450-0050 failed")
    call check (list_equal (int_eq, delete_duplicates (int_eq, make_list (500, 1)), list (1)), "test0450-0060 failed")
  end subroutine test0450

  subroutine test0460
    !
    ! Test is_false and is_not_false.
    !
    type(gcroot_t) :: obj

    obj = .false.
    call check (is_false (obj), "test0460-0010 failed")
    call check (is_false (.val. obj), "test0460-0020 failed")
    call check (.not. is_not_false (obj), "test0460-0030 failed")
    call check (.not. is_not_false (.val. obj), "test0460-0040 failed")

    obj = .true.
    call check (.not. is_false (obj), "test0460-0050 failed")
    call check (.not. is_false (.val. obj), "test0460-0060 failed")
    call check (is_not_false (obj), "test0460-0070 failed")
    call check (is_not_false (.val. obj), "test0460-0080 failed")

    obj = 123
    call check (.not. is_false (obj), "test0460-0090 failed")
    call check (.not. is_false (.val. obj), "test0460-0100 failed")
    call check (is_not_false (obj), "test0460-0110 failed")
    call check (is_not_false (.val. obj), "test0460-0120 failed")
  end subroutine test0460

  subroutine test0470
    type(cons_t) :: lst

    lst = list (1, 3, 5, 7, 8, 9, 11, 13, 14)
    call check (find (is_even, lst) .eqi. 8, "test0470-0010 failed")
    call check (list_equal (int_eq, find_tail (is_even, lst), list (8, 9, 11, 13, 14)), "test0470-0020 failed")

    lst = list ()
    call check (is_false (find (is_even, lst)), "test0470-0030 failed")
    call check (list_equal (int_eq, find_tail (is_even, lst), list ()), "test0470-0040 failed")

    lst = list (1, 3, 5, 7, 9, 11, 13)
    call check (is_false (find (is_even, lst)), "test0470-0050 failed")
    call check (list_equal (int_eq, find_tail (is_even, lst), list ()), "test0470-0060 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool
      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

  end subroutine test0470

  subroutine test0480
    type(cons_t) :: lst

    lst = list (1, 3, 5, 7, 8, 9, 11, 13, 14)
    call check (list_equal (int_eq, drop_while (is_odd, lst), list (8, 9, 11, 13, 14)), "test0480-0020 failed")

    lst = list ()
    call check (list_equal (int_eq, drop_while (is_odd, lst), list ()), "test0480-0040 failed")

    lst = list (1, 3, 5, 7, 9, 11, 13)
    call check (list_equal (int_eq, drop_while (is_odd, lst), list ()), "test0480-0060 failed")

    lst = drop_while (is_odd, circular_list (1, 3, 5, 8))
    call check (first (lst) .eqi. 8,  "test0480-0070 failed")
    call check (second (lst) .eqi. 1,  "test0480-0080 failed")
    call check (third (lst) .eqi. 3,  "test0480-0090 failed")
    call check (fourth (lst) .eqi. 5,  "test0480-0100 failed")
    call check (fifth (lst) .eqi. 8,  "test0480-0110 failed")
    call check (sixth (lst) .eqi. 1,  "test0480-0120 failed")
    call check (seventh (lst) .eqi. 3,  "test0480-0130 failed")
    call check (eighth (lst) .eqi. 5,  "test0480-0140 failed")
    call check (ninth (lst) .eqi. 8,  "test0480-0150 failed")
    call check (tenth (lst) .eqi. 1,  "test0480-0160 failed")

  contains

    function is_odd (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 1)
    end function is_odd

  end subroutine test0480

  subroutine test0490
    !
    ! FIXME: Add tests that the inputs to take_while are not
    ! destroyed.
    !

    ! An example from SRFI-1.
    call check (list_equal (int_eq, take_whilex (is_even, list (2, 18, 3, 10, 22, 9)), list (2, 18)), "test0490-0010 failed")
    call check (list_equal (int_eq, take_while (is_even, list (2, 18, 3, 10, 22, 9)), list (2, 18)), "test0490-0020 failed")

    call check (list_equal (int_eq, take_whilex (is_even, list (3, 10, 22, 9)), list ()), "test0490-0030 failed")
    call check (list_equal (int_eq, take_while (is_even, list (3, 10, 22, 9)), list ()), "test0490-0040 failed")

    call check (list_equal (int_eq, take_whilex (is_even, list (2)), list (2)), "test0490-0050 failed")
    call check (list_equal (int_eq, take_while (is_even, list (2)), list (2)), "test0490-0060 failed")

    call check (list_equal (int_eq, take_whilex (is_even, list (3)), list ()), "test0490-0070 failed")
    call check (list_equal (int_eq, take_while (is_even, list (3)), list ()), "test0490-0080 failed")

    call check (list_equal (int_eq, take_whilex (is_even, list ()), list ()), "test0490-0090 failed")
    call check (list_equal (int_eq, take_while (is_even, list ()), list ()), "test0490-0100 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

  end subroutine test0490

  subroutine test0500
    type(gcroot_t) :: lst
    type(cons_t) :: lst1, lst2

    ! An example from SRFI-1.
    lst = list (2, 18, 3, 10, 22, 9)
    call do_span (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst, list (2, 18, 3, 10, 22, 9)), "test0500-0010 failed")
    call check (list_equal (int_eq, lst1, list (2, 18)), "test0500-0020 failed")
    call check (list_equal (int_eq, lst2, list (3, 10, 22, 9)), "test0500-0030 failed")
    call do_spanx (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst1, list (2, 18)), "test0500-0040 failed")
    call check (list_equal (int_eq, lst2, list (3, 10, 22, 9)), "test0500-0050 failed")

    ! An example from SRFI-1.
    lst = list (3, 1, 4, 1, 5, 9)
    call do_break (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst, list (3, 1, 4, 1, 5, 9)), "test0500-0110 failed")
    call check (list_equal (int_eq, lst1, list (3, 1)), "test0500-0120 failed")
    call check (list_equal (int_eq, lst2, list (4, 1, 5, 9)), "test0500-0130 failed")
    call do_breakx (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst1, list (3, 1)), "test0500-0140 failed")
    call check (list_equal (int_eq, lst2, list (4, 1, 5, 9)), "test0500-0150 failed")

    lst = list (2, 18, 3, 10, 22, 9)
    call do_break (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst, list (2, 18, 3, 10, 22, 9)), "test0500-0210 failed")
    call check (list_equal (int_eq, lst1, list (2, 18, 3, 10, 22, 9)), "test0500-0220 failed")
    call check (list_equal (int_eq, lst2, list ()), "test0500-0230 failed")
    call do_breakx (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst1, list (2, 18, 3, 10, 22, 9)), "test0500-0240 failed")
    call check (list_equal (int_eq, lst2, list ()), "test0500-0250 failed")

    lst = list (3, 1, 4, 1, 5, 9)
    call do_span (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst, list (3, 1, 4, 1, 5, 9)), "test0500-0310 failed")
    call check (list_equal (int_eq, lst1, list ()), "test0500-0320 failed")
    call check (list_equal (int_eq, lst2, list (3, 1, 4, 1, 5, 9)), "test0500-0330 failed")
    call do_spanx (is_even, lst, lst1, lst2)
    call check (list_equal (int_eq, lst1, list ()), "test0500-0340 failed")
    call check (list_equal (int_eq, lst2, list (3, 1, 4, 1, 5, 9)), "test0500-0350 failed")

    call do_span (is_even, nil, lst1, lst2)
    call check (is_nil (lst1), "test0500-0400 failed")
    call check (is_nil (lst2), "test0500-0410 failed")
    call do_spanx (is_even, nil, lst1, lst2)
    call check (is_nil (lst1), "test0500-0420 failed")
    call check (is_nil (lst2), "test0500-0430 failed")
    call do_break (is_even, nil, lst1, lst2)
    call check (is_nil (lst1), "test0500-0440 failed")
    call check (is_nil (lst2), "test0500-0450 failed")
    call do_breakx (is_even, nil, lst1, lst2)
    call check (is_nil (lst1), "test0500-0460 failed")
    call check (is_nil (lst2), "test0500-0470 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

  end subroutine test0500

  subroutine test0510
    type(gcroot_t) :: lst
    type(gcroot_t) :: retval

    ! An example from SRFI-1.
    lst = list (2, 18, 3, 10, 22, 9)
    retval = span (is_even, lst)
    call check (list_equal (int_eq, lst, list (2, 18, 3, 10, 22, 9)), "test0510-0010 failed")
    call check (list_equal (int_eq, first (retval), list (2, 18)), "test0510-0020 failed")
    call check (list_equal (int_eq, second (retval), list (3, 10, 22, 9)), "test0510-0030 failed")
    retval = spanx (is_even, lst)
    call check (list_equal (int_eq, first (retval), list (2, 18)), "test0510-0040 failed")
    call check (list_equal (int_eq, second (retval), list (3, 10, 22, 9)), "test0510-0050 failed")

    ! An example from SRFI-1.
    lst = list (3, 1, 4, 1, 5, 9)
    retval = break (is_even, lst)
    call check (list_equal (int_eq, lst, list (3, 1, 4, 1, 5, 9)), "test0510-0110 failed")
    call check (list_equal (int_eq, first (retval), list (3, 1)), "test0510-0120 failed")
    call check (list_equal (int_eq, second (retval), list (4, 1, 5, 9)), "test0510-0130 failed")
    retval = breakx (is_even, lst)
    call check (list_equal (int_eq, first (retval), list (3, 1)), "test0510-0140 failed")
    call check (list_equal (int_eq, second (retval), list (4, 1, 5, 9)), "test0510-0150 failed")

    lst = list (2, 18, 3, 10, 22, 9)
    retval = break (is_even, lst)
    call check (list_equal (int_eq, lst, list (2, 18, 3, 10, 22, 9)), "test0510-0210 failed")
    call check (list_equal (int_eq, first (retval), list (2, 18, 3, 10, 22, 9)), "test0510-0220 failed")
    call check (list_equal (int_eq, second (retval), list ()), "test0510-0230 failed")
    retval = breakx (is_even, lst)
    call check (list_equal (int_eq, first (retval), list (2, 18, 3, 10, 22, 9)), "test0510-0240 failed")
    call check (list_equal (int_eq, second (retval), list ()), "test0510-0250 failed")

    lst = list (3, 1, 4, 1, 5, 9)
    retval = span (is_even, lst)
    call check (list_equal (int_eq, lst, list (3, 1, 4, 1, 5, 9)), "test0510-0310 failed")
    call check (list_equal (int_eq, first (retval), list ()), "test0510-0320 failed")
    call check (list_equal (int_eq, second (retval), list (3, 1, 4, 1, 5, 9)), "test0510-0330 failed")
    retval = spanx (is_even, lst)
    call check (list_equal (int_eq, first (retval), list ()), "test0510-0340 failed")
    call check (list_equal (int_eq, second (retval), list (3, 1, 4, 1, 5, 9)), "test0510-0350 failed")

    retval = span (is_even, nil)
    call check (is_nil (first (retval)), "test0510-0400 failed")
    call check (is_nil (second (retval)), "test0510-0410 failed")
    retval = spanx (is_even, nil)
    call check (is_nil (first (retval)), "test0510-0420 failed")
    call check (is_nil (second (retval)), "test0510-0430 failed")
    retval = break (is_even, nil)
    call check (is_nil (first (retval)), "test0510-0440 failed")
    call check (is_nil (second (retval)), "test0510-0450 failed")
    retval = breakx (is_even, nil)
    call check (is_nil (first (retval)), "test0510-0460 failed")
    call check (is_nil (second (retval)), "test0510-0470 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

  end subroutine test0510

  subroutine test0520

    call check (.not. some (is_even, iota (100, 1, 2)), "test0520-0010 failed")
    call check (some (is_even, append (iota (100, 1, 2), iota (100, 2, 2))), "test0520-0020 failed")
    call check (.not. some (is_even, nil), "test0520-0030 failed")

    call check (.not. some (is_lt, list (2, 3, 4), list (1, 2, 3)), "test0520-0040 failed")
    call check (.not. some (is_lt, list (), list (1, 2, 3)), "test0520-0050 failed")
    call check (.not. some (is_lt, list (2, 3, 4), list ()), "test0520-0060 failed")
    call check (some (is_lt, list (2, 3, 4), list (1, 4, 4)), "test0520-0070 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

    function is_lt (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function is_lt

  end subroutine test0520

  subroutine test0530

    call check (is_false (some_map (return_if_even, iota (100, 1, 2))), "test0530-0010 failed")
    call check (some_map (return_if_even, append (iota (100, 1, 2), iota (100, 20, 20))) .eqi. 20, "test0530-0020 failed")
    call check (is_false (some_map (return_if_even, nil)), "test0530-0030 failed")

    call check (is_false (some_map (return_sum_if_even, list (2, 3, 4), list (1, 2, 3))), "test0530-0040 failed")
    call check (is_false (some_map (return_sum_if_even, list (), list (1, 2, 3))), "test0530-0050 failed")
    call check (is_false (some_map (return_sum_if_even, list (2, 3, 4), list ())), "test0530-0060 failed")
    call check (some_map (return_sum_if_even, list (2, 3, 4), list (1, 4, 4)) .eqi. 8, "test0530-0070 failed")
    call check (some_map (return_sum_if_even, list (2, 3, 4), list (1, 3, 4)) .eqi. 6, "test0530-0080 failed")

  contains

    subroutine return_if_even (x, retval)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if (mod (int_cast (x), 2) == 0) then
         retval = x
      else
         retval = .false.
      end if
    end subroutine return_if_even

    subroutine return_sum_if_even (x, y, retval)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), allocatable, intent(out) :: retval

      integer :: sum

      call collect_garbage_now

      sum = int_cast (x) + int_cast (y)
      if (mod (sum, 2) == 0) then
         retval = sum
      else
         retval = .false.
      end if
    end subroutine return_sum_if_even

  end subroutine test0530

  subroutine test0540

    call check (.not. every (is_even, append (iota (100, 2, 2), iota (100, 1))), "test0540-0010 failed")
    call check (every (is_even, append (iota (100, 2, 2))), "test0540-0020 failed")
    call check (every (is_even, nil), "test0540-0030 failed")

    call check (.not. every (is_lt, list (2, 3, 4), list (1, 2, 3)), "test0540-0040 failed")
    call check (every (is_lt, list (), list (1, 2, 3)), "test0540-0050 failed")
    call check (every (is_lt, list (2, 3, 4), list ()), "test0540-0060 failed")
    call check (.not. every (is_lt, list (2, 3, 4), list (1, 4, 4)), "test0540-0070 failed")
    call check (every (is_lt, list (1, 2, 3), list (2, 3, 4)), "test0540-0080 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

    function is_lt (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function is_lt

  end subroutine test0540

  subroutine test0550

    call check (is_false (every_map (return_if_even, append (iota (100, 2, 2), iota (100, 1)))), "test0550-0010 failed")
    call check (every_map (return_if_even, append (iota (100, 2, 2))) .eqi. 200, "test0550-0020 failed")
    call check (logical_cast (every_map (return_if_even, nil)), "test0550-0030 failed")

    call check (is_false (every_map (return_sum_if_even, list (2, 3, 4), list (1, 2, 3))), "test0550-0040 failed")
    call check (logical_cast (every_map (return_sum_if_even, list (), list (1, 2, 3))), "test0550-0050 failed")
    call check (logical_cast (every_map (return_sum_if_even, list (2, 3, 4), list ())), "test0550-0060 failed")
    call check (is_false (every_map (return_sum_if_even, list (2, 3, 4), list (1, 4, 4))), "test0550-0070 failed")
    call check (is_false (every_map (return_sum_if_even, list (2, 3, 4), list (1, 3, 4))), "test0550-0080 failed")
    call check (every_map (return_sum_if_even, list (2, 3, 4), list (2, 3, 4)) .eqi. 8, "test0550-0090 failed")
    call check (every_map (return_sum_if_even, list (2, 3, 4), list (2, 3, 4, 5)) .eqi. 8, "test0550-0100 failed")

  contains

    subroutine return_if_even (x, retval)
      class(*), intent(in) :: x
      class(*), allocatable, intent(out) :: retval

      call collect_garbage_now

      if (mod (int_cast (x), 2) == 0) then
         retval = x
      else
         retval = .false.
      end if
    end subroutine return_if_even

    subroutine return_sum_if_even (x, y, retval)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), allocatable, intent(out) :: retval

      integer :: sum

      call collect_garbage_now

      sum = int_cast (x) + int_cast (y)
      if (mod (sum, 2) == 0) then
         retval = sum
      else
         retval = .false.
      end if
    end subroutine return_sum_if_even

  end subroutine test0550

  subroutine test0560

    ! An example from SRFI-1.
    call check (list_index0 (is_even, list (3, 1, 4, 1, 5, 9)) == 2, "test0560-0010 failed")
    call check (list_index1 (is_even, list (3, 1, 4, 1, 5, 9)) == 3, "test0560-0020 failed")
    call check (list_indexn (is_even, 2_sz, list (3, 1, 4, 1, 5, 9)) == 4, "test0560-0030 failed")
    call check (list_indexn (is_even, -2_sz, list (3, 1, 4, 1, 5, 9)) == 0, "test0560-0040 failed")

    ! An example from SRFI-1.
    call check (list_index0 (is_lt, list (3, 1, 4, 1, 5, 9), list (2, 7, 1, 8, 2)) == 1, "test0560-0110 failed")
    call check (list_index1 (is_lt, list (3, 1, 4, 1, 5, 9), list (2, 7, 1, 8, 2)) == 2, "test0560-0120 failed")
    call check (list_indexn (is_lt, 2_sz, list (3, 1, 4, 1, 5, 9), list (2, 7, 1, 8, 2)) == 3, "test0560-0130 failed")
    call check (list_indexn (is_lt, -2_sz, list (3, 1, 4, 1, 5, 9), list (2, 7, 1, 8, 2)) == -1, "test0560-0140 failed")

    ! An example from SRFI-1.
    call check (list_index0 (int_eq, list (3, 1, 4, 1, 5, 9, 2, 5, 6), list (2, 7, 1, 8, 2)) == -1, "test0560-0210 failed")
    call check (list_index1 (int_eq, list (3, 1, 4, 1, 5, 9, 2, 5, 6), list (2, 7, 1, 8, 2)) == -1, "test0560-0220 failed")
    call check (list_indexn (int_eq, 2_sz, list (3, 1, 4, 1, 5, 9, 2, 5, 6), list (2, 7, 1, 8, 2)) == -1, "test0560-0230 failed")
    call check (list_indexn (int_eq, -2_sz, list (3, 1, 4, 1, 5, 9, 2, 5, 6), list (2, 7, 1, 8, 2)) == -3, "test0560-0240 failed")

  contains

    function is_even (x) result (bool)
      class(*), intent(in) :: x
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (x), 2) == 0)
    end function is_even

    function is_lt (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function is_lt

  end subroutine test0560

  subroutine test0570
    type(cons_t) :: retval
    class(*), allocatable :: alst1, alst2

    retval = assoc (is_lt, 5, zip (iota (100, 1), iota (100, 10, 10)))
    call check (first (retval) .eqi. 6, "test0570-0010 failed")
    call check (second (retval) .eqi. 60, "test0570-0020 failed")

    retval = assoc (is_lt, 50000, zip (iota (100, 1), iota (100, 10, 10)))
    call check (is_nil (retval), "test0570-0030 failed")

    retval = assoc (int_eq, 7, alist_cons (2, 20, alist_cons (4, 40, alist_cons (7, 70, alist_cons (8, 80, nil)))))
    call check (car (retval) .eqi. 7, "test0570-0040 failed")
    call check (cdr (retval) .eqi. 70, "test0570-0050 failed")

    alst1 = zip (iota (100, 1), iota (100, 10, 10))
    alst2 = alist_copy (alst1)
    call set_cdr (alst1, nil)
    call check (is_nil (assoc (int_eq, 50, alst1)), "test0570-0060 failed")
    call check (car (assoc (int_eq, 50, alst2)) .eqi. 50, "test0570-0070 failed")
    call check (cadr (assoc (int_eq, 50, alst2)) .eqi. 500, "test0570-0080 failed")

    alst1 = zip (iota (100, 1), iota (100, 10, 10))
    alst2 = alist_delete (key_divides_value, 2, alst1)
    call check (list_equal (entry_eq, alst1, zip (iota (100, 1), iota (100, 10, 10))), "test0570-0090 failed")
    call check (list_equal (entry_eq, alst2, zip (iota (50, 1, 2), iota (50, 10, 20))), "test0570-0100 failed")

    alst1 = zip (iota (100, 1), iota (100, 10, 10))
    alst2 = alist_deletex (key_divides_value, 2, alst1)
    call check (list_equal (entry_eq, alst2, zip (iota (50, 1, 2), iota (50, 10, 20))), "test0570-0110 failed")

  contains

    function is_lt (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function is_lt

    function key_divides_value (key, val) result (bool)
      class(*), intent(in) :: key
      class(*), intent(in) :: val
      logical :: bool

      call collect_garbage_now

      bool = (mod (int_cast (val), int_cast (key)) == 0)
    end function key_divides_value

    function entry_eq (pair1, pair2) result (bool)
      class(*), intent(in) :: pair1
      class(*), intent(in) :: pair2
      logical :: bool

      ! DO NOT COLLECT GARBAGE in this function. We use it for
      ! list_equal tests and do not want to collect garbage during
      ! them.

      bool = (first (pair1) .eqi. first (pair2)) .and. (second (pair1) .eqi. second (pair2))
    end function entry_eq

  end subroutine test0570

  subroutine test0580

    call check (is_member (equal, 5, iota (100, 1)), "test0580-0010 failed")
    call check (.not. is_not_member (equal, 5, iota (100, 1)), "test0580-0020 failed")

    call check (.not. is_member (equal, -5, iota (100, 1)), "test0580-0030 failed")
    call check (is_not_member (equal, -5, iota (100, 1)), "test0580-0040 failed")

  contains

    function equal (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      bool = (int_cast (x) == int_cast (y))
    end function equal

  end subroutine test0580

  subroutine run_tests
    heap_size_limit = 0

    call test0010
    call test0020
    call test0030
    call test0040
    call test0050
    call test0060
    call test0065
    call test0070
    call test0075
    call test0080
    call test0090
    call check_heap_size
    call test0100
    call test0110
    call test0120
    call test0130
    call test0140
    call test0150
    call test0160
    call test0170
    call test0173
    call test0176
    call test0180
    call test0190
    call test0195
    call test0197
    call test0200
    call test0210
    call check_heap_size
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
    call test0355
    call test0360
    call test0370
    call test0380
    call test0390
    call test0395
    call test0400
    call test0405
    call test0410
    call test0420
    call test0430
    call test0440
    call test0450
    call test0460
    call test0470
    call test0480
    call test0490
    call test0500
    call test0510
    call test0520
    call test0530
    call test0540
    call test0550
    call test0560
    call test0570
    call test0580

    call collect_garbage_now
    call check (current_heap_size () == 0, "run_tests-0100 failed")
    call check (current_roots_count () == 0, "run_tests-0110 failed")
  end subroutine run_tests

end module test__cons_pairs

program main
  use, non_intrinsic :: test__cons_pairs
  implicit none
  call run_tests
end program main
