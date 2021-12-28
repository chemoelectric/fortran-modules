! -*- F90 -*- include(`common-macros.m4')m4_include([cadadr.m4])
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

!!$  pure function bincoef (n, k) result (coef)
!!$    integer, intent(in) :: n
!!$    integer, intent(in) :: k
!!$    integer :: coef
!!$    coef = nint (exp (log_gamma (n + 1.0D0) - log_gamma (n - k + 1.0D0) - log_gamma (k + 1.0D0)))
!!$  end function bincoef

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

m4_forloop([_i],1,CADADR_MAX,[dnl
    tree = build_tree (1, _i, 0)
m4_forloop([_k],0,m4_eval([(1 << (]_i[)) - 1]),[dnl
    leaf = int_cast ([c]m4_bits_to_ad_sequence(_i,_k)[r] (tree))
    call check (leaf == _k, "test0030-_i-_k failed")
])dnl
])dnl

  contains

    recursive function build_tree (m, n, k) result (tree)
      integer, intent(in) :: m, n, k
      class(cons_t), allocatable :: tree

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

    lst = list1 (123)
    call check (car (lst) .eqi. 123, "test0040-0010 failed")
    call check (is_nil (cdr (lst)), "test0040-0020 failed")
    call unlist1 (lst, obj1)
    call check (obj1 .eqi. 123, "test0040-0030 failed")
    call unlist1_with_tail (lst, obj1, tail)
    call check (obj1 .eqi. 123, "test0040-0040 failed")
    call check (is_nil (tail), "test0040-0050 failed")

    lst = list5 (1, 2, 3, 4, 5)
    call check (first (lst) .eqi. 1, "test0040-0110 failed")
    call check (second (lst) .eqi. 2, "test0040-0120 failed")
    call check (third (lst) .eqi. 3, "test0040-0130 failed")
    call check (fourth (lst) .eqi. 4, "test0040-0140 failed")
    call check (fifth (lst) .eqi. 5, "test0040-0150 failed")
    call check (is_nil (cddr (cdddr (lst))), "test0040-0160 failed")
    call unlist5 (lst, obj1, obj2, obj3, obj4, obj5)
    call check (obj1 .eqi. 1, "test0040-0210 failed")
    call check (obj2 .eqi. 2, "test0040-0220 failed")
    call check (obj3 .eqi. 3, "test0040-0230 failed")
    call check (obj4 .eqi. 4, "test0040-0240 failed")
    call check (obj5 .eqi. 5, "test0040-0250 failed")
    call unlist5_with_tail (lst, obj1, obj2, obj3, obj4, obj5, tail)
    call check (obj1 .eqi. 1, "test0040-0310 failed")
    call check (obj2 .eqi. 2, "test0040-0320 failed")
    call check (obj3 .eqi. 3, "test0040-0330 failed")
    call check (obj4 .eqi. 4, "test0040-0340 failed")
    call check (obj5 .eqi. 5, "test0040-0350 failed")
    call check (is_nil (tail), "test0040-0360 failed")
    call unlist4_with_tail (lst, obj1, obj2, obj3, obj4, tail)
    call check (obj1 .eqi. 1, "test0040-0410 failed")
    call check (obj2 .eqi. 2, "test0040-0420 failed")
    call check (obj3 .eqi. 3, "test0040-0430 failed")
    call check (obj4 .eqi. 4, "test0040-0440 failed")
    call check (car (tail) .eqi. 5, "test0040-0450 failed")
    call check (is_nil (cdr (tail)), "test0040-0460 failed")

    lst = list3_with_tail (1, 2, 3, list2 (4, 5))
    call check (first (lst) .eqi. 1, "test0040-1110 failed")
    call check (second (lst) .eqi. 2, "test0040-1120 failed")
    call check (third (lst) .eqi. 3, "test0040-1130 failed")
    call check (fourth (lst) .eqi. 4, "test0040-1140 failed")
    call check (fifth (lst) .eqi. 5, "test0040-1150 failed")
    call check (is_nil (cddr (cdddr (lst))), "test0040-1160 failed")
    call unlist5 (lst, obj1, obj2, obj3, obj4, obj5)
    call check (obj1 .eqi. 1, "test0040-1210 failed")
    call check (obj2 .eqi. 2, "test0040-1220 failed")
    call check (obj3 .eqi. 3, "test0040-1230 failed")
    call check (obj4 .eqi. 4, "test0040-1240 failed")
    call check (obj5 .eqi. 5, "test0040-1250 failed")
    call unlist5_with_tail (lst, obj1, obj2, obj3, obj4, obj5, tail)
    call check (obj1 .eqi. 1, "test0040-1310 failed")
    call check (obj2 .eqi. 2, "test0040-1320 failed")
    call check (obj3 .eqi. 3, "test0040-1330 failed")
    call check (obj4 .eqi. 4, "test0040-1340 failed")
    call check (obj5 .eqi. 5, "test0040-1350 failed")
    call check (is_nil (tail), "test0040-1360 failed")
    call unlist4_with_tail (lst, obj1, obj2, obj3, obj4, tail)
    call check (obj1 .eqi. 1, "test0040-1410 failed")
    call check (obj2 .eqi. 2, "test0040-1420 failed")
    call check (obj3 .eqi. 3, "test0040-1430 failed")
    call check (obj4 .eqi. 4, "test0040-1440 failed")
    call check (car (tail) .eqi. 5, "test0040-1450 failed")
    call check (is_nil (cdr (tail)), "test0040-1460 failed")
  end subroutine test0040

  subroutine test0050
    type(gcroot_t) :: tail

    call check (lists_are_equal (int_eq, nil, nil), "test0050-0010 failed")
    call check (lists_are_equal (int_eq, list1 (1), list1 (1)), "test0050-0020 failed")
    call check (lists_are_equal (int_eq, list2 (1, 2), list2 (1, 2)), "test0050-0030 failed")
    call check (lists_are_equal (int_eq, list5 (1, 2, 3, 4, 5), list5 (1, 2, 3, 4, 5)), "test0050-0040 failed")

    call check (.not. lists_are_equal (int_eq, nil, list1 (1)), "test0050-0110 failed")
    call check (.not. lists_are_equal (int_eq, list1 (1), nil), "test0050-0120 failed")
    call check (.not. lists_are_equal (int_eq, list4 (1, 2, 3, 4), list5 (1, 2, 3, 4, 5)), "test0050-0130 failed")
    call check (.not. lists_are_equal (int_eq, list5 (1, 2, 3, 4, 5), list4 (1, 2, 3, 4)), "test0050-0140 failed")
    call check (.not. lists_are_equal (int_eq, list5 (1, 2, 3, 4, 5), list5 (1, 2, 3, 4, 6)), "test0050-0150 failed")

    ! Check some lists with shared tails.
    tail = list3 (3, 2, 1)
    call check (lists_are_equal (int_eq, tail, tail), "test0050-0200 failed")
    call check (lists_are_equal (int_eq, 5 ** cons (4, tail), 5 ** cons (4, tail)), "test0050-0210 failed")
    call check (.not. lists_are_equal (int_eq, 5 ** cons (4, tail), 5 ** cons (40, tail)), "test0050-0220 failed")
  end subroutine test0050

  subroutine test0060
    type(gcroot_t) :: lst1, lst2, lst3

    lst1 = list1 (1)
    lst2 = list2 (1, 2)
    lst3 = list3 (1, 2, 3)

    call check (lists_are_equal (int_eq, take (nil, 0_sz), nil), "test0060-0010 failed")
    call check (lists_are_equal (int_eq, take (lst1, 0_sz), nil), "test0060-0020 failed")
    call check (lists_are_equal (int_eq, take (lst3, 0_sz), nil), "test0060-0030 failed")

    call check (lists_are_equal (int_eq, take (lst1, 1_sz), lst1), "test0060-0040 failed")
    call check (lists_are_equal (int_eq, take (lst3, 1_sz), lst1), "test0060-0050 failed")
    call check (lists_are_equal (int_eq, take (lst3, 2_sz), lst2), "test0060-0060 failed")
    call check (lists_are_equal (int_eq, take (lst3, 3_sz), lst3), "test0060-0070 failed")
  end subroutine test0060

  subroutine test0065
    type(cons_t) :: lst1, lst2, lst3

    lst1 = list1 (1)
    lst2 = list2 (1, 2)
    lst3 = list3 (1, 2, 3)

    call check (lists_are_equal (int_eq, take (nil, 0_sz), nil), "test0065-0010 failed")
    call check (lists_are_equal (int_eq, take (lst1, 0_sz), nil), "test0065-0020 failed")
    call check (lists_are_equal (int_eq, take (lst3, 0_sz), nil), "test0065-0030 failed")

    call check (lists_are_equal (int_eq, take (lst1, 1_sz), lst1), "test0065-0040 failed")
    call check (lists_are_equal (int_eq, take (lst3, 1_sz), lst1), "test0065-0050 failed")
    call check (lists_are_equal (int_eq, take (lst3, 2_sz), lst2), "test0065-0060 failed")
    call check (lists_are_equal (int_eq, take (lst3, 3_sz), lst3), "test0065-0070 failed")

    call check (lists_are_equal (int_eq, take (lst3, 3), lst3), "test0065-0075 failed")

    call check (lists_are_equal (int_eq, takex (nil, 0_sz), nil), "test0065-1010 failed")
    call check (lists_are_equal (int_eq, takex (list1 (1), 0_sz), nil), "test0065-1020 failed")
    call check (lists_are_equal (int_eq, takex (list3 (1, 2, 3), 0_sz), nil), "test0065-1030 failed")

    call check (lists_are_equal (int_eq, takex (list1 (1), 1_sz), lst1), "test0065-1040 failed")
    call check (lists_are_equal (int_eq, takex (list3 (1, 2, 3), 1_sz), lst1), "test0065-1050 failed")
    call check (lists_are_equal (int_eq, takex (list3 (1, 2, 3), 2_sz), lst2), "test0065-1060 failed")
    call check (lists_are_equal (int_eq, takex (list3 (1, 2, 3), 3_sz), lst3), "test0065-1070 failed")

    call check (lists_are_equal (int_eq, takex (list3 (1, 2, 3), 3), lst3), "test0065-1075 failed")
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
    call check (lists_are_equal (int_eq, make_list (0_sz, 5), nil), "test0080-0010 failed")
    call check (lists_are_equal (int_eq, make_list (1_sz, 5), 5 ** nil), "test0080-0020 failed")
    call check (lists_are_equal (int_eq, make_list (5_sz, 5), list5 (5, 5, 5, 5, 5)), "test0080-0030 failed")

    call check (lists_are_equal (int_eq, make_list (0, 5), nil), "test0080-0040 failed")
    call check (lists_are_equal (int_eq, make_list (1, 5), 5 ** nil), "test0080-0050 failed")
    call check (lists_are_equal (int_eq, make_list (5, 5), list5 (5, 5, 5, 5, 5)), "test0080-0060 failed")
  end subroutine test0080

  subroutine test0090
    call check (lists_are_equal (real_eq, list_tabulate0 (0_sz, make_real), nil), "test0090-0010 failed")
    call check (lists_are_equal (real_eq, list_tabulate1 (0_sz, make_real), nil), "test0090-0020 failed")
    call check (lists_are_equal (real_eq, list_tabulaten (0_sz, -1_sz, make_real), nil), "test0090-0030 failed")

    call check (lists_are_equal (real_eq, list_tabulate0 (1_sz, make_real), 0.0 ** nil), "test0090-0040 failed")
    call check (lists_are_equal (real_eq, list_tabulate1 (1_sz, make_real), 1.0 ** nil), "test0090-0050 failed")
    call check (lists_are_equal (real_eq, list_tabulaten (1_sz, -1_sz, make_real), (-1.0) ** nil), "test0090-0060 failed")

    call check (lists_are_equal (real_eq, list_tabulate0 (5_sz, make_real), list5 (0.0, 1.0, 2.0, 3.0, 4.0)), &
         "test0090-0070 failed")
    call check (lists_are_equal (real_eq, list_tabulate1 (5_sz, make_real), list5 (1.0, 2.0, 3.0, 4.0, 5.0)), &
         "test0090-0080 failed")
    call check (lists_are_equal (real_eq, list_tabulaten (5_sz, -1_sz, make_real), list5 (-1.0, 0.0, 1.0, 2.0, 3.0)), &
         "test0090-0090 failed")
  contains

    subroutine make_real (i, x)
      integer(sz), intent(in) :: i
      class(*), allocatable, intent(out) :: x
      x = real (i)
    end subroutine make_real

  end subroutine test0090

  subroutine test0100
    call check (lists_are_equal (int_eq, reverse (nil), nil), "test0100-0010 failed")
    call check (lists_are_equal (int_eq, reverse (123 ** nil), 123 ** nil), "test0100-0020 failed")
    call check (lists_are_equal (int_eq, reverse (list5 (1, 2, 3, 4, 5)), list5 (5, 4, 3, 2, 1)), "test0100-0030 failed")

    call check (lists_are_equal (int_eq, reversex (nil), nil), "test0100-0040 failed")
    call check (lists_are_equal (int_eq, reversex (123 ** nil), 123 ** nil), "test0100-0050 failed")
    call check (lists_are_equal (int_eq, reversex (list5 (1, 2, 3, 4, 5)), list5 (5, 4, 3, 2, 1)), "test0100-0060 failed")
  end subroutine test0100

  subroutine test0110
    call check (lists_are_equal (int_eq, take (circular_list (1 ** nil), 5_sz), list5 (1, 1, 1, 1, 1)), &
         "test0110-0010 failed")
    call check (lists_are_equal (int_eq, &
         take (circular_list (1 ** 2 ** 3 ** nil), 10_sz), &
         1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** nil), &
         "test0110-0020 failed")

    call check (lists_are_equal (int_eq, take (circular_listx (1 ** nil), 5_sz), list5 (1, 1, 1, 1, 1)), &
         "test0110-0030 failed")
    call check (lists_are_equal (int_eq, &
         take (circular_listx (1 ** 2 ** 3 ** nil), 10_sz), &
         1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** nil), &
         "test0110-0040 failed")

    call check (is_circular_list (circular_list (1 ** nil)), "test0110-0050 failed")
    call check (is_circular_list (circular_list (1 ** 2 ** 3 ** nil)), "test0110-0060 failed")
    call check (is_circular_list (4 ** 5 ** circular_list (1 ** 2 ** 3 ** nil)), "test0110-0070 failed")

    call check (.not. is_proper_list (circular_list (1 ** nil)), "test0110-0080 failed")
    call check (.not. is_proper_list (circular_list (1 ** 2 ** 3 ** nil)), "test0110-0090 failed")
    call check (.not. is_proper_list (4 ** 5 ** circular_list (1 ** 2 ** 3 ** nil)), "test0110-0100 failed")

    call check (.not. is_dotted_list (circular_list (1 ** nil)), "test0110-0110 failed")
    call check (.not. is_dotted_list (circular_list (1 ** 2 ** 3 ** nil)), "test0110-0120 failed")
    call check (.not. is_dotted_list (4 ** 5 ** circular_list (1 ** 2 ** 3 ** nil)), "test0110-0130 failed")
  end subroutine test0110

  subroutine test0120
    call check (last (1 ** nil) .eqi. 1, "test0120-0010 failed")
    call check (last (1 ** 2 ** 3 ** nil) .eqi. 3, "test0120-0020 failed")
  end subroutine test0120

  subroutine test0130
    type(gcroot_t) :: lst1, lst2

    lst1 = nil
    lst2 = list_copy (lst1)
    call check (lists_are_equal (int_eq, lst1, lst2), "test0130-0010 failed")

    lst1 = 123 ** nil
    lst2 = list_copy (lst1)
    call check (lists_are_equal (int_eq, lst1, lst2), "test0130-0020 failed")
    call check (is_nil (cdr (last_pair (lst2))), "test0130-0025 failed")

    lst1 = iota (100_sz, 1_sz)
    lst2 = list_copy (lst1)
    call check (lists_are_equal (size_kind_eq, lst1, lst2), "test0130-0030 failed")
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
    call check (lengthc (circular_list (iota (100_sz))) == -1_sz, "test0140-0140 failed")
    call check (lengthc (123 ** 456 ** circular_list (iota (100_sz))) == -1_sz, "test0140-0150 failed")
  end subroutine test0140

  subroutine test0150
    type(gcroot_t) :: lst

    call check (lists_are_equal (int_eq, take_right (nil, 0_sz), nil), "test0150-0010 failed")
    call check (lists_are_equal (int_eq, take_right (list1 (1), 0_sz), nil), "test0150-0020 failed")
    call check (lists_are_equal (size_kind_eq, take_right (iota (100_sz), 0_sz), nil), "test0150-0030 failed")

    call check (lists_are_equal (int_eq, take_right (list1 (1), 1_sz), list1 (1)), "test0150-0040 failed")
    call check (lists_are_equal (size_kind_eq, take_right (iota (100_sz), 1_sz), list1 (99_sz)), "test0150-0050 failed")

    call check (lists_are_equal (size_kind_eq, take_right (iota (100_sz), 5_sz), iota (5_sz, 95_sz)), "test0150-0060 failed")
    call check (lists_are_equal (int_eq, take_right (iota (100), 5), iota (5, 95)), "test0150-0065 failed")

    call check (take_right (5, 0_sz) .eqi. 5, "test0150-0070 failed")
    call check (take_right (cons (4, 5), 0_sz) .eqi. 5, "test0150-0080 failed")
    call check (car (take_right (3 ** cons (4, 5), 1_sz)) .eqi. 4, "test0150-0090 failed")
    call check (cdr (take_right (3 ** cons (4, 5), 1_sz)) .eqi. 5, "test0150-0100 failed")

    call check (lists_are_equal (int_eq, drop (nil, 0_sz), nil), "test0150-1010 failed")
    call check (lists_are_equal (int_eq, drop (list1 (1), 1_sz), nil), "test0150-1020 failed")
    call check (lists_are_equal (size_kind_eq, drop (iota (100_sz), 100_sz), nil), "test0150-1030 failed")

    call check (is_nil (drop (list1 (1), 1_sz)), "test0150-1040 failed")
    call check (lists_are_equal (size_kind_eq, drop (iota (100_sz), 99_sz), list1 (99_sz)), "test0150-1050 failed")

    call check (lists_are_equal (size_kind_eq, drop (iota (100_sz), 95_sz), iota (5_sz, 95_sz)), "test0150-1060 failed")
    call check (lists_are_equal (int_eq, drop (iota (100), 95), iota (5, 95)), "test0150-1065 failed")

    call check (drop (5, 0_sz) .eqi. 5, "test0150-1070 failed")
    call check (drop (cons (4, 5), 1_sz) .eqi. 5, "test0150-1080 failed")
    call check (car (drop (3 ** cons (4, 5), 1_sz)) .eqi. 4, "test0150-1090 failed")
    call check (cdr (drop (3 ** cons (4, 5), 1_sz)) .eqi. 5, "test0150-1100 failed")

    lst = list3 (1, 2, 3)

    call check (lists_are_equal (int_eq, take_right (lst, 0_sz), nil), "test0150-2010 failed")
    call check (lists_are_equal (int_eq, take_right (lst, 1_sz), list1 (3)), "test0150-2020 failed")
    call check (lists_are_equal (int_eq, take_right (lst, 2_sz), list2 (2, 3)), "test0150-2030 failed")
    call check (lists_are_equal (int_eq, take_right (lst, 3_sz), lst), "test0150-2040 failed")

    call check (lists_are_equal (int_eq, drop (lst, 0_sz), lst), "test0150-3010 failed")
    call check (lists_are_equal (int_eq, drop (lst, 1_sz), list2 (2, 3)), "test0150-3020 failed")
    call check (lists_are_equal (int_eq, drop (lst, 2_sz), list1 (3)), "test0150-3030 failed")
    call check (lists_are_equal (int_eq, drop (lst, 3_sz), nil), "test0150-3040 failed")
  end subroutine test0150

  subroutine test0160
    call check (lists_are_equal (int_eq, drop_right (nil, 0_sz), nil), "test0160-0010 failed")
    call check (lists_are_equal (int_eq, drop_right (list1 (1), 0_sz), list1 (1)), "test0160-0020 failed")
    call check (lists_are_equal (int_eq, drop_right (list1 (1), 1_sz), nil), "test0160-0030 failed")
    call check (lists_are_equal (size_kind_eq, drop_right (iota (100_sz), 0_sz), iota (100_sz)), "test0160-0040 failed")
    call check (lists_are_equal (size_kind_eq, drop_right (iota (100_sz), 100_sz), nil), "test0160-0050 failed")
    call check (lists_are_equal (size_kind_eq, drop_right (iota (100_sz), 50_sz), iota (50_sz)), "test0160-0060 failed")
    call check (lists_are_equal (size_kind_eq, drop_right (iota (100_sz), 25_sz), iota (75_sz)), "test0160-0070 failed")
    call check (lists_are_equal (size_kind_eq, drop_right (iota (100_sz), 75_sz), iota (25_sz)), "test0160-0080 failed")
    call check (lists_are_equal (int_eq, drop_right (iota (100), 75), iota (25)), "test0160-0085 failed")

    call check (lists_are_equal (int_eq, drop_rightx (nil, 0_sz), nil), "test0160-1010 failed")
    call check (lists_are_equal (int_eq, drop_rightx (list1 (1), 0_sz), list1 (1)), "test0160-1020 failed")
    call check (lists_are_equal (int_eq, drop_rightx (list1 (1), 1_sz), nil), "test0160-1030 failed")
    call check (lists_are_equal (size_kind_eq, drop_rightx (iota (100_sz), 0_sz), iota (100_sz)), "test0160-1040 failed")
    call check (lists_are_equal (size_kind_eq, drop_rightx (iota (100_sz), 100_sz), nil), "test0160-1050 failed")
    call check (lists_are_equal (size_kind_eq, drop_rightx (iota (100_sz), 50_sz), iota (50_sz)), "test0160-1060 failed")
    call check (lists_are_equal (size_kind_eq, drop_rightx (iota (100_sz), 25_sz), iota (75_sz)), "test0160-1070 failed")
    call check (lists_are_equal (size_kind_eq, drop_rightx (iota (100_sz), 75_sz), iota (25_sz)), "test0160-1080 failed")
    call check (lists_are_equal (int_eq, drop_rightx (iota (100), 75), iota (25)), "test1160-0085 failed")
  end subroutine test0160

  subroutine test0170
    type(cons_t) :: lst_left
    class(*), allocatable :: lst_right

    call split_at (nil, 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-0010 failed")
    call check (is_nil (lst_right), "test0170-0020 failed")

    call split_at (list3 (1, 2, 3), 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-0030 failed")
    call check (lists_are_equal (int_eq, lst_right, list3 (1, 2, 3)), "test0170-0040 failed")

    call split_at (list3 (1, 2, 3), 1_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list1 (1)), "test0170-0050 failed")
    call check (lists_are_equal (int_eq, lst_right, list2 (2, 3)), "test0170-0060 failed")

    call split_at (list3 (1, 2, 3), 2_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list2 (1, 2)), "test0170-0070 failed")
    call check (lists_are_equal (int_eq, lst_right, list1 (3)), "test0170-0080 failed")

    call split_at (list3 (1, 2, 3), 2, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list2 (1, 2)), "test0170-0085 failed")
    call check (lists_are_equal (int_eq, lst_right, list1 (3)), "test0170-0086 failed")

    call split_at (list3 (1, 2, 3), 3_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list3 (1, 2, 3)), "test0170-0090 failed")
    call check (is_nil (lst_right), "test0170-0100 failed")

    call split_at (1 ** cons (2, 3), 1_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list1 (1)), "test0170-0110 failed")
    call check (car (lst_right) .eqi. 2, "test0170-0120 failed")
    call check (cdr (lst_right) .eqi. 3, "test0170-0130 failed")

    call split_at (1 ** cons (2, 3), 2_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list2 (1, 2)), "test0170-0140 failed")
    call check (lst_right .eqi. 3, "test0170-0150 failed")

    call split_atx (nil, 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-1010 failed")
    call check (is_nil (lst_right), "test0170-1020 failed")

    call split_atx (list3 (1, 2, 3), 0_sz, lst_left, lst_right)
    call check (is_nil (lst_left), "test0170-1030 failed")
    call check (lists_are_equal (int_eq, lst_right, list3 (1, 2, 3)), "test0170-1040 failed")

    call split_atx (list3 (1, 2, 3), 1_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list1 (1)), "test0170-1050 failed")
    call check (lists_are_equal (int_eq, lst_right, list2 (2, 3)), "test0170-1060 failed")

    call split_atx (list3 (1, 2, 3), 2_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list2 (1, 2)), "test0170-1070 failed")
    call check (lists_are_equal (int_eq, lst_right, list1 (3)), "test0170-1080 failed")

    call split_atx (list3 (1, 2, 3), 2, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list2 (1, 2)), "test0170-1085 failed")
    call check (lists_are_equal (int_eq, lst_right, list1 (3)), "test0170-1086 failed")

    call split_atx (list3 (1, 2, 3), 3_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list3 (1, 2, 3)), "test0170-1090 failed")
    call check (is_nil (lst_right), "test0170-1100 failed")

    call split_atx (1 ** cons (2, 3), 1_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list1 (1)), "test0170-1110 failed")
    call check (car (lst_right) .eqi. 2, "test0170-1120 failed")
    call check (cdr (lst_right) .eqi. 3, "test0170-1130 failed")

    call split_atx (1 ** cons (2, 3), 2_sz, lst_left, lst_right)
    call check (lists_are_equal (int_eq, lst_left, list2 (1, 2)), "test0170-1140 failed")
    call check (lst_right .eqi. 3, "test0170-1150 failed")
  end subroutine test0170

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
    call check (lists_are_equal (int_eq, append (nil, nil), nil), "test0190-0010 failed")
    call check (append (nil, 123) .eqi. 123, "test0190-0020 failed")
    call check (lists_are_equal (int_eq, append (nil, list1 (123)), list1 (123)), "test0190-0030 failed")
    call check (lists_are_equal (int_eq, append (list1 (123), nil), list1 (123)), "test0190-0040 failed")
    call check (car (append (list1 (123), 45)) .eqi. 123, "test0190-0050 failed")
    call check (cdr (append (list1 (123), 45)) .eqi. 45, "test0190-0060 failed")
    call check (lists_are_equal (size_kind_eq, &
         append (iota (75_sz, 1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0190-0070 failed")

    call check (lists_are_equal (int_eq, appendx (nil, nil), nil), "test0190-1010 failed")
    call check (appendx (nil, 123) .eqi. 123, "test0190-1020 failed")
    call check (lists_are_equal (int_eq, appendx (nil, list1 (123)), list1 (123)), "test0190-1030 failed")
    call check (lists_are_equal (int_eq, appendx (list1 (123), nil), list1 (123)), "test0190-1040 failed")
    call check (car (appendx (list1 (123), 45)) .eqi. 123, "test0190-1050 failed")
    call check (cdr (appendx (list1 (123), 45)) .eqi. 45, "test0190-1060 failed")
    call check (lists_are_equal (size_kind_eq, &
         appendx (iota (75_sz, 1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0190-1070 failed")
  end subroutine test0190

  subroutine test0195
    call check (lists_are_equal (int_eq, append_reverse (nil, nil), nil), "test0195-0010 failed")
    call check (append_reverse (nil, 123) .eqi. 123, "test0195-0020 failed")
    call check (lists_are_equal (int_eq, append_reverse (nil, list1 (123)), list1 (123)), "test0195-0030 failed")
    call check (lists_are_equal (int_eq, append_reverse (list1 (123), nil), list1 (123)), "test0195-0040 failed")
    call check (car (append_reverse (list1 (123), 45)) .eqi. 123, "test0195-0050 failed")
    call check (cdr (append_reverse (list1 (123), 45)) .eqi. 45, "test0195-0060 failed")
    call check (lists_are_equal (size_kind_eq, &
         append_reverse (iota (75_sz, 75_sz, -1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0195-0070 failed")

    call check (lists_are_equal (int_eq, append_reversex (nil, nil), nil), "test0195-1010 failed")
    call check (append_reversex (nil, 123) .eqi. 123, "test0195-1020 failed")
    call check (lists_are_equal (int_eq, append_reversex (nil, list1 (123)), list1 (123)), "test0195-1030 failed")
    call check (lists_are_equal (int_eq, append_reversex (list1 (123), nil), list1 (123)), "test0195-1040 failed")
    call check (car (append_reversex (list1 (123), 45)) .eqi. 123, "test0195-1050 failed")
    call check (cdr (append_reversex (list1 (123), 45)) .eqi. 45, "test0195-1060 failed")
    call check (lists_are_equal (size_kind_eq, &
         append_reversex (iota (75_sz, 75_sz, -1_sz), iota (25_sz, 76_sz)), &
         iota (100_sz, 1_sz)), &
         "test0195-1070 failed")
  end subroutine test0195

  subroutine test0200
    call check (is_nil (concatenate (nil)), "test0200-0010 failed")
    call check (is_nil (concatenate (123)), "test0200-0020 failed")

    call check (lists_are_equal (int_eq, concatenate (list1 (123 ** nil)), 123 ** nil), "test0200-0030 failed")
    call check (lists_are_equal (int_eq, concatenate (list2 (123 ** nil, nil)), 123 ** nil), "test0200-0040 failed")
    call check (lists_are_equal (int_eq, concatenate (list2 (nil, 123 ** nil)), 123 ** nil), "test0200-0050 failed")

    call check (lists_are_equal (size_kind_eq, &
         concatenate (list5 (iota (25_sz, 1_sz), nil, iota (50_sz, 26_sz), iota (25_sz, 76_sz), nil)), &
         iota (100_sz, 1_sz)), &
         "test0200-0060 failed")

    call check (is_nil (concatenatex (nil)), "test0200-1010 failed")
    call check (is_nil (concatenatex (123)), "test0200-1020 failed")

    call check (lists_are_equal (int_eq, concatenatex (list1 (123 ** nil)), 123 ** nil), "test0200-1030 failed")
    call check (lists_are_equal (int_eq, concatenatex (list2 (123 ** nil, nil)), 123 ** nil), "test0200-1040 failed")
    call check (lists_are_equal (int_eq, concatenatex (list2 (nil, 123 ** nil)), 123 ** nil), "test0200-1050 failed")

    call check (lists_are_equal (size_kind_eq, &
         concatenatex (list5 (iota (25_sz, 1_sz), nil, iota (50_sz, 26_sz), iota (25_sz, 76_sz), nil)), &
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

    zipped1 = zip1 (lst1)
    zipped2 = zip2 (lst2, lst3)
    zipped3 = zip3 (lst2, lst3, lst5)
    zipped4 = zip4 (lst1, lst2, lst4, lst5)
    zipped5 = zip5 (lst1, lst2, lst3, lst4, lst5)

    call check (length (zipped1) == 300, "test0210-0010 failed")
    call check (length (zipped2) == 100, "test0210-0020 failed")
    call check (length (zipped3) == 100, "test0210-0030 failed")
    call check (length (zipped4) == 200, "test0210-0040 failed")
    call check (length (zipped5) == 100, "test0210-0050 failed")

    i = 1
    p = zipped1
    do while (is_pair (p))
       call check (lists_are_equal (int_eq, car (p), list1 (i)), "test0210-0110 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped2
    do while (is_pair (p))
       call check (lists_are_equal (int_eq, car (p), list2 (i + 1, i + 2)), "test0210-0120 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped3
    do while (is_pair (p))
       call check (lists_are_equal (int_eq, car (p), list3 (i + 1, i + 2, i + 4)), "test0210-0130 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped4
    do while (is_pair (p))
       call check (lists_are_equal (int_eq, car (p), list4 (i, i + 1, i + 3, i + 4)), "test0210-0140 failed")
       i = i + 1
       p = cdr (p)
    end do

    i = 1
    p = zipped5
    do while (is_pair (p))
       call check (lists_are_equal (int_eq, car (p), list5 (i, i + 1, i + 2, i + 3, i + 4)), "test0210-0150 failed")
       i = i + 1
       p = cdr (p)
    end do

    call check (lists_are_equal (int_eq, unzip1f (zipped1), lst1), "test0210-0200 failed")

    call unzip1 (zipped1, unzipped1)
    call check (lists_are_equal (int_eq, unzipped1, lst1), "test0210-0210 failed")

    call unzip2 (zipped2, unzipped2, unzipped3)
    call check (lists_are_equal (int_eq, unzipped2, take (lst2, 100)), "test0210-0220 failed")
    call check (lists_are_equal (int_eq, unzipped3, lst3), "test0210-0230 failed")

    call unzip3 (zipped3, unzipped2, unzipped3, unzipped5)
    call check (lists_are_equal (int_eq, unzipped2, take (lst2, 100)), "test0210-0240 failed")
    call check (lists_are_equal (int_eq, unzipped3, lst3), "test0210-0250 failed")
    call check (lists_are_equal (int_eq, unzipped5, take (lst5, 100)), "test0210-0260 failed")

    call unzip4 (zipped4, unzipped1, unzipped2, unzipped4, unzipped5)
    call check (lists_are_equal (int_eq, unzipped1, take (lst1, 200)), "test0210-0270 failed")
    call check (lists_are_equal (int_eq, unzipped2, lst2), "test0210-0280 failed")
    call check (lists_are_equal (int_eq, unzipped4, lst4), "test0210-0290 failed")
    call check (lists_are_equal (int_eq, unzipped5, take (lst5, 200)), "test0210-0300 failed")

    call unzip5 (zipped5, unzipped1, unzipped2, unzipped3, unzipped4, unzipped5)
    call check (lists_are_equal (int_eq, unzipped1, take (lst1, 100)), "test0210-0310 failed")
    call check (lists_are_equal (int_eq, unzipped2, take (lst2, 100)), "test0210-0320 failed")
    call check (lists_are_equal (int_eq, unzipped3, lst3), "test0210-0320 failed")
    call check (lists_are_equal (int_eq, unzipped4, take (lst4, 100)), "test0210-0340 failed")
    call check (lists_are_equal (int_eq, unzipped5, take (lst5, 100)), "test0210-0350 failed")

    call check (is_nil (zip1 (nil)), "test0210-0410 failed")
    call check (is_nil (zip2 (nil, iota (10))), "test0210-0420 failed")
    call check (is_nil (zip3 (iota (10), nil, nil)), "test0210-0430 failed")
    call check (is_nil (zip4 (iota (10), nil, iota (10), nil)), "test0210-0440 failed")
    call check (is_nil (zip5 (nil, nil, nil, iota (100), nil)), "test0210-0450 failed")

    call check (is_nil (unzip1f (nil)), "test0210-0500 failed")

    call unzip1 (nil, unzipped1)
    call check (is_nil (unzipped1), "test0210-0510 failed")

    call unzip2 (nil, unzipped1, unzipped2)
    call check (is_nil (unzipped1), "test0210-0520 failed")
    call check (is_nil (unzipped2), "test0210-0530 failed")

    call unzip3 (nil, unzipped1, unzipped2, unzipped3)
    call check (is_nil (unzipped1), "test0210-0540 failed")
    call check (is_nil (unzipped2), "test0210-0550 failed")
    call check (is_nil (unzipped3), "test0210-0560 failed")

    call unzip4 (nil, unzipped1, unzipped2, unzipped3, unzipped4)
    call check (is_nil (unzipped1), "test0210-0570 failed")
    call check (is_nil (unzipped2), "test0210-0580 failed")
    call check (is_nil (unzipped3), "test0210-0590 failed")
    call check (is_nil (unzipped4), "test0210-0600 failed")

    call unzip5 (nil, unzipped1, unzipped2, unzipped3, unzipped4, unzipped5)
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
    call check (list_count (pred, take (circular_list (list3 (-1, 0, 1)), 100)) == 100, "test0220-0060 failed")
    call check (list_count (pred, take (circular_list (list5 (-2, -1, 0, 1, 2)), 100)) == 60, "test0220-0070 failed")

  contains

    function pred (i) result (bool)
      class(*), intent(in) :: i
      logical :: bool

      integer :: ii

      ii = int_cast (i)
      bool = (-1 <= ii .and. ii <= 1)

      call collect_garbage_now  ! Test whether this messes things up.

    end function pred

  end subroutine test0220

  subroutine test0230
    type(cons_t) :: lst1, lst2, lst3

    ! Use fold to add the numbers in a list. (An example from SRFI-1.)
    call check (fold (kons_iadd, 0, iota (10, 1)) .eqi. 55, "test0230-0010 failed")

    ! Use fold to reverse a list. (An example from SRFI-1.)
    lst1 = iota (10, 1)
    lst2 = .tocons. fold (kcons, nil, lst1)
    call check (lists_are_equal (int_eq, lst1, iota (10, 1)), "test0230-0020 failed")
    call check (lists_are_equal (int_eq, lst2, iota (10, 10, -1)), "test0230-0030 failed")

    ! Use fold to do an append-reverse. (An example from SRFI-1.)
    lst1 = iota (10, 11)
    lst2 = iota (10, 10, -1)
    lst3 = .tocons. fold (kcons, lst1, lst2)
    call check (lists_are_equal (int_eq, lst1, iota (10, 11)), "test0230-0040 failed")
    call check (lists_are_equal (int_eq, lst2, iota (10, 10, -1)), "test0230-0050 failed")
    call check (lists_are_equal (int_eq, lst3, iota (20, 1)), "test0230-0060 failed")

    ! Count how many strings are in a list. (Adapted from an example
    ! in SRFI-1.)
    lst1 = 1 ** 2.0 ** str_t ('3.0') ** str_t ('x') ** 4.0 ** 5_sz ** str_t ('y') ** str_t ('z') &
         ** list3 (str_t ('a'), str_t ('b'), str_t ('c')) ** nil
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

  end subroutine test0230

  subroutine test0240
    type(cons_t) :: lst1, lst2

    ! Use fold_right to copy a list. (An example from SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = .tocons. fold_right (kcons, nil, lst1)
    call check (lists_are_equal (int_eq, lst1, iota (100, 1)), "test0240-0010 failed")
    call check (lists_are_equal (int_eq, lst2, iota (100, 1)), "test0240-0020 failed")

    ! Keep only the even elements of a list. (An example from SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = .tocons. fold_right (kcons_if_even, nil, lst1)
    call check (lists_are_equal (int_eq, lst1, iota (100, 1)), "test0240-0030 failed")
    call check (lists_are_equal (int_eq, lst2, iota (50, 2, 2)), "test0240-0040 failed")

    ! Try it again with a nil list.
    lst2 = .tocons. fold_right (kcons_if_even, nil, nil)
    call check (lists_are_equal (int_eq, lst2, nil), "test0240-0050 failed")

    ! Try it again, but with a non-nil tail.
    lst1 = iota (95, 1)
    lst2 = .tocons. fold_right (kcons_if_even, 96 ** 98 ** 100 ** nil, lst1)
    call check (lists_are_equal (int_eq, lst1, iota (95, 1)), "test0240-0060 failed")
    call check (lists_are_equal (int_eq, lst2, iota (50, 2, 2)), "test0240-0070 failed")

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

  end subroutine test0240

  subroutine test0250
    type(cons_t) :: lst1, lst2, p
    integer :: i

    ! Destructively reverse a list. (An example from SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = .tocons. pair_fold (ksetcdr, nil, lst1)
    call check (lists_are_equal (int_eq, lst1, 1 ** nil), "test0250-0010 failed")
    call check (lists_are_equal (int_eq, lst2, iota (100, 100, -1)), "test0250-0020 failed")

    ! Enumerate tails in order of ascending length.
    lst1 = iota (100, 1)
    lst2 = .tocons. pair_fold (kcopy, nil, lst1)
    call check (lists_are_equal (int_eq, lst1, iota (100, 1)), "test0250-0030 failed")
    i = 1
    p = lst2
    do while (is_pair (p))
       call check (lists_are_equal (int_eq, car (p), iota (i, 101 - i)), "test0250-0040 failed")
       i = i + 1
       p = .tocons. cdr (p)
    end do
    call check (is_nil (p), "test0250-0050 failed")

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

  end subroutine test0250

  subroutine test0260
    type(cons_t) :: lst1, lst2, lst3, p
    integer :: i

    ! A wasteful destructive append. (It does many more set_cdr than
    ! are necessary.)
    lst1 = iota (100, 1)
    lst2 = iota (100, 101)
    lst3 = .tocons. pair_fold_right (ksetcdr, lst2, lst1)
    call check (lists_are_equal (int_eq, lst1, iota (200, 1)), "test0260-0010 failed")
    call check (lists_are_equal (int_eq, lst2, iota (100, 101)), "test0260-0015 failed")
    call check (lists_are_equal (int_eq, lst3, iota (200, 1)), "test0260-0020 failed")

    ! Enumerate tails in order of descending length. (An example from
    ! SRFI-1.)
    lst1 = iota (100, 1)
    lst2 = .tocons. pair_fold_right (kcopy, nil, lst1)
    call check (lists_are_equal (int_eq, lst1, iota (100, 1)), "test0260-0030 failed")
    i = 1
    p = lst2
    do while (is_pair (p))
       call check (lists_are_equal (int_eq, car (p), iota (101 - i, i)), "test0260-0040 failed")
       i = i + 1
       p = .tocons. cdr (p)
    end do
    call check (is_nil (p), "test0260-0050 failed")

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

  end subroutine test0260

  subroutine test0270
    type(cons_t) :: lst1

    ! Find the maximum in a list of non-negative integers. (An example
    ! from SRFI-1.)
    lst1 = .tocons. concatenate (list5 (iota (10), iota (100), iota (1000), iota (100), iota (10)))
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
    type(cons_t) :: lst1, lst2, lst3

    ! An implementation of `concatenate'. (An example from SRFI-1.)
    lst1 = list5 (1 ** 2 ** 3 ** nil, 4 ** 5 ** nil, nil, 6 ** 7 ** 8 ** 9 ** 10 ** nil, nil)
    lst2 = .tocons. reduce_right (kappend, nil, lst1)
    lst3 = iota (10, 1)
    call check (lists_are_equal (int_eq, lst2, lst3), "test0280-0010 failed")

    ! Try it on a nil list
    lst1 = nil
    lst2 = .tocons. reduce_right (kappend, nil, lst1)
    lst3 = nil
    call check (lists_are_equal (int_eq, lst2, lst3), "test0280-0020 failed")

  contains

    recursive subroutine kappend (kar, kdr, kons)
      class(*), intent(in) :: kar, kdr
      class(*), allocatable, intent(out) :: kons

      call collect_garbage_now
      kons = append (kar, kdr)
    end subroutine kappend

  end subroutine test0280

  subroutine test0290

    ! List of squares 1**2,2**2,...,10**2. (An example from SRFI-1.)
    call check (lists_are_equal (int_eq, &
         unfold (k_gt_10, square_k, increment_k, 1), &
         list10 (1**2, 2**2, 3**2, 4**2, 5**2, 6**2, 7**2, 8**2, 9**2, 10**2)), &
         "test0290-0010 failed")

  contains

    recursive function k_gt_10 (k) result (bool)
      class(*), intent(in) :: k
      logical :: bool
      bool = (int_cast (k) > 10)
    end function k_gt_10

    recursive subroutine square_k (k, k_sq)
      class(*), intent(in) :: k
      class(*), allocatable, intent(out) :: k_sq
      k_sq = (int_cast (k)) ** 2
    end subroutine square_k

    recursive subroutine increment_k (k, k_incr)
      class(*), intent(in) :: k
      class(*), allocatable, intent(out) :: k_incr
      k_incr = (int_cast (k)) + 1
    end subroutine increment_k

  end subroutine test0290

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
    call test0180
    call test0190
    call test0195
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
