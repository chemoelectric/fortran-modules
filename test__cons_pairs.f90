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

module test__cons_pairs

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none
  private

  public :: run_tests

  integer, parameter :: sz = size_kind

  interface operator(.eqi.)
     module procedure integer_eq
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

  function integer_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = integer_cast (obj1) == integer_cast (obj2)
  end function integer_eq

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
    call check (car (.val. cons1) .eqr. 123.0, "test0010-0030 failed")
    call check (cdr (.val. cons1) .eqr. 456.0, "test0010-0040 failed")
    call uncons (.val. cons1, car1, cdr1)
    call check (car1 .eqr. 123.0, "test0010-0050 failed")
    call check (cdr1 .eqr. 456.0, "test0010-0060 failed")

    call check (is_pair (cons (123, 456)), "test0010-0070 failed")
    call check (is_pair (.val. cons1), "test0010-0080 failed")

    call check (.not. is_not_pair (cons (123, 456)), "test0010-0090 failed")
    call check (.not. is_not_pair (.val. cons1), "test0010-0100 failed")

    call check (.not. cons_t_eq (cons (123, 456), cons (123, 456)), "test0010-0110 failed")
    call check (cons_t_eq (.val. cons1, .val. cons1), "test0010-0120 failed")

    call check (.not. is_nil (cons (123, 456)), "test0010-0130 failed")
    call check (.not. is_nil (.val. cons1), "test0010-0140 failed")

    call check (is_not_nil (cons (123, 456)), "test0010-0150 failed")
    call check (is_not_nil (.val. cons1), "test0010-0160 failed")

    call check (.not. is_nil_list (cons (123, 456)), "test0010-0170 failed")
    call check (.not. is_nil_list (.val. cons1), "test0010-0180 failed")

    call check (is_nil (nil), "test0010-0190 failed")
    call check (.not. is_not_nil (nil), "test0010-0200 failed")
    call check (is_nil_list (nil), "test0010-0210 failed")

    call check (cons_t_eq (nil, nil), "test0010-0200 failed")
    call check (.not. cons_t_eq (nil, cons (1, nil)), "test0010-0210 failed")
    call check (.not. cons_t_eq (cons (1, nil), nil), "test0010-0210 failed")
  end subroutine test0010

  subroutine test0020
    type(gcroot_t) :: lst1
    logical :: agc_save

    agc_save = automatic_garbage_collection
    automatic_garbage_collection = .true.

    heap_size_limit = 1

    lst1 = 1 ** 2 ** 3 ** 4 ** 5 ** 6 ** 7 ** 8 ** 9 ** 10 ** nil
    call check (first (.val. lst1) .eqi. 1, "test0020-0010 failed")
    call check (second (.val. lst1) .eqi. 2, "test0020-0020 failed")
    call check (third (.val. lst1) .eqi. 3, "test0020-0030 failed")
    call check (fourth (.val. lst1) .eqi. 4, "test0020-0040 failed")
    call check (fifth (.val. lst1) .eqi. 5, "test0020-0050 failed")
    call check (sixth (.val. lst1) .eqi. 6, "test0020-0060 failed")
    call check (seventh (.val. lst1) .eqi. 7, "test0020-0070 failed")
    call check (eighth (.val. lst1) .eqi. 8, "test0020-0080 failed")
    call check (ninth (.val. lst1) .eqi. 9, "test0020-0090 failed")
    call check (tenth (.val. lst1) .eqi. 10, "test0020-0100 failed")
    call check (car (.val. lst1) .eqi. 1, "test0020-0110 failed")
    call check (cadr (.val. lst1) .eqi. 2, "test0020-0120 failed")
    call check (caddr (.val. lst1) .eqi. 3, "test0020-0130 failed")
    call check (cadddr (.val. lst1) .eqi. 4, "test0020-0140 failed")
    call check (is_proper_list (.val. lst1), "test0020-0200 failed")
    call check (.not. is_dotted_list (.val. lst1), "test0020-0210 failed")
    call check (.not. is_circular_list (.val. lst1), "test0020-0220 failed")

    call check (is_dotted_list (1), "test0020-0300 failed")
    call check (is_dotted_list (cons (1, 2)), "test0020-0310 failed")
    call check (is_dotted_list (1 ** 2 ** 3 ** cons (4, 5)), "test0020-0320 failed")
    call check (.not. is_proper_list (1), "test0020-0330 failed")
    call check (.not. is_proper_list (cons (1, 2)), "test0020-0340 failed")
    call check (.not. is_proper_list (1 ** 2 ** 3 ** cons (4, 5)), "test0020-0350 failed")
    call check (.not. is_circular_list (1), "test0020-0360 failed")
    call check (.not. is_circular_list (cons (1, 2)), "test0020-0370 failed")
    call check (.not. is_circular_list (1 ** 2 ** 3 ** cons (4, 5)), "test0020-0380 failed")

    automatic_garbage_collection = agc_save
  end subroutine test0020

  subroutine test0030
    type(gcroot_t) :: tree
    logical :: agc_save
    integer :: leaf

    agc_save = automatic_garbage_collection
    automatic_garbage_collection = .true.

    heap_size_limit = 1

    tree = build_tree (1, 1, 0)
    leaf = integer_cast (car (.val. tree))
    call check (leaf == 0, "test0030-1-0 failed")
    leaf = integer_cast (cdr (.val. tree))
    call check (leaf == 1, "test0030-1-1 failed")
    tree = build_tree (1, 2, 0)
    leaf = integer_cast (caar (.val. tree))
    call check (leaf == 0, "test0030-2-0 failed")
    leaf = integer_cast (cdar (.val. tree))
    call check (leaf == 1, "test0030-2-1 failed")
    leaf = integer_cast (cadr (.val. tree))
    call check (leaf == 2, "test0030-2-2 failed")
    leaf = integer_cast (cddr (.val. tree))
    call check (leaf == 3, "test0030-2-3 failed")
    tree = build_tree (1, 3, 0)
    leaf = integer_cast (caaar (.val. tree))
    call check (leaf == 0, "test0030-3-0 failed")
    leaf = integer_cast (cdaar (.val. tree))
    call check (leaf == 1, "test0030-3-1 failed")
    leaf = integer_cast (cadar (.val. tree))
    call check (leaf == 2, "test0030-3-2 failed")
    leaf = integer_cast (cddar (.val. tree))
    call check (leaf == 3, "test0030-3-3 failed")
    leaf = integer_cast (caadr (.val. tree))
    call check (leaf == 4, "test0030-3-4 failed")
    leaf = integer_cast (cdadr (.val. tree))
    call check (leaf == 5, "test0030-3-5 failed")
    leaf = integer_cast (caddr (.val. tree))
    call check (leaf == 6, "test0030-3-6 failed")
    leaf = integer_cast (cdddr (.val. tree))
    call check (leaf == 7, "test0030-3-7 failed")
    tree = build_tree (1, 4, 0)
    leaf = integer_cast (caaaar (.val. tree))
    call check (leaf == 0, "test0030-4-0 failed")
    leaf = integer_cast (cdaaar (.val. tree))
    call check (leaf == 1, "test0030-4-1 failed")
    leaf = integer_cast (cadaar (.val. tree))
    call check (leaf == 2, "test0030-4-2 failed")
    leaf = integer_cast (cddaar (.val. tree))
    call check (leaf == 3, "test0030-4-3 failed")
    leaf = integer_cast (caadar (.val. tree))
    call check (leaf == 4, "test0030-4-4 failed")
    leaf = integer_cast (cdadar (.val. tree))
    call check (leaf == 5, "test0030-4-5 failed")
    leaf = integer_cast (caddar (.val. tree))
    call check (leaf == 6, "test0030-4-6 failed")
    leaf = integer_cast (cdddar (.val. tree))
    call check (leaf == 7, "test0030-4-7 failed")
    leaf = integer_cast (caaadr (.val. tree))
    call check (leaf == 8, "test0030-4-8 failed")
    leaf = integer_cast (cdaadr (.val. tree))
    call check (leaf == 9, "test0030-4-9 failed")
    leaf = integer_cast (cadadr (.val. tree))
    call check (leaf == 10, "test0030-4-10 failed")
    leaf = integer_cast (cddadr (.val. tree))
    call check (leaf == 11, "test0030-4-11 failed")
    leaf = integer_cast (caaddr (.val. tree))
    call check (leaf == 12, "test0030-4-12 failed")
    leaf = integer_cast (cdaddr (.val. tree))
    call check (leaf == 13, "test0030-4-13 failed")
    leaf = integer_cast (cadddr (.val. tree))
    call check (leaf == 14, "test0030-4-14 failed")
    leaf = integer_cast (cddddr (.val. tree))
    call check (leaf == 15, "test0030-4-15 failed")

    automatic_garbage_collection = agc_save

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
    call check (car (.val. lst) .eqi. 123, "test0040-0010 failed")
    call check (is_nil (cdr (.val. lst)), "test0040-0020 failed")
    call unlist1 (.val. lst, obj1)
    call check (obj1 .eqi. 123, "test0040-0030 failed")
    call unlist1_with_tail (.val. lst, obj1, tail)
    call check (obj1 .eqi. 123, "test0040-0040 failed")
    call check (is_nil (tail), "test0040-0050 failed")

    lst = list5 (1, 2, 3, 4, 5)
    call check (first (.val. lst) .eqi. 1, "test0040-0110 failed")
    call check (second (.val. lst) .eqi. 2, "test0040-0120 failed")
    call check (third (.val. lst) .eqi. 3, "test0040-0130 failed")
    call check (fourth (.val. lst) .eqi. 4, "test0040-0140 failed")
    call check (fifth (.val. lst) .eqi. 5, "test0040-0150 failed")
    call check (is_nil (cddr (cdddr (.val. lst))), "test0040-0160 failed")
    call unlist5 (.val. lst, obj1, obj2, obj3, obj4, obj5)
    call check (obj1 .eqi. 1, "test0040-0210 failed")
    call check (obj2 .eqi. 2, "test0040-0220 failed")
    call check (obj3 .eqi. 3, "test0040-0230 failed")
    call check (obj4 .eqi. 4, "test0040-0240 failed")
    call check (obj5 .eqi. 5, "test0040-0250 failed")
    call unlist5_with_tail (.val. lst, obj1, obj2, obj3, obj4, obj5, tail)
    call check (obj1 .eqi. 1, "test0040-0310 failed")
    call check (obj2 .eqi. 2, "test0040-0320 failed")
    call check (obj3 .eqi. 3, "test0040-0330 failed")
    call check (obj4 .eqi. 4, "test0040-0340 failed")
    call check (obj5 .eqi. 5, "test0040-0350 failed")
    call check (is_nil (tail), "test0040-0360 failed")
    call unlist4_with_tail (.val. lst, obj1, obj2, obj3, obj4, tail)
    call check (obj1 .eqi. 1, "test0040-0410 failed")
    call check (obj2 .eqi. 2, "test0040-0420 failed")
    call check (obj3 .eqi. 3, "test0040-0430 failed")
    call check (obj4 .eqi. 4, "test0040-0440 failed")
    call check (car (tail) .eqi. 5, "test0040-0450 failed")
    call check (is_nil (cdr (tail)), "test0040-0460 failed")
  end subroutine test0040

  subroutine test0050
    type(gcroot_t) :: tail

    call check (lists_are_equal (integer_eq, nil, nil), "test0050-0010 failed")
    call check (lists_are_equal (integer_eq, list1 (1), list1 (1)), "test0050-0020 failed")
    call check (lists_are_equal (integer_eq, list2 (1, 2), list2 (1, 2)), "test0050-0030 failed")
    call check (lists_are_equal (integer_eq, list5 (1, 2, 3, 4, 5), list5 (1, 2, 3, 4, 5)), "test0050-0040 failed")

    call check (.not. lists_are_equal (integer_eq, nil, list1 (1)), "test0050-0110 failed")
    call check (.not. lists_are_equal (integer_eq, list1 (1), nil), "test0050-0120 failed")
    call check (.not. lists_are_equal (integer_eq, list4 (1, 2, 3, 4), list5 (1, 2, 3, 4, 5)), "test0050-0130 failed")
    call check (.not. lists_are_equal (integer_eq, list5 (1, 2, 3, 4, 5), list4 (1, 2, 3, 4)), "test0050-0140 failed")
    call check (.not. lists_are_equal (integer_eq, list5 (1, 2, 3, 4, 5), list5 (1, 2, 3, 4, 6)), "test0050-0150 failed")

    ! Check some lists with shared tails.
    tail = list3 (3, 2, 1)
    call check (lists_are_equal (integer_eq, .val. tail, .val. tail), "test0050-0200 failed")
    call check (lists_are_equal (integer_eq, 5 ** cons (4, .val. tail), 5 ** cons (4, .val. tail)), "test0050-0210 failed")
    call check (.not. lists_are_equal (integer_eq, 5 ** cons (4, .val. tail), 5 ** cons (40, .val. tail)), "test0050-0220 failed")
  end subroutine test0050

  subroutine test0060

    ! take and drop

    type(gcroot_t) :: lst1, lst2, lst3

    lst1 = list1 (1)
    lst2 = list2 (1, 2)
    lst3 = list3 (1, 2, 3)

    call check (lists_are_equal (integer_eq, take (nil, 0_sz), nil), "test0060-0010 failed")
    call check (lists_are_equal (integer_eq, take (.val. lst1, 0_sz), nil), "test0060-0020 failed")
    call check (lists_are_equal (integer_eq, take (.val. lst3, 0_sz), nil), "test0060-0030 failed")

    call check (lists_are_equal (integer_eq, take (.val. lst1, 1_sz), .val. lst1), "test0060-0040 failed")
    call check (lists_are_equal (integer_eq, take (.val. lst3, 1_sz), .val. lst1), "test0060-0050 failed")
    call check (lists_are_equal (integer_eq, take (.val. lst3, 2_sz), .val. lst2), "test0060-0060 failed")
    call check (lists_are_equal (integer_eq, take (.val. lst3, 3_sz), .val. lst3), "test0060-0070 failed")
  end subroutine test0060

  subroutine test0065

    ! take and drop

    type(cons_t) :: lst1, lst2, lst3

    lst1 = list1 (1)
    lst2 = list2 (1, 2)
    lst3 = list3 (1, 2, 3)

    call check (lists_are_equal (integer_eq, take (nil, 0_sz), nil), "test0065-0010 failed")
    call check (lists_are_equal (integer_eq, take (lst1, 0_sz), nil), "test0065-0020 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 0_sz), nil), "test0065-0030 failed")

    call check (lists_are_equal (integer_eq, take (lst1, 1_sz), lst1), "test0065-0040 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 1_sz), lst1), "test0065-0050 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 2_sz), lst2), "test0065-0060 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 3_sz), lst3), "test0065-0070 failed")
  end subroutine test0065

  subroutine test0070

    ! iota and list_ref

    type(gcroot_t) :: lst
    integer(sz) :: n
    integer(sz) :: i

    do n = 0_sz, 1000_sz, 100_sz

       lst = iota (n)
       do i = 0_sz, n - 1_sz, 100_sz
          call check (list_ref0 (.val. lst, i) .eqsz. i, "test0070-0010 failed")
          call check (list_ref1 (.val. lst, i + 1) .eqsz. i, "test0070-0020 failed")
          call check (list_refn (.val. lst, -50_sz, i - 50) .eqsz. i, "test0070-0030 failed")
       end do

       lst = iota (n, 1_sz)
       do i = 0_sz, n - 1_sz, 100_sz
          call check (list_ref0 (.val. lst, i) .eqsz. i + 1, "test0070-0040 failed")
          call check (list_ref1 (.val. lst, i + 1) .eqsz. i + 1, "test0070-0050 failed")
          call check (list_refn (.val. lst, -50_sz, i - 50) .eqsz. i + 1, "test0070-0060 failed")
       end do

       lst = iota (n, 100_sz, -10_sz)
       do i = 0_sz, n - 1_sz, 100_sz
          call check (list_ref0 (.val. lst, i) .eqsz. 100 - (10 * i), "test0070-0070 failed")
          call check (list_ref1 (.val. lst, i + 1) .eqsz. 100 - (10 * i), "test0070-0080 failed")
          call check (list_refn (.val. lst, -50_sz, i - 50) .eqsz. 100 - (10 * i), "test0070-0090 failed")
       end do

    end do
  end subroutine test0070

  subroutine run_tests
    call test0010
    call test0020
    call test0030
    call test0040
    call test0050
    call test0060
    call test0065
    call test0070
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
