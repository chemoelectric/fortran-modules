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
    logical :: agc_save

    agc_save = automatic_garbage_collection
    automatic_garbage_collection = .true.

    heap_size_limit = 1

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

    automatic_garbage_collection = agc_save
  end subroutine test0020

  subroutine test0030
    type(gcroot_t) :: tree
    logical :: agc_save
    integer :: leaf

    agc_save = automatic_garbage_collection
    automatic_garbage_collection = .true.

    heap_size_limit = 1

m4_forloop([_i],1,CADADR_MAX,[dnl
    tree = build_tree (1, _i, 0)
m4_forloop([_k],0,m4_eval([(1 << (]_i[)) - 1]),[dnl
    leaf = integer_cast ([c]m4_bits_to_ad_sequence(_i,_k)[r] (tree))
    call check (leaf == _k, "test0030-_i-_k failed")
])dnl
])dnl

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
    call check (lists_are_equal (integer_eq, tail, tail), "test0050-0200 failed")
    call check (lists_are_equal (integer_eq, 5 ** cons (4, tail), 5 ** cons (4, tail)), "test0050-0210 failed")
    call check (.not. lists_are_equal (integer_eq, 5 ** cons (4, tail), 5 ** cons (40, tail)), "test0050-0220 failed")
  end subroutine test0050

  subroutine test0060

    ! take and drop

    type(gcroot_t) :: lst1, lst2, lst3

    lst1 = list1 (1)
    lst2 = list2 (1, 2)
    lst3 = list3 (1, 2, 3)

    call check (lists_are_equal (integer_eq, take (nil, 0_sz), nil), "test0060-0010 failed")
    call check (lists_are_equal (integer_eq, take (lst1, 0_sz), nil), "test0060-0020 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 0_sz), nil), "test0060-0030 failed")

    call check (lists_are_equal (integer_eq, take (lst1, 1_sz), lst1), "test0060-0040 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 1_sz), lst1), "test0060-0050 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 2_sz), lst2), "test0060-0060 failed")
    call check (lists_are_equal (integer_eq, take (lst3, 3_sz), lst3), "test0060-0070 failed")
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

  subroutine test0080
    call check (lists_are_equal (integer_eq, make_list (0_sz, 5), nil), "test0080-0010 failed")
    call check (lists_are_equal (integer_eq, make_list (1_sz, 5), 5 ** nil), "test0080-0020 failed")
    call check (lists_are_equal (integer_eq, make_list (5_sz, 5), list5 (5, 5, 5, 5, 5)), "test0080-0030 failed")
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
    call check (lists_are_equal (integer_eq, reverse (nil), nil), "test0100-0010 failed")
    call check (lists_are_equal (integer_eq, reverse (123 ** nil), 123 ** nil), "test0100-0020 failed")
    call check (lists_are_equal (integer_eq, reverse (list5 (1, 2, 3, 4, 5)), list5 (5, 4, 3, 2, 1)), "test0100-0030 failed")

    call check (lists_are_equal (integer_eq, reversex (nil), nil), "test0100-0040 failed")
    call check (lists_are_equal (integer_eq, reversex (123 ** nil), 123 ** nil), "test0100-0050 failed")
    call check (lists_are_equal (integer_eq, reversex (list5 (1, 2, 3, 4, 5)), list5 (5, 4, 3, 2, 1)), "test0100-0060 failed")
  end subroutine test0100

  subroutine test0110
    call check (lists_are_equal (integer_eq, take (circular_list (1 ** nil), 5_sz), list5 (1, 1, 1, 1, 1)), &
         "test0110-0010 failed")
    call check (lists_are_equal (integer_eq, &
         take (circular_list (1 ** 2 ** 3 ** nil), 10_sz), &
         1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** nil), &
         "test0110-0020 failed")

    call check (lists_are_equal (integer_eq, take (circular_listx (1 ** nil), 5_sz), list5 (1, 1, 1, 1, 1)), &
         "test0110-0030 failed")
    call check (lists_are_equal (integer_eq, &
         take (circular_listx (1 ** 2 ** 3 ** nil), 10_sz), &
         1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** 2 ** 3 ** 1 ** nil), &
         "test0110-0040 failed")
  end subroutine test0110

  subroutine test0120
    call check (last (1 ** nil) .eqi. 1, "test0120-0010 failed")
    call check (last (1 ** 2 ** 3 ** nil) .eqi. 3, "test0120-0020 failed")
  end subroutine test0120

  subroutine run_tests
    call test0010
    call test0020
    call test0030
    call test0040
    call test0050
    call test0060
    call test0065
    call test0070
    call test0080
    call test0090
    call test0100
    call test0110
    call test0120
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
