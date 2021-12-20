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

  interface operator(.eqi.)
     module procedure integer_eq
  end interface operator(.eqi.)

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

  function real_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = real_cast (obj1) == real_cast (obj2)
  end function real_eq

  subroutine test001
    type(gcroot_t) :: cons1
    class(*), allocatable :: car1, cdr1

    call check (car (cons (123, 456)) .eqi. 123, "test001-0010 failed")
    call check (cdr (cons (123, 456)) .eqi. 456, "test001-0020 failed")

    cons1 = cons (123.0, 456.0)
    call check (car (cons1) .eqr. 123.0, "test001-0030 failed")
    call check (cdr (cons1) .eqr. 456.0, "test001-0040 failed")
    call uncons (cons1, car1, cdr1)
    call check (car1 .eqr. 123.0, "test001-0050 failed")
    call check (cdr1 .eqr. 456.0, "test001-0060 failed")

    call check (is_pair (cons (123, 456)), "test001-0070 failed")
    call check (is_pair (cons1), "test001-0080 failed")

    call check (.not. is_not_pair (cons (123, 456)), "test001-0090 failed")
    call check (.not. is_not_pair (cons1), "test001-0100 failed")

    call check (.not. pair_t_eq (cons (123, 456), cons (123, 456)), "test001-0110 failed")
    call check (pair_t_eq (cons1, cons1), "test001-0120 failed")

    call check (.not. is_nil (cons (123, 456)), "test001-0130 failed")
    call check (.not. is_nil (cons1), "test001-0140 failed")

    call check (is_not_nil (cons (123, 456)), "test001-0150 failed")
    call check (is_not_nil (cons1), "test001-0160 failed")

    call check (.not. is_nil_list (cons (123, 456)), "test001-0170 failed")
    call check (.not. is_nil_list (cons1), "test001-0180 failed")

    call check (is_nil (nil), "test001-0190 failed")
    call check (.not. is_not_nil (nil), "test001-0200 failed")
    call check (is_nil_list (nil), "test001-0210 failed")
  end subroutine test001

  subroutine test002
    type(gcroot_t) :: lst1
    logical :: agc_save

    agc_save = automatic_garbage_collection
    automatic_garbage_collection = .true.

    heap_size_limit = 1

    lst1 = 1 ** 2 ** 3 ** 4 ** 5 ** 6 ** 7 ** 8 ** 9 ** 10 ** nil
    call check (first (lst1) .eqi. 1, "test002-0010 failed")
    call check (second (lst1) .eqi. 2, "test002-0020 failed")
    call check (third (lst1) .eqi. 3, "test002-0030 failed")
    call check (fourth (lst1) .eqi. 4, "test002-0040 failed")
    call check (fifth (lst1) .eqi. 5, "test002-0050 failed")
    call check (sixth (lst1) .eqi. 6, "test002-0060 failed")
    call check (seventh (lst1) .eqi. 7, "test002-0070 failed")
    call check (eighth (lst1) .eqi. 8, "test002-0080 failed")
    call check (ninth (lst1) .eqi. 9, "test002-0090 failed")
    call check (tenth (lst1) .eqi. 10, "test002-0100 failed")
    call check (car (lst1) .eqi. 1, "test002-0110 failed")
    call check (cadr (lst1) .eqi. 2, "test002-0120 failed")
    call check (caddr (lst1) .eqi. 3, "test002-0130 failed")
    call check (cadddr (lst1) .eqi. 4, "test002-0140 failed")
    call check (is_proper_list (lst1), "test002-0200 failed")
    call check (.not. is_dotted_list (lst1), "test002-0210 failed")
    call check (.not. is_circular_list (lst1), "test002-0220 failed")

    call check (is_dotted_list (1), "test002-0300 failed")
    call check (is_dotted_list (cons (1, 2)), "test002-0310 failed")
    call check (is_dotted_list (1 ** 2 ** 3 ** cons (4, 5)), "test002-0320 failed")
    call check (.not. is_proper_list (1), "test002-0330 failed")
    call check (.not. is_proper_list (cons (1, 2)), "test002-0340 failed")
    call check (.not. is_proper_list (1 ** 2 ** 3 ** cons (4, 5)), "test002-0350 failed")
    call check (.not. is_circular_list (1), "test002-0360 failed")
    call check (.not. is_circular_list (cons (1, 2)), "test002-0370 failed")
    call check (.not. is_circular_list (1 ** 2 ** 3 ** cons (4, 5)), "test002-0380 failed")

    automatic_garbage_collection = agc_save
  end subroutine test002

  subroutine test003
    type(gcroot_t) :: tree
    logical :: agc_save
    integer :: leaf

    agc_save = automatic_garbage_collection
    automatic_garbage_collection = .true.

    heap_size_limit = 1

    tree = build_tree (1, 1, 0)
    leaf = integer_cast (car (tree))
    call check (leaf == 0, "test003-1-0 failed")
    leaf = integer_cast (cdr (tree))
    call check (leaf == 1, "test003-1-1 failed")
    tree = build_tree (1, 2, 0)
    leaf = integer_cast (caar (tree))
    call check (leaf == 0, "test003-2-0 failed")
    leaf = integer_cast (cdar (tree))
    call check (leaf == 1, "test003-2-1 failed")
    leaf = integer_cast (cadr (tree))
    call check (leaf == 2, "test003-2-2 failed")
    leaf = integer_cast (cddr (tree))
    call check (leaf == 3, "test003-2-3 failed")
    tree = build_tree (1, 3, 0)
    leaf = integer_cast (caaar (tree))
    call check (leaf == 0, "test003-3-0 failed")
    leaf = integer_cast (cdaar (tree))
    call check (leaf == 1, "test003-3-1 failed")
    leaf = integer_cast (cadar (tree))
    call check (leaf == 2, "test003-3-2 failed")
    leaf = integer_cast (cddar (tree))
    call check (leaf == 3, "test003-3-3 failed")
    leaf = integer_cast (caadr (tree))
    call check (leaf == 4, "test003-3-4 failed")
    leaf = integer_cast (cdadr (tree))
    call check (leaf == 5, "test003-3-5 failed")
    leaf = integer_cast (caddr (tree))
    call check (leaf == 6, "test003-3-6 failed")
    leaf = integer_cast (cdddr (tree))
    call check (leaf == 7, "test003-3-7 failed")
    tree = build_tree (1, 4, 0)
    leaf = integer_cast (caaaar (tree))
    call check (leaf == 0, "test003-4-0 failed")
    leaf = integer_cast (cdaaar (tree))
    call check (leaf == 1, "test003-4-1 failed")
    leaf = integer_cast (cadaar (tree))
    call check (leaf == 2, "test003-4-2 failed")
    leaf = integer_cast (cddaar (tree))
    call check (leaf == 3, "test003-4-3 failed")
    leaf = integer_cast (caadar (tree))
    call check (leaf == 4, "test003-4-4 failed")
    leaf = integer_cast (cdadar (tree))
    call check (leaf == 5, "test003-4-5 failed")
    leaf = integer_cast (caddar (tree))
    call check (leaf == 6, "test003-4-6 failed")
    leaf = integer_cast (cdddar (tree))
    call check (leaf == 7, "test003-4-7 failed")
    leaf = integer_cast (caaadr (tree))
    call check (leaf == 8, "test003-4-8 failed")
    leaf = integer_cast (cdaadr (tree))
    call check (leaf == 9, "test003-4-9 failed")
    leaf = integer_cast (cadadr (tree))
    call check (leaf == 10, "test003-4-10 failed")
    leaf = integer_cast (cddadr (tree))
    call check (leaf == 11, "test003-4-11 failed")
    leaf = integer_cast (caaddr (tree))
    call check (leaf == 12, "test003-4-12 failed")
    leaf = integer_cast (cdaddr (tree))
    call check (leaf == 13, "test003-4-13 failed")
    leaf = integer_cast (cadddr (tree))
    call check (leaf == 14, "test003-4-14 failed")
    leaf = integer_cast (cddddr (tree))
    call check (leaf == 15, "test003-4-15 failed")

    automatic_garbage_collection = agc_save

  contains

    recursive function build_tree (m, n, k) result (tree)
      integer, intent(in) :: m, n, k
      class(pair_t), allocatable :: tree

      if (m == n) then
         tree = cons (k, k + 1)
      else
         tree = cons (build_tree (m + 1, n, k), build_tree (m + 1, n, k + 2 ** (n - m)))
      end if
    end function build_tree

  end subroutine test003

  subroutine run_tests
    call test001
    call test002
    call test003
    call collect_garbage_now
    call check (current_heap_size () == 0, "run_tests-0100 failed")
    call check (current_roots_count () == 0, "run_tests-0110 failed")
  end subroutine run_tests

end module test__cons_pairs

program main
  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: test__cons_pairs

  implicit none

  call run_tests

end program main
