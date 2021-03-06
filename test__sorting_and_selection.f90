! -*- F90 -*- 
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

module test__sorting_and_selection

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: vectars
  use, non_intrinsic :: sorting_and_selection

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

  interface operator(.eqr.)
     module procedure real_eq
  end interface operator(.eqr.)

contains

  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("test__sorting_and_selection error: ", a)') msg
    error stop
  end subroutine error_abort

  subroutine check (boolean, msg)
    logical, intent(in) :: boolean
    character(*), intent(in) :: msg
    if (.not. boolean) call error_abort (msg)
  end subroutine check

  recursive function lstsort (less_than, lst) result (lst_ss)
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    lst_ss = stable_lstsort (less_than, lst)
  end function lstsort

  recursive function stable_lstsort (less_than, lst) result (lst_ss)
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    type(gcroot_t) :: lst_root

    lst_root = lst
    lst_ss = stable_lstsortx (less_than, list_copy (lst))
    call lst_root%discard
  end function stable_lstsort

  recursive function stable_lstsortx (less_than, lst) result (lst_ss)
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    type(gcroot_t) :: p

    p = lst
    if (is_not_pair (p)) then
       ! List of length zero.
       lst_ss = p
    else if (is_not_pair (cdr (p))) then
       ! List of length one.
       lst_ss = p
    else
       lst_ss = merge_sort (p, length (p))
    end if

  contains

    recursive function merge_sort (p, n) result (lst_ss)
      !
      ! A top-down merge sort using non-tail recursion.
      !
      type(gcroot_t), intent(in) :: p
      integer(sz), intent(in) :: n
      type(cons_t) :: lst_ss

      integer(sz) :: n_half
      type(cons_t) :: p_left
      class(*), allocatable :: p_right
      type(gcroot_t) :: p_left1
      type(gcroot_t) :: p_right1

      if (n == 1) then
         lst_ss = p
      else
         n_half = n / 2
         call do_split_atx (p, n_half, p_left, p_right)
         p_left1 = p_left
         p_right1 = p_right
         p_left1 = merge_sort (p_left1, n_half)
         p_right1 = merge_sort (p_right1, n - n_half)
         lst_ss = lstmergex (less_than, p_left1, p_right1)
      end if
    end function merge_sort

  end function stable_lstsortx

  recursive function lstmergex (less_than, lst1, lst2) result (lst_m)
    !
    ! It is assumed lst1 and lst2 are proper lists.
    !
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_m

    type(gcroot_t) :: p1
    type(gcroot_t) :: p2

    p1 = lst1
    p2 = lst2
    lst_m = merge_lists (p1, p2)

  contains

    recursive function merge_lists (p1, p2) result (lst_m)
      type(gcroot_t) :: p1
      type(gcroot_t) :: p2
      type(cons_t) :: lst_m

      type(gcroot_t) :: lst_m_root
      class(*), allocatable :: hd1, tl1
      class(*), allocatable :: hd2, tl2
      type(cons_t) :: cursor
      logical :: p1_is_active
      logical :: p1_is_active_is_changed
      logical :: done

      if (is_not_pair (p1)) then
         lst_m = p2
      else if (is_not_pair (p2)) then
         lst_m = p1
      else
         call uncons (p1, hd1, tl1)
         call uncons (p2, hd2, tl2)
         if (.not. less_than (hd2, hd1)) then
            p1_is_active = .true.
            cursor = p1
            p1 = tl1
         else
            p1_is_active = .false.
            cursor = p2
            p2 = tl2
         end if
         lst_m = cursor
         lst_m_root = lst_m
         done = .false.
         do while (.not. done)
            if (p1_is_active) then
               p1_is_active_is_changed = .false.
               do while (.not. p1_is_active_is_changed)
                  if (is_not_pair (p1)) then
                     call set_cdr (cursor, p2)
                     p1_is_active_is_changed = .true.
                     done = .true.
                  else
                     call uncons (p1, hd1, tl1)
                     call uncons (p2, hd2, tl2)
                     if (.not. less_than (hd2, hd1)) then
                        cursor = p1
                        p1 = tl1
                     else
                        call set_cdr (cursor, p2)
                        p1_is_active = .false.
                        p1_is_active_is_changed = .true.
                        cursor = p2
                        p2 = tl2
                     end if
                  end if
               end do
            else
               p1_is_active_is_changed = .false.
               do while (.not. p1_is_active_is_changed)
                  if (is_not_pair (p2)) then
                     call set_cdr (cursor, p1)
                     p1_is_active_is_changed = .true.
                     done = .true.
                  else
                     call uncons (p1, hd1, tl1)
                     call uncons (p2, hd2, tl2)
                     if (.not. less_than (hd2, hd1)) then
                        call set_cdr (cursor, p1)
                        p1_is_active = .true.
                        p1_is_active_is_changed = .true.
                        cursor = p1
                        p1 =  tl1
                     else
                        call set_cdr (cursor, p2)
                        cursor = p2
                        p2 =  tl2
                     end if
                  end if
               end do
            end if
         end do
      end if
      call lst_m_root%discard
    end function merge_lists

  end function lstmergex

  function vecsort (less_than, vec) result (vec_s)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_s

    vec_s = list_to_vectar (lstsort (less_than, vectar_to_list (vec)))
  end function vecsort

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

  function real_cast (obj) result (val)
    class(*), intent(in) :: obj
    real :: val
    select type (obj)
    type is (real)
       val = obj
    class default
       call error_abort ("real_cast of an incompatible object")
    end select
  end function real_cast

  function real_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1, obj2
    logical :: bool
    bool = real_cast (obj1) == real_cast (obj2)
  end function real_eq

  subroutine test0010
    !
    ! Tests of stable merge.
    !
    type(cons_t) :: lst1, lst2, lst3

    lst1 = list (2, 3, 5, 6, 10)
    lst2 = list (2, 4, 4, 5, 15)
    lst3 = list_mergex (less_than, lst1, lst2)
    call check (list_equal (int_eq, lst3, list10 (2, 2, 3, 4, 4, 5, 5, 6, 10, 15)), "test0010-0010 failed")

    lst1 = list (2, 3, 5, 6, 10)
    lst2 = list (2, 4, 4, 5, 15)
    lst3 = list_merge (less_than, lst1, lst2)
    call check (list_equal (int_eq, lst1, list (2, 3, 5, 6, 10)), "test0010-0020 failed")
    call check (list_equal (int_eq, lst2, list (2, 4, 4, 5, 15)), "test0010-0030 failed")
    call check (list_equal (int_eq, lst3, list (2, 2, 3, 4, 4, 5, 5, 6, 10, 15)), "test0010-0040 failed")

    lst1 = list (22, 31, 53, 61)
    lst2 = list (21, 42, 41, 52, 51)
    lst3 = list_mergex (is_lt_except_ones, lst1, lst2)
    call check (list_equal (int_eq, lst3, list (22, 21, 31, 42, 41, 53, 52, 51, 61)), "test0010-0050 failed")

    lst1 = list (22, 31, 53, 61)
    lst2 = list (21, 42, 41, 52, 51)
    lst3 = list_merge (is_lt_except_ones, lst1, lst2)
    call check (list_equal (int_eq, lst1, list (22, 31, 53, 61)), "test0010-0060 failed")
    call check (list_equal (int_eq, lst2, list (21, 42, 41, 52, 51)), "test0010-0070 failed")
    call check (list_equal (int_eq, lst3, list (22, 21, 31, 42, 41, 53, 52, 51, 61)), "test0010-0080 failed")

    lst3 = list_merge (less_than, list (), list ())
    call check (is_nil (lst3), "test0010-1010 failed")

    lst3 = list_merge (less_than, list (1, 2, 3), list ())
    call check (list_equal (int_eq, lst3, list (1, 2, 3)), "test0010-1020 failed")

    lst3 = list_merge (less_than, list (), list (1, 2, 3))
    call check (list_equal (int_eq, lst3, list (1, 2, 3)), "test0010-1020 failed")

    lst3 = list_merge (less_than, list (2, 4, 6), list (1, 3, 5))
    call check (list_equal (int_eq, lst3, list (1, 2, 3, 4, 5, 6)), "test0010-1030 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

    function is_lt_except_ones (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      integer :: x1
      integer :: y1

      call collect_garbage_now

      x1 = int_cast (x)
      x1 = x1 - mod (x1, 10)

      y1 = int_cast (y)
      y1 = y1 - mod (y1, 10)

      bool = (x1 < y1)
    end function is_lt_except_ones

  end subroutine test0010

  subroutine test0020
    !
    ! Tests of sorting.
    !
    type(cons_t) :: lst1, lst2
    type(gcroot_t) :: p
    integer :: i, k

    lst1 = iota (100, 100, -1)
    lst2 = list_sortx (less_than, lst1)
    call check (list_equal (int_eq, lst2, iota (100, 1)), "test0020-0010 failed")

    lst1 = iota (100, 100, -1)
    lst2 = list_sort (less_than, lst1)
    call check (list_equal (int_eq, lst1, iota (100, 100, -1)), "test0020-0020 failed")
    call check (list_equal (int_eq, lst2, iota (100, 1)), "test0020-0030 failed")

    lst1 = iota (100, 100, -1)
    lst2 = list_stable_sortx (less_than, lst1)
    call check (list_equal (int_eq, lst2, iota (100, 1)), "test0020-0110 failed")

    lst1 = iota (100, 100, -1)
    lst2 = list_stable_sort (less_than, lst1)
    call check (list_equal (int_eq, lst1, iota (100, 100, -1)), "test0020-0120 failed")
    call check (list_equal (int_eq, lst2, iota (100, 1)), "test0020-0130 failed")

    lst1 = make_0_to_99_strangely_ordered ()
    lst2 = list_sort (less_than, lst1)
    call check (list_equal (int_eq, lst1, make_0_to_99_strangely_ordered ()), "test0020-0200 failed")
    call check (list_equal (int_eq, lst2, iota (100)), "test0020-0210 failed")

    lst1 = make_0_to_99_strangely_ordered ()
    lst2 = list_sortx (less_than, lst1)
    call check (list_equal (int_eq, lst2, iota (100)), "test0020-0220 failed")

    lst1 = make_0_to_99_strangely_ordered ()
    lst2 = list_stable_sort (less_than, lst1)
    call check (list_equal (int_eq, lst1, make_0_to_99_strangely_ordered ()), "test0020-0230 failed")
    call check (list_equal (int_eq, lst2, iota (100)), "test0020-0240 failed")

    lst1 = make_0_to_99_strangely_ordered ()
    lst2 = list_stable_sortx (less_than, lst1)
    call check (list_equal (int_eq, lst2, iota (100)), "test0020-0250 failed")
    
    lst1 = make_0_to_99_strangely_ordered ()
    lst2 = list_stable_sort (is_lt_only_ones, lst1)
    call check (list_equal (int_eq, lst1, make_0_to_99_strangely_ordered ()), "test0020-0300 failed")
    p = lst2
    i = 0
    do while (is_pair (p))
       k = int_cast (car (p))
       call check (mod (k, 10) == i / 10, "test0020-0310 failed")
       call check (k / 10 == mod (i, 10), "test0020-0320 failed")
       i = i + 1
       p = cdr (p)
    end do
    call check (i == 100, "test0020-0330 failed")

    call check (is_nil (list_sort (less_than, nil)), "test0020-0410 failed")
    call check (is_nil (list_sortx (less_than, nil)), "test0020-0420 failed")
    call check (is_nil (list_stable_sort (less_than, nil)), "test0020-0430 failed")
    call check (is_nil (list_stable_sortx (less_than, nil)), "test0020-0440 failed")

    call check (list_equal (int_eq, list_sort (less_than, list (123)), list (123)), "test0020-0510 failed")
    call check (list_equal (int_eq, list_sortx (less_than, list (123)), list (123)), "test0020-0520 failed")
    call check (list_equal (int_eq, list_stable_sort (less_than, list (123)), list (123)), "test0020-0530 failed")
    call check (list_equal (int_eq, list_stable_sortx (less_than, list (123)), list (123)), "test0020-0540 failed")

    call check (list_equal (int_eq, list_sort (less_than, list (1, 2)), list (1, 2)), "test0020-0610 failed")
    call check (list_equal (int_eq, list_sortx (less_than, list (1, 2)), list (1, 2)), "test0020-0620 failed")
    call check (list_equal (int_eq, list_stable_sort (less_than, list (1, 2)), list (1, 2)), "test0020-0630 failed")
    call check (list_equal (int_eq, list_stable_sortx (less_than, list (1, 2)), list (1, 2)), "test0020-0640 failed")

    call check (list_equal (int_eq, list_sort (less_than, list (2, 1)), list (1, 2)), "test0020-0710 failed")
    call check (list_equal (int_eq, list_sortx (less_than, list (2, 1)), list (1, 2)), "test0020-0720 failed")
    call check (list_equal (int_eq, list_stable_sort (less_than, list (2, 1)), list (1, 2)), "test0020-0730 failed")
    call check (list_equal (int_eq, list_stable_sortx (less_than, list (2, 1)), list (1, 2)), "test0020-0740 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

    function is_lt_only_ones (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      integer :: x1
      integer :: y1

      call collect_garbage_now

      x1 = int_cast (x)
      x1 = mod (x1, 10)

      y1 = int_cast (y)
      y1 = mod (y1, 10)

      bool = (x1 < y1)
    end function is_lt_only_ones

    function make_0_to_99_strangely_ordered () result (lst)
      type(cons_t) :: lst

      lst = iota (10, 99, -1)
      lst = append (iota (10, 89, -1), lst)
      lst = append (iota (10, 79, -1), lst)
      lst = append (iota (10, 69, -1), lst)
      lst = append (iota (10, 59, -1), lst)
      lst = append (iota (10, 49, -1), lst)
      lst = append (iota (10, 39, -1), lst)
      lst = append (iota (10, 29, -1), lst)
      lst = append (iota (10, 19, -1), lst)
      lst = append (iota (10, 9, -1), lst)
    end function make_0_to_99_strangely_ordered

  end subroutine test0020

  subroutine test0030

    !
    ! Tests of list_is_sorted.
    !

    call check (.not. list_is_sorted (less_than, iota (100, 100, -1)), "test0030-0010 failed")
    call check (list_is_sorted (less_than, iota (100, 1)), "test0030-0020 failed")

    call check (.not. list_is_sorted (less_than, make_0_to_99_strangely_ordered ()), "test0030-0030 failed")
    call check (list_is_sorted (less_than, list_sortx (less_than, make_0_to_99_strangely_ordered ())), "test0030-0040 failed")

    call check (list_is_sorted (less_than, make_list (100, 123)), "test0030-0050 failed")
    call check (list_is_sorted (less_than, nil), "test0030-0060 failed")
    call check (list_is_sorted (less_than, list (123)), "test0030-0070 failed")

    call check (.not. list_is_sorted (is_lt_only_ones, make_0_to_99_strangely_ordered ()), "test0030-0080 failed")
    call check (list_is_sorted (is_lt_only_ones, list_stable_sortx (is_lt_only_ones, make_0_to_99_strangely_ordered ())), &
         "test0030-0090 failed")
    call check (.not. list_is_sorted (less_than, list_stable_sortx (is_lt_only_ones, make_0_to_99_strangely_ordered ())), &
         "test0030-0100 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

    function is_lt_only_ones (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      integer :: x1
      integer :: y1

      call collect_garbage_now

      x1 = int_cast (x)
      x1 = mod (x1, 10)

      y1 = int_cast (y)
      y1 = mod (y1, 10)

      bool = (x1 < y1)
    end function is_lt_only_ones

    function make_0_to_99_strangely_ordered () result (lst)
      type(cons_t) :: lst

      lst = iota (10, 99, -1)
      lst = append (iota (10, 89, -1), lst)
      lst = append (iota (10, 79, -1), lst)
      lst = append (iota (10, 69, -1), lst)
      lst = append (iota (10, 59, -1), lst)
      lst = append (iota (10, 49, -1), lst)
      lst = append (iota (10, 39, -1), lst)
      lst = append (iota (10, 29, -1), lst)
      lst = append (iota (10, 19, -1), lst)
      lst = append (iota (10, 9, -1), lst)
    end function make_0_to_99_strangely_ordered

  end subroutine test0030

  subroutine test0040
    type(cons_t) :: lst
    integer :: i

    call check (is_nil (list_delete_neighbor_dupsx (is_eq, nil)), "test0040-0010 failed")
    call check (list_delete_neighbor_dupsx (is_eq, 123) .eqi. 123, "test0040-0020 failed")
    call check (list_equal (int_eq, list_delete_neighbor_dupsx (is_eq, list (123)), list (123)), "test0040-0030 failed")
    call check (list_equal (int_eq, list_delete_neighbor_dupsx (is_eq, make_list (100, 123)), list (123)), &
         "test0040-0040 failed")

    do i = 1, 11, 10
       call check (list_equal (int_eq, &
            list_delete_neighbor_dupsx (is_eq, append (make_list (i, 123), list (456))), &
            list (123, 456)), &
            "test0040-0050 failed")
    end do

    do i = 1, 11, 10
       call check (list_equal (int_eq, &
            list_delete_neighbor_dupsx (is_eq, cons (123, make_list (i, 456))), &
            list (123, 456)), &
            "test0040-0060 failed")
    end do

    do i = 1, 11, 10
       call check (list_equal (int_eq, &
            list_delete_neighbor_dupsx (is_eq, cons (123, append (make_list (i, 456), list (789)))), &
            list (123, 456, 789)), &
            "test0040-0070 failed")
    end do

    lst = nil
    do i = 1, 10
       lst = append (lst, make_list (i, i))
    end do
    call check (list_equal (int_eq, list_delete_neighbor_dupsx (is_eq, lst), iota (10, 1)), "test0040-0080 failed")

    lst = nil
    do i = 1, 10
       lst = append (make_list (i, i), lst)
    end do
    call check (list_equal (int_eq, list_delete_neighbor_dupsx (is_eq, lst), iota (10, 10, -1)), "test0040-0090 failed")

    lst = nil
    do i = 1, 10
       if (mod (i, 3) == 0) then
          lst = append (make_list (i, i), lst)
       else
          lst = cons (i, lst)
       end if
    end do
    call check (list_equal (int_eq, list_delete_neighbor_dupsx (is_eq, lst), iota (10, 10, -1)), "test0040-0100 failed")

    lst = nil
    do i = 1, 10
       if (mod (i, 3) /= 0) then
          lst = append (make_list (i, i), lst)
       else
          lst = cons (i, lst)
       end if
    end do
    call check (list_equal (int_eq, list_delete_neighbor_dupsx (is_eq, lst), iota (10, 10, -1)), "test0040-0110 failed")

    call check (list_equal (int_eq, list_delete_neighbor_dupsx (is_eq, iota (10, 1)), iota (10, 1)), "test0040-0120 failed")

  contains

    recursive function is_eq (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) == int_cast (y))
    end function is_eq

  end subroutine test0040

  subroutine test0050
    type(cons_t) :: lst
    type(gcroot_t) :: lst_copy
    integer :: i

    call check (is_nil (list_delete_neighbor_dups (is_eq, nil)), "test0050-0010 failed")
    call check (list_delete_neighbor_dups (is_eq, 123) .eqi. 123, "test0050-0020 failed")

    lst = list (123)
    lst_copy = list_copy (lst)
    call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), list (123)), "test0050-0030 failed")
    call check (list_equal (int_eq, lst, lst_copy), "test0050-0035 failed")

    lst = make_list (100, 123)
    lst_copy = list_copy (lst)
    call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), list (123)), "test0050-0040 failed")
    call check (list_equal (int_eq, lst, lst_copy), "test0050-0045 failed")

    do i = 1, 11, 10
       lst = append (make_list (i, 123), list (456))
       lst_copy = list_copy (lst)
       call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), list (123, 456)), "test0050-0050 failed")
       call check (list_equal (int_eq, lst, lst_copy), "test0050-0055 failed")
    end do

    do i = 1, 11, 10
       lst = cons (123, make_list (i, 456))
       lst_copy = list_copy (lst)
       call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), list (123, 456)), "test0050-0060 failed")
       call check (list_equal (int_eq, lst, lst_copy), "test0050-0065 failed")
    end do

    do i = 1, 11, 10
       lst = append (make_list (i, 456), list (789))
       lst_copy = list_copy (lst)
       call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, cons (123, lst)), list (123, 456, 789)), &
            "test0050-0070 failed")
       call check (list_equal (int_eq, lst, lst_copy), "test0050-0075 failed")
    end do

    lst = nil
    do i = 1, 10
       lst = append (lst, make_list (i, i))
    end do
    lst_copy = list_copy (lst)
    call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), iota (10, 1)), "test0050-0080 failed")
    call check (list_equal (int_eq, lst, lst_copy), "test0050-0085 failed")

    lst = nil
    do i = 1, 10
       lst = append (make_list (i, i), lst)
    end do
    lst_copy = list_copy (lst)
    call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), iota (10, 10, -1)), "test0050-0090 failed")
    call check (list_equal (int_eq, lst, lst_copy), "test0050-0095 failed")

    lst = nil
    do i = 1, 10
       if (mod (i, 3) == 0) then
          lst = append (make_list (i, i), lst)
       else
          lst = cons (i, lst)
       end if
    end do
    lst_copy = list_copy (lst)
    call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), iota (10, 10, -1)), "test0050-0100 failed")
    call check (list_equal (int_eq, lst, lst_copy), "test0050-0105 failed")

    lst = nil
    do i = 1, 10
       if (mod (i, 3) /= 0) then
          lst = append (make_list (i, i), lst)
       else
          lst = cons (i, lst)
       end if
    end do
    lst_copy = list_copy (lst)
    call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), iota (10, 10, -1)), "test0050-0110 failed")
    call check (list_equal (int_eq, lst, lst_copy), "test0050-0115 failed")

    lst = iota (10, 1)
    lst_copy = list_copy (lst)
    call check (list_equal (int_eq, list_delete_neighbor_dups (is_eq, lst), iota (10, 1)), "test0050-0120 failed")
    call check (list_equal (int_eq, lst, lst_copy), "test0050-0125 failed")

    lst = list_delete_neighbor_dups (is_eq, 1 ** 1 ** 1 ** cons (1, 2))
    call check (car (lst) .eqi. 1, "test0050-0210 failed")
    call check (cdr (lst) .eqi. 2, "test0050-0220 failed")

  contains

    recursive function is_eq (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) == int_cast (y))
    end function is_eq

  end subroutine test0050

  subroutine test1010

    !
    ! There is a miniscule chance of one of these tests failing due to
    ! the shuffle producing a sorted array.
    !

    type(vectar_t) :: vec1, vec2
    type(gcroot_t) :: vec1_copy

    vec1 = vectar_append (vectar (1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 7, 7, 8, 9), &
         &                list_to_vectar (iota (90, 10)))
    vec1_copy = vectar_copy (vec1)
    call check (vectar_is_sorted (int_lt, vec1), "test1010-0010 failed")
    vec2 = vectar_shuffle (vec1)
    call check (.not. vectar_is_sorted (int_lt, vec2), "test1010-0030 failed")
    call check (vectar_equal (int_eq, vecsort (int_lt, vec2), vec1_copy), "test1010-0040 failed")
    call check (vectar_equal (int_eq, vec1, vec1_copy), "test1010-0050 failed")

    vec1 = vectar_append (vectar (1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 7, 7, 8, 9), &
         &                list_to_vectar (iota (90, 10)))
    vec1_copy = vectar_copy (vec1)
    call check (vectar_is_sorted (int_lt, vec1), "test1010-0110 failed")
    call check (vectar_length (vec1) == 104_sz, "test1010-0120 failed")
    vec2 = vectar_shuffle (range1 (vec1, 20, 79))
    call check (.not. vectar_is_sorted (int_lt, vec2), "test1010-0130 failed")
    call check (vectar_equal (int_eq, vectar_append (range1 (vec1_copy, 1, 19), &
         &                                           vecsort (int_lt, vec2), &
         &                                           range1 (vec1_copy, 80, 104)), &
         &                    vec1_copy), &
         &      "test1010-0140 failed")
    call check (vectar_equal (int_eq, vec1, vec1_copy), "test1010-0150 failed")

    call check (vectar_equal (int_eq, vectar_shuffle (vectar ()), vectar ()), "test1010-0210 failed")
    call check (vectar_equal (int_eq, vectar_shuffle (vectar (123)), vectar (123)), "test1010-0220 failed")

  end subroutine test1010

  subroutine test1020
    type(vectar_t) :: vec_m, vec1, vec2

    vec1 = list_to_vectar (iota (50, 1, 2))
    vec2 = list_to_vectar (iota (50, 2, 2))
    vec_m = vectar_merge (less_than, vec1, vec2)
    call check (list_equal (int_eq, vectar_to_list (vec1), iota (50, 1, 2)), "test1020-0010 failed")
    call check (list_equal (int_eq, vectar_to_list (vec2), iota (50, 2, 2)), "test1020-0020 failed")
    call check (list_equal (int_eq, vectar_to_list (vec_m), iota (100, 1)), "test1020-0030 failed")
    vec_m = vectar_merge (less_than, vec2, vec1)
    call check (list_equal (int_eq, vectar_to_list (vec1), iota (50, 1, 2)), "test1020-0040 failed")
    call check (list_equal (int_eq, vectar_to_list (vec2), iota (50, 2, 2)), "test1020-0050 failed")
    call check (list_equal (int_eq, vectar_to_list (vec_m), iota (100, 1)), "test1020-0060 failed")

    vec1 = list_to_vectar (iota (20, 1, 2))
    vec2 = list_to_vectar (iota (50, 2, 2))
    vec_m = vectar_merge (less_than, vec1, vec2)
    call check (list_equal (int_eq, vectar_to_list (vec1), iota (20, 1, 2)), "test1020-0110 failed")
    call check (list_equal (int_eq, vectar_to_list (vec2), iota (50, 2, 2)), "test1020-0120 failed")
    call check (list_equal (int_eq, vectar_to_list (vec_m), append (iota (40, 1), iota (30, 42, 2))), "test1020-0130 failed")
    vec_m = vectar_merge (less_than, vec2, vec1)
    call check (list_equal (int_eq, vectar_to_list (vec1), iota (20, 1, 2)), "test1020-0140 failed")
    call check (list_equal (int_eq, vectar_to_list (vec2), iota (50, 2, 2)), "test1020-0150 failed")
    call check (list_equal (int_eq, vectar_to_list (vec_m), append (iota (40, 1), iota (30, 42, 2))), "test1020-0160 failed")

    ! Stability test.
    vec1 = vectar (22, 31, 53, 61)
    vec2 = vectar (21, 42, 41, 52, 51)
    vec_m = vectar_merge (is_lt_except_ones, vec1, vec2)
    call check (vectar_equal (int_eq, vec_m, vectar (22, 21, 31, 42, 41, 53, 52, 51, 61)), "test1020-0210 failed")
    vec_m = vectar_merge (is_lt_except_ones, vec2, vec1)
    call check (vectar_equal (int_eq, vec_m, vectar (21, 22, 31, 42, 41, 52, 51, 53, 61)), "test1020-0220 failed")

    ! Check vectar ranges.
    vec1 = vectar (100, 100, 22, 31, 53, 61, 100, 21, 42, 41, 52, 51, 100)
    vec_m = vectar_merge (is_lt_except_ones, range1 (vec1, 3, 6), range1 (vec1, 8, 12))
    call check (vectar_equal (int_eq, vec_m, vectar (22, 21, 31, 42, 41, 53, 52, 51, 61)), "test1020-0310 failed")
    vec_m = vectar_merge (is_lt_except_ones, range1 (vec1, 8, 12), range1 (vec1, 3, 6))
    call check (vectar_equal (int_eq, vec_m, vectar (21, 22, 31, 42, 41, 52, 51, 53, 61)), "test1020-0320 failed")

    ! Check vectar ranges.
    vec1 = vectar (100, 22, 31, 53, 61, 100, 21, 42, 41, 52, 51, 100, 100, 100, 100, 100, 100, 100, 100, 100)
    call vectar_mergex (is_lt_except_ones, range1 (vec1, 12, 20), range1 (vec1, 2, 5), range1 (vec1, 7, 11))
    call check (vectar_equal (int_eq, vec1, vectar (100, 22, 31, 53, 61, 100, 21, 42, 41, 52, 51, &
         &                                          22, 21, 31, 42, 41, 53, 52, 51, 61)), &
         &      "test1020-0410 failed")
    call vectar_mergex (is_lt_except_ones, range1 (vec1, 12, 20), range1 (vec1, 7, 11), range1 (vec1, 2, 5))
    call check (vectar_equal (int_eq, vec1, vectar (100, 22, 31, 53, 61, 100, 21, 42, 41, 52, 51, &
         &                                          21, 22, 31, 42, 41, 52, 51, 53, 61)), &
         &      "test1020-0420 failed")

    vec_m = vectar_merge (less_than, vectar (), vectar ())
    call check (vectar_is_empty (vec_m), "test1020-1010 failed")

    vec_m = vectar_merge (less_than, vectar (1, 2, 3), vectar ())
    call check (vectar_equal (int_eq, vec_m, vectar (1, 2, 3)), "test1020-1020 failed")

    vec_m = vectar_merge (less_than, vectar (), vectar (1, 2, 3))
    call check (vectar_equal (int_eq, vec_m, vectar (1, 2, 3)), "test1020-1020 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

    function is_lt_except_ones (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      integer :: x1
      integer :: y1

      call collect_garbage_now

      x1 = int_cast (x)
      x1 = x1 - mod (x1, 10)

      y1 = int_cast (y)
      y1 = y1 - mod (y1, 10)

      bool = (x1 < y1)
    end function is_lt_except_ones

  end subroutine test1020

  subroutine test1030
    integer, parameter :: num_shuffles = 10

    integer, parameter :: num_stable_sort_examples = 10
    integer, parameter :: stable_sort_example_length = 1000
    integer, parameter :: jrandom = 1
    integer, parameter :: jsorted = 2

    type(vectar_t) :: vec1
    type(gcroot_t) :: stable_sort_examples(jrandom:jsorted, 1:num_stable_sort_examples)
    integer :: i, j

    call read_stable_sort_examples (stable_sort_examples)

    ! Some small sorts.
    vec1 = vectar (52, 22, 31, 42, 53, 61, 21, 41, 51)
    call vectar_stable_sortx (is_lt_except_ones, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (22, 21, 31, 42, 41, 52, 53, 51, 61)), "test1030-0010 failed")
    vec1 = vectar (41, 21, 31, 42, 22, 53, 61, 52, 51)
    call vectar_stable_sortx (is_lt_except_ones, vec1)
    call check (vectar_equal (int_eq, vec1, vectar (21, 22, 31, 41, 42, 53, 52, 51, 61)), "test1030-0020 failed")

    vec1 = list_to_vectar (append (iota (99, 99, -1), iota (101, 200, -1)))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1030-0030 failed")

    vec1 = list_to_vectar (append (iota (99, 197, -2), iota (99, 2, 2), list (199, 200)))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1030-0040 failed")

    vec1 = list_to_vectar (append (iota (99, 102), iota (101, 1)))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1030-0050 failed")

    vec1 = list_to_vectar (append (iota (101, 101, -1), iota (99, 200, -1)))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1030-0060 failed")

    vec1 = list_to_vectar (append (iota (99, 2, 2), list (199, 200), iota (99, 197, -2)))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1030-0070 failed")

    vec1 = list_to_vectar (append (iota (101, 100), iota (99, 1)))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1030-0080 failed")

    ! A large vectar that needs no sorting.
    vec1 = list_to_vectar (iota (1000))
    call vectar_stable_sortx (is_lt_except_ones, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (1000))), "test1030-1030 failed")

    ! A large vectar that needs only reversing.
    vec1 = list_to_vectar (iota (1000, 999, -1))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (1000))), "test1030-1035 failed")

    ! A shuffled vectar, sorted into ascending integer order.
    vec1 = list_to_vectar (iota (1000))
    do i = 1, num_shuffles
       call vectar_shufflex (vec1)
       call vectar_stable_sortx (less_than, vec1)
       call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (1000))), "test1030-1080 failed")
    end do

    ! Test the case where there is a final run of length 1.
    vec1 = list_to_vectar (append (iota (100), list (3)))
    call vectar_stable_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, vectar_append (vectar (0, 1, 2, 3, 3), list_to_vectar (iota (96, 4)))), &
         &      "test1030-1090 failed")

    ! Test stability by sorting randomized examples and comparing the
    ! results with copies that were already sorted.
    do i = 1, num_stable_sort_examples
       vec1 = vectar_copy (stable_sort_examples(jrandom, i))
       call vectar_stable_sortx (is_lt_except_ones, vec1)
       call check (vectar_equal (int_eq, vec1, stable_sort_examples(jsorted, i)), "test1030-2010 failed")
    end do

    ! Check sorting a range.
    vec1 = list_to_vectar (iota (100, 100, -1))
    call vectar_stable_sortx (less_than, range1 (vec1, 26, 75))
    call check (vectar_equal (int_eq, vec1, list_to_vectar (append (iota (25, 100, -1), iota (50, 26), iota (25, 25, -1)))), &
         &      "test1030-3010 failed")

    ! Roots as array entries need explicit discard,
    ! unfortunately. They will not be finalized automatically.
    do i = 1, num_stable_sort_examples
       do j = jrandom, jsorted
          call stable_sort_examples(j, i)%discard
       end do
    end do

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      block
        ! Running the garbage collector is slow, and there are a lot
        ! of comparisons in the sorting tests. Collect garbage only
        ! occasionally.
        integer(sz), save :: count = 0_sz
        if (count < 100_sz .or. mod (count, 100_sz) == 0_sz) then
           call collect_garbage_now
        end if
        count = count + 1_sz
      end block

      bool = (int_cast (x) < int_cast (y))
    end function less_than

    function is_lt_except_ones (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      integer :: x1
      integer :: y1

      block
        ! Running the garbage collector is slow, and there are a lot
        ! of comparisons in the sorting tests. Collect garbage only
        ! occasionally.
        integer(sz), save :: count = 0_sz
        if (count < 100_sz .or. mod (count, 100_sz) == 0_sz) then
           call collect_garbage_now
        end if
        count = count + 1_sz
      end block

      x1 = int_cast (x)
      x1 = x1 - mod (x1, 10)

      y1 = int_cast (y)
      y1 = y1 - mod (y1, 10)

      bool = (x1 < y1)
    end function is_lt_except_ones

    subroutine read_stable_sort_examples (vectars)
      type(gcroot_t) :: vectars(jrandom:jsorted, 1:num_stable_sort_examples)

      integer, parameter :: fileno = 20

      integer :: x(1:2000)
      integer :: i, j

      open (fileno, file = "stable-sort-examples.txt", status = "old")
      do i = 1, num_stable_sort_examples
         read (fileno,*) x
         vectars(jrandom, i) = make_vectar (stable_sort_example_length)
         do j = 1, stable_sort_example_length
            call vectar_set1 (vectars(1, i), j, x(j))
         end do
         vectars(jsorted, i) = make_vectar (stable_sort_example_length)
         do j = 1, stable_sort_example_length
            call vectar_set1 (vectars(2, i), j, x(j + stable_sort_example_length))
         end do
      end do
      close (fileno)
    end subroutine read_stable_sort_examples

  end subroutine test1030

  subroutine test1040
    integer, parameter :: num_shuffles = 10

    type(vectar_t) :: vec1
    integer :: i

    vec1 = list_to_vectar (append (iota (99, 99, -1), iota (101, 200, -1)))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1040-0030 failed")

    vec1 = list_to_vectar (append (iota (99, 197, -2), iota (99, 2, 2), list (199, 200)))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1040-0040 failed")

    vec1 = list_to_vectar (append (iota (99, 102), iota (101, 1)))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1040-0050 failed")

    vec1 = list_to_vectar (append (iota (101, 101, -1), iota (99, 200, -1)))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1040-0060 failed")

    vec1 = list_to_vectar (append (iota (99, 2, 2), list (199, 200), iota (99, 197, -2)))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1040-0070 failed")

    vec1 = list_to_vectar (append (iota (101, 100), iota (99, 1)))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (200, 1))), "test1040-0080 failed")

    ! A large vectar that needs no sorting.
    vec1 = list_to_vectar (iota (1000))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (1000))), "test1040-1030 failed")

    ! A large vectar that needs only reversing.
    vec1 = list_to_vectar (iota (1000, 999, -1))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (1000))), "test1040-1035 failed")

    ! A shuffled vectar, sorted into ascending integer order.
    vec1 = list_to_vectar (iota (1000))
    do i = 1, num_shuffles
       call vectar_shufflex (vec1)
       call vectar_sortx (less_than, vec1)
       call check (vectar_equal (int_eq, vec1, list_to_vectar (iota (1000))), "test1040-1080 failed")
    end do

    ! Test the case where there is a final run of length 1.
    vec1 = list_to_vectar (append (iota (100), list (3)))
    call vectar_sortx (less_than, vec1)
    call check (vectar_equal (int_eq, vec1, vectar_append (vectar (0, 1, 2, 3, 3), list_to_vectar (iota (96, 4)))), &
         &      "test1040-1090 failed")

    ! Check sorting a range.
    vec1 = list_to_vectar (iota (100, 100, -1))
    call vectar_sortx (less_than, range1 (vec1, 26, 75))
    call check (vectar_equal (int_eq, vec1, list_to_vectar (append (iota (25, 100, -1), iota (50, 26), iota (25, 25, -1)))), &
         &      "test1040-3010 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      block
        ! Running the garbage collector is slow, and there are a lot
        ! of comparisons in the sorting tests. Collect garbage only
        ! occasionally.
        integer(sz), save :: count = 0_sz
        if (count < 100_sz .or. mod (count, 100_sz) == 0_sz) then
           call collect_garbage_now
        end if
        count = count + 1_sz
      end block

      bool = (int_cast (x) < int_cast (y))
    end function less_than

  end subroutine test1040

  subroutine test1050

    !
    ! FIXME: Check that the original vector does not get clobbered.
    !

    integer, parameter :: num_stable_sort_examples = 10
    integer, parameter :: stable_sort_example_length = 1000
    integer, parameter :: jrandom = 1
    integer, parameter :: jsorted = 2

    type(vectar_t) :: vec1, vec2
    type(gcroot_t) :: stable_sort_examples(jrandom:jsorted, 1:num_stable_sort_examples)
    integer :: i, j

    call read_stable_sort_examples (stable_sort_examples)

    ! Some small sorts.
    vec1 = vectar (52, 22, 31, 42, 53, 61, 21, 41, 51)
    vec2 = vectar_stable_sort (is_lt_except_ones, vec1)
    call check (vectar_equal (int_eq, vec2, vectar (22, 21, 31, 42, 41, 52, 53, 51, 61)), "test1050-0010 failed")
    vec1 = vectar (41, 21, 31, 42, 22, 53, 61, 52, 51)
    vec2 = vectar_stable_sort (is_lt_except_ones, vec1)
    call check (vectar_equal (int_eq, vec2, vectar (21, 22, 31, 41, 42, 53, 52, 51, 61)), "test1050-0020 failed")

    ! Trigger a leftwards merge.
    vec1 = list_to_vectar (append (iota (99, 99, -1), iota (101, 200, -1)))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1050-0030 failed")

    ! Trigger a more complex leftwards merge.
    vec1 = list_to_vectar (append (iota (99, 197, -2), iota (99, 2, 2), list (199, 200)))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1050-0040 failed")

    ! Another leftwards merge.
    vec1 = list_to_vectar (append (iota (99, 102), iota (101, 1)))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1050-0050 failed")

    ! Trigger a rightwards merge.
    vec1 = list_to_vectar (append (iota (101, 101, -1), iota (99, 200, -1)))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1050-0060 failed")

    ! Trigger a more complex rightwards merge.
    vec1 = list_to_vectar (append (iota (99, 2, 2), list (199, 200), iota (99, 197, -2)))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1050-0070 failed")

    ! Another rightwards merge.
    vec1 = list_to_vectar (append (iota (101, 100), iota (99, 1)))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1050-0080 failed")

    ! A large vectar that needs no sorting.
    vec1 = list_to_vectar (iota (1000))
    vec2 = vectar_stable_sort (is_lt_except_ones, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (1000))), "test1050-1030 failed")

    ! A large vectar that needs only reversing.
    vec1 = list_to_vectar (iota (1000, 999, -1))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (1000))), "test1050-1035 failed")

    ! Test the case where there is a final run of length 1.
    vec1 = list_to_vectar (append (iota (100), list (3)))
    vec2 = vectar_stable_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, vectar_append (vectar (0, 1, 2, 3, 3), list_to_vectar (iota (96, 4)))), &
         &      "test1050-1090 failed")

    ! Test stability by sorting randomized examples and comparing the
    ! results with copies that were already sorted.
    do i = 1, num_stable_sort_examples
       vec2 = vectar_stable_sort (is_lt_except_ones, stable_sort_examples(jrandom, i))
       call check (vectar_equal (int_eq, vec2, stable_sort_examples(jsorted, i)), "test1050-2010 failed")
    end do

    ! Check sorting a range.
    vec1 = list_to_vectar (iota (100, 100, -1))
    vec2 = vectar_stable_sort (less_than, range1 (vec1, 26, 75))
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (50, 26))), "test1050-3010 failed")

    ! Roots as array entries need explicit discard,
    ! unfortunately. They will not be finalized automatically.
    do i = 1, num_stable_sort_examples
       do j = jrandom, jsorted
          call stable_sort_examples(j, i)%discard
       end do
    end do

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      block
        ! Running the garbage collector is slow, and there are a lot
        ! of comparisons in the sorting tests. Collect garbage only
        ! occasionally.
        integer(sz), save :: count = 0_sz
        if (count < 100_sz .or. mod (count, 100_sz) == 0_sz) then
           call collect_garbage_now
        end if
        count = count + 1_sz
      end block

      bool = (int_cast (x) < int_cast (y))
    end function less_than

    function is_lt_except_ones (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      integer :: x1
      integer :: y1

      block
        ! Running the garbage collector is slow, and there are a lot
        ! of comparisons in the sorting tests. Collect garbage only
        ! occasionally.
        integer(sz), save :: count = 0_sz
        if (count < 100_sz .or. mod (count, 100_sz) == 0_sz) then
           call collect_garbage_now
        end if
        count = count + 1_sz
      end block

      x1 = int_cast (x)
      x1 = x1 - mod (x1, 10)

      y1 = int_cast (y)
      y1 = y1 - mod (y1, 10)

      bool = (x1 < y1)
    end function is_lt_except_ones

    subroutine read_stable_sort_examples (vectars)
      type(gcroot_t) :: vectars(jrandom:jsorted, 1:num_stable_sort_examples)

      integer, parameter :: fileno = 20

      integer :: x(1:2000)
      integer :: i, j

      open (fileno, file = "stable-sort-examples.txt", status = "old")
      do i = 1, num_stable_sort_examples
         read (fileno,*) x
         vectars(jrandom, i) = make_vectar (stable_sort_example_length)
         do j = 1, stable_sort_example_length
            call vectar_set1 (vectars(1, i), j, x(j))
         end do
         vectars(jsorted, i) = make_vectar (stable_sort_example_length)
         do j = 1, stable_sort_example_length
            call vectar_set1 (vectars(2, i), j, x(j + stable_sort_example_length))
         end do
      end do
      close (fileno)
    end subroutine read_stable_sort_examples

  end subroutine test1050

  subroutine test1060

    !
    ! FIXME: Check that the original vector does not get clobbered.
    !

    type(vectar_t) :: vec1, vec2

    ! Trigger a leftwards merge.
    vec1 = list_to_vectar (append (iota (99, 99, -1), iota (101, 200, -1)))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1060-0030 failed")

    ! Trigger a more complex leftwards merge.
    vec1 = list_to_vectar (append (iota (99, 197, -2), iota (99, 2, 2), list (199, 200)))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1060-0040 failed")

    ! Another leftwards merge.
    vec1 = list_to_vectar (append (iota (99, 102), iota (101, 1)))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1060-0050 failed")

    ! Trigger a rightwards merge.
    vec1 = list_to_vectar (append (iota (101, 101, -1), iota (99, 200, -1)))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1060-0060 failed")

    ! Trigger a more complex rightwards merge.
    vec1 = list_to_vectar (append (iota (99, 2, 2), list (199, 200), iota (99, 197, -2)))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1060-0070 failed")

    ! Another rightwards merge.
    vec1 = list_to_vectar (append (iota (101, 100), iota (99, 1)))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (200, 1))), "test1060-0080 failed")

    ! A large vectar that needs no sorting.
    vec1 = list_to_vectar (iota (1000))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (1000))), "test1060-1030 failed")

    ! A large vectar that needs only reversing.
    vec1 = list_to_vectar (iota (1000, 999, -1))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (1000))), "test1060-1035 failed")

    ! Test the case where there is a final run of length 1.
    vec1 = list_to_vectar (append (iota (100), list (3)))
    vec2 = vectar_sort (less_than, vec1)
    call check (vectar_equal (int_eq, vec2, vectar_append (vectar (0, 1, 2, 3, 3), list_to_vectar (iota (96, 4)))), &
         &      "test1060-1090 failed")

    ! Check sorting a range.
    vec1 = list_to_vectar (iota (100, 100, -1))
    vec2 = vectar_sort (less_than, range1 (vec1, 26, 75))
    call check (vectar_equal (int_eq, vec2, list_to_vectar (iota (50, 26))), "test1060-3010 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      block
        ! Running the garbage collector is slow, and there are a lot
        ! of comparisons in the sorting tests. Collect garbage only
        ! occasionally.
        integer(sz), save :: count = 0_sz
        if (count < 100_sz .or. mod (count, 100_sz) == 0_sz) then
           call collect_garbage_now
        end if
        count = count + 1_sz
      end block

      bool = (int_cast (x) < int_cast (y))
    end function less_than

  end subroutine test1060

  subroutine test1070

    call check (vectar_equal (int_eq, vectar_delete_neighbor_dups (is_eq, vectar ()), vectar ()), "test1070-0010 failed")
    call check (vectar_equal (int_eq, vectar_delete_neighbor_dups (is_eq, vectar (1)), vectar (1)), "test1070-0020 failed")
    call check (vectar_equal (int_eq, vectar_delete_neighbor_dups (is_eq, vectar (1, 1)), vectar (1)), "test1070-0030 failed")
    call check (vectar_equal (int_eq, vectar_delete_neighbor_dups (is_eq, vectar (1, 1, 1, 1, 1)), vectar (1)), &
         &      "test1070-0040 failed")

    call check (vectar_equal (int_eq, vectar_delete_neighbor_dups (is_eq, vectar (1, 1, 2, 2, 2, 3, 4, 4)), &
         &                                                         vectar (1, 2, 3, 4)), &
         &      "test1070-0050 failed")

    call check (vectar_equal (int_eq, vectar_delete_neighbor_dups (is_eq, range1 (vectar (1, 1, 2, 2, 2, 3, 4, 4), 3, 6)), &
         &                                                         vectar (2, 3)), &
         &      "test1070-0060 failed")

  contains

    recursive function is_eq (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) == int_cast (y))
    end function is_eq

  end subroutine test1070

  subroutine test1080

    !
    ! FIXME: Test empty vectars, etc.
    !

    type(vectar_t) :: vec
    integer(sz) :: num_not_dups

    vec = vectar (0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
    call vectar_delete_neighbor_dupsx (is_eq, range1 (vec, 4_sz, vectar_length (vec)), num_not_dups)
    call check (num_not_dups == 6, "test1080-0010 failed")
    call check (vectar_equal (int_eq, vec, vectar (0, 0, 0, 1, 2, 3, 4, 5, 6, 4, 4, 5, 5, 6, 6)), "test1080-0020 failed")

  contains

    recursive function is_eq (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) == int_cast (y))
    end function is_eq

  end subroutine test1080

  subroutine test1090
    type(gcroot_t) :: vec
    type(vectar_t) :: vec1
    integer :: kth_smallest

    vec = vectar (30, 10, 50, 60, 50, 40, 20)

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 0_sz) .eqi. 10, "test1090-0010 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 0) .eqi. 10, "test1090-0020 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 1_sz) .eqi. 10, "test1090-0030 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 1) .eqi. 10, "test1090-0040 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 2_sz) .eqi. 10, "test1090-0050 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 2) .eqi. 10, "test1090-0060 failed")

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 1_sz) .eqi. 20, "test1090-0110 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 1) .eqi. 20, "test1090-0120 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 2_sz) .eqi. 20, "test1090-0130 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 2) .eqi. 20, "test1090-0140 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 3_sz) .eqi. 20, "test1090-0150 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 3) .eqi. 20, "test1090-0160 failed")

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 2_sz) .eqi. 30, "test1090-0210 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 2) .eqi. 30, "test1090-0220 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 3_sz) .eqi. 30, "test1090-0230 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 3) .eqi. 30, "test1090-0240 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 4_sz) .eqi. 30, "test1090-0250 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 4) .eqi. 30, "test1090-0260 failed")

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 3_sz) .eqi. 40, "test1090-0310 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 3) .eqi. 40, "test1090-0320 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 4_sz) .eqi. 40, "test1090-0330 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 4) .eqi. 40, "test1090-0340 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 5_sz) .eqi. 40, "test1090-0350 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 5) .eqi. 40, "test1090-0360 failed")

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 4_sz) .eqi. 50, "test1090-0410 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 4) .eqi. 50, "test1090-0420 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 5_sz) .eqi. 50, "test1090-0430 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 5) .eqi. 50, "test1090-0440 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 6_sz) .eqi. 50, "test1090-0450 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 6) .eqi. 50, "test1090-0460 failed")

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 5_sz) .eqi. 50, "test1090-0510 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 5) .eqi. 50, "test1090-0520 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 6_sz) .eqi. 50, "test1090-0530 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 6) .eqi. 50, "test1090-0540 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 7_sz) .eqi. 50, "test1090-0550 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 7) .eqi. 50, "test1090-0560 failed")

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 6_sz) .eqi. 60, "test1090-0610 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 6) .eqi. 60, "test1090-0620 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 7_sz) .eqi. 60, "test1090-0630 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 7) .eqi. 60, "test1090-0640 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 8_sz) .eqi. 60, "test1090-0650 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 8) .eqi. 60, "test1090-0660 failed")

    vec = vectar (60)

    call check (vectar_selectx0 (less_than, vectar_copy (vec), 0_sz) .eqi. 60, "test1090-1610 failed")
    call check (vectar_selectx0 (less_than, vectar_copy (vec), 0) .eqi. 60, "test1090-1620 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 1_sz) .eqi. 60, "test1090-1630 failed")
    call check (vectar_selectx1 (less_than, vectar_copy (vec), 1) .eqi. 60, "test1090-1640 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2_sz, 2_sz) .eqi. 60, "test1090-1650 failed")
    call check (vectar_selectxn (less_than, vectar_copy (vec), 2, 2) .eqi. 60, "test1090-1660 failed")

    vec = vectar (30, 70, 10, 50, 60, 50, 40, 20)

    vec1 = vectar_copy (vec)
    kth_smallest = int_cast (vectar_selectx1 (less_than, range1 (vec1, 3, 6), 1))
    call check (kth_smallest == 10, "test1090-2010 failed")
    call check (vectar_ref1 (vec1, 1) .eqi. vectar_ref1 (vec, 1), "test1090-2020 failed")
    call check (vectar_ref1 (vec1, 2) .eqi. vectar_ref1 (vec, 2), "test1090-2030 failed")
    call check (vectar_ref1 (vec1, 7) .eqi. vectar_ref1 (vec, 7), "test1090-2040 failed")
    call check (vectar_ref1 (vec1, 8) .eqi. vectar_ref1 (vec, 8), "test1090-2050 failed")

    vec1 = vectar_copy (vec)
    kth_smallest = int_cast (vectar_selectx1 (less_than, range1 (vec1, 3, 6), 2))
    call check (kth_smallest == 50, "test1090-2110 failed")
    call check (vectar_ref1 (vec1, 1) .eqi. vectar_ref1 (vec, 1), "test1090-2120 failed")
    call check (vectar_ref1 (vec1, 2) .eqi. vectar_ref1 (vec, 2), "test1090-2130 failed")
    call check (vectar_ref1 (vec1, 7) .eqi. vectar_ref1 (vec, 7), "test1090-2140 failed")
    call check (vectar_ref1 (vec1, 8) .eqi. vectar_ref1 (vec, 8), "test1090-2150 failed")

    vec1 = vectar_copy (vec)
    kth_smallest = int_cast (vectar_selectx1 (less_than, range1 (vec1, 3, 6), 3))
    call check (kth_smallest == 50, "test1090-2210 failed")
    call check (vectar_ref1 (vec1, 1) .eqi. vectar_ref1 (vec, 1), "test1090-2220 failed")
    call check (vectar_ref1 (vec1, 2) .eqi. vectar_ref1 (vec, 2), "test1090-2230 failed")
    call check (vectar_ref1 (vec1, 7) .eqi. vectar_ref1 (vec, 7), "test1090-2240 failed")
    call check (vectar_ref1 (vec1, 8) .eqi. vectar_ref1 (vec, 8), "test1090-2250 failed")

    vec1 = vectar_copy (vec)
    kth_smallest = int_cast (vectar_selectx1 (less_than, range1 (vec1, 3, 6), 4))
    call check (kth_smallest == 60, "test1090-2310 failed")
    call check (vectar_ref1 (vec1, 1) .eqi. vectar_ref1 (vec, 1), "test1090-2320 failed")
    call check (vectar_ref1 (vec1, 2) .eqi. vectar_ref1 (vec, 2), "test1090-2330 failed")
    call check (vectar_ref1 (vec1, 7) .eqi. vectar_ref1 (vec, 7), "test1090-2340 failed")
    call check (vectar_ref1 (vec1, 8) .eqi. vectar_ref1 (vec, 8), "test1090-2350 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      block
        ! Running the garbage collector is slow, and there are a lot
        ! of comparisons in the sorting tests. Collect garbage only
        ! occasionally.
        integer(sz), save :: count = 0_sz
        if (count < 100_sz .or. mod (count, 100_sz) == 0_sz) then
           call collect_garbage_now
        end if
        count = count + 1_sz
      end block

      bool = (int_cast (x) < int_cast (y))
    end function less_than

  end subroutine test1090

  subroutine test1100
    type(gcroot_t) :: vec_root
    type(gcroot_t) :: vec_sorted
    type(vectar_t) :: vec
    integer :: len
    integer :: repetition
    integer :: i
    real :: randnum

    do repetition = 1, 10
       do len = 1, 10
          vec_root = make_vectar (len)
          do i = 1, len
             call random_number (randnum)
             call vectar_set1 (vec_root, i, 1 + int (randnum * len))
          end do
          vec_sorted = vectar_sort (less_than, vec_root)
          do i = 0, len
             vec = vectar_copy (vec_root)
             call vectar_separatex (less_than, vec, i)
             vec_root = vec
             call vectar_sortx (less_than, range1 (vec_root, 1, i)) ! Sort the smaller elements.
             call vectar_sortx (less_than, range1 (vec_root, i + 1, len)) ! Sort the larger elements.
             call check (vectar_equal (int_eq, vec_root, vec_sorted), "test1100-0010 failed")
          end do
       end do
    end do

    do repetition = 1, 10
       len = 10
       vec_root = make_vectar (len)
       do i = 1, len
          if (i == 1 .or. i == 2 .or. i == len - 1 .or. i == len) then
             call vectar_set1 (vec_root, i, -i)
          else
             call random_number (randnum)
             call vectar_set1 (vec_root, i, 1 + int (randnum * len))
          end if
       end do
       vec_sorted = vectar_copy (vec_root)
       call vectar_sortx (less_than, range1 (vec_sorted, 3, len - 2))
       do i = 0, len - 4
          vec = vectar_copy (vec_root)
          call vectar_separatex (less_than, range1 (vec, 3, len - 2), i)
          vec_root = vec
          call vectar_sortx (less_than, range1 (vec_root, 3, i + 2)) ! Sort the smaller elements.
          call vectar_sortx (less_than, range1 (vec_root, i + 3, len - 2)) ! Sort the larger elements.
          call check (vectar_equal (int_eq, vec_root, vec_sorted), "test1100-0020 failed")
       end do
    end do

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

  end subroutine test1100

  subroutine test1110
    type(gcroot_t) :: vec1
    type(gcroot_t) :: vec1_sorted
    type(vectar_t) :: vec2
    type(cons_t) :: lst1
    real :: median

    vec1 = vectar (8.0, 6.0, 8.0, 1.0, 3.0, 2.0, 3.0, 5.0, 5.0)
    vec1_sorted = vectar_sort (less_than, vec1)
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_medianx (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 5.0, "test1110-0010 failed")
    call check (vectar_equal (real_eq, vec2, vec1_sorted), "test1110-0020 failed")
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_median (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 5.0, "test1110-0030 failed")
    call check (vectar_equal (real_eq, vec2, vec1), "test1110-0040 failed")

    vec1 = vectar (8.0, 6.0, 8.0, 1.0, 3.0, 2.0, -30.0, 50.0, 4.0)
    vec1_sorted = vectar_sort (less_than, vec1)
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_medianx (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 4.0, "test1110-0110 failed")
    call check (vectar_equal (real_eq, vec2, vec1_sorted), "test1110-0120 failed")
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_median (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 4.0, "test1110-0130 failed")
    call check (vectar_equal (real_eq, vec2, vec1), "test1110-0140 failed")

    vec1 = vectar (8.0, 6.0, 8.0, 1.0, 3.0, 2.0, 3.0, 5.0)
    vec1_sorted = vectar_sort (less_than, vec1)
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_medianx (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 4.0, "test1110-0210 failed")
    call check (vectar_equal (real_eq, vec2, vec1_sorted), "test1110-0220 failed")
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_median (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 4.0, "test1110-0230 failed")
    call check (vectar_equal (real_eq, vec2, vec1), "test1110-0240 failed")

    vec1 = vectar ()
    vec1_sorted = vectar_sort (less_than, vec1)
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_medianx (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 999.0, "test1110-0310 failed")
    call check (vectar_equal (real_eq, vec2, vec1_sorted), "test1110-0320 failed")
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_median (less_than, vec2, 999.0, mean_subr))
    call check (median .eqr. 999.0, "test1110-0330 failed")
    call check (vectar_equal (real_eq, vec2, vec1), "test1110-0340 failed")

    vec1 = vectar (8.0, 6.0, 8.0, 1.0, 3.0, 2.0, 3.0, 5.0)
    vec1_sorted = vectar_copy (vec1)
    call vectar_sortx (less_than, range1 (vec1_sorted, 3, 5))
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_medianx (less_than, range1 (vec2, 3, 5), 999.0, mean_subr))
    call check (median .eqr. 3.0, "test1110-0410 failed")
    call check (vectar_equal (real_eq, vec2, vec1_sorted), "test1110-0420 failed")
    vec2 = vectar_copy (vec1)
    median = real_cast (vectar_find_median (less_than, range1 (vec2, 3, 5), 999.0, mean_subr))
    call check (median .eqr. 3.0, "test1110-0430 failed")
    call check (vectar_equal (real_eq, vec2, vec1), "test1110-0440 failed")

    ! Test that knil is protected against garbage collections.
    vec1 = vectar (8.0, 6.0, 8.0, 1.0, 3.0, 2.0, 3.0, 5.0)
    vec1_sorted = vectar_sort (less_than, vec1)
    vec2 = vectar_copy (vec1)
    lst1 = iota (100, 1)
    median = real_cast (vectar_find_medianx (less_than, vec2, lst1, mean_subr))
    call check (median .eqr. 4.0, "test1110-0510 failed")
    call check (vectar_equal (real_eq, vec2, vec1_sorted), "test1110-0520 failed")
    call check (list_equal (int_eq, lst1, iota (100, 1)), "test1110-0530 failed")
    vec2 = vectar_copy (vec1)
    lst1 = iota (100, 1)
    median = real_cast (vectar_find_median (less_than, vec2, lst1, mean_subr))
    call check (median .eqr. 4.0, "test1110-0540 failed")
    call check (vectar_equal (real_eq, vec2, vec1), "test1110-0550 failed")
    call check (list_equal (int_eq, lst1, iota (100, 1)), "test1110-0560 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (real_cast (x) < real_cast (y))
    end function less_than

    subroutine mean_subr (x, y, mean)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      class(*), allocatable, intent(out) :: mean

      call collect_garbage_now

      mean = (real_cast (x) + real_cast (y)) / 2
    end subroutine mean_subr

  end subroutine test1110

  subroutine test2010

    call check (vectar_is_sorted (less_than, vectar ()), "test2010-0010 failed")
    call check (vectar_is_sorted (less_than, vectar (123)), "test2010-0020 failed")
    call check (vectar_is_sorted (less_than, vectar (123, 456)), "test2010-0040 failed")
    call check (vectar_is_sorted (less_than, vectar (123, 456, 789)), "test2010-0050 failed")
    call check (vectar_is_sorted (less_than, vectar (1, 2, 3, 4, 5, 6, 7, 8, 9)), "test2010-0060 failed")
    call check (vectar_is_sorted (less_than, vectar (1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 7, 7, 8, 9)), "test2010-0070 failed")

    call check (.not. vectar_is_sorted (less_than, vectar (1, 1, 2, 2, 3, 3, 2, 5, 6, 7, 7, 7, 8, 9)), "test2010-0080 failed")
    call check (.not. vectar_is_sorted (less_than, vectar (2, 1)), "test2010-0090 failed")

    call check (vectar_is_sorted (less_than, vectar (2, 2)), "test2010-0100 failed")
    call check (vectar_is_sorted (less_than, vectar (2, 2, 2, 2, 2, 2, 2)), "test2010-0110 failed")

  contains

    function less_than (x, y) result (bool)
      class(*), intent(in) :: x
      class(*), intent(in) :: y
      logical :: bool

      call collect_garbage_now

      bool = (int_cast (x) < int_cast (y))
    end function less_than

  end subroutine test2010

  subroutine test2020

    !
    ! There is a miniscule chance of one of these tests failing due to
    ! the shuffle producing a sorted array.
    !

    type(vectar_t) :: vec1
    type(gcroot_t) :: vec1_copy

    vec1 = vectar_append (vectar (1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 7, 7, 8, 9), &
         &                list_to_vectar (iota (90, 10)))
    vec1_copy = vectar_copy (vec1)
    call check (vectar_is_sorted (int_lt, vec1), "test2020-0010 failed")
    call vectar_shufflex (vec1)
    call check (.not. vectar_is_sorted (int_lt, vec1), "test2020-0020 failed")
    call check (vectar_equal (int_eq, vecsort (int_lt, vec1), vec1_copy), "test2020-0030 failed")

    vec1 = vectar_append (vectar (1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 7, 7, 8, 9), &
         &                list_to_vectar (iota (90, 10)))
    vec1_copy = vectar_copy (vec1)
    call check (vectar_is_sorted (int_lt, vec1), "test2020-0110 failed")
    call check (vectar_length (vec1) == 104_sz, "test2020-0120 failed")
    call vectar_shufflex (range1 (vec1, 20, 79))
    call check (vectar_is_sorted (int_lt, range1 (vec1, 1, 19)), "test2020-0130 failed")
    call check (.not. vectar_is_sorted (int_lt, range1 (vec1, 20, 79)), "test2020-0140 failed")
    call check (vectar_is_sorted (int_lt, range1 (vec1, 80, 104)), "test2020-0150 failed")
    call check (vectar_equal (int_eq, vecsort (int_lt, vec1), vec1_copy), "test2020-0160 failed")

    vec1 = vectar ()
    call vectar_shufflex (vec1)
    call check (vectar_equal (int_eq, vec1, vectar ()), "test2020-0210 failed")
    vec1 = vectar (123)
    call vectar_shufflex (vec1)
    call check (vectar_equal (int_eq, vec1, vectar (123)), "test2020-0220 failed")

  end subroutine test2020

  subroutine run_tests
    heap_size_limit = 0

    call unit_test__bottenbruch_searches
    call unit_test__hoare_partitioning

    ! List merging, sorting, etc.
    call test0010
    call test0020
    call test0030
    call test0040
    call test0050

    ! Vectar merging, sorting, selection, etc.
    call test1010
    call test1020
    call test1030
    call test1040
    call test1050
    call test1060
    call test1070
    call test1080
    call test1090
    call test1100
    call test1110

    ! Vectar shuffling.
    call test2010
    call test2020

    call collect_garbage_now
    call check (current_heap_size () == 0, "run_tests-0100 failed")
    call check (current_roots_count () == 0, "run_tests-0110 failed")
  end subroutine run_tests

end module test__sorting_and_selection

program main
  use, non_intrinsic :: test__sorting_and_selection
  implicit none
  call run_tests
end program main
