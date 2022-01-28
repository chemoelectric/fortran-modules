dnl  -*- F90 -*-
dnl
dnl  Copyright 2021, 2022 Barry Schwartz
dnl
dnl  Permission is hereby granted, free of charge, to any person
dnl  obtaining a copy of this software and associated documentation files
dnl  (the "Software"), to deal in the Software without restriction,
dnl  including without limitation the rights to use, copy, modify, merge,
dnl  publish, distribute, sublicense, and/or sell copies of the Software,
dnl  and to permit persons to whom the Software is furnished to do so,
dnl  subject to the following conditions:
dnl
dnl  The above copyright notice and this permission notice shall be
dnl  included in all copies or substantial portions of the Software.
dnl
dnl  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
dnl  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
dnl  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
dnl  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
dnl  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
dnl  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
dnl  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
dnl  SOFTWARE.
dnl
dnl  -----------------------------------------------------------------
dnl
dnl  List-sort procedures for use in test programs.
dnl
dnl
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
