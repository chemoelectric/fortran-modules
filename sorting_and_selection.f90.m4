! -*- F90 -*- include(`common-macros.m4')
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
!

dnl
dnl
dnl I have tried to keep this file compatible with "heirloom" m4
dnl implementations (for example, by using ASCII), and also compatible
dnl with the POSIX specification for m4.
dnl
dnl However, with an "heirloom" m4 you might have to increase buffer
dnl size with the -B option.
dnl
dnl

module sorting_and_selection
  !
  ! Sorting and selection procedures, based on Scheme Request for
  ! Implementation 132 (SRFI-132).
  ! https://srfi.schemers.org/srfi-132/srfi-132.html
  !
  ! See also SRFI-95 (https://srfi.schemers.org/srfi-95/srfi-95.html).
  !

  !
  ! Implementation note: Our implementations of `delete neighbor
  !                      duplicates' procedures may assume the
  !                      equality predicate is transitive.
  !

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: vectars

  implicit none
  private

  ! Is the given list in sorted order?
  public :: list_is_sorted

  ! Stable merge.
  public :: list_merge
  public :: list_mergex         ! Allowed to alter its inputs.

  ! Stable sort.
  public :: list_stable_sort
  public :: list_stable_sortx   ! Allowed to alter its inputs.

  ! Sort that may or may not be stable.
  public :: list_sort
  public :: list_sortx          ! Allowed to alter its inputs.

  ! Delete adjacent duplicates.
  public :: list_delete_neighbor_dups
  public :: list_delete_neighbor_dupsx ! Allowed to modify its input
                                       ! list.

  ! Is the given vectar in sorted order?
  public :: vectar_is_sorted

  ! Stable merge.
  public :: vectar_merge
  public :: vectar_mergex ! The output vectar is given as a parameter.

  ! Stable sort.
  public :: vectar_stable_sort
  public :: vectar_stable_sortx ! The input vectar is also the output
                                ! vectar.

  ! Sort that may or may not be stable.
  public :: vectar_sort
  public :: vectar_sortx ! The input vectar is also the output vectar.

  ! Delete adjacent duplicates.
  public :: vectar_delete_neighbor_dups
  public :: vectar_delete_neighbor_dupsx ! Reuse part of the input
                                         ! vectar.

  ! Generic functions for selecting the kth smallest element of a
  ! vectar.
  public :: vectar_selectx0     ! k starts at 0.
  public :: vectar_selectx1     ! k starts at 1.
  public :: vectar_selectxn     ! k starts at n.

  ! Implementations of the vectar selection procedures above.
  public :: vectar_selectx0_size_kind
  public :: vectar_selectx1_size_kind
  public :: vectar_selectxn_size_kind
  public :: vectar_selectx0_int
  public :: vectar_selectx1_int
  public :: vectar_selectxn_int

  ! Generic function for separating the k smallest elements from the
  ! others.
  public :: vectar_separatex

  ! Implementations of vectar_separatex.
  public :: vectar_separatex_size_kind
  public :: vectar_separatex_int

  ! Generic function to find the median, without altering the vectar.
  public :: vectar_find_median

  ! Implementations of vectar_find_median.
  public :: vectar_find_median_mean_subr

  ! Generic function to find the median, and also leave the vectar or
  ! vectar range sorted.
  public :: vectar_find_medianx

  ! Implementations of vectar_find_medianx.
  public :: vectar_find_medianx_mean_subr

!!!-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

  ! Some unit tests. Not for use in a program other than for testing.
  public :: unit_test__bottenbruch_searches
  public :: unit_test__hoare_partitioning

!!!-------------------------------------------------------------------
!!
!! SHUFFLING.
!!
!! Shuffling is not included in SRFI-132.
!!

  ! Function that creates a new vectar, containing the same elements
  ! as a given vectar or vectar range, but shuffled.
  public :: vectar_shuffle

  ! Subroutine that shuffles a vectar or vectar range in place.
  public :: vectar_shufflex

!!!-------------------------------------------------------------------

  ! The maximum size of a stack used by the stable sort. It should be
  ! good enough if bit_size(1_sz) <= 64. See in particular
  ! http://envisage-project.eu/proving-android-java-and-python-sorting-algorithm-is-broken-and-how-to-fix-it/
  !
  ! From the `timsort' code (before the fix detailed at the above
  ! address, and the later switch to `powersort' merge strategy):
  !
  !    /* The maximum number of entries in a MergeState's 
  !     * pending-runs stack.
  !     * This is enough to sort arrays of size up to about
  !     *     32 * phi ** MAX_MERGE_PENDING
  !     * where phi ~= 1.618.  85 is ridiculously large enough, 
  !     * good for an array with 2**64 elements.
  !     */
  !
  ! Presumably one likes to keep it a fixed size and as small as is
  ! reasonable.
  !
  integer, parameter :: run_stack_size = 85

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

  ! Private conversion to `size_kind'.
  interface operator(.sz.)
     module procedure int2sz
  end interface operator(.sz.)

  interface vectar_selectx0
     module procedure vectar_selectx0_size_kind
     module procedure vectar_selectx0_int
  end interface vectar_selectx0

  interface vectar_selectx1
     module procedure vectar_selectx1_size_kind
     module procedure vectar_selectx1_int
  end interface vectar_selectx1

  interface vectar_selectxn
     module procedure vectar_selectxn_size_kind
     module procedure vectar_selectxn_int
  end interface vectar_selectxn

  interface vectar_separatex
     module procedure vectar_separatex_size_kind
     module procedure vectar_separatex_int
  end interface vectar_separatex

  interface vectar_find_median
     module procedure vectar_find_median_mean_subr
  end interface vectar_find_median

  interface vectar_find_medianx
     module procedure vectar_find_medianx_mean_subr
  end interface vectar_find_medianx

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

!!!-------------------------------------------------------------------

contains

!!!-------------------------------------------------------------------

  ! LCOV_EXCL_START
  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module sorting_and_selection error: ", a)') msg
    error stop
  end subroutine error_abort_1
  ! LCOV_EXCL_STOP

  elemental function int2sz (i) result (j)
    integer, intent(in) :: i
    integer(sz) :: j

    j = i
  end function int2sz

  function int_less_than (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = .false.
    select type (x)
    type is (integer)
       select type (y)
       type is (integer)
          bool = (x < y)
       end select
    end select
  end function int_less_than

!!!-------------------------------------------------------------------

  recursive function list_merge (less_than, lst1, lst2) result (lst_m)
    !
    ! It is assumed lst1 and lst2 are proper lists.
    !
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root

    lst1_root = lst1
    lst2_root = lst2
    lst_m = list_mergex (less_than, list_copy (lst1), list_copy (lst2))
    call lst1_root%discard
    call lst2_root%discard
  end function list_merge

  recursive function list_mergex (less_than, lst1, lst2) result (lst_m)
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
                        p1 = tl1
                     else
                        call set_cdr (cursor, p2)
                        cursor = p2
                        p2 = tl2
                     end if
                  end if
               end do
            end if
         end do
      end if
      call lst_m_root%discard
    end function merge_lists

  end function list_mergex

  recursive function list_stable_sort (less_than, lst) result (lst_ss)
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    type(gcroot_t) :: lst_root

    lst_root = lst
    lst_ss = list_stable_sortx (less_than, list_copy (lst))
    call lst_root%discard
  end function list_stable_sort

  recursive function list_stable_sortx (less_than, lst) result (lst_ss)
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    integer, parameter :: array_sort_size = 512

    type(gcroot_t) :: p
    type(gcroot_t) :: vec1, vec2
    type(vectar_data_t), pointer :: workspace1, workspace2

    p = lst
    if (is_not_pair (p)) then
       ! List of length zero.
       lst_ss = p
    else if (is_not_pair (cdr (p))) then
       ! List of length one.
       lst_ss = p
    else
       ! An array to fill with data to be sorted.
       vec1 = make_vectar (array_sort_size)
       workspace1 => vectar_data_ptr (vec1)

       ! Workspace used by the array sort.
       vec2 = make_vectar (ishft (array_sort_size, -1))
       workspace2 => vectar_data_ptr (vec2)

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
         ! A list of length 1 is already sorted.
         lst_ss = p   ! Is it possible for this branch to be executed?
      else if (n <= array_sort_size) then
         lst_ss = small_sort (.tocons. p, n)
      else
         n_half = n / 2
         call do_split_atx (p, n_half, p_left, p_right)
         p_left1 = p_left
         p_right1 = p_right
         p_left1 = merge_sort (p_left1, n_half)
         p_right1 = merge_sort (p_right1, n - n_half)
         lst_ss = list_mergex (less_than, p_left1, p_right1)
      end if
    end function merge_sort

    recursive function small_sort (p, n) result (lst_ss)
      type(cons_t), intent(in) :: p
      integer(sz), intent(in) :: n
      type(cons_t) :: lst_ss

      if (.not. less_than (cadr (p), car (p))) then
         lst_ss = small_sort_with_a_nondescending_prefix (p, n)
      else
         lst_ss = small_sort_with_a_descending_prefix (p, n)
      end if
    end function small_sort

    recursive function small_sort_with_a_nondescending_prefix (p, n) result (lst_ss)
      type(cons_t), intent(in) :: p
      integer(sz), intent(in) :: n
      type(cons_t) :: lst_ss

      type(cons_t) :: q
      integer(sz) :: prefix_length
      logical :: done

      prefix_length = 2
      q = cdr (p)
      done = .false.
      do while (.not. done)
         if (prefix_length == n) then
            ! The list is already in non-descending order.
            lst_ss = p
            done = .true.
         else if (less_than (cadr (q), car (q))) then
            ! The list is not entirely in non-descending
            ! order. Sort it as an array.
            lst_ss = array_sort (p, n, prefix_length)
            done = .true.
         else
            prefix_length = prefix_length + 1
            q = .tocons. cdr (q)
         end if
      end do
    end function small_sort_with_a_nondescending_prefix

    recursive function small_sort_with_a_descending_prefix (p, n) result (lst_ss)
      type(cons_t), intent(in) :: p
      integer(sz), intent(in) :: n
      type(cons_t) :: lst_ss

      type(cons_t) :: q
      integer(sz) :: prefix_length
      logical :: done

      prefix_length = 2
      q = cdr (p)
      done = .false.
      do while (.not. done)
         if (prefix_length == n) then
            ! The list is entirely in descending order. Reverse it.
            lst_ss = reversex (p)
            done = .true.
         else if (.not. less_than (cadr (q), car (q))) then
            ! The list is not entirely in descending order. Sort it
            ! as an array.
            lst_ss = array_sort_with_prefix_reversed (p, n, prefix_length)
            done = .true.
         else
            prefix_length = prefix_length + 1
            q = .tocons. cdr (q)
         end if
      end do
    end function small_sort_with_a_descending_prefix

    recursive function array_sort (p, n, prefix_length) result (lst_ss)
      !
      ! Put the data into an array and sort the array.
      !
      type(cons_t), intent(in) :: p
      integer(sz), intent(in) :: n
      integer(sz), intent(in) :: prefix_length
      type(cons_t) :: lst_ss

      type(cons_t) :: q
      integer(sz) :: i

      ! Fill workspace1 with the data.
      q = p
      do i = 0_sz, n - 1
         workspace1%array(i)%element = car (q)
         q = .tocons. cdr (q)
      end do

      ! Sort everything to the right of the ordered prefix.
      call stable_mergesort (less_than, workspace1, prefix_length, n - 1, workspace2)

      ! Merge the prefix and the tail.
      call merge_subarray (less_than, workspace1, 0_sz, prefix_length, n - 1, workspace2)

      ! Rather than allocate new CONS pairs, fill the original list
      ! with the sorted data.
      q = p
      do i = 0_sz, n - 1
         call set_car (q, workspace1%array(i)%element)
         q = .tocons. cdr (q)
      end do

      lst_ss = p
    end function array_sort

    recursive function array_sort_with_prefix_reversed (p, n, prefix_length) result (lst_ss)
      !
      ! Put the data into an array, with the prefix reversed, and sort
      ! the array.
      !
      type(cons_t), intent(in) :: p
      integer(sz), intent(in) :: n
      integer(sz), intent(in) :: prefix_length
      type(cons_t) :: lst_ss

      type(cons_t) :: q
      integer(sz) :: i

      ! Fill workspace1 with the data, reversing the prefix.
      q = p
      do i = prefix_length - 1, 0_sz, -1_sz
         workspace1%array(i)%element = car (q)
         q = .tocons. cdr (q)
      end do
      do i = prefix_length, n - 1
         workspace1%array(i)%element = car (q)
         q = .tocons. cdr (q)
      end do

      ! Sort everything to the right of the ordered prefix.
      call stable_mergesort (less_than, workspace1, prefix_length, n - 1, workspace2)

      ! Merge the prefix and the tail.
      call merge_subarray (less_than, workspace1, 0_sz, prefix_length, n - 1, workspace2)

      ! Rather than allocate new CONS pairs, fill the original list
      ! with the sorted data.
      q = p
      do i = 0_sz, n - 1
         call set_car (q, workspace1%array(i)%element)
         q = .tocons. cdr (q)
      end do

      lst_ss = p
    end function array_sort_with_prefix_reversed

  end function list_stable_sortx

  recursive function list_sort (less_than, lst) result (lst_ss)
    !
    ! The current implementation is just list_stable_sort.
    !
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    lst_ss = list_stable_sort (less_than, lst)
  end function list_sort

  recursive function list_sortx (less_than, lst) result (lst_ss)
    !
    ! The current implementation is just list_stable_sortx.
    !
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    lst_ss = list_stable_sortx (less_than, lst)
  end function list_sortx

  recursive function list_is_sorted (less_than, lst) result (is_sorted)
    procedure(list_predicate2_t) :: less_than
    class(*), intent(in) :: lst
    logical :: is_sorted

    class(*), allocatable :: last_value
    class(*), allocatable :: next_value
    class(*), allocatable :: next_pair
    logical :: done

    if (is_nil_list (lst)) then
       is_sorted = .true.
    else if (is_nil_list (cdr (lst))) then
       is_sorted = .true.
    else
       block
         type(gcroot_t) :: lst_root
         lst_root = lst
         call uncons (lst, last_value, next_pair)
         done = .false.
         do while (.not. done)
            if (is_nil_list (next_pair)) then
               is_sorted = .true.
               done = .true.
            else
               call uncons (next_pair, next_value, next_pair)
               if (less_than (next_value, last_value)) then
                  is_sorted = .false.
                  done = .true.
               else
                  last_value = next_value
               end if
            end if
         end do
         call lst_root%discard
       end block
    end if
  end function list_is_sorted

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function list_delete_neighbor_dupsx (pred, lst) result (lst_dnd)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_dnd

    type(gcroot_t) :: lst_root
    class(*), allocatable :: element
    class(*), allocatable :: p, q
    logical :: done

    lst_dnd = .autoval. lst
    if (is_pair (lst_dnd)) then
       lst_root = lst_dnd
       p = lst_dnd
       call uncons (p, element, q)
       do while (is_pair (q))
          if (.not. pred (element, car (q))) then
             ! Adjacent elements do not match. Advance to the next
             ! position.
             p = q
             call uncons (p, element, q)
          else
             ! A run of duplicates is detected. Now p is the pair that
             ! will have to have its CDR changed. Advance q to the
             ! end of the run.
             done = .false.
             do while (.not. done)
                if (is_not_pair (q)) then
                   ! q has reached the end of the input list.
                   call set_cdr (p, q)
                   done = .true.
                else if (.not. pred (element, car (q))) then
                   ! q has reached a mismatched element.
                   call set_cdr (p, q)
                   done = .true.
                else
                   call uncons (q, element, q)
                end if
             end do
          end if
       end do
    end if
    call lst_root%discard
  end function list_delete_neighbor_dupsx

  recursive function list_delete_neighbor_dups (pred, lst) result (lst_dnd)
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_dnd

    type(gcroot_t) :: lst_root

    if (is_not_pair (lst)) then
       lst_dnd = .autoval. lst
    else
       lst_root = lst ! Protect the original list from garbage
                      ! collections instigated by pred.
       lst_dnd = look_for_duplicates (.tocons. lst)
    end if
    call lst_root%discard

  contains

    recursive function look_for_duplicates (lst) result (retval)
      type(cons_t), intent(in) :: lst
      type(cons_t) :: retval

      type(gcroot_t) :: retval_root
      type(cons_t) :: p
      class(*), allocatable :: q
      type(cons_t) :: last_pair
      type(cons_t) :: new_segment
      type(cons_t) :: new_last_pair

      ! Find either the first adjacent duplicate pair or the end of
      ! the list.
      q = find_duplicates (lst)
      if (is_not_pair (q)) then
         ! There are no adjacent duplicates anywhere in the original
         ! list. Simply return the original list.
         retval = lst
      else
         ! A run of duplicates has been found. Build a new list.
         call copy_sublist (lst, q, retval, last_pair)
         retval_root = retval ! Protect the output list from garbage
                              ! collections instigated by pred.
         do while (is_pair (q))
            q = skip_duplicates (.tocons. q)
            if (is_not_pair (q)) then
               ! The input list ends on a run of duplicates. Terminate
               ! the output list.
               call set_cdr_unless_nil (last_pair, q)
            else
               ! Start a new segment.
               p = q
               q = find_duplicates (p)
               if (is_not_pair (q)) then
                  ! Append a shared tail.
                  call set_cdr (last_pair, p)
               else
                  ! Copy another section of the input list, and append
                  ! it to the output list.
                  call copy_sublist (p, q, new_segment, new_last_pair)
                  call set_cdr (last_pair, new_segment)
                  last_pair = new_last_pair
               endif
            end if
         end do
      end if
      call retval_root%discard
    end function look_for_duplicates

    recursive function find_duplicates (p) result (q)
      type(cons_t), intent(in) :: p
      class(*), allocatable :: q

      type(cons_t) :: prev
      logical :: done

      prev = p
      done = .false.
      do while (.not. done)
         q = cdr (prev)
         if (is_not_pair (q)) then
            ! q is the end of the input list.
            done = .true.
         else if (pred (car (prev), car (q))) then
            ! q is the start of a run of duplicates.
            done = .true.
         else
            ! Continue looking for a pair of duplicates.
            prev = q
         end if
      end do
    end function find_duplicates

    recursive function skip_duplicates (p) result (q)
      type(cons_t), intent(in) :: p
      class(*), allocatable :: q

      type(cons_t) :: prev
      logical :: done

      prev = p
      done = .false.
      do while (.not. done)
         q = cdr (prev)
         if (is_not_pair (q)) then
            ! q is the end of the input list.
            done = .true.
         else if (.not. pred (car (prev), car (q))) then
            ! q is the start of a new segment.
            done = .true.
         else
            ! Continue skipping duplicates.
            prev = q
         end if
      end do
    end function skip_duplicates

    subroutine set_cdr_unless_nil (pair, cdr_value)
      type(cons_t), intent(in) :: pair
      class(*), intent(in) :: cdr_value

      if (is_not_nil (cdr_value)) then
         call set_cdr (pair, cdr_value)
      end if
    end subroutine set_cdr_unless_nil

    subroutine copy_sublist (p, q, sublst_copy, last_pair)
      type(cons_t), intent(in) :: p
      class(*), intent(in) :: q
      type(cons_t), intent(out) :: sublst_copy
      type(cons_t), intent(out) :: last_pair

      class(*), allocatable :: head, tail
      type(cons_t) :: new_pair

      call uncons (p, head, tail)
      sublst_copy = head ** nil
      last_pair = sublst_copy
      do while (.not. cons_t_eq (tail, q))
         new_pair = (car (tail)) ** nil
         call set_cdr (last_pair, new_pair)
         last_pair = new_pair
         tail = cdr (tail)
      end do
    end subroutine copy_sublist

  end function list_delete_neighbor_dups

!!!-------------------------------------------------------------------

  recursive function vectar_is_sorted (less_than, vec) result (bool)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    logical :: bool

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data

    vec_root = vec

    vecr = vec
    data => vectar_data_ptr (vecr)
    bool = data_is_sorted (less_than, data, vecr%istart0(), vecr%iend0())

    call vec_root%discard
  end function vectar_is_sorted

  recursive function data_is_sorted (less_than, data, ileft, iright) result (bool)
    !
    ! Is data[ileft .. iright] free of any descending sequences?
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright
    logical::bool

    integer(sz) :: i

    bool = .true.
    i = ileft
    do while (bool .and. i < iright)
       bool = .not. less_than (data%array(i + 1)%element, data%array(i)%element)
       i = i + 1
    end do
  end function data_is_sorted

!!!-------------------------------------------------------------------

  recursive function vectar_merge (less_than, vec1, vec2) result (vec_m)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    type(vectar_t) :: vec_m

    type(vectar_range_t) :: vecr1
    type(vectar_range_t) :: vecr2

    vecr1 = vec1
    vecr2 = vec2
    vec_m = make_vectar (vecr1%length() + vecr2%length())
    call vectar_mergex (less_than, vec_m, vecr1, vecr2)
  end function vectar_merge

  recursive subroutine vectar_mergex (less_than, vec_m, vec1, vec2)

    !
    ! It is assumed that vec_m does not overlap either of vec1 or
    ! vec2. This assumption is carried over from SRFI-132.
    !

    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec_m
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2

    type(gcroot_t) :: vec_m_root, vec1_root, vec2_root
    type(vectar_range_t) :: vecr_m, vecr1, vecr2
    class(vectar_data_t), pointer :: data_m, data1, data2
    integer(sz) :: i0_m, i0_1, i0_2
    integer(sz) :: len_m, len1, len2
    integer(sz) :: i, j, k

    vec_m_root = vec_m
    vec1_root = vec1
    vec2_root = vec2

    vecr_m = vec_m
    i0_m = vecr_m%istart0()
    len_m = vecr_m%length()

    vecr1 = vec1
    i0_1 = vecr1%istart0()
    len1 = vecr1%length()

    vecr2 = vec2
    i0_2 = vecr2%istart0()
    len2 = vecr2%length()

    if (len_m /= len1 + len2) then
       call error_abort ("vectar_mergex arguments have incompatible lengths") ! LCOV_EXCL_LINE
    end if

    data_m => vectar_data_ptr (vecr_m)
    data1 => vectar_data_ptr (vecr1)
    data2 => vectar_data_ptr (vecr2)

    j = 0_sz
    k = 0_sz
    i = 0_sz
    do while (i < len_m)
       if (j == len1) then
          ! The rest of the result is from vecr2.
          do while (i < len_m)
             data_m%array(i0_m + i) = data2%array(i0_2 + k)
             k = k + 1
             i = i + 1
          end do
       else if (k == len2) then
          ! The rest of the result is from vecr1
          do while (i < len_m)
             data_m%array(i0_m + i) = data1%array(i0_1 + j)
             j = j + 1
             i = i + 1
          end do
       else if (.not. less_than (data2%array(i0_2 + k)%element, data1%array(i0_1 + j)%element)) then
          data_m%array(i0_m + i) = data1%array(i0_1 + j)
          j = j + 1
          i = i + 1
       else
          data_m%array(i0_m + i) = data2%array(i0_2 + k)
          k = k + 1
          i = i + 1
       end if
    end do

    call vec_m_root%discard
    call vec1_root%discard
    call vec2_root%discard
  end subroutine vectar_mergex

!!!-------------------------------------------------------------------

  recursive subroutine vectar_stable_sortx (less_than, vec)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec

    type(gcroot_t) :: vec_root, workspace_root
    type(vectar_range_t) :: vecr, workspace_range
    class(vectar_data_t), pointer :: data, workspace
    integer(sz) :: i, k

    vec_root = vec

    vecr = vec
    if (vecr%length() /= 0) then
       data => vectar_data_ptr (vecr)

       i = vecr%istart0()
       k = vecr%iend0()

       workspace_root = make_vectar (((k - i) + 1) / 2)
       workspace_range = workspace_root
       workspace => vectar_data_ptr (workspace_range)

       call stable_mergesort (less_than, data, i, k, workspace)
    end if

    call workspace_root%discard
    call vec_root%discard
  end subroutine vectar_stable_sortx

  recursive subroutine vectar_sortx (less_than, vec)
    !
    ! The current vectar_sortx is simply vectar_stable_sortx.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec

    call vectar_stable_sortx (less_than, vec)
  end subroutine vectar_sortx

  recursive function vectar_stable_sort (less_than, vec) result (vec_s)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_s

    vec_s = vectar_copy (vec)
    call vectar_stable_sortx (less_than, vec_s)
  end function vectar_stable_sort

  recursive function vectar_sort (less_than, vec) result (vec_s)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_s

    vec_s = vectar_copy (vec)
    call vectar_sortx (less_than, vec_s)
  end function vectar_sort

!!!-------------------------------------------------------------------

  function vectar_shuffle (vec) result (vec_shuffled)
    use, intrinsic :: iso_fortran_env, only: real64
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_shuffled

    !
    ! Fisher-Yates shuffle.
    !
    ! See
    ! https://en.wikipedia.org/w/index.php?title=Fisher%E2%80%93Yates_shuffle&oldid=1063206771#The_%22inside-out%22_algorithm
    !

    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(vectar_data_t), pointer :: data_shuffled
    integer(sz) :: i0, len
    real(real64) :: randnum
    integer(sz) :: i, j

    vecr = vec
    data => vectar_data_ptr (vecr)
    i0 = vecr%istart0()
    len = vecr%length()

    vec_shuffled = make_vectar (len)
    data_shuffled => vectar_data_ptr (vec_shuffled)

    do i = 0_sz, len - 1
       call random_number (randnum)
       j = int (randnum * (i + 1), kind = sz)
       if (j /= i) then
          data_shuffled%array(i) = data_shuffled%array(j)
       end if
       data_shuffled%array(j) = data%array(i0 + i)
    end do
  end function vectar_shuffle

  subroutine vectar_shufflex (vec)
    use, intrinsic :: iso_fortran_env, only: real64
    class(*), intent(in) :: vec

    !
    ! Fisher-Yates shuffle.
    !
    ! See
    ! https://en.wikipedia.org/w/index.php?title=Fisher%E2%80%93Yates_shuffle&oldid=1063206771#The_modern_algorithm
    !

    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: i0, len
    real(real64) :: randnum
    integer(sz) :: i, j
    class(*), allocatable :: tmp

    vecr = vec
    data => vectar_data_ptr (vecr)
    i0 = vecr%istart0()
    len = vecr%length()
    do i = 0_sz, len - 2
       call random_number (randnum)
       j = i + int (randnum * (len - i), kind = sz)
       tmp = data%array(i0 + i)%element
       data%array(i0 + i)%element = data%array(i0 + j)%element
       data%array(i0 + j)%element = tmp
    end do
  end subroutine vectar_shufflex

!!!-------------------------------------------------------------------

  recursive function vectar_delete_neighbor_dups (pred, vec) result (vec_dnd)
    use, intrinsic :: iso_fortran_env, only: int8
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_dnd

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    class(vectar_data_t), pointer :: data_dnd
    integer(int8), allocatable :: dup_marks(:)
    integer(sz) :: istart0, iend0
    integer(sz) :: i, j
    integer(sz) :: num_dups
    integer(sz) :: num_elements_minus_dups

    ! Protect against garbage collections instigated by pred.
    vec_root = vec

    vecr = vec
    data => vectar_data_ptr (vecr)
    istart0 = vecr%istart0()
    iend0 = vecr%iend0()
    allocate (dup_marks(istart0:iend0), source = 0_int8)

    ! Set marks where there are neighbor duplicates, and count the
    ! duplicates.
    num_dups = 0
    do i = istart0 + 1, iend0
       if (pred (data%array(i - 1)%element, data%array(i)%element)) then
          dup_marks(i) = 1_int8
          num_dups = num_dups + 1
       end if
    end do

    ! There are no more calls to pred. Discard the root.
    call vec_root%discard

    ! Make a new vectar, leaving out the marked entries.
    num_elements_minus_dups = vecr%length() - num_dups
    vec_dnd = make_vectar (num_elements_minus_dups)
    data_dnd => vectar_data_ptr (vec_dnd)
    j = 0_sz
    do i = istart0, iend0
       if (dup_marks(i) == 0_int8) then
          data_dnd%array(j) = data%array(i)
          j = j + 1
       end if
    end do
  end function vectar_delete_neighbor_dups

  recursive subroutine vectar_delete_neighbor_dupsx (pred, vec, num_elements_minus_dups)
    !
    ! The outputs are different from those of
    ! vector-delete-neighbor-dups! in SRFI-132.
    !
    use, intrinsic :: iso_fortran_env, only: int8
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: vec
    integer(sz), intent(out) :: num_elements_minus_dups

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: istart0, iend0
    integer(sz) :: i, j
    integer(sz) :: num_dups

    ! Protect against garbage collections instigated by pred.
    vec_root = vec

    vecr = vec
    data => vectar_data_ptr (vecr)
    istart0 = vecr%istart0()
    iend0 = vecr%iend0()

    ! Set marks where there are neighbor duplicates, and count the
    ! duplicates.
    num_dups = 0
    j = istart0
    do i = istart0 + 1, iend0
       if (pred (data%array(i - 1)%element, data%array(i)%element)) then
          data%array(j) = data%array(i)
          j = j + 1
          num_dups = num_dups + 1
       end if
    end do

    num_elements_minus_dups = vecr%length() - num_dups
  end subroutine vectar_delete_neighbor_dupsx

!!!-------------------------------------------------------------------

  recursive function vectar_selectx0_size_kind (less_than, vec, k) result (kth_smallest)
    use, intrinsic :: iso_fortran_env, only: real64
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: k
    class(*), allocatable :: kth_smallest

    !
    ! The implementation is a quickselect with randomly selected
    ! pivots. This algorithm has O(n) worst case expected running
    ! time.
    !

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: vecr_length
    integer(sz) :: i, j
    integer(sz) :: ipivot
    integer(sz) :: k_adjusted_for_range
    real(real64) :: randnum

    vec_root = vec

    vecr = vec
    vecr_length = vecr%length()
    if (k < 0 .or. vecr_length <= k) then
       ! LCOV_EXCL_START
       if (vecr_length == 0) then
          call error_abort ("attempted selection from an empty vectar or empty vectar range")
       else
          call error_abort ("selection index out of range")
       end if
       ! LCOV_EXCL_STOP
    else
       data => vectar_data_ptr (vecr)
       i = vecr%istart0()
       j = vecr%iend0()
       k_adjusted_for_range = k + i
       do while (i /= j)
          ! Pick a pivot at random.
          call random_number (randnum)
          ipivot = i + int (randnum * (j - i + 1), kind = sz)

          ! Partition around the pivot.
          call hoare_partitioning (less_than, data, i, j, ipivot, ipivot)

          if (ipivot < k_adjusted_for_range) then
             i = ipivot + 1
          else if (ipivot == k_adjusted_for_range) then
             i = ipivot
             j = ipivot
          else
             j = ipivot - 1
          end if
       end do
       kth_smallest = data%array(i)%element
    end if

    call vec_root%discard
  end function vectar_selectx0_size_kind

  recursive function vectar_selectx1_size_kind (less_than, vec, k) result (kth_smallest)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: k
    class(*), allocatable :: kth_smallest

    kth_smallest = vectar_selectx0_size_kind (less_than, vec, k - 1_sz)
  end function vectar_selectx1_size_kind

  recursive function vectar_selectxn_size_kind (less_than, vec, n, k) result (kth_smallest)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: k
    class(*), allocatable :: kth_smallest

    kth_smallest = vectar_selectx0_size_kind (less_than, vec, k - n)
  end function vectar_selectxn_size_kind

  recursive function vectar_selectx0_int (less_than, vec, k) result (kth_smallest)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer, intent(in) :: k
    class(*), allocatable :: kth_smallest

    kth_smallest = vectar_selectx0_size_kind (less_than, vec, .sz. k)
  end function vectar_selectx0_int

  recursive function vectar_selectx1_int (less_than, vec, k) result (kth_smallest)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer, intent(in) :: k
    class(*), allocatable :: kth_smallest

    kth_smallest = vectar_selectx1_size_kind (less_than, vec, .sz. k)
  end function vectar_selectx1_int

  recursive function vectar_selectxn_int (less_than, vec, n, k) result (kth_smallest)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: k
    class(*), allocatable :: kth_smallest

    kth_smallest = vectar_selectxn_size_kind (less_than, vec, .sz. n, .sz. k)
  end function vectar_selectxn_int

!!!-------------------------------------------------------------------

  recursive subroutine vectar_separatex_size_kind (less_than, vec, k)
    use, intrinsic :: iso_fortran_env, only: real64
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: k

    !
    ! The implementation is a quickselect with randomly selected
    ! pivots. This algorithm has O(n) worst case expected running
    ! time.
    !

    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    class(vectar_data_t), pointer :: data
    integer(sz) :: vecr_length
    integer(sz) :: i, j
    integer(sz) :: ipivot
    integer(sz) :: k1
    real(real64) :: randnum

    vec_root = vec

    vecr = vec
    vecr_length = vecr%length()
    if (k < 0 .or. vecr_length < k) then
       ! LCOV_EXCL_START
       if (vecr_length == 0) then
          call error_abort ("attempted selection from an empty vectar or empty vectar range")
       else
          call error_abort ("number out of range")
       end if
       ! LCOV_EXCL_STOP
    else if (0 < k .and. k < vecr_length) then
       data => vectar_data_ptr (vecr)
       i = vecr%istart0()
       j = vecr%iend0()
       k1 = k + i - 1
       do while (i /= j)
          ! Pick a pivot at random.
          call random_number (randnum)
          ipivot = i + int (randnum * (j - i + 1), kind = sz)

          ! Partition around the pivot.
          call hoare_partitioning (less_than, data, i, j, ipivot, ipivot)

          if (ipivot < k1) then
             i = ipivot + 1
          else if (ipivot == k1) then
             i = ipivot
             j = ipivot
          else
             j = ipivot - 1
          end if
       end do
    end if

    call vec_root%discard
  end subroutine vectar_separatex_size_kind

  recursive subroutine vectar_separatex_int (less_than, vec, k)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    integer, intent(in) :: k

    call vectar_separatex_size_kind (less_than, vec, .sz. k)
  end subroutine vectar_separatex_int

!!!-------------------------------------------------------------------

  recursive function vectar_find_median_mean_subr (less_than, vec, knil, mean_subr) result (median)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    class(*), intent(in) :: knil
    procedure(vectar_map2_subr_t) :: mean_subr
    class(*), allocatable :: median

    type(gcroot_t) :: x, y
    type(gcroot_t) :: knil_root
    type(gcroot_t) :: vec_root
    type(gcroot_t) :: vec_copy
    type(vectar_range_t) :: vecr
    integer(sz) :: n, half_n
    class(*), allocatable :: mean

    vec_root = vec
    knil_root = knil

    vecr = vec
    n = vecr%length()
    if (n == 0) then
       median = knil
    else
       vec_copy = vectar_copy (vecr)
       half_n = n / 2
       if (mod (n, 2_sz) == 1) then
          median = vectar_selectx0 (less_than, vec_copy, half_n)
       else
          call vectar_separatex (less_than, vec_copy, half_n)
          x = vectar_selectx0 (less_than, range0 (vec_copy, 0_sz, half_n - 1), half_n - 1)
          y = vectar_selectx0 (less_than, range0 (vec_copy, half_n, n - 1), 0_sz)
          call mean_subr (.val. x, .val. y, mean)
          median = mean
       end if
    end if

    call vec_root%discard
    call knil_root%discard
  end function vectar_find_median_mean_subr

  recursive function vectar_find_medianx_mean_subr (less_than, vec, knil, mean_subr) result (median)
    procedure(vectar_predicate2_t) :: less_than
    class(*), intent(in) :: vec
    class(*), intent(in) :: knil
    procedure(vectar_map2_subr_t) :: mean_subr
    class(*), allocatable :: median

    type(gcroot_t) :: x, y
    type(gcroot_t) :: knil_root
    type(gcroot_t) :: vec_root
    type(vectar_range_t) :: vecr
    integer(sz) :: n, half_n
    class(*), allocatable :: mean

    vec_root = vec
    knil_root = knil

    vecr = vec
    n = vecr%length()
    if (n == 0) then
       median = knil
    else
       call vectar_sortx (less_than, vecr)
       half_n = n / 2
       if (mod (n, 2_sz) == 1) then
          median = vectar_ref0 (vecr, half_n)
       else
          x = vectar_ref0 (vecr, half_n - 1)
          y = vectar_ref0 (vecr, half_n)
          call mean_subr (.val. x, .val. y, mean)
          median = mean
       end if
    end if

    call vec_root%discard
    call knil_root%discard
  end function vectar_find_medianx_mean_subr

!!!-------------------------------------------------------------------
!!!
!!! `Bottenbruch searches': binary search procedures that do not do an
!!! equality test.
!!!

  recursive function bottenbruch_search (less_than, data, ileft, iright, x) result (index)
    !
    ! Do a search on the data whose first element is at ileft and
    ! whose last element is at iright. Return `index' such that:
    !
    !    * if x is less than the element at ileft, then index = ileft;
    !
    !    * otherwise, x is greater than or equal to the element at
    !      index (and therefore to every element to the left of
    !      index), and less than everything to the right of index.
    !
    ! References:
    !
    !    * H. Bottenbruch, `Structure and use of ALGOL 60', Journal of
    !      the ACM, Volume 9, Issue 2, April 1962,
    !      pp.161-221. https://doi.org/10.1145/321119.321120
    !      The general algorithm is described on pages 214 and 215.
    !
    !    * https://en.wikipedia.org/w/index.php?title=Binary_search_algorithm&oldid=1062988272#Alternative_procedure
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright
    class(*), intent(in) :: x
    integer(sz) :: index

    integer(sz) :: i, j, k
    integer(sz) :: k_minus_j

    j = ileft
    k = iright
    do while (k /= j)
       ! Set i := ceil ((j + k) / 2).
       k_minus_j = k - j
       i = j + ishft (k_minus_j, -1) + ibits (k_minus_j, 0, 1)
       if (less_than (x, data%array(i)%element)) then
          k = i - 1
       else
          j = i
       end if
    end do
    index = j
  end function bottenbruch_search

  recursive function bottenbruch_search2 (less_than, data, ileft, iright, x) result (index)
    !
    ! Do a search on the data whose first element is at ileft and
    ! whose last element is at iright. Return `index' such that:
    !
    !    * if x is greater than the element at iright, then index =
    !      iright;
    !
    !    * otherwise, x is greater than every element to the left of
    !      index, and less than or equal to the elements at index and
    !      to the right of index.
    !
    ! References:
    !
    !    * H. Bottenbruch, `Structure and use of ALGOL 60', Journal of
    !      the ACM, Volume 9, Issue 2, April 1962,
    !      pp.161-221. https://doi.org/10.1145/321119.321120
    !      The general algorithm is described on pages 214 and 215.
    !
    !    * https://en.wikipedia.org/w/index.php?title=Binary_search_algorithm&oldid=1062988272#Alternative_procedure
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright
    class(*), intent(in) :: x
    integer(sz) :: index

    integer(sz) :: i, j, k
    integer(sz) :: k_minus_j

    j = ileft
    k = iright
    do while (k /= j)
       ! Set i := floor ((j + k) / 2).
       k_minus_j = k - j
       i = j + ishft (k_minus_j, -1)
       if (less_than (data%array(i)%element, x)) then
          ! x is greater than the element at i.
          j = i + 1
       else
          k = i
       end if
    end do
    index = j
  end function bottenbruch_search2

  subroutine unit_test__bottenbruch_search
    use, intrinsic :: iso_fortran_env, only: real64
    integer, parameter :: number_of_vectars = 100
    integer(sz), parameter :: veclen = 1024_sz
    integer, parameter :: modulus = 64

    type(gcroot_t) :: vec
    integer :: vectar_number
    integer(sz) :: i
    real(real64) :: randnum
    integer :: x
    class(vectar_data_t), pointer :: data

    vec = make_vectar (veclen)
    do vectar_number = 1, number_of_vectars
       do i = 0_sz, veclen - 1
          call random_number (randnum)
          call vectar_set0 (vec, i, int (randnum * (modulus)))
       end do
       vec = list_to_vectar (list_sortx (int_less_than, vectar_to_list (vec)))
       data => vectar_data_ptr (vec)
       do x = -1, modulus
          i = bottenbruch_search (int_less_than, data, 0_sz, veclen - 1, x)
          if (i /= 0) then
             if (int_less_than (x, vectar_ref0 (vec, i))) then
                ! Unless i is 0, x must be greater than or equal
                ! to the element at i (and therefore also to every
                ! element to the left of i).
                call error_abort ("unit_test__bottenbruch_search__test1 0010 failed") ! LCOV_EXCL_LINE
             end if
          end if
          if (i /= veclen - 1) then
             if (.not. int_less_than (x, vectar_ref0 (vec, i + 1))) then
                ! x must be less than the element at i+1 (and
                ! therefore also to everything else to the right of
                ! i).
                call error_abort ("unit_test__bottenbruch_search__test1 0020 failed") ! LCOV_EXCL_LINE
             end if
          end if
       end do
    end do
  end subroutine unit_test__bottenbruch_search

  subroutine unit_test__bottenbruch_search2
    use, intrinsic :: iso_fortran_env, only: real64
    integer, parameter :: number_of_vectars = 100
    integer(sz), parameter :: veclen = 1024_sz
    integer, parameter :: modulus = 64

    type(gcroot_t) :: vec
    integer :: vectar_number
    integer(sz) :: i
    real(real64) :: randnum
    integer :: x
    class(vectar_data_t), pointer :: data

    vec = make_vectar (veclen)
    do vectar_number = 1, number_of_vectars
       do i = 0_sz, veclen - 1
          call random_number (randnum)
          call vectar_set0 (vec, i, int (randnum * (modulus)))
       end do
       vec = list_to_vectar (list_sortx (int_less_than, vectar_to_list (vec)))
       data => vectar_data_ptr (vec)
       do x = -1, modulus
          i = bottenbruch_search2 (int_less_than, data, 0_sz, veclen - 1, x)
          if (i /= veclen - 1) then
             if (int_less_than (vectar_ref0 (vec, i), x)) then
                ! Unless i is veclen - 1, x must be less than or equal
                ! to the element at i (and therefore also to every
                ! element to the right of i).
                call error_abort ("unit_test__bottenbruch_search__test1 0010 failed") ! LCOV_EXCL_LINE
             end if
          end if
          if (i /= 0) then
             if (.not. int_less_than (vectar_ref0 (vec, i - 1), x)) then
                ! x must be greater than the element at i-1 (and
                ! therefore greater than everything to the left of i).
                call error_abort ("unit_test__bottenbruch_search__test1 0020 failed") ! LCOV_EXCL_LINE
             end if
          end if
       end do
    end do
  end subroutine unit_test__bottenbruch_search2

  subroutine unit_test__bottenbruch_searches
    call unit_test__bottenbruch_search
    call unit_test__bottenbruch_search2
  end subroutine unit_test__bottenbruch_searches

!!!-------------------------------------------------------------------
!!!
!!! Hoare-style partitioning for use in quicksort and quickselect
!!! implementations.
!!!
  
  recursive subroutine hoare_partitioning (less_than, data, istart, iend, ipivot, ipivot_final)
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: istart
    integer(sz), intent(in) :: iend
    integer(sz), intent(in) :: ipivot
    integer(sz), intent(inout) :: ipivot_final

    integer(sz) :: i, j
    class(*), allocatable :: pivot

    ! Move the pivot to the last element.
    call swap_entries (ipivot, iend)

    pivot = data%array(iend)%element

    i = istart - 1
    j = iend
    do while (i /= j)
       ! Move i so everything to the left of i is less than or equal
       ! to the pivot.
       i = i + 1
       do while (i /= j .and. .not. less_than (pivot, data%array(i)%element))
          i = i + 1
       end do

       ! Move j so everything to the right of j is greater than or
       ! equal to the pivot.
       if (i /= j) then
          j = j - 1
          do while (i /= j .and. .not. less_than (data%array(j)%element, pivot))
             j = j - 1
          end do
       end if

       call swap_entries (i, j)
    end do

    ! Move the pivot to its proper position, with everything that is
    ! less than the pivot to its left.
    call swap_entries (i, iend)

    ipivot_final = i

  contains

    subroutine swap_entries (i, j)
      integer(sz), intent(in) :: i, j

      class(*), allocatable :: tmp

      if (i /= j) then
         tmp = data%array(i)%element
         data%array(i)%element = data%array(j)%element
         data%array(j)%element = tmp
      end if
    end subroutine swap_entries

  end subroutine hoare_partitioning

  subroutine unit_test__hoare_partitioning

    type(gcroot_t) :: vec1, vec2
    integer(sz) :: i, j
    real :: randnum

    call check_vectar_and_pivot_index (vectar (5), 0_sz)

    call check_vectar_and_pivot_index (vectar (5, 6), 0_sz)
    call check_vectar_and_pivot_index (vectar (5, 6), 1_sz)

    call check_vectar_and_pivot_index (vectar (6, 5), 0_sz)
    call check_vectar_and_pivot_index (vectar (6, 5), 1_sz)

    call check_vectar_and_pivot_index (vectar (5, 5), 0_sz)
    call check_vectar_and_pivot_index (vectar (5, 5), 1_sz)

    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 4), 0_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 4), 1_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 4), 2_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 4), 3_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 4), 4_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 4), 5_sz)

    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 5), 0_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 5), 1_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 5), 2_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 5), 3_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 5), 4_sz)
    call check_vectar_and_pivot_index (vectar (5, 3, 1, 6, 2, 5), 5_sz)

    call check_vectar_and_pivot_index (vectar (5, 5, 5, 5, 5, 5), 0_sz)
    call check_vectar_and_pivot_index (vectar (5, 5, 5, 5, 5, 5), 1_sz)
    call check_vectar_and_pivot_index (vectar (5, 5, 5, 5, 5, 5), 2_sz)
    call check_vectar_and_pivot_index (vectar (5, 5, 5, 5, 5, 5), 3_sz)
    call check_vectar_and_pivot_index (vectar (5, 5, 5, 5, 5, 5), 4_sz)
    call check_vectar_and_pivot_index (vectar (5, 5, 5, 5, 5, 5), 5_sz)

    ! Test on vectars with randomized contents.
    do i = 1, 101, 5
       vec1 = make_vectar (i)
       do j = 1, vectar_length (vec1)
          call random_number (randnum)
          call vectar_set1 (vec1, j, int (randnum * 20_sz, kind = sz))
       end do
       do j = 1, vectar_length (vec1)
          vec2 = vectar_copy (vec1)
          call check_vectar_and_pivot_index (vec2, j - 1)
       end do
    end do

  contains

    subroutine check_vectar_and_pivot_index (vec, ipivot)
      class(*), intent(in) :: vec
      integer(sz), intent(in) :: ipivot

      type(gcroot_t) :: vec_root
      type(vectar_range_t) :: vecr
      class(vectar_data_t), pointer :: data
      integer(sz) :: imiddle
      integer :: pivot

      vec_root = vec

      vecr = vec
      data => vectar_data_ptr (vecr)
      select type (x => data%array(ipivot)%element)
      type is (integer)
         pivot = x
      end select
      call hoare_partitioning (int_less_than, data, vecr%istart0(), vecr%iend0(), ipivot, imiddle)
      call check_partition (data, vecr%istart0(), imiddle, vecr%iend0(), pivot)

      call vec_root%discard
    end subroutine check_vectar_and_pivot_index

    subroutine check_partition (data, istart, imiddle, iend, pivot)
      class(vectar_data_t), pointer, intent(in) :: data
      integer(sz), intent(in) :: istart
      integer(sz), intent(in) :: imiddle
      integer(sz), intent(in) :: iend
      integer, intent(in) :: pivot

      integer(sz) :: i

      select type (x => data%array(imiddle)%element)
      type is (integer)
         if (x /= pivot) then
            call error_abort ("unit_test__hoare_partitioning 0010 failed")
         end if
      end select
      do i = istart, imiddle - 1
         if (int_less_than (data%array(imiddle)%element, data%array(i)%element)) then
            call error_abort ("unit_test__hoare_partitioning 0020 failed")
         end if
      end do
      do i = imiddle + 1, iend
         if (int_less_than (data%array(i)%element, data%array(imiddle)%element)) then
            call error_abort ("unit_test__hoare_partitioning 0030 failed")
         end if
      end do
    end subroutine check_partition

  end subroutine unit_test__hoare_partitioning

!!!-------------------------------------------------------------------
!!!
!!! Stable sorting of an array of vectar_element_t.
!!!

  recursive subroutine stable_mergesort (less_than, data, i, k, workspace)
    !
    ! Sort data[i .. k], using a workspace at least
    ! floor((k - i + 1) / 2) vectar_element_t long.
    !
    ! The overall method is depth-first, top-down recursion on halves
    ! of the array.
    !
    ! We tried a bottom-up method similar to that employed in Python's
    ! sort function, and it worked and seemed fast. However, we
    ! decided that top-down recursion was easier to understand and get
    ! right.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i, k
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz), parameter :: small_length = 64

    integer(sz) :: len
    integer(sz) :: j

    if (i < k) then
       len = k - i + 1
       if (len <= small_length) then
          call small_stable_sort (less_than, data, i, k)
       else
          j = i + ishft (len, -1) + ibits (len, 0, 1)
          call stable_mergesort (less_than, data, i, j - 1, workspace)
          call stable_mergesort (less_than, data, j, k, workspace)
          call merge_subarray (less_than, data, i, j, k, workspace)
       end if
    end if
  end subroutine stable_mergesort

  recursive subroutine small_stable_sort (less_than, data, i, k)
    !
    ! Sort data[i .. k], where i < k.
    !
    ! FIXME: If k - u is largish, perhaps use a mergesort instead of
    !        going straight to insertion sort.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i, k

    integer(sz) :: u
    logical :: done

    u = i + 1
    if (.not. less_than (data%array(u)%element, data%array(i)%element)) then
       ! The initial pair is non-descending. See how long this trend
       ! continues.
       done = .false.
       do while (.not. done)
          if (u == k) then
             ! The end of the data has been reached. The data was
             ! already sorted!
             done = .true.
          else if (less_than (data%array(u + 1)%element, data%array(u)%element)) then
             ! The ascending sequence has ended. Insert the rest of
             ! the data.
             call stable_binary_insertion_sort (less_than, data, i, u, k)
             done = .true.
          else
             u = u + 1
          end if
       end do
    else
       ! The initial pair is descending. See how long this trend
       ! continues.
       done = .false.
       do while (.not. done)
          if (u == k) then
             ! The end of the data has been reached. The data is
             ! already sorted in descending order. Reverse it.
             call reverse_in_place (data, i, k)
             done = .true.
          else if (.not. less_than (data%array(u + 1)%element, data%array(u)%element)) then
             ! The descending sequence has ended. Reverse it and then
             ! insert the rest of the data.
             call reverse_in_place (data, i, u)
             call stable_binary_insertion_sort (less_than, data, i, u, k)
             done = .true.
          else
             u = u + 1
          end if
       end do
    end if
  end subroutine small_stable_sort

  recursive subroutine merge_subarray (less_than, data, i, j, k, workspace)
    !
    ! Merge data[i .. j-1] with data[j .. k].
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i, j, k
    class(vectar_data_t), pointer, intent(in) :: workspace

    if (.not. less_than (data%array(j)%element, data%array(j - 1)%element)) then
       ! data[i .. k] is already sorted. No merging is necessary.
       continue
    else
       ! Some merging will be necessary.
       call merge_unsorted_subarray (less_than, data, i, j, k, workspace)
    end if
  end subroutine merge_subarray

  recursive subroutine merge_unsorted_subarray (less_than, data, i, j, k, workspace)
    !
    ! Merge data[i .. j-1] with data[j .. k].
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i, j, k
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz) :: i1

    i1 = bottenbruch_search (less_than, data, i, j - 1, data%array(j)%element)
    if (i1 == i) then
       ! Skip to the next step.
       call merge_subarray_with_left_side_shortened (less_than, data, i, j, k, workspace)
    else
       ! Everything to the left of i1 is already in place and can be
       ! left alone.
       call merge_subarray_with_left_side_shortened (less_than, data, i1, j, k, workspace)
    end if
  end subroutine merge_unsorted_subarray

  recursive subroutine merge_subarray_with_left_side_shortened (less_than, data, i, j, k, workspace)
    !
    ! Merge data[i .. j-1] with data[j .. k].
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i, j, k
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz) :: k1

    k1 = bottenbruch_search2 (less_than, data, j, k, data%array(j - 1)%element)
    if (k1 == k) then
       ! Skip to the next step.
       call merge_subarray_with_sides_shortened (less_than, data, i, j, k, workspace)
    else
       ! Everything at k1 and to its right is already in place and can
       ! be left alone.
       call merge_subarray_with_sides_shortened (less_than, data, i, j, k1 - 1, workspace)
    end if
  end subroutine merge_subarray_with_left_side_shortened

  recursive subroutine merge_subarray_with_sides_shortened (less_than, data, i, j, k, workspace)
    !
    ! Merge data[i .. j-1] with data[j .. k].
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i, j, k
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz) :: u

    if (j - i < k - j) then
       ! The left side will fit in a workspace of half the total
       ! length of the array. Copy the left side to workspace, then
       ! merge leftwards, filling the abandoned space.
       do u = i, j - 1
          workspace%array(u - i) = data%array(u)
       end do
       call merge_going_leftwards (less_than, data, i, j, k, workspace, 0_sz, j - i - 1)
    else
       ! The right side will fit in a workspace of half the total
       ! length of the array. Copy the right side to workspace, then
       ! merge rightwards, filling the abandoned space.
       do u = j, k
          workspace%array(u - j) = data%array(u)
       end do
       call merge_going_rightwards (less_than, data, i, j - 1, k, workspace, 0_sz, k - j)
    end if
  end subroutine merge_subarray_with_sides_shortened

  recursive subroutine stable_binary_insertion_sort (less_than, data, ileft, ipresorted, iright)
    !
    ! Sort the data whose first element is at ileft and whose last
    ! element is at iright, presuming everything from ileft to
    ! ipresorted (inclusively) is already sorted.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: ipresorted
    integer(sz), intent(in) :: iright

    integer(sz) :: i
    integer(sz) :: j
    class(vectar_element_t), pointer :: p_ileft
    class(vectar_element_t), pointer :: p_i
    class(vectar_element_t), allocatable :: elem_i

    p_ileft => data%array(ileft)
    do i = ipresorted + 1, iright
       p_i => data%array(i)
       j = bottenbruch_search (less_than, data, ileft, i - 1, p_i%element)
       if (j == ileft) then
          if (less_than (p_i%element, p_ileft%element)) then
             call insert_at_j
          else
             call insert_after_j
          end if
       else
          call insert_after_j
       end if
    end do

  contains

    subroutine insert_at_j
      elem_i = p_i
      call move_elements_right (j, i - 1)
      data%array(j) = elem_i
    end subroutine insert_at_j

    subroutine insert_after_j
      if (j + 1 /= i) then
         elem_i = p_i
         call move_elements_right (j + 1, i - 1)
         data%array(j + 1) = elem_i
      end if
    end subroutine insert_after_j

    subroutine move_elements_right (ifirst, ilast)
      integer(sz), intent(in) :: ifirst
      integer(sz), intent(in) :: ilast

      integer(sz) :: k

      do k = ilast, ifirst, -1_sz
         data%array(k + 1) = data%array(k)
      end do
    end subroutine move_elements_right

  end subroutine stable_binary_insertion_sort

  subroutine reverse_in_place (data, ileft, iright)
    !
    ! Reverse the data whose first element is at ileft and whose last
    ! element is at iright.
    !
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iright

    class(vectar_element_t), allocatable :: tmp
    integer(sz) :: i, j

    i = ileft
    j = iright
    do while (i < j)
       tmp = data%array(i)
       data%array(i) = data%array(j)
       data%array(j) = tmp
       i = i + 1
       j = j - 1
    end do
  end subroutine reverse_in_place

  recursive subroutine merge_going_leftwards (less_than, data2, itarget, irunstart2, irunend2, &
       &                                      data1, irunstart1, irunend1)
    !
    ! Currently this is just a straightforward stable merge, filling
    ! empty space at the left of data2, with data1 as the run having
    ! priority in the merge.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data2
    integer(sz), intent(in) :: itarget
    integer(sz), intent(in) :: irunstart2
    integer(sz), intent(in) :: irunend2
    class(vectar_data_t), pointer, intent(in) :: data1
    integer(sz), intent(in) :: irunstart1
    integer(sz), intent(in) :: irunend1

    integer(sz) :: i, i1, i2
    integer(sz) :: last1, last2

    i = itarget
    last1 = irunend1 + 1
    last2 = irunend2 + 1
    i1 = irunstart1
    i2 = irunstart2
    do while (i /= last2)
       if (i1 == last1) then
          ! The rest of the merger is from data2 and already is in
          ! place.
          if (i /= i2) then
             call error_abort ("internal error in merge_going_leftwards") ! LCOV_EXCL_LINE
          end if
          i = last2
       else if (i2 == last2) then
          ! Copy the remainder of data1.
          do while (i1 /= last1)
             data2%array(i) = data1%array(i1)
             i = i + 1
             i1 = i1 + 1
          end do
       else if (less_than (data2%array(i2)%element, data1%array(i1)%element)) then
          ! The element from data2 is strictly less than the element
          ! from data1, and so belongs in front. Move the element in
          ! data2 leftwards.
          data2%array(i) = data2%array(i2)
          i = i + 1
          i2 = i2 + 1
       else
          ! Copy an element from data1.
          data2%array(i) = data1%array(i1)
          i = i + 1
          i1 = i1 + 1
       end if
    end do
  end subroutine merge_going_leftwards

  recursive subroutine merge_going_rightwards (less_than, data1, irunstart1, irunend1, itarget, &
       &                                       data2, irunstart2, irunend2)
    !
    ! Currently this is just a straightforward stable merge, filling
    ! empty space at the right of data1, with data1 as the run having
    ! priority in the merge.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data1
    integer(sz), intent(in) :: irunstart1
    integer(sz), intent(in) :: irunend1
    integer(sz), intent(in) :: itarget
    class(vectar_data_t), pointer, intent(in) :: data2
    integer(sz), intent(in) :: irunstart2
    integer(sz), intent(in) :: irunend2

    integer(sz) :: i, i1, i2
    integer(sz) :: last1, last2

    i = itarget
    last1 = irunstart1 - 1
    last2 = irunstart2 - 1
    i1 = irunend1
    i2 = irunend2
    do while (i /= last1)
       if (i2 == last2) then
          ! The rest of the merger is from data1 and already is in
          ! place.
          if (i /= i1) then
             call error_abort ("internal error in merge_going_rightwards") ! LCOV_EXCL_LINE
          end if
          i = last1
       else if (i1 == last1) then
          ! Copy the remainder of data2
          do while (i2 /= last2)
             data1%array(i) = data2%array(i2)
             i = i - 1
             i2 = i2 - 1
          end do
       else if (less_than (data2%array(i2)%element, data1%array(i1)%element)) then
          ! The element from data2 is strictly less than the element
          ! from data1, and so belongs in front. Move the element in
          ! data1 rightwards.
          data1%array(i) = data1%array(i1)
          i = i - 1
          i1 = i1 - 1
       else
          ! Copy an element from data2.
          data1%array(i) = data2%array(i2)
          i = i - 1
          i2 = i2 - 1
       end if
    end do
  end subroutine merge_going_rightwards

!!!-------------------------------------------------------------------

end module sorting_and_selection
