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
!


module sorting_and_selection
  !
  ! Sorting and selection procedures, based on Scheme Request for
  ! Implementation 132 (SRFI-132).
  ! https://srfi.schemers.org/srfi-132/srfi-132.html
  !
  ! See also SRFI-95 (https://srfi.schemers.org/srfi-95/srfi-95.html).
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

  ! Some unit tests. Not for use in a program other than for testing.
  public :: vectar_stable_mergesort_unit_tests

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

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

!!!-------------------------------------------------------------------

contains

!!!-------------------------------------------------------------------

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module sorting_and_selection error: ", a)') msg
    error stop
  end subroutine error_abort_1

!!$  subroutine strange_error
!!$    call error_abort ("a strange error, possibly use of an object already garbage-collected")
!!$  end subroutine strange_error

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

  function int_equal (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = .false.
    select type (x)
    type is (integer)
       select type (y)
       type is (integer)
          bool = (x == y)
       end select
    end select
  end function int_equal

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
                  else if (is_not_pair (p2)) then
                     call set_cdr (cursor, p1)
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
                  if (is_not_pair (p1)) then
                     call set_cdr (cursor, p2)
                     p1_is_active_is_changed = .true.
                     done = .true.
                  else if (is_not_pair (p2)) then
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

    integer, parameter :: small_size = 10

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

    recursive function insertion_sort (p, n) result (lst_ss)
      !
      ! Put CONS pairs into an array and do an insertion sort on the
      ! array. (This was faster than a list insertion sort I wrote.)
      !
      type(gcroot_t), intent(in) :: p
      integer(sz), intent(in) :: n
      type(cons_t) :: lst_ss

      type(cons_t), dimension(1:small_size) :: array
      type(cons_t) :: q, x
      integer(sz) :: i, j
      logical :: done

      ! Fill the array with CONS pairs.
      q = p
      do i = 1, n
         array(i) = q
         q = cdr (q)
      end do

      ! Do an insertion sort on the array.
      do i = 2, n
         x = array(i)
         j = i - 1
         done = .false.
         do while (.not. done)
            if (j == 0) then
               done = .true.
            else if (.not. less_than (car (x), car (array(j)))) then
               done = .true.
            else
               array(j + 1) = array(j)
               j = j - 1
            end if
         end do
         array(j + 1) = x
      end do

      ! Connect the CONS pairs into a list.
      call set_cdr (array(n), nil)
      do i = n - 1, 1, -1
         call set_cdr (array(i), array(i + 1))
      end do

      ! The result.
      lst_ss = array(1)
    end function insertion_sort

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

      if (n <= small_size) then
         if (list_is_sorted (less_than, p)) then
            ! Save a lot of activity, if the segment is already
            ! sorted.
            lst_ss = p
         else
            lst_ss = insertion_sort (p, n)
         end if
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
       call error_abort ("vectar_mergex arguments have incompatible lengths")
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
    integer(sz) :: idatastart, idataend

    vec_root = vec

    vecr = vec
    data => vectar_data_ptr (vecr)

    idatastart = vecr%istart0()
    idataend = vecr%iend0()

    workspace_root = make_vectar (((idataend - idatastart) + 1) / 2)
    workspace_range = workspace_root
    workspace => vectar_data_ptr (workspace_range)

    call stable_mergesort (less_than, data, idatastart, idataend, workspace)

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

  recursive function data_is_sorted (less_than, data, ileft, iright) result (bool)
    !
    ! Is dataileft .. iright free of any descending sequences?
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

  subroutine unit_test__bottenbruch_search__test1
    integer, parameter :: number_of_vectars = 100
    integer(sz), parameter :: veclen = 1024_sz
    integer, parameter :: modulus = 64

    type(gcroot_t) :: vec
    integer :: vectar_number
    integer(sz) :: i
    real :: randnum
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
                call error_abort ("unit_test__bottenbruch_search__test1 0010 failed")
             end if
          end if
          if (i /= veclen - 1) then
             if (.not. int_less_than (x, vectar_ref0 (vec, i + 1))) then
                ! x must be less than the element at i+1 (and
                ! therefore also to everything else to the right of
                ! i).
                call error_abort ("unit_test__bottenbruch_search__test1 0020 failed")
             end if
          end if
       end do
    end do
  end subroutine unit_test__bottenbruch_search__test1

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

  subroutine unit_test__bottenbruch_search2__test1
    integer, parameter :: number_of_vectars = 100
    integer(sz), parameter :: veclen = 1024_sz
    integer, parameter :: modulus = 64

    type(gcroot_t) :: vec
    integer :: vectar_number
    integer(sz) :: i
    real :: randnum
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
                call error_abort ("unit_test__bottenbruch_search__test1 0010 failed")
             end if
          end if
          if (i /= 0) then
             if (.not. int_less_than (vectar_ref0 (vec, i - 1), x)) then
                ! x must be greater than the element at i-1 (and
                ! therefore greater than everything to the left of i).
                call error_abort ("unit_test__bottenbruch_search__test1 0020 failed")
             end if
          end if
       end do
    end do
  end subroutine unit_test__bottenbruch_search2__test1

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

  recursive subroutine gather_an_increasing_run (less_than, data, ileft, iend, iright)
    !
    ! Set iright so from ileft to iright, inclusive, there is a
    ! (non-strictly) increasing run of elements. Elements may be
    ! reversed to make it so.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iend
    integer(sz), intent(out) :: iright

    integer(sz) :: itrial
    logical :: done

    if (ileft == iend) then
       ! A final run of one.
       iright = ileft
    else
       iright = ileft + 1
       if (less_than (data%array(iright)%element, data%array(ileft)%element)) then
          ! The sequence is strictly decreasing. Reverse it and so get a
          ! strictly increasing sequence. (Reversing a non-strictly
          ! decreasing sequence would be an unstable sort.)
          done = .false.
          do while (.not. done)
             if (iright == iend) then
                ! This is the final run.
                done = .true.
             else
                itrial = iright + 1
                if (.not. less_than (data%array(itrial)%element, data%array(iright)%element)) then
                   ! The next element would be equal or increasing.
                   done = .true.
                else
                   ! Keep going.
                   iright = itrial
                end if
             end if
          end do
          ! Make the sequence increasing.
          call reverse_in_place (data, ileft, iright)
       else
          ! The sequence is increasing.
          done = .false.
          do while (.not. done)
             if (iright == iend) then
                ! This is the final run.
                done = .true.
             else
                itrial = iright + 1
                if (less_than (data%array(itrial)%element, data%array(iright)%element)) then
                   ! The next element would be decreasing.
                   done = .true.
                else
                   ! Keep going.
                   iright = itrial
                end if
             end if
          end do
       end if
    end if
  end subroutine gather_an_increasing_run

  recursive subroutine gather_an_adequately_long_increasing_run (less_than, data, ileft, iend, min_length, iright)
    !
    ! Set iright so from ileft to iright, inclusively, there is a
    ! (non-strictly) increasing run of elements, either of the given
    ! min_length or at end of the data. Elements may be sorted to make
    ! it so.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: ileft
    integer(sz), intent(in) :: iend
    integer(sz), intent(in) :: min_length
    integer(sz), intent(out) :: iright

    integer(sz) :: ipresorted

    call gather_an_increasing_run (less_than, data, ileft, iend, ipresorted)
    if (ipresorted == iend .or. min_length - 1 <= ipresorted - ileft) then
       iright = ipresorted
    else
       iright = min (iend, ileft + (min_length - 1))
       call stable_binary_insertion_sort (less_than, data, ileft, ipresorted, iright)
    end if
  end subroutine gather_an_adequately_long_increasing_run

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
             call error_abort ("internal error in merge_going_leftwards")
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
             call error_abort ("internal error in merge_going_rightwards")
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

  recursive subroutine merge_two_runs (less_than, data, i, j, k, workspace)
    !
    ! Merge sorted datai .. j-1 and sorted dataj .. k, giving
    ! sorted datai .. k.
    !
    ! `workspace' is a vectar of length at least
    !
    !     floor ((k - i + 1) / 2)
    !
    ! It has to be protected from garbage collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: i
    integer(sz), intent(in) :: j
    integer(sz), intent(in) :: k
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz) :: i1, k1
    integer(sz) :: u

    ! Find some elements on the left that definitely will not need to
    ! be moved, because they are less than or equal to the first
    ! element of the right side of the merge.
    i1 = bottenbruch_search (less_than, data, i, j - 1, data%array(j)%element)

    ! Find some elements on the right that definitely will not need to
    ! be moved, because they are greater than or equal to the last
    ! element of the left side of the merge.
    k1 = bottenbruch_search2 (less_than, data, j, k, data%array(j - 1)%element)
    if (k1 /= k) then
       k1 = k1 - 1
    end if

    if (j - i1 < k1 - j) then
       ! The left side is shorter than or equal in length to the right
       ! side. Copy the left side to workspace, then merge leftwards.
       do u = i1, j - 1
          workspace%array(u - i1) = data%array(u)
       end do
       call merge_going_leftwards (less_than, data, i1, j, k1, workspace, 0_sz, j - i1 - 1)
    else
       ! The left side is longer than the right side side. Copy the
       ! right side to workspace, then merge rightwards.
       do u = j, k1
          workspace%array(u - j) = data%array(u)
       end do
       call merge_going_rightwards (less_than, data, i1, j - 1, k1, workspace, 0_sz, k1 - j)
    end if
  end subroutine merge_two_runs

  recursive subroutine restore_run_stack_invariant (less_than, data, run_stack, stack_count, workspace)
    !
    ! Merge run_stack contents until the invariant is met.
    !
    ! The stack invariant is taken from the corrected later versions
    ! of Timsort. See
    ! envisage-project.eu/proving-android-java-and-python-sorting-algorithm-is-broken-and-how-to-fix-it/
    !
    ! Note that the run_stack indexing starts at position 0. All other
    ! entries are the end indices of runs; we put a fake end index in
    ! position 0. The stack_size counter does not count the fake stack
    ! entry.
    !
    ! `workspace' is a vectar of length at least half the length of
    ! the data (rounded down). It has to be protected from garbage
    ! collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(inout) :: run_stack(0:run_stack_size)
    integer, intent(inout) :: stack_count
    class(vectar_data_t), pointer, intent(in) :: workspace

    logical :: the_invariant_is_established
    integer :: n

    n = stack_count
    the_invariant_is_established = .false.
    do while (.not. the_invariant_is_established .and. 2 <= n)
       if (stack_needs_reduction_after_comparing_three_entries (n)) then
          if (runlen (n - 2) < runlen (n)) then
             call merge_two_runs (less_than, data, &
                  &               run_stack (n - 3) + 1, run_stack (n - 2) + 1, run_stack (n - 1), &
                  &               workspace)
             run_stack (n - 2) = run_stack (n - 1)
             run_stack (n - 1) = run_stack (n)
             n = n - 1
          else
             call merge_two_runs (less_than, data, &
                  &               run_stack (n - 2) + 1, run_stack (n - 1) + 1, run_stack (n), &
                  &               workspace)
             run_stack (n - 1) = run_stack (n)
             n = n - 1
          end if
       else if (runlen (n - 1) <= runlen (n)) then
          call merge_two_runs (less_than, data, &
               &               run_stack (n - 2) + 1, run_stack (n - 1) + 1, run_stack (n), &
               &               workspace)
          run_stack (n - 1) = run_stack (n)
          n = n - 1
       else
          the_invariant_is_established = .true.
       end if
    end do
    stack_count = n

  contains

    function stack_needs_reduction_after_comparing_three_entries (n) result (bool)
      integer, intent(in) :: n
      logical :: bool

      if (4 <= n) then
         bool = (runlen (n - 2) <= runlen (n - 1) + runlen (n)) &
              &    .or. (runlen (n - 3) <= runlen (n - 2) + runlen (n - 1))
      else if (3 <= n) then
         bool = (runlen (n - 2) <= runlen (n - 1) + runlen (n))
      else
         bool = .false.
      end if
    end function stack_needs_reduction_after_comparing_three_entries

    function runlen (i) result (len)
      integer, intent(in) :: i
      integer(sz) :: len

      len = run_stack(i) - run_stack(i - 1)
    end function runlen

  end subroutine restore_run_stack_invariant

  recursive subroutine unit_test__restore_run_stack_invariant__test1
    type(gcroot_t) :: vec, workspace_vec
    type(vectar_data_t), pointer :: data, workspace
    integer(sz) :: run_stack(0:run_stack_size)
    integer :: stack_count

    workspace_vec = make_vectar (2000, 1)
    workspace => vectar_data_ptr (workspace_vec)

    run_stack(0) = -1_sz

    ! Test 3 <= n, (runlen (n - 2) <= runlen (n - 1) + runlen (n)),
    ! with runlen(n - 2) <= runlen(n). This should reduce the stack
    ! height by 1, by merging the 2nd and 3rd entries.
    vec = list_to_vectar (append (iota (11, 0, 2), iota (11, 1, 2), iota (39, 22)))
    data => vectar_data_ptr (vec)
    run_stack(1) = 10_sz
    run_stack(2) = 30_sz
    run_stack(3) = 60_sz
    stack_count = 3
    call restore_run_stack_invariant (int_less_than, data, run_stack, stack_count, workspace)
    if (stack_count /= 2) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0010 failed")
    end if
    if (run_stack(0) /= -1_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0020 failed")
    end if
    if (run_stack(1) /= 30_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0030 failed")
    end if
    if (run_stack(2) /= 60_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0040 failed")
    end if
    if (.not. vectar_equal (int_equal, vec, list_to_vectar (iota (61)))) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 0050 failed")
    end if

    ! Test 4 <= n, (runlen (n - 2) <= runlen (n - 1) + runlen (n)),
    ! with runlen(n - 2) <= runlen(n). This should reduce the stack
    ! height by 1, by merging the 2nd and 3rd entries. (FIXME: write a
    ! stronger test of correct merge.)
    vec = list_to_vectar (iota (1045, 1))
    data => vectar_data_ptr (vec)
    run_stack(1) = 1000_sz
    run_stack(2) = 1010_sz
    run_stack(3) = 1030_sz
    run_stack(4) = 1045_sz
    stack_count = 4
    call restore_run_stack_invariant (int_less_than, data, run_stack, stack_count, workspace)
    if (stack_count /= 3) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1010 failed")
    end if
    if (run_stack(0) /= -1_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1020 failed")
    end if
    if (run_stack(1) /= 1000_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1040 failed")
    end if
    if (run_stack(2) /= 1030_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1040 failed")
    end if
    if (run_stack(3) /= 1045_sz) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1050 failed")
    end if
    if (.not. vectar_equal (int_equal, vec, list_to_vectar (iota (1045, 1)))) then
       call error_abort ("unit_test__restore_run_stack_invariant__test1 1060 failed")
    end if

    ! FIXME: Add a test for 4 <= n, (runlen (n - 3) <= runlen (n - 2)
    !        + runlen (n - 1)), with runlen(n - 2) <= runlen(n). This
    !        is the extra condition that was added by Stijn de Gouw et
    !        al. (`Verifying OpenJDK s Sort Method for Generic
    !        Collections', J Autom Reason. 2019; 62(1): 93 126. DOI:
    !        10.1007/s10817-017-9426-4

  end subroutine unit_test__restore_run_stack_invariant__test1

  recursive subroutine reduce_the_run_stack_to_depth_1 (less_than, data, run_stack, stack_count, workspace)
    !
    ! Merge run_stack contents until the run stack contains just one,
    ! big run (which is the sorted result).
    !
    ! Note that the run_stack indexing starts at position 0. All other
    ! entries are the end indices of runs; we put a fake end index in
    ! position 0. The stack_size counter does not count the fake stack
    ! entry.
    !
    ! `workspace' is a vectar of length at least half the length of
    ! the data (rounded down). It has to be protected from garbage
    ! collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(inout) :: run_stack(0:run_stack_size)
    integer, intent(inout) :: stack_count
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer :: n

    n = stack_count
    do while (n /= 1)
       if (stack_needs_merge_inside_it (n)) then
          call merge_two_runs (less_than, data, &
               &               run_stack (n - 3) + 1, run_stack (n - 2) + 1, run_stack (n - 1), &
               &               workspace)
          run_stack (n - 2) = run_stack (n - 1)
          run_stack (n - 1) = run_stack (n)
          n = n - 1
       else
          call merge_two_runs (less_than, data, &
               &               run_stack (n - 2) + 1, run_stack (n - 1) + 1, run_stack (n), &
               &               workspace)
          run_stack (n - 1) = run_stack (n)
          n = n - 1
       end if
    end do
    stack_count = n

  contains

    function stack_needs_merge_inside_it (n) result (bool)
      integer, intent(in) :: n
      logical :: bool

      if (3 <= n) then
         bool = (runlen (n - 2) < runlen (n))
      else
         bool = .false.
      end if
    end function stack_needs_merge_inside_it

    function runlen (i) result (len)
      integer, intent(in) :: i
      integer(sz) :: len

      len = run_stack(i) - run_stack(i - 1)
    end function runlen

  end subroutine reduce_the_run_stack_to_depth_1

  recursive subroutine unit_test__reduce_the_run_stack_to_depth_1__test1
    type(gcroot_t) :: vec, workspace_vec
    type(vectar_data_t), pointer :: data, workspace
    integer(sz) :: run_stack(0:run_stack_size)
    integer :: stack_count

    workspace_vec = make_vectar (2000, 1)
    workspace => vectar_data_ptr (workspace_vec)

    run_stack(0) = -1_sz

    ! Test 3 <= n, runlen(n - 2) <= runlen(n). This should reduce the
    ! stack height by 1, by merging the 2nd and 3rd entries. Then the
    ! stack height will go to 1, through another merge; however, code
    ! coverage tools can show that first merge happened.
    vec = list_to_vectar (append (iota (11, 0, 2), iota (11, 1, 2), iota (39, 22)))
    data => vectar_data_ptr (vec)
    run_stack(1) = 10_sz
    run_stack(2) = 30_sz
    run_stack(3) = 60_sz
    stack_count = 3
    call reduce_the_run_stack_to_depth_1 (int_less_than, data, run_stack, stack_count, workspace)
    if (stack_count /= 1) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0010 failed")
    end if
    if (run_stack(0) /= -1_sz) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0020 failed")
    end if
    if (run_stack(1) /= 60_sz) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0030 failed")
    end if
    if (.not. vectar_equal (int_equal, vec, list_to_vectar (iota (61)))) then
       call error_abort ("unit_test__reduce_the_run_stack_to_depth_1__test1 0040 failed")
    end if
  end subroutine unit_test__reduce_the_run_stack_to_depth_1__test1

  function choose_minimum_run_length (data_length) result (min_run_length)
    !
    ! Minimum run length as suggested by Tim Peters.
    !
    ! See
    ! https://en.wikipedia.org/w/index.php?title=Timsort&oldid=1065277889#Minimum_run_size
    !
    !    "The final algorithm takes the six most significant bits of
    !    the size of the array, adds one if any of the remaining bits
    !    are set, and uses that result as the minrun. This algorithm
    !    works for all arrays, including those smaller than 64; for
    !    arrays of size 63 or less, this sets minrun equal to the
    !    array size and Timsort reduces to an insertion sort."
    !
    integer(sz), intent(in) :: data_length
    integer(sz) :: min_run_length

    integer :: total_bits_count
    integer :: leading_zeros_count
    integer :: significant_bits_count
    integer :: right_bits_count
    integer(sz) :: left_bits
    integer(sz) :: right_bits

    total_bits_count = bit_size (data_length)
    leading_zeros_count = leadz (data_length)
    significant_bits_count = total_bits_count - leading_zeros_count
    right_bits_count = max (0, significant_bits_count - 6)
    left_bits = ibits (data_length, right_bits_count, 6)
    right_bits = ibits (data_length, 0, right_bits_count)
    if (right_bits == 0) then
       min_run_length = left_bits
    else
       min_run_length = left_bits + 1
    end if
  end function choose_minimum_run_length

  recursive subroutine stable_mergesort (less_than, data, idatastart, idataend, workspace)
    !
    ! A adaptive natural mergesort using a run stack similar to that
    ! employed by Tim Peters in older versions of the CPython sort
    ! function.
    !
    ! Aside: I do not call this implementation `timsort', because that
    ! is not a particular algorithm, and there is much confusion in
    ! the programming community as to what constitutes
    ! `timsort'. Indeed, the algorithm is `simply' mergesort done
    ! iteratively, which I have myself programmed in years past --
    ! albeit in much, MUCH simpler form. The enhancements to iterative
    ! mergesort that Python employs change from time to time and are
    ! very, very, VERY clever.
    !
    ! (FIXME: Switch to using the `powersort' merge strategy, from
    ! J. Ian Munro and Sebastian Wild, "Nearly-optimal mergesorts:
    ! fast, practical sorting methods that optimally adapt to existing
    ! runs". Peters cites this paper in the CPython sources, in file
    ! Objects/listsort.txt.)
    !
    ! Note that the run_stack indexing starts at position 0. All other
    ! entries are the end indices of runs; we put a fake end index in
    ! position 0. The stack_size counter does not count the fake stack
    ! entry.
    !
    ! `workspace' is a vectar of length at least floor((idataend0 -
    ! idatastart0 + 1)/2). Both `data' and `workspace' must be rooted
    ! to protect them from garbage collection.
    !
    procedure(vectar_predicate2_t) :: less_than
    class(vectar_data_t), pointer, intent(in) :: data
    integer(sz), intent(in) :: idatastart
    integer(sz), intent(in) :: idataend
    class(vectar_data_t), pointer, intent(in) :: workspace

    integer(sz) :: run_stack(0:run_stack_size)
    integer :: stack_count
    integer(sz) :: iright
    integer(sz) :: data_length
    integer(sz) :: min_run_length

    data_length = (idataend - idatastart) + 1

    if (data_length <= 1) then
       ! The data is already sorted.
       continue
    else
       min_run_length = choose_minimum_run_length (data_length)

       run_stack(0) = idatastart - 1_sz
       stack_count = 0

       do while (run_stack(stack_count) /= idataend)
          call gather_an_adequately_long_increasing_run (less_than, data, &
               &                                         run_stack(stack_count) + 1, idataend, &
               &                                         min_run_length, iright)

          if (stack_count == run_stack_size) then
             call error_abort ("adaptive mergesort stack size exceeded")
          end if
          stack_count = stack_count + 1
          run_stack(stack_count) = iright

          call restore_run_stack_invariant (less_than, data, run_stack, stack_count, workspace)
       end do

       call reduce_the_run_stack_to_depth_1 (less_than, data, run_stack, stack_count, workspace)

    end if
  end subroutine stable_mergesort

  subroutine vectar_stable_mergesort_unit_tests
    call unit_test__bottenbruch_search__test1
    call unit_test__bottenbruch_search2__test1
    call unit_test__restore_run_stack_invariant__test1
    call unit_test__reduce_the_run_stack_to_depth_1__test1
  end subroutine vectar_stable_mergesort_unit_tests

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

end module sorting_and_selection
