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

!
! Generate solutions to the n-queens problem.
!
! This implementation is based *extremely* loosely on the public
! domain Icon Program Library implementation by Steven B. Wampler.
! The Icon program employs backtracking assignments to arrays; our
! approach, by contrast, is functional and uses linked lists.
!
! Indeed, there is little resemblance between the two programs. It
! might prove interesting to compare them.
!
! The following commentary is taken from the Icon program:
!
! ############################################################################
! #
! #       File:     queens.icn
! #
! #       Subject:  Program to generate solutions to the n-queens problem
! #
! #       Author:   Stephen B. Wampler
! #
! #       Date:     June 10, 1988
! #
! ############################################################################
! #
! #   This file is in the public domain.
! #
! ############################################################################
! #  
! #     This program displays the solutions to the non-attacking n-
! #  queens problem: the ways in which n queens can be placed on an
! #  n-by-n chessboard so that no queen can attack another. A positive
! #  integer can be given as a command line argument to specify the
! #  number of queens. For example,
! #  
! #          iconx queens -n8
! #  
! #  displays the solutions for 8 queens on an 8-by-8 chessboard.  The
! #  default value in the absence of an argument is 6.  One solution
! #  for six queens is:
! #  
! #         -------------------------
! #         |   | Q |   |   |   |   |
! #         -------------------------
! #         |   |   |   | Q |   |   |
! #         -------------------------
! #         |   |   |   |   |   | Q |
! #         -------------------------
! #         | Q |   |   |   |   |   |
! #         -------------------------
! #         |   |   | Q |   |   |   |
! #         -------------------------
! #         |   |   |   |   | Q |   |
! #         -------------------------
! #  
! #  Comments: There are many approaches to programming solutions to
! #  the n-queens problem.  This program is worth reading for
! #  its programming techniques.
! #  
!

program example__n_queens

  use, intrinsic :: iso_fortran_env, only: output_unit

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none

  ! Positions of coordinates within a position triple. The triple is
  ! stored as a length-3 list. One usefulness of this design is: a
  ! list of such triples may be unzipped into three lists, one each
  ! for the ranks, the `down' diagonals, and the `up' diagonals.
  integer, parameter :: i_rank = 1
  integer, parameter :: i_diag_down = 2
  integer, parameter :: i_diag_up = 3

  ! .true. is good for testing that necessary values are rooted.
  ! .false. to collect garbage only when the heap reaches a limit.
  logical :: aggressive_garbage_collection = .true.

  integer :: arg_count
  integer :: stat
  character(80) :: arg

  type(gcroot_t) :: board_sizes

  arg_count = command_argument_count ()
  if (arg_count < 1) then
     call print_usage (output_unit)
  else
     board_sizes = nil
     block
       integer :: i
       integer :: board_size
       do i = 1, arg_count
          call get_command_argument (i, arg)
          read (arg, *, iostat = stat) board_size
          if (stat /= 0 .or. board_size < 1) then
             board_size = -1
          end if
          board_sizes = cons (board_size, board_sizes)
       end do
       board_sizes = reversex (board_sizes)
     end block

     if (is_member (int_eq, -1, board_sizes)) then
        call print_usage (output_unit)
     else
        ! Use pair_for_each as a way to distinguish the last
        ! BOARD_SIZE from the others. The last entry will be the final
        ! pair, and so its CDR will *not* be a pair.
        call pair_for_each (find_and_print_all_solutions, &
             &              circular_list (output_unit), &
             &              board_sizes)
     end if
  end if

contains

  subroutine print_usage (outp)
    integer, intent(in) :: outp

    write (outp, '("Usage: example__n_queens BOARD_SIZE [BOARD_SIZE...]")')
    write (outp, '("Each BOARD_SIZE must be at least 1.")')
    write (outp, '("For each BOARD_SIZE, all solutions are computed before any is printed.")')
  end subroutine print_usage

  subroutine find_and_print_all_solutions (outp_pair, board_sizes)
    class(*), intent(in) :: outp_pair
    class(*), intent(in) :: board_sizes

    integer :: n_outp
    type(gcroot_t) :: all_solutions

    n_outp = int_cast (car (outp_pair))

    all_solutions = find_all_solutions (car (board_sizes))
    call check_garbage
    call print_all_solutions (n_outp, car (board_sizes), all_solutions)
    call check_garbage
    if (is_pair (cdr (board_sizes))) then
       ! Space between one BOARD_SIZE and another.
       write (n_outp, '()')
    end if
  end subroutine find_and_print_all_solutions

  function find_all_solutions (board_size) result (all_solutions)
    class(*), intent(in) :: board_size
    type(cons_t) :: all_solutions

    class(*), allocatable :: solutions

    call find_solutions_from_positions_so_far (board_size, nil, solutions)
    all_solutions = .tocons. solutions
  end function find_all_solutions

  recursive subroutine find_solutions_from_positions_so_far (board_size, positions_so_far, solutions)
    class(*), intent(in) :: board_size
    class(*), intent(in) :: positions_so_far
    class(*), allocatable, intent(out) :: solutions

    integer :: file
    type(cons_t) :: positions
    type(cons_t) :: new_pos_so_fars
    type(cons_t) :: one_solution

    if (length (positions_so_far) == int_cast (board_size)) then
       call unzip (positions_so_far, one_solution) ! Keep only the ranks.
       solutions = list (one_solution)
    else
       file = int (length (positions_so_far)) + 1
       positions = expand_file_legally (int_cast (board_size), file, positions_so_far)
       new_pos_so_fars = map (kons, positions, circular_list (positions_so_far))
       solutions = concatenate (map (find_solutions_from_positions_so_far, &
            &                   circular_list (board_size), new_pos_so_fars))
    end if
  end subroutine find_solutions_from_positions_so_far

  function expand_file_legally (board_size, file, positions_so_far) result (positions)
    !
    ! Return a list of all the positions in a given file, under the
    ! constraint that a queen placed in the position not be under
    ! attack.
    !
    integer, intent(in) :: board_size
    integer, intent(in) :: file
    class(*), intent(in) :: positions_so_far
    type(cons_t) :: positions

    positions = expand_file (board_size, file)
    positions = remove_illegal_positions (positions, positions_so_far)
  end function expand_file_legally

  function expand_file (board_size, file) result (positions)
    !
    ! Return a list of all the positions in a given file.
    !
    integer, intent(in) :: board_size
    integer, intent(in) :: file
    type(cons_t) :: positions

    positions = map (place_queen_subr, &
         &           circular_list (board_size), &
         &           iota (board_size, 1), &
         &           circular_list (file))
  end function expand_file

  function place_queen (board_size, rank, file) result (pos)
    !
    ! Given a position as rank and file, return the preferred
    ! representation of the position, as a triple of its rank and its
    ! two diagonals.
    !
    integer, intent(in) :: board_size
    integer, intent(in) :: rank
    integer, intent(in) :: file
    type(cons_t) :: pos

    integer :: diag_down
    integer :: diag_up

    diag_down = rank + file - 1
    diag_up = board_size + rank - file
    pos = list (rank, diag_down, diag_up)
  end function place_queen

  subroutine place_queen_subr (board_size, rank, file, pos)
    !
    ! For use with `map' functions.
    !
    class(*), intent(in) :: board_size
    class(*), intent(in) :: rank
    class(*), intent(in) :: file
    class(*), allocatable, intent(out) :: pos

    pos = place_queen (int_cast (board_size), int_cast (rank), int_cast (file))
  end subroutine place_queen_subr

  function remove_illegal_positions (new_positions, positions_so_far) result (legal_positions)
    class(*), intent(in) :: new_positions
    class(*), intent(in) :: positions_so_far
    type(cons_t) :: legal_positions

    legal_positions = filter_map (keep_legal_position, new_positions, &
         &                        circular_list (positions_so_far))
  end function remove_illegal_positions

  subroutine keep_legal_position (position, positions_so_far, retval)
    class(*), intent(in) :: position
    class(*), intent(in) :: positions_so_far
    class(*), allocatable, intent(out) :: retval

    if (position_is_legal (position, positions_so_far)) then
       retval = position
    else
       retval = .false.
    end if
  end subroutine keep_legal_position

  function position_is_legal (new_position, positions_so_far) result (bool)
    class(*), intent(in) :: new_position
    class(*), intent(in) :: positions_so_far
    logical :: bool

    type(cons_t) :: ranks
    type(cons_t) :: diags_down
    type(cons_t) :: diags_up

    !
    ! The positions_so_far list looks, for instance, like this:
    !
    !    list (list (rank3, diag_down3, diag_up3),
    !          list (rank2, diag_down2, diag_up2),
    !          list (rank1, diag_down1, diag_up1))
    !
    ! Unzipping it gives three lists:
    !
    !    list (rank3, rank2, rank1)
    !
    !    list (diag_down3, diag_down2, diag_down1)
    !
    !    list (diag_up3, diag_up2, diag_up1)
    !
    call unzip (positions_so_far, ranks, diags_down, diags_up)

    if (is_member (int_eq, list_ref1 (new_position, i_rank), ranks)) then
       bool = .false.
    else if (is_member (int_eq, list_ref1 (new_position, i_diag_down), diags_down)) then
       bool = .false.
    else if (is_member (int_eq, list_ref1 (new_position, i_diag_up), diags_up)) then
       bool = .false.
    else
       bool = .true.
    end if
  end function position_is_legal

  subroutine print_all_solutions (outp, board_size, all_solutions)
    class(*), intent(in) :: outp
    class(*), intent(in) :: board_size
    class(*), intent(in) :: all_solutions

    integer(size_kind) :: n

    n = length (all_solutions)
    write (int_cast (outp), '("For a board ", I0, " by ", I0, ", ")', advance = 'no') &
         &    int_cast (board_size), int_cast (board_size)
    if (n == 1) then
       write (int_cast (outp), '("there is ", I0, " solution.")') n
    else
       write (int_cast (outp), '("there are ", I0, " solutions.")') n
    end if
    call for_each (print_spaced_solution, circular_list (outp), &
         &         circular_list (board_size), all_solutions)
  end subroutine print_all_solutions

  subroutine print_spaced_solution (outp, board_size, solution)
    class(*), intent(in) :: outp
    class(*), intent(in) :: board_size
    class(*), intent(in) :: solution

    write (int_cast (outp), '()', advance = 'yes')
    call print_solution (outp, board_size, solution)
  end subroutine print_spaced_solution

  subroutine print_solution (outp, board_size, solution)
    class(*), intent(in) :: outp
    class(*), intent(in) :: board_size
    class(*), intent(in) :: solution

    integer :: n_outp
    integer :: n_board_size
    integer :: rank
    integer :: file
    integer :: file_of_queen

    n_outp = int_cast (outp)
    n_board_size = int_cast (board_size)

    do rank = n_board_size, 1, -1
       do file = 1, n_board_size
          write (n_outp, '("----")', advance = 'no')
       end do
       write (n_outp, '("-")', advance = 'yes')

       file_of_queen = n_board_size - int (list_index0 (int_eq, circular_list (rank), solution))

       do file = 1, n_board_size
          if (file == file_of_queen) then
             write (n_outp, '("| Q ")', advance = 'no')
          else
             write (n_outp, '("|   ")', advance = 'no')
          end if
       end do
       write (n_outp, '("|")', advance = 'yes')       
    end do

    do file = 1, n_board_size
       write (n_outp, '("----")', advance = 'no')
    end do
    write (n_outp, '("-")', advance = 'yes')
  end subroutine print_solution

  subroutine kons (x, y, xy)
    class(*), intent(in) :: x
    class(*), intent(in) :: y
    class(*), allocatable, intent(out) :: xy

    xy = cons (x, y)
  end subroutine kons

  function int_cast (x) result (val)
    class(*), intent(in) :: x
    integer :: val

    select type (x)
    type is (integer)
       val = x
    class default
       error stop
    end select
  end function int_cast

  function int_eq (x, y) result (bool)
    class(*), intent(in) :: x
    class(*), intent(in) :: y
    logical :: bool

    bool = (int_cast (x) == int_cast (y))
  end function int_eq

  subroutine check_garbage
    if (aggressive_garbage_collection) then
       call collect_garbage_now
    else
       call check_heap_size
    end if
  end subroutine check_garbage

end program example__n_queens
