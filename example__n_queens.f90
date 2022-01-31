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

  integer :: arg_count
  integer :: stat
  character(80) :: arg1
  integer :: board_size
  type(cons_t) :: all_solutions

  arg_count = command_argument_count ()
  if (arg_count == 1) then
     call get_command_argument (1, arg1)
     read (arg1, *, iostat = stat) board_size
     if (stat /= 0) then
        call print_usage (output_unit)
     else if (board_size < 1) then
        call print_usage (output_unit)
     else
        all_solutions = find_all_solutions (board_size)
        call print_all_solutions (output_unit, board_size, all_solutions)
     end if
  else
     call print_usage (output_unit)
  end if

contains

  subroutine print_usage (outp)
    integer, intent(in) :: outp

    write (outp, '("Usage: example__n_queens BOARD_SIZE")')
    write (outp, '("BOARD_SIZE must be at least 1.")')
    write (outp, '("All solutions are computed before any is printed.")')
  end subroutine print_usage

  function find_all_solutions (board_size) result (all_solutions)
    integer, intent(in) :: board_size
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

    if (length (positions_so_far) == int_cast (board_size)) then
       solutions = list (positions_so_far)
    else
       file = int (length (positions_so_far)) + 1
       positions = expand_file_legally (int_cast (board_size), file, positions_so_far)
       new_pos_so_fars = map (kons, positions, circular_list (positions_so_far))
       solutions = concatenate (map (find_solutions_from_positions_so_far, &
            &                        circular_list (board_size), new_pos_so_fars))
    end if
  end subroutine find_solutions_from_positions_so_far

  function expand_file_legally (board_size, file, positions_so_far) result (positions)
    !
    ! Return a list of all the positions in a given file, under the
    ! constraint that queen not be under attack.
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

    call unzip (positions_so_far, ranks, diags_down, diags_up)
    bool = .not. is_member (int_eq, list_ref1 (new_position, i_rank), ranks) &
         & .and. .not. is_member (int_eq, list_ref1 (new_position, i_diag_down), diags_down) &
         & .and. .not. is_member (int_eq, list_ref1 (new_position, i_diag_up), diags_up)
  end function position_is_legal

  subroutine print_all_solutions (outp, board_size, all_solutions)
    integer, intent(in) :: outp
    class(*), intent(in) :: board_size
    class(*), intent(in) :: all_solutions

    integer(size_kind) :: n

    n = length (all_solutions)
    if (n == 1) then
       write (outp, '("There is ", I0, " solution.")') n
    else
       write (outp, '("There are ", I0, " solutions.")') n
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
    type(cons_t) :: ranks
    integer :: rank
    integer :: file
    integer :: file_of_queen

    n_outp = int_cast (outp)
    n_board_size = int_cast (board_size)

    call unzip (solution, ranks)

    do rank = n_board_size, 1, -1
       do file = 1, n_board_size
          write (n_outp, '("----")', advance = 'no')
       end do
       write (n_outp, '("-")', advance = 'yes')

       file_of_queen = n_board_size - int (list_index0 (int_eq, circular_list (rank), ranks))

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

end program example__n_queens
