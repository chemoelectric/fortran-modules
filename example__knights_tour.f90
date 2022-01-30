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
! Knight's tour, based on a Rosetta Code example in Common Lisp.
!
! The solution is by Warnsdorff's rule. See
! https://en.wikipedia.org/w/index.php?title=Knight%27s_tour&oldid=1066880156#Warnsdorff's_rule
!

program example__knights_tour

  use, intrinsic :: iso_fortran_env, only: output_unit

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: lsets
  use, non_intrinsic :: vectars
  use, non_intrinsic :: sorting_and_selection

  implicit none

  integer, parameter :: outp = output_unit

  ! The dimension of a side.
  integer, parameter :: side = 8

  ! `Algebraic' chess notation.
  character, parameter :: xnotation(0:side - 1) = (/ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'i' /)
  character, parameter :: ynotation(0:side - 1) = (/ '1', '2', '3', '4', '5', '6', '7', '8' /)

  type(gcroot_t) :: chessboard
  type(gcroot_t) :: knight_directions

!!$  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
!!$  character(:), allocatable :: foo
!!$  complex :: pos1
!!$  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME

  ! Used this to check whether a position is legal.
  chessboard = generate_chessboard (side)

  ! Add any of these to a position to get a new position.
  knight_directions = list ((1, 2),    &
       &                    (2, 1),    &
       &                    (1, -2),   &
       &                    (2, -1),   &
       &                    (-1, 2),   &
       &                    (-2, 1),   &
       &                    (-1, -2),  &
       &                    (-2, -1))

!!$  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
!!$  call complex_to_algebraic_notation ((1, 2), foo)
!!$  print*,foo
!!$  call algebraic_notation_to_complex (foo, pos1)
!!$  print*,int(real(pos1)),int(aimag(pos1))
!!$  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME

  call make_and_print_tour ((1,1), .false.) ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME

contains

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

  function cmplx_cast (x) result (val)
    class(*), intent(in) :: x
    complex :: val

    select type (x)
    type is (complex)
       val = x
    class default
       error stop
    end select
  end function cmplx_cast

  subroutine cmplx_add (x, y, sum)
    class(*), intent(in) :: x
    class(*), intent(in) :: y
    class(*), allocatable, intent(out) :: sum

    sum = (cmplx_cast (x) + cmplx_cast (y))
  end subroutine cmplx_add

  function cmplx_eq (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = (cmplx_cast (x) == cmplx_cast (y))
  end function cmplx_eq

  function real_part_lt (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = (real (cmplx_cast (x)) < real (cmplx_cast (y)))
  end function real_part_lt

  subroutine cdr_subr (x, cdr_val)
    class(*), intent(in) :: x
    class(*), allocatable, intent(out) :: cdr_val

    cdr_val = cdr (x)
  end subroutine cdr_subr

  subroutine cdrs_subr (x, cdrs_val)
    class(*), intent(in) :: x
    class(*), allocatable, intent(out) :: cdrs_val

    cdrs_val = map (cdr_subr, x)
  end subroutine cdrs_subr

  subroutine keep_cmplx_nonmember (x, lst, retval)
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: retval

    if (is_not_member (cmplx_eq, x, lst)) then
       retval = x
    else
       retval = .false.
    end if
  end subroutine keep_cmplx_nonmember

  function generate_chessboard (n) result (board)
    integer, intent(in) :: n
    type(cons_t) :: board

    integer :: i
    integer :: j
    complex :: pos

    board = nil
    do i = 0, n - 1
       do j = 0, n - 1
          pos = cmplx (i, j)
          board = pos ** board
       end do
    end do
    board = reversex (board)
  end function generate_chessboard

  subroutine complex_to_algebraic_notation (position, notation)
    complex, intent(in) :: position
    character(:), allocatable, intent(out) :: notation

    integer :: x, y

    x = int (real (position))
    y = int (aimag (position))
    notation = xnotation (x) // ynotation (y)
  end subroutine complex_to_algebraic_notation

  subroutine algebraic_notation_to_complex (notation, position)
    character(2), intent(in) :: notation
    complex, intent(out) :: position

    integer :: x, y

    x = findloc (xnotation, notation(1:1), dim = 1) - 1
    y = findloc (ynotation, notation(2:2), dim = 1) - 1
    if (x == -1 .or. y == -1) then
       write (outp, '(A2, " is not a legal position; using a1 instead")') notation
       x = 0
       y = 0
    end if
    position = cmplx (x, y)
  end subroutine algebraic_notation_to_complex

  function find_legal_moves (moves_list) result (legal_moves)
    class(*), intent(in) :: moves_list
    type(cons_t) :: legal_moves

    type(gcroot_t) :: possibilities
    complex :: current_position

    if (is_false (car (moves_list))) then
       ! Move legality has already failed.
       legal_moves = nil
    else
       current_position = cmplx_cast (car (moves_list))

       ! Generate possible new moves.
       possibilities = map (cmplx_add, circular_list (car (moves_list)), knight_directions)

       ! The move must stay within the chessboard.
       possibilities = lset_intersection (cmplx_eq, possibilities, chessboard)

       ! The position must not already have been visited.
       legal_moves = filter_map (keep_cmplx_nonmember, possibilities, circular_list (moves_list))
    end if
  end function find_legal_moves

  function warnsdorff_rule (pair1, pair2) result (bool)
    !
    ! A sorting predicate.
    !
    ! Use Warnsdorff's rule to select between two moves: if one move
    ! has a lower weight, take it; otherwise, select either move at
    ! random.
    !
    ! pair1 and pair2 have the form `cons (n, move)' where n is the
    ! weight.
    !
    class(*), intent(in) :: pair1
    class(*), intent(in) :: pair2
    logical :: bool

    integer :: n1
    integer :: n2
    real :: randnum

    n1 = int_cast (car (pair1))
    n2 = int_cast (car (pair2))
    if (n1 == n2) then
       ! Random tie-break. (This method fails for much larger
       ! chessboards.)
       call random_number (randnum)
       bool = (randnum < 0.5)
    else
       bool = (n1 < n2)
    end if
  end function warnsdorff_rule

  function return_weighted_moves (moves) result (weighted_moves)
    !
    ! For each legal move from a position, look ahead one move and
    ! return a pair `cons (n, move)', where the weight n is how many
    ! legal moves follow.
    !
    ! If n == 0 one will have reached a dead end, and so the pair is
    ! left out. Therefore, if every move that follows is a dead end,
    ! then the return value will be a nil list; so will it be if there
    ! were no legal moves in the first place.
    !
    class(*), intent(in) :: moves
    type(cons_t) :: weighted_moves

    type(gcroot_t) :: candidates

    candidates = find_legal_moves (moves)
    weighted_moves = filter_map (cons_weight_and_move, candidates, circular_list (moves))
  end function return_weighted_moves

  subroutine cons_weight_and_move (mv, moves, pair)
    class(*), intent(in) :: mv
    class(*), intent(in) :: moves
    class(*), allocatable, intent(out) :: pair

    integer :: weight

    weight = int (length (find_legal_moves (cons (mv, moves))))
    if (weight == 0) then
       ! A weight of zero represents a dead end. Leave this move out.
       pair = .false.
    else
       pair = cons (weight, mv)
    end if
  end subroutine cons_weight_and_move

  function pick_among_weighted_moves (moves) result (move_picked)
    !
    ! From a list of weighted moves, pick one according to
    ! Warnsdorff's rule.
    !
    class(*), intent(in) :: moves
    complex :: move_picked

    type(gcroot_t) :: vec

    ! Choose the move with the least weight.
    vec = list_to_vectar (moves)
    move_picked = cmplx_cast (cdr (vectar_selectx1 (warnsdorff_rule, vec, 1)))
  end function pick_among_weighted_moves

  function make_warnsdorff_move (moves_list) result (new_moves_list)
    class(*), intent(in) :: moves_list
    type(cons_t) :: new_moves_list

    type(gcroot_t) :: weighted_moves
    complex :: next_move

    weighted_moves = return_weighted_moves (moves_list)
    if (is_nil_list (weighted_moves)) then
       new_moves_list = nil  ! There are no legal moves.
    else
       next_move = pick_among_weighted_moves (weighted_moves)
       new_moves_list = cons (next_move, moves_list)
    end if
  end function make_warnsdorff_move

  function make_final_move (moves_list) result (new_moves_list)
    class(*), intent(in) :: moves_list
    type(cons_t) :: new_moves_list

    type(gcroot_t) :: legal_moves
    type(gcroot_t) :: next_move

    legal_moves = find_legal_moves (moves_list)
    if (is_nil_list (legal_moves)) then
       new_moves_list = nil  ! There are no legal moves.
    else
       next_move = car (legal_moves)
       new_moves_list = cons (next_move, moves_list)
    end if
  end function make_final_move

  function make_tour (moves_list) result (tour)
    class(*), intent(in) :: moves_list
    type(cons_t) :: tour

    type(gcroot_t) :: moves

    moves = moves_list
    do while (length (moves) /= length (chessboard))
       do while (length (moves) /= length (chessboard) - 1)
          moves = make_warnsdorff_move (moves)
          if (is_nil_list (moves)) then
             ! At this point, there was no legal move. Start over.
             moves = last_pair (moves_list)
          end if
       end do
       moves = make_final_move (moves)
       if (is_nil_list (moves)) then
          ! At this point, there was no legal move. Start over.
          moves = last_pair (moves_list)
       end if
    end do
    tour = .tocons. moves
  end function make_tour

  function make_tour_from_starting_position (starting_position, tour_must_be_closed) result (tour)
    complex, intent(in) :: starting_position
    logical, intent(in) :: tour_must_be_closed
    type(cons_t) :: tour

    if (tour_must_be_closed) then
       tour = make_closed_tour (starting_position ** nil)
    else
       tour = make_tour (starting_position ** nil)
    end if
  end function make_tour_from_starting_position

  function make_closed_tour (moves_list) result (tour)
    class(*), intent(in) :: moves_list
    type(cons_t) :: tour

    tour = make_tour (moves_list)
    do while (.not. tour_is_closed (tour))
       tour = make_tour (moves_list)
    end do
  end function make_closed_tour

  function tour_is_closed (tour) result (bool)
    class(*), intent(in) :: tour
    logical :: bool

    type(gcroot_t) :: tour_start
    type(gcroot_t) :: tour_end
    type(gcroot_t) :: legal_moves

    tour_start = last (tour)
    tour_end = first (tour)

    ! From tour_end, is tour_start a legal move?
    legal_moves = find_legal_moves (list (tour_end))
    bool = is_member (cmplx_eq, tour_start, legal_moves)
  end function tour_is_closed

  subroutine print_tour_linear (tour)
    class(*), intent(in) :: tour

    type(gcroot_t) :: p
    character(:), allocatable :: notation
    logical :: with_separator

    with_separator = .false.
    p = reverse (tour)
    do while (is_pair (p))
       if (with_separator) then
          write (outp, '(" -> ")', advance = 'no')
       end if
       with_separator = .true.
       call complex_to_algebraic_notation (cmplx_cast (car (p)), notation)
       write (outp, '(A)', advance = 'no') notation
       p = cdr (p)
    end do
  end subroutine print_tour_linear

  subroutine print_tour_matrix (tour)
    class(*), intent(in) :: tour

    call for_each (print_tour_matrix_row, tour_to_matrix (tour))
  end subroutine print_tour_matrix

  subroutine print_tour_matrix_row (row)
    class(*), intent(in) :: row

    call for_each (print_tour_matrix_row_element, row)
  end subroutine print_tour_matrix_row

  subroutine print_tour_matrix_row_element (element)
    class(*), intent(in) :: element

    write (outp, '(I3)', advance = 'no') int_cast (element)
  end subroutine print_tour_matrix_row_element

  function tour_to_matrix (tour) result (matrix)
    class(*), intent(in) :: tour
    type(cons_t) :: matrix

    type(gcroot_t) :: ordered_indexed_tour

    ordered_indexed_tour = map (index_to_row, &
         &                      iota (side, side - 1, -1), &
         &                      circular_list (index_tour (tour)))

    ! Extract the indices.
    matrix = map (cdrs_subr, ordered_indexed_tour)
  end function tour_to_matrix

  subroutine index_to_row (i, indexed_tour, row)
    class(*), intent(in) :: i
    class(*), intent(in) :: indexed_tour
    class(*), allocatable, intent(out) :: row

    row = get_row (int_cast (i), indexed_tour)
  end subroutine index_to_row

  function index_tour (tour) result (indexed_tour)
    class(*), intent(in) :: tour
    type(cons_t) :: indexed_tour

    type(gcroot_t) :: tour_reversed
    integer :: i

    tour_reversed = reverse (tour)

    indexed_tour = nil
    do i = 1, int (length (tour_reversed))
       indexed_tour = cons (list_ref1 (tour_reversed, i), i) ** indexed_tour
    end do
    indexed_tour = reversex (indexed_tour)
  end function index_tour

  function get_row (n, tour) result (row)
    integer, intent(in) :: n
    class(*), intent(in) :: tour
    type(cons_t) :: row

    row = filter_map (keep_row_element, circular_list (n), tour)
    row = list_sortx (real_part_lt, row)
  end function get_row

  subroutine keep_row_element (n, element, retval)
    class(*), intent(in) :: n
    class(*), intent(in) :: element
    class(*), allocatable, intent(out) :: retval

    ! In a row, the imaginary parts (the vertical coordinates) are
    ! equal.
    if (int_cast (n) == aimag (cmplx_cast (car (element)))) then
       retval = element
    else
       retval = .false.
    end if
  end subroutine keep_row_element

  subroutine make_and_print_tour (starting_position, tour_must_be_closed)
    complex, intent(in) :: starting_position
    logical, intent(in) :: tour_must_be_closed

    type(gcroot_t) :: tour

    tour = make_tour_from_starting_position (starting_position, tour_must_be_closed)
    call print_tour_linear (tour)
    write (outp, '()', advance = 'yes')
    write (outp, '()', advance = 'yes')
    call print_tour_matrix (tour)
  end subroutine make_and_print_tour

end program example__knights_tour
