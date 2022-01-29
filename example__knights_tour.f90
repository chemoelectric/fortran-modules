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

program example__knights_tour
  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs
  use, non_intrinsic :: lsets

  implicit none

  ! The dimension of a side.
  integer, parameter :: side = 8

  ! `Algebraic' chess notation.
  character, parameter :: xnotation(0:side - 1) = (/ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'i' /)
  character, parameter :: ynotation(0:side - 1) = (/ '1', '2', '3', '4', '5', '6', '7', '8' /)

  type(gcroot_t) :: chessboard
  type(gcroot_t) :: knight_directions

  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
  character(:), allocatable :: foo
  complex :: pos1
  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME

  chessboard = generate_chessboard (side)

  knight_directions = list (cons (1, 2),    &
       &                    cons (2, 1),    &
       &                    cons (1, -2),   &
       &                    cons (2, -1),   &
       &                    cons (-1, 2),   &
       &                    cons (-2, 1),   &
       &                    cons (-1, -2),  &
       &                    cons (-2, -1))


  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
  call position_to_algebraic_notation ((1, 2), foo)
  print*,foo
  call algebraic_notation_to_position (foo, pos1)
  print*,int(real(pos1)),int(aimag(pos1))
  ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME

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

  function cmplx_eq (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = (cmplx_cast (x) == cmplx_cast (y))
  end function cmplx_eq

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

  subroutine position_to_algebraic_notation (position, notation)
    complex, intent(in) :: position
    character(:), allocatable, intent(out) :: notation

    integer :: x, y

    x = int (real (position))
    y = int (aimag (position))
    notation = xnotation (x) // ynotation (y)
  end subroutine position_to_algebraic_notation

  subroutine algebraic_notation_to_position (notation, position)
    character(2), intent(in) :: notation
    complex, intent(out) :: position

    integer :: x, y

    x = findloc (xnotation, notation(1:1), dim = 1) - 1
    y = findloc (ynotation, notation(2:2), dim = 1) - 1
    if (x == -1 .or. y == -1) then
       write (*, '(A2, " is not a legal position; using a1 instead")') notation
       x = 0
       y = 0
    end if
    position = cmplx (x, y)
  end subroutine algebraic_notation_to_position

  function find_legal_moves (moves_list) result (legal_moves)
    class(*), intent(in) :: moves_list
    type(cons_t) :: legal_moves

    type(gcroot_t) :: possibilities
    type(gcroot_t) :: p
    complex :: current_position
    complex :: direction

    current_position = cmplx_cast (car (moves_list))

    ! Generate possible new moves.
    possibilities = nil
    p = knight_directions
    do while (is_pair (p))
       direction = cmplx_cast (car (p))
       possibilities = cons (current_position + direction, possibilities)
       p = cdr (p)
    end do

    ! The move must stay within the chessboard.
    possibilities = lset_intersection (cmplx_eq, possibilities, chessboard)

    ! The move must not already have been visited.
    legal_moves = nil
    p = possibilities
    do while (is_pair (p))
       if (is_nil (member (cmplx_eq, cmplx_cast (car (p)), moves_list))) then
          legal_moves = car (p) ** legal_moves
       end if
       p = cdr (p)
    end do
  end function find_legal_moves

  function w_rule (a, b) result (bool)
    !
    ! A sorting predicate.
    !
    ! Use Warnsdorff's rule to select between two moves: if one move
    ! has a lower weight, take it; otherwise, select either move at
    ! random.
    !
    ! The weight is stored as the CAR of a pair.
    !
    class(*), intent(in) :: a
    class(*), intent(in) :: b
    logical :: bool

    integer :: weight_a
    integer :: weight_b
    real :: randnum

    weight_a = int_cast (car (a))
    weight_b = int_cast (car (b))
    if (weight_a < weight_b) then
       bool = .true.
    else if (weight_b < weight_a) then
       bool = .false.
    else
       call random_number (randnum)
       bool = (randnum < 0.5)
    end if
  end function w_rule

  function return_weighted_moves (moves) result (weighted_moves)
    !
    ! For each legal move from a position, look ahead one move and
    ! return a pair `cons (n, move)', where the weight n is how many
    ! legal moves follow.
    !
    class(*), intent(in) :: moves
    type(cons_t) :: weighted_moves

    type(gcroot_t) :: candidates
    type(gcroot_t) :: mv
    integer :: weight

    weighted_moves = nil    
    candidates = find_legal_moves (moves)
    do while (is_pair (candidates))
       mv = car (candidates)
       weight = int (length (find_legal_moves (cons (mv, moves))))
       weighted_moves = cons (weight, mv) ** weighted_moves
       candidates = cdr (candidates)
    end do
  end function return_weighted_moves

  function pick_among_weighted_moves (moves) result (move_picked)
    !
    ! From a list of weighted moves, pick one according to w_rule.
    !
    class(*), intent(in) :: moves
    complex :: move_picked

    type(gcroot_t) :: possible_moves

    ! Prune dead ends one move early.
    possible_moves = filter (weight_is_nonzero, moves)

    ! Implementation note: the following could be done by moving the
    ! data into a vectar and using a selection algorithm instead of a
    ! sort.
    call list_sortx (w_rule, possible_moves)
    move_picked = cmplx_cast (cdar (possible_moves))
  end function pick_among_weighted_moves

  function weight_is_nonzero (weighted_move) result (bool)
    class(*), intent(in) :: weighted_move
    logical :: bool

    bool = (int_cast (car (weighted_move)) /= 0)
  end function weight_is_nonzero

  function make_move (moves_list) result (new_moves_list)
    class(*), intent(in) :: moves_list
    type(cons_t) :: new_moves_list

    type(gcroot_t) :: weighted_moves
    type(gcroot_t) :: legal_moves
    type(gcroot_t) :: next_move

    if (length (moves_list) < length (chessboard) - 1) then
       weighted_moves = return_weighted_moves (moves_list)
       next_move = pick_among_weighted_moves (weighted_moves)
    else
       legal_moves = find_legal_moves (moves_list)
       if (is_nil_list (legal_moves)) then
          next_move = .false.   ! There is no legal move.
       else
          next_move = car (legal_moves)
       end if
    end if
    new_moves_list = cons (next_move, moves_list)
  end function make_move

  function make_tour (moves_list) result (tour)
    class(*), intent(in) :: moves_list
    type(cons_t) :: tour

    type(gcroot_t) :: moves
    logical :: done

    moves = moves_list
    done = .false.
    do while (.not. done)
       if (is_false (car (moves_list))) then
          ! At this point, there was no legal move. Start over.
          moves = last_pair (moves_list)
       else if (length (moves_list) /= length (chessboard)) then
          moves = make_move (moves)
       else
          done = .true.
       end if
    end do
    tour = .tocons. moves
  end function make_tour

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
    bool = is_not_nil (member (cmplx_eq, tour_start, legal_moves))
  end function tour_is_closed

  subroutine print_tour_linear
  end subroutine print_tour_linear

end program example__knights_tour
