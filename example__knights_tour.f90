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
! Knight's tour, based on a Rosetta Code example in Scheme, by Panda
! <panbaoxiang@hotmail.com>
!

program example__knights_tour
  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none

  integer, parameter :: xdimension = 8
  integer, parameter :: ydimension = 8

  ! Chess notation.
  character, parameter :: xletter(0:xdimension - 1) = (/ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'i' /)

  ! A record of the paths and numbers of conquered territories.
  type(gcroot_t) :: dictionary
  integer :: counter

  ! Forbidden territories: conquered or cul-de-sac.
  type(gcroot_t) :: forbidden

  ! The starting position, and the knight's tour computed from it.
  integer :: starting_position
  type(gcroot_t) :: tour

  dictionary = nil
  counter = 1

  forbidden = nil

  starting_position = 32 ! 35 ! FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
  tour = go (starting_position)

  call print_tour (.tocons. tour)

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

  function int_eq (x, y) result (bool)
    class(*), intent(in) :: x, y
    logical :: bool

    bool = (int_cast (x) == int_cast (y))
  end function int_eq

  ! A position is represented as ((ydimension * x) + y), where x and y
  ! run 0..(xdimension-1) and 0..(ydimension-1), respectively.
  function decode (position) result (xy)
    integer, intent(in) :: position
    type(cons_t) :: xy

    xy = cons (position / ydimension, mod (position, ydimension))
  end function decode

  subroutine print_position (position)
    integer, intent(in) :: position

    type(gcroot_t) :: xy
    integer :: x, y

    xy = decode (position)
    x = int_cast (car (xy))
    y = int_cast (cdr (xy))
    write (*, '(A1, I1)', advance = 'no') xletter(x), y + 1
  end subroutine print_position

  subroutine print_tour (tour)
    type(cons_t), intent(in) :: tour

    type(gcroot_t) :: p
    logical :: needs_separator
    integer :: position_count

    position_count = 0
    needs_separator = .false.
    p = tour
    do while (is_pair (p))
       if (needs_separator) then
          write (*, '("-")', advance = 'no')
       end if
       needs_separator = .true.

       if (position_count == 8) then
          write (*, '()', advance = 'yes')
          position_count = 1
       else
          position_count = position_count + 1
       end if

       call print_position (int_cast (car (p)))

       p = cdr (p)
    end do
    write (*, '()', advance = 'yes')
  end subroutine print_tour

  function renew (position) result (new_position)
    integer, intent(in) :: position
    integer :: new_position

    type(gcroot_t) :: rules
    type(gcroot_t) :: possible

    rules = list ((2 * ydimension) + 1 + position,  &
         &        (2 * ydimension) - 1 + position,  &
         &        (-2 * ydimension) + 1 + position, &
         &        (-2 * ydimension) - 1 + position, &
         &        ydimension + 2 + position,        &
         &        ydimension - 2 + position,        &
         &        position - ydimension - 2,        &
         &        position - ydimension + 2)
    possible = filter (is_possible, rules)
    if (is_nil (possible)) then
       forbidden = cons (car (dictionary), forbidden)
       dictionary = cdr (dictionary)
       counter = counter - 1
       new_position = int_cast (car (dictionary))
    else
       dictionary = cons (car (possible), dictionary)
       forbidden = dictionary
       counter = counter + 1
       new_position = int_cast (car (possible))
    end if
  end function renew

  function is_possible (position) result (bool)
    class(*), intent(in) :: position
    logical :: bool

    integer :: pos

    pos = int_cast (position)
    bool = is_nil (member (int_eq, pos, forbidden)) &
         & .and. 0 <= pos                           &
         & .and. pos < xdimension * ydimension
  end function is_possible

  function go (position) result (tour)
    integer, intent(in) :: position
    type(cons_t) :: tour

    integer :: pos

    pos = position
    do while (counter /= xdimension * ydimension)
       pos = renew (pos)
    end do
    tour = reverse (dictionary)
  end function go

end program example__knights_tour
