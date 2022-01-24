! -*- F90 -*- 
!
! Copyright 2021, 2022 Barry Schwartz
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

module boxes
  !
  ! Boxes in the fashion of SRFI-111.
  ! https://srfi.schemers.org/srfi-111/srfi-111.html
  !

  use, non_intrinsic :: garbage_collector

  implicit none
  private

  public :: box_t            ! The type for garbage-collectible boxes.

  public :: is_box           ! Is the object either a box or a gcroot_t containing a box?
  public :: box_t_cast       ! Convert an object to a box, if possible.
  public :: operator(.tobox.) ! A synonym for box_t_cast.

  public :: box              ! Put an object in a box.
  public :: unbox            ! Copy an object from a box.
  public :: set_box          ! Change the contents of a box.

  public :: autobox          ! Put an object in a box, if it is not already boxed.
  public :: autounbox        ! Copy an object from a box, if it is boxed.

  type :: box_data_t
     class(*), allocatable :: contents
  end type box_data_t

  type, extends (collectible_t) :: box_t
   contains
     procedure, pass :: get_branch => box_t_get_branch
     procedure, pass :: assign => box_t_assign
     generic :: assignment(=) => assign
  end type box_t

  interface operator(.tobox.)
     module procedure box_t_cast
  end interface operator(.tobox.)

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

contains

!!!-------------------------------------------------------------------

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module boxes error: ", a)') msg
    error stop
  end subroutine error_abort_1

  subroutine strange_error
    call error_abort ("a strange error, possibly use of an object already garbage-collected")
  end subroutine strange_error

!!!-------------------------------------------------------------------

  subroutine box_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(box_t), intent(in) :: this
    integer(size_kind), intent(in) :: branch_number
    logical, intent(out) :: branch_number_out_of_range
    class(*), allocatable, intent(out) :: branch

    class(*), pointer :: data
    
    branch_number_out_of_range = .true.
    if (branch_number == 1) then
       data => this%heap_element%data
       select type (data)
       class is (box_data_t)
          branch = data%contents
          branch_number_out_of_range = .false.
       end select
    end if
  end subroutine box_t_get_branch

!!!-------------------------------------------------------------------

  function is_box (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool
    select type (obj)
    class is (box_t)
       bool = .true.
    class default
       bool = .false.
    end select
  end function is_box

  recursive subroutine box_t_assign (dst, src)
    class(box_t), intent(inout) :: dst
    class(*), intent(in) :: src

    select type (obj => src)
    type is (box_t)
       dst%heap_element => obj%heap_element
    class default
       call error_abort ("assignment to box_t of an incompatible object")
    end select
  end subroutine box_t_assign

  recursive function box_t_cast (obj) result (the_box)
    class(*), intent(in) :: obj
    type(box_t) :: the_box

    select type (src => .autoval. obj)
    class is (box_t)
       the_box%heap_element => src%heap_element
    class default
       call error_abort ("assignment to box_t of an incompatible object")
    end select
  end function box_t_cast

!!!-------------------------------------------------------------------

  recursive function box (contents) result (the_box)
    class(*), intent(in) :: contents
    type(box_t), allocatable :: the_box

    type(heap_element_t), pointer :: new_element
    type(box_data_t), pointer :: data

    allocate (data)
    data%contents = .autoval. contents
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    allocate (the_box)
    the_box%heap_element => new_element
  end function box

  recursive function unbox (the_box) result (contents)
    class(*), intent(in) :: the_box
    class(*), allocatable :: contents

    class(*), pointer :: data

    select type (some_box => .autoval. the_box)
    class is (box_t)
       data => some_box%heap_element%data
       select type (data)
       class is (box_data_t)
          contents = data%contents
       class default
          call strange_error
       end select
    class default
       call error_abort ("unbox of a non-box")
    end select
  end function unbox

  recursive subroutine set_box (the_box, contents)
    class(*), intent(inout) :: the_box
    class(*), intent(in) :: contents

    type(box_data_t), pointer :: data
    type(box_t) :: box1

    box1 = the_box
    deallocate (box1%heap_element%data)
    allocate (data)
    data%contents = .autoval. contents
    box1%heap_element%data => data
  end subroutine set_box

!!!-------------------------------------------------------------------

  recursive function autobox (contents) result (the_box)
    class(*), intent(in) :: contents
    type(box_t) :: the_box

    select type (stuff => .autoval. contents)
    class is (box_t)
       the_box = stuff
    class default
       the_box = box (stuff)
    end select
  end function autobox

  recursive function autounbox (the_box) result (contents)
    class(*), intent(in) :: the_box
    class(*), allocatable :: contents

    select type (some_box => .autoval. the_box)
    class is (box_t)
       contents = unbox (some_box)
    class default
       contents = some_box
    end select
  end function autounbox

!!!-------------------------------------------------------------------

end module boxes
