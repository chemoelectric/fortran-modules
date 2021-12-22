! -*- F90 -*- include(`common-macros.m4')m4_include([cadadr.m4])
!
! Copyright 2021 Barry Schwartz
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

module boxes
  !
  ! Boxes in the fashion of SRFI-111.
  ! https://srfi.schemers.org/srfi-111/srfi-111.html
  !

  !
  ! NOTE: Unless you know what you are doing, you should use
  !       `type(gcroot_t)' from module `garbage_collector' to hold
  !       values of type `box_t'. Otherwise the garbage collector
  !       might collect your work unexpectedly.
  !

  use, non_intrinsic :: garbage_collector

  implicit none
  private

  public :: box_t            ! The type for garbage-collectible boxes.

  public :: is_box           ! Is the object either a box or a gcroot_t containing a box?
  public :: box_t_cast       ! Convert an object to a box, if possible.

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
  end type box_t

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module boxes error: ", a)') msg
    error stop
  end subroutine error_abort_1

  subroutine disallow_gcroot (obj)
    class(*), intent(in) :: obj
    select type (obj)
    class is (gcroot_t)
       call error_abort ("gcroot_t is not allowed in this context")
    end select
  end subroutine disallow_gcroot

  subroutine strange_error
    call error_abort ("a strange error, possibly use of an object already garbage-collected")
  end subroutine strange_error

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine box_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(box_t), intent(in) :: this
    integer(size_kind), intent(in) :: branch_number
    class(*), allocatable :: branch

    class(*), pointer :: data
    logical :: branch_number_out_of_range
    
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  function box_t_cast (obj) result (the_box)
    class(*), intent(in) :: obj
    class(box_t), allocatable :: the_box

    call disallow_gcroot (obj)

    select type (obj)
    class is (box_t)
       the_box = obj
    class default
      call error_abort ("box_t_cast of an incompatible object")
    end select
  end function box_t_cast

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function box (contents) result (the_box)
    class(*), intent(in) :: contents
    type(box_t), allocatable :: the_box

    type(heap_element_t), pointer :: new_element
    type(box_data_t), pointer :: data

    call disallow_gcroot (contents)

    allocate (data)
    data%contents = contents
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

    call disallow_gcroot (the_box)

    select type (the_box)
    class is (box_t)
       data => the_box%heap_element%data
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
    class(box_t), intent(inout) :: the_box
    class(*), intent(in) :: contents

    type(box_data_t), pointer :: data

    call disallow_gcroot (contents)

    deallocate (the_box%heap_element%data)
    allocate (data)
    data%contents = contents
    the_box%heap_element%data => data
  end subroutine set_box

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function autobox (contents) result (the_box)
    class(*), intent(in) :: contents
    class(box_t), allocatable :: the_box

    call disallow_gcroot (contents)

    select type (contents)
    class is (box_t)
       the_box = contents
    class default
       the_box = box (contents)
    end select
  end function autobox

  recursive function autounbox (the_box) result (contents)
    class(*), intent(in) :: the_box
    class(*), allocatable :: contents

    call disallow_gcroot (the_box)

    select type (the_box)
    class is (box_t)
       contents = unbox (the_box)
    class default
       contents = the_box
    end select
  end function autounbox

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module boxes
