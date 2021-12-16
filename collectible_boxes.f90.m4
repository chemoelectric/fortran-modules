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

module collectible_boxes ! FIXME: Consider calling this "collected_boxes" or just "boxes".

  use, non_intrinsic :: garbage_collector

  implicit none
  private

  public :: box_t ! FIXME: Consider giving this a different name, and making "box_t" the garbage collection root kind.
  public :: box
  public :: unbox ! FIXME: This should be generic that takes either type of box, or should be a module procedure.
  public :: set_box ! FIXME: This should be generic that takes either type of box, or should be a module procedure.

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

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("collectible_boxes error: ", a)') msg
    error stop
  end subroutine error_abort_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function box_t_get_branch (this, branch_number) result (branch)
    class(box_t), intent(in) :: this
    integer(size_kind), intent(in) :: branch_number
    class(*), allocatable :: branch

    class(*), pointer :: data
    logical :: branch_is_set = .false.
    
    if (branch_number == 1) then
       data => this%heap_element%data
       select type (data)
       class is (box_data_t)
          branch = data%contents
          branch_is_set = .true.
       end select
    end if
    if (.not. branch_is_set) then
       branch = nil_branch_t ()
    end if
  end function box_t_get_branch

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function box (contents)
    class(*), intent(in) :: contents
    class(box_t), allocatable :: box

    type(heap_element_t), pointer :: new_element
    type(box_data_t), pointer :: data

    allocate (data)
    data%contents = contents
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    box%heap_element => new_element
  end function box

  function unbox (box) result (contents)
    class(box_t), intent(in) :: box
    class(*), allocatable :: contents

    class(*), pointer :: data

    data => box%heap_element%data
    select type (data)
    class is (box_data_t)
       contents = data
    class default
       call error_abort ("internal error")
    end select
  end function unbox

  subroutine set_box (box, contents)
    class(box_t), intent(inout) :: box
    class(*), intent(in) :: contents

    type(box_data_t), pointer :: data

    deallocate (box%heap_element%data)
    allocate (data)
    data%contents = contents
    box%heap_element%data => data
  end subroutine set_box


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module collectible_boxes
