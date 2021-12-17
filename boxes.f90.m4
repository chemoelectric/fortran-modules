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

  use, non_intrinsic :: garbage_collector

  implicit none
  private

  public :: box_t
  public :: box
  public :: unbox
  public :: set_box

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

  recursive function box (contents) result (the_box)
    class(*), intent(in) :: contents
    class(box_t), allocatable :: the_box

    type(heap_element_t), pointer :: new_element
    type(box_data_t), pointer :: data

    select type (contents)
    class is (gcroot_t)
       the_box = box (contents%val)
    class default
       allocate (data)
       data%contents = contents
       allocate (new_element)
       new_element%data => data
       call heap_insert (new_element)
       the_box%heap_element => new_element
    end select
  end function box

  recursive function unbox (the_box) result (contents)
    class(*), intent(in) :: the_box
    class(*), allocatable :: contents

    class(*), pointer :: data

    select type (the_box)
    class is (box_t)
       data => the_box%heap_element%data
       select type (data)
       class is (box_data_t)
          contents = data
       class default
          call error_abort ("internal error")
       end select
    class is (gcroot_t)
       contents = unbox (the_box%val)
    class default
       call error_abort ("unbox of a non-box")
    end select
  end function unbox

  recursive subroutine set_box (the_box, contents)
    class(*), intent(inout) :: the_box
    class(*), intent(in) :: contents

    type(box_data_t), pointer :: data

    select type (the_box)
    class is (box_t)
       deallocate (the_box%heap_element%data)
       allocate (data)
       data%contents = contents
       the_box%heap_element%data => data
    class is (gcroot_t)
       call set_box (the_box%val, contents)
    class default
       call error_abort ("set_box of a non-box")
    end select
  end subroutine set_box


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module boxes
