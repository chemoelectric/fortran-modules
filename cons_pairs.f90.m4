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

module cons_pairs
  !
  ! CONS-pairs (Lisp-style lists and trees) in the fashion of SRFI-1.
  ! https://srfi.schemers.org/srfi-1/srfi-1.html
  !
  ! Significant differences from SRFI-1 include:
  !
  !    * There are merge and sort routines in this module.
  !
  !    * The lset operations are not included in this module.
  !
  ! This core module for CONS-pairs avoids code that might cause
  ! gfortran to create trampolines. (I did not want to obey this
  ! restriction when implementing lset operations; that is why they
  ! are not included in this module.)
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

  public :: cons_t           ! The type for garbage-collectible CONS-pairs.

  public :: is_cons          ! Is the object either a CONS-pair or a gcroot_t containing a CONS-pair?
  public :: cons_t_cast      ! Convert an object to a CONS-pair, if possible.

!!$  public :: box              ! Put an object in a box.
!!$  public :: unbox            ! Copy an object from a box.
!!$  public :: set_box          ! Change the contents of a box.
!!$
!!$  public :: autobox          ! Put an object in a box, if it is not already boxed.
!!$  public :: autounbox        ! Copy an object from a box, if it is boxed.
!!$
!!$  type :: box_data_t
!!$     class(*), allocatable :: contents
!!$  end type box_data_t
!!$
!!$  type, extends (collectible_t) :: box_t
!!$   contains
!!$     procedure, pass :: get_branch => box_t_get_branch
!!$  end type box_t

  type :: cons_data_t
     class(*), allocatable :: car
     class(*), allocatable :: cdr
  end type cons_data_t

  type, extends (collectible_t) :: cons_t
   contains
     procedure, pass :: get_branch => cons_t_get_branch
  end type cons_t

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module cons_pairs error: ", a)') msg
    error stop
  end subroutine error_abort_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cons_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(cons_t), intent(in) :: this
    integer(size_kind), intent(in) :: branch_number
    class(*), allocatable :: branch

    class(*), pointer :: data
    logical :: branch_number_out_of_range

    branch_number_out_of_range = .true.
    if (branch_number == 1) then
       data => this%heap_element%data
       select type (data)
       class is (cons_data_t)
          branch = data%car
          branch_number_out_of_range = .false.
       end select
    else if (branch_number == 2) then
       data => this%heap_element%data
       select type (data)
       class is (cons_data_t)
          branch = data%cdr
          branch_number_out_of_range = .false.
       end select
    end if
  end subroutine cons_t_get_branch

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function is_cons (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool
    bool = .false.
    select type (obj)
    class is (cons_t)
       bool = .true.
    class is (gcroot_t)
       select type (val => obj%val ())
       class is (cons_t)
          bool = .true.
       end select
    end select
  end function is_cons

  function cons_t_cast (obj) result (the_cons)
    class(*), intent(in) :: obj
    class(cons_t), allocatable :: the_cons
    select type (obj)
    class is (cons_t)
       the_cons = obj
    class is (gcroot_t)
       select type (val => obj%val ())
       class is (cons_t)
          the_cons = val
       class default
          call error_abort ("cons_t_cast of an incompatible object")
       end select
    class default
      call error_abort ("cons_t_cast of an incompatible object")
    end select
  end function cons_t_cast

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  recursive function cons (contents) result (the_cons)
!!$    class(*), intent(in) :: contents
!!$    type(cons_t), allocatable :: the_cons
!!$
!!$    type(heap_element_t), pointer :: new_element
!!$    type(cons_data_t), pointer :: data
!!$
!!$    select type (contents)
!!$    class is (gcroot_t)
!!$       the_cons = cons (contents%val ())
!!$    class default
!!$       allocate (data)
!!$       data%contents = contents
!!$       allocate (new_element)
!!$       new_element%data => data
!!$       call heap_insert (new_element)
!!$       allocate (the_cons)
!!$       the_cons%heap_element => new_element
!!$    end select
!!$  end function cons
!!$
!!$  recursive function uncons (the_cons) result (contents)
!!$    class(*), intent(in) :: the_cons
!!$    class(*), allocatable :: contents
!!$
!!$    class(*), pointer :: data
!!$
!!$    select type (the_cons)
!!$    class is (cons_t)
!!$       m4_if(DEBUGGING,[true],[write (*,*) "uncons of a cons_t"])
!!$       data => the_cons%heap_element%data
!!$       select type (data)
!!$       class is (cons_data_t)
!!$          contents = data%contents
!!$       class default
!!$          call error_abort ("a strange error, possibly use of a collected object")
!!$       end select
!!$    class is (gcroot_t)
!!$       m4_if(DEBUGGING,[true],[write (*,*) "uncons of a gcroot_t"])
!!$       contents = uncons (the_cons%val ())
!!$    class default
!!$       call error_abort ("uncons of a non-cons")
!!$    end select
!!$  end function uncons
!!$
!!$  recursive subroutine set_cons (the_cons, contents)
!!$    class(*), intent(inout) :: the_cons
!!$    class(*), intent(in) :: contents
!!$
!!$    type(cons_data_t), pointer :: data
!!$
!!$    select type (the_cons)
!!$    class is (cons_t)
!!$       deallocate (the_cons%heap_element%data)
!!$       allocate (data)
!!$       data%contents = contents
!!$       the_cons%heap_element%data => data
!!$    class is (gcroot_t)
!!$       block
!!$         class(collectible_t), pointer :: ptr
!!$         ptr => the_cons%get_pointer ()
!!$         call set_cons (ptr, contents)
!!$       end block
!!$    class default
!!$       call error_abort ("set_cons of a non-cons")
!!$    end select
!!$  end subroutine set_cons
!!$
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$
!!$  recursive function autocons (contents) result (the_cons)
!!$    class(*), intent(in) :: contents
!!$    class(cons_t), allocatable :: the_cons
!!$    if (is_cons (contents)) then
!!$       the_cons = cons_t_cast (contents)
!!$    else
!!$       the_cons = cons (contents)
!!$    end if
!!$  end function autocons
!!$
!!$  recursive function autouncons (the_cons) result (contents)
!!$    class(*), intent(in) :: the_cons
!!$    class(*), allocatable :: contents
!!$    if (is_cons (the_cons)) then
!!$       contents = uncons (the_cons)
!!$    else
!!$       contents = the_cons
!!$    end if
!!$  end function autouncons

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module cons_pairs