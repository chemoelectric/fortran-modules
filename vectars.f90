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
! The cons_pairs module is based in part on the SRFI-1 reference
! implementation in Scheme, which was released as follows:
!
!   "Copyright (c) 1998, 1999 by Olin Shivers. You may do as you
!    please with this code as long as you do not remove this copyright
!    notice or hold me liable for its use."
!
! The cons_pairs module is written entirely in Fortran.
!


module vectars
  !
  ! Vectors (single-dimensional arrays) in the fashion of Scheme
  ! Request for Implementation 133 (SRFI-133).
  ! https://srfi.schemers.org/srfi-133/srfi-133.html
  !
  ! The name `vectar' (short for `vector array') is used instead of
  ! `vector', to avoid confusion with Gibbs vectors.
  !

  !
  ! WARNING: I reserve the right to turn most procedures into generic
  !          procedures. This may affect your code if you try to pass
  !          this module's procedures directly to other procedures;
  !          that is, the name of the actual, non-generic
  !          implementation may change. (Something such as is_nil or
  !          is_pair or is_not_nil or is_not_pair is quite unlikely to
  !          be made generic, however.)
  !

  use, non_intrinsic :: garbage_collector
  use, non_intrinsic :: cons_pairs

  implicit none
  private

  ! The type for a vectar.
  public :: vectar_t

  ! Do two objects refer to the same vectar in the garbage collector's
  ! heap?
  public :: vectar_t_eq

  ! Convert an object to a vectar, if possible.
  public :: vectar_t_cast
  public :: operator(.tovectar.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type :: vectar_data_t
     class(*), allocatable :: array(:) ! Zero-indexed storage.
  end type vectar_data_t

  type, extends (collectible_t) :: vectar_t
   contains
     procedure, pass :: get_branch => vectar_t_get_branch
     procedure, pass :: assign => vectar_t_assign
     generic :: assignment(=) => assign
  end type vectar_t

  interface operator(.tovectar.)
     module procedure vectar_t_cast
  end interface operator(.tovectar.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module vectars error: ", a)') msg
    error stop
  end subroutine error_abort_1

!!$  subroutine strange_error
!!$    call error_abort ("a strange error, possibly use of an object already garbage-collected")
!!$  end subroutine strange_error

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vectar_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(vectar_t), intent(in) :: this
    integer(sz), intent(in) :: branch_number
    class(*), allocatable :: branch

    class(*), pointer :: data
    logical :: branch_number_out_of_range

    ! A NIL-list has zero branches. A pair has two branches.

    branch_number_out_of_range = .true.
    if (associated (this%heap_element)) then
       if (0_sz <= branch_number) then
          data => this%heap_element%data
          select type (data)
          class is (vectar_data_t)
             if (branch_number - 1_sz <= ubound (data%array, 1, sz)) then
                branch = data%array(branch_number - 1_sz)
                branch_number_out_of_range = .false.
             end if
          end select
       end if
    end if
  end subroutine vectar_t_get_branch

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vectar_t_assign (dst, src)
    class(vectar_t), intent(inout) :: dst
    class(*), intent(in) :: src

    select type (src)
    class is (vectar_t)
       dst%heap_element => src%heap_element
    class is (gcroot_t)
       select type (val => .val. src)
       class is (vectar_t)
          dst%heap_element => val%heap_element
       class default
          call error_abort ("assignment to vectar_t of an incompatible gcroot_t object")
       end select
    class default
       call error_abort ("assignment to vectar_t of an incompatible object")
    end select
  end subroutine vectar_t_assign

  function vectar_t_cast (obj) result (vec)
    class(*), intent(in) :: obj
    type(vectar_t) :: vec

    vec = obj
  end function vectar_t_cast

  recursive function vectar_t_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    logical :: bool

    type(vectar_t) :: o1, o2

    o1 = obj1
    o2 = obj2

    if (associated (o1%heap_element)) then
       if (associated (o2%heap_element)) then
          bool = associated (o1%heap_element, o2%heap_element)
       else
          bool = .false.
       end if
    else
       bool = .not. associated (o2%heap_element)
    end if
  end function vectar_t_eq

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module vectars
