! -*- F90 -*- include(`common-macros.m4')m4_include([cadadr.m4])
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

module vectars
  !
  ! Vectors (single-dimensional arrays) in the fashion of Scheme
  ! Request for Implementation 133 (SRFI-133).
  ! https://srfi.schemers.org/srfi-133/srfi-133.html
  !
  ! The name `vectar' (short for `vector array') is used instead of
  ! `vector', to avoid confusion with Gibbs vectors.
  !
  ! Conversions between vectars and strings are not included in this
  ! module, despite that they are included in SRFI-133.
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

  ! Generic function: make a vectar from elements passed as arguments.
  public :: vectar

  ! Implementations of vectar.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: vectar[]n
])dnl

  ! Generic function: make a vectar of one value repeated.
  public :: make_vectar

  ! Implementations of make_vectar.
  public :: make_vectar_size_kind
  public :: make_vectar_int

  ! Return the length of a vectar, as an INTEGER([SIZE_KIND]).
  public :: vectar_length

  ! Generic functions: return a vectar element.
  public :: vectar_ref0         ! Indices run 0, 1, 2, ...
  public :: vectar_ref1         ! Indices run 1, 2, 3, ...
  public :: vectar_refn         ! Indices run n, n+1, n+2, ...

  ! Implementations of the vectar_refX functions.
  public :: vectar_ref0_size_kind
  public :: vectar_ref1_size_kind
  public :: vectar_refn_size_kind
  public :: vectar_ref0_int
  public :: vectar_ref1_int
  public :: vectar_refn_int

  ! Generic subroutines: set a vectar element.
  public :: vectar_set0         ! Indices run 0, 1, 2, ...
  public :: vectar_set1         ! Indices run 1, 2, 3, ...
  public :: vectar_setn         ! Indices run n, n+1, n+2, ...

  ! Implementations of the vectar_setX subroutines.
  public :: vectar_set0_size_kind
  public :: vectar_set1_size_kind
  public :: vectar_setn_size_kind
  public :: vectar_set0_int
  public :: vectar_set1_int
  public :: vectar_setn_int

  ! Vectar-list conversions.
  public :: vectar_to_list
  public :: reverse_vectar_to_list
  public :: list_to_vectar
  public :: reverse_list_to_vectar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

  type :: vectar_element_t
     class(*), allocatable :: element
  end type vectar_element_t

  type :: vectar_data_t
     integer(sz) :: length
     type(vectar_element_t), allocatable :: array(:) ! Zero-based.
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

  interface vectar
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure vectar[]n
])dnl
  end interface vectar

  interface make_vectar
     module procedure make_vectar_size_kind
     module procedure make_vectar_int
  end interface make_vectar

  interface vectar_ref0
     module procedure vectar_ref0_size_kind
     module procedure vectar_ref0_int
  end interface vectar_ref0

  interface vectar_ref1
     module procedure vectar_ref1_size_kind
     module procedure vectar_ref1_int
  end interface vectar_ref1

  interface vectar_refn
     module procedure vectar_refn_size_kind
     module procedure vectar_refn_int
  end interface vectar_refn

  interface vectar_set0
     module procedure vectar_set0_size_kind
     module procedure vectar_set0_int
  end interface vectar_set0

  interface vectar_set1
     module procedure vectar_set1_size_kind
     module procedure vectar_set1_int
  end interface vectar_set1

  interface vectar_setn
     module procedure vectar_setn_size_kind
     module procedure vectar_setn_int
  end interface vectar_setn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module vectars error: ", a)') msg
    error stop
  end subroutine error_abort_1

  subroutine strange_error
    call error_abort ("a strange error, possibly use of an object already garbage-collected")
  end subroutine strange_error

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar_data_ptr (vec) result (data_ptr)
    class(*), intent(in) :: vec
    type(vectar_data_t), pointer :: data_ptr

    select type (v => .autoval. vec)
    class is (vectar_t)
       if (associated (v%heap_element)) then
          select type (data => v%heap_element%data)
          class is (vectar_data_t)
             data_ptr => data
          class default
             call strange_error
          end select
       else
          call error_abort ("vectar_t not properly allocated")
       end if
    class default
       call error_abort ("expected a vectar_t")
    end select
  end function vectar_data_ptr

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
                branch = data%array(branch_number - 1_sz)%element
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

  function vectar0 () result (vec)
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 0_sz
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar0

m4_forloop([n],[1],LISTN_MAX,[dnl
  function vectar[]n (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &            ])obj[]k])) result (vec)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = m4_eval(n)_sz
    allocate (data%array(0_sz:m4_eval(n - 1)_sz))
m4_forloop([k],[1],n,[dnl
    data%array(m4_eval(k - 1)_sz) = vectar_element_t (.autoval. obj[]k)
])dnl
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar[]n

])
dnl
  function make_vectar_size_kind (size, fill) result (vec)
    integer(sz), intent(in) :: size
    class(*), intent(in) :: fill
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    if (size < 0_sz) then
       call error_abort ("vectar size must be at least zero")
    else
       allocate (data)
       data%length = size
       if (0_sz < size) then
          allocate (data%array(0_sz:(size - 1_sz)), source = vectar_element_t (.autoval. fill))
       end if
       allocate (new_element)
       new_element%data => data
       call heap_insert (new_element)
       vec%heap_element => new_element
    end if
  end function make_vectar_size_kind

  function make_vectar_int (size, fill) result (vec)
    integer, intent(in) :: size
    class(*), intent(in) :: fill
    type(vectar_t) :: vec

    integer(sz) :: size1

    size1 = size
    vec = make_vectar_size_kind (size1, fill)
  end function make_vectar_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar_length (vec) result (len)
    class(*), intent(in) :: vec
    integer(sz) :: len

    type(vectar_data_t), pointer :: data

    data => vectar_data_ptr (vec)
    len = data%length
  end function vectar_length

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar_ref0_size_kind (vec, i) result (element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    type(vectar_data_t), pointer :: data

    data => vectar_data_ptr (vec)
    if (i < 0_sz .or. data%length <= i) then
       call error_abort ("vectar_t index out of range")
    end if
    element = data%array(i)%element
  end function vectar_ref0_size_kind

  function vectar_ref1_size_kind (vec, i) result (element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = vectar_ref0_size_kind (vec, i - 1)
  end function vectar_ref1_size_kind

  function vectar_refn_size_kind (vec, n, i) result (element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = vectar_ref0_size_kind (vec, i - n)
  end function vectar_refn_size_kind

  function vectar_ref0_int (vec, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: ii

    ii = i
    element = vectar_ref0_size_kind (vec, ii)
  end function vectar_ref0_int

  function vectar_ref1_int (vec, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: ii

    ii = i
    element = vectar_ref1_size_kind (vec, ii)
  end function vectar_ref1_int

  function vectar_refn_int (vec, n, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: ii
    integer(sz) :: nn

    ii = i
    nn = n
    element = vectar_refn_size_kind (vec, nn, ii)
  end function vectar_refn_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vectar_set0_size_kind (vec, i, element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), intent(in) :: element

    type(vectar_data_t), pointer :: data

    data => vectar_data_ptr (vec)
    if (i < 0_sz .or. data%length <= i) then
       call error_abort ("vectar_t index out of range")
    end if
    data%array(i)%element = element
  end subroutine vectar_set0_size_kind

  subroutine vectar_set1_size_kind (vec, i, element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i
    class(*), intent(in) :: element

    call vectar_set0_size_kind (vec, i - 1, element)
  end subroutine vectar_set1_size_kind

  subroutine vectar_setn_size_kind (vec, n, i, element)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i
    class(*), intent(in) :: element

    call vectar_set0_size_kind (vec, i - n, element)
  end subroutine vectar_setn_size_kind

  subroutine vectar_set0_int (vec, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), intent(in) :: element

    integer(sz) :: ii

    ii = i
    call vectar_set0_size_kind (vec, ii, element)
  end subroutine vectar_set0_int

  subroutine vectar_set1_int (vec, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), intent(in) :: element

    integer(sz) :: ii

    ii = i
    call vectar_set1_size_kind (vec, ii, element)
  end subroutine vectar_set1_int

  subroutine vectar_setn_int (vec, n, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), intent(in) :: element

    integer(sz) :: ii
    integer(sz) :: nn

    ii = i
    nn = n
    call vectar_setn_size_kind (vec, nn, ii, element)
  end subroutine vectar_setn_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar_to_list (vec) result (lst)
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    integer(sz) :: i

    lst = nil
    do i = vectar_length (vec) - 1, 0, -1
       lst = vectar_ref0 (vec, i) ** lst
    end do
  end function vectar_to_list

  function reverse_vectar_to_list (vec) result (lst)
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    integer(sz) :: i

    lst = nil
    do i = 0, vectar_length (vec) - 1
       lst = vectar_ref0 (vec, i) ** lst
    end do
  end function reverse_vectar_to_list

  function list_to_vectar (lst) result (vec)
    class(*), intent(in) :: lst
    type(vectar_t) :: vec

    integer(sz) :: n
    integer(sz) :: i
    type(cons_t) :: p
    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    n = length (.autoval. lst)
    allocate (data)
    data%length = n
    if (0_sz < n) then
       allocate (data%array(0_sz:(n - 1_sz)))
       i = 0
       p = .autoval. lst
       do while (is_pair (p))
          data%array(i) = vectar_element_t (car (p))
          i = i + 1
          p = cdr (p)
       end do
    end if
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function list_to_vectar

  function reverse_list_to_vectar (lst) result (vec)
    class(*), intent(in) :: lst
    type(vectar_t) :: vec

    integer(sz) :: n
    integer(sz) :: i
    type(cons_t) :: p
    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    n = length (.autoval. lst)
    allocate (data)
    data%length = n
    if (0_sz < n) then
       allocate (data%array(0_sz:(n - 1_sz)))
       i = n - 1
       p = .autoval. lst
       do while (is_pair (p))
          data%array(i) = vectar_element_t (car (p))
          i = i - 1
          p = cdr (p)
       end do
    end if
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function reverse_list_to_vectar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module vectars
