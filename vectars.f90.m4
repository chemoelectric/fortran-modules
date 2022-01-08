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

  ! Tests for the vectar_t type.
  public :: is_vectar          ! Is the given object a vectar_t?
  public :: is_not_vectar      ! Is the given object *not* a vectar_t?

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

  ! Is a vectar empty? That is, is its length equal to zero?
  public :: vectar_is_empty

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

  ! Generic subroutines: swap vectar elements.
  public :: vectar_swap0        ! Indices run 0, 1, 2, ...
  public :: vectar_swap1        ! Indices run 1, 2, 3, ...
  public :: vectar_swapn        ! Indices run n, n+1, n+2, ...

  ! Implementations of the vectar_swapX subroutines.
  public :: vectar_swap0_size_kind
  public :: vectar_swap1_size_kind
  public :: vectar_swapn_size_kind
  public :: vectar_swap0_int
  public :: vectar_swap1_int
  public :: vectar_swapn_int

  ! Vector equality.
  public :: vectar_equal        ! A generic function.
  public :: apply_vectar_equal  ! Compare a list of vectars.

  ! Implementations of vectar_equal.
m4_forloop([n],[0],LISTN_MAX,[dnl
  public :: vectar_equal[]n
])dnl

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

  interface vectar_swap0
     module procedure vectar_swap0_size_kind
     module procedure vectar_swap0_int
  end interface vectar_swap0

  interface vectar_swap1
     module procedure vectar_swap1_size_kind
     module procedure vectar_swap1_int
  end interface vectar_swap1

  interface vectar_swapn
     module procedure vectar_swapn_size_kind
     module procedure vectar_swapn_int
  end interface vectar_swapn

  interface vectar_equal
m4_forloop([n],[0],LISTN_MAX,[dnl
     module procedure vectar_equal[]n
])dnl
  end interface vectar_equal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Types for predicates.
  public :: vectar_predicate1_t ! A predicate taking 1 argument.
m4_forloop([n],[2],ZIP_MAX,[dnl
  public :: vectar_predicate[]n[]_t ! A predicate taking n arguments.
])dnl

  abstract interface
m4_forloop([n],[1],ZIP_MAX,[dnl
     recursive function vectar_predicate[]n[]_t (x1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
          ])x[]k])) result (bool)
m4_forloop([k],[1],n,[dnl
       class(*), intent(in) :: [x]k
])dnl
       logical :: bool
     end function vectar_predicate[]n[]_t
])dnl
  end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

  type :: vectar_data_p_t
     type(vectar_data_t), pointer :: data
  end type vectar_data_p_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

  function vectar_data_p (vec) result (p)
    class(*), intent(in) :: vec
    type(vectar_data_p_t) :: p

    p%data => vectar_data_ptr (vec)
  end function vectar_data_p

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

  pure function is_vectar (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (vectar_t)
       bool = .true.
    class default
       bool = .false.
    end select
  end function is_vectar

  pure function is_not_vectar (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (vectar_t)
       bool = .false.
    class default
       bool = .true.
    end select
  end function is_not_vectar

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

  function vectar_is_empty (vec) result (bool)
    class(*), intent(in) :: vec
    logical :: bool

    type(vectar_data_t), pointer :: data

    data => vectar_data_ptr (vec)
    bool = (data%length == 0_sz)
  end function vectar_is_empty

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

  subroutine vectar_swap0_size_kind (vec, i, j)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i, j

    type(vectar_data_t), pointer :: data
    class(*), allocatable :: tmp

    data => vectar_data_ptr (vec)
    if (i < 0_sz .or. j < 0_sz .or. data%length <= i .or. data%length <= j) then
       call error_abort ("vectar_t index out of range")
    end if
    if (i /= j) then
       tmp = data%array(i)%element
       data%array(i)%element = data%array(j)%element
       data%array(j)%element = tmp
    end if
  end subroutine vectar_swap0_size_kind

  subroutine vectar_swap1_size_kind (vec, i, j)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: i, j

    call vectar_swap0_size_kind (vec, i - 1, j - 1)
  end subroutine vectar_swap1_size_kind

  subroutine vectar_swapn_size_kind (vec, n, i, j)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i, j

    call vectar_swap0_size_kind (vec, i - n, j - n)
  end subroutine vectar_swapn_size_kind

  subroutine vectar_swap0_int (vec, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: i, j

    integer(sz) :: ii, jj

    ii = i
    jj = j
    call vectar_swap0_size_kind (vec, ii, jj)
  end subroutine vectar_swap0_int

  subroutine vectar_swap1_int (vec, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: i, j

    integer(sz) :: ii, jj

    ii = i
    jj = j
    call vectar_swap1_size_kind (vec, ii, jj)
  end subroutine vectar_swap1_int

  subroutine vectar_swapn_int (vec, n, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i, j

    integer(sz) :: ii, jj
    integer(sz) :: nn

    ii = i
    jj = j
    nn = n
    call vectar_swapn_size_kind (vec, nn, ii, jj)
  end subroutine vectar_swapn_int

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

  recursive function vectar_equal0 (pred) result (bool)
    procedure(vectar_predicate2_t) :: pred
    logical :: bool

    bool = .true.
  end function vectar_equal0

  recursive function vectar_equal1 (pred, vec1) result (bool)
    procedure(vectar_predicate2_t) :: pred
    class(*), intent(in) :: vec1
    logical :: bool

    select type (vec1)
    class is (vectar_t)
       bool = .true.
    class default
       call error_abort ("expected a vectar_t")
    end select
  end function vectar_equal1

m4_forloop([n],[2],LISTN_MAX,[dnl
  recursive function vectar_equal[]n (equal, vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 5),[1],[&
       &                            ])vec[]k])) result (bool)
    procedure(vectar_predicate2_t) :: equal
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: vec[]k
])dnl
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 4),[1],[&
         &          ])vec[]k]))
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal[]n
])dnl
dnl
  recursive function apply_vectar_equal (equal, vectars) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vectars
    logical :: bool

    integer(sz) :: n
    type(vectar_t), allocatable :: v(:)
    type(vectar_data_p_t), allocatable :: p(:)

    n = length (vectars)
    if (n == 0_sz) then
       ! No vectars were given.
       bool = .true.
    else if (n == 1_sz) then
       ! Only one vectar was given.
       bool = .true.
    else
       allocate (v(1_sz:n))
       allocate (p(1_sz:n))
       call fill_v_and_p
       if (.not. lengths_are_equal ()) then
          bool = .false.
       else
          bool = check_elements ()
       end if
    end if

  contains

    subroutine fill_v_and_p
      integer(sz) :: i
      type(cons_t) :: lst

      lst = vectars
      do i = 1_sz, n
         v(i) = car (lst)
         p(i) = vectar_data_p (v(i))
         lst = cdr (lst)
      end do
    end subroutine fill_v_and_p

    function lengths_are_equal () result (bool)
      integer(sz) :: i
      integer(sz) :: len
      logical :: bool

      bool = .true.
      len = p(1)%data%length
      i = 2_sz
      do while (bool .and. i <= n)
         bool = (p(i)%data%length == len)
         i = i + 1
      end do
    end function lengths_are_equal

    recursive function check_elements () result (bool)
      integer(sz) :: len
      integer(sz) :: i_vec
      integer(sz) :: i_elem
      logical :: bool

      len = p(1)%data%length
      bool = .true.
      i_vec = 1_sz
      do while (bool .and. i_vec < n)
         !
         ! NOTE: One could check vectar_t_eq (v(i_vec), v(i_vec + 1)))
         !       here, but SRFI-133 does not, because of how IEEE
         !       floating point behaves.
         !
         ! Specifically: a NaN is unequal with itself, by the usual
         ! reckoning of equality. Therefore a list of NaN would not
         ! should be regarded as unequal with itself.
         !
         i_elem = 0_sz
         do while (bool .and. i_elem < len)
            bool = equal (p(i_vec)%data%array(i_elem)%element, &
                 &        p(i_vec + 1)%data%array(i_elem)%element)
            i_elem = i_elem + 1
         end do
         i_vec = i_vec + 1
      end do
    end function check_elements

  end function apply_vectar_equal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module vectars
