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


module vectars
  !
  ! Vectors (single-dimensional arrays) in the fashion of Scheme
  ! Request for Implementation 133 (SRFI-133).
  ! https://srfi.schemers.org/srfi-133/srfi-133.html
  !
  ! An important difference from SRFI-133 is that we use `ranges' --
  ! with inclusive end index -- in place of optional `start' and `end'
  ! parameters (with exclusive end index). Inclusive end indices are
  ! more Fortranish; also, this approach reduces the number of
  ! different implementations of procedures needed. (FUTURE PROJECT:
  ! objects with Icon-style indexing.)
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

  ! The type for a `range' of a vectar (which is a thing used by some
  ! procedures).
  public :: vectar_range_t

  ! Tests for the vectar_t type.
  public :: is_vectar          ! Is the given object a vectar_t?
  public :: is_not_vectar      ! Is the given object *not* a vectar_t?

  ! Do two objects refer to the same vectar in the garbage collector's
  ! heap?
  public :: vectar_t_eq

  ! Convert an object to a vectar, if possible.
  public :: vectar_t_cast
  public :: operator(.tovectar.)

  ! Convert an object to a vectar range, if possible.
  public :: vectar_range_t_cast
  public :: operator(.tovecrange.)

  ! Generic function: make a vectar from elements passed as arguments.
  public :: vectar

  ! Implementations of vectar.
  public :: vectar0
  public :: vectar1
  public :: vectar2
  public :: vectar3
  public :: vectar4
  public :: vectar5
  public :: vectar6
  public :: vectar7
  public :: vectar8
  public :: vectar9
  public :: vectar10
  public :: vectar11
  public :: vectar12
  public :: vectar13
  public :: vectar14
  public :: vectar15
  public :: vectar16
  public :: vectar17
  public :: vectar18
  public :: vectar19
  public :: vectar20

  ! Generic function: make a vectar of one value repeated.
  public :: make_vectar

  ! Implementations of make_vectar.
  public :: make_vectar_size_kind
  public :: make_vectar_int

  ! Return the length of a vectar, as an INTEGER(SIZE_KIND).
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
  public :: vectar_equal0
  public :: vectar_equal1
  public :: vectar_equal2
  public :: vectar_equal3
  public :: vectar_equal4
  public :: vectar_equal5
  public :: vectar_equal6
  public :: vectar_equal7
  public :: vectar_equal8
  public :: vectar_equal9
  public :: vectar_equal10
  public :: vectar_equal11
  public :: vectar_equal12
  public :: vectar_equal13
  public :: vectar_equal14
  public :: vectar_equal15
  public :: vectar_equal16
  public :: vectar_equal17
  public :: vectar_equal18
  public :: vectar_equal19
  public :: vectar_equal20

  ! Vectar-list conversions.
  public :: vectar_to_list
  public :: reverse_vectar_to_list
  public :: list_to_vectar
  public :: reverse_list_to_vectar

  ! Type-unbound generic versions of the `range' type-bound
  ! procedures.
  public :: vectar_range0
  public :: vectar_range1
  public :: vectar_rangen
  public :: range0              ! Synonym for vectar_range0.
  public :: range1              ! Synonym for vectar_range1.
  public :: rangen              ! Synonym for vectar_rangen.

  ! Implementations of the `range' functions.
  public :: vectar_range0_size_kind
  public :: vectar_range0_int
  public :: vectar_range1_size_kind
  public :: vectar_range1_int
  public :: vectar_rangen_size_kind
  public :: vectar_rangen_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! A private synonym for `size_kind'.
  integer, parameter :: sz = size_kind

  ! Private conversion to `size_kind'.
  interface operator(.sz.)
     module procedure int2sz
  end interface operator(.sz.)

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

     procedure, pass :: range0_size_kind => vectar_t_range0_size_kind
     procedure, pass :: range0_int => vectar_t_range0_int
     generic :: range0 => range0_size_kind
     generic :: range0 => range0_int

     procedure, pass :: range1_size_kind => vectar_t_range1_size_kind
     procedure, pass :: range1_int => vectar_t_range1_int
     generic :: range1 => range1_size_kind
     generic :: range1 => range1_int

     procedure, pass :: rangen_size_kind => vectar_t_rangen_size_kind
     procedure, pass :: rangen_int => vectar_t_rangen_int
     generic :: rangen => rangen_size_kind
     generic :: rangen => rangen_int
  end type vectar_t

  type :: vectar_range_t
     type(vectar_t), private :: vec_
     integer(sz), private :: index_
     integer(sz), private :: length_
   contains
     procedure, pass :: vec => vectar_range_t_vec

     procedure, pass :: istart0 => vectar_range_t_istart0
     procedure, pass :: iend0 => vectar_range_t_iend0

     procedure, pass :: istart1 => vectar_range_t_istart1
     procedure, pass :: iend1 => vectar_range_t_iend1

     procedure, pass :: istartn_size_kind => vectar_range_t_istartn_size_kind
     procedure, pass :: istartn_int => vectar_range_t_istartn_int
     generic :: istartn => istartn_size_kind
     generic :: istartn => istartn_int

     procedure, pass :: iendn_size_kind => vectar_range_t_iendn_size_kind
     procedure, pass :: iendn_int => vectar_range_t_iendn_int
     generic :: iendn => iendn_size_kind
     generic :: iendn => iendn_int
  end type vectar_range_t

  interface operator(.tovectar.)
     module procedure vectar_t_cast
  end interface operator(.tovectar.)

  interface operator(.tovecrange.)
     module procedure vectar_range_t_cast
  end interface operator(.tovecrange.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface vectar_range0
     module procedure vectar_range0_size_kind
     module procedure vectar_range0_int
  end interface vectar_range0

  interface range0
     module procedure vectar_range0_size_kind
     module procedure vectar_range0_int
  end interface range0

  interface vectar_range1
     module procedure vectar_range1_size_kind
     module procedure vectar_range1_int
  end interface vectar_range1

  interface range1
     module procedure vectar_range1_size_kind
     module procedure vectar_range1_int
  end interface range1

  interface vectar_rangen
     module procedure vectar_rangen_size_kind
     module procedure vectar_rangen_int
  end interface vectar_rangen

  interface rangen
     module procedure vectar_rangen_size_kind
     module procedure vectar_rangen_int
  end interface rangen

  interface vectar
     module procedure vectar0
     module procedure vectar1
     module procedure vectar2
     module procedure vectar3
     module procedure vectar4
     module procedure vectar5
     module procedure vectar6
     module procedure vectar7
     module procedure vectar8
     module procedure vectar9
     module procedure vectar10
     module procedure vectar11
     module procedure vectar12
     module procedure vectar13
     module procedure vectar14
     module procedure vectar15
     module procedure vectar16
     module procedure vectar17
     module procedure vectar18
     module procedure vectar19
     module procedure vectar20
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
     module procedure vectar_equal0
     module procedure vectar_equal1
     module procedure vectar_equal2
     module procedure vectar_equal3
     module procedure vectar_equal4
     module procedure vectar_equal5
     module procedure vectar_equal6
     module procedure vectar_equal7
     module procedure vectar_equal8
     module procedure vectar_equal9
     module procedure vectar_equal10
     module procedure vectar_equal11
     module procedure vectar_equal12
     module procedure vectar_equal13
     module procedure vectar_equal14
     module procedure vectar_equal15
     module procedure vectar_equal16
     module procedure vectar_equal17
     module procedure vectar_equal18
     module procedure vectar_equal19
     module procedure vectar_equal20
  end interface vectar_equal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Types for predicates.
  public :: vectar_predicate1_t ! A predicate taking 1 argument.
  public :: vectar_predicate2_t ! A predicate taking 2 arguments.
  public :: vectar_predicate3_t ! A predicate taking 3 arguments.
  public :: vectar_predicate4_t ! A predicate taking 4 arguments.
  public :: vectar_predicate5_t ! A predicate taking 5 arguments.
  public :: vectar_predicate6_t ! A predicate taking 6 arguments.
  public :: vectar_predicate7_t ! A predicate taking 7 arguments.
  public :: vectar_predicate8_t ! A predicate taking 8 arguments.
  public :: vectar_predicate9_t ! A predicate taking 9 arguments.
  public :: vectar_predicate10_t ! A predicate taking 10 arguments.

  abstract interface
     recursive function vectar_predicate1_t (x1) result (bool)
       class(*), intent(in) :: x1
       logical :: bool
     end function vectar_predicate1_t
     recursive function vectar_predicate2_t (x1, x2) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       logical :: bool
     end function vectar_predicate2_t
     recursive function vectar_predicate3_t (x1, x2, x3) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       logical :: bool
     end function vectar_predicate3_t
     recursive function vectar_predicate4_t (x1, x2, x3, x4) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       logical :: bool
     end function vectar_predicate4_t
     recursive function vectar_predicate5_t (x1, x2, x3, x4, x5) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       logical :: bool
     end function vectar_predicate5_t
     recursive function vectar_predicate6_t (x1, x2, x3, x4, x5, &
          x6) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       logical :: bool
     end function vectar_predicate6_t
     recursive function vectar_predicate7_t (x1, x2, x3, x4, x5, &
          x6, x7) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       logical :: bool
     end function vectar_predicate7_t
     recursive function vectar_predicate8_t (x1, x2, x3, x4, x5, &
          x6, x7, x8) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       class(*), intent(in) :: x8
       logical :: bool
     end function vectar_predicate8_t
     recursive function vectar_predicate9_t (x1, x2, x3, x4, x5, &
          x6, x7, x8, x9) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       class(*), intent(in) :: x8
       class(*), intent(in) :: x9
       logical :: bool
     end function vectar_predicate9_t
     recursive function vectar_predicate10_t (x1, x2, x3, x4, x5, &
          x6, x7, x8, x9, x10) result (bool)
       class(*), intent(in) :: x1
       class(*), intent(in) :: x2
       class(*), intent(in) :: x3
       class(*), intent(in) :: x4
       class(*), intent(in) :: x5
       class(*), intent(in) :: x6
       class(*), intent(in) :: x7
       class(*), intent(in) :: x8
       class(*), intent(in) :: x9
       class(*), intent(in) :: x10
       logical :: bool
     end function vectar_predicate10_t
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

!!$  elemental function sz2sz (i) result (j)
!!$    integer(sz), intent(in) :: i
!!$    integer(sz) :: j
!!$
!!$    j = i
!!$  end function sz2sz

  elemental function int2sz (i) result (j)
    integer, intent(in) :: i
    integer(sz) :: j

    j = i
  end function int2sz

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

  function vectar_range0_size_kind (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (.tovectar. vec, istart, iend)
  end function vectar_range0_size_kind

  function vectar_range0_int (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_int (.tovectar. vec, istart, iend)
  end function vectar_range0_int

  function vectar_range1_size_kind (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range1_size_kind (.tovectar. vec, istart, iend)
  end function vectar_range1_size_kind

  function vectar_range1_int (vec, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range1_int (.tovectar. vec, istart, iend)
  end function vectar_range1_int

  function vectar_rangen_size_kind (vec, n, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer(sz), intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_rangen_size_kind (.tovectar. vec, n, istart, iend)
  end function vectar_rangen_size_kind

  function vectar_rangen_int (vec, n, istart, iend) result (range)
    class(*), intent(in) :: vec
    integer, intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_rangen_int (.tovectar. vec, n, istart, iend)
  end function vectar_rangen_int

  function vectar_t_range0_size_kind (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    integer(sz) :: len

    range%vec_ = vec
    range%index_ = istart

    len = vectar_length (vec)

    if (istart < 0_sz .or. len < istart) then
       call error_abort ("vectar_t range start out of range")
    end if

    if (iend < istart) then
       range%length_ = 0
    else
       range%length_ = (iend - istart) + 1
    end if

    ! Any value of iend less than istart is legal.
    if (len <= iend) then
       call error_abort ("vectar_t range end out of range")
    end if
  end function vectar_t_range0_size_kind

  function vectar_t_range0_int (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, .sz. istart, .sz. iend)
  end function vectar_t_range0_int

  function vectar_t_range1_size_kind (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer(sz), intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, istart - 1, iend - 1)
  end function vectar_t_range1_size_kind

  function vectar_t_range1_int (vec, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer, intent(in) :: istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, (.sz. istart) - 1, (.sz. iend) - 1)
  end function vectar_t_range1_int

  function vectar_t_rangen_size_kind (vec, n, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer(sz), intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, istart - n, iend - n)
  end function vectar_t_rangen_size_kind

  function vectar_t_rangen_int (vec, n, istart, iend) result (range)
    class(vectar_t), intent(in) :: vec
    integer, intent(in) :: n, istart, iend
    type(vectar_range_t) :: range

    range = vectar_t_range0_size_kind (vec, (.sz. istart) - (.sz. n), (.sz. iend) - (.sz. n))
  end function vectar_t_rangen_int

  function vectar_range_t_vec (range) result (vec)
    class(vectar_range_t), intent(in) :: range
    type(vectar_t) :: vec

    vec = range%vec_
  end function vectar_range_t_vec
     
  pure function vectar_range_t_istart0 (range) result (istart0)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: istart0

    istart0 = range%index_
  end function vectar_range_t_istart0
     
  pure function vectar_range_t_iend0 (range) result (iend0)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: iend0

    iend0 = range%index_ + (range%length_ - 1)
  end function vectar_range_t_iend0

  pure function vectar_range_t_istart1 (range) result (istart1)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: istart1

    istart1 = range%index_ + 1
  end function vectar_range_t_istart1
     
  pure function vectar_range_t_iend1 (range) result (iend1)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: iend1

    iend1 = range%index_ + range%length_
  end function vectar_range_t_iend1

  pure function vectar_range_t_istartn_size_kind (range, n) result (istartn)
    class(vectar_range_t), intent(in) :: range
    integer(sz), intent(in) :: n
    integer(sz) :: istartn

    istartn = range%index_ + n
  end function vectar_range_t_istartn_size_kind
     
  pure function vectar_range_t_iendn_size_kind (range, n) result (iendn)
    class(vectar_range_t), intent(in) :: range
    integer(sz), intent(in) :: n
    integer(sz) :: iendn

    iendn = range%index_ + (range%length_ - 1) + n
  end function vectar_range_t_iendn_size_kind

  pure function vectar_range_t_istartn_int (range, n) result (istartn)
    class(vectar_range_t), intent(in) :: range
    integer, intent(in) :: n
    integer(sz) :: istartn

    istartn = range%index_ + (.sz. n)
  end function vectar_range_t_istartn_int
     
  pure function vectar_range_t_iendn_int (range, n) result (iendn)
    class(vectar_range_t), intent(in) :: range
    integer, intent(in) :: n
    integer(sz) :: iendn

    iendn = range%index_ + (range%length_ - 1) + (.sz. n)
  end function vectar_range_t_iendn_int

  recursive function vectar_range_t_cast (obj) result (range)
    class(*), intent(in) :: obj
    type(vectar_range_t) :: range

    type(vectar_data_t), pointer :: data

    select type (obj)
    class is (vectar_range_t)
       range = obj
    class is (vectar_t)
       data => vectar_data_ptr (obj)
       range = obj%range1 (1_sz, data%length)
    class is (gcroot_t)
       range = vectar_range_t_cast (.val. obj)
    class default
       call error_abort ("vectar_range_t_cast of an incompatible object")
    end select
  end function vectar_range_t_cast

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

  function vectar1 (obj1) result (vec)
    class(*), intent(in) :: obj1
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 1_sz
    allocate (data%array(0_sz:0_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar1

  function vectar2 (obj1, obj2) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 2_sz
    allocate (data%array(0_sz:1_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar2

  function vectar3 (obj1, obj2, obj3) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 3_sz
    allocate (data%array(0_sz:2_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar3

  function vectar4 (obj1, obj2, obj3, obj4) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 4_sz
    allocate (data%array(0_sz:3_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar4

  function vectar5 (obj1, obj2, obj3, obj4, obj5) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 5_sz
    allocate (data%array(0_sz:4_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar5

  function vectar6 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 6_sz
    allocate (data%array(0_sz:5_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar6

  function vectar7 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 7_sz
    allocate (data%array(0_sz:6_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar7

  function vectar8 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 8_sz
    allocate (data%array(0_sz:7_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar8

  function vectar9 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 9_sz
    allocate (data%array(0_sz:8_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar9

  function vectar10 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 10_sz
    allocate (data%array(0_sz:9_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar10

  function vectar11 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 11_sz
    allocate (data%array(0_sz:10_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar11

  function vectar12 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 12_sz
    allocate (data%array(0_sz:11_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar12

  function vectar13 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 13_sz
    allocate (data%array(0_sz:12_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar13

  function vectar14 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 14_sz
    allocate (data%array(0_sz:13_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar14

  function vectar15 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 15_sz
    allocate (data%array(0_sz:14_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar15

  function vectar16 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 16_sz
    allocate (data%array(0_sz:15_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar16

  function vectar17 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 17_sz
    allocate (data%array(0_sz:16_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar17

  function vectar18 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17, obj18) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    class(*), intent(in) :: obj18
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 18_sz
    allocate (data%array(0_sz:17_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    data%array(17_sz) = vectar_element_t (.autoval. obj18)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar18

  function vectar19 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17, obj18, obj19) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    class(*), intent(in) :: obj18
    class(*), intent(in) :: obj19
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 19_sz
    allocate (data%array(0_sz:18_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    data%array(17_sz) = vectar_element_t (.autoval. obj18)
    data%array(18_sz) = vectar_element_t (.autoval. obj19)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar19

  function vectar20 (obj1, obj2, obj3, obj4, obj5, &
       &            obj6, obj7, obj8, obj9, obj10, &
       &            obj11, obj12, obj13, obj14, obj15, &
       &            obj16, obj17, obj18, obj19, obj20) result (vec)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: obj10
    class(*), intent(in) :: obj11
    class(*), intent(in) :: obj12
    class(*), intent(in) :: obj13
    class(*), intent(in) :: obj14
    class(*), intent(in) :: obj15
    class(*), intent(in) :: obj16
    class(*), intent(in) :: obj17
    class(*), intent(in) :: obj18
    class(*), intent(in) :: obj19
    class(*), intent(in) :: obj20
    type(vectar_t) :: vec

    type(heap_element_t), pointer :: new_element
    type(vectar_data_t), pointer :: data

    allocate (data)
    data%length = 20_sz
    allocate (data%array(0_sz:19_sz))
    data%array(0_sz) = vectar_element_t (.autoval. obj1)
    data%array(1_sz) = vectar_element_t (.autoval. obj2)
    data%array(2_sz) = vectar_element_t (.autoval. obj3)
    data%array(3_sz) = vectar_element_t (.autoval. obj4)
    data%array(4_sz) = vectar_element_t (.autoval. obj5)
    data%array(5_sz) = vectar_element_t (.autoval. obj6)
    data%array(6_sz) = vectar_element_t (.autoval. obj7)
    data%array(7_sz) = vectar_element_t (.autoval. obj8)
    data%array(8_sz) = vectar_element_t (.autoval. obj9)
    data%array(9_sz) = vectar_element_t (.autoval. obj10)
    data%array(10_sz) = vectar_element_t (.autoval. obj11)
    data%array(11_sz) = vectar_element_t (.autoval. obj12)
    data%array(12_sz) = vectar_element_t (.autoval. obj13)
    data%array(13_sz) = vectar_element_t (.autoval. obj14)
    data%array(14_sz) = vectar_element_t (.autoval. obj15)
    data%array(15_sz) = vectar_element_t (.autoval. obj16)
    data%array(16_sz) = vectar_element_t (.autoval. obj17)
    data%array(17_sz) = vectar_element_t (.autoval. obj18)
    data%array(18_sz) = vectar_element_t (.autoval. obj19)
    data%array(19_sz) = vectar_element_t (.autoval. obj20)
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    vec%heap_element => new_element
  end function vectar20


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

    element = vectar_ref0_size_kind (vec, .sz. i)
  end function vectar_ref0_int

  function vectar_ref1_int (vec, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), allocatable :: element

    element = vectar_ref1_size_kind (vec, .sz. i)
  end function vectar_ref1_int

  function vectar_refn_int (vec, n, i) result (element)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), allocatable :: element

    element = vectar_refn_size_kind (vec, .sz. n, .sz. i)
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

    call vectar_set0_size_kind (vec, .sz. i, element)
  end subroutine vectar_set0_int

  subroutine vectar_set1_int (vec, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: i
    class(*), intent(in) :: element

    call vectar_set1_size_kind (vec, .sz. i, element)
  end subroutine vectar_set1_int

  subroutine vectar_setn_int (vec, n, i, element)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), intent(in) :: element

    call vectar_setn_size_kind (vec, .sz. n, .sz. i, element)
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

    call vectar_swap0_size_kind (vec, .sz. i, .sz. j)
  end subroutine vectar_swap0_int

  subroutine vectar_swap1_int (vec, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: i, j

    call vectar_swap1_size_kind (vec, .sz. i, .sz. j)
  end subroutine vectar_swap1_int

  subroutine vectar_swapn_int (vec, n, i, j)
    class(*), intent(in) :: vec
    integer, intent(in) :: n
    integer, intent(in) :: i, j

    call vectar_swapn_size_kind (vec, .sz. n, .sz. i, .sz. j)
  end subroutine vectar_swapn_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar_to_list (vec) result (lst)
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    type(vectar_t) :: v
    type(vectar_range_t) :: range
    integer(sz) :: i

    range = .tovecrange. vec
    v = range%vec()
    lst = nil
    do i = range%iend0(), range%istart0(), -1
       lst = vectar_ref0 (v, i) ** lst
    end do
  end function vectar_to_list

  function reverse_vectar_to_list (vec) result (lst)
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    type(vectar_t) :: v
    type(vectar_range_t) :: range
    integer(sz) :: i

    range = .tovecrange. vec
    v = range%vec()
    lst = nil
    do i = range%istart0(), range%iend0()
       lst = vectar_ref0 (v, i) ** lst
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

  recursive function vectar_equal2 (equal, vec1, vec2) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal2
  recursive function vectar_equal3 (equal, vec1, vec2, vec3) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal3
  recursive function vectar_equal4 (equal, vec1, vec2, vec3, vec4) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal4
  recursive function vectar_equal5 (equal, vec1, vec2, vec3, vec4, vec5) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal5
  recursive function vectar_equal6 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal6
  recursive function vectar_equal7 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal7
  recursive function vectar_equal8 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal8
  recursive function vectar_equal9 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal9
  recursive function vectar_equal10 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal10
  recursive function vectar_equal11 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal11
  recursive function vectar_equal12 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal12
  recursive function vectar_equal13 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal13
  recursive function vectar_equal14 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13, vec14) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    class(*), intent(in) :: vec14
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13, vec14)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal14
  recursive function vectar_equal15 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13, vec14, vec15) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    class(*), intent(in) :: vec14
    class(*), intent(in) :: vec15
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13, vec14, vec15)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal15
  recursive function vectar_equal16 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13, vec14, vec15, &
       &                            vec16) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    class(*), intent(in) :: vec14
    class(*), intent(in) :: vec15
    class(*), intent(in) :: vec16
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13, vec14, vec15, vec16)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal16
  recursive function vectar_equal17 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13, vec14, vec15, &
       &                            vec16, vec17) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    class(*), intent(in) :: vec14
    class(*), intent(in) :: vec15
    class(*), intent(in) :: vec16
    class(*), intent(in) :: vec17
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13, vec14, vec15, vec16, &
         &          vec17)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal17
  recursive function vectar_equal18 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13, vec14, vec15, &
       &                            vec16, vec17, vec18) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    class(*), intent(in) :: vec14
    class(*), intent(in) :: vec15
    class(*), intent(in) :: vec16
    class(*), intent(in) :: vec17
    class(*), intent(in) :: vec18
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13, vec14, vec15, vec16, &
         &          vec17, vec18)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal18
  recursive function vectar_equal19 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13, vec14, vec15, &
       &                            vec16, vec17, vec18, vec19) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    class(*), intent(in) :: vec14
    class(*), intent(in) :: vec15
    class(*), intent(in) :: vec16
    class(*), intent(in) :: vec17
    class(*), intent(in) :: vec18
    class(*), intent(in) :: vec19
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13, vec14, vec15, vec16, &
         &          vec17, vec18, vec19)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal19
  recursive function vectar_equal20 (equal, vec1, vec2, vec3, vec4, vec5, &
       &                            vec6, vec7, vec8, vec9, vec10, &
       &                            vec11, vec12, vec13, vec14, vec15, &
       &                            vec16, vec17, vec18, vec19, vec20) result (bool)
    procedure(vectar_predicate2_t) :: equal
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    class(*), intent(in) :: vec10
    class(*), intent(in) :: vec11
    class(*), intent(in) :: vec12
    class(*), intent(in) :: vec13
    class(*), intent(in) :: vec14
    class(*), intent(in) :: vec15
    class(*), intent(in) :: vec16
    class(*), intent(in) :: vec17
    class(*), intent(in) :: vec18
    class(*), intent(in) :: vec19
    class(*), intent(in) :: vec20
    logical :: bool

    type(cons_t) :: vectars

    vectars = list (vec1, vec2, vec3, vec4, &
         &          vec5, vec6, vec7, vec8, &
         &          vec9, vec10, vec11, vec12, &
         &          vec13, vec14, vec15, vec16, &
         &          vec17, vec18, vec19, vec20)
    bool = apply_vectar_equal (equal, vectars)
  end function vectar_equal20
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
