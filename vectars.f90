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

  ! Generic function: make a vectar of one value repeated (or with
  ! unspecified values).
  public :: make_vectar

  ! Implementations of make_vectar.
  public :: make_vectar_unspecified_fill_size_kind
  public :: make_vectar_unspecified_fill_int
  public :: make_vectar_fill_size_kind
  public :: make_vectar_fill_int

  ! Copy all or part of a vectar, to a new vectar.
  public :: vectar_copy

  ! Copy all or part of a vectar, to a new vectar, but with the order
  ! of elements reversed.
  public :: vectar_reverse_copy

  ! Generic function: return a new vector, appending the contents of
  ! vectars or vectar ranges to each other. This function does both
  ! what `vector-append' and `vector-append-subvectors' do in
  ! SRFI-133.
  public :: vectar_append

  ! Implementations of vectar_append.
  public :: vectar_append0
  public :: vectar_append1
  public :: vectar_append2
  public :: vectar_append3
  public :: vectar_append4
  public :: vectar_append5
  public :: vectar_append6
  public :: vectar_append7
  public :: vectar_append8
  public :: vectar_append9
  public :: vectar_append10
  public :: vectar_append11
  public :: vectar_append12
  public :: vectar_append13
  public :: vectar_append14
  public :: vectar_append15
  public :: vectar_append16
  public :: vectar_append17
  public :: vectar_append18
  public :: vectar_append19
  public :: vectar_append20

  ! Return a new vector, appending the contents of the vectars and
  ! vectar ranges given by a list. (In SRFI-133, `vector-concatenate'
  ! cannot append subvectors, the way our function here can.)
  public :: vectar_concatenate

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

  ! Some subroutines that (like the vectar sets and vectar swaps)
  ! alter the contents of a vectar. These accept vectar ranges, as
  ! well. (The `x' in the names is to remind us that the corresponding
  ! SRFI-133 procedures have a `!' in their names. We have not used
  ! `x' in the names of vectar set and swap subroutines, however.)
  public :: vectar_fillx        ! Fill with a repeated value.
  public :: vectar_reversex     ! Reversal in place.
  public :: vectar_copyx0       ! Generic function: copy data,
                                ! zero-based indexing. Source and
                                ! destination are allowed to overlap.
  public :: vectar_copyx1       ! Generic function: copy data,
                                ! one-based indexing. Source and
                                ! destination are allowed to overlap.
  public :: vectar_copyxn       ! Generic function: copy data,
                                ! n-based indexing. Source and
                                ! destination are allowed to overlap.

  ! Implementations of vectar_copyx0.
  public :: vectar_copyx0_size_kind
  public :: vectar_copyx0_int

  ! Implementations of vectar_copyx1.
  public :: vectar_copyx1_size_kind
  public :: vectar_copyx1_int

  ! Implementations of vectar_copyxn.
  public :: vectar_copyxn_size_kind
  public :: vectar_copyxn_int

  ! Vector equality. These accept vectar ranges and so are, in that
  ! respect, more general than their SRFI-133 equivalents.
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
     procedure, pass :: assign => vectar_range_t_assign
     generic :: assignment(=) => assign

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

     procedure, pass :: length => vectar_range_t_length
  end type vectar_range_t

  interface operator(.tovectar.)
     module procedure vectar_t_cast
  end interface operator(.tovectar.)

  interface operator(.tovecrange.)
     module procedure vectar_range_t_cast
  end interface operator(.tovecrange.)

  ! A private type for `unspecified' values.
  type :: unspecified_t
  end type unspecified_t

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
     module procedure make_vectar_unspecified_fill_size_kind
     module procedure make_vectar_unspecified_fill_int
     module procedure make_vectar_fill_size_kind
     module procedure make_vectar_fill_int
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

  interface vectar_copyx0
     module procedure vectar_copyx0_size_kind
     module procedure vectar_copyx0_int
  end interface vectar_copyx0

  interface vectar_copyx1
     module procedure vectar_copyx1_size_kind
     module procedure vectar_copyx1_int
  end interface vectar_copyx1

  interface vectar_copyxn
     module procedure vectar_copyxn_size_kind
     module procedure vectar_copyxn_int
  end interface vectar_copyxn

  interface vectar_append
     module procedure vectar_append0
     module procedure vectar_append1
     module procedure vectar_append2
     module procedure vectar_append3
     module procedure vectar_append4
     module procedure vectar_append5
     module procedure vectar_append6
     module procedure vectar_append7
     module procedure vectar_append8
     module procedure vectar_append9
     module procedure vectar_append10
     module procedure vectar_append11
     module procedure vectar_append12
     module procedure vectar_append13
     module procedure vectar_append14
     module procedure vectar_append15
     module procedure vectar_append16
     module procedure vectar_append17
     module procedure vectar_append18
     module procedure vectar_append19
     module procedure vectar_append20
  end interface vectar_append

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

    !
    ! NOTES:
    !
    !   * Any value of iend less than istart is legal, and indicates a
    !     range of length zero.
    !
    !   * If iend is less than istart, then istart may hold any value.
    !

    integer(sz) :: len

    range%vec_ = vec
    range%index_ = istart

    len = vectar_length (vec)

    if (iend < istart) then
       range%length_ = 0
    else
       if (istart < 0_sz .or. len <= iend) then
          call error_abort ("vectar_t range indices are out of range")
       end if
       range%length_ = (iend - istart) + 1
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

  pure function vectar_range_t_length (range) result (len)
    class(vectar_range_t), intent(in) :: range
    integer(sz) :: len

    len = range%length_
  end function vectar_range_t_length

  recursive subroutine vectar_range_t_assign (dst, src)
    class(vectar_range_t), intent(inout) :: dst
    class(*), intent(in) :: src

    type(vectar_data_t), pointer :: data

    select type (src)
    class is (vectar_range_t)
       dst%vec_ = src%vec_
       dst%index_ = src%index_
       dst%length_ = src%length_
    class is (vectar_t)
       data => vectar_data_ptr (src)
       dst%vec_ = src
       dst%index_ = 0_sz
       dst%length_ = data%length
    class is (gcroot_t)
       call vectar_range_t_assign (dst, .val. src)
    class default
       call error_abort ("assignment to vectar_range_t from an incompatible object")
    end select
  end subroutine vectar_range_t_assign

  recursive function vectar_range_t_cast (obj) result (range)
    class(*), intent(in) :: obj
    type(vectar_range_t) :: range

    range = obj
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


  function make_vectar_unspecified_fill_size_kind (size) result (vec)
    integer(sz), intent(in) :: size
    type(vectar_t) :: vec

    vec = make_vectar_fill_size_kind (size, unspecified_t ())
  end function make_vectar_unspecified_fill_size_kind

  function make_vectar_unspecified_fill_int (size) result (vec)
    integer, intent(in) :: size
    type(vectar_t) :: vec

    vec = make_vectar_fill_size_kind (.sz. size, unspecified_t ())
  end function make_vectar_unspecified_fill_int

  function make_vectar_fill_size_kind (size, fill) result (vec)
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
  end function make_vectar_fill_size_kind

  function make_vectar_fill_int (size, fill) result (vec)
    integer, intent(in) :: size
    class(*), intent(in) :: fill
    type(vectar_t) :: vec

    vec = make_vectar_fill_size_kind (.sz. size, fill)
  end function make_vectar_fill_int

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

  subroutine vectar_fillx (vec, fill)
    class(*), intent(in) :: vec
    class(*), intent(in) :: fill

    type(vectar_range_t) :: range
    type(vectar_data_t), pointer :: data
    integer(sz) :: i

    range = vec
    data => vectar_data_ptr (range%vec())
    do i = range%istart0(), range%iend0()
       data%array(i)%element = fill
    end do
  end subroutine vectar_fillx

  subroutine vectar_reversex (vec)
    class(*), intent(in) :: vec

    type(vectar_range_t) :: range
    type(vectar_data_t), pointer :: data
    integer(sz) :: i, j
    class(*), allocatable :: tmp

    range = vec
    data => vectar_data_ptr (range%vec())
    i = range%istart0()
    j = range%iend0()
    do while (i < j)
       tmp = data%array(i)%element
       data%array(i)%element = data%array(j)%element
       data%array(j)%element = tmp
       i = i + 1
       j = j - 1
    end do
  end subroutine vectar_reversex

  subroutine vectar_copyx0_size_kind (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: i
    class(*), intent(in) :: src

    type(vectar_range_t) :: src_range
    type(vectar_data_t), pointer :: src_data, dst_data
    integer(sz) :: copy_len
    integer(sz) :: j, k

    src_range = src

    src_data => vectar_data_ptr (src_range%vec())
    dst_data => vectar_data_ptr (dst)

    if (i < 0_sz .or. dst_data%length <= i) then
       call error_abort ("vectar_copyx0 destination index is out of range")
    end if

    copy_len = src_range%length()

    if (dst_data%length - i < copy_len) then
       call error_abort ("vectar_copyx0 destination is shorter than the source")
    end if

    if (i <= src_range%istart0()) then
       j = src_range%istart0()
       k = i
       do while (k < i + copy_len)
          dst_data%array(k)%element = src_data%array(j)%element
          j = j + 1
          k = k + 1
       end do
    else
       j = src_range%istart0() + copy_len
       k = i + copy_len
       do while (i < k)
          j = j - 1
          k = k - 1
          dst_data%array(k)%element = src_data%array(j)%element
       end do
    end if
  end subroutine vectar_copyx0_size_kind

  subroutine vectar_copyx0_int (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, .sz. i, src)
  end subroutine vectar_copyx0_int

  subroutine vectar_copyx1_size_kind (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, i - 1_sz, src)
  end subroutine vectar_copyx1_size_kind

  subroutine vectar_copyx1_int (dst, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, (.sz. i) - 1_sz, src)
  end subroutine vectar_copyx1_int

  subroutine vectar_copyxn_size_kind (dst, n, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer(sz) :: n
    integer(sz) :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, i - n, src)
  end subroutine vectar_copyxn_size_kind

  subroutine vectar_copyxn_int (dst, n, i, src)
    !
    ! Copy from the source (which may be a range) to the destination,
    ! starting at destination index i.
    !
    class(*), intent(in) :: dst
    integer :: n
    integer :: i
    class(*), intent(in) :: src

    call vectar_copyx0_size_kind (dst, (.sz. i) - (.sz. n), src)
  end subroutine vectar_copyxn_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function vectar_to_list (vec) result (lst)
    class(*), intent(in) :: vec
    type(cons_t) :: lst

    type(vectar_t) :: v
    type(vectar_range_t) :: range
    integer(sz) :: i

    range = vec
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

    range = vec
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

  function vectar_copy (vec) result (vec_copy)

    !
    ! DESIGN NOTE: I had considered supporting SRFI-43-style vector
    ! copy, wherein it is possible to extend the length of the
    ! destination vector. Then, however, I remembered I was using
    ! vectar_range_t instead of `start' and `end' parameters. It is
    ! not legal for the end of a vectar_range_t to extend past the end
    ! of the source vector. Therefore SRFI-43-style vector copy is not
    ! possible.
    !

    class(*), intent(in) :: vec
    type(vectar_t) :: vec_copy

    type(vectar_t) :: v
    type(vectar_range_t) :: range
    type(vectar_data_t), pointer :: src, dst
    integer(sz) :: istart, iend, size, i

    range = vec

    v = range%vec()
    istart = range%istart0()
    iend = range%iend0()
    size = (iend - istart) + 1_sz

    vec_copy = make_vectar (size)

    if (0_sz < size) then
       src => vectar_data_ptr (v)
       dst => vectar_data_ptr (vec_copy)

       do i = 0_sz, size - 1_sz
          dst%array(i) = src%array(istart + i)
       end do
    end if
  end function vectar_copy

  function vectar_reverse_copy (vec) result (vec_copy)
    class(*), intent(in) :: vec
    type(vectar_t) :: vec_copy

    type(vectar_t) :: v
    type(vectar_range_t) :: range
    type(vectar_data_t), pointer :: src, dst
    integer(sz) :: istart, iend, size, i

    range = vec

    v = range%vec()
    istart = range%istart0()
    iend = range%iend0()

    size = (iend - istart) + 1_sz

    vec_copy = make_vectar (size)

    if (0_sz < size) then
       src => vectar_data_ptr (v)
       dst => vectar_data_ptr (vec_copy)

       do i = 0_sz, size - 1_sz
          dst%array(i) = src%array(iend - i)
       end do
    end if
  end function vectar_reverse_copy

  function vectar_append0 () result (vec_a)
    type(vectar_t) :: vec_a

    vec_a = vectar ()
  end function vectar_append0

  function vectar_append1 (vec1) result (vec_a)
    class(*), intent(in) :: vec1
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_t) :: v1
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do

  end function vectar_append1
  function vectar_append2 (vec1, vec2) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do

  end function vectar_append2
  function vectar_append3 (vec1, vec2, vec3) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do

  end function vectar_append3
  function vectar_append4 (vec1, vec2, vec3, vec4) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do

  end function vectar_append4
  function vectar_append5 (vec1, vec2, vec3, vec4, vec5) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do

  end function vectar_append5
  function vectar_append6 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do

  end function vectar_append6
  function vectar_append7 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do

  end function vectar_append7
  function vectar_append8 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do

  end function vectar_append8
  function vectar_append9 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9) result (vec_a)
    class(*), intent(in) :: vec1
    class(*), intent(in) :: vec2
    class(*), intent(in) :: vec3
    class(*), intent(in) :: vec4
    class(*), intent(in) :: vec5
    class(*), intent(in) :: vec6
    class(*), intent(in) :: vec7
    class(*), intent(in) :: vec8
    class(*), intent(in) :: vec9
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do

  end function vectar_append9
  function vectar_append10 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do

  end function vectar_append10
  function vectar_append11 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do

  end function vectar_append11
  function vectar_append12 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do

  end function vectar_append12
  function vectar_append13 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do

  end function vectar_append13
  function vectar_append14 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13, vec14) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_range_t) :: range14
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_t) :: v14
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: src14
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()
    range14 = vec14
    v14 = range14%vec()
    src14 => vectar_data_ptr (v14)
    len_vec_a = len_vec_a + range14%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do
    do i = range14%istart0(), range14%iend0()
       dst%array(j) = src14%array(i)
       j = j + 1_sz
    end do

  end function vectar_append14
  function vectar_append15 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13, vec14, vec15) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_range_t) :: range14
    type(vectar_range_t) :: range15
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_t) :: v14
    type(vectar_t) :: v15
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: src14
    type(vectar_data_t), pointer :: src15
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()
    range14 = vec14
    v14 = range14%vec()
    src14 => vectar_data_ptr (v14)
    len_vec_a = len_vec_a + range14%length()
    range15 = vec15
    v15 = range15%vec()
    src15 => vectar_data_ptr (v15)
    len_vec_a = len_vec_a + range15%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do
    do i = range14%istart0(), range14%iend0()
       dst%array(j) = src14%array(i)
       j = j + 1_sz
    end do
    do i = range15%istart0(), range15%iend0()
       dst%array(j) = src15%array(i)
       j = j + 1_sz
    end do

  end function vectar_append15
  function vectar_append16 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13, vec14, vec15, &
       &                   vec16) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_range_t) :: range14
    type(vectar_range_t) :: range15
    type(vectar_range_t) :: range16
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_t) :: v14
    type(vectar_t) :: v15
    type(vectar_t) :: v16
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: src14
    type(vectar_data_t), pointer :: src15
    type(vectar_data_t), pointer :: src16
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()
    range14 = vec14
    v14 = range14%vec()
    src14 => vectar_data_ptr (v14)
    len_vec_a = len_vec_a + range14%length()
    range15 = vec15
    v15 = range15%vec()
    src15 => vectar_data_ptr (v15)
    len_vec_a = len_vec_a + range15%length()
    range16 = vec16
    v16 = range16%vec()
    src16 => vectar_data_ptr (v16)
    len_vec_a = len_vec_a + range16%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do
    do i = range14%istart0(), range14%iend0()
       dst%array(j) = src14%array(i)
       j = j + 1_sz
    end do
    do i = range15%istart0(), range15%iend0()
       dst%array(j) = src15%array(i)
       j = j + 1_sz
    end do
    do i = range16%istart0(), range16%iend0()
       dst%array(j) = src16%array(i)
       j = j + 1_sz
    end do

  end function vectar_append16
  function vectar_append17 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13, vec14, vec15, &
       &                   vec16, vec17) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_range_t) :: range14
    type(vectar_range_t) :: range15
    type(vectar_range_t) :: range16
    type(vectar_range_t) :: range17
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_t) :: v14
    type(vectar_t) :: v15
    type(vectar_t) :: v16
    type(vectar_t) :: v17
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: src14
    type(vectar_data_t), pointer :: src15
    type(vectar_data_t), pointer :: src16
    type(vectar_data_t), pointer :: src17
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()
    range14 = vec14
    v14 = range14%vec()
    src14 => vectar_data_ptr (v14)
    len_vec_a = len_vec_a + range14%length()
    range15 = vec15
    v15 = range15%vec()
    src15 => vectar_data_ptr (v15)
    len_vec_a = len_vec_a + range15%length()
    range16 = vec16
    v16 = range16%vec()
    src16 => vectar_data_ptr (v16)
    len_vec_a = len_vec_a + range16%length()
    range17 = vec17
    v17 = range17%vec()
    src17 => vectar_data_ptr (v17)
    len_vec_a = len_vec_a + range17%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do
    do i = range14%istart0(), range14%iend0()
       dst%array(j) = src14%array(i)
       j = j + 1_sz
    end do
    do i = range15%istart0(), range15%iend0()
       dst%array(j) = src15%array(i)
       j = j + 1_sz
    end do
    do i = range16%istart0(), range16%iend0()
       dst%array(j) = src16%array(i)
       j = j + 1_sz
    end do
    do i = range17%istart0(), range17%iend0()
       dst%array(j) = src17%array(i)
       j = j + 1_sz
    end do

  end function vectar_append17
  function vectar_append18 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13, vec14, vec15, &
       &                   vec16, vec17, vec18) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_range_t) :: range14
    type(vectar_range_t) :: range15
    type(vectar_range_t) :: range16
    type(vectar_range_t) :: range17
    type(vectar_range_t) :: range18
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_t) :: v14
    type(vectar_t) :: v15
    type(vectar_t) :: v16
    type(vectar_t) :: v17
    type(vectar_t) :: v18
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: src14
    type(vectar_data_t), pointer :: src15
    type(vectar_data_t), pointer :: src16
    type(vectar_data_t), pointer :: src17
    type(vectar_data_t), pointer :: src18
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()
    range14 = vec14
    v14 = range14%vec()
    src14 => vectar_data_ptr (v14)
    len_vec_a = len_vec_a + range14%length()
    range15 = vec15
    v15 = range15%vec()
    src15 => vectar_data_ptr (v15)
    len_vec_a = len_vec_a + range15%length()
    range16 = vec16
    v16 = range16%vec()
    src16 => vectar_data_ptr (v16)
    len_vec_a = len_vec_a + range16%length()
    range17 = vec17
    v17 = range17%vec()
    src17 => vectar_data_ptr (v17)
    len_vec_a = len_vec_a + range17%length()
    range18 = vec18
    v18 = range18%vec()
    src18 => vectar_data_ptr (v18)
    len_vec_a = len_vec_a + range18%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do
    do i = range14%istart0(), range14%iend0()
       dst%array(j) = src14%array(i)
       j = j + 1_sz
    end do
    do i = range15%istart0(), range15%iend0()
       dst%array(j) = src15%array(i)
       j = j + 1_sz
    end do
    do i = range16%istart0(), range16%iend0()
       dst%array(j) = src16%array(i)
       j = j + 1_sz
    end do
    do i = range17%istart0(), range17%iend0()
       dst%array(j) = src17%array(i)
       j = j + 1_sz
    end do
    do i = range18%istart0(), range18%iend0()
       dst%array(j) = src18%array(i)
       j = j + 1_sz
    end do

  end function vectar_append18
  function vectar_append19 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13, vec14, vec15, &
       &                   vec16, vec17, vec18, vec19) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_range_t) :: range14
    type(vectar_range_t) :: range15
    type(vectar_range_t) :: range16
    type(vectar_range_t) :: range17
    type(vectar_range_t) :: range18
    type(vectar_range_t) :: range19
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_t) :: v14
    type(vectar_t) :: v15
    type(vectar_t) :: v16
    type(vectar_t) :: v17
    type(vectar_t) :: v18
    type(vectar_t) :: v19
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: src14
    type(vectar_data_t), pointer :: src15
    type(vectar_data_t), pointer :: src16
    type(vectar_data_t), pointer :: src17
    type(vectar_data_t), pointer :: src18
    type(vectar_data_t), pointer :: src19
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()
    range14 = vec14
    v14 = range14%vec()
    src14 => vectar_data_ptr (v14)
    len_vec_a = len_vec_a + range14%length()
    range15 = vec15
    v15 = range15%vec()
    src15 => vectar_data_ptr (v15)
    len_vec_a = len_vec_a + range15%length()
    range16 = vec16
    v16 = range16%vec()
    src16 => vectar_data_ptr (v16)
    len_vec_a = len_vec_a + range16%length()
    range17 = vec17
    v17 = range17%vec()
    src17 => vectar_data_ptr (v17)
    len_vec_a = len_vec_a + range17%length()
    range18 = vec18
    v18 = range18%vec()
    src18 => vectar_data_ptr (v18)
    len_vec_a = len_vec_a + range18%length()
    range19 = vec19
    v19 = range19%vec()
    src19 => vectar_data_ptr (v19)
    len_vec_a = len_vec_a + range19%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do
    do i = range14%istart0(), range14%iend0()
       dst%array(j) = src14%array(i)
       j = j + 1_sz
    end do
    do i = range15%istart0(), range15%iend0()
       dst%array(j) = src15%array(i)
       j = j + 1_sz
    end do
    do i = range16%istart0(), range16%iend0()
       dst%array(j) = src16%array(i)
       j = j + 1_sz
    end do
    do i = range17%istart0(), range17%iend0()
       dst%array(j) = src17%array(i)
       j = j + 1_sz
    end do
    do i = range18%istart0(), range18%iend0()
       dst%array(j) = src18%array(i)
       j = j + 1_sz
    end do
    do i = range19%istart0(), range19%iend0()
       dst%array(j) = src19%array(i)
       j = j + 1_sz
    end do

  end function vectar_append19
  function vectar_append20 (vec1, vec2, vec3, vec4, vec5, &
       &                   vec6, vec7, vec8, vec9, vec10, &
       &                   vec11, vec12, vec13, vec14, vec15, &
       &                   vec16, vec17, vec18, vec19, vec20) result (vec_a)
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
    type(vectar_t) :: vec_a

    type(vectar_range_t) :: range1
    type(vectar_range_t) :: range2
    type(vectar_range_t) :: range3
    type(vectar_range_t) :: range4
    type(vectar_range_t) :: range5
    type(vectar_range_t) :: range6
    type(vectar_range_t) :: range7
    type(vectar_range_t) :: range8
    type(vectar_range_t) :: range9
    type(vectar_range_t) :: range10
    type(vectar_range_t) :: range11
    type(vectar_range_t) :: range12
    type(vectar_range_t) :: range13
    type(vectar_range_t) :: range14
    type(vectar_range_t) :: range15
    type(vectar_range_t) :: range16
    type(vectar_range_t) :: range17
    type(vectar_range_t) :: range18
    type(vectar_range_t) :: range19
    type(vectar_range_t) :: range20
    type(vectar_t) :: v1
    type(vectar_t) :: v2
    type(vectar_t) :: v3
    type(vectar_t) :: v4
    type(vectar_t) :: v5
    type(vectar_t) :: v6
    type(vectar_t) :: v7
    type(vectar_t) :: v8
    type(vectar_t) :: v9
    type(vectar_t) :: v10
    type(vectar_t) :: v11
    type(vectar_t) :: v12
    type(vectar_t) :: v13
    type(vectar_t) :: v14
    type(vectar_t) :: v15
    type(vectar_t) :: v16
    type(vectar_t) :: v17
    type(vectar_t) :: v18
    type(vectar_t) :: v19
    type(vectar_t) :: v20
    type(vectar_data_t), pointer :: src1
    type(vectar_data_t), pointer :: src2
    type(vectar_data_t), pointer :: src3
    type(vectar_data_t), pointer :: src4
    type(vectar_data_t), pointer :: src5
    type(vectar_data_t), pointer :: src6
    type(vectar_data_t), pointer :: src7
    type(vectar_data_t), pointer :: src8
    type(vectar_data_t), pointer :: src9
    type(vectar_data_t), pointer :: src10
    type(vectar_data_t), pointer :: src11
    type(vectar_data_t), pointer :: src12
    type(vectar_data_t), pointer :: src13
    type(vectar_data_t), pointer :: src14
    type(vectar_data_t), pointer :: src15
    type(vectar_data_t), pointer :: src16
    type(vectar_data_t), pointer :: src17
    type(vectar_data_t), pointer :: src18
    type(vectar_data_t), pointer :: src19
    type(vectar_data_t), pointer :: src20
    type(vectar_data_t), pointer :: dst
    integer(sz) :: len_vec_a
    integer(sz) :: i, j

    len_vec_a = 0_sz
    range1 = vec1
    v1 = range1%vec()
    src1 => vectar_data_ptr (v1)
    len_vec_a = len_vec_a + range1%length()
    range2 = vec2
    v2 = range2%vec()
    src2 => vectar_data_ptr (v2)
    len_vec_a = len_vec_a + range2%length()
    range3 = vec3
    v3 = range3%vec()
    src3 => vectar_data_ptr (v3)
    len_vec_a = len_vec_a + range3%length()
    range4 = vec4
    v4 = range4%vec()
    src4 => vectar_data_ptr (v4)
    len_vec_a = len_vec_a + range4%length()
    range5 = vec5
    v5 = range5%vec()
    src5 => vectar_data_ptr (v5)
    len_vec_a = len_vec_a + range5%length()
    range6 = vec6
    v6 = range6%vec()
    src6 => vectar_data_ptr (v6)
    len_vec_a = len_vec_a + range6%length()
    range7 = vec7
    v7 = range7%vec()
    src7 => vectar_data_ptr (v7)
    len_vec_a = len_vec_a + range7%length()
    range8 = vec8
    v8 = range8%vec()
    src8 => vectar_data_ptr (v8)
    len_vec_a = len_vec_a + range8%length()
    range9 = vec9
    v9 = range9%vec()
    src9 => vectar_data_ptr (v9)
    len_vec_a = len_vec_a + range9%length()
    range10 = vec10
    v10 = range10%vec()
    src10 => vectar_data_ptr (v10)
    len_vec_a = len_vec_a + range10%length()
    range11 = vec11
    v11 = range11%vec()
    src11 => vectar_data_ptr (v11)
    len_vec_a = len_vec_a + range11%length()
    range12 = vec12
    v12 = range12%vec()
    src12 => vectar_data_ptr (v12)
    len_vec_a = len_vec_a + range12%length()
    range13 = vec13
    v13 = range13%vec()
    src13 => vectar_data_ptr (v13)
    len_vec_a = len_vec_a + range13%length()
    range14 = vec14
    v14 = range14%vec()
    src14 => vectar_data_ptr (v14)
    len_vec_a = len_vec_a + range14%length()
    range15 = vec15
    v15 = range15%vec()
    src15 => vectar_data_ptr (v15)
    len_vec_a = len_vec_a + range15%length()
    range16 = vec16
    v16 = range16%vec()
    src16 => vectar_data_ptr (v16)
    len_vec_a = len_vec_a + range16%length()
    range17 = vec17
    v17 = range17%vec()
    src17 => vectar_data_ptr (v17)
    len_vec_a = len_vec_a + range17%length()
    range18 = vec18
    v18 = range18%vec()
    src18 => vectar_data_ptr (v18)
    len_vec_a = len_vec_a + range18%length()
    range19 = vec19
    v19 = range19%vec()
    src19 => vectar_data_ptr (v19)
    len_vec_a = len_vec_a + range19%length()
    range20 = vec20
    v20 = range20%vec()
    src20 => vectar_data_ptr (v20)
    len_vec_a = len_vec_a + range20%length()

    vec_a = make_vectar (len_vec_a)

    dst => vectar_data_ptr (vec_a)

    j = 0_sz
    do i = range1%istart0(), range1%iend0()
       dst%array(j) = src1%array(i)
       j = j + 1_sz
    end do
    do i = range2%istart0(), range2%iend0()
       dst%array(j) = src2%array(i)
       j = j + 1_sz
    end do
    do i = range3%istart0(), range3%iend0()
       dst%array(j) = src3%array(i)
       j = j + 1_sz
    end do
    do i = range4%istart0(), range4%iend0()
       dst%array(j) = src4%array(i)
       j = j + 1_sz
    end do
    do i = range5%istart0(), range5%iend0()
       dst%array(j) = src5%array(i)
       j = j + 1_sz
    end do
    do i = range6%istart0(), range6%iend0()
       dst%array(j) = src6%array(i)
       j = j + 1_sz
    end do
    do i = range7%istart0(), range7%iend0()
       dst%array(j) = src7%array(i)
       j = j + 1_sz
    end do
    do i = range8%istart0(), range8%iend0()
       dst%array(j) = src8%array(i)
       j = j + 1_sz
    end do
    do i = range9%istart0(), range9%iend0()
       dst%array(j) = src9%array(i)
       j = j + 1_sz
    end do
    do i = range10%istart0(), range10%iend0()
       dst%array(j) = src10%array(i)
       j = j + 1_sz
    end do
    do i = range11%istart0(), range11%iend0()
       dst%array(j) = src11%array(i)
       j = j + 1_sz
    end do
    do i = range12%istart0(), range12%iend0()
       dst%array(j) = src12%array(i)
       j = j + 1_sz
    end do
    do i = range13%istart0(), range13%iend0()
       dst%array(j) = src13%array(i)
       j = j + 1_sz
    end do
    do i = range14%istart0(), range14%iend0()
       dst%array(j) = src14%array(i)
       j = j + 1_sz
    end do
    do i = range15%istart0(), range15%iend0()
       dst%array(j) = src15%array(i)
       j = j + 1_sz
    end do
    do i = range16%istart0(), range16%iend0()
       dst%array(j) = src16%array(i)
       j = j + 1_sz
    end do
    do i = range17%istart0(), range17%iend0()
       dst%array(j) = src17%array(i)
       j = j + 1_sz
    end do
    do i = range18%istart0(), range18%iend0()
       dst%array(j) = src18%array(i)
       j = j + 1_sz
    end do
    do i = range19%istart0(), range19%iend0()
       dst%array(j) = src19%array(i)
       j = j + 1_sz
    end do
    do i = range20%istart0(), range20%iend0()
       dst%array(j) = src20%array(i)
       j = j + 1_sz
    end do

  end function vectar_append20
  function vectar_concatenate (vectars) result (vec_c)
    class(*), intent(in) :: vectars
    type(vectar_t) :: vec_c

    integer(sz) :: num_vectars

    num_vectars = length (vectars)

    select case (num_vectars)
    case (0)
       vec_c = vectar ()
    case (1)
       block
         class(*), allocatable :: vec1
         call unlist (vectars, vec1)
         vec_c = vectar_append (vec1)
       end block
    case (2)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         call unlist (vectars, vec1, vec2)
         vec_c = vectar_append (vec1, vec2)
       end block
    case (3)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         call unlist (vectars, vec1, vec2, vec3)
         vec_c = vectar_append (vec1, vec2, vec3)
       end block
    case (4)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         call unlist (vectars, vec1, vec2, vec3, vec4)
         vec_c = vectar_append (vec1, vec2, vec3, vec4)
       end block
    case (5)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5)
       end block
    case (6)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6)
       end block
    case (7)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7)
       end block
    case (8)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8)
       end block
    case (9)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9)
       end block
    case (10)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10)
       end block
    case (11)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11)
       end block
    case (12)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12)
       end block
    case (13)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13)
       end block
    case (14)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         class(*), allocatable :: vec14
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13, vec14)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13, vec14)
       end block
    case (15)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         class(*), allocatable :: vec14
         class(*), allocatable :: vec15
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13, vec14, vec15)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13, vec14, vec15)
       end block
    case (16)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         class(*), allocatable :: vec14
         class(*), allocatable :: vec15
         class(*), allocatable :: vec16
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13, vec14, vec15, &
              &       vec16)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13, vec14, vec15, &
              &                 vec16)
       end block
    case (17)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         class(*), allocatable :: vec14
         class(*), allocatable :: vec15
         class(*), allocatable :: vec16
         class(*), allocatable :: vec17
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13, vec14, vec15, &
              &       vec16, vec17)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13, vec14, vec15, &
              &                 vec16, vec17)
       end block
    case (18)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         class(*), allocatable :: vec14
         class(*), allocatable :: vec15
         class(*), allocatable :: vec16
         class(*), allocatable :: vec17
         class(*), allocatable :: vec18
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13, vec14, vec15, &
              &       vec16, vec17, vec18)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13, vec14, vec15, &
              &                 vec16, vec17, vec18)
       end block
    case (19)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         class(*), allocatable :: vec14
         class(*), allocatable :: vec15
         class(*), allocatable :: vec16
         class(*), allocatable :: vec17
         class(*), allocatable :: vec18
         class(*), allocatable :: vec19
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13, vec14, vec15, &
              &       vec16, vec17, vec18, vec19)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13, vec14, vec15, &
              &                 vec16, vec17, vec18, vec19)
       end block
    case (20)
       block
         class(*), allocatable :: vec1
         class(*), allocatable :: vec2
         class(*), allocatable :: vec3
         class(*), allocatable :: vec4
         class(*), allocatable :: vec5
         class(*), allocatable :: vec6
         class(*), allocatable :: vec7
         class(*), allocatable :: vec8
         class(*), allocatable :: vec9
         class(*), allocatable :: vec10
         class(*), allocatable :: vec11
         class(*), allocatable :: vec12
         class(*), allocatable :: vec13
         class(*), allocatable :: vec14
         class(*), allocatable :: vec15
         class(*), allocatable :: vec16
         class(*), allocatable :: vec17
         class(*), allocatable :: vec18
         class(*), allocatable :: vec19
         class(*), allocatable :: vec20
         call unlist (vectars, vec1, vec2, vec3, vec4, vec5, &
              &       vec6, vec7, vec8, vec9, vec10, &
              &       vec11, vec12, vec13, vec14, vec15, &
              &       vec16, vec17, vec18, vec19, vec20)
         vec_c = vectar_append (vec1, vec2, vec3, vec4, vec5, &
              &                 vec6, vec7, vec8, vec9, vec10, &
              &                 vec11, vec12, vec13, vec14, vec15, &
              &                 vec16, vec17, vec18, vec19, vec20)
       end block
    case default
       block
         type(cons_t) :: p
         type(cons_t) :: vecs_reversed
         type(vectar_range_t) :: range
         integer(sz) :: len_vec_c
         integer(sz) :: i, j
         type(vectar_data_t), pointer :: src, dst

         vecs_reversed = nil
         len_vec_c = 0_sz
         p = vectars
         do i = 1_sz, num_vectars
            range = car (p)
            vecs_reversed = range ** vecs_reversed
            len_vec_c = len_vec_c + range%length()
            p = cdr (p)
         end do

         vec_c = make_vectar (len_vec_c)

         dst => vectar_data_ptr (vec_c)

         j = len_vec_c
         p = vecs_reversed
         do while (is_pair (p))
            range = car (p)
            src => vectar_data_ptr (range%vec())
            do i = range%iend0(), range%istart0(), -1
               j = j - 1
               dst%array(j) = src%array(i)
            end do
            p = cdr (p)
         end do
       end block
    end select
  end function vectar_concatenate

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
    type(vectar_range_t), allocatable :: vr(:)
    type(vectar_data_p_t), allocatable :: p(:)

    n = length (vectars)
    if (n == 0_sz) then
       ! No vectars were given.
       bool = .true.
    else if (n == 1_sz) then
       ! Only one vectar was given.
       bool = .true.
    else
       allocate (vr(1_sz:n))
       allocate (p(1_sz:n))
       call fill_vr_and_p
       if (.not. lengths_are_equal ()) then
          bool = .false.
       else
          bool = check_elements ()
       end if
    end if

  contains

    subroutine fill_vr_and_p
      integer(sz) :: i
      type(cons_t) :: lst

      lst = vectars
      do i = 1_sz, n
         vr(i) = car (lst)
         p(i) = vectar_data_p (vr(i)%vec())
         lst = cdr (lst)
      end do
    end subroutine fill_vr_and_p

    function lengths_are_equal () result (bool)
      integer(sz) :: i
      integer(sz) :: len
      logical :: bool

      bool = .true.
      len = vr(1)%length()
      i = 2_sz
      do while (bool .and. i <= n)
         bool = (vr(i)%length() == len)
         i = i + 1
      end do
    end function lengths_are_equal

    recursive function check_elements () result (bool)
      !
      ! NOTE: One could check for shared storage here, and conclude
      ! that it is equal to itself, but SRFI-133 does not, because of
      ! how IEEE floating point behaves.
      !
      ! Specifically: a NaN is unequal to itself. Therefore a list of
      ! NaN should be regarded as unequal to itself.
      !
      integer(sz) :: i_vec
      integer(sz) :: i_elem1, i_elem2
      integer(sz) :: i_last1
      logical :: bool

      bool = .true.
      i_vec = 1_sz
      do while (bool .and. i_vec < n)
         i_elem2 = vr(i_vec + 1)%istart0()
         i_elem1 = vr(i_vec)%istart0()
         i_last1 = vr(i_vec)%iend0()
         do while (bool .and. i_elem1 <= i_last1)
            bool = equal (p(i_vec)%data%array(i_elem1)%element, &
                 &        p(i_vec + 1)%data%array(i_elem2)%element)
            i_elem2 = i_elem2 + 1
            i_elem1 = i_elem1 + 1
         end do
         i_vec = i_vec + 1
      end do
    end function check_elements

  end function apply_vectar_equal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module vectars
