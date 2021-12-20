! -*- F90 -*- 
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

  public :: pair_t           ! The type for garbage-collectible CONS-pairs.
  public :: nil_t            ! The type for a NIL-list.
  public :: nil              ! The canonical NIL-list (commonly written '() in Scheme).

  public :: is_pair          ! Is the object either a CONS-pair or a gcroot_t containing a CONS-pair?
  public :: is_not_pair      ! Is the object neither a CONS-pair nor a gcroot_t containing a CONS-pair?
  public :: pair_t_cast      ! Convert an object to a CONS-pair, if possible.
  public :: pair_t_eq        ! Are two pair_t the same pair_t? That is, do they share their storage?

  ! `is_nil' is equivalent to SRFI-1's `null?' procedure.
  public :: is_nil           ! Is the object either a NIL-list or a gcroot_t containing a NIL-list?
  public :: is_not_nil       ! Is the object neither a NIL-list nor a gcroot_t containing a NIL-list?

  ! `is_nil_list' is equivalent to SRFI-1's `null-list?' procedure.
  public :: is_nil_list

  public :: cons             ! The fundamental CONS-pair constructor.
  public :: uncons           ! The fundamental CONS-pair deconstructor (called `car+cdr' in SRFI-1).
  public :: car              ! Get just the CAR.
  public :: cdr              ! Get just the CDR.
  public :: set_car          ! Change the CAR.
  public :: set_cdr          ! Change the CDR.

  public :: operator(**)     ! For notation such as `1 ** 2.0 ** "3" ** nil'.

  ! Permutations of car and cdr, for returning elements of a tree.
  public :: caar
  public :: cdar
  public :: cadr
  public :: cddr
  public :: caaar
  public :: cdaar
  public :: cadar
  public :: cddar
  public :: caadr
  public :: cdadr
  public :: caddr
  public :: cdddr
  public :: caaaar
  public :: cdaaar
  public :: cadaar
  public :: cddaar
  public :: caadar
  public :: cdadar
  public :: caddar
  public :: cdddar
  public :: caaadr
  public :: cdaadr
  public :: cadadr
  public :: cddadr
  public :: caaddr
  public :: cdaddr
  public :: cadddr
  public :: cddddr

  ! Return one of the first ten elements of a list.
  public :: first
  public :: second
  public :: third
  public :: fourth
  public :: fifth
  public :: sixth
  public :: seventh
  public :: eighth
  public :: ninth
  public :: tenth

  ! SRFI-1 does not have these `next' procedures.
  public :: next_left        ! Replace a variable's value with its CAR (x = car (x)).
  public :: next_right       ! Replace a variable's value with its CDR (x = cdr (x)).

  ! SRFI-1 does not have `classify_list', although it does have
  ! procedures this module derives from it (`proper-list?',
  ! `dotted-list?', and `circular-list?).
  public :: classify_list    ! Classify a list as proper, dotted, or circular.
  public :: is_proper_list   ! A list that terminates in a NIL.
  public :: is_dotted_list   ! A list that terminates in a non-NIL.
  public :: is_circular_list ! A list that does not terminate.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type :: pair_data_t
     class(*), allocatable :: car
     class(*), allocatable :: cdr
  end type pair_data_t

  type, extends (collectible_t) :: pair_t
   contains
     procedure, pass :: get_branch => pair_t_get_branch
  end type pair_t

  type :: nil_t
  end type nil_t

  type(nil_t), parameter :: nil = nil_t ()

  interface operator(**)
     module procedure infix_right_cons_pair
     module procedure infix_right_cons_nil
  end interface operator(**)

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
    write (error_unit, '("module cons_pairs error: ", a)') msg
    error stop
  end subroutine error_abort_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pair_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(pair_t), intent(in) :: this
    integer(size_kind), intent(in) :: branch_number
    class(*), allocatable :: branch

    class(*), pointer :: data
    logical :: branch_number_out_of_range

    branch_number_out_of_range = .true.
    if (branch_number == 1) then
       data => this%heap_element%data
       select type (data)
       class is (pair_data_t)
          branch = data%car
          branch_number_out_of_range = .false.
       end select
    else if (branch_number == 2) then
       data => this%heap_element%data
       select type (data)
       class is (pair_data_t)
          branch = data%cdr
          branch_number_out_of_range = .false.
       end select
    end if
  end subroutine pair_t_get_branch

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function is_pair (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .false.
    select type (obj)
    class is (pair_t)
       bool = .true.
    class is (gcroot_t)
       select type (val => obj%val ())
       class is (pair_t)
          bool = .true.
       end select
    end select
  end function is_pair

  function is_not_pair (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .not. is_pair (obj)
  end function is_not_pair

  function pair_t_cast (obj) result (the_pair)
    class(*), intent(in) :: obj
    class(pair_t), allocatable :: the_pair

    select type (obj)
    class is (pair_t)
       the_pair = obj
    class is (gcroot_t)
       select type (val => obj%val ())
       class is (pair_t)
          the_pair = val
       class default
          call error_abort ("pair_t_cast of an incompatible object")
       end select
    class default
      call error_abort ("pair_t_cast of an incompatible object")
    end select
  end function pair_t_cast

  recursive function pair_t_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    logical :: bool

    select type (obj1)
    class is (pair_t)
       select type (obj2)
       class is (pair_t)
          bool = associated (obj1%heap_element, obj2%heap_element)
       class is (gcroot_t)
          bool = pair_t_eq (obj1, obj2%val ())
       class default
          call error_abort ("the second argument to pair_t_eq is illegal")
       end select
    class is (gcroot_t)
       select type (obj2)
       class is (pair_t)
          bool = pair_t_eq (obj1%val (), obj2)
       class is (gcroot_t)
          bool = pair_t_eq (obj1%val (), obj2%val ())
       class default
          call error_abort ("the second argument to pair_t_eq is illegal")
       end select
    class default
       call error_abort ("the first argument to pair_t_eq is illegal")
    end select
  end function pair_t_eq

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function is_nil (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .false.
    select type (obj)
    class is (nil_t)
       bool = .true.
    class is (gcroot_t)
       select type (val => obj%val ())
       class is (nil_t)
          bool = .true.
       end select
    end select
  end function is_nil

  function is_not_nil (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .true.
    select type (obj)
    class is (nil_t)
       bool = .false.
    class is (gcroot_t)
       select type (val => obj%val ())
       class is (nil_t)
          bool = .false.
       end select
    end select
  end function is_not_nil

  recursive function is_nil_list (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (nil_t)
       bool = .true.
    class is (pair_t)
       bool = .false.
    class is (gcroot_t)
       bool = is_nil_list (obj%val ())
    class default
       call error_abort ("is_nil_list of an object that is neither pair nor nil")
    end select
  end function is_nil_list

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function cons (car_value, cdr_value) result (the_pair)
    class(*), intent(in) :: car_value
    class(*), intent(in) :: cdr_value
    type(pair_t), allocatable :: the_pair

    type(heap_element_t), pointer :: new_element
    type(pair_data_t), pointer :: data

    select type (car_value)
    class is (gcroot_t)
       select type (cdr_value)
       class is (gcroot_t)
          the_pair = cons (car_value%val (), cdr_value%val ())
       class default
          the_pair = cons (car_value%val (), cdr_value)
       end select
    class default
       select type (cdr_value)
       class is (gcroot_t)
          the_pair = cons (car_value, cdr_value%val ())
       class default
          allocate (data)
          data%car = car_value
          data%cdr = cdr_value
          allocate (new_element)
          new_element%data => data
          call heap_insert (new_element)
          allocate (the_pair)
          the_pair%heap_element => new_element
       end select
    end select
  end function cons

  recursive function infix_right_cons_pair (car_value, cdr_value) result (the_pair)
    class(*), intent(in) :: car_value
    class(pair_t), intent(in) :: cdr_value
    type(pair_t), allocatable :: the_pair

    type(heap_element_t), pointer :: new_element
    type(pair_data_t), pointer :: data

    select type (car_value)
    class is (gcroot_t)
       the_pair = infix_right_cons_pair (car_value%val (), cdr_value)
    class default
       allocate (data)
       data%car = car_value
       data%cdr = cdr_value
       allocate (new_element)
       new_element%data => data
       call heap_insert (new_element)
       allocate (the_pair)
       the_pair%heap_element => new_element
    end select
  end function infix_right_cons_pair

  recursive function infix_right_cons_nil (car_value, cdr_value) result (the_pair)
    class(*), intent(in) :: car_value
    class(nil_t), intent(in) :: cdr_value
    type(pair_t), allocatable :: the_pair

    type(heap_element_t), pointer :: new_element
    type(pair_data_t), pointer :: data

    select type (car_value)
    class is (gcroot_t)
       the_pair = infix_right_cons_nil (car_value%val (), cdr_value)
    class default
       allocate (data)
       data%car = car_value
       data%cdr = cdr_value
       allocate (new_element)
       new_element%data => data
       call heap_insert (new_element)
       allocate (the_pair)
       the_pair%heap_element => new_element
    end select
  end function infix_right_cons_nil

  recursive subroutine uncons (the_pair, car_value, cdr_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable, intent(inout) :: car_value
    class(*), allocatable, intent(inout) :: cdr_value

    class(*), pointer :: data
    class(*), allocatable :: car_val
    class(*), allocatable :: cdr_val

    select type (the_pair)
    class is (pair_t)
       write (*,*) "uncons of a pair_t"
       data => the_pair%heap_element%data
       select type (data)
       class is (pair_data_t)
          car_val = data%car
          cdr_val = data%cdr
       class default
          call error_abort ("a strange error, possibly use of an object already garbage-collected")
       end select
       car_value = car_val
       cdr_value = cdr_val
    class is (gcroot_t)
       write (*,*) "uncons of a gcroot_t"
       call uncons (the_pair%val (), car_value, cdr_value)
    class default
       call error_abort ("uncons of a non-pair")
    end select
  end subroutine uncons

  recursive function car (the_pair) result (car_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable :: car_value

    class(*), pointer :: data

    select type (the_pair)
    class is (pair_t)
       write (*,*) "car of a pair_t"
       data => the_pair%heap_element%data
       select type (data)
       class is (pair_data_t)
          car_value = data%car
       class default
          call error_abort ("a strange error, possibly use of an object already garbage-collected")
       end select
    class is (gcroot_t)
       write (*,*) "car of a gcroot_t"
       car_value = car (the_pair%val ())
    class default
       call error_abort ("car of a non-pair")
    end select
  end function car

  recursive function cdr (the_pair) result (cdr_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable :: cdr_value

    class(*), pointer :: data

    select type (the_pair)
    class is (pair_t)
       write (*,*) "cdr of a pair_t"
       data => the_pair%heap_element%data
       select type (data)
       class is (pair_data_t)
          cdr_value = data%cdr
       class default
          call error_abort ("a strange error, possibly use of an object already garbage-collected")
       end select
    class is (gcroot_t)
       write (*,*) "cdr of a gcroot_t"
       cdr_value = cdr (the_pair%val ())
    class default
       call error_abort ("cdr of a non-pair")
    end select
  end function cdr

  recursive subroutine set_car (the_pair, car_value)
    class(*), intent(inout) :: the_pair
    class(*), intent(in) :: car_value

    type(pair_data_t), pointer :: data

    select type (the_pair)
    class is (pair_t)
       deallocate (the_pair%heap_element%data)
       allocate (data)
       data%car = car_value
       the_pair%heap_element%data => data
    class is (gcroot_t)
       block
         class(collectible_t), pointer :: ptr
         ptr => the_pair%ptr ()
         call set_car (ptr, car_value)
       end block
    class default
       call error_abort ("set_car of a non-pair")
    end select
  end subroutine set_car

  recursive subroutine set_cdr (the_pair, cdr_value)
    class(*), intent(inout) :: the_pair
    class(*), intent(in) :: cdr_value

    type(pair_data_t), pointer :: data

    select type (the_pair)
    class is (pair_t)
       deallocate (the_pair%heap_element%data)
       allocate (data)
       data%cdr = cdr_value
       the_pair%heap_element%data => data
    class is (gcroot_t)
       block
         class(collectible_t), pointer :: ptr
         ptr => the_pair%ptr ()
         call set_cdr (ptr, cdr_value)
       end block
    class default
       call error_abort ("set_cdr of a non-pair")
    end select
  end subroutine set_cdr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function caar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_left (element)
  end function caar

  function cdar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_right (element)
  end function cdar

  function cadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_left (element)
  end function cadr

  function cddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_right (element)
  end function cddr

  function caaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_left (element)
    call next_left (element)
  end function caaar

  function cdaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_left (element)
    call next_right (element)
  end function cdaar

  function cadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_right (element)
    call next_left (element)
  end function cadar

  function cddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_right (element)
    call next_right (element)
  end function cddar

  function caadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_left (element)
    call next_left (element)
  end function caadr

  function cdadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_left (element)
    call next_right (element)
  end function cdadr

  function caddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function caddr

  function cdddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_right (element)
    call next_right (element)
  end function cdddr

  function caaaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_left (element)
    call next_left (element)
    call next_left (element)
  end function caaaar

  function cdaaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_left (element)
    call next_left (element)
    call next_right (element)
  end function cdaaar

  function cadaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_left (element)
    call next_right (element)
    call next_left (element)
  end function cadaar

  function cddaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_left (element)
    call next_right (element)
    call next_right (element)
  end function cddaar

  function caadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_right (element)
    call next_left (element)
    call next_left (element)
  end function caadar

  function cdadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_right (element)
    call next_left (element)
    call next_right (element)
  end function cdadar

  function caddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function caddar

  function cdddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_left (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
  end function cdddar

  function caaadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_left (element)
    call next_left (element)
    call next_left (element)
  end function caaadr

  function cdaadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_left (element)
    call next_left (element)
    call next_right (element)
  end function cdaadr

  function cadadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_left (element)
    call next_right (element)
    call next_left (element)
  end function cadadr

  function cddadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_left (element)
    call next_right (element)
    call next_right (element)
  end function cddadr

  function caaddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_right (element)
    call next_left (element)
    call next_left (element)
  end function caaddr

  function cdaddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_right (element)
    call next_left (element)
    call next_right (element)
  end function cdaddr

  function cadddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function cadddr

  function cddddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
  end function cddddr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function first (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_left (element)
  end function first

  function second (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_left (element)
  end function second

  function third (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function third

  function fourth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function fourth

  function fifth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function fifth

  function sixth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function sixth

  function seventh (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function seventh

  function eighth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function eighth

  function ninth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function ninth

  function tenth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_right (element)
    call next_left (element)
  end function tenth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine next_left (obj)
    class(*), allocatable, intent(inout) :: obj
    obj = car (obj)
  end subroutine next_left

  subroutine next_right (obj)
    class(*), allocatable, intent(inout) :: obj
    obj = cdr (obj)
  end subroutine next_right

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine classify_list (obj, is_dotted, is_circular)
    !
    ! An object that is not a pair_t or a nil_t is considered dotted.
    !
    ! Dotted and circular are mutually exclusive.
    !
    ! If an object is neither dotted nor circular, then it is a proper
    ! list.
    !
    class(*), intent(in) :: obj
    logical, intent(out) :: is_dotted
    logical, intent(out) :: is_circular

    ! Detect circularity by having a `lead' reference move through the
    ! list at a higher rate than a `lag' reference. In a circular
    ! list, eventually `lead' will catch up with `lag'.
    class(*), allocatable :: lead
    class(*), allocatable :: lag

    logical :: is_dot
    logical :: is_circ
    logical :: done

    lead = obj

    lag = lead
    is_dot = .false.
    is_circ = .false.
    done = .false.
    do while (.not. done)
       if (is_not_pair (lead)) then
          is_dot = is_not_nil (lead)
          done = .true.
       else
          call next_right (lead)
          if (is_not_pair (lead)) then
             is_dot = is_not_nil (lead)
             done = .true.
          else
             call next_right (lead)
             call next_right (lag)
             if (is_pair (lead)) then
                if (pair_t_eq (lead, lag)) then
                   is_circ = .true.
                   done = .true.
                end if
             end if
          end if
       end if
    end do

    is_dotted = is_dot
    is_circular = is_circ
  end subroutine classify_list

  function is_proper_list (obj) result (is_proper)
    class(*), intent(in) :: obj
    logical :: is_proper

    logical :: is_dot
    logical :: is_circ

    call classify_list (obj, is_dot, is_circ)
    is_proper = (.not. is_dot) .and. (.not. is_circ)
  end function is_proper_list

  function is_dotted_list (obj) result (is_dotted)
    !
    ! Note that is_dotted_list(4), is_dotted_list("abc"), etc., return
    ! .true.
    !
    ! One consequence is that
    !
    !    .not. is_dotted_list (x)
    !
    ! is equivalent to
    !
    !    is_proper_list (x) .or. is_circular_list (x)
    !
    class(*), intent(in) :: obj
    logical :: is_dotted

    logical :: is_dot
    logical :: is_circ

    call classify_list (obj, is_dot, is_circ)
    is_dotted = is_dot
  end function is_dotted_list

  function is_circular_list (obj) result (is_circular)
    class(*), intent(in) :: obj
    logical :: is_circular

    logical :: is_dot
    logical :: is_circ

    call classify_list (obj, is_dot, is_circ)
    is_circular = is_circ
  end function is_circular_list

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module cons_pairs
