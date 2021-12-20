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

  ! Make and unmake lists of particular lengths.
  public :: list1
  public :: list2
  public :: list3
  public :: list4
  public :: list5
  public :: list6
  public :: list7
  public :: list8
  public :: list9
  public :: list10
  public :: list11
  public :: list12
  public :: list13
  public :: list14
  public :: list15
  public :: list16
  public :: list17
  public :: list18
  public :: list19
  public :: list20
  public :: unlist1
  public :: unlist2
  public :: unlist3
  public :: unlist4
  public :: unlist5
  public :: unlist6
  public :: unlist7
  public :: unlist8
  public :: unlist9
  public :: unlist10
  public :: unlist11
  public :: unlist12
  public :: unlist13
  public :: unlist14
  public :: unlist15
  public :: unlist16
  public :: unlist17
  public :: unlist18
  public :: unlist19
  public :: unlist20
  public :: unlist1_with_tail
  public :: unlist2_with_tail
  public :: unlist3_with_tail
  public :: unlist4_with_tail
  public :: unlist5_with_tail
  public :: unlist6_with_tail
  public :: unlist7_with_tail
  public :: unlist8_with_tail
  public :: unlist9_with_tail
  public :: unlist10_with_tail
  public :: unlist11_with_tail
  public :: unlist12_with_tail
  public :: unlist13_with_tail
  public :: unlist14_with_tail
  public :: unlist15_with_tail
  public :: unlist16_with_tail
  public :: unlist17_with_tail
  public :: unlist18_with_tail
  public :: unlist19_with_tail
  public :: unlist20_with_tail

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

  function list1 (obj1) result (lst)
    class(*), intent(in) :: obj1
    type(pair_t) :: lst

    lst = obj1 ** nil
  end function list1

  function list2 (obj1, obj2) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    type(pair_t) :: lst

    lst = obj2 ** nil
    lst = obj1 ** lst
  end function list2

  function list3 (obj1, obj2, obj3) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    type(pair_t) :: lst

    lst = obj3 ** nil
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list3

  function list4 (obj1, obj2, obj3, obj4) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    type(pair_t) :: lst

    lst = obj4 ** nil
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list4

  function list5 (obj1, obj2, obj3, obj4, obj5) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    type(pair_t) :: lst

    lst = obj5 ** nil
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list5

  function list6 (obj1, obj2, obj3, obj4, obj5, obj6) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    type(pair_t) :: lst

    lst = obj6 ** nil
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list6

  function list7 (obj1, obj2, obj3, obj4, obj5, obj6, obj7) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    type(pair_t) :: lst

    lst = obj7 ** nil
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list7

  function list8 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    type(pair_t) :: lst

    lst = obj8 ** nil
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list8

  function list9 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    type(pair_t) :: lst

    lst = obj9 ** nil
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list9

  function list10 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10) result (lst)
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
    type(pair_t) :: lst

    lst = obj10 ** nil
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list10

  function list11 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11) result (lst)
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
    type(pair_t) :: lst

    lst = obj11 ** nil
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list11

  function list12 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12) result (lst)
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
    type(pair_t) :: lst

    lst = obj12 ** nil
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list12

  function list13 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13) result (lst)
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
    type(pair_t) :: lst

    lst = obj13 ** nil
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list13

  function list14 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14) result (lst)
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
    type(pair_t) :: lst

    lst = obj14 ** nil
    lst = obj13 ** lst
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list14

  function list15 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15) result (lst)
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
    type(pair_t) :: lst

    lst = obj15 ** nil
    lst = obj14 ** lst
    lst = obj13 ** lst
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list15

  function list16 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16) result (lst)
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
    type(pair_t) :: lst

    lst = obj16 ** nil
    lst = obj15 ** lst
    lst = obj14 ** lst
    lst = obj13 ** lst
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list16

  function list17 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17) result (lst)
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
    type(pair_t) :: lst

    lst = obj17 ** nil
    lst = obj16 ** lst
    lst = obj15 ** lst
    lst = obj14 ** lst
    lst = obj13 ** lst
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list17

  function list18 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18) result (lst)
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
    type(pair_t) :: lst

    lst = obj18 ** nil
    lst = obj17 ** lst
    lst = obj16 ** lst
    lst = obj15 ** lst
    lst = obj14 ** lst
    lst = obj13 ** lst
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list18

  function list19 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19) result (lst)
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
    type(pair_t) :: lst

    lst = obj19 ** nil
    lst = obj18 ** lst
    lst = obj17 ** lst
    lst = obj16 ** lst
    lst = obj15 ** lst
    lst = obj14 ** lst
    lst = obj13 ** lst
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list19

  function list20 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19, obj20) result (lst)
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
    type(pair_t) :: lst

    lst = obj20 ** nil
    lst = obj19 ** lst
    lst = obj18 ** lst
    lst = obj17 ** lst
    lst = obj16 ** lst
    lst = obj15 ** lst
    lst = obj14 ** lst
    lst = obj13 ** lst
    lst = obj12 ** lst
    lst = obj11 ** lst
    lst = obj10 ** lst
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list20

  subroutine unlist1 (lst, obj1)
    !
    ! This subroutine `unlists' the 1 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist1 of a list that is too long")
    end if
  end subroutine unlist1

  subroutine unlist2 (lst, obj1, obj2)
    !
    ! This subroutine `unlists' the 2 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist2 of a list that is too long")
    end if
  end subroutine unlist2

  subroutine unlist3 (lst, obj1, obj2, obj3)
    !
    ! This subroutine `unlists' the 3 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist3 of a list that is too long")
    end if
  end subroutine unlist3

  subroutine unlist4 (lst, obj1, obj2, obj3, obj4)
    !
    ! This subroutine `unlists' the 4 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist4 of a list that is too long")
    end if
  end subroutine unlist4

  subroutine unlist5 (lst, obj1, obj2, obj3, obj4, obj5)
    !
    ! This subroutine `unlists' the 5 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist5 of a list that is too long")
    end if
  end subroutine unlist5

  subroutine unlist6 (lst, obj1, obj2, obj3, obj4, obj5, obj6)
    !
    ! This subroutine `unlists' the 6 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist6 of a list that is too long")
    end if
  end subroutine unlist6

  subroutine unlist7 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7)
    !
    ! This subroutine `unlists' the 7 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist7 of a list that is too long")
    end if
  end subroutine unlist7

  subroutine unlist8 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8)
    !
    ! This subroutine `unlists' the 8 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist8 of a list that is too long")
    end if
  end subroutine unlist8

  subroutine unlist9 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9)
    !
    ! This subroutine `unlists' the 9 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist9 of a list that is too long")
    end if
  end subroutine unlist9

  subroutine unlist10 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10)
    !
    ! This subroutine `unlists' the 10 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist10 of a list that is too long")
    end if
  end subroutine unlist10

  subroutine unlist11 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11)
    !
    ! This subroutine `unlists' the 11 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist11 of a list that is too long")
    end if
  end subroutine unlist11

  subroutine unlist12 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12)
    !
    ! This subroutine `unlists' the 12 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist12 of a list that is too long")
    end if
  end subroutine unlist12

  subroutine unlist13 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13)
    !
    ! This subroutine `unlists' the 13 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist13 of a list that is too long")
    end if
  end subroutine unlist13

  subroutine unlist14 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14)
    !
    ! This subroutine `unlists' the 14 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    call uncons (tail, obj14, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist14 of a list that is too long")
    end if
  end subroutine unlist14

  subroutine unlist15 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15)
    !
    ! This subroutine `unlists' the 15 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    call uncons (tail, obj14, tail)
    call uncons (tail, obj15, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist15 of a list that is too long")
    end if
  end subroutine unlist15

  subroutine unlist16 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16)
    !
    ! This subroutine `unlists' the 16 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    call uncons (tail, obj14, tail)
    call uncons (tail, obj15, tail)
    call uncons (tail, obj16, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist16 of a list that is too long")
    end if
  end subroutine unlist16

  subroutine unlist17 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17)
    !
    ! This subroutine `unlists' the 17 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    call uncons (tail, obj14, tail)
    call uncons (tail, obj15, tail)
    call uncons (tail, obj16, tail)
    call uncons (tail, obj17, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist17 of a list that is too long")
    end if
  end subroutine unlist17

  subroutine unlist18 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18)
    !
    ! This subroutine `unlists' the 18 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17
    class(*), allocatable, intent(inout) :: obj18

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    call uncons (tail, obj14, tail)
    call uncons (tail, obj15, tail)
    call uncons (tail, obj16, tail)
    call uncons (tail, obj17, tail)
    call uncons (tail, obj18, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist18 of a list that is too long")
    end if
  end subroutine unlist18

  subroutine unlist19 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19)
    !
    ! This subroutine `unlists' the 19 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17
    class(*), allocatable, intent(inout) :: obj18
    class(*), allocatable, intent(inout) :: obj19

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    call uncons (tail, obj14, tail)
    call uncons (tail, obj15, tail)
    call uncons (tail, obj16, tail)
    call uncons (tail, obj17, tail)
    call uncons (tail, obj18, tail)
    call uncons (tail, obj19, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist19 of a list that is too long")
    end if
  end subroutine unlist19

  subroutine unlist20 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19, obj20)
    !
    ! This subroutine `unlists' the 20 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17
    class(*), allocatable, intent(inout) :: obj18
    class(*), allocatable, intent(inout) :: obj19
    class(*), allocatable, intent(inout) :: obj20

    class(*), allocatable :: tail

    tail = pair_t_cast (lst)
    call uncons (tail, obj1, tail)
    call uncons (tail, obj2, tail)
    call uncons (tail, obj3, tail)
    call uncons (tail, obj4, tail)
    call uncons (tail, obj5, tail)
    call uncons (tail, obj6, tail)
    call uncons (tail, obj7, tail)
    call uncons (tail, obj8, tail)
    call uncons (tail, obj9, tail)
    call uncons (tail, obj10, tail)
    call uncons (tail, obj11, tail)
    call uncons (tail, obj12, tail)
    call uncons (tail, obj13, tail)
    call uncons (tail, obj14, tail)
    call uncons (tail, obj15, tail)
    call uncons (tail, obj16, tail)
    call uncons (tail, obj17, tail)
    call uncons (tail, obj18, tail)
    call uncons (tail, obj19, tail)
    call uncons (tail, obj20, tail)
    if (is_pair (tail)) then
       call error_abort ("unlist20 of a list that is too long")
    end if
  end subroutine unlist20

  subroutine unlist1_with_tail (lst, obj1, tail)
    !
    ! This subroutine `unlists' the leading 1 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    tail = tl
  end subroutine unlist1_with_tail

  subroutine unlist2_with_tail (lst, obj1, obj2, tail)
    !
    ! This subroutine `unlists' the leading 2 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    tail = tl
  end subroutine unlist2_with_tail

  subroutine unlist3_with_tail (lst, obj1, obj2, obj3, tail)
    !
    ! This subroutine `unlists' the leading 3 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    tail = tl
  end subroutine unlist3_with_tail

  subroutine unlist4_with_tail (lst, obj1, obj2, obj3, obj4, tail)
    !
    ! This subroutine `unlists' the leading 4 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    tail = tl
  end subroutine unlist4_with_tail

  subroutine unlist5_with_tail (lst, obj1, obj2, obj3, obj4, obj5, tail)
    !
    ! This subroutine `unlists' the leading 5 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    tail = tl
  end subroutine unlist5_with_tail

  subroutine unlist6_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, tail)
    !
    ! This subroutine `unlists' the leading 6 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    tail = tl
  end subroutine unlist6_with_tail

  subroutine unlist7_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, tail)
    !
    ! This subroutine `unlists' the leading 7 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    tail = tl
  end subroutine unlist7_with_tail

  subroutine unlist8_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, tail)
    !
    ! This subroutine `unlists' the leading 8 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    tail = tl
  end subroutine unlist8_with_tail

  subroutine unlist9_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, tail)
    !
    ! This subroutine `unlists' the leading 9 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    tail = tl
  end subroutine unlist9_with_tail

  subroutine unlist10_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, tail)
    !
    ! This subroutine `unlists' the leading 10 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    tail = tl
  end subroutine unlist10_with_tail

  subroutine unlist11_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, tail)
    !
    ! This subroutine `unlists' the leading 11 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    tail = tl
  end subroutine unlist11_with_tail

  subroutine unlist12_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, tail)
    !
    ! This subroutine `unlists' the leading 12 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    tail = tl
  end subroutine unlist12_with_tail

  subroutine unlist13_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, tail)
    !
    ! This subroutine `unlists' the leading 13 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    tail = tl
  end subroutine unlist13_with_tail

  subroutine unlist14_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, tail)
    !
    ! This subroutine `unlists' the leading 14 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    call uncons (tl, obj14, tl)
    tail = tl
  end subroutine unlist14_with_tail

  subroutine unlist15_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, tail)
    !
    ! This subroutine `unlists' the leading 15 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    call uncons (tl, obj14, tl)
    call uncons (tl, obj15, tl)
    tail = tl
  end subroutine unlist15_with_tail

  subroutine unlist16_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, tail)
    !
    ! This subroutine `unlists' the leading 16 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    call uncons (tl, obj14, tl)
    call uncons (tl, obj15, tl)
    call uncons (tl, obj16, tl)
    tail = tl
  end subroutine unlist16_with_tail

  subroutine unlist17_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, tail)
    !
    ! This subroutine `unlists' the leading 17 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    call uncons (tl, obj14, tl)
    call uncons (tl, obj15, tl)
    call uncons (tl, obj16, tl)
    call uncons (tl, obj17, tl)
    tail = tl
  end subroutine unlist17_with_tail

  subroutine unlist18_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, tail)
    !
    ! This subroutine `unlists' the leading 18 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17
    class(*), allocatable, intent(inout) :: obj18
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    call uncons (tl, obj14, tl)
    call uncons (tl, obj15, tl)
    call uncons (tl, obj16, tl)
    call uncons (tl, obj17, tl)
    call uncons (tl, obj18, tl)
    tail = tl
  end subroutine unlist18_with_tail

  subroutine unlist19_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19, tail)
    !
    ! This subroutine `unlists' the leading 19 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17
    class(*), allocatable, intent(inout) :: obj18
    class(*), allocatable, intent(inout) :: obj19
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    call uncons (tl, obj14, tl)
    call uncons (tl, obj15, tl)
    call uncons (tl, obj16, tl)
    call uncons (tl, obj17, tl)
    call uncons (tl, obj18, tl)
    call uncons (tl, obj19, tl)
    tail = tl
  end subroutine unlist19_with_tail

  subroutine unlist20_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19, obj20, tail)
    !
    ! This subroutine `unlists' the leading 20 elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1
    class(*), allocatable, intent(inout) :: obj2
    class(*), allocatable, intent(inout) :: obj3
    class(*), allocatable, intent(inout) :: obj4
    class(*), allocatable, intent(inout) :: obj5
    class(*), allocatable, intent(inout) :: obj6
    class(*), allocatable, intent(inout) :: obj7
    class(*), allocatable, intent(inout) :: obj8
    class(*), allocatable, intent(inout) :: obj9
    class(*), allocatable, intent(inout) :: obj10
    class(*), allocatable, intent(inout) :: obj11
    class(*), allocatable, intent(inout) :: obj12
    class(*), allocatable, intent(inout) :: obj13
    class(*), allocatable, intent(inout) :: obj14
    class(*), allocatable, intent(inout) :: obj15
    class(*), allocatable, intent(inout) :: obj16
    class(*), allocatable, intent(inout) :: obj17
    class(*), allocatable, intent(inout) :: obj18
    class(*), allocatable, intent(inout) :: obj19
    class(*), allocatable, intent(inout) :: obj20
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = pair_t_cast (lst)
    call uncons (tl, obj1, tl)
    call uncons (tl, obj2, tl)
    call uncons (tl, obj3, tl)
    call uncons (tl, obj4, tl)
    call uncons (tl, obj5, tl)
    call uncons (tl, obj6, tl)
    call uncons (tl, obj7, tl)
    call uncons (tl, obj8, tl)
    call uncons (tl, obj9, tl)
    call uncons (tl, obj10, tl)
    call uncons (tl, obj11, tl)
    call uncons (tl, obj12, tl)
    call uncons (tl, obj13, tl)
    call uncons (tl, obj14, tl)
    call uncons (tl, obj15, tl)
    call uncons (tl, obj16, tl)
    call uncons (tl, obj17, tl)
    call uncons (tl, obj18, tl)
    call uncons (tl, obj19, tl)
    call uncons (tl, obj20, tl)
    tail = tl
  end subroutine unlist20_with_tail

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
