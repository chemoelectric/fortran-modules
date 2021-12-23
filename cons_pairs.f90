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

  public :: cons_t           ! The type for NIL-lists and garbage-collectible CONS-pairs.
  public :: nil              ! The canonical NIL-list (commonly written '() in Scheme).

  public :: is_pair          ! Is the object either a CONS-pair or a gcroot_t containing a CONS-pair?
  public :: is_not_pair      ! Is the object neither a CONS-pair nor a gcroot_t containing a CONS-pair?

  ! `is_nil' is equivalent to SRFI-1's `null?' procedure.
  public :: is_nil           ! Is the object either a NIL-list or a gcroot_t containing a NIL-list?
  public :: is_not_nil       ! Is the object neither a NIL-list nor a gcroot_t containing a NIL-list?

  ! `is_nil_list' is equivalent to SRFI-1's `null-list?' procedure.
  public :: is_nil_list

  public :: cons_t_cast      ! Convert an object to a CONS-pair, if possible.
  public :: operator(.tocons.) ! A synonym for cons_t_cast.
  public :: cons_t_eq        ! Are two cons_t either both NIL or pairs that share their storage?

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

  public :: list_ref0        ! Return any one of the 0th, 1st, 2nd, etc., elements.
  public :: list_ref1        ! Return any one of the 1st, 2nd, 3rd, etc., elements.
  public :: list_refn        ! Return any one of the nth, (n+1)th, (n+2)th, etc., elements.

  ! Make and unmake a list of particular length, or of a certain
  ! length and also a tail. (SRFI-1 has `list' and `cons*' have
  ! related functionality.)
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
  public :: list1_with_tail
  public :: list2_with_tail
  public :: list3_with_tail
  public :: list4_with_tail
  public :: list5_with_tail
  public :: list6_with_tail
  public :: list7_with_tail
  public :: list8_with_tail
  public :: list9_with_tail
  public :: list10_with_tail
  public :: list11_with_tail
  public :: list12_with_tail
  public :: list13_with_tail
  public :: list14_with_tail
  public :: list15_with_tail
  public :: list16_with_tail
  public :: list17_with_tail
  public :: list18_with_tail
  public :: list19_with_tail
  public :: list20_with_tail
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

  public :: take             ! Return a freshly allocated copy of the first n elements of a list.
  public :: drop             ! Return a common tail containing all but the first n elements of a list.

  public :: last_pair        ! Return the last pair of a list.
  public :: last             ! Return the last CAR of a list.

  public :: make_list        ! Return a list of repeated values.

  ! Return a list of values determined by a procedure.
  public :: list_tabulate_init_proc_t ! The type for the initialization procedure.
  public :: list_tabulate0   ! Indices start at 0.
  public :: list_tabulate1   ! Indices start at 1.
  public :: list_tabulaten   ! Indices start at n.

  abstract interface
     recursive subroutine list_tabulate_init_proc_t (i, x)
       import size_kind
       integer(size_kind), intent(in) :: i
       class(*), allocatable, intent(out) :: x
     end subroutine list_tabulate_init_proc_t
  end interface

  ! iota: return a list containing a sequence of equally spaced integers.
  public :: iota             ! `iota' = the generic function.
  public :: iota_of_length
  public :: iota_of_length_start
  public :: iota_of_length_start_step

  public :: list_copy        ! Make a copy of a list.
  public :: reverse          ! Make a copy of a list, but reversed.
  public :: reversex         ! Like reverse, but allowed to destroy its inputs.
  public :: circular_list    ! Make a copy of a list, but with the tail connected to the head.
  public :: circular_listx   ! Like circular_list, but allowed to destroy its inputs. (SRFI-1 has no close equivalent.)

  public :: lists_are_equal  ! Test whether two lists are `equal'. (Equivalent to SRFI-1's `list='.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Types for predicates.
  public :: list_predicate1_t ! A predicate taking one argument.
  public :: list_predicate2_t ! A predicate taking two arguments.

  abstract interface
     recursive function list_predicate1_t (x) result (bool)
       !
       ! For passing one-argument predicates to procedures.
       !
       class(*), intent(in) :: x
       logical :: bool
     end function list_predicate1_t
  end interface

  abstract interface
     recursive function list_predicate2_t (x, y) result (bool)
       !
       ! For passing two-argument predicates to procedures.
       !
       class(*), intent(in) :: x, y
       logical :: bool
     end function list_predicate2_t
  end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type :: pair_data_t
     class(*), allocatable :: car
     class(*), allocatable :: cdr
  end type pair_data_t

  ! A cons_t is NIL if its heap_element pointer is not associated.
  type, extends (collectible_t) :: cons_t
   contains
     procedure, pass :: get_branch => cons_t_get_branch
  end type cons_t

  type(cons_t), parameter :: nil = cons_t ()

  interface operator(**)
     module procedure infix_right_cons
  end interface operator(**)

  interface operator(.tocons.)
     module procedure cons_t_cast
  end interface operator(.tocons.)

  interface iota
     module procedure iota_of_length
     module procedure iota_of_length_start
     module procedure iota_of_length_start_step
  end interface iota

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
    write (error_unit, '("module cons_pairs error: ", a)') msg
    error stop
  end subroutine error_abort_1

  subroutine strange_error
    call error_abort ("a strange error, possibly use of an object already garbage-collected")
  end subroutine strange_error

  function copy_first_pair (lst) result (lst_copy)
    class(*), intent(in) :: lst
    type(cons_t) :: lst_copy

    class(*), allocatable :: head, tail

    call uncons (lst, head, tail)
    lst_copy = cons (head, tail)
  end function copy_first_pair

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cons_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    class(cons_t), intent(in) :: this
    integer(sz), intent(in) :: branch_number
    class(*), allocatable :: branch

    class(*), pointer :: data
    logical :: branch_number_out_of_range

    ! A NIL-list has zero branches. A pair has two branches.

    branch_number_out_of_range = .true.
    if (associated (this%heap_element)) then
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
    end if
  end subroutine cons_t_get_branch

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function is_pair (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (cons_t)
       bool = associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
          bool = associated (val%heap_element)
       class default
          bool = .false.
       end select
    class default
       bool = .false.
    end select
  end function is_pair

  function is_not_pair (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .not. is_pair (obj)
  end function is_not_pair

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function is_nil (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (cons_t)
       bool = .not. associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
         bool = .not. associated (val%heap_element)
      class default
         bool = .false.
      end select
    class default
       bool = .false.
    end select
  end function is_nil

  function is_not_nil (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    bool = .not. is_nil (obj)
  end function is_not_nil

  recursive function is_nil_list (obj) result (bool)
    class(*), intent(in) :: obj
    logical :: bool

    select type (obj)
    class is (cons_t)
       bool = .not. associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
         bool = .not. associated (val%heap_element)
      class default
         call error_abort ("is_nil_list of a gcroot_t whose value is not a cons_t")
      end select
    class default
       call error_abort ("is_nil_list of an object that is not a cons_t")
    end select
  end function is_nil_list

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function cons_t_cast (obj) result (lst)
    class(*), intent(in) :: obj
    type(cons_t) :: lst

    select type (obj)
    class is (cons_t)
       lst = obj
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
          lst = val
       class default
          call error_abort ("cons_t_cast of an incompatible gcroot_t object")
       end select
    class default
      call error_abort ("cons_t_cast of an incompatible object")
    end select
  end function cons_t_cast

  recursive function cons_t_eq (obj1, obj2) result (bool)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    logical :: bool

    type(cons_t) :: o1, o2

    o1 = .tocons. obj1
    o2 = .tocons. obj2

    if (associated (o1%heap_element)) then
       if (associated (o2%heap_element)) then
          bool = associated (o1%heap_element, o2%heap_element)
       else
          bool = .false.
       end if
    else
       bool = .not. associated (o2%heap_element)
    end if
  end function cons_t_eq

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function cons (car_value, cdr_value) result (the_pair)
    class(*), intent(in) :: car_value
    class(*), intent(in) :: cdr_value
    type(cons_t) :: the_pair

    type(heap_element_t), pointer :: new_element
    type(pair_data_t), pointer :: data

    allocate (data)
    data%car = .autoval. car_value
    data%cdr = .autoval. cdr_value
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    the_pair%heap_element => new_element
  end function cons

  recursive function infix_right_cons (car_value, cdr_value) result (the_pair)
    class(*), intent(in) :: car_value
    class(cons_t), intent(in) :: cdr_value
    type(cons_t) :: the_pair

    type(heap_element_t), pointer :: new_element
    type(pair_data_t), pointer :: data

    allocate (data)
    data%car = .autoval. car_value
    data%cdr = cdr_value
    allocate (new_element)
    new_element%data => data
    call heap_insert (new_element)
    the_pair%heap_element => new_element
  end function infix_right_cons

  recursive subroutine uncons (the_pair, car_value, cdr_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable, intent(inout) :: car_value
    class(*), allocatable, intent(inout) :: cdr_value

    class(*), allocatable :: car_val
    class(*), allocatable :: cdr_val

    select type (pair => .autoval. the_pair)
    class is (cons_t)
       if (associated (pair%heap_element)) then
          select type (data => pair%heap_element%data)
          class is (pair_data_t)
             car_val = data%car
             cdr_val = data%cdr
          class default
             call strange_error
          end select
          car_value = car_val
          cdr_value = cdr_val
       else
          call error_abort ("uncons of a nil list")
       end if
    class default
       call error_abort ("uncons of an object that is not a cons_t")
    end select
  end subroutine uncons

  recursive function car (the_pair) result (car_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable :: car_value

    select type (pair => .autoval. the_pair)
    class is (cons_t)
       if (associated (pair%heap_element)) then
          select type (data => pair%heap_element%data)
          class is (pair_data_t)
             car_value = data%car
          class default
             call strange_error
          end select
       else
          call error_abort ("car of a nil list")
       end if
    class default
       call error_abort ("car of an object that is not a cons_t")
    end select
  end function car

  recursive function cdr (the_pair) result (cdr_value)
    class(*), intent(in) :: the_pair
    class(*), allocatable :: cdr_value

    select type (pair => .autoval. the_pair)
    class is (cons_t)
       if (associated (pair%heap_element)) then
          select type (data => pair%heap_element%data)
          class is (pair_data_t)
             cdr_value = data%cdr
          class default
             call strange_error
          end select
       else
          call error_abort ("cdr of a nil list")
       end if
    class default
       call error_abort ("cdr of an object that is not a cons_t")
    end select
  end function cdr

  recursive subroutine set_car (the_pair, car_value)
    class(*), intent(in) :: the_pair
    class(*), intent(in) :: car_value

    type(cons_t) :: pair

    pair = .tocons. the_pair
    if (associated (pair%heap_element)) then
       select type (data => pair%heap_element%data)
       class is (pair_data_t)
          data%car = .autoval. car_value
       end select
    else
       call error_abort ("set_car of a nil list")
    end if
  end subroutine set_car

  recursive subroutine set_cdr (the_pair, cdr_value)
    class(*), intent(in) :: the_pair
    class(*), intent(in) :: cdr_value

    type(cons_t) :: pair

    pair = .tocons. the_pair
    if (associated (pair%heap_element)) then
       select type (data => pair%heap_element%data)
       class is (pair_data_t)
          data%cdr = .autoval. cdr_value
       end select
    else
       call error_abort ("set_cdr of a nil list")
    end if
  end subroutine set_cdr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function caar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = car (element)
  end function caar

  function cdar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = cdr (element)
  end function cdar

  function cadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = car (element)
  end function cadr

  function cddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = cdr (element)
  end function cddr

  function caaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = car (element)
    element = car (element)
  end function caaar

  function cdaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = car (element)
    element = cdr (element)
  end function cdaar

  function cadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = cdr (element)
    element = car (element)
  end function cadar

  function cddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = cdr (element)
    element = cdr (element)
  end function cddar

  function caadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = car (element)
    element = car (element)
  end function caadr

  function cdadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = car (element)
    element = cdr (element)
  end function cdadr

  function caddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function caddr

  function cdddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
  end function cdddr

  function caaaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = car (element)
    element = car (element)
    element = car (element)
  end function caaaar

  function cdaaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = car (element)
    element = car (element)
    element = cdr (element)
  end function cdaaar

  function cadaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = car (element)
    element = cdr (element)
    element = car (element)
  end function cadaar

  function cddaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = car (element)
    element = cdr (element)
    element = cdr (element)
  end function cddaar

  function caadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = cdr (element)
    element = car (element)
    element = car (element)
  end function caadar

  function cdadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = cdr (element)
    element = car (element)
    element = cdr (element)
  end function cdadar

  function caddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function caddar

  function cdddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = car (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
  end function cdddar

  function caaadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = car (element)
    element = car (element)
    element = car (element)
  end function caaadr

  function cdaadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = car (element)
    element = car (element)
    element = cdr (element)
  end function cdaadr

  function cadadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = car (element)
    element = cdr (element)
    element = car (element)
  end function cadadr

  function cddadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = car (element)
    element = cdr (element)
    element = cdr (element)
  end function cddadr

  function caaddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = cdr (element)
    element = car (element)
    element = car (element)
  end function caaddr

  function cdaddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = cdr (element)
    element = car (element)
    element = cdr (element)
  end function cdaddr

  function cadddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function cadddr

  function cddddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
  end function cddddr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function first (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = car (element)
  end function first

  function second (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = car (element)
  end function second

  function third (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function third

  function fourth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function fourth

  function fifth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function fifth

  function sixth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function sixth

  function seventh (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function seventh

  function eighth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function eighth

  function ninth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function ninth

  function tenth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = cdr (element)
    element = car (element)
  end function tenth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function list_ref0 (lst, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = car (drop (lst, i))
  end function list_ref0

  function list_ref1 (lst, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = list_ref0 (lst, i - 1_sz)
  end function list_ref1

  function list_refn (lst, n, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = list_ref0 (lst, i - n)
  end function list_refn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function list1 (obj1) result (lst)
    class(*), intent(in) :: obj1
    type(cons_t) :: lst

    lst = obj1 ** nil
  end function list1

  function list2 (obj1, obj2) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    type(cons_t) :: lst

    lst = obj2 ** nil
    lst = obj1 ** lst
  end function list2

  function list3 (obj1, obj2, obj3) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    type(cons_t) :: lst

    lst = obj3 ** nil
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list3

  function list4 (obj1, obj2, obj3, obj4) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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
    type(cons_t) :: lst

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

  function list1_with_tail (obj1, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj1, tail)
  end function list1_with_tail

  function list2_with_tail (obj1, obj2, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj2, tail)
    lst = obj1 ** lst
  end function list2_with_tail

  function list3_with_tail (obj1, obj2, obj3, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj3, tail)
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list3_with_tail

  function list4_with_tail (obj1, obj2, obj3, obj4, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj4, tail)
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list4_with_tail

  function list5_with_tail (obj1, obj2, obj3, obj4, obj5, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj5, tail)
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list5_with_tail

  function list6_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj6, tail)
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list6_with_tail

  function list7_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj7, tail)
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list7_with_tail

  function list8_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj8, tail)
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list8_with_tail

  function list9_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, tail) result (lst)
    class(*), intent(in) :: obj1
    class(*), intent(in) :: obj2
    class(*), intent(in) :: obj3
    class(*), intent(in) :: obj4
    class(*), intent(in) :: obj5
    class(*), intent(in) :: obj6
    class(*), intent(in) :: obj7
    class(*), intent(in) :: obj8
    class(*), intent(in) :: obj9
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj9, tail)
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list9_with_tail

  function list10_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj10, tail)
    lst = obj9 ** lst
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list10_with_tail

  function list11_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj11, tail)
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
  end function list11_with_tail

  function list12_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj12, tail)
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
  end function list12_with_tail

  function list13_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj13, tail)
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
  end function list13_with_tail

  function list14_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj14, tail)
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
  end function list14_with_tail

  function list15_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj15, tail)
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
  end function list15_with_tail

  function list16_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj16, tail)
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
  end function list16_with_tail

  function list17_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj17, tail)
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
  end function list17_with_tail

  function list18_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj18, tail)
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
  end function list18_with_tail

  function list19_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj19, tail)
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
  end function list19_with_tail

  function list20_with_tail (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, &
obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18, obj19, obj20, tail) result (lst)
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
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj20, tail)
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
  end function list20_with_tail

  subroutine unlist1 (lst, obj1)
    !
    ! This subroutine `unlists' the 1 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
    class(*), allocatable, intent(inout) :: obj1

    class(*), allocatable :: tail

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tail = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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

    tl = lst
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
    ! An object that is not a cons_t or a nil_t is considered dotted.
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
    type(gcroot_t) :: lead
    type(gcroot_t) :: lag

    logical :: is_dot
    logical :: is_circ
    logical :: done

    lead = .autoval. obj

    lag = lead
    is_dot = .false.
    is_circ = .false.
    done = .false.
    do while (.not. done)
       if (is_not_pair (lead)) then
          is_dot = is_not_nil (lead)
          done = .true.
       else
          lead = cdr (lead)
          if (is_not_pair (lead)) then
             is_dot = is_not_nil (lead)
             done = .true.
          else
             lead = cdr (lead)
             lag = cdr (lag)
             if (is_pair (lead)) then
                if (cons_t_eq (lead, lag)) then
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

  function take (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_t

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    integer(sz) :: i

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (n <= 0) then
          lst_t = nil
       else
          if (is_not_pair (lst1)) then
             call error_abort ("positive `take' of a nil list")
          else
             call uncons (lst1, head, tail)
             cursor = head ** nil
             lst_t = cursor
             i = n - 1
             do while (0 < i .and. is_pair (tail))
                call uncons (tail, head, tail)
                new_pair = head ** nil
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                i = i - 1
             end do
          end if
       end if
    class default
       call error_abort ("`take' of an object that is not a cons_t")
    end select
  end function take

  function drop (lst, n) result (lst_d)
    !
    ! Dotted lists are handled correctly, unless they are degenerate
    ! (that is, not a cons_t).
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_d

    type(gcroot_t) :: lst1
    integer(sz) :: i

    lst1 = lst
    do i = 1_sz, n
       lst1 = cdr (lst1)
    end do
    lst_d = .tocons. lst1
  end function drop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function last_pair (lst) result (the_last_pair)
    class(*), intent(in) :: lst
    type(cons_t) :: the_last_pair

    type(gcroot_t) :: tail

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       the_last_pair = lst1
       if (is_pair (the_last_pair)) then
          tail = cdr (the_last_pair)
          do while (is_pair (tail))
             the_last_pair = .tocons. tail
             tail = cdr (the_last_pair)
          end do
       else
          call error_abort ("last_pair of a nil list")
       end if
    class default
       call error_abort ("last_pair of an object with no pairs")
    end select
  end function last_pair

  function last (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element

    element = car (last_pair (lst))
  end function last

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function make_list (length, fill_value) result (lst)
    integer(sz), intent(in) :: length
    class(*), intent(in) :: fill_value
    type(cons_t) :: lst

    integer(sz) :: i

    lst = nil
    do i = 1_sz, length
       lst = fill_value ** lst
    end do
  end function make_list

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function list_tabulate0 (length, init_subr) result (lst)
    integer(sz), intent(in) :: length
    procedure(list_tabulate_init_proc_t) :: init_subr
    type(cons_t) :: lst

    lst = list_tabulaten (length, 0_sz, init_subr)
  end function list_tabulate0

  function list_tabulate1 (length, init_subr) result (lst)
    integer(sz), intent(in) :: length
    procedure(list_tabulate_init_proc_t) :: init_subr
    type(cons_t) :: lst

    lst = list_tabulaten (length, 1_sz, init_subr)
  end function list_tabulate1

  function list_tabulaten (length, n, init_subr) result (lst)
    !
    ! NOTE: The order in which calls to init_subr are made is to be
    !       considered unspecified.
    !
    integer(sz), intent(in) :: length
    integer(sz), intent(in) :: n
    procedure(list_tabulate_init_proc_t) :: init_subr
    type(cons_t) :: lst

    integer(sz) :: i
    class(*), allocatable :: x

    ! Use a gcroot_t, to protect against any garbage collections done
    ! by init_subr.
    type(gcroot_t) :: lst1

    ! In this implementation: work backwards, from greater indices to
    ! lesser ones, so there will be no need to reverse the list.
    lst1 = nil
    do i = length - 1_sz, 0_sz, -1_sz
       call init_subr (n + i, x)
       lst1 = cons (x, lst1)
    end do
    lst = .tocons. lst1
  end function list_tabulaten

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function list_copy (lst) result (lst_c)
    !
    ! In the current implementation: dotted lists will be copied,
    ! unless they are degenerate (that is, not a cons_t). But circular
    ! lists are not handled.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: lst_c

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (is_nil (lst1)) then
          lst_c = nil
       else
          call uncons (lst1, head, tail)
          cursor = head ** nil
          lst_c = cursor
          do while (is_pair (tail))
             call uncons (tail, head, tail)
             new_pair = head ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
          end do
          call set_cdr (cursor, tail)
       end if
    class default
       call error_abort ("list_copy of an object that is not a cons_t")
    end select
  end function list_copy

  function reverse (lst) result (lst_r)
    !
    ! In this implementation, the final CDR of any dotted list
    ! (including any non-cons_t object) is dropped, but the operation
    ! proceeds otherwise. I would not rely on that behavior, however.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    type(gcroot_t) :: tail
    
    lst_r = nil
    tail = lst
    do while (is_pair (tail))
       lst_r = car (tail) ** lst_r
       tail = cdr (tail)
    end do
  end function reverse

  function reversex (lst) result (lst_r)
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    type(cons_t) :: lst2

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (is_pair (lst1)) then
          lst2 = copy_first_pair (lst1)
          call reverse_in_place (lst2)
          lst_r = lst2
       else
          lst_r = nil
       end if
    class default
       lst_r = nil
    end select
  end function reversex

  subroutine reverse_in_place (lst)
    type(cons_t), intent(inout) :: lst

    type(cons_t) :: lst_r
    type(cons_t) :: tail

    lst_r = nil
    do while (is_pair (lst))
       select type (tmp => cdr (lst))
       class is (cons_t)
          tail = tmp
       class default
          call error_abort ("list reversal of an object that is not a pair")
       end select
       call set_cdr (lst, lst_r)
       lst_r = lst
       lst = tail
    end do
    lst = lst_r
  end subroutine reverse_in_place

  function circular_list (lst) result (clst)
    !
    ! Make a fully circular list with the same CARs as lst.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: clst

    type(cons_t) :: the_last_pair

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (is_pair (lst1)) then
          clst = reverse (lst1)
          the_last_pair = clst
          call reverse_in_place (clst)
          call set_cdr (the_last_pair, clst)
       else
          call error_abort ("circular_list of a nil list")
       end if
    class default
       call error_abort ("circular_list of an object with no pairs")
    end select
  end function circular_list

  function circular_listx (lst) result (clst)
    !
    ! Connect the tail of lst to its head, destructively.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: clst

    type(cons_t) :: the_last_pair

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       the_last_pair = last_pair (lst1)
       call set_cdr (the_last_pair, lst1)
       clst = lst1
    class default
       call error_abort ("destructive_circular_list of an object with no pairs")
    end select
  end function circular_listx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function iota_of_length (length) result (lst)
    integer(sz), intent(in) :: length
    class(*), allocatable :: lst

    lst = iota_of_length_start_step (length, 0_sz, 1_sz)
  end function iota_of_length

  function iota_of_length_start (length, start) result (lst)
    integer(sz), intent(in) :: length, start
    class(*), allocatable :: lst

    lst = iota_of_length_start_step (length, start, 1_sz)
  end function iota_of_length_start

  function iota_of_length_start_step (length, start, step) result (lst)
    integer(sz), intent(in) :: length, start, step
    class(*), allocatable :: lst

    integer(sz) :: i, n

    if (length < 0_sz) then
       call error_abort ("iota with negative length")
    else if (length == 0_sz) then
       lst = nil
    else
       ! Go through the sequence backwards, so we will not have to
       ! reverse the resulting list.
       n = start + ((length - 1_sz) * step)
       lst = nil
       do i = 1_sz, length
          lst = cons (n, lst)
          n = n - step
       end do
    end if
  end function iota_of_length_start_step

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function lists_are_equal (pred, lst1, lst2) result (bool)
    !
    ! An equivalent to SRFI-1 `(list= pred lst1 lst2)'.
    !
    ! In the call
    !
    !    lists_are_equal (pred, lst1, lst2)
    !
    ! pred is applied with an element of lst1 as its first argument
    ! and an element of lst2 as its second argument.
    !
    ! The pred function must be some kind of `equality' test, and not
    ! just any predicate (such as a `less than' test); in particular,
    !
    !    pred (x, y)
    !
    ! must return .true. if x and y are the same object. (Therefore
    ! shared tails always are `equal'.)
    !
    ! The current implementation does not handle circular lists.
    !
    ! WARNING: It is an error to call this procedure if either lst1 or
    !          lst2 is a dotted list.
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst1, lst2
    logical :: bool

    type(cons_t) :: p, q
    class(*), allocatable :: p_hd, q_hd
    class(*), allocatable :: p_tl, q_tl
    logical :: done

    type(gcroot_t) :: lst1_root, lst2_root

    ! Protect against garbage collections in the predicate.
    lst1_root = lst1
    lst2_root = lst2

    p = .tocons. lst1
    q = .tocons. lst2
    done = .false.
    do while (.not. done)
       if (is_not_pair (p)) then
          ! lst1 comes to an end.
          bool = is_not_pair (q) ! Does lst2 also come to an end?
          done = .true.
       else if (is_not_pair (q)) then
          ! lst2 comes to an end (even though lst1 does not).
          bool = .false.
          done = .true.
       else if (cons_t_eq (p, q)) then
          ! The two lists share a tail.
          bool = .true.
          done = .true.
       else
          call uncons (p, p_hd, p_tl)
          call uncons (q, q_hd, q_tl)
          if (.not. pred (p_hd, q_hd)) then
             ! The predicate failed for some elements of lst1 and lst2
             ! respectively.
             bool = .false.
             done = .true.
          else
             p = cons_t_cast (p_tl)
             q = cons_t_cast (q_tl)
          end if
       end if
    end do

    call lst1_root%discard
    call lst2_root%discard
  end function lists_are_equal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module cons_pairs
