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
  ! Please keep in mind that the term `list' is used only loosely in
  ! Scheme. The fundamental `list' object types are actually the
  ! `null' -- in this module called `nil' -- and the CAR-CDR
  ! pair. Given the nil and pairs, one can construct practically any
  ! kind of binary tree or graph; also, the pairs can be used as
  ! efficient 2-tuples.
  !
  ! Furthermore, objects other than the nil and pairs (such as 123,
  ! "abc", 4.0, .true., etc.) can be -- and in this module will be --
  ! regarded as degenerate `dotted lists' -- as a sort of generalized
  ! nil. \footnote{The term `dotted list' is Scheme-talk for a linked
  ! list that ends in something other than a nil or back-reference. A
  ! list that ends in a nil is called ‘proper’ and a list that ends in
  ! a back-reference is called ‘circular’.}
  !
  ! NOTE: Fortran procedures do not take multiple arguments, the way
  ! Scheme procedures do. However, the zip2, zip3, etc., functions can
  ! be used to turn multiple-argument problems into single-argument
  ! problems.
  !

  !
  ! NOTE: Unless you know what you are doing, you should use
  !       `type(gcroot_t)' from module `garbage_collector' to hold
  !       values of type `cons_t'. Otherwise the garbage collector
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
m4_forloop([n],[2],CADADR_MAX,[m4_length_n_cadadr_public_declarations(n)])dnl

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

  public :: list_ref0        ! Generic function: return any one of the 0th, 1st, 2nd, etc., elements.
  public :: list_ref1        ! Generic function: Return any one of the 1st, 2nd, 3rd, etc., elements.
  public :: list_refn        ! Generic function: Return any one of the nth, (n+1)th, (n+2)th, etc., elements.
  public :: list_ref0_size_kind ! Versions for INTEGER(SIZE_KIND).
  public :: list_ref1_size_kind
  public :: list_refn_size_kind
  public :: list_ref0_int    ! Versions for INTEGER of the default kind.
  public :: list_ref1_int
  public :: list_refn_int

  ! Make and unmake a list of particular length, or of a certain
  ! length and also a tail. (SRFI-1 has `list' and `cons*' have
  ! related functionality.)
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: list[]n
])dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: list[]n[]_with_tail
])dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: unlist[]n
])dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: unlist[]n[]_with_tail
])dnl

  ! Zipping: joining the elements of separate lists into a list of
  ! lists.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: zip[]n
])dnl

  ! Unzipping: separating the elements of a list of lists into
  ! separate lists.
m4_forloop([n],[1],ZIP_MAX,[dnl
  public :: unzip[]n
])dnl

  ! unzip1f is the same as unzip1, except as a function instead of a
  ! subroutine.
  public :: unzip1f

  ! SRFI-1 does not have `classify_list', although it does have
  ! procedures this module derives from it (`proper-list?',
  ! `dotted-list?', and `circular-list?).
  public :: classify_list    ! Classify a list as proper, dotted, or circular.
  public :: is_proper_list   ! A list that terminates in a NIL.
  public :: is_dotted_list   ! A list that terminates in a non-NIL.
  public :: is_circular_list ! A list that does not terminate.

  public :: length           ! The length of a proper or dotted list.
  public :: lengthc          ! The length of a proper or dotted list, or -1 for a circular list. (SRFI-1 has `length+'.)

  ! The generics:
  public :: take             ! Return a freshly allocated copy of the first n elements of a list.
  public :: takex            ! Like take, but allowed to destroy its inputs. (Currently, it cannot handle circular lists.)
  public :: drop             ! Return a common tail containing all but the first n elements of a list.
  public :: take_right       ! Return a common tail containing the last n elements of a list.
  public :: drop_right       ! Return a freshly allocated copy of all but the last n elements of a list.
  public :: drop_rightx      ! Like drop_right, but allowed to destroy its inputs.
  public :: split_at         ! Do both take and drop, at the same time.
  public :: split_atx        ! Like split_at, but allowed to destroy its inputs.
  ! Versions for INTEGER(SIZE_KIND).
  public :: take_size_kind
  public :: takex_size_kind
  public :: drop_size_kind
  public :: take_right_size_kind
  public :: drop_right_size_kind
  public :: drop_rightx_size_kind
  public :: split_at_size_kind
  public :: split_atx_size_kind
  ! Versions for INTEGER of the default kind.
  public :: take_int
  public :: takex_int
  public :: drop_int
  public :: take_right_int
  public :: drop_right_int
  public :: drop_rightx_int
  public :: split_at_int
  public :: split_atx_int

  public :: last_pair        ! Return the last pair of a list.
  public :: last             ! Return the last CAR of a list.

  public :: make_list        ! Generic: return a list of repeated values.
  public :: make_list_size_kind ! Version for INTEGER(SIZE_KIND).
  public :: make_list_int       ! Version for INTEGER of the default kind.

  ! Return a list of values determined by a procedure.
  public :: list_tabulate_init_proc_t ! The type for the initialization procedure.
  public :: list_tabulate0   ! Indices start at 0_size_kind.
  public :: list_tabulate1   ! Indices start at 1_size_kind.
  public :: list_tabulaten   ! Indices start at n.

  abstract interface
     recursive subroutine list_tabulate_init_proc_t (i, x)
       import size_kind
       integer(size_kind), intent(in) :: i
       class(*), allocatable, intent(out) :: x
     end subroutine list_tabulate_init_proc_t
  end interface

  ! iota: return a list containing a sequence of equally spaced integers.
  public :: iota             ! `iota' = the most generic function.
  public :: iota_of_length   ! Other generics.
  public :: iota_of_length_start
  public :: iota_of_length_start_step
  public :: iota_of_length_size_kind ! Versions for INTEGER(SIZE_KIND).
  public :: iota_of_length_start_size_kind
  public :: iota_of_length_start_step_size_kind
  public :: iota_of_length_int ! Versions for INTEGER of the default kind.
  public :: iota_of_length_start_int
  public :: iota_of_length_start_step_int

  public :: list_copy        ! Make a copy of a list.
  public :: reverse          ! Make a copy of a list, but reversed.
  public :: reversex         ! Like reverse, but allowed to destroy its inputs.
  public :: append           ! Concatenate two lists.
  public :: appendx          ! Like append, but allowed to destroy its *first* argument (but not the latter argument).
  public :: append_reverse   ! Concatenate the reverse of the first list to the (unreversed) second list.
  public :: append_reversex  ! Like append_reverse, but allowed to destroy its *first* argument (but not the latter argument).
  public :: concatenate      ! Concatenate the lists in a list of lists.
  public :: concatenatex     ! Like concatenate, but allowed to destroy its inputs.

  ! Although `circular_list' and `circular_listx' gets their names
  ! from the SRFI-1 `circular-list', as input they take a regular
  ! list, rather than multiple arguments for individual list elements.
  public :: circular_list    ! Make a copy of a list, but with the tail connected to the head.
  public :: circular_listx   ! Like circular_list, but allowed to destroy its inputs.

  public :: lists_are_equal  ! Test whether two lists are `equal'. (Equivalent to SRFI-1's `list='.)
  public :: list_count       ! Count elements that satisfy a predicate. (Counting proceeds in left-to-right order.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! FOLDS AND UNFOLDS
!!

  public :: fold             ! `The fundamental list iterator.'
  public :: fold_right       ! `The fundamental list recursion operator.'
  public :: pair_fold        ! Like fold, but applied to sublists instead of elements.
  public :: pair_fold_right  ! Like fold_right, but applied to sublists instead of elements.
  public :: reduce           ! A variant of fold. See SRFI-1.
  public :: reduce_right     ! A variant of fold_right. See SRFI-1.

  public :: unfold           ! Generic: `The fundamental recursive list constructor.' See SRFI-1.
  public :: unfold_with_tail_gen ! One of the implementations of `unfold'.
  public :: unfold_with_nil_tail ! One of the implementations of `unfold'.

  public :: unfold_right     ! Generic: `The fundamental iterative list constructor.' See SRFI-1.
  public :: unfold_right_with_tail     ! One of the implementations of `unfold_right'.
  public :: unfold_right_with_nil_tail ! One of the implementations of `unfold_right'.

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

  ! Types for folds, unfolds, and maps.
  public :: list_kons_proc_t
  public :: list_map_proc_t

  abstract interface
     recursive subroutine list_kons_proc_t (kar, kdr, kons_result)
       !
       ! The type of the `kons' argument to a fold procedure.
       !
       class(*), intent(in) :: kar, kdr
       class(*), allocatable, intent(out) :: kons_result
     end subroutine list_kons_proc_t
  end interface

  abstract interface
     recursive subroutine list_map_proc_t (input, output)
       class(*), intent(in) :: input
       class(*), allocatable, intent(out) :: output
     end subroutine list_map_proc_t
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

  interface list_ref0
     module procedure list_ref0_size_kind
     module procedure list_ref0_int
  end interface list_ref0

  interface list_ref1
     module procedure list_ref1_size_kind
     module procedure list_ref1_int
  end interface list_ref1

  interface list_refn
     module procedure list_refn_size_kind
     module procedure list_refn_int
  end interface list_refn

  interface take
     module procedure take_size_kind
     module procedure take_int
  end interface take

  interface takex
     module procedure takex_size_kind
     module procedure takex_int
  end interface takex

  interface drop
     module procedure drop_size_kind
     module procedure drop_int
  end interface drop

  interface take_right
     module procedure take_right_size_kind
     module procedure take_right_int
  end interface take_right

  interface drop_right
     module procedure drop_right_size_kind
     module procedure drop_right_int
  end interface drop_right

  interface drop_rightx
     module procedure drop_rightx_size_kind
     module procedure drop_rightx_int
  end interface drop_rightx

  interface split_at
     module procedure split_at_size_kind
     module procedure split_at_int
  end interface split_at

  interface split_atx
     module procedure split_atx_size_kind
     module procedure split_atx_int
  end interface split_atx

  interface make_list
     module procedure make_list_size_kind
     module procedure make_list_int
  end interface make_list

  interface iota
     module procedure iota_of_length_size_kind
     module procedure iota_of_length_start_size_kind
     module procedure iota_of_length_start_step_size_kind
     module procedure iota_of_length_int
     module procedure iota_of_length_start_int
     module procedure iota_of_length_start_step_int
  end interface iota

  interface iota_of_length
     module procedure iota_of_length_size_kind
     module procedure iota_of_length_int
  end interface iota_of_length

  interface iota_of_length_start
     module procedure iota_of_length_start_size_kind
     module procedure iota_of_length_start_int
  end interface iota_of_length_start

  interface iota_of_length_start_step
     module procedure iota_of_length_start_step_size_kind
     module procedure iota_of_length_start_step_int
  end interface iota_of_length_start_step

  interface unfold
     module procedure unfold_with_tail_gen
     module procedure unfold_with_nil_tail
  end interface unfold

  interface unfold_right
     module procedure unfold_right_with_tail
     module procedure unfold_right_with_nil_tail
  end interface unfold_right

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

    bool = .false.
    select type (obj)
    class is (cons_t)
       bool = associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
          bool = associated (val%heap_element)
       end select
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

    bool = .false.
    select type (obj)
    class is (cons_t)
       bool = .not. associated (obj%heap_element)
    class is (gcroot_t)
       select type (val => .val. obj)
       class is (cons_t)
          bool = .not. associated (val%heap_element)
       end select
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

m4_forloop([n],[2],CADADR_MAX,[m4_length_n_cadadr_definitions(n)])dnl
dnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function first (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([1],[element])dnl
  end function first

  function second (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([2],[element])dnl
  end function second

  function third (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([3],[element])dnl
  end function third

  function fourth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([4],[element])dnl
  end function fourth

  function fifth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([5],[element])dnl
  end function fifth

  function sixth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([6],[element])dnl
  end function sixth

  function seventh (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([7],[element])dnl
  end function seventh

  function eighth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([8],[element])dnl
  end function eighth

  function ninth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([9],[element])dnl
  end function ninth

  function tenth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = lst
m4_bits_to_get_nth_element([10],[element])dnl
  end function tenth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function list_ref0_size_kind (lst, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = car (drop (lst, i))
  end function list_ref0_size_kind

  function list_ref1_size_kind (lst, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = list_ref0_size_kind (lst, i - 1_sz)
  end function list_ref1_size_kind

  function list_refn_size_kind (lst, n, i) result (element)
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    integer(sz), intent(in) :: i
    class(*), allocatable :: element

    element = list_ref0_size_kind (lst, i - n)
  end function list_refn_size_kind

  function list_ref0_int (lst, i) result (element)
    class(*), intent(in) :: lst
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: ii

    ii = i
    element = list_ref0_size_kind (lst, ii)
  end function list_ref0_int

  function list_ref1_int (lst, i) result (element)
    class(*), intent(in) :: lst
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: ii

    ii = i
    element = list_ref1_size_kind (lst, ii)
  end function list_ref1_int

  function list_refn_int (lst, n, i) result (element)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    integer, intent(in) :: i
    class(*), allocatable :: element

    integer(sz) :: nn
    integer(sz) :: ii

    nn = n
    ii = i
    element = list_refn_size_kind (lst, nn, ii)
  end function list_refn_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dnl
m4_forloop([n],[1],LISTN_MAX,[
  function list[]n (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
])obj[]k])) result (lst)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    type(cons_t) :: lst

    lst = obj[]n ** nil
dnl
m4_forloop([k],[2],n,[dnl
    lst = obj[]m4_eval(n - k + 1) ** lst
])dnl
  end function list[]n
])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[
  function list[]n[]_with_tail (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
])obj[]k]), tail) result (lst)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    class(*), intent(in) :: tail
    type(cons_t) :: lst

    lst = cons (obj[]n, tail)
dnl
m4_forloop([k],[2],n,[dnl
    lst = obj[]m4_eval(n - k + 1) ** lst
])dnl
  end function list[]n[]_with_tail
])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[
  subroutine unlist[]n (lst, obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
])obj[]k]))
    !
    ! This subroutine `unlists' the n elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(*), intent(in) :: lst
m4_forloop([k],[1],n,[dnl
    class(*), allocatable, intent(inout) :: obj[]k
])dnl

    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, obj1, tail)
dnl
m4_forloop([k],[2],n,[dnl
    call uncons (tail, obj[]k, tail)
])dnl
    if (is_pair (tail)) then
       call error_abort ("unlist[]n[] of a list that is too long")
    end if
  end subroutine unlist[]n
])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[
  subroutine unlist[]n[]_with_tail (lst, obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
])obj[]k]), tail)
    !
    ! This subroutine `unlists' the leading n elements of lst, and
    ! also returns the tail.
    !
    class(*), intent(in) :: lst
m4_forloop([k],[1],n,[dnl
    class(*), allocatable, intent(inout) :: obj[]k
])dnl
    class(*), allocatable, intent(inout) :: tail

    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, obj1, tl)
dnl
m4_forloop([k],[2],n,[dnl
    call uncons (tl, obj[]k, tl)
])dnl
    tail = tl
  end subroutine unlist[]n[]_with_tail
])dnl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dnl
m4_forloop([n],[1],ZIP_MAX,[
  function zip[]n (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
       ])lst[]k])) result (lst_z)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: lst[]k
])dnl
    type(cons_t) :: lst_z

m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: head[]k, tail[]k
])dnl
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

m4_forloop([k],[1],n,[dnl
    tail[]k = .autoval. lst[]k
])dnl

    if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
       lst_z = nil
m4_if(k,n,[dnl
    else
],[dnl
    else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
dnl
m4_forloop([k],[1],n,[dnl
       call uncons (tail[]k, head[]k, tail[]k)
])dnl
       row = nil
m4_forloop([k],[1],n,[dnl
       row = head[]m4_eval(n - k + 1) ** row
])dnl
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
          continue
m4_if(k,n,[dnl
       else
],[dnl
       else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          done = .false.
          do while (.not. done)
m4_forloop([k],[1],n,[dnl
             call uncons (tail[]k, head[]k, tail[]k)
])dnl
             row = nil
m4_forloop([k],[1],n,[dnl
             row = head[]m4_eval(n - k + 1) ** row
])dnl
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
                done = .true.
m4_if(k,n,[dnl
             end if
],[dnl
             else if (is_not_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          end do
       end if
    end if
  end function zip[]n
])dnl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dnl
m4_forloop([n],[1],ZIP_MAX,[
  subroutine unzip[]n (lst_zipped, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
       ])lst[]k]))
    class(*), intent(in) :: lst_zipped
m4_forloop([k],[1],n,[dnl
    type(cons_t), intent(inout) :: lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    type(cons_t) :: cursor[]k
])dnl

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
m4_forloop([k],[1],n,[dnl
       lst[]k = nil
])dnl
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip[]n, expected a cons_t")
       end select
m4_forloop([k],[1],n,[dnl
       call uncons (head_zipped, head, tl)
       lst[]k = head ** nil
       cursor[]k = lst[]k
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip[]n, expected a cons_t")
       end select
])dnl
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip[]n, expected a cons_t")
          end select
m4_forloop([k],[1],n,[dnl
m4_if(k,n,[dnl
          head = car (head_zipped)
],[dnl
          call uncons (head_zipped, head, tl)
])dnl
          new_pair = head ** nil
          call set_cdr (cursor[]k, new_pair)
          cursor[]k = new_pair
m4_if(k,n,[],[dnl
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip[]n, expected a cons_t")
          end select
])dnl
])dnl
       end do
    end if
  end subroutine unzip[]n
])dnl

  function unzip1f (lst_zipped) result (lst)
    class(*), intent(in) :: lst_zipped
    type(cons_t) :: lst

    call unzip1 (lst_zipped, lst)
  end function unzip1f

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
    class(*), allocatable :: lead
    class(*), allocatable :: lag

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

  function length (lst) result (len)
    !
    ! Returns zero as the length of a degenerate dotted list.
    !
    class(*), intent(in) :: lst
    integer(sz) :: len

    class(*), allocatable :: tail

    len = 0
    select type (lst1 => .autoval. lst)
    class is (cons_t)
       tail = lst1
       do while (is_pair (tail))
          len = len + 1
          tail = cdr (tail)
       end do
    end select
  end function length

  function lengthc (lst) result (len)
    !
    ! A variant of length that returns -1 if the list is circular.
    !
    class(*), intent(in) :: lst
    integer(sz) :: len

    ! Detect circularity by having a `lead' reference move through the
    ! list at a higher rate than a `lag' reference. In a circular
    ! list, eventually `lead' will catch up with `lag'.
    class(*), allocatable :: lead
    class(*), allocatable :: lag

    logical :: done

    len = 0
    lead = .autoval. lst
    lag = lead
    done = .false.
    do while (.not. done .and. is_pair (lead))
       lead = cdr (lead)
       len = len + 1
       if (is_pair (lead)) then
          lead = cdr (lead)
          lag = cdr (lag)
          len = len + 1
          select type (lead)
          class is (cons_t)
             select type (lag)
             class is (cons_t)
                if (cons_t_eq (lead, lag)) then
                   len = -1
                   done = .true.
                end if
             end select
          end select
       else
          done = .true.
       end if
    end do
  end function lengthc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function take_size_kind (lst, n) result (lst_t)
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
  end function take_size_kind

  function take_int (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_t

    integer(sz) :: nn

    nn = n
    lst_t = take_size_kind (lst, nn)
  end function take_int

  function takex_size_kind (lst, n) result (lst_t)
    !
    ! NOTE: This implementation cannot handle circular lists. If lst
    !       may be circular, either check first and call `take'
    !       instead of `takex', if lst is circular; or else simply use
    !       `take' instead.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_t

    type(cons_t) :: lst1
    type(cons_t) :: new_last_pair

    if (n <= 0) then
       lst_t = nil
    else
       lst1 = .tocons. lst
       if (is_not_pair (lst1)) then
          lst_t = nil
       else
          lst_t = lst1
          new_last_pair = .tocons. (drop (lst_t, n - 1))
          call set_cdr (new_last_pair, nil)
       end if
    end if
  end function takex_size_kind

  function takex_int (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_t

    integer(sz) :: nn

    nn = n
    lst_t = takex_size_kind (lst, nn)
  end function takex_int

  function drop_size_kind (lst, n) result (lst_d)
    !
    ! If lst is dotted, then the result will be dotted.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    class(*), allocatable :: lst_d

    integer(sz) :: i

    lst_d = .autoval. lst
    do i = 1_sz, n
       lst_d = cdr (lst_d)
    end do
  end function drop_size_kind

  function drop_int (lst, n) result (lst_d)
    !
    ! If lst is dotted, then the result will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: lst_d

    integer(sz) :: nn

    nn = n
    lst_d = drop_size_kind (lst, nn)
  end function drop_int

  function take_right_size_kind (lst, n) result (lst_tr)
    !
    ! lst may be dotted, in which case the result will be dotted. lst
    ! must not be circular.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    class(*), allocatable :: lst_tr

    class(*), allocatable :: p

    lst_tr = .autoval. lst
    p = drop (lst_tr, n)
    do while (is_pair (p))
       lst_tr = cdr (lst_tr)
       p = cdr (p)
    end do
  end function take_right_size_kind

  function take_right_int (lst, n) result (lst_t)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: lst_t

    integer(sz) :: nn

    nn = n
    lst_t = take_right_size_kind (lst, nn)
  end function take_right_int

  function drop_right_size_kind (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_dr

    lst_dr = take (lst, length (lst) - n)
  end function drop_right_size_kind

  function drop_right_int (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_dr

    integer(sz) :: nn

    nn = n
    lst_dr = drop_right_size_kind (lst, nn)
  end function drop_right_int

  function drop_rightx_size_kind (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t) :: lst_dr

    lst_dr = takex (lst, length (lst) - n)
  end function drop_rightx_size_kind

  function drop_rightx_int (lst, n) result (lst_dr)
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_dr

    integer(sz) :: nn

    nn = n
    lst_dr = drop_rightx_size_kind (lst, nn)
  end function drop_rightx_int

  subroutine split_at_size_kind (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    type(cons_t) :: lst_t
    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    integer(sz) :: i

    if (n <= 0) then
       lst_left = nil
       lst_right = .autoval. lst
    else
       select type (lst1 => .autoval. lst)
       class is (cons_t)
          if (is_nil (lst1)) then
             call error_abort ("positive split_at of a nil list")
          else
             call uncons (lst1, head, tail)
             lst_t = cons (head, tail)
             cursor = lst_t
             i = n - 1
             do while (0 < i .and. is_pair (tail))
                call uncons (tail, head, tail)
                new_pair = cons (head, tail)
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                i = i - 1
             end do
             if (i == 0) then
                call set_cdr (cursor, nil)
             else
                call error_abort ("split_at of a list that is too short")
             end if
             lst_left = lst_t
             lst_right = tail
          end if
       class default
          call error_abort ("positive split_at of an object with no pairs")
       end select
    end if
  end subroutine split_at_size_kind

  subroutine split_at_int (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    integer(sz) :: nn

    nn = n
    call split_at_size_kind (lst, nn, lst_left, lst_right)
  end subroutine split_at_int

  subroutine split_atx_size_kind (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! lst_left will be a cons_t, but lst_right need not be.
    !
    class(*), intent(in) :: lst
    integer(sz), intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    class(*), allocatable :: lst1

    if (n <= 0) then
       lst_left = nil
       lst_right = .autoval. lst
    else
       lst1 = .autoval. lst
       if (is_not_pair (lst1)) then
          call error_abort ("positive split_atx of an object with no pairs")
       else
          lst_left = .tocons. lst1
          lst1 = drop (lst_left, n - 1)
          lst_right = cdr (lst1)
          call set_cdr (lst1, nil)
       end if
    end if
  end subroutine split_atx_size_kind

  subroutine split_atx_int (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! If lst is dotted, then lst_right will be dotted.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t), intent(inout) :: lst_left
    class(*), allocatable, intent(inout) :: lst_right

    integer(sz) :: nn

    nn = n
    call split_atx_size_kind (lst, nn, lst_left, lst_right)
  end subroutine split_atx_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function last_pair (lst) result (the_last_pair)
    class(*), intent(in) :: lst
    type(cons_t) :: the_last_pair

    class(*), allocatable :: tail

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

  function make_list_size_kind (length, fill_value) result (lst)
    integer(sz), intent(in) :: length
    class(*), intent(in) :: fill_value
    type(cons_t) :: lst

    integer(sz) :: i

    lst = nil
    do i = 1_sz, length
       lst = fill_value ** lst
    end do
  end function make_list_size_kind

  function make_list_int (length, fill_value) result (lst)
    integer, intent(in) :: length
    class(*), intent(in) :: fill_value
    type(cons_t) :: lst

    integer(sz) :: len

    len = length
    lst = make_list_size_kind (len, fill_value)
  end function make_list_int

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
    ! Dotted lists will be copied, but the current implementation
    ! cannot copy a circular list.
    !
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_c

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair

    tail = .autoval. lst
    if (is_not_pair (tail)) then
       lst_c = tail
    else
       call uncons (tail, head, tail)
       cursor = head ** nil
       lst_c = cursor
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          new_pair = head ** nil
          call set_cdr (cursor, new_pair)
          cursor = new_pair
       end do
       if (is_not_nil (tail)) then
          call set_cdr (cursor, tail)
       end if
    end if
  end function list_copy

  function reverse (lst) result (lst_r)
    !
    ! The final CDR of any dotted list (including any non-cons_t
    ! object) is dropped.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    class(*), allocatable :: tail
    
    lst_r = nil
    tail = .autoval. lst
    do while (is_pair (tail))
       lst_r = car (tail) ** lst_r
       tail = cdr (tail)
    end do
  end function reverse

  function reversex (lst) result (lst_r)
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    select type (lst1 => .autoval. lst)
    class is (cons_t)
       if (is_pair (lst1)) then
          lst_r = lst1
          call reverse_in_place (lst_r)
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

  function append (lst1, lst2) result (lst_a)
    !
    ! The tail of the result is shared with lst2. The CAR elements of
    ! lst1 are copied; the last CDR of lst1 is dropped.
    !
    ! The result need not be a cons_t.
    !
    class(*), intent(in) :: lst1, lst2
    class(*), allocatable :: lst_a

    class(*), allocatable :: lst1a
    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    type(cons_t) :: new_lst

    lst1a = .autoval. lst1
    if (is_not_pair (lst1a)) then
       lst_a = .autoval. lst2
    else
       call uncons (lst1a, head, tail)
       new_lst = head ** nil
       cursor = new_lst
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          new_pair = head ** nil
          call set_cdr (cursor, new_pair)
          cursor = new_pair
       end do
       if (is_not_nil (tail)) then
          call set_cdr (cursor, tail)
       end if
       call set_cdr (cursor, .autoval. lst2)
       lst_a = new_lst
    end if
  end function append

  function appendx (lst1, lst2) result (lst_a)
    !
    ! appendx is *not* allowed to destroy lst2, and in fact includes
    ! it in the result as a shared tail.
    !
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_a

    class(*), allocatable :: lst1a

    lst1a = .autoval. lst1
    if (is_not_pair (lst1a)) then
       lst_a = .autoval. lst2
    else
       lst_a = .tocons. lst1a
       call set_cdr (last_pair (lst_a), .autoval. lst2)
    end if
  end function appendx

  function append_reverse (lst1, lst2) result (lst_ar)
    !
    ! The tail of the result is shared with lst2. The CAR elements of
    ! lst1 are copied; the last CDR of the reverse of lst1 is dropped.
    !
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_ar

    class(*), allocatable :: head
    class(*), allocatable :: tail

    lst_ar = .autoval. lst2
    tail = .autoval. lst1
    do while (is_pair (tail))
       call uncons (tail, head, tail)
       lst_ar = cons (head, lst_ar)
    end do
  end function append_reverse

  function append_reversex (lst1, lst2) result (lst_ar)
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_ar

    lst_ar = appendx (reversex (lst1), lst2)
  end function append_reversex

  function concatenate (lists) result (lst_concat)
    !
    ! If `lists' is empty, then the result is a nil list.
    !
    class(*), intent(in) :: lists
    class(*), allocatable :: lst_concat

    class(*), allocatable :: lists1
    type(cons_t) :: lists_r
    class(*), allocatable :: head, tail

    lists1 = .autoval. lists
    if (is_not_pair (lists1)) then
       lst_concat = nil
    else
       lists_r = reverse (lists1)
       lst_concat = car (lists_r)
       tail = cdr (lists_r)
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          lst_concat = append (head, lst_concat)
       end do
    end if
  end function concatenate

  function concatenatex (lists) result (lst_concat)
    !
    ! If `lists' is empty, then the result is a nil list.
    !
    ! This implementation calls reversex (thus, perhaps contrarily to
    ! the programmer's expectations, destroying `lists'), and it uses
    ! appendx to do the concatenations.
    !
    class(*), intent(in) :: lists
    class(*), allocatable :: lst_concat

    class(*), allocatable :: lists1
    type(cons_t) :: lists_r
    class(*), allocatable :: head, tail

    lists1 = .autoval. lists
    if (is_not_pair (lists1)) then
       lst_concat = nil
    else
       lists_r = reversex (lists1)
       lst_concat = car (lists_r)
       tail = cdr (lists_r)
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          lst_concat = appendx (head, lst_concat)
       end do
    end if
  end function concatenatex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
       call error_abort ("circular_listx of an object with no pairs")
    end select
  end function circular_listx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function iota_of_length_size_kind (length) result (lst)
    integer(sz), intent(in) :: length
    type(cons_t) :: lst

    lst = iota_of_length_start_step_size_kind (length, 0_sz, 1_sz)
  end function iota_of_length_size_kind

  function iota_of_length_start_size_kind (length, start) result (lst)
    integer(sz), intent(in) :: length, start
    type(cons_t) :: lst

    lst = iota_of_length_start_step_size_kind (length, start, 1_sz)
  end function iota_of_length_start_size_kind

  function iota_of_length_start_step_size_kind (length, start, step) result (lst)
    integer(sz), intent(in) :: length, start, step
    type(cons_t) :: lst

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
  end function iota_of_length_start_step_size_kind

  function iota_of_length_int (length) result (lst)
    integer, intent(in) :: length
    type(cons_t) :: lst

    lst = iota_of_length_start_step (length, 0, 1)
  end function iota_of_length_int

  function iota_of_length_start_int (length, start) result (lst)
    integer, intent(in) :: length, start
    type(cons_t) :: lst

    lst = iota_of_length_start_step (length, start, 1)
  end function iota_of_length_start_int

  function iota_of_length_start_step_int (length, start, step) result (lst)
    integer, intent(in) :: length, start, step
    type(cons_t) :: lst

    integer :: i, n

    if (length < 0) then
       call error_abort ("iota with negative length")
    else if (length == 0) then
       lst = nil
    else
       ! Go through the sequence backwards, so we will not have to
       ! reverse the resulting list.
       n = start + ((length - 1) * step)
       lst = nil
       do i = 1, length
          lst = cons (n, lst)
          n = n - step
       end do
    end if
  end function iota_of_length_start_step_int

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

  recursive function list_count (pred, lst) result (n)
    !
    ! This is called `list_count' instead of `count' because Fortran
    ! already has an intrinsic `count' function.
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    integer(sz) :: n

    type(gcroot_t) :: lst_root
    class(*), allocatable :: head
    class(*), allocatable :: tail

    ! Protect lst against garbage collections performed by `pred'.
    lst_root = lst

    n = 0
    tail = .autoval. lst
    do while (is_pair (tail))
       call uncons (tail, head, tail)
       if (pred (head)) then
          n = n + 1
       end if
    end do

    call lst_root%discard
  end function list_count

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function fold (kons, knil, lst) result (retval)
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    type(gcroot_t) :: lst_root
    class(*), allocatable :: new_retval
    class(*), allocatable :: head, tail

    ! Protect against garbage collections performed by kons.
    lst_root = lst

    retval = knil
    tail = .autoval. lst
    do while (is_pair (tail))
       call uncons (tail, head, tail)
       call kons (head, retval, new_retval)
       retval = new_retval
    end do

    call lst_root%discard
  end function fold

  recursive function fold_right (kons, knil, lst) result (retval)
    !
    ! WARNING: This implementation is recursive and uses O(n) stack
    !          space. If you need to do something like this
    !          iteratively, you can use `fold' on the reverse of lst.
    !
    !          A recursive implementation tends to be faster, at least
    !          in functional languages:
    !
    !             * the list need not be reversed,
    !
    !             * on most hardware, the stack puts values near each
    !               other in memory.
    !
    !          In any case, a recursive implementation illustrates
    !          the fundamental meaning of the operation.
    !
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    type(gcroot_t) :: lst_root

    ! Protect against garbage collections performed by kons.
    lst_root = lst

    retval = recursion (.autoval. lst)

    call lst_root%discard

  contains

    recursive function recursion (lst) result (retval)
      class(*), intent(in) :: lst
      class(*), allocatable :: retval

      if (is_not_pair (lst)) then
         retval = knil
      else
         call kons (car (lst), recursion (cdr (lst)), retval)
      end if
    end function recursion

  end function fold_right

  recursive function pair_fold (kons, knil, lst) result (retval)
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    class(*), allocatable :: new_retval
    class(*), allocatable :: tail, new_tail

    type(gcroot_t) :: lst_root

    ! Protect against garbage collections performed by kons.
    lst_root = lst

    retval = knil
    tail = .autoval. lst
    do while (is_pair (tail))
       new_tail = cdr (tail)
       call kons (tail, retval, new_retval)
       retval = new_retval
       tail = new_tail
    end do

    call lst_root%discard
  end function pair_fold

  recursive function pair_fold_right (kons, knil, lst) result (retval)
    !
    ! WARNING: This implementation is recursive and uses O(n) stack
    !          space.
    !
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    type(gcroot_t) :: lst_root

    ! Protect against garbage collections performed by kons.
    lst_root = lst

    retval = recursion (.autoval. lst)

    call lst_root%discard

  contains

    recursive function recursion (lst) result (retval)
      class(*), intent(in) :: lst
      class(*), allocatable :: retval

      if (is_not_pair (lst)) then
         retval = knil
      else
         call kons (lst, recursion (cdr (lst)), retval)
      end if
    end function recursion

  end function pair_fold_right

  recursive function reduce (kons, right_identity, lst) result (retval)
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: right_identity
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    class(*), allocatable :: head, tail

    tail = .autoval. lst
    if (is_pair (tail)) then
       call uncons (tail, head, tail)
       retval = fold (kons, head, tail)
    else
       retval = .autoval. right_identity
    end if
  end function reduce

  recursive function reduce_right (kons, right_identity, lst) result (retval)
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: right_identity
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    class(*), allocatable :: head, tail

    tail = .autoval. lst
    if (is_pair (tail)) then
       call uncons (tail, head, tail)
       block
         type(gcroot_t) :: lst_root
         lst_root = lst ! Protect against garbage collections performed by kons.
         retval = recursion (head, tail)
         call lst_root%discard
       end block
    else
       retval = .autoval. right_identity
    end if

  contains

    recursive function recursion (head, lst1) result (retval)
      class(*), intent(in) :: head, lst1
      class(*), allocatable :: retval

      class(*), allocatable :: hd, tl

      if (is_pair (lst1)) then
         call uncons (lst1, hd, tl)
         call kons (head, recursion (hd, tl), retval)
      else
         retval = head
      end if
    end function recursion

  end function reduce_right

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function unfold_with_tail_gen (pred, f, g, seed, tail_gen) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map_proc_t) :: f
    procedure(list_map_proc_t) :: g
    class(*), intent(in) :: seed
    procedure(list_map_proc_t) :: tail_gen
    class(*), allocatable :: lst

    type(gcroot_t) :: retval

    call recursion (.autoval. seed, retval)
    lst = .val. retval

  contains

    recursive subroutine recursion (seed, retval)
      class(*), intent(in) :: seed
      type(gcroot_t), intent(inout) :: retval

      class(*), allocatable :: new_element, new_seed
      type(gcroot_t) :: seed_root, new_element_root

      seed_root = seed          ! Protect the seed.
      if (pred (seed)) then
         call tail_gen (seed, lst)
      else
         call f (seed, new_element)
         new_element_root = new_element ! Protect the new element.
         call g (seed, new_seed)
         call recursion (new_seed, retval)
         retval = cons (new_element, retval)
         call new_element_root%discard
      end if
      call seed_root%discard
    end subroutine recursion

  end function unfold_with_tail_gen

  recursive function unfold_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map_proc_t) :: f
    procedure(list_map_proc_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst

    type(gcroot_t) :: retval

    call recursion (.autoval. seed, retval)
    lst = .val. retval

  contains

    recursive subroutine recursion (seed, retval)
      class(*), intent(in) :: seed
      type(gcroot_t), intent(inout) :: retval

      class(*), allocatable :: new_element, new_seed
      type(gcroot_t) :: seed_root, new_element_root

      seed_root = seed          ! Protect the seed.
      if (pred (seed)) then
         lst = nil
      else
         call f (seed, new_element)
         new_element_root = new_element ! Protect the new element.
         call g (seed, new_seed)
         call recursion (new_seed, retval)
         retval = cons (new_element, retval)
         call new_element_root%discard
      end if
      call seed_root%discard
    end subroutine recursion

  end function unfold_with_nil_tail

  recursive function unfold_right_with_tail (pred, f, g, seed, tail) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map_proc_t) :: f
    procedure(list_map_proc_t) :: g
    class(*), intent(in) :: seed
    class(*), intent(in) :: tail
    class(*), allocatable :: lst

    class(*), allocatable :: new_element, new_seed
    type(gcroot_t) :: current_seed
    type(gcroot_t) :: retval

    retval = tail
    current_seed = seed
    do while (.not. pred (.val. current_seed))
       call f (.val. current_seed, new_element)
       retval = cons (new_element, retval)
       call g (.val. current_seed, new_seed)
       current_seed = new_seed
    end do
    lst = .val. retval
  end function unfold_right_with_tail

  recursive function unfold_right_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map_proc_t) :: f
    procedure(list_map_proc_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst

    lst = unfold_right_with_tail (pred, f, g, seed, nil)
  end function unfold_right_with_nil_tail

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module cons_pairs
