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

module cons_types
  !
  ! Lisp-style CONS-pairs for Fortran.
  !
  ! Please use module cons_lists, rather than this module directly.
  !

  implicit none
  private

  public :: cons_pair_t
  public :: cons_t
  public :: nil_list

  ! A private type representing the CAR-CDR-tuple of a CONS-pair.
  type :: cons_pair_t
     class(*), allocatable :: car
     class(*), allocatable :: cdr
  end type cons_pair_t

  ! A public type that is a NIL-list or a reference to a
  ! CAR-CDR-tuple. The class of a Lisp-like list structure is
  ! `class(cons_t)'.
  type :: cons_t
     class(cons_pair_t), pointer :: p => null ()
  end type cons_t

  ! The canonical NIL-list.
  type(cons_t), parameter :: nil_list = cons_t (null ())

end module cons_types

module cons_procedure_types
  !
  ! Procedured types used by module cons_lists.
  !
  ! Please use module cons_lists, rather than this module directly.
  !

  abstract interface

     recursive subroutine list_foreach_procedure_t (x)
       !
       ! The type of a subroutine passed to list_foreach.
       !
       use :: cons_types
       class(*), intent(in) :: x
     end subroutine list_foreach_procedure_t

     recursive subroutine list_modify_elements_procedure_t (x)
       !
       ! The type of a subroutine passed to list_modify_elements.
       !
       use :: cons_types
       class(*), intent(inout), allocatable :: x
     end subroutine list_modify_elements_procedure_t

     recursive subroutine list_filter_map_procedure_t (x, keep)
       !
       ! The type of a subroutine passed to list_filter_map. If the
       ! value of `x' is to be mapped and kept, set `x' to the mapped
       ! value and `keep' .true.; otherwise set `keep' .false.
       !
       use :: cons_types
       class(*), intent(inout), allocatable :: x
       logical, intent(out) :: keep
     end subroutine list_filter_map_procedure_t

     recursive subroutine list_kons_procedure_t (kar, kdr, kons_result)
       !
       ! The type of the `kons' argument to a fold procedure.
       !
       use :: cons_types
       class(*), intent(in) :: kar, kdr
       class(*), allocatable, intent(out) :: kons_result
     end subroutine list_kons_procedure_t

     recursive function list_predicate1_t (x) result (bool)
       !
       ! For passing one-argument predicates to procedures.
       !
       use :: cons_types
       class(*), intent(in) :: x
       logical :: bool
     end function list_predicate1_t

     recursive function list_predicate2_t (x, y) result (bool)
       !
       ! For passing two-argument predicates to procedures.
       !
       use :: cons_types
       class(*), intent(in) :: x, y
       logical :: bool
     end function list_predicate2_t

     recursive function list_comparison2_t (x, y) result (i)
       !
       ! Return i < 0 if x < y; 0 if x == y; i > 0 if x > y.
       !
       use :: cons_types
       class(*), intent(in) :: x, y
       integer :: i
     end function list_comparison2_t

  end interface

end module cons_procedure_types

module cons_lists
  !
  ! Lisp-style CONS-pairs for Fortran, and a suite of list routines.
  !
  ! The names of routines are based loosely on those in the Scheme
  ! Request for Implementation SRFI-1 (List Library).  See
  ! https://srfi.schemers.org/srfi-1/srfi-1.html
  !
  ! Procedures with `destructive' in their names are the `linear
  ! updating' alternatives: they are allowed to (but NOT required to)
  ! destroy their inputs. They might not work with circular lists as
  ! inputs.
  !
  ! Please keep in mind that the term `list' is used only loosely in
  ! Scheme. The fundamental `list' object types are actually the nil
  ! `list' and the CAR-CDR pair; furthermore, objects other than those
  ! can be (and in this module will be) regarded as degenerate `dotted
  ! lists'.
  !
  ! NOTE: Fortran procedures do not take multiple arguments, the way
  ! Scheme procedures do. However, the list_zip2, list_zip3, etc.,
  ! functions can be used to turn multiple-argument problems into
  ! single-argument problems.
  !

  use :: cons_types
  use :: cons_procedure_types

  implicit none
  private

  public :: cons_t              ! The type of a NIL-list or CONS-pair.
  public :: nil_list            ! The canonical NIL-list.

  public :: is_nil_or_pair      ! Is an object either a NIL-list or CONS-pair?
  public :: is_nil_list         ! Is an object a NIL-list?
  public :: is_cons_pair        ! Is an object a CONS-pair?

  public :: is_not_nil_or_pair  ! Is an object neither a NIL-list nor CONS-pair?
  public :: is_not_nil_list     ! Is an object not a NIL-list?
  public :: is_not_cons_pair    ! Is an object not a CONS-pair?

  public :: list_is_nil         ! Is a cons_t a NIL-list?
  public :: list_is_pair        ! Is a cons_t a CONS-pair?

  ! cons_t_eq(x,y) is like Scheme's `(eq? x y)' for two lists.
  public :: cons_t_eq           ! Are the two cons_t equivalent?

  public :: cons                ! The fundamental CONS-pair constructor.
  public :: uncons              ! The fundamental CONS-pair deconstructor.

  ! Notation: `elem1 ** elem2 ** elem3 ** nil_list'
  public :: operator(**)        ! An infix notation for `list_cons'.
  public :: list_cons           ! CONS assuming the right side is a list.

  public :: set_car             ! Change the CAR of a CONS-pair.
  public :: set_cdr             ! Change the CDR of a CONS-pair.

  public :: cons_t_cast         ! Assume an object is a cons_t.

  public :: list_length      ! The number of CAR elements in a proper or dotted list.
  public :: list_length_plus ! The number of CAR elements in a proper or dotted list, or -1 for a circular list.
  public :: is_proper_list   ! Is an object a list but neither dotted nor circular?
  public :: is_dotted_list   ! Is an object a non-list or a `list' that ends in something other than nil?
  public :: is_circular_list ! Is an object a circular list?
  public :: list_classify    ! Is the object a dotted list? Is it a circular list?

  ! Permutations of car and cdr, for returning elements of a tree.
m4_forloop([n],[1],CADADR_MAX,[m4_length_n_cadadr_public_declarations(n)])dnl

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

  ! Return a numbered element of a list.
  public :: list_ref0       ! Return the ith element, starting at i=0.
  public :: list_ref1       ! Return the ith element, starting at i=1.
  public :: list_refn       ! Return the ith element, starting at i=n.

  public :: list_last       ! Return the last CAR element.
  public :: list_last_pair  ! Return the last CONS-pair of a (possibly dotted) list.

  public :: make_list ! Make a list that is one element value repeated.

  ! Return a list that is a sequence of integers. (The name `IOTA' is
  ! taken from SRFI-1 and was inspired by APL.)
  public :: iota            ! `iota' = the generic procedure.
  public :: iota_given_length
  public :: iota_given_length_start
  public :: iota_given_length_start_step

  public :: circular_list       ! Make a circular list by joining ends.

  ! Make and unmake lists of the given lengths.
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: list[]n
])dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: unlist[]n
])dnl
m4_forloop([n],[1],LISTN_MAX,[dnl
  public :: unlist[]n[]_with_tail
])dnl

  public :: list_reverse          ! Make a reversed copy.
  public :: list_reverse_in_place ! Reverse a list without copying. (The argument must be a cons_t.)
  public :: list_copy             ! Make a copy in the original order.
  public :: list_take             ! Copy the first n CAR elements.
  public :: list_drop             ! Drop the first n CAR elements (by performing n CDR operations).
  public :: list_take_right       ! Roughly, `return the last n elements' (but see SRFI-1).
  public :: list_drop_right       ! Roughly, `drop the last n elements' (but see SRFI-1).
  public :: list_split            ! A combination of list_take and list_drop.
  public :: list_append           ! Concatenate two lists.
  public :: list_append_reverse   ! Concatenate the reverse of one list to another list.
  public :: list_append_in_place  ! Concatenate two lists, without copying.
  public :: list_append_reverse_in_place ! Reverse the first list and then append, without copying.
  public :: list_concatenate      ! Concatenate a list of lists.

  public :: list_destructive_reverse
  public :: list_destructive_take

  ! Zipping: joining the elements of separate lists into a list of
  ! lists.
  public :: list_zip1 ! Box each element of a list in a length-1 list.
m4_forloop([n],[2],ZIP_MAX,[dnl
  public :: list_zip[]n
])dnl

  ! Unzipping: separating the elements of a list of lists into
  ! separate lists.
  public :: list_unzip1 ! Unbox each element of a list of length-1 lists.
m4_forloop([n],[2],ZIP_MAX,[dnl
  public :: list_unzip[]n
])dnl

  ! list_unzip1f is the same as list_unzip1, except as a function
  ! instead of a subroutine.
  public :: list_unzip1f ! Unboxes elements from length-1 lists.

  public :: list_foreach_procedure_t
  public :: list_foreach      ! Call a subroutine on list elements, to produce side effects.
  public :: list_pair_foreach ! Call a subroutine on list pairs, to produce side effects.

  public :: list_map          ! A generic function for mapping element values.
  public :: list_map_in_place ! A generic function for mapping in place.
  public :: list_append_map   ! A generic function for mapping then concatenating.

  ! Call a one-argument subroutine on list elements, to modify (map)
  ! their values.
  public :: list_modify_elements_procedure_t
  public :: list_modify_elements          ! Can be called as `list_map'.
  public :: list_modify_elements_in_place ! Can be called as `list_map_in_place'.
  public :: list_append_modify_elements   ! Can be called as `list_append_map'.

  ! Types for predicates.
  public :: list_predicate1_t ! A predicate taking one argument.
  public :: list_predicate2_t ! A predicate taking two arguments.

  ! Searching.
  public :: list_find       ! Find a list's first element that satisfies a predicate.
  public :: list_find_tail  ! Find a list's first tail whose CAR satisfies a predicate.
  public :: list_take_while ! Keep only initial elements that satisfy a predicate.
  public :: list_drop_while ! Drop initial elements that satisfy a predicate.
  public :: list_span       ! Split where a predicate is first unsatisfied.
  public :: list_break      ! Split where a predicate is first satisfied.
  public :: list_any        ! Do any of the elements satisfy a predicate?
  public :: list_every      ! Do all the elements satisfy a predicate?
  public :: list_index0     ! Return the index (starting at 0) of the first match.
  public :: list_index1     ! Return the index (starting at 1) of the first match.
  public :: list_indexn     ! Return the index (starting at n) of the first match.

  public :: list_equals     ! Test equality of two lists (though actually this is much more general).
  !
  ! FIXME: Add something like list_equals that ignores the end of a
  !        dotted list.
  !

  public :: list_count      ! Count elements that satisfy a predicate.
  public :: list_filter     ! Keep elements that satisfy a predicate.
  public :: list_remove     ! Remove elements that satisfy a predicate.
  public :: list_partition  ! Do both `filter' and `remove' at the same time.
  public :: list_delete     ! Remove elements that satisfy a comparison with a given object.
  public :: list_delete_duplicates ! O(n**2) duplicate-element deletion.
  public :: list_filter_map ! Filter out elements while mapping those kept.
  public :: list_fold       ! `The fundamental list iterator.'
  public :: list_fold_right ! `The fundamental list recursion operator.' (Not for use on VERY long lists.)
  public :: list_pair_fold  ! Like list_fold, but applied to sublists.
  public :: list_pair_fold_right ! Like list_fold_right, but applied to sublists. (Not for use on VERY long lists.)
  public :: list_reduce     ! A variant of list_fold. (See SRFI-1.)
  public :: list_reduce_right ! A variant of list_fold_right. (See SRFI-1. Not for use on VERY long lists.)

  ! `The fundamental recursive list constructor.' See
  ! SRFI-1. Implemented recursively and so not for use to make VERY
  ! long lists. (Lists tens of thousands of pairs long, for instance.)
  public :: list_unfold               ! The generic function.
  public :: list_unfold_with_tail_gen ! A special case of the generic `list_unfold'.
  public :: list_unfold_with_nil_tail ! A special case of the generic `list_unfold'.

  ! `The fundamental iterative list constructor.' See SRFI-1.
  public :: list_unfold_right               ! The generic functions.
  public :: list_unfold_right_with_tail     ! A special case of the generic `list_unfold_right'.
  public :: list_unfold_right_with_nil_tail ! A special case of the generic `list_unfold_right'.

  ! Association lists: lists of key-value CONS-pairs.
  public :: alist_assoc  ! Return the key-value pair with a given key (though actually this is more general).
  public :: alist_cons   ! CONS a key-value pair.
  public :: alist_copy   ! Copy an association list, making copies of the key-value pairs.
  public :: alist_delete ! Delete all entries with a given key (though actually this is more general).

  ! Sorting. (NOTE: Sorting is not included in SRFI-1.)
  public :: list_merge                   ! Merge two sorted lists.
  public :: list_destructive_merge
  public :: list_stable_sort             ! Stable mergesort.
  public :: list_destructive_stable_sort

  ! Overloading of `iota'.
  interface iota
     module procedure iota_given_length
     module procedure iota_given_length_start
     module procedure iota_given_length_start_step
  end interface iota

  ! A right-associative CONSing operator. The right argument must be a
  ! class(cons_t).
  interface operator(**)
     module procedure list_cons
  end interface operator(**)

  ! Overloading of `list_map'.
  interface list_map
     module procedure list_modify_elements
  end interface list_map

  ! Overloading of `list_map_in_place'.
  interface list_map_in_place
     module procedure list_modify_elements_in_place
  end interface list_map_in_place

  ! Overloading of `list_append_map'.
  interface list_append_map
     module procedure list_append_modify_elements
  end interface list_append_map

  ! Overloading of `list_unfold'.
  interface list_unfold
     module procedure list_unfold_with_tail_gen
     module procedure list_unfold_with_nil_tail
  end interface list_unfold

  ! Overloading of `list_unfold_right'.
  interface list_unfold_right
     module procedure list_unfold_right_with_tail
     module procedure list_unfold_right_with_nil_tail
  end interface list_unfold_right

contains

m4_if(DEBUGGING,[true],[dnl
  function integer_cast (obj) result (int)
    class(*), intent(in) :: obj
    integer :: int
    select type (obj)
    type is (integer)
       int = obj
    end select
  end function integer_cast
])dnl
dnl
  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("cons_lists error: ", a)') msg
    error stop
  end subroutine error_abort

  function copy_head (lst) result (lst1)
    class(*), intent(in) :: lst
    type(cons_t) :: lst1

    class(*), allocatable :: head, tail

    call uncons (lst, head, tail)
    lst1 = cons (head, tail)
  end function copy_head

  function is_nil_or_pair (obj) result (is_cons)
    class(*), intent(in) :: obj
    logical is_cons
    select type (obj)
    class is (cons_t)
       is_cons = .true.
    class default
       is_cons = .false.
    end select
  end function is_nil_or_pair

  function is_nil_list (obj) result (is_nil)
    class(*), intent(in) :: obj
    logical is_nil
    select type (obj)
    class is (cons_t)
       is_nil = .not. associated (obj%p)
    class default
       is_nil = .false.
    end select
  end function is_nil_list

  function is_cons_pair (obj) result (is_pair)
    class(*), intent(in) :: obj
    logical is_pair
    select type (obj)
    class is (cons_t)
       is_pair = associated (obj%p)
    class default
       is_pair = .false.
    end select
  end function is_cons_pair

  function is_not_nil_or_pair (obj) result (is_not_cons)
    class(*), intent(in) :: obj
    logical :: is_not_cons
    is_not_cons = .not. is_nil_or_pair (obj)
  end function is_not_nil_or_pair

  function is_not_nil_list (obj) result (is_not_nil)
    class(*), intent(in) :: obj
    logical :: is_not_nil
    is_not_nil = .not. is_nil_list (obj)
  end function is_not_nil_list

  function is_not_cons_pair (obj) result (is_not_pair)
    class(*), intent(in) :: obj
    logical :: is_not_pair
    is_not_pair = .not. is_cons_pair (obj)
  end function is_not_cons_pair
  
  function list_is_nil (lst) result (is_nil)
    class(cons_t), intent(in) :: lst
    logical is_nil
    is_nil = .not. associated (lst%p)
  end function list_is_nil

  function list_is_pair (lst) result (is_pair)
    class(cons_t), intent(in) :: lst
    logical is_pair
    is_pair = associated (lst%p)
  end function list_is_pair

  function cons_t_eq (lst1, lst2) result (eq)
    class(cons_t), intent(in) :: lst1, lst2
    logical :: eq
    if (associated (lst1%p)) then
       eq = associated (lst2%p) .and. associated (lst1%p, lst2%p)
    else
       eq = .not. associated (lst2%p)
    end if
  end function cons_t_eq

  function cons (car_value, cdr_value) result (pair)
    class(*), intent(in) :: car_value
    class(*), intent(in) :: cdr_value
    type(cons_t) :: pair

    type(cons_pair_t) :: car_cdr

    car_cdr%car = car_value
    car_cdr%cdr = cdr_value
    allocate (pair%p, source = car_cdr)
  end function cons

  subroutine uncons (pair, car_value, cdr_value)
    class(*) :: pair
    class(*), allocatable :: car_value, cdr_value

    class(*), allocatable :: car_val, cdr_val

    select type (pair)
    class is (cons_t)
       if (list_is_pair (pair)) then
          car_val = pair%p%car
          cdr_val = pair%p%cdr
       else
          call error_abort ("uncons of nil list")
       end if
    class default
       call error_abort ("uncons of an object with no pairs")
    end select
    car_value = car_val
    cdr_value = cdr_val
  end subroutine uncons

  function list_cons (car_value, cdr_value) result (pair)
    class(*), intent(in) :: car_value
    class(cons_t), intent(in) :: cdr_value
    type(cons_t) :: pair

    type(cons_pair_t) :: car_cdr

    car_cdr%car = car_value
    car_cdr%cdr = cdr_value
    allocate (pair%p, source = car_cdr)
  end function list_cons

  subroutine set_car (pair, car_value)
    class(cons_t) :: pair
    class(*), intent(in) :: car_value
    if (list_is_pair (pair)) then
       pair%p%car = car_value
    else
       call error_abort ("set_car of nil list")
    end if
  end subroutine set_car

  subroutine set_cdr (pair, cdr_value)
    class(cons_t) :: pair
    class(*), intent(in) :: cdr_value
    if (list_is_pair (pair)) then
       pair%p%cdr = cdr_value
    else
       call error_abort ("set_cdr of nil list")
    end if
  end subroutine set_cdr

  function cons_t_cast (obj) result (lst)
    !
    ! Cast to cons_t, if possible.
    !
    class(*), intent(in) :: obj
    type(cons_t) :: lst
    select type (obj)
    class is (cons_t)
       lst = obj
    class default
       call error_abort ("cons_t_cast of an incompatible object")
    end select
  end function cons_t_cast

  function list_length (lst) result (length)
    class(*), intent(in) :: lst
    integer :: length

    class(*), allocatable :: tail

    length = 0
    tail = lst
    do while (is_cons_pair (tail))
       length = length + 1
       tail = cdr (tail)
    end do
  end function list_length

  function list_length_plus (lst) result (length)
    !
    ! A variant of list_length that returns -1 if the list is
    ! circular.
    !
    class(*), intent(in) :: lst
    integer :: length

    ! Detect circularity by having a `lead' reference move through the
    ! list at a higher rate than a `lag' reference. In a circular
    ! list, eventually `lead' will catch up with `lag'.
    class(*), allocatable :: lead
    class(*), allocatable :: lag

    logical :: done

    length = 0
    lead = lst
    lag = lst
    done = .false.
    do while (.not. done .and. is_cons_pair (lead))
       lead = cdr (lead)
       length = length + 1
       if (is_cons_pair (lead)) then
          lead = cdr (lead)
          lag = cdr (lag)
          length = length + 1
          select type (lead)
          class is (cons_t)
             select type (lag)
             class is (cons_t)
                if (cons_t_eq (lead, lag)) then
                   length = -1
                   done = .true.
                end if
             end select
          end select
       else
          done = .true.
       end if
    end do
  end function list_length_plus

  function is_proper_list (obj) result (is_proper)
    class(*), intent(in) :: obj
    logical :: is_proper

    logical :: is_dot
    logical :: is_circ

    call list_classify (obj, is_dot, is_circ)
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

    call list_classify (obj, is_dot, is_circ)
    is_dotted = is_dot
  end function is_dotted_list

  function is_circular_list (obj) result (is_circular)
    class(*), intent(in) :: obj
    logical :: is_circular

    logical :: is_dot
    logical :: is_circ

    call list_classify (obj, is_dot, is_circ)
    is_circular = is_circ
  end function is_circular_list

  subroutine list_classify (obj, is_dotted, is_circular)
    !
    ! An object that is not a cons_t is considered dotted.
    !
    ! Dotted and circular are mutually exclusive.
    !
    ! If an object is neither dotted nor circular, then it is a proper
    ! list.
    !
    class(*) :: obj
    logical :: is_dotted
    logical :: is_circular

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
       if (.not. is_cons_pair (lead)) then
          is_dot = .not. is_nil_list (lead)
          done = .true.
       else
          lead = cdr (lead)
          if (.not. is_cons_pair (lead)) then
             is_dot = .not. is_nil_list (lead)
             done = .true.
          else
             lead = cdr (lead)
             lag = cdr (lag)
             select type (lead)
             class is (cons_t)
                select type (lag)
                class is (cons_t)
                   if (cons_t_eq (lead, lag)) then
                      is_circ = .true.
                      done = .true.
                   end if
                end select
             end select
          end if
       end if
    end do

    is_dotted = is_dot
    is_circular = is_circ
  end subroutine list_classify

  function car (pair) result (element)
    class(*), intent(in) :: pair
    class(*), allocatable :: element
    select type (pair)
    class is (cons_t)
       if (list_is_pair (pair)) then
          element = pair%p%car
       else
          call error_abort ("car of nil list")
       end if
    class default
       call error_abort ("car of an object with no pairs")
    end select
  end function car

  function cdr (pair) result (element)
    class(*), intent(in) :: pair
    class(*), allocatable :: element
    select type (pair)
    class is (cons_t)
       if (list_is_pair (pair)) then
          element = pair%p%cdr
       else
          call error_abort ("cdr of nil list")
       end if
    class default
       call error_abort ("cdr of an object with no pairs")
    end select
  end function cdr

m4_forloop([n],[2],CADADR_MAX,[m4_length_n_cadadr_definitions(n)])dnl
dnl
  function first (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (lst)
  end function first

  function second (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (lst))
  end function second

  function third (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (lst)))
  end function third

  function fourth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (lst))))
  end function fourth

  function fifth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (cdr (lst)))))
  end function fifth

  function sixth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (cdr (cdr (lst))))))
  end function sixth

  function seventh (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (cdr (cdr (cdr (lst)))))))
  end function seventh

  function eighth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (lst))))))))
  end function eighth

  function ninth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (lst)))))))))
  end function ninth

  function tenth (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (lst))))))))))
  end function tenth

  function list_ref0 (lst, i) result (element)
    !
    ! Return the ith element of lst, starting at i=0.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: i
    class(*), allocatable :: element
    element = list_refn (lst, i, 0)
  end function list_ref0

  function list_ref1 (lst, i) result (element)
    !
    ! Return the ith element of lst, starting at i=1.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: i
    class(*), allocatable :: element
    element = list_refn (lst, i, 1)
  end function list_ref1

  function list_refn (lst, i, n) result (element)
    !
    ! Return the ith element of lst, starting at i=n.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: i, n
    class(*), allocatable :: element

    class(*), allocatable :: tail
    integer :: j

    tail = lst
    j = n
    do while (j < i)
       tail = cdr (tail)
       j = j + 1
    end do
    element = car (tail)
  end function list_refn

  function list_last (lst) result (element)
    class(*), intent(in) :: lst
    class(*), allocatable :: element

    type(cons_t) :: last_pair
    class(*), allocatable :: x, y

    last_pair = list_last_pair (lst)
    call uncons (last_pair, x, y)
    element = x
  end function list_last

  function list_last_pair (lst) result (last_pair)
    class(*), intent(in) :: lst
    type(cons_t) :: last_pair

    type(cons_t) :: lst1
    class(*), allocatable :: tail

    select type (lst)
    class is (cons_t)
       lst1 = lst
       if (list_is_pair (lst1)) then
          tail = cdr (lst1)
          do while (is_cons_pair (tail))
             lst1 = cons_t_cast (tail)
             tail = cdr (lst1)
          end do
          last_pair = lst1
       else
          call error_abort ("list_last_pair of empty list")
       end if
    class default
       call error_abort ("list_last_pair of an object with no pairs")
    end select
  end function list_last_pair

  function make_list (length, fill_value) result (lst)
    integer, intent(in) :: length
    class(*), intent(in) :: fill_value
    type(cons_t) :: lst

    integer :: i

    lst = nil_list
    do i = 1, length
       lst = cons (fill_value, lst)
    end do
  end function make_list

  function iota_given_length (length) result (lst)
    integer, intent(in) :: length
    class(cons_t), allocatable :: lst
    lst = iota_given_length_start_step (length, 0, 1)
  end function iota_given_length

  function iota_given_length_start (length, start) result (lst)
    integer, intent(in) :: length, start
    class(cons_t), allocatable :: lst
    lst = iota_given_length_start_step (length, start, 1)
  end function iota_given_length_start

  function iota_given_length_start_step (length, start, step) result (lst)
    integer, intent(in) :: length, start, step
    class(cons_t), allocatable :: lst

    integer :: i, n

    !
    ! Go through the sequence backwards, so we will not have to
    ! reverse the resulting list.
    !
    n = start + ((length - 1) * step)
    lst = nil_list
    do i = 1, length
       lst = cons (n, lst)
       n = n - step
    end do
  end function iota_given_length_start_step

  function circular_list (lst) result (clst)
    !
    ! Make a circular list with the same CARs as lst.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: clst

    type(cons_t) :: last

    select type (lst)
    class is (cons_t)
       if (list_is_pair (lst)) then
          clst = list_reverse (lst)
          last = clst
          call list_reverse_in_place (clst)
          call set_cdr (last, clst)
       else
          call error_abort ("circular_list of a nil list")
       end if
    class default
       call error_abort ("circular_list of an object with no pairs")
    end select
  end function circular_list
dnl
m4_forloop([n],[1],LISTN_MAX,[
  function list[]n (obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
])obj[]k])) result (lst)
m4_forloop([k],[1],n,[dnl
    class(*), intent(in) :: obj[]k
])dnl
    type(cons_t) :: lst

    lst = obj[]n ** nil_list
dnl
m4_forloop([k],[2],n,[    lst = obj[]m4_eval(n - k + 1) ** lst
])dnl
  end function list[]n
])dnl
dnl
m4_forloop([n],[1],LISTN_MAX,[
  subroutine unlist[]n (lst, obj1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
])obj[]k]))
    !
    ! This subroutine `unlists' the n elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: obj[]k
])dnl

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
dnl
m4_forloop([k],[2],n,[dnl
    call uncons (tail, head, tail)
    obj[]k = head
])dnl
    if (is_cons_pair (tail)) then
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
    class(cons_t) :: lst
m4_forloop([k],[1],n,[dnl
    class(*), allocatable :: obj[]k
])dnl
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
dnl
m4_forloop([k],[2],n,[    call uncons (tl, hd, tl)
    obj[]k = hd
])dnl
    tail = tl
  end subroutine unlist[]n[]_with_tail
])dnl

  function list_reverse (lst) result (lst_r)
    !
    ! The final CDR of any dotted list (including any non-cons_t
    ! object) is dropped.  The result is a cons_t.
    !
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    class(*), allocatable :: tail
    
    lst_r = nil_list
    tail = lst
    do while (is_cons_pair (tail))
       lst_r = cons (car (tail), lst_r)
       tail = cdr (tail)
    end do
  end function list_reverse

  function list_destructive_reverse (lst) result (lst_r)
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    type(cons_t) :: lst1

    select type (lst)
    class is (cons_t)
       if (list_is_pair (lst)) then
          lst1 = copy_head (lst)
          call list_reverse_in_place (lst1)
          lst_r = lst1
       else
          lst_r = nil_list
       end if
    class default
       lst_r = nil_list
    end select
  end function list_destructive_reverse

  subroutine list_reverse_in_place (lst)
    type(cons_t), intent(inout) :: lst

    type(cons_t) :: lst_r
    type(cons_t) :: tail

    lst_r = nil_list
    do while (list_is_pair (lst))
       tail = cons_t_cast (cdr (lst))
       call set_cdr (lst, lst_r)
       lst_r = lst
       lst = tail
    end do
    lst = lst_r
  end subroutine list_reverse_in_place

  function list_copy (lst) result (lst_c)
    !
    ! Because lst may be a degenerate dotted list, the result need not
    ! be a cons_t.
    !
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_c

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    
    select type (lst)
    class is (cons_t)
       if (list_is_nil (lst)) then
          lst_c = nil_list
       else
          call uncons (lst, head, tail)
          cursor = cons (head, tail)
          lst_c = cursor
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             new_pair = cons (head, tail)
             call set_cdr (cursor, new_pair)
             cursor = new_pair
          end do
       end if
    class default
       lst_c = lst
    end select
  end function list_copy

  function list_take (lst, n) result (lst_t)
    !
    ! list_take *copies* the first n `CAR' elements, and returns a
    ! list.
    !
    ! lst may be dotted or circular. If it is a degenerate dotted
    ! list, then n must not be positive.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_t

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    integer :: i

    if (n <= 0) then
       lst_t = nil_list
    else
       select type (lst)
       class is (cons_t)
          if (list_is_nil (lst)) then
             call error_abort ("positive list_take of a nil list")
          else
             call uncons (lst, head, tail)
             lst_t = cons (head, tail)
             cursor = lst_t
             i = n - 1
             do while (0 < i .and. is_cons_pair (tail))
                call uncons (tail, head, tail)
                new_pair = cons (head, tail)
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                i = i - 1
             end do
             if (i == 0) then
                call set_cdr (cursor, nil_list)
             else
                call error_abort ("list_take of a list that is too short")
             end if
          end if
       class default
          call error_abort ("positive list_take of an object with no pairs")
       end select
    end if
  end function list_take

  function list_destructive_take (lst, n) result (lst_t)
    !
    ! This will not work as you might expect, if the input is a
    ! circular list.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_t

    type(cons_t) :: lst1

    if (n <= 0) then
       lst_t = nil_list
    else if (.not. is_cons_pair (lst)) then
       lst_t = nil_list
    else
       lst1 = copy_head (lst)
       call set_cdr (cons_t_cast (list_drop (lst1, n - 1)), nil_list)
       lst_t = lst1
    end if
  end function list_destructive_take

  function list_drop (lst, n) result (obj)
    !
    ! list_drop does *not* copy the elements it `keeps'; it is
    ! equivalent to repeating CDR n times.
    !
    ! lst may be dotted or circular. The result need not be a cons_t.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: obj

    integer :: i

    obj = lst
    do i = 1, n
       if (is_cons_pair (obj)) then
          obj = cdr (obj)
       else
          call error_abort ("list_drop of a list that is too short")
       end if
    end do
  end function list_drop

  function list_take_right (lst, n) result (obj)
    !
    ! The result might not be a cons_t.
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: obj

    class(*), allocatable :: lead
    class(*), allocatable :: lag

    lag = lst
    lead = list_drop (lst, n)
    do while (is_cons_pair (lead))
       lag = cdr (lag)
       lead = cdr (lead)
    end do
    obj = lag
  end function list_take_right

  function list_drop_right (lst, n) result (lst_dr)
    !
    ! list_drop_right *copies* the elements. The result is a cons_t.
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_dr
    lst_dr = list_take (lst, list_length (lst) - n)
  end function list_drop_right

  subroutine list_split (lst, n, lst_left, lst_right)
    !
    ! If n is positive, then lst must be a CONS-pair.
    !
    ! lst_left will be a cons_t, but lst_right need not be.
    !
    class(*) :: lst
    integer :: n
    type(cons_t) :: lst_left
    class(*), allocatable :: lst_right

    type(cons_t) :: lst_t
    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    integer :: i

    if (n <= 0) then
       lst_left = nil_list
       lst_right = lst
    else
       select type (lst)
       class is (cons_t)
          if (list_is_nil (lst)) then
             call error_abort ("positive list_split of a nil list")
          else
             call uncons (lst, head, tail)
             lst_t = cons (head, tail)
             cursor = lst_t
             i = n - 1
             do while (0 < i .and. is_cons_pair (tail))
                call uncons (tail, head, tail)
                new_pair = cons (head, tail)
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                i = i - 1
             end do
             if (i == 0) then
                call set_cdr (cursor, nil_list)
             else
                call error_abort ("list_split of a list that is too short")
             end if
             lst_left = lst_t
             lst_right = tail
          end if
       class default
          call error_abort ("positive list_split of an object with no pairs")
       end select
    end if
  end subroutine list_split

  function list_append (lst1, lst2) result (lst_a)
    !
    ! The tail of the result is shared with lst2. The CAR elements of
    ! lst1 are copied; the last CDR of lst1 is dropped.
    !
    ! The result need not be a cons_t.
    !
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_a

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    type(cons_t) :: new_lst

    select type (lst1)
    class is (cons_t)
       if (list_is_nil (lst1)) then
          lst_a = lst2
       else
          call uncons (lst1, head, tail)
          new_lst = cons (head, tail)
          cursor = new_lst
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             new_pair = cons (head, tail)
             call set_cdr (cursor, new_pair)
             cursor = new_pair
          end do
          call set_cdr (cursor, lst2)
          lst_a = new_lst
       end if
    class default
       lst_a = lst2
    end select
  end function list_append

  function list_append_reverse (lst1, lst2) result (lst_ar)
    !
    ! The tail of the result is shared with lst2. The CAR elements of
    ! lst1 are copied; the last CDR of the reverse of lst1 is dropped.
    !
    ! The result need not be a cons_t.
    !
    class(*) :: lst1, lst2
    class(*), allocatable :: lst_ar

    class(*), allocatable :: head
    class(*), allocatable :: tail

    lst_ar = lst2
    select type (lst1)
    class is (cons_t)
       lst_ar = lst2
       tail = lst1
       do while (is_cons_pair (tail))
          call uncons (tail, head, tail)
          lst_ar = cons (head, lst_ar)
       end do
    end select
  end function list_append_reverse

  subroutine list_append_in_place (lst1, lst2)
    !
    ! lst1 must be a non-empty, non-circular list.
    !
    type(cons_t) :: lst1
    class(*) :: lst2
    if (list_is_nil (lst1)) then
       call error_abort ("list_append_in_place to an empty list")
    else
       call set_cdr (list_last_pair (lst1), lst2)
    end if
  end subroutine list_append_in_place

  subroutine list_append_reverse_in_place (lst1, lst2)
    !
    ! lst1 must be a non-empty, non-circular list.
    !
    type(cons_t) :: lst1
    class(*) :: lst2
    if (list_is_nil (lst1)) then
       call error_abort ("list_append_reverse_in_place to an empty list")
    else
       call list_reverse_in_place (lst1)
       call list_append_in_place (lst1, lst2)
    end if
  end subroutine list_append_reverse_in_place

  function list_concatenate (lists) result (lst_concat)
    !
    ! The result need not be a cons_t.
    !
    ! If lists is nil, then the result is a nil list.
    !
    class(cons_t), intent(in) :: lists
    class(*), allocatable :: lst_concat

    type(cons_t) :: lists_r
    class(*), allocatable :: tail

    if (list_is_nil (lists)) then
       lst_concat = nil_list
    else
       lists_r = list_reverse (lists)
       lst_concat = car (lists_r)
       tail = cdr (lists_r)
       do while (is_cons_pair (tail))
          lst_concat = list_append (car (tail), lst_concat)
          tail = cdr (tail)
       end do
    end if
  end function list_concatenate
dnl
m4_forloop([n],[1],ZIP_MAX,[
  function list_zip[]n (lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
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

    ! Using is_cons_pair rather than is_nil_list lets us handle
    ! degenerate dotted lists at the same time as nil lists.
    if (.not. is_cons_pair (lst1)) then
m4_forloop([k],[1],n,[dnl
       lst_z = nil_list
m4_if(k,n,[dnl
    else
],[dnl
    else if (.not. is_cons_pair (lst[]m4_eval(k + 1))) then
])dnl
])dnl
dnl
m4_forloop([k],[1],n,[dnl
       call uncons (lst[]k, head[]k, tail[]k)
])dnl
       row = nil_list
m4_forloop([k],[1],n,[dnl
       row = head[]m4_eval(n - k + 1) ** row
])dnl
       lst_z = row ** nil_list
       cursor = lst_z
       if (.not. is_cons_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
          continue
m4_if(k,n,[dnl
       else
],[dnl
       else if (.not. is_cons_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          done = .false.
          do while (.not. done)
m4_forloop([k],[1],n,[dnl
             call uncons (tail[]k, head[]k, tail[]k)
])dnl
             row = nil_list
m4_forloop([k],[1],n,[dnl
             row = head[]m4_eval(n - k + 1) ** row
])dnl
             new_pair = row ** nil_list
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (.not. is_cons_pair (tail1)) then
m4_forloop([k],[1],n,[dnl
                done = .true.
m4_if(k,n,[dnl
             end if
],[dnl
             else if (.not. is_cons_pair (tail[]m4_eval(k + 1))) then
])dnl
])dnl
          end do
       end if
    end if
  end function list_zip[]n
])dnl
dnl
m4_forloop([n],[1],ZIP_MAX,[
  subroutine list_unzip[]n (lst_zipped, lst1[]m4_forloop([k],[2],n,[, m4_if(m4_eval(k % 10),[1],[&
])lst[]k]))
    class(*) :: lst_zipped
m4_forloop([k],[1],n,[dnl
    type(cons_t) :: lst[]k
])dnl

m4_forloop([k],[1],n,[dnl
    type(cons_t) :: cursor[]k
])dnl

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
m4_forloop([k],[1],n,[dnl
          lst[]k = nil_list
])dnl
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
m4_forloop([k],[1],n,[dnl
          call uncons (head_zipped, head, tl)
          lst[]k = head ** nil_list
          cursor[]k = lst[]k
          head_zipped = cons_t_cast (tl)
])dnl
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
m4_forloop([k],[1],n,[dnl
m4_if(k,n,[dnl
             head = car (head_zipped)
],[dnl
             call uncons (head_zipped, head, tl)
])dnl
             new_pair = head ** nil_list
             call set_cdr (cursor[]k, new_pair)
             cursor[]k = new_pair
m4_if(k,n,[],[dnl
             head_zipped = cons_t_cast (tl)
])dnl
])dnl
          end do
       end if
    class default
m4_forloop([k],[1],n,[dnl
       lst[]k = nil_list
])dnl
    end select
  end subroutine list_unzip[]n
])dnl

  function list_unzip1f (lst_zipped) result (lst)
    class(*), intent(in) :: lst_zipped
    type(cons_t) :: lst
    call list_unzip1 (lst_zipped, lst)
  end function list_unzip1f

  recursive subroutine list_foreach (subr, lst)
    !
    ! Run a subroutine on list elements, for the sake of side effects
    ! (such as printing). The work is guaranteed to be done in list
    ! order.
    !
    procedure(list_foreach_procedure_t) :: subr
    class(*), intent(in) :: lst

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    do while (is_cons_pair (tail))
       call uncons (tail, head, tail)
       call subr (head)
    end do
  end subroutine list_foreach

  recursive subroutine list_pair_foreach (subr, lst)
    !
    ! Run a subroutine on list pairs, for the sake of side effects
    ! (such as printing). The work is guaranteed to be done in list
    ! order. The application may reliably apply set_cdr to the pairs
    ! it is given.
    !
    procedure(list_foreach_procedure_t) :: subr
    class(*), intent(in) :: lst

    class(*), allocatable :: lst1
    class(*), allocatable :: tail

    lst1 = lst
    do while (is_cons_pair (lst1))
       tail = cdr (lst1)
       call subr (lst1)
       lst1 = tail
    end do
  end subroutine list_pair_foreach

  recursive function list_modify_elements (subr, lst) result (lst_m)
    !
    ! Modify the elements of a list, using a subroutine to map the
    ! individual elements. The work is guaranteed to be done in list
    ! order.
    !
    ! (This is like SRFI-1's `map-in-order'.)
    !
    ! If lst is a dotted list, its final CDR is retained. The return
    ! value thus might not be a cons_t.
    !
    procedure(list_modify_elements_procedure_t) :: subr
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_m

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair

    if (is_cons_pair (lst)) then
       call uncons (lst, head, tail)
       call subr (head)
       cursor = cons (head, tail)
       lst_m = cursor
       do while (is_cons_pair (tail))
          call uncons (tail, head, tail)
          call subr (head)
          new_pair = cons (head, tail)
          call set_cdr (cursor, new_pair)
          cursor = new_pair
       end do
    else
       lst_m = lst
    end if
  end function list_modify_elements

  recursive subroutine list_modify_elements_in_place (subr, lst)
    !
    ! Modify the elements of a list, in place, using a subroutine to
    ! map the individual elements. The work is guaranteed to be done
    ! in list order.
    !
    ! If lst is a dotted list, its final CDR is left unmodified.
    !
    procedure(list_modify_elements_procedure_t) :: subr
    class(*), intent(in) :: lst

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: lst1
    
    tail = lst
    do while (is_cons_pair (tail))
       lst1 = cons_t_cast (tail)
       call uncons (lst1, head, tail)
       call subr (head)
       call set_car (lst1, head)
    end do
  end subroutine list_modify_elements_in_place

  subroutine list_append_modify_elements__get_next (subr, lst, next, tail)
    !
    ! Get the next element that is not a nil list.
    !
    ! (To avoid having a Fortran compiler generate a trampoline, this
    ! subroutine is lambda-lifted rather than nested within
    ! list_append_modify_elements.)
    !
    procedure(list_modify_elements_procedure_t) :: subr
    class(*) :: lst
    class(*), allocatable :: next
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    call uncons (lst, hd, tl)
    call subr (hd)
    do while (is_nil_list (hd) .and. is_cons_pair (tl))
       call uncons (tl, hd, tl)
       call subr (hd)
    end do
    next = hd
    tail = tl
  end subroutine list_append_modify_elements__get_next

  recursive function list_append_modify_elements (subr, lst) result (lst_am)
    !
    ! Modify the elements of a list, using a subroutine to map the
    ! individual elements. The outputs should be lists, and they will
    ! be appended to each other.
    !
    ! (This is like SRFI-1's `append-map', but calling a subroutine
    ! instead of a function for each element.)
    !
    ! If lst is a dotted list, its final CDR is ignored.
    !
    procedure(list_modify_elements_procedure_t) :: subr
    class(*), intent(in) :: lst
    type(cons_t) :: lst_am

    class(*), allocatable :: tail
    class(*), allocatable :: lst_start
    class(*), allocatable :: lst_part
    type(cons_t) :: lst1
    type(cons_t) :: cursor

    lst_am = nil_list
    if (is_cons_pair (lst)) then
       call list_append_modify_elements__get_next (subr, lst, lst_start, tail)
       if (is_cons_pair (lst_start)) then
          lst1 = cons_t_cast (lst_start)
          lst_am = lst1
          cursor = list_last_pair (lst1)
          do while (is_cons_pair (tail))
             call list_append_modify_elements__get_next (subr, tail, lst_part, tail)
             select type (lst_part)
             class is (cons_t)
                lst1 = lst_part
                call set_cdr (cursor, lst1)
                cursor = list_last_pair (lst1)
             class default
                call error_abort ("list_append_modify_elements with a non-list element")
             end select
          end do
       end if
    end if
  end function list_append_modify_elements

  recursive subroutine list_find (pred, lst, match_found, match)
    !
    ! If `match_found' is set to .false., then `match' is left unchanged.
    !
    procedure(list_predicate1_t) :: pred
    class(*) :: lst
    logical, intent(out) :: match_found
    class(*), allocatable :: match

    class(*), allocatable :: tail
    class(*), allocatable :: element

    match_found = .false.
    tail = lst
    do while (.not. match_found .and. is_cons_pair (tail))
       element = car (tail)
       if (pred (element)) then
          match_found = .true.
          match = element
       else
          tail = cdr (tail)
       end if
    end do
  end subroutine list_find

  recursive subroutine list_find_tail (pred, lst, match_found, match)
    !
    ! If `match_found' is set to .false., then `match' is left unchanged.
    !
    procedure(list_predicate1_t) :: pred
    class(*) :: lst
    logical, intent(out) :: match_found
    class(*), allocatable :: match

    class(*), allocatable :: tail

    match_found = .false.
    tail = lst
    do while (.not. match_found .and. is_cons_pair (tail))
       if (pred (car (tail))) then
          match_found = .true.
          match = tail
       else
          tail = cdr (tail)
       end if
    end do
  end subroutine list_find_tail

  recursive function list_take_while (pred, lst) result (match)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    type(cons_t) :: match

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    logical :: match_found

    match = nil_list
    if (is_cons_pair (lst)) then
       call uncons (lst, head, tail)
       if (pred (head)) then
          cursor = head ** nil_list
          match = cursor
          match_found = .false.
          do while (.not. match_found .and. is_cons_pair (tail))
             call uncons (tail, head, tail)
             if (pred (head)) then
                new_pair = head ** nil_list
                call set_cdr (cursor, new_pair)
                cursor = new_pair
             else
                match_found = .true.
             end if
          end do
       end if
    end if
  end function list_take_while

  recursive function list_drop_while (pred, lst) result (match)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: match

    class(*), allocatable :: tail
    logical :: match_found

    match_found = .false.
    tail = lst
    do while (.not. match_found .and. is_cons_pair (tail))
       if (pred (car (tail))) then
          tail = cdr (tail)
       else
          match_found = .true.
       end if
    end do
    match = tail
  end function list_drop_while

  recursive subroutine list_span (pred, lst, lst_initial, lst_rest)
    procedure(list_predicate1_t) :: pred
    class(*) :: lst
    type(cons_t) :: lst_initial
    class(*), allocatable :: lst_rest

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    type(cons_t) :: initial
    class(*), allocatable :: rest
    logical :: match_found

    initial = nil_list
    rest = lst
    if (is_cons_pair (lst)) then
       call uncons (lst, head, tail)
       if (pred (head)) then
          cursor = head ** nil_list
          rest = tail
          initial = cursor
          match_found = .false.
          do while (.not. match_found .and. is_cons_pair (tail))
             call uncons (tail, head, tail)
             if (pred (head)) then
                new_pair = head ** nil_list
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                rest = tail
             else
                match_found = .true.
             end if
          end do
       end if
    end if
    lst_initial = initial
    lst_rest = rest
  end subroutine list_span

  recursive subroutine list_break (pred, lst, lst_initial, lst_rest)
    procedure(list_predicate1_t) :: pred
    class(*) :: lst
    type(cons_t) :: lst_initial
    class(*), allocatable :: lst_rest

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    type(cons_t) :: initial
    class(*), allocatable :: rest
    logical :: match_found

    initial = nil_list
    rest = lst
    if (is_cons_pair (lst)) then
       call uncons (lst, head, tail)
       if (.not. pred (head)) then
          cursor = head ** nil_list
          rest = tail
          initial = cursor
          match_found = .false.
          do while (.not. match_found .and. is_cons_pair (tail))
             call uncons (tail, head, tail)
             if (.not. pred (head)) then
                new_pair = head ** nil_list
                call set_cdr (cursor, new_pair)
                cursor = new_pair
                rest = tail
             else
                match_found = .true.
             end if
          end do
       end if
    end if
    lst_initial = initial
    lst_rest = rest
  end subroutine list_break

  recursive function list_any (pred, lst) result (match_found)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    logical :: match_found

    class(*), allocatable :: tail
    logical :: found

    found = .false.
    tail = lst
    do while (.not. found .and. is_cons_pair (tail))
       if (pred (car (tail))) then
          found = .true.
       else
          tail = cdr (tail)
       end if
    end do
    match_found = found
  end function list_any

  recursive function list_every (pred, lst) result (mismatch_found)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    logical :: mismatch_found

    class(*), allocatable :: tail
    logical :: bool

    bool = .false.
    tail = lst
    do while (.not. bool .and. is_cons_pair (tail))
       if (pred (car (tail))) then
          tail = cdr (tail)
       else
          bool = .true.
       end if
    end do
    mismatch_found = .not. bool
  end function list_every

  recursive function list_index0 (pred, lst) result (index)
    !
    ! Returns -1 if there is no match.
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    integer :: index
    index = list_indexn (pred, lst, 0)
  end function list_index0

  recursive function list_index1 (pred, lst) result (index)
    !
    ! Returns 0 if there is no match.
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    integer :: index
    index = list_indexn (pred, lst, 1)
  end function list_index1

  recursive function list_indexn (pred, lst, n) result (index)
    !
    ! Returns n - 1 if there is no match.
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    integer :: index

    class(*), allocatable :: tail
    integer :: i
    logical :: found

    i = n
    found = .false.
    tail = lst
    do while (.not. found .and. is_cons_pair (tail))
       if (pred (car (tail))) then
          found = .true.
       else
          i = i + 1
          tail = cdr (tail)
       end if
    end do
    if (found) then
       index = i
    else
       index = n - 1
    end if
  end function list_indexn

  recursive function list_equals (pred, lst1, lst2) result (bool)
    !
    ! In the call
    !
    !    list_equals (pred, lst1, lst2)
    !
    ! pred is applied with an element of lst1 as its first argument
    ! and an element of lst2 as its second argument.
    !
    ! The current implementation does not handle circular lists.
    !
    ! It is an error if lst1 or lst2 is a dotted list.
    !
    ! NOTE: THE NAME list_equals IS A MISNOMER. The function is really
    !       application of ANY predicate to all the element pairs.
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst1, lst2
    logical :: bool

    type(cons_t) :: p, q
    class(*), allocatable :: p_hd, p_tl, q_hd, q_tl

    bool = .true.
    select type (lst1)
    class is (cons_t)
       select type (lst2)
       class is (cons_t)
          p = lst1
          q = lst2
          do while (bool .and. list_is_pair (p))
             if (list_is_nil (q)) then
                bool = .false.
             else
                call uncons (p, p_hd, p_tl)
                call uncons (q, q_hd, q_tl)
                if (.not. pred (p_hd, q_hd)) then
                   bool = .false.
                else
                   select type (p_tl)
                   class is (cons_t)
                      p = p_tl
                   class default
                      call error_abort ("first argument to list_equals is a dotted list")
                   end select
                   select type (q_tl)
                   class is (cons_t)
                      q = q_tl
                   class default
                      call error_abort ("second argument to list_equals is a dotted list")
                   end select
                end if
             end if
          end do
          if (.not. list_is_nil (q)) then
             bool = .false.
          end if
       class default
          call error_abort ("second argument to list_equals is not a cons_t")
       end select
    class default
       call error_abort ("first argument to list_equals is not a cons_t")
    end select
  end function list_equals

  recursive function list_count (pred, lst) result (count)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    integer :: count

    class(*), allocatable :: head
    class(*), allocatable :: tail
    integer :: n

    n = 0
    tail = lst
    do while (is_cons_pair (tail))
       call uncons (tail, head, tail)
       if (pred (head)) then
          n = n + 1
       end if
    end do
    count = n
  end function list_count

  recursive function skip_satisfying_elements (pred, lst) result (next)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: next

    class(*), allocatable :: p
    logical :: found_end_or_unsatisfying_element

    p = lst
    found_end_or_unsatisfying_element = .false.
    do while (.not. found_end_or_unsatisfying_element)
       if (.not. is_cons_pair (p)) then
          found_end_or_unsatisfying_element = .true.
       else if (.not. pred (car (p))) then
          found_end_or_unsatisfying_element = .true.
       else
          p = cdr (p)
       end if
    end do
    next = p
  end function skip_satisfying_elements

  recursive function skip_unsatisfying_elements (pred, lst) result (next)
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: next

    class(*), allocatable :: p
    logical :: found_end_or_satisfying_element

    p = lst
    found_end_or_satisfying_element = .false.
    do while (.not. found_end_or_satisfying_element)
       if (.not. is_cons_pair (p)) then
          found_end_or_satisfying_element = .true.
       else if (pred (car (p))) then
          found_end_or_satisfying_element = .true.
       else
          p = cdr (p)
       end if
    end do
    next = p
  end function skip_unsatisfying_elements

  subroutine copy_list_segment (from, to, segment, last_pair)
    class(*), intent(in) :: from
    class(*), intent(in) :: to
    type(cons_t) :: segment
    type(cons_t) :: last_pair

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair

    call uncons (from, head, tail)
    cursor = cons (head, nil_list)
    segment = cursor
    do while (.not. cons_t_eq (cons_t_cast (tail), cons_t_cast (to)))
       call uncons (tail, head, tail)
       new_pair = cons (head, nil_list)
       call set_cdr (cursor, new_pair)
       cursor = new_pair
    end do
    last_pair = cursor
  end subroutine copy_list_segment

  recursive function list_filter (pred, lst) result (lst_f)
    !
    ! This implementation tries to share the longest possible tail
    ! with the original.
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_f

    class(*), allocatable :: retval
    class(*), allocatable :: current_position
    class(*), allocatable :: lookahead
    type(cons_t) :: segment
    type(cons_t) :: cursor, next_cursor
    logical :: done

    current_position = skip_unsatisfying_elements (pred, lst)
    if (.not. is_cons_pair (current_position)) then
       ! There are no elements that satisfy the predicate.
       retval = current_position
    else
       lookahead = skip_satisfying_elements (pred, cdr (current_position))
       if (.not. is_cons_pair (lookahead)) then
          ! A tail of the original is the entire result.
          retval = current_position
       else
          ! One must construct a new list, though it may share a tail
          ! with the original.
          call copy_list_segment (current_position, lookahead, segment, cursor)
          retval = segment
          current_position = skip_unsatisfying_elements (pred, cdr (lookahead))
          done = .false.
          do while (.not. done)
             if (.not. is_cons_pair (current_position)) then
                ! The current position is the end of the list (a nil
                ! list or a non-list).
                call set_cdr (cursor, current_position)
                done = .true.
             else
                lookahead = skip_satisfying_elements (pred, cdr (current_position))
                if (.not. is_cons_pair (lookahead)) then
                   ! We have found a common tail.
                   call set_cdr (cursor, current_position)
                   done = .true.
                else
                   ! Append another segment.
                   call copy_list_segment (current_position, lookahead, segment, next_cursor)
                   call set_cdr (cursor, segment)
                   cursor = next_cursor
                   current_position = skip_unsatisfying_elements (pred, cdr (lookahead))
                end if
             end if
          end do
       end if
    end if
    lst_f = retval
  end function list_filter

  recursive function list_remove (pred, lst) result (lst_r)
    !
    ! This implementation tries to share the longest possible tail
    ! with the original.
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_r

    class(*), allocatable :: retval
    class(*), allocatable :: current_position
    class(*), allocatable :: lookahead
    type(cons_t) :: segment
    type(cons_t) :: cursor, next_cursor
    logical :: done

    current_position = skip_satisfying_elements (pred, lst)
    if (.not. is_cons_pair (current_position)) then
       ! There are no elements that do not satisfy the predicate.
       retval = current_position
    else
       lookahead = skip_unsatisfying_elements (pred, cdr (current_position))
       if (.not. is_cons_pair (lookahead)) then
          ! A tail of the original is the entire result.
          retval = current_position
       else
          ! One must construct a new list, though it may share a tail
          ! with the original.
          call copy_list_segment (current_position, lookahead, segment, cursor)
          retval = segment
          current_position = skip_satisfying_elements (pred, cdr (lookahead))
          done = .false.
          do while (.not. done)
             if (.not. is_cons_pair (current_position)) then
                ! The current position is the end of the list (a nil
                ! list or a non-list).
                call set_cdr (cursor, current_position)
                done = .true.
             else
                lookahead = skip_unsatisfying_elements (pred, cdr (current_position))
                if (.not. is_cons_pair (lookahead)) then
                   ! We have found a common tail.
                   call set_cdr (cursor, current_position)
                   done = .true.
                else
                   ! Append another segment.
                   call copy_list_segment (current_position, lookahead, segment, next_cursor)
                   call set_cdr (cursor, segment)
                   cursor = next_cursor
                   current_position = skip_satisfying_elements (pred, cdr (lookahead))
                end if
             end if
          end do
       end if
    end if
    lst_r = retval
  end function list_remove

  recursive subroutine list_partition (pred, lst, lst_f, lst_r)
    !
    ! This implementation tries to share the longest possible tail
    ! with the original.
    !
    ! At this time, what happens to the end of a dotted list should be
    ! considered unspecified.
    !
    procedure(list_predicate1_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_f ! The `filter' output.
    class(*), allocatable :: lst_r ! The `remove' output.

    class(*), allocatable :: retval_f, retval_r
    class(*), allocatable :: current_position
    class(*), allocatable :: lookahead
    type(cons_t) :: segment
    type(cons_t) :: cursor_f
    type(cons_t) :: cursor_r
    type(cons_t) :: next_cursor
    logical :: done

    if (.not. is_cons_pair (lst)) then
       ! There are no elements that satisfy the predicate, and none
       ! that do not satisfy the predicate.
       !
       ! The original (possibly degenerate) list is entirely
       ! arbitrarily put in retval_f instead of retval_r.
       retval_f = lst
       retval_r = nil_list
    else
       current_position = lst
       if (pred (car (current_position))) then
          call first_position_satisfies
       else
          call first_position_does_not_satisfy
       end if
    end if

    lst_f = retval_f
    lst_r = retval_r

  contains

    subroutine first_position_satisfies
      lookahead = skip_satisfying_elements (pred, cdr (current_position))
      if (.not. is_cons_pair (lookahead)) then
         ! The original is the entire retval_f.
         retval_f = current_position
         retval_r = nil_list
      else
         ! One must construct a new list, though it may share a
         ! tail with the original.
         call copy_list_segment (current_position, lookahead, segment, cursor_f)
         retval_f = segment
         current_position = lookahead
         lookahead = skip_unsatisfying_elements (pred, cdr (current_position))
         if (.not. is_cons_pair (lookahead)) then
            ! A tail of the original is the entire retval_r.
            retval_r = current_position
         else
            ! One must construct a new list, though it may share a
            ! tail with the original.
            call copy_list_segment (current_position, lookahead, segment, cursor_r)
            retval_r = segment
            current_position = lookahead
            done = .false.
            do while (.not. done)
               if (.not. is_cons_pair (current_position)) then
                  ! The current position is the end of the list (a nil
                  ! list or a non-list).
                  call set_cdr (cursor_f, current_position)
                  done = .true.
               else
                  lookahead = skip_satisfying_elements (pred, cdr (current_position))
                  if (.not. is_cons_pair (lookahead)) then
                     ! We have found a common tail to attach to
                     ! setval_f (via cursor_f).
                     call set_cdr (cursor_f, current_position)
                     done = .true.
                  else
                     ! Append another segment to retval_f (using
                     ! cursor_f).
                     call copy_list_segment (current_position, lookahead, segment, next_cursor)
                     call set_cdr (cursor_f, segment)
                     cursor_f = next_cursor
                     current_position = lookahead
                     lookahead = skip_unsatisfying_elements (pred, cdr (current_position))
                     if (.not. is_cons_pair (lookahead)) then
                        ! We have found a common tail to attach to
                        ! setval_r (via cursor_r).
                        call set_cdr (cursor_r, current_position)
                        done = .true.
                     else
                        ! Append another segment to retval_r (using
                        ! cursor_r).
                        call copy_list_segment (current_position, lookahead, segment, next_cursor)
                        call set_cdr (cursor_r, segment)
                        cursor_r = next_cursor
                        current_position = lookahead
                     end if
                  end if
               end if
            end do
         end if
      end if
    end subroutine first_position_satisfies

    subroutine first_position_does_not_satisfy
      lookahead = skip_unsatisfying_elements (pred, cdr (current_position))
      if (.not. is_cons_pair (lookahead)) then
         ! The original is the entire retval_r.
         retval_f = nil_list
         retval_r = current_position
      else
         ! One must construct a new list, though it may share a
         ! tail with the original.
         call copy_list_segment (current_position, lookahead, segment, cursor_r)
         retval_r = segment
         current_position = lookahead
         lookahead = skip_satisfying_elements (pred, cdr (current_position))
         if (.not. is_cons_pair (lookahead)) then
            ! A tail of the original is the entire retval_f.
            retval_f = current_position
         else
            ! One must construct a new list, though it may share a
            ! tail with the original.
            call copy_list_segment (current_position, lookahead, segment, cursor_f)
            retval_f = segment
            current_position = lookahead
            done = .false.
            do while (.not. done)
               if (.not. is_cons_pair (current_position)) then
                  ! The current position is the end of the list (a nil
                  ! list or a non-list).
                  call set_cdr (cursor_r, current_position)
                  done = .true.
               else
                  lookahead = skip_unsatisfying_elements (pred, cdr (current_position))
                  if (.not. is_cons_pair (lookahead)) then
                     ! We have found a common tail to attach to
                     ! setval_r (via cursor_r).
                     call set_cdr (cursor_r, current_position)
                     done = .true.
                  else
                     ! Append another segment to retval_r (using
                     ! cursor_r).
                     call copy_list_segment (current_position, lookahead, segment, next_cursor)
                     call set_cdr (cursor_r, segment)
                     cursor_r = next_cursor
                     current_position = lookahead
                     lookahead = skip_satisfying_elements (pred, cdr (current_position))
                     if (.not. is_cons_pair (lookahead)) then
                        ! We have found a common tail to attach to
                        ! setval_f (via cursor_f).
                        call set_cdr (cursor_f, current_position)
                        done = .true.
                     else
                        ! Append another segment to retval_f (using
                        ! cursor_f).
                        call copy_list_segment (current_position, lookahead, segment, next_cursor)
                        call set_cdr (cursor_f, segment)
                        cursor_f = next_cursor
                        current_position = lookahead
                     end if
                  end if
               end if
            end do
         end if
      end if
    end subroutine first_position_does_not_satisfy

  end subroutine list_partition

  recursive function skip_quasiequal_elements (x, pred, lst) result (next)
    class(*), intent(in) :: x
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: next

    class(*), allocatable :: p
    logical :: found_end_or_unsatisfying_element

    p = lst
    found_end_or_unsatisfying_element = .false.
    do while (.not. found_end_or_unsatisfying_element)
       if (.not. is_cons_pair (p)) then
          found_end_or_unsatisfying_element = .true.
       else if (.not. pred (x, car (p))) then
          found_end_or_unsatisfying_element = .true.
       else
          p = cdr (p)
       end if
    end do
    next = p
  end function skip_quasiequal_elements

  recursive function skip_quasiunequal_elements (x, pred, lst) result (next)
    class(*), intent(in) :: x
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: next

    class(*), allocatable :: p
    logical :: found_end_or_satisfying_element

    p = lst
    found_end_or_satisfying_element = .false.
    do while (.not. found_end_or_satisfying_element)
       if (.not. is_cons_pair (p)) then
          found_end_or_satisfying_element = .true.
       else if (pred (x, car (p))) then
          found_end_or_satisfying_element = .true.
       else
          p = cdr (p)
       end if
    end do
    next = p
  end function skip_quasiunequal_elements

  recursive function list_delete (pred, x, lst) result (lst_d)
    !
    ! NOTE: The argument order is different from that of SRFI-1's
    !       `delete' procedure.
    !
    ! This implementation tries to share the longest possible tail
    ! with the original.
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_d

    class(*), allocatable :: retval
    class(*), allocatable :: current_position
    class(*), allocatable :: lookahead
    type(cons_t) :: segment
    type(cons_t) :: cursor, next_cursor
    logical :: done

    current_position = skip_quasiequal_elements (x, pred, lst)
    if (.not. is_cons_pair (current_position)) then
       ! There are no elements that do not satisfy the predicate.
       retval = current_position
    else
       lookahead = skip_quasiunequal_elements (x, pred, cdr (current_position))
       if (.not. is_cons_pair (lookahead)) then
          ! A tail of the original is the entire result.
          retval = current_position
       else
          ! One must construct a new list, though it may share a tail
          ! with the original.
          call copy_list_segment (current_position, lookahead, segment, cursor)
          retval = segment
          current_position = skip_quasiequal_elements (x, pred, cdr (lookahead))
          done = .false.
          do while (.not. done)
             if (.not. is_cons_pair (current_position)) then
                ! The current position is the end of the list (a nil
                ! list or a non-list).
                call set_cdr (cursor, current_position)
                done = .true.
             else
                lookahead = skip_quasiunequal_elements (x, pred, cdr (current_position))
                if (.not. is_cons_pair (lookahead)) then
                   ! We have found a common tail.
                   call set_cdr (cursor, current_position)
                   done = .true.
                else
                   ! Append another segment.
                   call copy_list_segment (current_position, lookahead, segment, next_cursor)
                   call set_cdr (cursor, segment)
                   cursor = next_cursor
                   current_position = skip_quasiequal_elements (x, pred, cdr (lookahead))
                end if
             end if
          end do
       end if
    end if
    lst_d = retval
  end function list_delete

  recursive function contains_match (lst, pred, x) result (match_found)
    class(*), intent(in) :: lst
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: x
    logical :: match_found

    class(*), allocatable :: head, tail

    match_found = .false.
    tail = lst
    do while (.not. match_found .and. is_cons_pair (tail))
       call uncons (tail, head, tail)
       match_found = pred (head, x)
    end do
  end function contains_match

  recursive subroutine skip_duplicated_elements (kept_elements, pred, lst, next)
    type(cons_t), intent(in) :: kept_elements
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: next

    class(*), allocatable :: p
    logical :: done

    p = lst
    done = .false.
    do while (.not. done)
       if (.not. is_cons_pair (p)) then
          done = .true.
       else if (.not. contains_match (kept_elements, pred, car (p))) then
          done = .true.
       else
          p = cdr (p)
       end if
    end do
    next = p
  end subroutine skip_duplicated_elements

  recursive subroutine skip_unduplicated_elements (kept_elements, pred, lst, next)
    type(cons_t), intent(inout) :: kept_elements
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable, intent(out) :: next

    type(cons_t) :: kept
    class(*), allocatable :: p
    class(*), allocatable :: element
    logical :: done

    kept = kept_elements
    p = lst
    done = .false.
    do while (.not. done)
       if (.not. is_cons_pair (p)) then
          done = .true.
       else
          element = car (p)
          if (contains_match (kept, pred, element)) then
             done = .true.
          else
             kept = element ** kept
             p = cdr (p)
          end if
       end if
    end do
    next = p
    kept_elements = kept
  end subroutine skip_unduplicated_elements

  recursive function list_delete_duplicates (pred, lst) result (lst_dd)
    !
    ! NOTE: The argument order is different from that of SRFI-1's
    !       `delete-duplicates' procedure.
    !
    ! This implementation tries to share a tail with the original,
    ! although not necessarily the longest one. (Finding the longest
    ! tail may require working backwards from the end of lst, but this
    ! implementation works forwards from the beginning.)
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_dd

    type(cons_t) :: kept_elements
    class(*), allocatable :: retval
    class(*), allocatable :: current_position
    class(*), allocatable :: lookahead
    type(cons_t) :: segment
    type(cons_t) :: cursor, next_cursor
    logical :: done

    kept_elements = nil_list

    current_position = lst
    if (.not. is_cons_pair (current_position)) then
       ! There are no elements from which to delete duplicates.
       retval = current_position
    else
       call skip_unduplicated_elements (kept_elements, pred, current_position, lookahead)
       if (.not. is_cons_pair (lookahead)) then
          ! The original is the entire result.
          retval = current_position
       else
          ! One must construct a new list, though it may share a tail
          ! with the original.
          call copy_list_segment (current_position, lookahead, segment, cursor)
          retval = segment
          call skip_duplicated_elements (kept_elements, pred, cdr (lookahead), current_position)
          done = .false.
          do while (.not. done)
             if (.not. is_cons_pair (current_position)) then
                ! The current position is the end of the list (a nil
                ! list or a non-list).
                call set_cdr (cursor, current_position)
                done = .true.
             else
                kept_elements = car (current_position) ** kept_elements
                call skip_unduplicated_elements (kept_elements, pred, cdr (current_position), lookahead)
                if (.not. is_cons_pair (lookahead)) then
                   ! We have found a common tail.
                   call set_cdr (cursor, current_position)
                   done = .true.
                else
                   ! Append another segment.
                   call copy_list_segment (current_position, lookahead, segment, next_cursor)
                   call set_cdr (cursor, segment)
                   cursor = next_cursor
                   call skip_duplicated_elements (kept_elements, pred, cdr (lookahead), current_position)
                end if
             end if
          end do
       end if
    end if
    lst_dd = retval
  end function list_delete_duplicates

  recursive function list_filter_map (subr, lst) result (lst_fm)
    procedure(list_filter_map_procedure_t) :: subr
    class(*), intent(in) :: lst
    class(*), allocatable :: lst_fm

    class(*), allocatable :: retval
    class(*), allocatable :: element
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    logical :: all_done
    logical :: keep

    retval = nil_list
    tail = lst
    all_done = .false.
    do while (.not. all_done)
       if (.not. is_cons_pair (tail)) then
          if (is_cons_pair (retval)) then
             call set_cdr (cursor, tail) ! Preserve the end of a dotted list.
          else
             retval = tail ! Preserve a degenerate dotted list.
          end if
          all_done = .true.
       else
          call uncons (tail, element, tail)
          call subr (element, keep)
          if (keep) then
             if (is_cons_pair (retval)) then
                new_pair = element ** nil_list
                call set_cdr (cursor, new_pair)
                cursor = new_pair
             else
                cursor = element ** nil_list
                retval = cursor
             end if
          end if
       end if
    end do
    lst_fm = retval
  end function list_filter_map

  recursive function list_fold (kons, knil, lst) result (folded_result)
    procedure(list_kons_procedure_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: folded_result

    class(*), allocatable :: retval, new_retval
    class(*), allocatable :: head, tail

    retval = knil
    tail = lst
    do while (is_cons_pair (tail))
       call uncons (tail, head, tail)
       call kons (head, retval, new_retval)
       retval = new_retval
    end do
    folded_result = retval
  end function list_fold

  recursive function list_fold_right (kons, knil, lst) result (folded_result)
    !
    ! WARNING: This implementation is recursive and uses O(n) stack
    !          space. If you need a non-recursive equivalent, reverse
    !          the list and then use `list_fold'.
    !
    procedure(list_kons_procedure_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: folded_result

    folded_result = recursion (lst)

  contains

    recursive function recursion (lst) result (folded_result)
      class(*), intent(in) :: lst
      class(*), allocatable :: folded_result

      class(*), allocatable :: head, tail

      if (.not. is_cons_pair (lst)) then
         folded_result = knil
      else
         call uncons (lst, head, tail)
         call kons (head, recursion (tail), folded_result)
      end if
    end function recursion

  end function list_fold_right

  recursive function list_pair_fold (kons, knil, lst) result (folded_result)
    procedure(list_kons_procedure_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: folded_result

    class(*), allocatable :: retval, new_retval
    class(*), allocatable :: tail, new_tail

    retval = knil
    tail = lst
    do while (is_cons_pair (tail))
       new_tail = cdr (tail)
       call kons (tail, retval, new_retval)
       retval = new_retval
       tail = new_tail
    end do
    folded_result = retval
  end function list_pair_fold

  recursive function list_pair_fold_right (kons, knil, lst) result (folded_result)
    !
    ! WARNING: This implementation is recursive and uses O(n) stack
    !          space. If you need a non-recursive equivalent, reverse
    !          the list and then use `list_fold'.
    !
    procedure(list_kons_procedure_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: folded_result

    folded_result = recursion (lst)

  contains

    recursive function recursion (lst) result (folded_result)
      class(*), intent(in) :: lst
      class(*), allocatable :: folded_result
      if (.not. is_cons_pair (lst)) then
         folded_result = knil
      else
         call kons (lst, recursion (cdr (lst)), folded_result)
      end if
    end function recursion

  end function list_pair_fold_right

  recursive function list_reduce (kons, right_identity, lst) result (reduced_result)
    procedure(list_kons_procedure_t) :: kons
    class(*), intent(in) :: right_identity
    class(*), intent(in) :: lst
    class(*), allocatable :: reduced_result

    class(*), allocatable :: head, tail

    if (is_cons_pair (lst)) then
       call uncons (lst, head, tail)
       reduced_result = list_fold (kons, head, tail)
    else
       reduced_result = right_identity
    end if
  end function list_reduce

  recursive function list_reduce_right (kons, right_identity, lst) result (reduced_result)
    procedure(list_kons_procedure_t) :: kons
    class(*), intent(in) :: right_identity
    class(*), intent(in) :: lst
    class(*), allocatable :: reduced_result

    class(*), allocatable :: head, tail

    if (is_cons_pair (lst)) then
       call uncons (lst, head, tail)
       reduced_result = recursion (head, tail)
    else
       reduced_result = right_identity
    end if

  contains

    recursive function recursion (head, lst1) result (reduced_result)
      class(*), intent(in) :: head, lst1
      class(*), allocatable :: reduced_result

      class(*), allocatable :: hd, tl

      if (is_cons_pair (lst1)) then
         call uncons (lst1, hd, tl)
         call kons (head, recursion (hd, tl), reduced_result)
      else
         reduced_result = head
      end if
    end function recursion

  end function list_reduce_right

  recursive function list_unfold_with_tail_gen (pred, f, g, seed, tail_gen) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_modify_elements_procedure_t) :: f
    procedure(list_modify_elements_procedure_t) :: g
    class(*), intent(in) :: seed
    procedure(list_modify_elements_procedure_t) :: tail_gen
    class(*), allocatable :: lst

    lst = recursion (seed)

  contains

    recursive function recursion (seed) result (lst)
      class(*), intent(in) :: seed
      class(*), allocatable :: lst

      class(*), allocatable :: f_of_seed
      class(*), allocatable :: g_of_seed
      class(*), allocatable :: tail_gen_of_seed

      if (pred (seed)) then
         tail_gen_of_seed = seed
         call tail_gen (tail_gen_of_seed)
         lst = tail_gen_of_seed
      else
         f_of_seed = seed
         call f (f_of_seed)
         g_of_seed = seed
         call g (g_of_seed)
         lst = cons (f_of_seed, recursion (g_of_seed))
      end if
    end function recursion

  end function list_unfold_with_tail_gen

  recursive function list_unfold_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_modify_elements_procedure_t) :: f
    procedure(list_modify_elements_procedure_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst

    lst = recursion (seed)

  contains

    recursive function recursion (seed) result (lst)
      class(*), intent(in) :: seed
      class(*), allocatable :: lst

      class(*), allocatable :: f_of_seed
      class(*), allocatable :: g_of_seed

      if (pred (seed)) then
         lst = nil_list
      else
         f_of_seed = seed
         call f (f_of_seed)
         g_of_seed = seed
         call g (g_of_seed)
         lst = cons (f_of_seed, recursion (g_of_seed))
      end if
    end function recursion

  end function list_unfold_with_nil_tail

  recursive function list_unfold_right_with_tail (pred, f, g, seed, tail) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_modify_elements_procedure_t) :: f
    procedure(list_modify_elements_procedure_t) :: g
    class(*), intent(in) :: seed
    class(*), intent(in) :: tail
    class(*), allocatable :: lst

    class(*), allocatable :: f_of_seed
    class(*), allocatable :: current_seed
    class(*), allocatable :: retval

    retval = tail
    current_seed = seed
    do while (.not. pred (current_seed))
       f_of_seed = current_seed
       call f (f_of_seed)
       retval = cons (f_of_seed, retval)
       call g (current_seed)
    end do
    lst = retval
  end function list_unfold_right_with_tail

  recursive function list_unfold_right_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_modify_elements_procedure_t) :: f
    procedure(list_modify_elements_procedure_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst
    lst = list_unfold_right_with_tail (pred, f, g, seed, nil_list)
  end function list_unfold_right_with_nil_tail

  function alist_cons (k, v, alst) result (cons_kv_alst)
    class(*), intent(in) :: k, v, alst
    class(*), allocatable :: cons_kv_alst
    cons_kv_alst = cons (cons (k, v), alst)
  end function alist_cons

  function alist_copy (alst) result (alst_c)
    class(*), intent(in) :: alst
    class(*), allocatable :: alst_c

    class(*), allocatable :: head, tail, k, v
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    
    select type (alst)
    class is (cons_t)
       if (list_is_nil (alst)) then
          alst_c = nil_list
       else
          call uncons (alst, head, tail)
          call uncons (head, k, v)
          cursor = cons (cons (k, v), tail)
          alst_c = cursor
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             call uncons (head, k, v)
             new_pair = cons (cons (k, v), tail)
             call set_cdr (cursor, new_pair)
             cursor = new_pair
          end do
       end if
    class default
       alst_c = alst
    end select
  end function alist_copy

  recursive function skip_key_matches (x, pred, lst) result (next)
    class(*), intent(in) :: x
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: next

    class(*), allocatable :: p
    logical :: found_end_or_unsatisfying_element

    p = lst
    found_end_or_unsatisfying_element = .false.
    do while (.not. found_end_or_unsatisfying_element)
       if (.not. is_cons_pair (p)) then
          found_end_or_unsatisfying_element = .true.
       else if (.not. pred (x, caar (p))) then
          found_end_or_unsatisfying_element = .true.
       else
          p = cdr (p)
       end if
    end do
    next = p
  end function skip_key_matches

  recursive function skip_key_mismatches (x, pred, lst) result (next)
    class(*), intent(in) :: x
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: lst
    class(*), allocatable :: next

    class(*), allocatable :: p
    logical :: found_end_or_satisfying_element

    p = lst
    found_end_or_satisfying_element = .false.
    do while (.not. found_end_or_satisfying_element)
       if (.not. is_cons_pair (p)) then
          found_end_or_satisfying_element = .true.
       else if (pred (x, caar (p))) then
          found_end_or_satisfying_element = .true.
       else
          p = cdr (p)
       end if
    end do
    next = p
  end function skip_key_mismatches

  recursive function alist_delete (pred, key, alst) result (alst_d)
    !
    ! NOTE: The argument order is different from that of SRFI-1's
    !       `alist-delete' procedure.
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    class(*), allocatable :: alst_d

    class(*), allocatable :: retval
    class(*), allocatable :: current_position
    class(*), allocatable :: lookahead
    type(cons_t) :: segment
    type(cons_t) :: cursor, next_cursor
    logical :: done

    current_position = skip_key_matches (key, pred, alst)
    if (.not. is_cons_pair (current_position)) then
       ! There are no elements that do not satisfy the predicate.
       retval = current_position
    else
       lookahead = skip_key_mismatches (key, pred, cdr (current_position))
       if (.not. is_cons_pair (lookahead)) then
          ! A tail of the original is the entire result.
          retval = current_position
       else
          ! One must construct a new list, though it may share a tail
          ! with the original.
          call copy_list_segment (current_position, lookahead, segment, cursor)
          retval = segment
          current_position = skip_key_matches (key, pred, cdr (lookahead))
          done = .false.
          do while (.not. done)
             if (.not. is_cons_pair (current_position)) then
                ! The current position is the end of the list (a nil
                ! list or a non-list).
                call set_cdr (cursor, current_position)
                done = .true.
             else
                lookahead = skip_key_mismatches (key, pred, cdr (current_position))
                if (.not. is_cons_pair (lookahead)) then
                   ! We have found a common tail.
                   call set_cdr (cursor, current_position)
                   done = .true.
                else
                   ! Append another segment.
                   call copy_list_segment (current_position, lookahead, segment, next_cursor)
                   call set_cdr (cursor, segment)
                   cursor = next_cursor
                   current_position = skip_key_matches (key, pred, cdr (lookahead))
                end if
             end if
          end do
       end if
    end if
    alst_d = retval
  end function alist_delete

  function alist_assoc (pred, key, alst) result (alst_a)
    !
    ! NOTE: SRFI-1's `assoc' returns `#f' if there is no match. This
    !       function returns a nil list, instead.
    !
    ! NOTE: The argument order is different from that of SRFI-1's
    !       `assoc' procedure.
    !
    procedure(list_predicate2_t) :: pred
    class(*), intent(in) :: key
    class(*), intent(in) :: alst
    type(cons_t) :: alst_a

    class(*), allocatable :: sublist

    sublist = skip_key_mismatches (key, pred, alst)
    if (is_cons_pair (sublist)) then
       alst_a = cons_t_cast (car (sublist))
    else
       alst_a = nil_list
    end if
  end function alist_assoc

  recursive function list_merge (compare, lst1, lst2) result (lst_m)
    !
    ! It is assumed lst1 and lst2 are proper lists.
    !
    procedure(list_comparison2_t) :: compare 
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_m
    lst_m = list_destructive_merge (compare, list_copy (lst1), list_copy (lst2))
  end function list_merge

  recursive function list_destructive_merge (compare, lst1, lst2) result (lst_m)
    !
    ! It is assumed lst1 and lst2 are proper lists.
    !
    procedure(list_comparison2_t) :: compare 
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_m

    type(cons_t) :: p1
    type(cons_t) :: p2

    select type (lst1)
    class is (cons_t)
       select type (lst2)
       class is (cons_t)
          p1 = lst1
          p2 = lst2
          lst_m = merge_lists (p1, p2)
       class default
          call error_abort ("the third argument to list_destructive_merge is not a cons_t")
       end select
    class default
       call error_abort ("the first argument to list_destructive_merge is not a cons_t")
    end select

  contains

    recursive function merge_lists (p1, p2) result (lst_m)
      type(cons_t) :: p1
      type(cons_t) :: p2
      type(cons_t) :: lst_m

      class(*), allocatable :: hd1, tl1
      class(*), allocatable :: hd2, tl2
      type(cons_t) :: cursor
      logical :: p1_is_active
      logical :: p1_is_active_is_changed
      logical :: done

      if (.not. list_is_pair (p1)) then
         lst_m = p2
      else if (.not. list_is_pair (p2)) then
         lst_m = p1
      else
         call uncons (p1, hd1, tl1)
         call uncons (p2, hd2, tl2)
         if (compare (hd1, hd2) <= 0) then
            p1_is_active = .true.
            cursor = p1
            p1 = cons_t_cast (tl1)
         else
            p1_is_active = .false.
            cursor = p2
            p2 = cons_t_cast (tl2)
         end if
         lst_m = cursor
         done = .false.
         do while (.not. done)
            if (p1_is_active) then
               p1_is_active_is_changed = .false.
               do while (.not. p1_is_active_is_changed)
                  if (.not. list_is_pair (p1)) then
                     call set_cdr (cursor, p2)
                     p1_is_active_is_changed = .true.
                     done = .true.
                  else if (.not. list_is_pair (p2)) then
                     call set_cdr (cursor, p1)
                     p1_is_active_is_changed = .true.
                     done = .true.
                  else
                     call uncons (p1, hd1, tl1)
                     call uncons (p2, hd2, tl2)
                     if (compare (hd1, hd2) <= 0) then
                        cursor = p1
                        p1 = cons_t_cast (tl1)
                     else
                        call set_cdr (cursor, p2)
                        p1_is_active = .false.
                        p1_is_active_is_changed = .true.
                        cursor = p2
                        p2 = cons_t_cast (tl2)
                     end if
                  end if
               end do
            else
               p1_is_active_is_changed = .false.
               do while (.not. p1_is_active_is_changed)
                  if (.not. list_is_pair (p1)) then
                     call set_cdr (cursor, p2)
                     p1_is_active_is_changed = .true.
                     done = .true.
                  else if (.not. list_is_pair (p2)) then
                     call set_cdr (cursor, p1)
                     p1_is_active_is_changed = .true.
                     done = .true.
                  else
                     call uncons (p1, hd1, tl1)
                     call uncons (p2, hd2, tl2)
                     if (compare (hd1, hd2) <= 0) then
                        call set_cdr (cursor, p1)
                        p1_is_active = .true.
                        p1_is_active_is_changed = .true.
                        cursor = p1
                        p1 = cons_t_cast (tl1)
                     else
                        call set_cdr (cursor, p2)
                        cursor = p2
                        p2 = cons_t_cast (tl2)
                     end if
                  end if
               end do
            end if
         end do
      end if
    end function merge_lists

  end function list_destructive_merge

  recursive function list_stable_sort (compare, lst) result (lst_ms)
    procedure(list_comparison2_t) :: compare 
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ms
    lst_ms = list_destructive_stable_sort (compare, list_copy (lst))
  end function list_stable_sort

  recursive function list_destructive_stable_sort (compare, lst) result (lst_ss)
    procedure(list_comparison2_t) :: compare 
    class(*), intent(in) :: lst
    type(cons_t) :: lst_ss

    integer, parameter :: small_size = 11

    type(cons_t) :: p

    select type (lst)
    class is (cons_t)
       p = lst
       if (.not. list_is_pair (p)) then
          ! List of length zero.
          lst_ss = p
       else if (.not. is_cons_pair (cdr (p))) then
          ! List of length one.
          lst_ss = p
       else
          lst_ss = merge_sort (p, list_length (p))
       end if
    class default
       call error_abort ("the second argument to list_destructive_stable_sort is not a cons_t")
    end select

  contains

    recursive function insertion_sort (p, n) result (lst_ss)
      !
      ! Put CONS pairs into an array and do an insertion sort on the
      ! array.
      !
      type(cons_t) :: p
      integer :: n
      type(cons_t) :: lst_ss

      type(cons_t), dimension(1:small_size) :: array
      type(cons_t) :: x
      integer :: i, j
      logical :: done

      if (n <= 1) then
         lst_ss = p
      else
         ! Fill the array with CONS pairs.
         do i = 1, n
            array(i) = p
            p = cons_t_cast (cdr (p))
         end do

         ! Do an insertion sort on the array.
         do i = 2, n
            x = array(i)
            j = i - 1
            done = .false.
            do while (.not. done)
               if (j == 0) then
                  done = .true.
               else if (compare (car (array(j)), car (x)) <= 0) then
                  done = .true.
               else
                  array(j + 1) = array(j)
                  j = j - 1
               end if
            end do
            array(j + 1) = x
         end do

         ! Connect the CONS pairs into a list.
         call set_cdr (array(n), nil_list)
         do i = n - 1, 1, -1
            call set_cdr (array(i), array(i + 1))
         end do

         ! The result.
         lst_ss = array(1)
      end if
    end function insertion_sort

    recursive function merge_sort (p, n) result (lst_ss)
      !
      ! A top-down merge sort using non-tail recursion.
      !
      type(cons_t) :: p
      integer :: n
      type(cons_t) :: lst_ss

      integer :: n_half
      type(cons_t) :: p_left, p_right
      class(*), allocatable :: p_tail

      if (n <= small_size) then
         lst_ss = insertion_sort (p, n)
      else
         n_half = n / 2
         call list_split (p, n_half, p_left, p_tail) ! FIXME: Use list_destructive_split, once it is implemented.
         p_left = merge_sort (p_left, n_half)
         p_right = merge_sort (cons_t_cast (p_tail), n - n_half)
         lst_ss = list_destructive_merge (compare, p_left, p_right)
      end if
    end function merge_sort

  end function list_destructive_stable_sort

end module cons_lists
