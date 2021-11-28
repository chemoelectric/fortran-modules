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

     function list_mapfunc_t (x) result (y)
       !
       ! list_mapfunc_t is the type of a function called by
       ! `list_map'. It takes multiple values (or list values) of
       ! input as the list x, and outputs multiple values (or list
       ! values) as the list y.
       !
       use :: cons_types
       class(cons_t), intent(in) :: x
       type(cons_t) :: y
     end function list_mapfunc_t

     function list_predfunc_t (x) result (bool)
       !
       ! FIXME: Document this.
       !
       use :: cons_types
       class(cons_t), intent(in) :: x
       logical :: bool
     end function list_predfunc_t

     function list_orderfunc_t (x) result (sign)
       !
       ! FIXME: Document this.
       !
       use :: cons_types
       class(cons_t), intent(in) :: x
       integer :: sign
     end function list_orderfunc_t

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
  ! Please keep in mind that the term `list' is used only loosely in
  ! Scheme. The fundamental `list' object types are actually the nil
  ! `list' and the CAR-CDR pair; furthermore, objects other than those
  ! can be (and in this module will be) regarded as degenerate `dotted
  ! lists'.
  !

  use :: cons_types
  use :: cons_procedure_types

  implicit none
  private

  public :: cons_t              ! The type of a CONS-pair or NIL-list.
  public :: nil_list            ! The canonical NIL-list.

  public :: is_nil_list         ! Is an object a NIL-list?
  public :: is_cons_pair        ! Is an object a CONS-pair?

  public :: list_is_nil         ! Is a cons_t a NIL-list?
  public :: list_is_pair        ! Is a cons_t a CONS-pair?

  ! cons_t_eq(x,y) is like Scheme's `(eq? x y)' for two lists.
  public :: cons_t_eq           ! Are the two cons_t equivalent?

  public :: cons            ! The fundamental CONS-pair constructor.
  public :: uncons          ! The fundamental CONS-pair deconstructor.

  ! Notation: `elem1 ** elem2 ** elem3 ** nil_list'
  public :: operator(**)     ! An infix notation for `list_cons'.
  public :: list_cons        ! CONS assuming the right side is a list.

  public :: set_car             ! Change the CAR of a CONS-pair.
  public :: set_cdr             ! Change the CDR of a CONS-pair.

  public :: cons_t_cast         ! Assume an object is a cons_t.

  public :: list_length ! The number of CAR elements in a proper or dotted list.
  public :: is_proper_list ! Is an object a list but neither dotted nor circular?
  public :: is_dotted_list ! Is an object a non-list or a `list' that ends in something other than nil?
  public :: is_circular_list    ! Is an object a circular list?
  public :: list_classify ! Is the object a dotted list? Is it a circular list?

  ! Permutations of car and cdr, for returning elements of a tree.
  public :: car
  public :: cdr
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

  ! Return a numbered element of a list.
  public :: list_ref0       ! Return the ith element, starting at i=0.
  public :: list_ref1       ! Return the ith element, starting at i=1.
  public :: list_refn       ! Return the ith element, starting at i=n.

  public :: list_last           ! Return the last CAR element.
  public :: list_last_pair ! Return the last CONS-pair of a (possibly dotted) list.

  public :: make_list ! Make a list that is one element value repeated.

  ! Return a list that is a sequence of integers. (The name `IOTA' is
  ! taken from SRFI-1 and was inspired by APL.)
  public :: iota        ! The overloaded identifier for the following.
  public :: iota_given_length
  public :: iota_given_length_start
  public :: iota_given_length_start_step

  public :: circular_list       ! Make a circular list.

  ! Make and unmake lists of the given lengths.
  public :: list1
  public :: list2
  public :: list3
  public :: list4
  public :: list5
  public :: list6
  public :: list7
  public :: list8
  public :: list9
  public :: unlist1
  public :: unlist2
  public :: unlist3
  public :: unlist4
  public :: unlist5
  public :: unlist6
  public :: unlist7
  public :: unlist8
  public :: unlist9
  public :: unlist1_with_tail
  public :: unlist2_with_tail
  public :: unlist3_with_tail
  public :: unlist4_with_tail
  public :: unlist5_with_tail
  public :: unlist6_with_tail
  public :: unlist7_with_tail
  public :: unlist8_with_tail
  public :: unlist9_with_tail

  public :: list_reverse          ! Make a reversed copy.
  public :: list_reverse_in_place ! Reverse a list without copying. (The argument must be a cons_t.)
  public :: list_copy             ! Make a copy in the original order.
  public :: list_take             ! Copy the first n CAR elements.
  public :: list_drop ! Drop the first n CAR elements (by performing n CDR operations).
  public :: list_take_right ! Roughly, `return the last n elements' (but see SRFI-1).
  public :: list_drop_right ! Roughly, `drop the last n elements' (but see SRFI-1).
  public :: list_split     ! A combination of list_take and list_drop.
  public :: list_append    ! Concatenate two lists.
  public :: list_append_reverse ! Concatenate the reverse of one list to another list.
  public :: list_append_in_place ! Concatenate two lists, without copying.
  public :: list_append_reverse_in_place ! Reverse the first list and then append, without copying.
  public :: list_concatenate    ! Concatenate a list of lists.

  ! Zipping: joining the elements of separate lists into a list of
  ! lists.
  public :: list_zip  ! Use the elements of a list as the arguments.
  public :: list_zip1 ! Box each element of a list in a length-1 list.
  public :: list_zip2
  public :: list_zip3
  public :: list_zip4
  public :: list_zip5
  public :: list_zip6
  public :: list_zip7
  public :: list_zip8
  public :: list_zip9

  ! Unzipping: separating the elements of a list of lists into
  ! separate lists. (The list_unzip1, list_unzip2, ...,
  ! implementations may be significantly faster than list_unzip.)
  public :: list_unzip ! Return the separated lists as a list of lists.
  public :: list_unzip1
  public :: list_unzip2
  public :: list_unzip3
  public :: list_unzip4
  public :: list_unzip5
  public :: list_unzip6
  public :: list_unzip7
  public :: list_unzip8
  public :: list_unzip9

  public :: list_map ! Map list elements. (This is like SRFI-1 `map-in-order'.)
  public :: list_mapfunc_t

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

contains

  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("cons_lists error: ", a)') msg
    call abort
  end subroutine error_abort

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

  function caar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (car (tree))
  end function caar

  function cdar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (car (tree))
  end function cdar

  function cadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (cdr (tree))
  end function cadr

  function cddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (cdr (tree))
  end function cddr

  function caaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (car (car (tree)))
  end function caaar

  function cdaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (car (car (tree)))
  end function cdaar

  function cadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (cdr (car (tree)))
  end function cadar

  function cddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (cdr (car (tree)))
  end function cddar

  function caadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (car (cdr (tree)))
  end function caadr

  function cdadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (car (cdr (tree)))
  end function cdadr

  function caddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (cdr (cdr (tree)))
  end function caddr

  function cdddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (cdr (cdr (tree)))
  end function cdddr

  function caaaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (car (car (car (tree))))
  end function caaaar

  function cdaaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (car (car (car (tree))))
  end function cdaaar

  function cadaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (cdr (car (car (tree))))
  end function cadaar

  function cddaar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (cdr (car (car (tree))))
  end function cddaar

  function caadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (car (cdr (car (tree))))
  end function caadar

  function cdadar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (car (cdr (car (tree))))
  end function cdadar

  function caddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (cdr (cdr (car (tree))))
  end function caddar

  function cdddar (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (cdr (cdr (car (tree))))
  end function cdddar

  function caaadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (car (car (cdr (tree))))
  end function caaadr

  function cdaadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (car (car (cdr (tree))))
  end function cdaadr

  function cadadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (cdr (car (cdr (tree))))
  end function cadadr

  function cddadr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (cdr (car (cdr (tree))))
  end function cddadr

  function caaddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (car (cdr (cdr (tree))))
  end function caaddr

  function cdaddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (car (cdr (cdr (tree))))
  end function cdaddr

  function cadddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = car (cdr (cdr (cdr (tree))))
  end function cadddr

  function cddddr (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = cdr (cdr (cdr (cdr (tree))))
  end function cddddr

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

  function list1 (obj1) result (lst)
    class(*), intent(in) :: obj1
    type(cons_t) :: lst

    lst = obj1 ** nil_list
  end function list1

  function list2 (obj1, obj2) result (lst)
    class(*), intent(in) :: obj1, obj2
    type(cons_t) :: lst

    lst = obj2 ** nil_list
    lst = obj1 ** lst
  end function list2

  function list3 (obj1, obj2, obj3) result (lst)
    class(*), intent(in) :: obj1, obj2, obj3
    type(cons_t) :: lst

    lst = obj3 ** nil_list
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list3

  function list4 (obj1, obj2, obj3, obj4) result (lst)
    class(*), intent(in) :: obj1, obj2, obj3, obj4
    type(cons_t) :: lst

    lst = obj4 ** nil_list
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list4

  function list5 (obj1, obj2, obj3, obj4, obj5) result (lst)
    class(*), intent(in) :: obj1, obj2, obj3, obj4, obj5
    type(cons_t) :: lst

    lst = obj5 ** nil_list
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list5

  function list6 (obj1, obj2, obj3, obj4, obj5, obj6) result (lst)
    class(*), intent(in) :: obj1, obj2, obj3, obj4, obj5, obj6
    type(cons_t) :: lst

    lst = obj6 ** nil_list
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list6

  function list7 (obj1, obj2, obj3, obj4, obj5, obj6, obj7) result (lst)
    class(*), intent(in) :: obj1, obj2, obj3, obj4, obj5, obj6, obj7
    type(cons_t) :: lst

    lst = obj7 ** nil_list
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list7

  function list8 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8) result (lst)
    class(*), intent(in) :: obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8
    type(cons_t) :: lst

    lst = obj8 ** nil_list
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list8

  function list9 (obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9) result (lst)
    class(*), intent(in) :: obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9
    type(cons_t) :: lst

    lst = obj9 ** nil_list
    lst = obj8 ** lst
    lst = obj7 ** lst
    lst = obj6 ** lst
    lst = obj5 ** lst
    lst = obj4 ** lst
    lst = obj3 ** lst
    lst = obj2 ** lst
    lst = obj1 ** lst
  end function list9

  subroutine unlist1 (lst, obj1)
    !
    ! This subroutine `unlists' the 1 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist1 of a list that is too long")
    end if
  end subroutine unlist1

  subroutine unlist2 (lst, obj1, obj2)
    !
    ! This subroutine `unlists' the 2 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist2 of a list that is too long")
    end if
  end subroutine unlist2

  subroutine unlist3 (lst, obj1, obj2, obj3)
    !
    ! This subroutine `unlists' the 3 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    call uncons (tail, head, tail)
    obj3 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist3 of a list that is too long")
    end if
  end subroutine unlist3

  subroutine unlist4 (lst, obj1, obj2, obj3, obj4)
    !
    ! This subroutine `unlists' the 4 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    call uncons (tail, head, tail)
    obj3 = head
    call uncons (tail, head, tail)
    obj4 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist4 of a list that is too long")
    end if
  end subroutine unlist4

  subroutine unlist5 (lst, obj1, obj2, obj3, obj4, obj5)
    !
    ! This subroutine `unlists' the 5 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    call uncons (tail, head, tail)
    obj3 = head
    call uncons (tail, head, tail)
    obj4 = head
    call uncons (tail, head, tail)
    obj5 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist5 of a list that is too long")
    end if
  end subroutine unlist5

  subroutine unlist6 (lst, obj1, obj2, obj3, obj4, obj5, obj6)
    !
    ! This subroutine `unlists' the 6 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    call uncons (tail, head, tail)
    obj3 = head
    call uncons (tail, head, tail)
    obj4 = head
    call uncons (tail, head, tail)
    obj5 = head
    call uncons (tail, head, tail)
    obj6 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist6 of a list that is too long")
    end if
  end subroutine unlist6

  subroutine unlist7 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7)
    !
    ! This subroutine `unlists' the 7 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6, obj7

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    call uncons (tail, head, tail)
    obj3 = head
    call uncons (tail, head, tail)
    obj4 = head
    call uncons (tail, head, tail)
    obj5 = head
    call uncons (tail, head, tail)
    obj6 = head
    call uncons (tail, head, tail)
    obj7 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist7 of a list that is too long")
    end if
  end subroutine unlist7

  subroutine unlist8 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8)
    !
    ! This subroutine `unlists' the 8 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    call uncons (tail, head, tail)
    obj3 = head
    call uncons (tail, head, tail)
    obj4 = head
    call uncons (tail, head, tail)
    obj5 = head
    call uncons (tail, head, tail)
    obj6 = head
    call uncons (tail, head, tail)
    obj7 = head
    call uncons (tail, head, tail)
    obj8 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist8 of a list that is too long")
    end if
  end subroutine unlist8

  subroutine unlist9 (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9)
    !
    ! This subroutine `unlists' the 9 elements of lst (which is
    ! allowed to be dotted, in which case the extra value is ignored).
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9

    class(*), allocatable :: head
    class(*), allocatable :: tail

    tail = lst
    call uncons (tail, head, tail)
    obj1 = head
    call uncons (tail, head, tail)
    obj2 = head
    call uncons (tail, head, tail)
    obj3 = head
    call uncons (tail, head, tail)
    obj4 = head
    call uncons (tail, head, tail)
    obj5 = head
    call uncons (tail, head, tail)
    obj6 = head
    call uncons (tail, head, tail)
    obj7 = head
    call uncons (tail, head, tail)
    obj8 = head
    call uncons (tail, head, tail)
    obj9 = head
    if (is_cons_pair (tail)) then
       call error_abort ("unlist9 of a list that is too long")
    end if
  end subroutine unlist9

  subroutine unlist1_with_tail (lst, obj1, tail)
    !
    ! This subroutine `unlists' the leading 1 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    tail = tl
  end subroutine unlist1_with_tail

  subroutine unlist2_with_tail (lst, obj1, obj2, tail)
    !
    ! This subroutine `unlists' the leading 2 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    tail = tl
  end subroutine unlist2_with_tail

  subroutine unlist3_with_tail (lst, obj1, obj2, obj3, tail)
    !
    ! This subroutine `unlists' the leading 3 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    call uncons (tl, hd, tl)
    obj3 = hd
    tail = tl
  end subroutine unlist3_with_tail

  subroutine unlist4_with_tail (lst, obj1, obj2, obj3, obj4, tail)
    !
    ! This subroutine `unlists' the leading 4 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    call uncons (tl, hd, tl)
    obj3 = hd
    call uncons (tl, hd, tl)
    obj4 = hd
    tail = tl
  end subroutine unlist4_with_tail

  subroutine unlist5_with_tail (lst, obj1, obj2, obj3, obj4, obj5, tail)
    !
    ! This subroutine `unlists' the leading 5 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    call uncons (tl, hd, tl)
    obj3 = hd
    call uncons (tl, hd, tl)
    obj4 = hd
    call uncons (tl, hd, tl)
    obj5 = hd
    tail = tl
  end subroutine unlist5_with_tail

  subroutine unlist6_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, tail)
    !
    ! This subroutine `unlists' the leading 6 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    call uncons (tl, hd, tl)
    obj3 = hd
    call uncons (tl, hd, tl)
    obj4 = hd
    call uncons (tl, hd, tl)
    obj5 = hd
    call uncons (tl, hd, tl)
    obj6 = hd
    tail = tl
  end subroutine unlist6_with_tail

  subroutine unlist7_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, tail)
    !
    ! This subroutine `unlists' the leading 7 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6, obj7
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    call uncons (tl, hd, tl)
    obj3 = hd
    call uncons (tl, hd, tl)
    obj4 = hd
    call uncons (tl, hd, tl)
    obj5 = hd
    call uncons (tl, hd, tl)
    obj6 = hd
    call uncons (tl, hd, tl)
    obj7 = hd
    tail = tl
  end subroutine unlist7_with_tail

  subroutine unlist8_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, tail)
    !
    ! This subroutine `unlists' the leading 8 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    call uncons (tl, hd, tl)
    obj3 = hd
    call uncons (tl, hd, tl)
    obj4 = hd
    call uncons (tl, hd, tl)
    obj5 = hd
    call uncons (tl, hd, tl)
    obj6 = hd
    call uncons (tl, hd, tl)
    obj7 = hd
    call uncons (tl, hd, tl)
    obj8 = hd
    tail = tl
  end subroutine unlist8_with_tail

  subroutine unlist9_with_tail (lst, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, tail)
    !
    ! This subroutine `unlists' the leading 9 elements of lst, and
    ! also returns the tail.
    !
    class(cons_t) :: lst
    class(*), allocatable :: obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9
    class(*), allocatable :: tail

    class(*), allocatable :: hd
    class(*), allocatable :: tl

    tl = lst
    call uncons (tl, hd, tl)
    obj1 = hd
    call uncons (tl, hd, tl)
    obj2 = hd
    call uncons (tl, hd, tl)
    obj3 = hd
    call uncons (tl, hd, tl)
    obj4 = hd
    call uncons (tl, hd, tl)
    obj5 = hd
    call uncons (tl, hd, tl)
    obj6 = hd
    call uncons (tl, hd, tl)
    obj7 = hd
    call uncons (tl, hd, tl)
    obj8 = hd
    call uncons (tl, hd, tl)
    obj9 = hd
    tail = tl
  end subroutine unlist9_with_tail

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

  subroutine list_reverse_in_place (lst)
    type(cons_t), intent(inout) :: lst

    type(cons_t) :: lst_r
    type(cons_t) :: tail

    lst_r = nil_list
    do while (is_cons_pair (lst))
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

    class(cons_t), allocatable :: tail

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
    call set_cdr (list_last_pair (lst1), lst2)
  end subroutine list_append_in_place

  subroutine list_append_reverse_in_place (lst1, lst2)
    !
    ! lst1 must be a non-empty, non-circular list.
    !
    type(cons_t) :: lst1
    class(*) :: lst2
    call list_reverse_in_place (lst1)
    call list_append_in_place (lst1, lst2)
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

  function list_zip (lists) result (lst_z)
    type :: zipper_t
       type(cons_t) :: lst
       type(cons_t) :: tails
       logical :: exhausted
    end type zipper_t

    class(cons_t), intent(in) :: lists
    type(cons_t) :: lst_z

    type(zipper_t) :: zipper

    zipper%lst = nil_list
    zipper%tails = list_reverse (lists)
    zipper%exhausted = .false.
    do while (.not. zipper%exhausted .and. is_cons_pair (zipper%tails))
       zipper = zip_one_row (zipper)
    end do
    lst_z = zipper%lst
    call list_reverse_in_place (lst_z)

  contains

    function zip_one_row (zipper_in) result (zipper_out)
      type(zipper_t), intent(in) :: zipper_in
      type(zipper_t) :: zipper_out

      class(*), allocatable :: p
      type(cons_t) :: z
      type(cons_t) :: t

      zipper_out = zipper_in

      z = nil_list
      t = nil_list
      p = zipper_in%tails
      do while (.not. zipper_out%exhausted .and. is_cons_pair (p))
         if (is_cons_pair (car (p))) then
            z = cons (caar (p), z)
            t = cons (cdar (p), t)
            p = cdr (p)
         else
            zipper_out%exhausted = .true.
         end if
      end do
      if (.not. zipper_out%exhausted) then
         zipper_out%lst = cons (z, zipper_out%lst)
         call list_reverse_in_place (t)
         zipper_out%tails = t
      end if
    end function zip_one_row

  end function list_zip

  function list_zip1 (lst1) result (lst_z)
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst1 ** nil_list
    lst_z = list_zip (lists)
  end function list_zip1

  function list_zip2 (lst1, lst2) result (lst_z)
    class(*), intent(in) :: lst1, lst2
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst2 ** nil_list
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip2

  function list_zip3 (lst1, lst2, lst3) result (lst_z)
    class(*), intent(in) :: lst1, lst2, lst3
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst3 ** nil_list
    lists = lst2 ** lists
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip3

  function list_zip4 (lst1, lst2, lst3, lst4) result (lst_z)
    class(*), intent(in) :: lst1, lst2, lst3, lst4
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst4 ** nil_list
    lists = lst3 ** lists
    lists = lst2 ** lists
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip4

  function list_zip5 (lst1, lst2, lst3, lst4, lst5) result (lst_z)
    class(*), intent(in) :: lst1, lst2, lst3, lst4, lst5
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst5 ** nil_list
    lists = lst4 ** lists
    lists = lst3 ** lists
    lists = lst2 ** lists
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip5

  function list_zip6 (lst1, lst2, lst3, lst4, lst5, lst6) result (lst_z)
    class(*), intent(in) :: lst1, lst2, lst3, lst4, lst5, lst6
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst6 ** nil_list
    lists = lst5 ** lists
    lists = lst4 ** lists
    lists = lst3 ** lists
    lists = lst2 ** lists
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip6

  function list_zip7 (lst1, lst2, lst3, lst4, lst5, lst6, lst7) result (lst_z)
    class(*), intent(in) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst7 ** nil_list
    lists = lst6 ** lists
    lists = lst5 ** lists
    lists = lst4 ** lists
    lists = lst3 ** lists
    lists = lst2 ** lists
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip7

  function list_zip8 (lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8) result (lst_z)
    class(*), intent(in) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst8 ** nil_list
    lists = lst7 ** lists
    lists = lst6 ** lists
    lists = lst5 ** lists
    lists = lst4 ** lists
    lists = lst3 ** lists
    lists = lst2 ** lists
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip8

  function list_zip9 (lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9) result (lst_z)
    class(*), intent(in) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9
    type(cons_t) :: lst_z

    type(cons_t) :: lists

    lists = lst9 ** nil_list
    lists = lst8 ** lists
    lists = lst7 ** lists
    lists = lst6 ** lists
    lists = lst5 ** lists
    lists = lst4 ** lists
    lists = lst3 ** lists
    lists = lst2 ** lists
    lists = lst1 ** lists
    lst_z = list_zip (lists)
  end function list_zip9

  function list_unzip (lst, n) result (lists)
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lists

    class(*), allocatable :: lst1, row, element
    type(cons_t) :: head, tail
    integer :: i

    lists = make_list (n, nil_list)

    lst1 = lst
    do while (is_cons_pair (lst1))
       call uncons (lst1, row, lst1)
       tail = lists
       lists = nil_list
       do i = 1, n
          call uncons (row, element, row)
          lists = (element ** cons_t_cast (car (tail))) ** lists
          tail = cons_t_cast (cdr (tail))
       end do
       call list_reverse_in_place (lists)
    end do

    tail = lists
    do while (list_is_pair (tail))
       head = cons_t_cast (car (tail))
       call list_reverse_in_place (head)
       call set_car (tail, head)
       tail = cons_t_cast (cdr (tail))
    end do
  end function list_unzip

  subroutine list_unzip1 (lst_zipped, lst1)
    class(*) :: lst_zipped
    type(cons_t) :: lst1

    type(cons_t) :: cursor1

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
    end select
  end subroutine list_unzip1

  subroutine list_unzip2 (lst_zipped, lst1, lst2)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
    end select
  end subroutine list_unzip2

  subroutine list_unzip3 (lst_zipped, lst1, lst2, lst3)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2, lst3

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
          lst3 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst3 = head ** nil_list
          cursor3 = lst3
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor3, new_pair)
             cursor3 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
       lst3 = nil_list
    end select
  end subroutine list_unzip3

  subroutine list_unzip4 (lst_zipped, lst1, lst2, lst3, lst4)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2, lst3, lst4

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
          lst3 = nil_list
          lst4 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst3 = head ** nil_list
          cursor3 = lst3
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst4 = head ** nil_list
          cursor4 = lst4
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor3, new_pair)
             cursor3 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor4, new_pair)
             cursor4 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
       lst3 = nil_list
       lst4 = nil_list
    end select
  end subroutine list_unzip4

  subroutine list_unzip5 (lst_zipped, lst1, lst2, lst3, lst4, lst5)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4
    type(cons_t) :: cursor5

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
          lst3 = nil_list
          lst4 = nil_list
          lst5 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst3 = head ** nil_list
          cursor3 = lst3
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst4 = head ** nil_list
          cursor4 = lst4
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst5 = head ** nil_list
          cursor5 = lst5
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor3, new_pair)
             cursor3 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor4, new_pair)
             cursor4 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor5, new_pair)
             cursor5 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
       lst3 = nil_list
       lst4 = nil_list
       lst5 = nil_list
    end select
  end subroutine list_unzip5

  subroutine list_unzip6 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4
    type(cons_t) :: cursor5
    type(cons_t) :: cursor6

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
          lst3 = nil_list
          lst4 = nil_list
          lst5 = nil_list
          lst6 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst3 = head ** nil_list
          cursor3 = lst3
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst4 = head ** nil_list
          cursor4 = lst4
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst5 = head ** nil_list
          cursor5 = lst5
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst6 = head ** nil_list
          cursor6 = lst6
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor3, new_pair)
             cursor3 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor4, new_pair)
             cursor4 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor5, new_pair)
             cursor5 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor6, new_pair)
             cursor6 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
       lst3 = nil_list
       lst4 = nil_list
       lst5 = nil_list
       lst6 = nil_list
    end select
  end subroutine list_unzip6

  subroutine list_unzip7 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6, lst7)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4
    type(cons_t) :: cursor5
    type(cons_t) :: cursor6
    type(cons_t) :: cursor7

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
          lst3 = nil_list
          lst4 = nil_list
          lst5 = nil_list
          lst6 = nil_list
          lst7 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst3 = head ** nil_list
          cursor3 = lst3
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst4 = head ** nil_list
          cursor4 = lst4
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst5 = head ** nil_list
          cursor5 = lst5
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst6 = head ** nil_list
          cursor6 = lst6
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst7 = head ** nil_list
          cursor7 = lst7
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor3, new_pair)
             cursor3 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor4, new_pair)
             cursor4 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor5, new_pair)
             cursor5 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor6, new_pair)
             cursor6 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor7, new_pair)
             cursor7 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
       lst3 = nil_list
       lst4 = nil_list
       lst5 = nil_list
       lst6 = nil_list
       lst7 = nil_list
    end select
  end subroutine list_unzip7

  subroutine list_unzip8 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4
    type(cons_t) :: cursor5
    type(cons_t) :: cursor6
    type(cons_t) :: cursor7
    type(cons_t) :: cursor8

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
          lst3 = nil_list
          lst4 = nil_list
          lst5 = nil_list
          lst6 = nil_list
          lst7 = nil_list
          lst8 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst3 = head ** nil_list
          cursor3 = lst3
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst4 = head ** nil_list
          cursor4 = lst4
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst5 = head ** nil_list
          cursor5 = lst5
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst6 = head ** nil_list
          cursor6 = lst6
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst7 = head ** nil_list
          cursor7 = lst7
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst8 = head ** nil_list
          cursor8 = lst8
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor3, new_pair)
             cursor3 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor4, new_pair)
             cursor4 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor5, new_pair)
             cursor5 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor6, new_pair)
             cursor6 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor7, new_pair)
             cursor7 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor8, new_pair)
             cursor8 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
       lst3 = nil_list
       lst4 = nil_list
       lst5 = nil_list
       lst6 = nil_list
       lst7 = nil_list
       lst8 = nil_list
    end select
  end subroutine list_unzip8

  subroutine list_unzip9 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9)
    class(*) :: lst_zipped
    type(cons_t) :: lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4
    type(cons_t) :: cursor5
    type(cons_t) :: cursor6
    type(cons_t) :: cursor7
    type(cons_t) :: cursor8
    type(cons_t) :: cursor9

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    select type (lst_zipped)
    class is (cons_t)
       if (list_is_nil (lst_zipped)) then
          lst1 = nil_list
          lst2 = nil_list
          lst3 = nil_list
          lst4 = nil_list
          lst5 = nil_list
          lst6 = nil_list
          lst7 = nil_list
          lst8 = nil_list
          lst9 = nil_list
       else
          call uncons (lst_zipped, head, tail)
          head_zipped = cons_t_cast (head)
          call uncons (head_zipped, head, tl)
          lst1 = head ** nil_list
          cursor1 = lst1
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst2 = head ** nil_list
          cursor2 = lst2
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst3 = head ** nil_list
          cursor3 = lst3
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst4 = head ** nil_list
          cursor4 = lst4
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst5 = head ** nil_list
          cursor5 = lst5
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst6 = head ** nil_list
          cursor6 = lst6
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst7 = head ** nil_list
          cursor7 = lst7
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst8 = head ** nil_list
          cursor8 = lst8
          head_zipped = cons_t_cast (tl)
          call uncons (head_zipped, head, tl)
          lst9 = head ** nil_list
          cursor9 = lst9
          head_zipped = cons_t_cast (tl)
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             head_zipped = cons_t_cast (head)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor1, new_pair)
             cursor1 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor2, new_pair)
             cursor2 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor3, new_pair)
             cursor3 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor4, new_pair)
             cursor4 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor5, new_pair)
             cursor5 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor6, new_pair)
             cursor6 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor7, new_pair)
             cursor7 = new_pair
             head_zipped = cons_t_cast (tl)
             call uncons (head_zipped, head, tl)
             new_pair = head ** nil_list
             call set_cdr (cursor8, new_pair)
             cursor8 = new_pair
             head_zipped = cons_t_cast (tl)
             head = car (head_zipped)
             new_pair = head ** nil_list
             call set_cdr (cursor9, new_pair)
             cursor9 = new_pair
          end do
       end if
    class default
       lst1 = nil_list
       lst2 = nil_list
       lst3 = nil_list
       lst4 = nil_list
       lst5 = nil_list
       lst6 = nil_list
       lst7 = nil_list
       lst8 = nil_list
       lst9 = nil_list
    end select
  end subroutine list_unzip9

  function list_map (func, inputs) result (outputs)
    !
    ! Map elements of lists. The work is guaranteed to be done in
    ! list order.
    !
    ! Both `inputs' and `outputs' may be thought of as sequences of
    ! multiple values in `zipped list' format.
    !
    procedure(list_mapfunc_t) :: func
    class(*), intent(in) :: inputs
    type(cons_t) :: outputs

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair

    select type (inputs)
    class is (cons_t)
       if (list_is_nil (inputs)) then
          outputs = nil_list
       else
          call uncons (inputs, head, tail)
          cursor = func (cons_t_cast (head)) ** nil_list
          outputs = cursor
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             new_pair = func (cons_t_cast (head)) ** nil_list
             call set_cdr (cursor, new_pair)
             cursor = new_pair
          end do
       end if
    class default
       ! Ignore the `tail' of a degenerate dotted list.
       outputs = nil_list
    end select
  end function list_map

end module cons_lists
