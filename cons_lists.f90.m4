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
dnl

module cons_lists
  !
  !
  ! Lisp-style CONS-pairs for Fortran, and a suite of list routines.
  !
  ! The names of routines are based loosely on those in the Scheme
  ! Request for Implementation SRFI-1 (List Library).  See
  ! https://srfi.schemers.org/srfi-1/srfi-1.html
  !
  !

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

  public :: assume_list         ! Assume an object is a cons_t.

  public :: list_length    ! The length of a *proper* CONS-list.
  public :: is_proper_list ! Is an object a list but neither dotted nor circular?
  public :: is_dotted_object ! Is an object a non-list or a dotted list?
  public :: is_dotted_list ! Is an object a dotted list (but not a non-list)?
  public :: is_circular_list    ! Is an object a circular list?

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

  public :: list_last      ! Return the last element of a proper list.
  public :: list_last_pair ! Return the last CONS-pair of a (possibly dotted) list.

  public :: make_list ! Make a list that is one element value repeated.

  ! Return a list that is a sequence of integers. (The name `IOTA' is
  ! taken from SRFI-1 and was inspired by APL.)
  public :: iota        ! The overloaded identifier for the following.
  public :: iota_given_length
  public :: iota_given_length_start
  public :: iota_given_length_start_step

  public :: circular_list       ! Make a circular list.

  public :: list_reverse          ! Make a reversed copy.
  public :: list_reverse_in_place ! Reverse a list without copying.
  public :: list_copy             ! Make a copy in the original order.
  public :: list_take             ! Copy the first n elements.
  public :: list_drop ! Drop the first n elements (by performing n CDR operations).
  public :: list_take_right ! Roughly, `return the last n elements' (but see SRFI-1).

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

  ! A private type representing the CAR-CDR-tuple of a CONS-pair.
  type :: cons_pair_t
     class(*), allocatable, private :: car
     class(*), allocatable, private :: cdr
  end type cons_pair_t

  ! A public type that is a NIL-list or a reference to a
  ! CAR-CDR-tuple. The class of a Lisp-like list structure is
  ! `class(cons_t)'.
  type :: cons_t
     class(cons_pair_t), pointer, private :: p => null ()
  end type cons_t

  ! The canonical NIL-list.
  type(cons_t), parameter :: nil_list = cons_t (null ())

contains

  subroutine error_abort (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("cons_lists error: ", a)') msg
    CALL_ABORT
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
       call error_abort ("uncons of non-list")
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

  function assume_list (obj) result (lst)
    class(*), intent(in) :: obj
    type(cons_t) :: lst
    select type (obj)
    class is (cons_t)
       lst = obj
    class default
       call error_abort ("assume_list with non-list element")
    end select
  end function assume_list

  function list_length (lst) result (length)
    class(*), intent(in) :: lst
    integer :: length

    class(cons_t), allocatable :: tail

    select type (lst)
    class is (cons_t)
       length = 0
       tail = lst
       do while (is_cons_pair (tail))
          length = length + 1
          tail = cdr (tail)
       end do
       if (.not. is_nil_list (tail)) then
          call error_abort ("list_length of a dotted list")
       end if
    class default
       call error_abort ("list_length of a non-list")
    end select
  end function list_length

  function is_proper_list (obj) result (is_proper)
    class(*), intent(in) :: obj
    logical :: is_proper

    class(*), allocatable :: lead, lag
    logical :: done

    is_proper = .true.
    lead = obj
    lag = obj
    done = .false.
    do while (.not. done)
       if (.not. is_cons_pair (lead)) then
          is_proper = is_nil_list (lead)
          done = .true.
       else
          lead = cdr (lead)
          if (.not. is_cons_pair (lead)) then
             is_proper = is_nil_list (lead)
             done = .true.
          else
             lead = cdr (lead)
             lag = cdr (lag)
             select type (lead)
             class is (cons_t)
                select type (lag)
                class is (cons_t)
                   if (cons_t_eq (lead, lag)) then
                      is_proper = .false. ! Circular list.
                      done = .true.
                   end if
                end select
             end select
          end if
       end if
    end do
  end function is_proper_list

  function is_dotted_object (obj) result (is_dotted)
    !
    ! `is_dotted_object(4)', etc., return .true.
    !
    ! This peculiar behavior is that of `dotted-list?' in SRFI-1, and
    ! has a logic to it. In particular:
    !
    !    .not. is_dotted_object (x)
    !
    ! is equivalent to
    !
    !    is_proper_list (x) .or. is_circular_list (x)
    !
    class(*), intent(in) :: obj
    logical :: is_dotted

    class(*), allocatable :: lead, lag
    logical :: done

    is_dotted = .true.
    lead = obj
    lag = obj
    done = .false.
    do while (.not. done)
       if (.not. is_cons_pair (lead)) then
          is_dotted = .not. is_nil_list (lead)
          done = .true.
       else
          lead = cdr (lead)
          if (.not. is_cons_pair (lead)) then
             is_dotted = .not. is_nil_list (lead)
             done = .true.
          else
             lead = cdr (lead)
             lag = cdr (lag)
             select type (lead)
             class is (cons_t)
                select type (lag)
                class is (cons_t)
                   if (cons_t_eq (lead, lag)) then
                      is_dotted = .false. ! Circular list.
                      done = .true.
                   end if
                end select
             end select
          end if
       end if
    end do
  end function is_dotted_object

  function is_dotted_list (obj) result (is_dotted)
    !
    ! Fortran programmers may find this function more `friendly' than
    ! is_dotted_object, given that CONS is not a fundamental part of
    ! Fortran (as it is in Scheme).
    !
    !    is_dotted_list (x)
    !
    ! is equivalent to
    !
    !    is_dotted_object (x) .and. is_cons_pair (x)
    !
    ! Note that `is_dotted_object (nil_list)' is .false.
    !
    class(*), intent(in) :: obj
    logical :: is_dotted

    select type (obj)
    class is (cons_t)
       is_dotted = is_dotted_object (obj)
    class default
       is_dotted = .false.
    end select  
  end function is_dotted_list

  function is_circular_list (obj) result (is_circular)
    class(*), intent(in) :: obj
    logical :: is_circular

    class(*), allocatable :: lead, lag
    logical :: done

    is_circular = .false.
    lead = obj
    lag = obj
    done = .false.
    do while (.not. done)
       if (.not. is_cons_pair (lead)) then
          done = .true.
       else
          lead = cdr (lead)
          if (.not. is_cons_pair (lead)) then
             done = .true.
          else
             lead = cdr (lead)
             lag = cdr (lag)
             select type (lead)
             class is (cons_t)
                select type (lag)
                class is (cons_t)
                   if (cons_t_eq (lead, lag)) then
                      is_circular = .true.
                      done = .true.
                   end if
                end select
             end select
          end if
       end if
    end do
  end function is_circular_list

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
       call error_abort ("car of non-list")
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
       call error_abort ("cdr of non-list")
    end select
  end function cdr

m4_forloop([n],[2],CADADR_MAX,[m4_length_n_cadadr_definitions(n)])dnl
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
    if (is_nil_list (y)) then
       element = x
    else
       call error_abort ("list_last of dotted list")
    end if
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
             lst1 = assume_list (tail)
             tail = cdr (lst1)
          end do
          last_pair = lst1
       else
          call error_abort ("list_last_pair of empty list")
       end if
    class default
       call error_abort ("list_last_pair of non-list")
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
          call error_abort ("circular_list of empty list")
       end if
    class default
       call error_abort ("circular_list of non-list")
    end select
  end function circular_list

  function list_reverse (lst) result (lst_r)
    class(*), intent(in) :: lst
    type(cons_t) :: lst_r

    class(*), allocatable :: tail
    
    select type (lst)
    class is (cons_t)
       lst_r = nil_list
       tail = lst
       do while (is_cons_pair (tail))
          lst_r = cons (car (tail), lst_r)
          tail = cdr (tail)
       end do
       if (.not. is_nil_list (tail)) then
          call error_abort ("list_reverse of a dotted list")
       end if
    class default
       call error_abort ("list_reverse of a non-list")
    end select
  end function list_reverse

  subroutine list_reverse_in_place (lst)
    type(cons_t), intent(inout) :: lst

    type(cons_t) :: lst_r
    type(cons_t) :: tail

    lst_r = nil_list
    do while (is_cons_pair (lst))
       tail = assume_list (cdr (lst))
       call set_cdr (lst, lst_r)
       lst_r = lst
       lst = tail
    end do
    lst = lst_r
  end subroutine list_reverse_in_place

  function list_copy (lst) result (lst_c)
    class(*), intent(in) :: lst
    type(cons_t) :: lst_c

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
          lst_c = cons (head, tail)
          cursor = lst_c
          do while (is_cons_pair (tail))
             call uncons (tail, head, tail)
             new_pair = cons (head, tail)
             call set_cdr (cursor, new_pair)
             cursor = new_pair
          end do
       end if
    class default
       call error_abort ("list_copy of a non-list")
    end select
  end function list_copy

  function list_take (lst, n) result (lst_t)
    !
    ! list_take *copies* the first n `CAR' elements, and returns a
    ! list.
    !
    ! lst may be dotted or circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    type(cons_t) :: lst_t

    class(*), allocatable :: head
    class(*), allocatable :: tail
    type(cons_t) :: cursor
    type(cons_t) :: new_pair
    integer :: i

    select type (lst)
    class is (cons_t)
       if (n <= 0 .or. list_is_nil (lst)) then
          lst_t = nil_list
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
       call error_abort ("list_take of a non-list")
    end select
  end function list_take

  function list_drop (lst, n) result (obj)
    !
    ! list_drop does *not* copy the elements it `keeps'; it is
    ! equivalent to repeating CDR n times. The result might *not* be a
    ! list.
    !
    ! lst may be dotted or circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: obj

    integer :: i

    select type (lst)
    class is (cons_t)
       obj = lst
       do i = 1, n
          select type (lst => obj)
          class is (cons_t)
             if (list_is_pair (lst)) then
                obj = cdr (lst)
             else
                call error_abort ("list_drop of a list that is too short")
             end if
          end select
       end do
    class default
       call error_abort ("list_drop of a non-list")
    end select
  end function list_drop

  function list_take_right (lst, n) result (obj)
    !
    ! The result might *not* be a list.
    !
    ! lst may be dotted, but must not be circular.
    !
    class(*), intent(in) :: lst
    integer, intent(in) :: n
    class(*), allocatable :: obj

    class(*), allocatable :: lead, lag

    select type (lst)
    class is (cons_t)
       lag = lst
       lead = list_drop (lst, n)
       do while (is_cons_pair (lead))
          lag = cdr (lag)
          lead = cdr (lead)
       end do
       obj = lag
    class default
       call error_abort ("list_take_right of a non-list")
    end select
  end function list_take_right

end module cons_lists
