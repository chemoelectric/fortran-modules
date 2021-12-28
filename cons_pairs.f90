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
  ! list that ends in a nil is called  proper  and a list that ends in
  ! a back-reference is called  circular .}
  !

  !
  ! NOTE: Fortran procedures do not take variable numbers of
  ! arguments, the way Scheme procedures do. However, the zip
  ! functions can be used to turn multiple-argument problems into
  ! single-argument problems.
  !
  ! FIXME: Extend procedures such as `map' to take more arguments, by
  !        making them generic procedures. One can use m4 to generate
  !        much of the code. Also we can use generics to reserve the
  !        possibility of map taking a function as its proc, instead
  !        of a procedure. (At the time of this writing, gfortran did
  !        not seem to work sensibly if you tried to use a function as
  !        the proc in such cases.)
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

  public :: list_ref0        ! Generic function: return any one of the 0th, 1st, 2nd, etc., elements.
  public :: list_ref1        ! Generic function: Return any one of the 1st, 2nd, 3rd, etc., elements.
  public :: list_refn        ! Generic function: Return any one of the nth, (n+1)th, (n+2)th, etc., elements.
  public :: list_ref0_size_kind ! Versions for INTEGER(int64).
  public :: list_ref1_size_kind
  public :: list_refn_size_kind
  public :: list_ref0_int    ! Versions for INTEGER of the default kind.
  public :: list_ref1_int
  public :: list_refn_int

  ! Make or unmake a list. (SRFI-1 has `list', and its `cons*'
  ! procedure is similar to our `list_with_tail'.)
  public :: list             ! Generic function.
  public :: list_with_tail   ! Generic function.
  public :: unlist           ! Generic function.
  public :: unlist_with_tail ! Generic function.
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

  ! Zipping: joining the elements of separate lists into a list of
  ! lists.
  public :: zip              ! Generic function.
  public :: zip1
  public :: zip2
  public :: zip3
  public :: zip4
  public :: zip5
  public :: zip6
  public :: zip7
  public :: zip8
  public :: zip9
  public :: zip10

  ! Unzipping: separating the elements of a list of lists into
  ! separate lists.
  public :: unzip            ! Generic function.
  public :: unzip1
  public :: unzip2
  public :: unzip3
  public :: unzip4
  public :: unzip5
  public :: unzip6
  public :: unzip7
  public :: unzip8
  public :: unzip9
  public :: unzip10

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
  ! Versions for INTEGER(int64).
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
  public :: make_list_size_kind ! Version for INTEGER(int64).
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
  public :: iota_of_length_size_kind ! Versions for INTEGER(int64).
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

  public :: map              ! Generic function: map list elements in an unspecified order.
  public :: map_in_order     ! Generic function: map list elements left-to-right. (A kind of combination of map and for_each.)
  public :: map1_subr        ! map for 1 lists, with a subroutine as proc.
  public :: map2_subr        ! map for 2 lists, with a subroutine as proc.
  public :: map3_subr        ! map for 3 lists, with a subroutine as proc.
  public :: map4_subr        ! map for 4 lists, with a subroutine as proc.
  public :: map5_subr        ! map for 5 lists, with a subroutine as proc.
  public :: map6_subr        ! map for 6 lists, with a subroutine as proc.
  public :: map7_subr        ! map for 7 lists, with a subroutine as proc.
  public :: map8_subr        ! map for 8 lists, with a subroutine as proc.
  public :: map9_subr        ! map for 9 lists, with a subroutine as proc.
  public :: map10_subr        ! map for 10 lists, with a subroutine as proc.
  public :: map1_in_order_subr ! map_in_order for 1 lists, with a subroutine as proc.
  public :: map2_in_order_subr ! map_in_order for 2 lists, with a subroutine as proc.
  public :: map3_in_order_subr ! map_in_order for 3 lists, with a subroutine as proc.
  public :: map4_in_order_subr ! map_in_order for 4 lists, with a subroutine as proc.
  public :: map5_in_order_subr ! map_in_order for 5 lists, with a subroutine as proc.
  public :: map6_in_order_subr ! map_in_order for 6 lists, with a subroutine as proc.
  public :: map7_in_order_subr ! map_in_order for 7 lists, with a subroutine as proc.
  public :: map8_in_order_subr ! map_in_order for 8 lists, with a subroutine as proc.
  public :: map9_in_order_subr ! map_in_order for 9 lists, with a subroutine as proc.
  public :: map10_in_order_subr ! map_in_order for 10 lists, with a subroutine as proc.

  !public :: for_each         ! Generic function: Perform side effects on list elements, in order from left to right.

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

  ! Types for folds, unfolds, maps, and side effects.
  public :: list_kons_proc_t

  abstract interface
     recursive subroutine list_kons_proc_t (kar, kdr, kons)
       !
       ! The type of the `kons' argument to a fold procedure.
       !
       class(*), intent(in) :: kar, kdr
       class(*), allocatable, intent(out) :: kons
     end subroutine list_kons_proc_t
  end interface

  public :: list_map1_subr_t
  public :: list_map2_subr_t
  public :: list_map3_subr_t
  public :: list_map4_subr_t
  public :: list_map5_subr_t
  public :: list_map6_subr_t
  public :: list_map7_subr_t
  public :: list_map8_subr_t
  public :: list_map9_subr_t
  public :: list_map10_subr_t

  abstract interface
     recursive subroutine list_map1_subr_t (input1, output)
       class(*), intent(in) :: input1
       class(*), allocatable, intent(out) :: output
     end subroutine list_map1_subr_t
     recursive subroutine list_map2_subr_t (input1, input2, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), allocatable, intent(out) :: output
     end subroutine list_map2_subr_t
     recursive subroutine list_map3_subr_t (input1, input2, input3, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), allocatable, intent(out) :: output
     end subroutine list_map3_subr_t
     recursive subroutine list_map4_subr_t (input1, input2, input3, input4, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), allocatable, intent(out) :: output
     end subroutine list_map4_subr_t
     recursive subroutine list_map5_subr_t (input1, input2, input3, input4, input5, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), allocatable, intent(out) :: output
     end subroutine list_map5_subr_t
     recursive subroutine list_map6_subr_t (input1, input2, input3, input4, input5, &
input6, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), allocatable, intent(out) :: output
     end subroutine list_map6_subr_t
     recursive subroutine list_map7_subr_t (input1, input2, input3, input4, input5, &
input6, input7, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), allocatable, intent(out) :: output
     end subroutine list_map7_subr_t
     recursive subroutine list_map8_subr_t (input1, input2, input3, input4, input5, &
input6, input7, input8, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), allocatable, intent(out) :: output
     end subroutine list_map8_subr_t
     recursive subroutine list_map9_subr_t (input1, input2, input3, input4, input5, &
input6, input7, input8, input9, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), intent(in) :: input9
       class(*), allocatable, intent(out) :: output
     end subroutine list_map9_subr_t
     recursive subroutine list_map10_subr_t (input1, input2, input3, input4, input5, &
input6, input7, input8, input9, input10, output)
       class(*), intent(in) :: input1
       class(*), intent(in) :: input2
       class(*), intent(in) :: input3
       class(*), intent(in) :: input4
       class(*), intent(in) :: input5
       class(*), intent(in) :: input6
       class(*), intent(in) :: input7
       class(*), intent(in) :: input8
       class(*), intent(in) :: input9
       class(*), intent(in) :: input10
       class(*), allocatable, intent(out) :: output
     end subroutine list_map10_subr_t
  end interface

  abstract interface
     recursive subroutine list_side_effect_proc_t (input)
       class(*), intent(in) :: input
     end subroutine list_side_effect_proc_t
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

  interface list
     module procedure list1
     module procedure list2
     module procedure list3
     module procedure list4
     module procedure list5
     module procedure list6
     module procedure list7
     module procedure list8
     module procedure list9
     module procedure list10
     module procedure list11
     module procedure list12
     module procedure list13
     module procedure list14
     module procedure list15
     module procedure list16
     module procedure list17
     module procedure list18
     module procedure list19
     module procedure list20
  end interface list

  interface list_with_tail
     module procedure list1_with_tail
     module procedure list2_with_tail
     module procedure list3_with_tail
     module procedure list4_with_tail
     module procedure list5_with_tail
     module procedure list6_with_tail
     module procedure list7_with_tail
     module procedure list8_with_tail
     module procedure list9_with_tail
     module procedure list10_with_tail
     module procedure list11_with_tail
     module procedure list12_with_tail
     module procedure list13_with_tail
     module procedure list14_with_tail
     module procedure list15_with_tail
     module procedure list16_with_tail
     module procedure list17_with_tail
     module procedure list18_with_tail
     module procedure list19_with_tail
     module procedure list20_with_tail
  end interface list_with_tail

  interface unlist
     module procedure unlist1
     module procedure unlist2
     module procedure unlist3
     module procedure unlist4
     module procedure unlist5
     module procedure unlist6
     module procedure unlist7
     module procedure unlist8
     module procedure unlist9
     module procedure unlist10
     module procedure unlist11
     module procedure unlist12
     module procedure unlist13
     module procedure unlist14
     module procedure unlist15
     module procedure unlist16
     module procedure unlist17
     module procedure unlist18
     module procedure unlist19
     module procedure unlist20
  end interface unlist

  interface unlist_with_tail
     module procedure unlist1_with_tail
     module procedure unlist2_with_tail
     module procedure unlist3_with_tail
     module procedure unlist4_with_tail
     module procedure unlist5_with_tail
     module procedure unlist6_with_tail
     module procedure unlist7_with_tail
     module procedure unlist8_with_tail
     module procedure unlist9_with_tail
     module procedure unlist10_with_tail
     module procedure unlist11_with_tail
     module procedure unlist12_with_tail
     module procedure unlist13_with_tail
     module procedure unlist14_with_tail
     module procedure unlist15_with_tail
     module procedure unlist16_with_tail
     module procedure unlist17_with_tail
     module procedure unlist18_with_tail
     module procedure unlist19_with_tail
     module procedure unlist20_with_tail
  end interface unlist_with_tail

  interface zip
     module procedure zip1
     module procedure zip2
     module procedure zip3
     module procedure zip4
     module procedure zip5
     module procedure zip6
     module procedure zip7
     module procedure zip8
     module procedure zip9
     module procedure zip10
  end interface zip

  interface unzip
     module procedure unzip1
     module procedure unzip2
     module procedure unzip3
     module procedure unzip4
     module procedure unzip5
     module procedure unzip6
     module procedure unzip7
     module procedure unzip8
     module procedure unzip9
     module procedure unzip10
  end interface unzip

  interface map
     module procedure map1_subr
     module procedure map2_subr
     module procedure map3_subr
     module procedure map4_subr
     module procedure map5_subr
     module procedure map6_subr
     module procedure map7_subr
     module procedure map8_subr
     module procedure map9_subr
     module procedure map10_subr
  end interface map

  interface map_in_order
     module procedure map1_in_order_subr
     module procedure map2_in_order_subr
     module procedure map3_in_order_subr
     module procedure map4_in_order_subr
     module procedure map5_in_order_subr
     module procedure map6_in_order_subr
     module procedure map7_in_order_subr
     module procedure map8_in_order_subr
     module procedure map9_in_order_subr
     module procedure map10_in_order_subr
  end interface map_in_order

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

  function zip1 (lst1) result (lst_z)
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1

    if (is_not_pair (tail1)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       row = nil
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             row = nil
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip1

  function zip2 (lst1, lst2) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       row = nil
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             row = nil
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip2

  function zip3 (lst1, lst2, lst3) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       row = nil
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             row = nil
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip3

  function zip4 (lst1, lst2, lst3, lst4) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3
    tail4 = .autoval. lst4

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else if (is_not_pair (tail4)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       row = nil
       row = head4 ** row
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else if (is_not_pair (tail4)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             row = nil
             row = head4 ** row
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip4

  function zip5 (lst1, lst2, lst3, lst4, lst5) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3
    tail4 = .autoval. lst4
    tail5 = .autoval. lst5

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else if (is_not_pair (tail4)) then
       lst_z = nil
    else if (is_not_pair (tail5)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       row = nil
       row = head5 ** row
       row = head4 ** row
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else if (is_not_pair (tail4)) then
          continue
       else if (is_not_pair (tail5)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             row = nil
             row = head5 ** row
             row = head4 ** row
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip5

  function zip6 (lst1, lst2, lst3, lst4, lst5, lst6) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3
    tail4 = .autoval. lst4
    tail5 = .autoval. lst5
    tail6 = .autoval. lst6

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else if (is_not_pair (tail4)) then
       lst_z = nil
    else if (is_not_pair (tail5)) then
       lst_z = nil
    else if (is_not_pair (tail6)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       row = nil
       row = head6 ** row
       row = head5 ** row
       row = head4 ** row
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else if (is_not_pair (tail4)) then
          continue
       else if (is_not_pair (tail5)) then
          continue
       else if (is_not_pair (tail6)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             row = nil
             row = head6 ** row
             row = head5 ** row
             row = head4 ** row
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip6

  function zip7 (lst1, lst2, lst3, lst4, lst5, lst6, lst7) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3
    tail4 = .autoval. lst4
    tail5 = .autoval. lst5
    tail6 = .autoval. lst6
    tail7 = .autoval. lst7

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else if (is_not_pair (tail4)) then
       lst_z = nil
    else if (is_not_pair (tail5)) then
       lst_z = nil
    else if (is_not_pair (tail6)) then
       lst_z = nil
    else if (is_not_pair (tail7)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       row = nil
       row = head7 ** row
       row = head6 ** row
       row = head5 ** row
       row = head4 ** row
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else if (is_not_pair (tail4)) then
          continue
       else if (is_not_pair (tail5)) then
          continue
       else if (is_not_pair (tail6)) then
          continue
       else if (is_not_pair (tail7)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             row = nil
             row = head7 ** row
             row = head6 ** row
             row = head5 ** row
             row = head4 ** row
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip7

  function zip8 (lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    class(*), allocatable :: head8, tail8
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3
    tail4 = .autoval. lst4
    tail5 = .autoval. lst5
    tail6 = .autoval. lst6
    tail7 = .autoval. lst7
    tail8 = .autoval. lst8

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else if (is_not_pair (tail4)) then
       lst_z = nil
    else if (is_not_pair (tail5)) then
       lst_z = nil
    else if (is_not_pair (tail6)) then
       lst_z = nil
    else if (is_not_pair (tail7)) then
       lst_z = nil
    else if (is_not_pair (tail8)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       call uncons (tail8, head8, tail8)
       row = nil
       row = head8 ** row
       row = head7 ** row
       row = head6 ** row
       row = head5 ** row
       row = head4 ** row
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else if (is_not_pair (tail4)) then
          continue
       else if (is_not_pair (tail5)) then
          continue
       else if (is_not_pair (tail6)) then
          continue
       else if (is_not_pair (tail7)) then
          continue
       else if (is_not_pair (tail8)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             call uncons (tail8, head8, tail8)
             row = nil
             row = head8 ** row
             row = head7 ** row
             row = head6 ** row
             row = head5 ** row
             row = head4 ** row
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             else if (is_not_pair (tail8)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip8

  function zip9 (lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    class(*), allocatable :: head8, tail8
    class(*), allocatable :: head9, tail9
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3
    tail4 = .autoval. lst4
    tail5 = .autoval. lst5
    tail6 = .autoval. lst6
    tail7 = .autoval. lst7
    tail8 = .autoval. lst8
    tail9 = .autoval. lst9

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else if (is_not_pair (tail4)) then
       lst_z = nil
    else if (is_not_pair (tail5)) then
       lst_z = nil
    else if (is_not_pair (tail6)) then
       lst_z = nil
    else if (is_not_pair (tail7)) then
       lst_z = nil
    else if (is_not_pair (tail8)) then
       lst_z = nil
    else if (is_not_pair (tail9)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       call uncons (tail8, head8, tail8)
       call uncons (tail9, head9, tail9)
       row = nil
       row = head9 ** row
       row = head8 ** row
       row = head7 ** row
       row = head6 ** row
       row = head5 ** row
       row = head4 ** row
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else if (is_not_pair (tail4)) then
          continue
       else if (is_not_pair (tail5)) then
          continue
       else if (is_not_pair (tail6)) then
          continue
       else if (is_not_pair (tail7)) then
          continue
       else if (is_not_pair (tail8)) then
          continue
       else if (is_not_pair (tail9)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             call uncons (tail8, head8, tail8)
             call uncons (tail9, head9, tail9)
             row = nil
             row = head9 ** row
             row = head8 ** row
             row = head7 ** row
             row = head6 ** row
             row = head5 ** row
             row = head4 ** row
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             else if (is_not_pair (tail8)) then
                done = .true.
             else if (is_not_pair (tail9)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip9

  function zip10 (lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9, lst10) result (lst_z)
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_z

    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    class(*), allocatable :: head8, tail8
    class(*), allocatable :: head9, tail9
    class(*), allocatable :: head10, tail10
    type(cons_t) :: row
    type(cons_t) :: new_pair
    type(cons_t) :: cursor
    logical :: done

    tail1 = .autoval. lst1
    tail2 = .autoval. lst2
    tail3 = .autoval. lst3
    tail4 = .autoval. lst4
    tail5 = .autoval. lst5
    tail6 = .autoval. lst6
    tail7 = .autoval. lst7
    tail8 = .autoval. lst8
    tail9 = .autoval. lst9
    tail10 = .autoval. lst10

    if (is_not_pair (tail1)) then
       lst_z = nil
    else if (is_not_pair (tail2)) then
       lst_z = nil
    else if (is_not_pair (tail3)) then
       lst_z = nil
    else if (is_not_pair (tail4)) then
       lst_z = nil
    else if (is_not_pair (tail5)) then
       lst_z = nil
    else if (is_not_pair (tail6)) then
       lst_z = nil
    else if (is_not_pair (tail7)) then
       lst_z = nil
    else if (is_not_pair (tail8)) then
       lst_z = nil
    else if (is_not_pair (tail9)) then
       lst_z = nil
    else if (is_not_pair (tail10)) then
       lst_z = nil
    else
       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       call uncons (tail8, head8, tail8)
       call uncons (tail9, head9, tail9)
       call uncons (tail10, head10, tail10)
       row = nil
       row = head10 ** row
       row = head9 ** row
       row = head8 ** row
       row = head7 ** row
       row = head6 ** row
       row = head5 ** row
       row = head4 ** row
       row = head3 ** row
       row = head2 ** row
       row = head1 ** row
       lst_z = row ** nil
       cursor = lst_z
       if (is_not_pair (tail1)) then
          continue
       else if (is_not_pair (tail2)) then
          continue
       else if (is_not_pair (tail3)) then
          continue
       else if (is_not_pair (tail4)) then
          continue
       else if (is_not_pair (tail5)) then
          continue
       else if (is_not_pair (tail6)) then
          continue
       else if (is_not_pair (tail7)) then
          continue
       else if (is_not_pair (tail8)) then
          continue
       else if (is_not_pair (tail9)) then
          continue
       else if (is_not_pair (tail10)) then
          continue
       else
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             call uncons (tail8, head8, tail8)
             call uncons (tail9, head9, tail9)
             call uncons (tail10, head10, tail10)
             row = nil
             row = head10 ** row
             row = head9 ** row
             row = head8 ** row
             row = head7 ** row
             row = head6 ** row
             row = head5 ** row
             row = head4 ** row
             row = head3 ** row
             row = head2 ** row
             row = head1 ** row
             new_pair = row ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             else if (is_not_pair (tail8)) then
                done = .true.
             else if (is_not_pair (tail9)) then
                done = .true.
             else if (is_not_pair (tail10)) then
                done = .true.
             end if
          end do
       end if
    end if
  end function zip10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine unzip1 (lst_zipped, lst1)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1

    type(cons_t) :: cursor1

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip1, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip1, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip1, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
       end do
    end if
  end subroutine unzip1

  subroutine unzip2 (lst_zipped, lst1, lst2)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip2, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip2, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip2, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip2, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip2, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
       end do
    end if
  end subroutine unzip2

  subroutine unzip3 (lst_zipped, lst1, lst2, lst3)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip3, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip3, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip3, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip3, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip3, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip3, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip3, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
       end do
    end if
  end subroutine unzip3

  subroutine unzip4 (lst_zipped, lst1, lst2, lst3, lst4)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3
    type(cons_t), intent(inout) :: lst4

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
       lst4 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip4, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip4, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip4, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip4, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst4 = head ** nil
       cursor4 = lst4
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip4, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip4, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip4, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip4, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip4, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor4, new_pair)
          cursor4 = new_pair
       end do
    end if
  end subroutine unzip4

  subroutine unzip5 (lst_zipped, lst1, lst2, lst3, lst4, lst5)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3
    type(cons_t), intent(inout) :: lst4
    type(cons_t), intent(inout) :: lst5

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

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
       lst4 = nil
       lst5 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip5, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip5, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip5, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip5, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst4 = head ** nil
       cursor4 = lst4
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip5, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst5 = head ** nil
       cursor5 = lst5
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip5, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip5, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip5, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip5, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip5, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor4, new_pair)
          cursor4 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip5, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor5, new_pair)
          cursor5 = new_pair
       end do
    end if
  end subroutine unzip5

  subroutine unzip6 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3
    type(cons_t), intent(inout) :: lst4
    type(cons_t), intent(inout) :: lst5
    type(cons_t), intent(inout) :: lst6

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

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
       lst4 = nil
       lst5 = nil
       lst6 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip6, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip6, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip6, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip6, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst4 = head ** nil
       cursor4 = lst4
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip6, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst5 = head ** nil
       cursor5 = lst5
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip6, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst6 = head ** nil
       cursor6 = lst6
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip6, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip6, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip6, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip6, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip6, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor4, new_pair)
          cursor4 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip6, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor5, new_pair)
          cursor5 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip6, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor6, new_pair)
          cursor6 = new_pair
       end do
    end if
  end subroutine unzip6

  subroutine unzip7 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6, lst7)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3
    type(cons_t), intent(inout) :: lst4
    type(cons_t), intent(inout) :: lst5
    type(cons_t), intent(inout) :: lst6
    type(cons_t), intent(inout) :: lst7

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

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
       lst4 = nil
       lst5 = nil
       lst6 = nil
       lst7 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst4 = head ** nil
       cursor4 = lst4
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst5 = head ** nil
       cursor5 = lst5
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst6 = head ** nil
       cursor6 = lst6
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst7 = head ** nil
       cursor7 = lst7
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip7, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip7, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip7, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip7, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip7, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor4, new_pair)
          cursor4 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip7, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor5, new_pair)
          cursor5 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip7, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor6, new_pair)
          cursor6 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip7, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor7, new_pair)
          cursor7 = new_pair
       end do
    end if
  end subroutine unzip7

  subroutine unzip8 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3
    type(cons_t), intent(inout) :: lst4
    type(cons_t), intent(inout) :: lst5
    type(cons_t), intent(inout) :: lst6
    type(cons_t), intent(inout) :: lst7
    type(cons_t), intent(inout) :: lst8

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

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
       lst4 = nil
       lst5 = nil
       lst6 = nil
       lst7 = nil
       lst8 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst4 = head ** nil
       cursor4 = lst4
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst5 = head ** nil
       cursor5 = lst5
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst6 = head ** nil
       cursor6 = lst6
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst7 = head ** nil
       cursor7 = lst7
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst8 = head ** nil
       cursor8 = lst8
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip8, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor4, new_pair)
          cursor4 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor5, new_pair)
          cursor5 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor6, new_pair)
          cursor6 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor7, new_pair)
          cursor7 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip8, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor8, new_pair)
          cursor8 = new_pair
       end do
    end if
  end subroutine unzip8

  subroutine unzip9 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3
    type(cons_t), intent(inout) :: lst4
    type(cons_t), intent(inout) :: lst5
    type(cons_t), intent(inout) :: lst6
    type(cons_t), intent(inout) :: lst7
    type(cons_t), intent(inout) :: lst8
    type(cons_t), intent(inout) :: lst9

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

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
       lst4 = nil
       lst5 = nil
       lst6 = nil
       lst7 = nil
       lst8 = nil
       lst9 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst4 = head ** nil
       cursor4 = lst4
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst5 = head ** nil
       cursor5 = lst5
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst6 = head ** nil
       cursor6 = lst6
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst7 = head ** nil
       cursor7 = lst7
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst8 = head ** nil
       cursor8 = lst8
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst9 = head ** nil
       cursor9 = lst9
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip9, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor4, new_pair)
          cursor4 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor5, new_pair)
          cursor5 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor6, new_pair)
          cursor6 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor7, new_pair)
          cursor7 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor8, new_pair)
          cursor8 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip9, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor9, new_pair)
          cursor9 = new_pair
       end do
    end if
  end subroutine unzip9

  subroutine unzip10 (lst_zipped, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9, lst10)
    class(*), intent(in) :: lst_zipped
    type(cons_t), intent(inout) :: lst1
    type(cons_t), intent(inout) :: lst2
    type(cons_t), intent(inout) :: lst3
    type(cons_t), intent(inout) :: lst4
    type(cons_t), intent(inout) :: lst5
    type(cons_t), intent(inout) :: lst6
    type(cons_t), intent(inout) :: lst7
    type(cons_t), intent(inout) :: lst8
    type(cons_t), intent(inout) :: lst9
    type(cons_t), intent(inout) :: lst10

    type(cons_t) :: cursor1
    type(cons_t) :: cursor2
    type(cons_t) :: cursor3
    type(cons_t) :: cursor4
    type(cons_t) :: cursor5
    type(cons_t) :: cursor6
    type(cons_t) :: cursor7
    type(cons_t) :: cursor8
    type(cons_t) :: cursor9
    type(cons_t) :: cursor10

    class(*), allocatable :: head
    class(*), allocatable :: tail
    class(*), allocatable :: tl
    type(cons_t) :: head_zipped
    type(cons_t) :: new_pair

    tail = .autoval. lst_zipped

    if (is_not_pair (tail)) then
       lst1 = nil
       lst2 = nil
       lst3 = nil
       lst4 = nil
       lst5 = nil
       lst6 = nil
       lst7 = nil
       lst8 = nil
       lst9 = nil
       lst10 = nil
    else
       call uncons (tail, head, tail)
       select type (head)
       class is (cons_t)
          head_zipped = head
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst1 = head ** nil
       cursor1 = lst1
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst2 = head ** nil
       cursor2 = lst2
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst3 = head ** nil
       cursor3 = lst3
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst4 = head ** nil
       cursor4 = lst4
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst5 = head ** nil
       cursor5 = lst5
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst6 = head ** nil
       cursor6 = lst6
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst7 = head ** nil
       cursor7 = lst7
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst8 = head ** nil
       cursor8 = lst8
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst9 = head ** nil
       cursor9 = lst9
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       call uncons (head_zipped, head, tl)
       lst10 = head ** nil
       cursor10 = lst10
       select type (tl)
       class is (cons_t)
          head_zipped = tl
       class default
          call error_abort ("in unzip10, expected a cons_t")
       end select
       do while (is_pair (tail))
          call uncons (tail, head, tail)
          select type (head)
          class is (cons_t)
             head_zipped = head
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor1, new_pair)
          cursor1 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor2, new_pair)
          cursor2 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor3, new_pair)
          cursor3 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor4, new_pair)
          cursor4 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor5, new_pair)
          cursor5 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor6, new_pair)
          cursor6 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor7, new_pair)
          cursor7 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor8, new_pair)
          cursor8 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          call uncons (head_zipped, head, tl)
          new_pair = head ** nil
          call set_cdr (cursor9, new_pair)
          cursor9 = new_pair
          select type (tl)
          class is (cons_t)
             head_zipped = tl
          class default
             call error_abort ("in unzip10, expected a cons_t")
          end select
          head = car (head_zipped)
          new_pair = head ** nil
          call set_cdr (cursor10, new_pair)
          cursor10 = new_pair
       end do
    end if
  end subroutine unzip10

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
!!
!! map
!!

  recursive function map1_subr (proc, lst1) result (lst_m)
    procedure(list_map1_subr_t) :: proc
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_m

    lst_m = map1_in_order_subr (proc, lst1)
  end function map1_subr

  recursive function map2_subr (proc, lst1, lst2) result (lst_m)
    procedure(list_map2_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_m

    lst_m = map2_in_order_subr (proc, lst1, lst2)
  end function map2_subr

  recursive function map3_subr (proc, lst1, lst2, lst3) result (lst_m)
    procedure(list_map3_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_m

    lst_m = map3_in_order_subr (proc, lst1, lst2, lst3)
  end function map3_subr

  recursive function map4_subr (proc, lst1, lst2, lst3, lst4) result (lst_m)
    procedure(list_map4_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_m

    lst_m = map4_in_order_subr (proc, lst1, lst2, lst3, lst4)
  end function map4_subr

  recursive function map5_subr (proc, lst1, lst2, lst3, lst4, lst5) result (lst_m)
    procedure(list_map5_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_m

    lst_m = map5_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5)
  end function map5_subr

  recursive function map6_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6) result (lst_m)
    procedure(list_map6_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_m

    lst_m = map6_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6)
  end function map6_subr

  recursive function map7_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7) result (lst_m)
    procedure(list_map7_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_m

    lst_m = map7_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7)
  end function map7_subr

  recursive function map8_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8) result (lst_m)
    procedure(list_map8_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_m

    lst_m = map8_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8)
  end function map8_subr

  recursive function map9_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9) result (lst_m)
    procedure(list_map9_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_m

    lst_m = map9_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9)
  end function map9_subr

  recursive function map10_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9, lst10) result (lst_m)
    procedure(list_map10_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_m

    lst_m = map10_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9, lst10)
  end function map10_subr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! map_in_order
!!

  recursive function map1_in_order_subr (proc, lst1) result (lst_m)
    procedure(list_map1_subr_t) :: proc
    class(*), intent(in) :: lst1
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else
       lst1_root = lst1

       tail1 = .autoval. lst1

       call uncons (tail1, head1, tail1)
       call proc (head1, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call proc (head1, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
    end if
  end function map1_in_order_subr

  recursive function map2_in_order_subr (proc, lst1, lst2) result (lst_m)
    procedure(list_map2_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call proc (head1, head2, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call proc (head1, head2, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
    end if
  end function map2_in_order_subr

  recursive function map3_in_order_subr (proc, lst1, lst2, lst3) result (lst_m)
    procedure(list_map3_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call proc (head1, head2, head3, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call proc (head1, head2, head3, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
    end if
  end function map3_in_order_subr

  recursive function map4_in_order_subr (proc, lst1, lst2, lst3, lst4) result (lst_m)
    procedure(list_map4_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    type(gcroot_t) :: lst4_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else if (is_not_pair (lst4)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3
       lst4_root = lst4

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3
       tail4 = .autoval. lst4

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call proc (head1, head2, head3, head4, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else if (is_not_pair (tail4)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call proc (head1, head2, head3, head4, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
       call lst4_root%discard
    end if
  end function map4_in_order_subr

  recursive function map5_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5) result (lst_m)
    procedure(list_map5_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    type(gcroot_t) :: lst4_root
    type(gcroot_t) :: lst5_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else if (is_not_pair (lst4)) then
       lst_m = nil
    else if (is_not_pair (lst5)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3
       lst4_root = lst4
       lst5_root = lst5

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3
       tail4 = .autoval. lst4
       tail5 = .autoval. lst5

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call proc (head1, head2, head3, head4, head5, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else if (is_not_pair (tail4)) then
          lst_m = cursor
       else if (is_not_pair (tail5)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call proc (head1, head2, head3, head4, head5, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
       call lst4_root%discard
       call lst5_root%discard
    end if
  end function map5_in_order_subr

  recursive function map6_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6) result (lst_m)
    procedure(list_map6_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    type(gcroot_t) :: lst4_root
    type(gcroot_t) :: lst5_root
    type(gcroot_t) :: lst6_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else if (is_not_pair (lst4)) then
       lst_m = nil
    else if (is_not_pair (lst5)) then
       lst_m = nil
    else if (is_not_pair (lst6)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3
       lst4_root = lst4
       lst5_root = lst5
       lst6_root = lst6

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3
       tail4 = .autoval. lst4
       tail5 = .autoval. lst5
       tail6 = .autoval. lst6

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call proc (head1, head2, head3, head4, head5, head6, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else if (is_not_pair (tail4)) then
          lst_m = cursor
       else if (is_not_pair (tail5)) then
          lst_m = cursor
       else if (is_not_pair (tail6)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call proc (head1, head2, head3, head4, head5, head6, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
       call lst4_root%discard
       call lst5_root%discard
       call lst6_root%discard
    end if
  end function map6_in_order_subr

  recursive function map7_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7) result (lst_m)
    procedure(list_map7_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    type(gcroot_t) :: lst4_root
    type(gcroot_t) :: lst5_root
    type(gcroot_t) :: lst6_root
    type(gcroot_t) :: lst7_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else if (is_not_pair (lst4)) then
       lst_m = nil
    else if (is_not_pair (lst5)) then
       lst_m = nil
    else if (is_not_pair (lst6)) then
       lst_m = nil
    else if (is_not_pair (lst7)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3
       lst4_root = lst4
       lst5_root = lst5
       lst6_root = lst6
       lst7_root = lst7

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3
       tail4 = .autoval. lst4
       tail5 = .autoval. lst5
       tail6 = .autoval. lst6
       tail7 = .autoval. lst7

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       call proc (head1, head2, head3, head4, head5, head6, head7, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else if (is_not_pair (tail4)) then
          lst_m = cursor
       else if (is_not_pair (tail5)) then
          lst_m = cursor
       else if (is_not_pair (tail6)) then
          lst_m = cursor
       else if (is_not_pair (tail7)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             call proc (head1, head2, head3, head4, head5, head6, head7, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
       call lst4_root%discard
       call lst5_root%discard
       call lst6_root%discard
       call lst7_root%discard
    end if
  end function map7_in_order_subr

  recursive function map8_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8) result (lst_m)
    procedure(list_map8_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    type(gcroot_t) :: lst4_root
    type(gcroot_t) :: lst5_root
    type(gcroot_t) :: lst6_root
    type(gcroot_t) :: lst7_root
    type(gcroot_t) :: lst8_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    class(*), allocatable :: head8, tail8
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else if (is_not_pair (lst4)) then
       lst_m = nil
    else if (is_not_pair (lst5)) then
       lst_m = nil
    else if (is_not_pair (lst6)) then
       lst_m = nil
    else if (is_not_pair (lst7)) then
       lst_m = nil
    else if (is_not_pair (lst8)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3
       lst4_root = lst4
       lst5_root = lst5
       lst6_root = lst6
       lst7_root = lst7
       lst8_root = lst8

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3
       tail4 = .autoval. lst4
       tail5 = .autoval. lst5
       tail6 = .autoval. lst6
       tail7 = .autoval. lst7
       tail8 = .autoval. lst8

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       call uncons (tail8, head8, tail8)
       call proc (head1, head2, head3, head4, head5, head6, head7, head8, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else if (is_not_pair (tail4)) then
          lst_m = cursor
       else if (is_not_pair (tail5)) then
          lst_m = cursor
       else if (is_not_pair (tail6)) then
          lst_m = cursor
       else if (is_not_pair (tail7)) then
          lst_m = cursor
       else if (is_not_pair (tail8)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             call uncons (tail8, head8, tail8)
             call proc (head1, head2, head3, head4, head5, head6, head7, head8, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             else if (is_not_pair (tail8)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
       call lst4_root%discard
       call lst5_root%discard
       call lst6_root%discard
       call lst7_root%discard
       call lst8_root%discard
    end if
  end function map8_in_order_subr

  recursive function map9_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9) result (lst_m)
    procedure(list_map9_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    type(gcroot_t) :: lst4_root
    type(gcroot_t) :: lst5_root
    type(gcroot_t) :: lst6_root
    type(gcroot_t) :: lst7_root
    type(gcroot_t) :: lst8_root
    type(gcroot_t) :: lst9_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    class(*), allocatable :: head8, tail8
    class(*), allocatable :: head9, tail9
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else if (is_not_pair (lst4)) then
       lst_m = nil
    else if (is_not_pair (lst5)) then
       lst_m = nil
    else if (is_not_pair (lst6)) then
       lst_m = nil
    else if (is_not_pair (lst7)) then
       lst_m = nil
    else if (is_not_pair (lst8)) then
       lst_m = nil
    else if (is_not_pair (lst9)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3
       lst4_root = lst4
       lst5_root = lst5
       lst6_root = lst6
       lst7_root = lst7
       lst8_root = lst8
       lst9_root = lst9

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3
       tail4 = .autoval. lst4
       tail5 = .autoval. lst5
       tail6 = .autoval. lst6
       tail7 = .autoval. lst7
       tail8 = .autoval. lst8
       tail9 = .autoval. lst9

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       call uncons (tail8, head8, tail8)
       call uncons (tail9, head9, tail9)
       call proc (head1, head2, head3, head4, head5, head6, head7, head8, head9, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else if (is_not_pair (tail4)) then
          lst_m = cursor
       else if (is_not_pair (tail5)) then
          lst_m = cursor
       else if (is_not_pair (tail6)) then
          lst_m = cursor
       else if (is_not_pair (tail7)) then
          lst_m = cursor
       else if (is_not_pair (tail8)) then
          lst_m = cursor
       else if (is_not_pair (tail9)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             call uncons (tail8, head8, tail8)
             call uncons (tail9, head9, tail9)
             call proc (head1, head2, head3, head4, head5, head6, head7, head8, head9, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             else if (is_not_pair (tail8)) then
                done = .true.
             else if (is_not_pair (tail9)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
       call lst4_root%discard
       call lst5_root%discard
       call lst6_root%discard
       call lst7_root%discard
       call lst8_root%discard
       call lst9_root%discard
    end if
  end function map9_in_order_subr

  recursive function map10_in_order_subr (proc, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8, lst9, lst10) result (lst_m)
    procedure(list_map10_subr_t) :: proc
    class(*), intent(in) :: lst1
    class(*), intent(in) :: lst2
    class(*), intent(in) :: lst3
    class(*), intent(in) :: lst4
    class(*), intent(in) :: lst5
    class(*), intent(in) :: lst6
    class(*), intent(in) :: lst7
    class(*), intent(in) :: lst8
    class(*), intent(in) :: lst9
    class(*), intent(in) :: lst10
    type(cons_t) :: lst_m

    type(gcroot_t) :: lst1_root
    type(gcroot_t) :: lst2_root
    type(gcroot_t) :: lst3_root
    type(gcroot_t) :: lst4_root
    type(gcroot_t) :: lst5_root
    type(gcroot_t) :: lst6_root
    type(gcroot_t) :: lst7_root
    type(gcroot_t) :: lst8_root
    type(gcroot_t) :: lst9_root
    type(gcroot_t) :: lst10_root
    class(*), allocatable :: head1, tail1
    class(*), allocatable :: head2, tail2
    class(*), allocatable :: head3, tail3
    class(*), allocatable :: head4, tail4
    class(*), allocatable :: head5, tail5
    class(*), allocatable :: head6, tail6
    class(*), allocatable :: head7, tail7
    class(*), allocatable :: head8, tail8
    class(*), allocatable :: head9, tail9
    class(*), allocatable :: head10, tail10
    class(*), allocatable :: proc_result
    logical :: done
    type(gcroot_t) :: retval
    type(cons_t) :: new_pair
    type(cons_t) :: cursor

    if (is_not_pair (lst1)) then
       lst_m = nil
    else if (is_not_pair (lst2)) then
       lst_m = nil
    else if (is_not_pair (lst3)) then
       lst_m = nil
    else if (is_not_pair (lst4)) then
       lst_m = nil
    else if (is_not_pair (lst5)) then
       lst_m = nil
    else if (is_not_pair (lst6)) then
       lst_m = nil
    else if (is_not_pair (lst7)) then
       lst_m = nil
    else if (is_not_pair (lst8)) then
       lst_m = nil
    else if (is_not_pair (lst9)) then
       lst_m = nil
    else if (is_not_pair (lst10)) then
       lst_m = nil
    else
       lst1_root = lst1
       lst2_root = lst2
       lst3_root = lst3
       lst4_root = lst4
       lst5_root = lst5
       lst6_root = lst6
       lst7_root = lst7
       lst8_root = lst8
       lst9_root = lst9
       lst10_root = lst10

       tail1 = .autoval. lst1
       tail2 = .autoval. lst2
       tail3 = .autoval. lst3
       tail4 = .autoval. lst4
       tail5 = .autoval. lst5
       tail6 = .autoval. lst6
       tail7 = .autoval. lst7
       tail8 = .autoval. lst8
       tail9 = .autoval. lst9
       tail10 = .autoval. lst10

       call uncons (tail1, head1, tail1)
       call uncons (tail2, head2, tail2)
       call uncons (tail3, head3, tail3)
       call uncons (tail4, head4, tail4)
       call uncons (tail5, head5, tail5)
       call uncons (tail6, head6, tail6)
       call uncons (tail7, head7, tail7)
       call uncons (tail8, head8, tail8)
       call uncons (tail9, head9, tail9)
       call uncons (tail10, head10, tail10)
       call proc (head1, head2, head3, head4, head5, head6, head7, head8, head9, head10, proc_result)
       cursor = proc_result ** nil
       if (is_not_pair (tail1)) then
          lst_m = cursor
       else if (is_not_pair (tail2)) then
          lst_m = cursor
       else if (is_not_pair (tail3)) then
          lst_m = cursor
       else if (is_not_pair (tail4)) then
          lst_m = cursor
       else if (is_not_pair (tail5)) then
          lst_m = cursor
       else if (is_not_pair (tail6)) then
          lst_m = cursor
       else if (is_not_pair (tail7)) then
          lst_m = cursor
       else if (is_not_pair (tail8)) then
          lst_m = cursor
       else if (is_not_pair (tail9)) then
          lst_m = cursor
       else if (is_not_pair (tail10)) then
          lst_m = cursor
       else
          retval = cursor
          done = .false.
          do while (.not. done)
             call uncons (tail1, head1, tail1)
             call uncons (tail2, head2, tail2)
             call uncons (tail3, head3, tail3)
             call uncons (tail4, head4, tail4)
             call uncons (tail5, head5, tail5)
             call uncons (tail6, head6, tail6)
             call uncons (tail7, head7, tail7)
             call uncons (tail8, head8, tail8)
             call uncons (tail9, head9, tail9)
             call uncons (tail10, head10, tail10)
             call proc (head1, head2, head3, head4, head5, head6, head7, head8, head9, head10, proc_result)
             new_pair = proc_result ** nil
             call set_cdr (cursor, new_pair)
             cursor = new_pair
             if (is_not_pair (tail1)) then
                done = .true.
             else if (is_not_pair (tail2)) then
                done = .true.
             else if (is_not_pair (tail3)) then
                done = .true.
             else if (is_not_pair (tail4)) then
                done = .true.
             else if (is_not_pair (tail5)) then
                done = .true.
             else if (is_not_pair (tail6)) then
                done = .true.
             else if (is_not_pair (tail7)) then
                done = .true.
             else if (is_not_pair (tail8)) then
                done = .true.
             else if (is_not_pair (tail9)) then
                done = .true.
             else if (is_not_pair (tail10)) then
                done = .true.
             end if
          end do
          lst_m = .tocons. retval
       end if

       call lst1_root%discard
       call lst2_root%discard
       call lst3_root%discard
       call lst4_root%discard
       call lst5_root%discard
       call lst6_root%discard
       call lst7_root%discard
       call lst8_root%discard
       call lst9_root%discard
       call lst10_root%discard
    end if
  end function map10_in_order_subr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function fold (kons, knil, lst) result (retval)
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    type(gcroot_t) :: lst_root
    type(gcroot_t) :: retval_root
    class(*), allocatable :: new_retval
    class(*), allocatable :: head, tail

    ! Protect against garbage collections performed by kons.
    lst_root = lst

    retval = knil
    tail = .autoval. lst
    do while (is_pair (tail))
       call uncons (tail, head, tail)
       retval_root = retval
       call kons (head, retval, new_retval)
       retval = new_retval
    end do

    call retval_root%discard
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

    lst_root = lst
    retval = recursion (.autoval. lst)
    call lst_root%discard

  contains

    recursive function recursion (lst) result (retval)
      class(*), intent(in) :: lst
      class(*), allocatable :: retval

      type(gcroot_t) :: recursion_result

      if (is_not_pair (lst)) then
         retval = knil
      else
         recursion_result = recursion (cdr (lst))
         call kons (car (lst), .val. recursion_result, retval)
      end if
    end function recursion

  end function fold_right

  recursive function pair_fold (kons, knil, lst) result (retval)
    procedure(list_kons_proc_t) :: kons
    class(*), intent(in) :: knil
    class(*), intent(in) :: lst
    class(*), allocatable :: retval

    class(*), allocatable :: new_retval
    type(gcroot_t) :: tail, new_tail

    type(gcroot_t) :: lst_root
    type(gcroot_t) :: retval_root

    ! Protect against garbage collections performed by kons.
    lst_root = lst

    retval = knil
    tail = lst_root
    do while (is_pair (tail))
       new_tail = cdr (tail)
       retval_root = retval
       call kons (.val. tail, retval, new_retval)
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

    lst_root = lst
    retval = recursion (.autoval. lst)
    call lst_root%discard

  contains

    recursive function recursion (lst) result (retval)
      class(*), intent(in) :: lst
      class(*), allocatable :: retval

      type(gcroot_t) :: recursion_result

      if (is_not_pair (lst)) then
         retval = knil
      else
         recursion_result = recursion (cdr (lst))
         call kons (lst, .val. recursion_result, retval)
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
      type(gcroot_t) :: recursion_result

      if (is_pair (lst1)) then
         call uncons (lst1, hd, tl)
         recursion_result = recursion (hd, tl)
         call kons (head, .val. recursion_result, retval)
      else
         retval = head
      end if
    end function recursion

  end function reduce_right

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function unfold_with_tail_gen (pred, f, g, seed, tail_gen) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    procedure(list_map1_subr_t) :: tail_gen
    class(*), allocatable :: lst

    lst = recursion (.autoval. seed)

  contains

    recursive function recursion (seed) result (retval)
      class(*), intent(in) :: seed
      class(*), allocatable :: retval

      class(*), allocatable :: elem
      class(*), allocatable :: sd
      type(gcroot_t) :: new_element
      type(gcroot_t) :: old_seed
      type(gcroot_t) :: new_seed
      type(gcroot_t) :: recursion_result

      old_seed = seed
      if (pred (.val. old_seed)) then
         call tail_gen (.val. old_seed, retval)
      else
         call f (.val. old_seed, elem)
         new_element = elem
         call g (.val. old_seed, sd)
         new_seed = sd
         recursion_result = recursion (.val. new_seed)
         retval = cons (.val. new_element, recursion_result)
      end if
    end function recursion

  end function unfold_with_tail_gen

  recursive function unfold_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst

    lst = recursion (.autoval. seed)

  contains

    recursive function recursion (seed) result (retval)
      class(*), intent(in) :: seed
      class(*), allocatable :: retval

      class(*), allocatable :: elem
      class(*), allocatable :: sd
      type(gcroot_t) :: new_element
      type(gcroot_t) :: old_seed
      type(gcroot_t) :: new_seed
      type(gcroot_t) :: recursion_result

      old_seed = seed
      if (pred (.val. old_seed)) then
         retval = nil
      else
         call f (.val. old_seed, elem)
         new_element = elem
         call g (.val. old_seed, sd)
         new_seed = sd
         recursion_result = recursion (.val. new_seed)
         retval = cons (.val. new_element, recursion_result)
      end if
    end function recursion

  end function unfold_with_nil_tail

  recursive function unfold_right_with_tail (pred, f, g, seed, tail) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    class(*), intent(in) :: tail
    class(*), allocatable :: lst

    class(*), allocatable :: elem
    class(*), allocatable :: sd
    type(gcroot_t) :: new_element
    type(gcroot_t) :: current_seed
    type(gcroot_t) :: retval

    retval = tail
    current_seed = seed
    do while (.not. pred (.val. current_seed))
       call f (.val. current_seed, elem)
       new_element = elem
       retval = cons (.val. new_element, .val. retval)
       call g (.val. current_seed, sd)
       current_seed = sd
    end do
    lst = .val. retval
  end function unfold_right_with_tail

  recursive function unfold_right_with_nil_tail (pred, f, g, seed) result (lst)
    procedure(list_predicate1_t) :: pred
    procedure(list_map1_subr_t) :: f
    procedure(list_map1_subr_t) :: g
    class(*), intent(in) :: seed
    class(*), allocatable :: lst

    lst = unfold_right_with_tail (pred, f, g, seed, nil)
  end function unfold_right_with_nil_tail

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module cons_pairs
