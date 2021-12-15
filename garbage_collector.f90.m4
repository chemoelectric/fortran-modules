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

module garbage_collector

  use, intrinsic :: iso_fortran_env, only: int8
  use, intrinsic :: iso_fortran_env, only: int64
  use :: unused_variables

  implicit none
  private

  integer, parameter :: size_kind = int64

  integer, parameter :: bits_kind = int8
  integer(bits_kind), parameter :: mark_bit = int (b'00000001')

  type :: heap_element_t
     class(*), pointer :: data => null ()              ! The actual data.
     class(heap_element_t), pointer :: prev => null () ! The previous object in the heap.
     class(heap_element_t), pointer :: next => null () ! The next object in the heap.
     integer(bits_kind) :: bits = 0                    ! The mark bit and any other such bits.
  end type heap_element_t

  ! The head and tail of a doubly-linked circular list, representing
  ! the current contents of the heap.
  class(heap_element_t), pointer :: heap => null ()
  integer(size_kind) :: heap_count = 0 ! The current number of entries, NOT counting the head-tail.

  ! Extend this type to represent different kinds of heap entries.
  type :: collectible_t
     class(heap_element_t), pointer :: heap_element => null () ! A pointer a node in the heap.
   contains
     procedure, pass :: get_branch => collectible_t_get_branch ! Called to find `reachable' nodes.
  end type collectible_t

  ! Use this type to store the collectible_t items you have
  ! constructed into trees, graphs, etc.
  type :: root_t
     class(collectible_t), pointer :: collectible => null ()
     class(root_t), pointer :: prev => null () ! The previous root in the roots list.
     class(root_t), pointer :: next => null () ! The next root in the roots list.
   contains
     final :: root_t_finalize
  end type root_t

  ! The head and tail of a doubly-linked circular list, representing
  ! the current roots.
  class(root_t), pointer :: roots => null ()
  integer(size_kind) :: roots_count = 0 ! The current number of entries, NOT counting the head-tail.

  type :: nil_branch_t
     ! Contains nothing.
  end type nil_branch_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  elemental subroutine set_bit (obj, bit_mask)
    class(heap_element_t), intent(inout) :: obj
    integer(bits_kind), intent(in) :: bit_mask
    obj%bits = ior (obj%bits, bit_mask)
  end subroutine set_bit

  elemental subroutine clear_bit (obj, bit_mask)
    class(heap_element_t), intent(inout) :: obj
    integer(bits_kind), intent(in) :: bit_mask
    obj%bits = iand (obj%bits, not (bit_mask))
  end subroutine clear_bit

  elemental function bit_is_set (obj, bit_mask) result (bool)
    class(heap_element_t), intent(in) :: obj
    integer(bits_kind), intent(in) :: bit_mask
    logical :: bool
    bool = (iand (obj%bits, bit_mask) /= 0)
  end function bit_is_set

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  elemental subroutine set_marked (obj)
    class(heap_element_t), intent(inout) :: obj
    call set_bit (obj, mark_bit)
  end subroutine set_marked

  elemental subroutine set_unmarked (obj)
    class(heap_element_t), intent(inout) :: obj
    call clear_bit (obj, mark_bit)
  end subroutine set_unmarked

  elemental function is_marked (obj) result (bool)
    class(heap_element_t), intent(in) :: obj
    logical :: bool
    bool = bit_is_set (obj, mark_bit)
  end function is_marked

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialize_heap
    if (.not. associated (heap)) then
       allocate (heap)
       heap%prev => heap
       heap%next => heap
       heap_count = 0
    end if
  end subroutine initialize_heap

  function is_heap_head (p) result (bool)
    class(heap_element_t), pointer, intent(in) :: p
    logical :: bool
    bool = associated (p, heap)
  end function is_heap_head

  subroutine heap_insert (after_this, data, new_element)
    class(heap_element_t), pointer, intent(in) :: after_this
    class(*), intent(in) :: data
    class(heap_element_t), pointer, intent(out) :: new_element

    call initialize_heap

    allocate (new_element)
    allocate (new_element%data, source = data)
    new_element%next => after_this%next
    new_element%prev => after_this
    after_this%next => new_element

    heap_count = heap_count + 1
  end subroutine heap_insert

  subroutine heap_remove (this_one)
    class(heap_element_t), pointer, intent(inout) :: this_one

    this_one%prev%next => this_one%next
    this_one%next%prev => this_one%prev
    deallocate (this_one%data)
    deallocate (this_one)

    heap_count = heap_count - 1
  end subroutine heap_remove

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialize_roots
    if (.not. associated (roots)) then
       allocate (roots)
       roots%prev => roots
       roots%next => roots
    end if
  end subroutine initialize_roots

  function is_roots_head (p) result (bool)
    class(root_t), pointer, intent(in) :: p
    logical :: bool
    bool = associated (p, roots)
  end function is_roots_head

  subroutine roots_insert (after_this, collectible, new_root)
    class(root_t), pointer, intent(in) :: after_this
    class(collectible_t), intent(in) :: collectible
    class(root_t), pointer, intent(out) :: new_root

    call initialize_roots

    allocate (new_root)
    allocate (new_root%collectible, source = collectible)
    new_root%next => after_this%next
    new_root%prev => after_this
    after_this%next => new_root

    roots_count = roots_count + 1
  end subroutine roots_insert

  subroutine roots_remove (this_one)
    class(root_t), pointer, intent(inout) :: this_one

    this_one%prev%next => this_one%next
    this_one%next%prev => this_one%prev
    deallocate (this_one%collectible)
    deallocate (this_one)

    roots_count = roots_count - 1
  end subroutine roots_remove

  subroutine root_t_finalize (this)
    type(root_t), target, intent(inout) :: this

    class(root_t), pointer :: this_one

    this_one => this
    call roots_remove (this_one)
  end subroutine root_t_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function branch_is_nil (x) result (bool)
    class(*), intent(in) :: x
    logical :: bool
    select type (x)
    type is (nil_branch_t)
       bool = .true.
    class default
       bool = .false.
    end select
  end function branch_is_nil

  function collectible_t_get_branch (this, branch_number) result (branch)
    !
    ! `get_branch' returns the `branch_numberth' branch element of
    ! `this', for use in the mark phase to find reachable heap
    ! entries. Numbering starts at 1 and proceeds consecutively; an
    ! out of range positive number shall cause the return value to be
    ! a `type(nil_branch_t)' object.
    !
    ! The base type of the class contains no data and so can return no
    ! branches.
    !
    class(collectible_t), intent(in) :: this
    integer(size_kind), intent(in) :: branch_number
    class(*), allocatable :: branch

    call unused_variable (this)
    call unused_variable (branch_number)

    branch = nil_branch_t ()
  end function collectible_t_get_branch

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! THE MARK PHASE.
!!

  subroutine mark_from_roots

    type :: work_stack_element_t
       class(collectible_t), pointer :: collectible
       type(work_stack_element_t), pointer :: next
    end type work_stack_element_t

    type(work_stack_element_t), pointer :: work_stack => null ()

    class(root_t), pointer :: root

    root => roots
    do while (.not. is_roots_head (root))
       ! Mark the root object for keeping.
       call set_marked (root%collectible%heap_element)

       ! Push the root object to the stack for reachability analysis.
       block
         type(work_stack_element_t), pointer :: tmp
         allocate (tmp)
         tmp%collectible => root%collectible
         tmp%next => work_stack
         work_stack => tmp
       end block

       ! Find things that can be reached from the root object.
       call mark_reachables

       root => root%next
    end do

  contains

    subroutine mark_reachables

      class(collectible_t), pointer :: collectible

      integer(size_kind) :: branch_number
      class(*), allocatable :: branch

      do while (associated (work_stack))
         ! Pop the top of the stack.
         collectible => work_stack%collectible
         block
           type(work_stack_element_t), pointer :: tmp
           tmp => work_stack
           deallocate (work_stack)
           work_stack => tmp
         end block

         branch_number = 1
         branch = collectible%get_branch (branch_number)
         do while (.not. branch_is_nil (branch))
            select type (branch)
            class is (collectible_t)
               ! The branch is a reachable object; mark it for keeping.
               call set_marked (branch%heap_element)

               ! Push the object to the stack, to see if anything can
               ! be reached through it.
               block
                 type(work_stack_element_t), pointer :: tmp
                 allocate (tmp)
                 tmp%collectible => root%collectible
                 tmp%next => work_stack
                 work_stack => tmp
               end block
            class default
               ! Ignore anything that is not `class(collectible_t)'.
               continue
            end select
            branch_number = branch_number + 1
            branch = collectible%get_branch (branch_number)
         end do
      end do
    end subroutine mark_reachables

  end subroutine mark_from_roots

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! THE SWEEP PHASE.
!!

  subroutine sweep
    class(heap_element_t), pointer :: heap_element

    heap_element => heap
    do while (.not. is_heap_head (heap_element))
       if (is_marked (heap_element)) then
          ! Keep this heap element.
          call set_unmarked (heap_element)
       else
          call heap_remove (heap_element)
       end if
       heap_element => heap_element%next
    end do
  end subroutine sweep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module garbage_collector