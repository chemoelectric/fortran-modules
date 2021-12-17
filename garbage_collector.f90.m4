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

  use, intrinsic :: iso_c_binding ! In case someone chooses a C type for size_kind.
  use, intrinsic :: iso_fortran_env
  use, non_intrinsic :: unused_variables

  implicit none
  private

  public :: size_kind
  public :: heap_element_t ! FIXME: Is there a good way, or a need, to make this private?
  public :: collectible_t
  public :: gcroot_t
  public :: nil_branch_t
  public :: current_heap_size
  public :: current_roots_count
  public :: heap_insert
  public :: initialize_garbage_collector
  public :: collect_garbage_now

  integer, parameter :: size_kind = SIZE_KIND

  integer, parameter :: bits_kind = int8
  integer(bits_kind), parameter :: mark_bit = int (b'00000001')

  type :: heap_element_t
     class(*), pointer :: data => null ()              ! The actual data.
     class(heap_element_t), pointer :: prev => null () ! The previous object in the heap.
     class(heap_element_t), pointer :: next => null () ! The next object in the heap.
     integer(bits_kind) :: bits = 0                    ! The mark bit and any other such bits.
   contains
     procedure, pass :: insert => heap_element_t_insert
     procedure, pass :: remove => heap_element_t_remove
  end type heap_element_t

  ! Extend this type to represent different kinds of heap entries.
  type :: collectible_t
     class(heap_element_t), pointer :: heap_element => null () ! A pointer a node in the heap.
   contains
     procedure, pass :: get_branch => collectible_t_get_branch ! Called to find `reachable' nodes.
  end type collectible_t

  type :: root_t
     class(collectible_t), pointer :: collectible => null ()
     class(root_t), pointer :: prev => null () ! The previous root in the roots list.
     class(root_t), pointer :: next => null () ! The next root in the roots list.
  end type root_t

  ! Use this type to store the collectible_t items you have
  ! constructed into trees, graphs, etc. It may also store
  ! non-collectible values.
  type :: gcroot_t
     class(*), pointer :: root => null ()
   contains
     procedure, pass :: get_value => gcroot_t_get_value
     procedure, pass :: get_pointer => gcroot_t_get_pointer
     procedure, pass :: assign => gcroot_t_assign
     generic :: assignment(=) => assign
     final :: gcroot_t_finalize
  end type gcroot_t

  type :: nil_branch_t
     ! Contains nothing. This type is used as a sentinel.
  end type nil_branch_t
  
  ! The head and tail of a doubly-linked circular list,
  ! representing the current contents of the heap.
  class(heap_element_t), pointer :: heap => null ()

  ! The current number of heap elements, NOT counting the
  ! `heap' node.
  integer(size_kind) :: heap_count = 0

  ! The head and tail of a doubly-linked circular list,
  ! representing the current roots.
  class(root_t), pointer :: roots => null ()

  ! The current number of roots, NOT counting the `roots' node.
  integer(size_kind) :: roots_count = 0

  interface error_abort
     module procedure error_abort_1
  end interface error_abort

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine error_abort_1 (msg)
    use iso_fortran_env, only : error_unit
    character(*), intent(in) :: msg
    write (error_unit, '()')
    write (error_unit, '("module garbage_collector error: ", a)') msg
    error stop
  end subroutine error_abort_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pure function current_heap_size () result (size)
    integer(size_kind) :: size
    size = heap_count
  end function current_heap_size

  pure function current_roots_count () result (count)
    integer(size_kind) :: count
    count = roots_count
  end function current_roots_count

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

  subroutine heap_insert (new_element)
    class(heap_element_t), pointer, intent(in) :: new_element
    call initialize_garbage_collector
    call heap%insert (new_element)
  end subroutine heap_insert

  subroutine heap_element_t_insert (after_this, new_element)
    class(heap_element_t), target, intent(inout) :: after_this
    class(heap_element_t), pointer, intent(in) :: new_element

    new_element%next => after_this%next
    new_element%prev => after_this
    after_this%next => new_element

    heap_count = heap_count + 1
  end subroutine heap_element_t_insert

  subroutine heap_element_t_remove (this_one)

    !
    ! NOTE: Deallocating the heap_element_t itself is the
    !       responsibility of the caller.
    !

    class(heap_element_t), intent(inout) :: this_one

    this_one%prev%next => this_one%next
    this_one%next%prev => this_one%prev
    if (associated (this_one%data)) then
       deallocate (this_one%data)
    end if

    heap_count = heap_count - 1
  end subroutine heap_element_t_remove

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

    m4_if(DEBUGGING,[true],[write (*,*) "inserting a root into the roots list"])
    m4_if(DEBUGGING,[true],[write (*,*) "   inserting at head of roots list?     ", is_roots_head (after_this)])

    allocate (new_root)
    allocate (new_root%collectible, source = collectible)
    new_root%next => after_this%next
    new_root%prev => after_this
    after_this%next => new_root

    roots_count = roots_count + 1
  end subroutine roots_insert

  subroutine roots_remove (this_one)
    class(root_t), pointer, intent(inout) :: this_one
    m4_if(DEBUGGING,[true],[write (*,*) "removing a root from the roots list"])
    this_one%prev%next => this_one%next
    this_one%next%prev => this_one%prev
    if (associated (this_one%collectible)) then
       deallocate (this_one%collectible)
    end if
    roots_count = roots_count - 1
    m4_if(DEBUGGING,[true],[write (*,*) "   new roots_count = ", roots_count])
  end subroutine roots_remove

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function gcroot_t_get_value (this) result (retval)
    class(gcroot_t), intent(in) :: this
    class(*), allocatable :: retval
    select type (root => this%root)
    class is (root_t)
       retval = root%collectible
    class default
       retval = root
    end select
  end function gcroot_t_get_value

  function gcroot_t_get_pointer (this) result (ptr)
    class(gcroot_t), intent(in) :: this
    class(collectible_t), pointer :: ptr
    select type (root => this%root)
    class is (root_t)
       ptr => root%collectible
    class default
       call error_abort ("gcroot_t_get_pointer of a non-collectible object")
    end select
  end function gcroot_t_get_pointer

  subroutine gcroot_t_assign (dst, src)
    class(gcroot_t), intent(inout) :: dst
    class(*), intent(in) :: src

    m4_if(DEBUGGING,[true],[write (*,*) "roots_count before = ", roots_count])

    select type (src)
    class is (collectible_t)
       ! Create a new root.
       m4_if(DEBUGGING,[true],[write (*,*) "gcroot_t_assign of a collectible_t"])
       block
         class(root_t), pointer :: new_root
         call roots_insert (roots, src, new_root)
         dst%root => new_root
       end block
    class is (gcroot_t)
       m4_if(DEBUGGING,[true],[write (*,*) "gcroot_t_assign of a gcroot_t"])
       select type (root => src%root)
       class is (root_t)
          ! Copy the root.
          m4_if(DEBUGGING,[true],[write (*,*) "    which is a root"])
          block
            class(root_t), pointer :: new_root
            call roots_insert (roots, root%collectible, new_root)
            dst%root => new_root
          end block
       class default
          ! Copy the non-collectible data.
          m4_if(DEBUGGING,[true],[write (*,*) "    which is not a root"])
          allocate (dst%root, source = root)
       end select
    class default
       ! Copy the non-collectible data.
       m4_if(DEBUGGING,[true],[write (*,*) "gcroot_t_assign of non-collectible data"])
       allocate (dst%root, source = src)
    end select

    m4_if(DEBUGGING,[true],[write (*,*) "roots_count after  = ", roots_count])

    !
    ! FIXME: PUT AN OPTIONAL AUTOMATIC GARBAGE COLLECTOR RUN HERE
    !

  end subroutine gcroot_t_assign

  subroutine gcroot_t_finalize (this)
    type(gcroot_t), intent(inout) :: this
    if (associated (this%root)) then
       select type (root => this%root)
       class is (root_t)
          call roots_remove (root)
          deallocate (root)
       class default
          deallocate (this%root)
       end select
    end if
  end subroutine gcroot_t_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! THE GARBAGE COLLECTOR.

  subroutine initialize_garbage_collector
    call initialize_heap
  end subroutine initialize_garbage_collector

  subroutine collect_garbage
    call mark_from_roots
    call sweep
  end subroutine collect_garbage

  subroutine collect_garbage_now
    !!
    !! Call `collect_garbage_now' to do what its name says. You may
    !! call it explicitly or you may automate calls to it. However,
    !! you must not have it called while a collectible_t is being
    !! constructed but not yet assigned to a gcroot_t.
    !!
    call collect_garbage
  end subroutine collect_garbage_now

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
               ! The branch is a reachable object, possibly already
               ! marked for keeping.
               if (.not. is_marked (branch%heap_element)) then

                  ! Mark the reachable object for keeping.
                  call set_marked (branch%heap_element)

                  ! Push the object to the stack, to see if anything
                  ! else can be reached through it.
                  block
                    type(work_stack_element_t), pointer :: tmp
                    allocate (tmp)
                    tmp%collectible => collectible
                    tmp%next => work_stack
                    work_stack => tmp
                  end block

               end if
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
          call heap_element%remove ()
          deallocate (heap_element)
       end if
       heap_element => heap_element%next
    end do
  end subroutine sweep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module garbage_collector
