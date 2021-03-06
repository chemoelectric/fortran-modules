! -*- F90 -*- 
!
! Copyright 2021, 2022 Barry Schwartz
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

module garbage_collector

  use, intrinsic :: iso_c_binding ! In case someone chooses a C type for size_kind.
  use, intrinsic :: iso_fortran_env

  implicit none
  private

  public :: operator(.val.)
  public :: operator(.autoval.)

  public :: size_kind

  public :: heap_element_t
  public :: collectible_t
  public :: gcroot_t

  public :: current_heap_size
  public :: current_roots_count

  public :: heap_insert

  public :: initialize_garbage_collector
  public :: collect_garbage_now
  public :: check_heap_size
  public :: heap_size_limit
  public :: heap_size_padding

  integer, parameter :: size_kind = int64
  integer, parameter :: size_kind_bits = bit_size (1_size_kind)
  integer, public :: doubling_limit_bits = size_kind_bits ! Settable, but for testing only.

  integer, parameter :: bits_kind = int8
  integer(bits_kind), parameter :: mark_bit = int (b'00000001')

  type :: heap_element_t
     class(*), pointer :: data => null ()              ! The actual data.
     class(heap_element_t), pointer :: prev => null () ! The previous object in the heap.
     class(heap_element_t), pointer :: next => null () ! The next object in the heap.
     integer(bits_kind) :: bits = 0                    ! The mark bit and any other such bits.
  end type heap_element_t

  ! Extend this type to represent different kinds of heap entries.
  type :: collectible_t
     class(heap_element_t), pointer :: heap_element => null () ! A pointer to a node in the heap.
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
     procedure, pass :: val => gcroot_t_val
     procedure, pass :: assign => gcroot_t_assign
     procedure, pass :: discard => gcroot_t_discard
     generic :: assignment(=) => assign
     generic :: operator(.val.) => val
     final :: gcroot_t_finalize
  end type gcroot_t

  ! The head (and tail) of a doubly-linked circular list,
  ! representing the current contents of the heap.
  class(heap_element_t), pointer :: heap => null ()

  ! The current heap size, not counting the head element.
  integer(size_kind) :: heap_count = 0

  ! The head (and tail) of a doubly-linked circular list,
  ! representing the current roots.
  class(root_t), pointer :: roots => null ()

  ! The current roots count, not counting the head element.
  integer(size_kind) :: roots_count = 0

  ! Parameters for `check_heap_size'.
  integer(size_kind) :: heap_size_limit = 2 ** 8
  integer(size_kind) :: heap_size_padding = 64

  logical :: garbage_collector_is_initialized = .false.

  interface operator(.autoval.)
     module procedure gcroot_t_autoval
  end interface operator(.autoval.)


contains

!!!-------------------------------------------------------------------


  function current_heap_size () result (size)
    integer(size_kind) :: size

    size = heap_count

  end function current_heap_size

  function current_roots_count () result (count)
    integer(size_kind) :: count

    count = roots_count

  end function current_roots_count

!!!-------------------------------------------------------------------

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

!!!-------------------------------------------------------------------

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

!!!-------------------------------------------------------------------

  subroutine initialize_heap
    if (.not. associated (heap)) then
       allocate (heap)
       heap%prev => heap
       heap%next => heap
    end if
  end subroutine initialize_heap

  function is_heap_head (p) result (bool)
    class(heap_element_t), pointer, intent(in) :: p
    logical :: bool
    bool = associated (p, heap)
  end function is_heap_head

  subroutine heap_insert (new_element)
    class(heap_element_t), pointer, intent(in) :: new_element

    if (.not. garbage_collector_is_initialized) then
       call initialize_garbage_collector
    end if

    new_element%next => heap%next
    new_element%prev => heap
    new_element%prev%next => new_element
    new_element%next%prev => new_element

    heap_count = heap_count + 1
  end subroutine heap_insert

  subroutine heap_remove (this_one)
    !
    ! NOTE: Deallocating the heap_element_t itself is the
    !       responsibility of the caller.
    !

    class(heap_element_t), pointer, intent(in) :: this_one

    class(heap_element_t), pointer :: prev, next

    next => this_one%next
    prev => this_one%prev
    prev%next => next
    next%prev => prev
    if (associated (this_one%data)) then
       deallocate (this_one%data)
    end if

    heap_count = heap_count - 1
  end subroutine heap_remove

!!!-------------------------------------------------------------------

  subroutine collectible_t_get_branch (this, branch_number, branch_number_out_of_range, branch)
    !
    ! `get_branch' returns the `branch_numberth' branch element of
    ! `this', for use in the mark phase to find reachable heap
    ! entries. Numbering starts at 1 and proceeds consecutively.
    !
    ! The behavior for `branch_number' less than 1 is not specified.
    !
    ! The value of `branch' is unspecified, if
    ! `branch_number_out_of_range' is .false.
    !
    ! The base type of the class contains no data and so can return no
    ! branches.
    !
    class(collectible_t), intent(in) :: this
    integer(size_kind), intent(in) :: branch_number
    logical, intent(out) :: branch_number_out_of_range
    class(*), allocatable, intent(out) :: branch

    branch_number_out_of_range = .true.
  end subroutine collectible_t_get_branch

!!!-------------------------------------------------------------------

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

    if (.not. garbage_collector_is_initialized) then
       call initialize_garbage_collector
    end if

    

    allocate (new_root)
    allocate (new_root%collectible, source = collectible)
    new_root%next => after_this%next
    new_root%prev => after_this
    new_root%prev%next => new_root
    new_root%next%prev => new_root

    roots_count = roots_count + 1

    

  end subroutine roots_insert

  subroutine roots_remove (this_one)
    class(root_t), pointer, intent(in) :: this_one

    class(root_t), pointer :: next, prev

    

    next => this_one%next
    prev => this_one%prev
    prev%next => next
    next%prev => prev
    if (associated (this_one%collectible)) deallocate (this_one%collectible)

    roots_count = roots_count - 1

    

  end subroutine roots_remove

!!!-------------------------------------------------------------------

  pure function gcroot_t_val (this) result (retval)
    class(gcroot_t), intent(in) :: this
    class(*), allocatable :: retval
    select type (root => this%root)
    class is (root_t)
       retval = root%collectible
    class default
       retval = root
    end select
  end function gcroot_t_val

  pure function gcroot_t_autoval (obj) result (retval)
    !
    ! Do (.val. obj) if obj is a gcroot. Otherwise just copy the
    ! object, unaltered.
    !
    class(*), intent(in) :: obj
    class(*), allocatable :: retval
    select type (obj)
    class is (gcroot_t)
       retval = gcroot_t_val (obj)
    class default
       retval = obj
    end select
  end function gcroot_t_autoval

  recursive subroutine gcroot_t_assign (dst, src)
    class(gcroot_t), intent(inout) :: dst
    class(*), intent(in) :: src

    select type (src)
    class is (collectible_t)
       if (associated (src%heap_element)) then
          ! Create a new root.
          
          select type (root => dst%root)
          class is (root_t)
             ! Reuse the roots list entry.
             if (associated (root%collectible)) deallocate (root%collectible)
             allocate (root%collectible, source = src)
          class default
             if (associated (dst%root)) deallocate (dst%root)
             block
               class(root_t), pointer :: new_root
               call roots_insert (roots, src, new_root)
               dst%root => new_root
             end block
          end select
       else
          ! A NIL-list or some such object that is technically a
          ! collectible_t, but not treated as a heap object.
          
          call gcroot_t_finalize (dst)
          allocate (dst%root, source = src)
       end if
    class is (gcroot_t)
       call gcroot_t_assign (dst, .val. src)
    class default
       ! Copy the non-collectible data.
       
       call gcroot_t_finalize (dst)
       allocate (dst%root, source = src)
    end select
  end subroutine gcroot_t_assign

  subroutine gcroot_t_discard (this)
    !
    ! Explicitly finalize the contents of a gcroot_t.
    !
    ! After the discard, the value (if any) of the variable is not
    ! specified.
    !
    class(gcroot_t), intent(inout) :: this

    ! Simply assign an arbitrary non-collectible value.
    this = 9.80665 ! The acceleration due to gravity at sea level, in SI units.
  end subroutine gcroot_t_discard

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

!!!-------------------------------------------------------------------
!!!
!!! THE GARBAGE COLLECTOR.
!!!

  subroutine initialize_garbage_collector
    call really_initialize_garbage_collector
    garbage_collector_is_initialized = .true.
  end subroutine initialize_garbage_collector

  subroutine really_initialize_garbage_collector
    call initialize_heap
    call initialize_roots
  end subroutine really_initialize_garbage_collector

  subroutine collect_garbage
    if (.not. garbage_collector_is_initialized) then
       call initialize_garbage_collector
    end if
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

  subroutine check_heap_size
    !!
    !! Call `check_heap_size' if you want to collect garbage only
    !! `sometimes'.
    !!
    !! The heap_size_limit will be increased automatically if garbage
    !! collection does not clear up `enough' space.
    !!
    
    
    
    if (heap_size_limit < heap_count) then
       
       call collect_garbage
       
       ! Possibly increase heap_size_limit until it is greater than
       ! heap_count plus a bit of padding space.
       block
         integer(size_kind) :: doubling_limit
         integer(size_kind) :: max_size
         doubling_limit = (2_size_kind ** (doubling_limit_bits - 2)) - 1_size_kind
         max_size = (2_size_kind * doubling_limit) + 1_size_kind
         do while (heap_size_limit /= max_size .and. heap_size_limit - heap_size_padding < heap_count)
            if (doubling_limit < heap_size_limit) then
               heap_size_limit = max_size
            else
               heap_size_limit = 2 * max (heap_size_limit, 1_size_kind)
            end if
            
         end do
       end block
    end if
  end subroutine check_heap_size

!!!-------------------------------------------------------------------
!!!
!!! THE MARK PHASE.
!!!

  subroutine mark_from_roots

    type :: work_stack_element_t
       class(collectible_t), pointer :: collectible
       type(work_stack_element_t), pointer :: next
    end type work_stack_element_t

    type(work_stack_element_t), pointer :: work_stack => null ()

    class(root_t), pointer :: this_root
    class(root_t), pointer :: next_root

    

    this_root => roots%next
    do while (.not. is_roots_head (this_root))
       next_root => this_root%next

       ! Mark the root object for keeping.
       
       call set_marked (this_root%collectible%heap_element)

       ! Push the root object to the stack for reachability analysis.
       
       block
         type(work_stack_element_t), pointer :: tmp
         allocate (tmp)
         allocate (tmp%collectible, source = this_root%collectible)
         tmp%next => work_stack
         work_stack => tmp
       end block

       ! Find things that can be reached from the root object.
       
       call mark_reachables

       this_root => next_root
    end do

  contains

    subroutine mark_reachables

      class(collectible_t), pointer :: collectible

      integer(size_kind) :: branch_number
      logical :: branch_number_out_of_range
      class(*), allocatable :: branch

      do while (associated (work_stack))
         ! Pop the top of the stack.
         
         block
           type(work_stack_element_t), pointer :: tmp
           tmp => work_stack
           work_stack => tmp%next
           collectible => tmp%collectible
           deallocate (tmp)
         end block

         branch_number = 1
         call collectible%get_branch (branch_number, branch_number_out_of_range, branch)
         do while (.not. branch_number_out_of_range)
            
            select type (branch)
            class is (collectible_t)
               if (associated (branch%heap_element)) then ! Exclude things such `nil' that are not actually collectible.
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
                       allocate (tmp%collectible, source = branch)
                       tmp%next => work_stack
                       work_stack => tmp
                     end block
                  end if
               end if
            end select
            branch_number = branch_number + 1
            call collectible%get_branch (branch_number, branch_number_out_of_range, branch)
         end do

         deallocate (collectible)
      end do
    end subroutine mark_reachables

  end subroutine mark_from_roots

!!!-------------------------------------------------------------------
!!!
!!! THE SWEEP PHASE.
!!!

  subroutine sweep
    class(heap_element_t), pointer :: heap_element
    class(heap_element_t), pointer :: next_heap_element

    

    heap_element => heap%next
    do while (.not. is_heap_head (heap_element))
       next_heap_element => heap_element%next
       
       if (is_marked (heap_element)) then
          ! Keep this heap element.
          
          call set_unmarked (heap_element)
       else
          
          call heap_remove (heap_element)
          deallocate (heap_element)
       end if
       heap_element => next_heap_element
    end do
  end subroutine sweep

!!!-------------------------------------------------------------------

end module garbage_collector
