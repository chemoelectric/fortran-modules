program cons_lists_demo
  use :: cons_lists
  implicit none
  
  interface
     subroutine printobj (obj)
       class(*), intent(in) :: obj
     end subroutine printobj
  end interface

  type(cons_t), target :: lst1, lst2, lst3, lst4
  !class(cons_t), allocatable :: listoid1, listoid2, listoid3, listoid4
  class(*), allocatable :: x, y, z
  integer :: i, j, k

  write (*,*) "------------------------------------------------"

  lst1 = cons (1, 2)
  lst2 = lst1
  call printobj (car (lst1))
  call printobj (cdr (lst1))
  call printobj (car (lst2))
  call printobj (cdr (lst2))

  write (*,*) ". . . . . . . . . . . ."

  call set_car (lst1, 10)
  call printobj (car (lst1))
  call printobj (cdr (lst1))
  call printobj (car (lst2))
  call printobj (cdr (lst2))

  write (*,*) ". . . . . . . . . . . ."

  call set_cdr (lst1, 20)
  call printobj (car (lst1))
  call printobj (cdr (lst1))
  call printobj (car (lst2))
  call printobj (cdr (lst2))

  write (*,*) ". . . . . . . . . . . ."

  lst1 = cons (1, cons (2, cons (3, nil_list)))
  lst2 = lst1
  call printobj (cadr (lst1))
  call printobj (cadr (lst2))
  select type (sublst => cdr (lst1))
  class is (cons_t)
     call set_car (sublst, 5)
  end select
  call printobj (cadr (lst1))
  call printobj (cadr (lst2))
  select type (sublst => cddr (lst1))
  class is (cons_t)
     call set_car (sublst, 50)
  end select
  call printobj (caddr (lst1))
  call printobj (caddr (lst2))
  call set_cdr (assume_list (cddr (lst1)), cons(100, cons (200, nil_list)))
  call printobj (cadddr (lst1))
  call printobj (cadddr (lst2))
  call printobj (fifth (lst1))
  call printobj (fifth (lst2))

  write (*,*) "------------------------------------------------"

  lst1 = cons (1, cons (2.0, cons (.true., cons ("abc", nil_list))))

  call printobj (car (lst1))
  call printobj (first (lst1))
  call printobj (list_ref0 (lst1, 0))
  call printobj (list_ref1 (lst1, 1))

  call printobj (cadr (lst1))
  call printobj (second (lst1))
  call printobj (list_ref0 (lst1, 1))
  call printobj (list_ref1 (lst1, 2))

  call printobj (caddr (lst1))
  call printobj (third (lst1))
  call printobj (list_ref0 (lst1, 2))
  call printobj (list_ref1 (lst1, 3))

  call printobj (cadddr (lst1))
  call printobj (fourth (lst1))
  call printobj (list_ref0 (lst1, 3))
  call printobj (list_ref1 (lst1, 4))

  write (*,*) "------------------------------------------------"

  lst1 = cons (1, 2)
  call uncons (lst1, x, y)
  call printobj (x)
  call printobj (y)

  write (*,*) "------------------------------------------------"

  lst1 = iota (10)
  do i = 1, 10
     call printobj (list_ref1 (lst1, i))
  end do

  write (*,*) ". . . . . . . . . . . ."

  lst2 = list_reverse (lst1)
  do i = 1, 10
     call printobj (list_ref1 (lst2, i))
  end do

  write (*,*) ". . . . . . . . . . . ."

  lst3 = iota (10)
  call list_reverse_in_place (lst3)
  do i = 1, 10
     call printobj (list_ref1 (lst3, i))
     write (*,*) "  . . ."
  end do
  lst4 = list_copy (lst3)
  call list_reverse_in_place (lst3)
  do i = 1, 10
     call printobj (list_ref1 (lst3, i))
     call printobj (list_ref1 (lst4, i))
     write (*,*) "  . . ."
  end do

  write (*,*) "list lengths: ", list_length (lst3), list_length (lst4)

  write (*,*) "------------------------------------------------"

  call printobj (list_last (cons (1, cons (2, cons (3, 4)))))

  write (*,*) "------------------------------------------------"

  lst1 = circular_list (cons (1, cons (2, cons (3, nil_list))))
  do i = 1, 24
     call printobj (list_ref1 (lst1, i))
  end do

  write (*,*) "------------------------------------------------"

  lst1 = make_list (10, "Q")
  do i = 1, 10
     call printobj (list_ref1 (lst1, i))
  end do

  write (*,*) "------------------------------------------------"

  lst1 = "a" ** 2 ** 3.0 ** nil_list
  do i = 1, 3
     call printobj (list_ref1 (lst1, i))
  end do

  write (*,*) "------------------------------------------------"

end program cons_lists_demo

subroutine printobj (obj)
  class(*), intent(in) :: obj
  select type (obj)
  type is (integer)
     write (*,*) "integer = ", obj
  type is (real)
     write (*,*) "real = ", obj
  type is (logical)
     write (*,*) "logical = ", obj
  type is (character(*))
     write (*,*) "character(*) = ", obj
  class default
     write (*,*) "Error: printobj called with unsupported type"
  end select
end subroutine printobj
