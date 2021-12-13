divert(-1)

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


dnl  Generate `a' or `d' from an integer's bit. Zero is `a', one
dnl  is `d'.
m4_define([m4_least_bit_to_a_or_d],
    [m4_translit(m4_eval([(($1) & 1) == 1]),[01],[ad])])

dnl  Generate `car' or `cdr' from an integer's bit. Zero is `car',
dnl  one is `cdr'.
m4_define([m4_least_bit_to_car_or_cdr],
    [[c]m4_least_bit_to_a_or_d($1)r])

dnl  Generate sequence like `aadaddda'.
m4_define([m4_bits_to_ad_sequence],
    [m4_forloop([_m4_index],0,m4_eval([($1) - 1]),
       [m4_least_bit_to_a_or_d(m4_eval([($2) >> ]_m4_index))])])

dnl  Generate a sequence like `car (cdr (car (cdr ('.
m4_define([m4_bits_to_car_cdr_and_left_parentheses],
    [m4_forloop([_m4_index],0,m4_eval([($1) - 1]),
       [m4_least_bit_to_car_or_cdr(m4_eval([($2) >> ]_m4_index))[ ]m4_left_parenthesis])])

dnl  Generate a run of right parentheses.
m4_define([m4_n_right_parentheses],
    [m4_forloop([_m4_index],0,m4_eval([($1) - 1]),[m4_right_parenthesis])])

dnl  Generate a sequence of function calls such as
dnl  `car (cdr (car (cdr (x))))'.
m4_define([m4_bits_to_car_cdr_call],
    [m4_bits_to_car_cdr_and_left_parentheses([$1],[$2])[]$3[]m4_n_right_parentheses([$1])])

dnl  Generate a sequence of function calls such as
dnl
dnl     x = cdr (x)
dnl     x = car (x)
dnl     x = cdr (x)
dnl     x = car (x)
dnl
m4_define([m4_bits_to_car_cdr_assignments],
    [m4_forloop([_m4_index],0,m4_eval([($1) - 1]),[dnl
    $3 = m4_least_bit_to_car_or_cdr(m4_eval([($2) >> ]m4_eval(($1) - 1 - _m4_index))) ($3)
])])

dnl  Generate a sequence of function calls such as
dnl
dnl     x = cdr (x)
dnl     x = cdr (x)
dnl     x = cdr (x)
dnl     x = cdr (x)
dnl     x = cdr (x)
dnl     x = car (x)
dnl
dnl  for extracting the nth element of a list.
m4_define([m4_bits_to_get_nth_element],
    [m4_bits_to_car_cdr_assignments([$1],m4_eval((1 << ($1)) - 2),[$2])])

dnl  Generate the `public ::' declarations for all the things like
dnl  `cadadr', of a given length.
m4_define([m4_length_n_cadadr_public_declarations],
    [m4_forloop([_m4_bits],0,m4_eval([(1 << ($1)) - 1]),[dnl
  public :: c[]m4_bits_to_ad_sequence([$1],_m4_bits)]r[
])])

dnl  Generate the definition for a thing like `cadadr'.
m4_define([m4_cadadr_definition],[dnl
  function c[]m4_bits_to_ad_sequence([$1],[$2])r (tree) result (element)
    class(*), intent(in) :: tree
    class(*), allocatable :: element
    element = tree
m4_bits_to_car_cdr_assignments([$1],[$2],[element])dnl
  end function c[]m4_bits_to_ad_sequence([$1],[$2])r
])

dnl  Generate definitions for things all the things like `cadadr' of a
dnl  given length.
m4_define([m4_length_n_cadadr_definitions],
    [m4_forloop([_m4_bits],0,m4_eval([(1 << ($1)) - 1]),
       [m4_cadadr_definition([$1],_m4_bits)
])])

divert[]dnl
