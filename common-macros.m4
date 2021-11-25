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

changecom
changequote(`[',`]')

define([m4_undefine],defn([undefine]))     m4_undefine([undefine])
define([m4_define],defn([define]))         m4_undefine([define])
m4_define([m4_include],defn([include]))    m4_undefine([include])
m4_define([m4_pushdef],defn([pushdef]))    m4_undefine([pushdef])
m4_define([m4_popdef],defn([popdef]))      m4_undefine([popdef])
m4_define([m4_shift],defn([shift]))        m4_undefine([shift])
m4_define([m4_if],defn([ifelse]))          m4_undefine([ifelse])
m4_define([m4_ifdef],defn([ifdef]))        m4_undefine([ifdef])
m4_define([m4_translit],defn([translit]))  m4_undefine([translit])
m4_define([m4_index],defn([index]))        m4_undefine([index])
m4_define([m4_incr],defn([incr]))          m4_undefine([incr])
m4_define([m4_decr],defn([decr]))          m4_undefine([decr])
m4_define([m4_len],defn([len]))            m4_undefine([len])
m4_define([m4_divnum],defn([divnum]))      m4_undefine([divnum])
m4_define([m4_eval],defn([eval]))          m4_undefine([eval])
m4_define([m4_substr],defn([substr]))      m4_undefine([substr])

dnl
dnl  m4_foreachq(x, [item_1, item_2, ..., item_n], statement)
dnl
m4_define([m4_foreachq],
  [m4_if([$2],[],[],
    [m4_pushdef([$1])_$0([$1],[$3],[],$2)m4_popdef([$1])])])
m4_define([_m4_foreachq],
  [m4_if([$#],[3],[],
    [m4_define([$1],[$4])$2[]$0([$1],[$2],m4_shift(m4_shift(m4_shift($@))))])])

dnl
dnl  m4_forloop(variable, from, to, statement)
dnl
m4_define([m4_forloop],[m4_if(m4_eval([($2) <= ($3)]),[1],
  [m4_pushdef([$1])_$0([$1],m4_eval([$2]),
    m4_eval([$3]),[$4])m4_popdef([$1])])])
m4_define([_m4_forloop],
  [m4_define([$1],[$2])$4[]m4_if([$2],[$3],[],
    [$0([$1],m4_incr([$2]),[$3],[$4])])])

m4_define([m4_left_parenthesis],[(])
m4_define([m4_right_parenthesis],[)])

divert[]dnl
