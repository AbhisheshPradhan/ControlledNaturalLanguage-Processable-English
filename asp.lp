prop(A,successful) :- class(A,student), pred(A,work).

pred(B,work) ; pred(B,party) :- class(B,student), pred(B,C,study_at), named(C,macquarie_university).

:- class(D,student), prop(D,E,enrolled_in), named(E,information_technology), pred(D,party).

named(1,tom).

class(1,student).

pred(1,2,study_at).

named(2,macquarie_university).

prop(1,3,enrolled_in).

named(3,information_technology).

named(4,bob).

class(4,student).

pred(4,2,study_at).

-pred(4,work).

answer(named(F,G)) :- named(F,G), prop(F,successful).

