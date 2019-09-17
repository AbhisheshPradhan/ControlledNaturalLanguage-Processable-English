class(1,agreement_purchase_sell).

class(A,agreement) :- class(A,agreement_purchase_sell).

named(2,rolf_schwitter).

class(2,seller).

named(3,tracy_yap_realty).

class(3,listing_brokerage).

pred(2,3,appoint).

prop(2,4,included_in).

class(4,agreement).

pred(3,5,receive).

class(5,notice).

pred(5,4,belong_to).

pred(3,6,give).

class(6,notice).

pred(6,4,belong_to).

class(B,brokerage) :- class(B,listing_brokerage).

pred(C,D,represent) :- class(D,seller), pred(D,C,appoint), class(C,brokerage).

pred(E,F,deal_with) :- class(E,brokerage), pred(E,G,represent), class(G,seller), prop(G,H,included_in), class(H,agreement), pred(E,F,receive), class(F,notice), pred(F,H,belong_to).

named(7,arun_majumdar).

class(7,buyer).

named(8,mc_grath).

class(8,buyer_brokerage).

class(I,brokerage) :- class(I,buyer_brokerage).

pred(J,K,represent) :- class(K,buyer), pred(K,J,appoint), class(J,brokerage).

pred(7,8,appoint).

prop(7,4,included_in).

pred(8,9,receive).

class(9,notice).

pred(9,4,belong_to).

pred(8,10,give).

class(10,notice).

pred(10,4,belong_to).

pred(L,M,deal_with) :- class(L,brokerage), pred(L,N,represent), class(N,buyer), prop(N,O,included_in), class(O,agreement), pred(L,M,receive), class(M,notice), pred(M,O,belong_to).

:- class(P,brokerage), pred(P,Q,represent), class(Q,seller), prop(Q,R,included_in), class(R,agreement), pred(P,S,represent), class(S,buyer), prop(S,R,included_in), pred(P,T,receive), class(T,notice), pred(T,R,belong_to).

:- class(U,brokerage), pred(U,V,represent), class(V,seller), prop(V,W,included_in), class(W,agreement), pred(U,X,represent), class(X,buyer), prop(X,W,included_in), pred(U,Y,give), class(Y,notice), pred(Y,W,belong_to).

