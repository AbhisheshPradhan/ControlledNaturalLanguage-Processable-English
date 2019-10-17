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

class(7,agreement_purchase_sell).

class(C,agreement) :- class(C,agreement_purchase_sell).

class(2,seller).

class(3,listing_brokerage).

pred(2,3,appoint).

prop(2,4,included_in).

pred(3,10,receive).

class(10,notice).

pred(10,4,belong_to).

pred(3,11,give).

class(11,notice).

pred(11,4,belong_to).

class(D,brokerage) :- class(D,listing_brokerage).

pred(E,F,represent) :- class(F,seller), pred(F,E,appoint), class(E,brokerage).

pred(G,H,deal_with) :- class(G,brokerage), pred(G,I,represent), class(I,seller), prop(I,J,included_in), class(J,agreement), pred(G,H,receive), class(H,notice), pred(H,J,belong_to).

named(12,arun_majumdar).

class(12,buyer).

named(13,mc_grath).

class(13,buyer_brokerage).

class(K,brokerage) :- class(K,buyer_brokerage).

pred(L,M,represent) :- class(M,buyer), pred(M,L,appoint), class(L,brokerage).

pred(12,13,appoint).

prop(12,4,included_in).

pred(13,14,receive).

class(14,notice).

pred(14,4,belong_to).

pred(13,15,give).

class(15,notice).

pred(15,4,belong_to).

pred(N,O,deal_with) :- class(N,brokerage), pred(N,P,represent), class(P,buyer), prop(P,Q,included_in), class(Q,agreement), pred(N,O,receive), class(O,notice), pred(O,Q,belong_to).

:- class(R,brokerage), pred(R,S,represent), class(S,seller), prop(S,T,included_in), class(T,agreement), pred(R,U,represent), class(U,buyer), prop(U,T,included_in), pred(R,V,receive), class(V,notice), pred(V,T,belong_to).

:- class(W,brokerage), pred(W,X,represent), class(X,seller), prop(X,Y,included_in), class(Y,agreement), pred(W,Z,represent), class(Z,buyer), prop(Z,Y,included_in), pred(W,A1,give), class(A1,notice), pred(A1,Y,belong_to).

