% ==================================================================
% Project: PENG ASP
% Module:  anaphora_resolution.pl
% Author:  Rolf Schwitter
% Date:    2019-01-02
% ==================================================================

:- module(anaphora_resolution, [anaphora_resolution/1]).


% ------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).


% ------------------------------------------------------------------
% Anaphora Resolution: Definite Noun Phrase
% ------------------------------------------------------------------

anaphora_resolution([def, proc, X, C1, [[variable(X, VName), class(X, CName)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(def, X, [variable(X, VName), class(X, CName)], C1, Flag),
   (
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[variable(X, VName), class(X, CName)|C2]|CT] = C3, 
      A2 = A3
   ).

anaphora_resolution([def, gen, X, [[class(X, CName), variable(X, VName)|C1]|CT], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(def, [variable(X, VName), class(X, CName)], A1, Flag),
   Flag = '+'
   ->
   C2 = C3,
   A1 = A3.


% ------------------------------------------------------------------
% 2018-10-12
%

anaphora_resolution([def, proc, X, C1, [[class(X, CName), prop(X, PName)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(def, X, [class(X, CName), prop(X, PName)], C1, Flag),
   (  
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[class(X, CName), prop(X, PName)|C2]|CT] = C3,
      (
	 member([class(X, CName), prop(X, PName)], A1),
         A1 = A3
      ;
         A2 = A3
      )
   ).


anaphora_resolution([def, gen, X, [[prop(X, PName), class(X, CName)|C1]|CT], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(def, [class(X, CName), prop(X, PName)], A1, Flag),
   Flag = '+'
   ->
   C2 = C3,
   A1 = A3.


% ------------------------------------------------------------------
% 2018-09-08; can be merged with "cardinal"
%

anaphora_resolution([def, proc, X, C1, [[class(X, CName), data_prop(X, CNum, ordinal)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(def, X, [class(X, CName), data_prop(X, CNum, ordinal)], C1, Flag),
   (  
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[class(X, CName), data_prop(X, CNum, ordinal)|C2]|CT] = C3,
      (
	 member([class(X, CName), data_prop(X, CNum, ordinal)], A1),
         A1 = A3
      ;
         A2 = A3
      )
   ).


anaphora_resolution([def, gen, X, [[data_prop(X, CNum, ordinal), class(X, CName)|C1]|CT], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(def, [class(X, CName), data_prop(X, CNum, ordinal)], A1, Flag),
   Flag = '+'
   ->
   C2 = C3,
   A1 = A3.


% ------------------------------------------------------------------
% 2018-09-05
%

anaphora_resolution([def, proc, X, C1, [[class(X, CName), data_prop(X, CNum, cardinal)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(def, X, [class(X, CName), data_prop(X, CNum, cardinal)], C1, Flag),
   (  
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[class(X, CName), data_prop(X, CNum, cardinal)|C2]|CT] = C3,
      (
	 member([class(X, CName), data_prop(X, CNum, cardinal)], A1),
         A1 = A3
      ;
         A2 = A3
      )
   ).


anaphora_resolution([def, gen, X, [[data_prop(X, CNum, cardinal), class(X, CName)|C1]|CT], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(def, [class(X, CName), data_prop(X, CNum, cardinal)], A1, Flag),
   Flag = '+'
   ->
   C2 = C3,
   A1 = A3.


% ------------------------------------------------------------------

anaphora_resolution([def, proc, X, C1, [[class(X, CName)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(def, X, [class(X, CName)], C1, Flag),
   (  
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[class(X, CName)|C2]|CT] = C3,
      (
	 member([class(X, CName)], A1),
         A1 = A3
      ;
         A2 = A3
      )
   ).


anaphora_resolution([def, gen, X, [[class(X, CName)|C1]|CT], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(def, [class(X, CName)], A1, Flag),
   Flag = '+'
   ->
   C2 = C3,
   A1 = A3.


% ------------------------------------------------------------------

anaphora_resolution([def, proc, X, C1, [[named(X, Name), class(X, CName)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(def, X, [named(X, Name), class(X, CName)], C1, Flag),
   (  
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[named(X, Name), class(X, CName)|C2]|CT] = C3,
      (
	 member([named(X, Name), class(X, CName)], A1),
         A1 = A3
      ;
         A2 = A3
      )
   ).


anaphora_resolution([def, gen, X, [[class(X, CName), named(X, Name)|C1]|CT], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(def, [named(X, Name), class(X, CName)], A1, Flag),
   Flag = '+'
   ->
   C2 = C3,
   A1 = A3.


% ------------------------------------------------------------------
% Anaphor resolution: Proper Name
% ------------------------------------------------------------------

anaphora_resolution([pname, proc, X, C1, [[named(X, Name)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(pname, X, [named(X, Name)], C1, Flag1),
   (
      Flag1 = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag1 = '-'
      ->
      [[named(X, Name)|C2]|CT] = C3,
      search_antecedent_space(pname, [named(X, Name)], A1, Flag2),
      (
         Flag2 = '+'
         ->
         A1 = A3
      ;
         Flag2 = '-'
         ->
         (
	    A1  = [List],
	    is_list(List),
	    A3  = [[named(X, Name)|List]]
	 ;
            reverse(A1, [List|Rest]),
            reverse([[named(X, Name)|List]|Rest], A3)
	 )
      )
   ).


anaphora_resolution([pname, gen, X, [[named(X, Name)|_]|_], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(pname, [named(X, Name)], A1, Flag),
   (  
      Flag = '+'
      ->
      C2 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      C2 = C3,
      (
	 A1  = [List],
	 is_list(List),
	 A3  = [[named(X, Name)|List]]
      ;
         reverse(A1, [List|Rest]),
         reverse([[named(X, Name)|List]|Rest], A3)
      )
   ).


% ------------------------------------------------------------------
% Anaphor resolution: Numeric Variable
% ------------------------------------------------------------------

anaphora_resolution([nvar, proc, X, C1, [[data_prop(X, CNum, cardinal), variable(X, VName)|C2]|CT], C3, A1, A2, A3]) :-
   search_clause_space(nvar, X, [data_prop(X, CNum, cardinal), variable(X, VName)], C1, Flag),
   (  
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[data_prop(X, CNum, cardinal), variable(X, VName)|C2]|CT] = C3,
      (
	 member([data_prop(X, CNum, cardinal), variable(X, VName)], A1),
         A1 = A3
      ;
         A2 = A3
      )
   ).


anaphora_resolution([nvar, gen, X, [[variable(X, VName), data_prop(X, CNum, cardinal)|C1]|CT], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(nvar, [variable(X, VName), data_prop(X, CNum, cardinal)], A1, Flag),
   Flag = '+'
   ->
   C2 = C3,
   A1 = A3.


% ------------------------------------------------------------------
% Anaphor resolution: Noun Number
% ------------------------------------------------------------------

anaphora_resolution([number, proc, subj, X, C1, [[data_prop(X, Number, Type), class(X, CName)|C2]|Sup], C3, A1, A2, A3]) :-
   search_clause_space(number, X, [data_prop(X, Number, Type), class(X, CName)], C1, Flag),
   (
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[data_prop(X, Number, Type), class(X, CName)|C2]|Sup] = C3,
      A1 = [Ante|Antes],
      A3 = [[data_prop(X, Number, Type), class(X, CName)|Ante]|Antes]
   ).


anaphora_resolution([number, proc, obj, X0, C1, C2, C3, A1, A2, A3]) :-
   C1 = [[data_prop(X1, Num1, Type), class(X1, CName), prop(X0, X1, PName)|ClauseList1]|Sup1],
   C2 =	[[data_prop(X2, Num2, Type)|ClauseList2]|Sup2],
   search_clause_space(number, X2, [data_prop(X2, Num2, Type), class(X2, CName)], [ClauseList2|Sup2], Flag),
   (
      Flag = '+'
      ->
      [[prop(X0, X2, PName)|ClauseList2]|Sup2] = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[data_prop(X2, Num2, Type), class(X2, CName), prop(X0, X2, PName)|ClauseList2]|Sup2] = C3,
      A1 = [Ante|Antes],
      A3 = [[data_prop(X2, Num2, Type), class(X2, CName)|Ante]|Antes]
   ).


anaphora_resolution([number, proc, obj, X0, C1, C2, C3, A1, A2, A3]) :-
   C2 =	[[data_prop(X2, Num2, Type), prop(X0, X1, PName)|ClauseList1]|Sup1],
   search_clause_space(number, X2, [data_prop(X2, Num2, Type), class(X2, CName)], C1, Flag),
   (
      Flag = '+'
      ->
      [[prop(X0, X2, PName), prop(X0, X1, PName)|ClauseList1]|Sup1] = C3,
      A1 = A3
   ;
      member(class(_, Name), ClauseList1),
      Flag = '-'
      ->
      [[data_prop(X2, Num2, Type), class(X2, Name), prop(X0, X2, PName),
        prop(X0, X1, PName)|ClauseList1]|Sup1] = C3,
      A1 = [Ante|Antes],
      A3 = [[data_prop(X2, Num2, Type), class(X2, Name)|Ante]|Antes]
   ).


anaphora_resolution([number, proc, obj, X, C1, [[data_prop(X, Number, Type), class(X, CName)|ClauseList]|Sup], C3, A1, A2, A3]) :-
   search_clause_space(number, X, [data_prop(X, Number, Type), class(X, CName)], [ClauseList|Sup], Flag),
   (
      Flag = '+'
      ->
      C1 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      [[data_prop(X, Number, Type), class(X, CName)|ClauseList]|Sup] = C3,
      A1 = [Ante|Antes],
      A3 = [[data_prop(X, Number, Type), class(X, CName)|Ante]|Antes]
   ).

 
% ------------------------------------------------------------------

anaphora_resolution([number, gen, subj, X, [[class(X, CName), data_prop(X, Number, Type)|ClauseList]|Sup], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(number, [data_prop(X, Number, Type), class(X, CName)], A1, Flag),
   (
      Flag = '+'
      ->
      C2 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      C2 = C3,
      A2 = A3
   ).


anaphora_resolution([number, gen, obj, X, [[class(X, CName), data_prop(X, Number, Type)|ClauseList]|Sup], C2, C3, A1, A2, A3]) :-
   search_antecedent_space(number, [data_prop(X, Number, Type), class(X, CName)], A1, Flag),
   (
      Flag = '+'
      ->
      C2 = C3,
      A1 = A3
   ;
      Flag = '-'
      ->
      C2 = C3,
      (
         A2 = [[data_prop(X, Number, Type), class(X, CName)|R]|T],
         A3 = [[data_prop(X, Number, Type), class(X, CName)|R]|T]
      ;
         A2 = [[data_prop(X, Number, Type)|R]|T],
         A3 = [[data_prop(X, Number, Type), class(X, CName)|R]|T]
      ;
         A2 = A3
      )
   ).


% ==================================================================
% search_clause_space/6
% ==================================================================

search_clause_space(Type, X, Anaphor, [], '-').

search_clause_space(Type, X, Anaphor, [ClauseList|Rest], Flag1) :-
   resolve(Type, X, Anaphor, ClauseList, Flag2),
   (
      Flag2 = '+'
      ->
      Flag1 = '+'
   ;
      Flag2 = '-'
      ->
      search_clause_space(Type, X, Anaphor, Rest, Flag1)
   ).


% ------------------------------------------------------------------
% resolve/5
% ------------------------------------------------------------------

resolve(Type, X, Class, [], '-').


% ------------------------------------------------------------------
% resolve/5: Definite Noun Phrase
% ------------------------------------------------------------------

resolve(def, X, [variable(X, VName1), class(X, CName1)],
	        [variable(Y, VName2), class(Y, CName2)|Rest], '+') :-
   VName1 == VName2,
   CName1 == CName2,
   X = Y.

resolve(def, X, [class(X, CName1), data_prop(X, CNum1, cardinal)],
	        [class(Y, CName2), data_prop(Y, CNum2, cardinal)|Rest], '+') :-
   CName1 == CName2,
   CNum1 == CNum2,
   X = Y.

resolve(def, X, [class(X, CName1), prop(X, PName1)],
	        [class(Y, CName2), prop(Y, PName2)|Rest], '+') :-
   CName1 == CName2,
   PName1 == PName2,
   X = Y.

resolve(def, X, [named(X, PName1), class(X, CName1)],
	        [named(Y, PName2), class(Y, CName2)|Rest], '+') :-
   PName1 == PName2,
   CName1 == CName2,
   X = Y.

resolve(def, X, [data_prop(X, Number1, Type1), class(X, CName1)],
	        [data_prop(Y, Number2, Type2), class(Y, CName2)|Rest], '+') :-
   Number1 == Number2,
   Type1 == Type2,
   CName1 == CName2,
   X = Y.

resolve(def, X, [class(X, CName1)],
	        [class(Y, CName2)|Rest], '+') :-
   CName1 == CName2,
   X = Y.
	
		
% ------------------------------------------------------------------
% resolve/5: Proper Name
% ------------------------------------------------------------------

resolve(pname, X, [named(X, Name2)], [named(Y, Name1)|Rest], '+') :-
   Name1 == Name2,
   X = Y.
  	

% ------------------------------------------------------------------
% resolve/5: Numerical Variable
% ------------------------------------------------------------------

resolve(nvar, Num, [data_prop(X, pos_int(Num), Type1), variable(X, VName1)],
	           [data_prop(Y, pos_int(Num), Type2), variable(Y, VName2)|Rest], '+') :-
   VName1 == VName2, 
   Type1 == Type2.


% ------------------------------------------------------------------
% resolve/5: Class Number
% ------------------------------------------------------------------

resolve(number, X, [data_prop(X, Num1, Type1), class(X, CName1)],
	           [data_prop(Y, Num2, Type2), class(Y, CName2)|Rest], '+') :-
   Num1 = Num2,
   Type1 = Type2,
   CName1 = CName2,
   X = Y.


% ------------------------------------------------------------------
% resolve/5: Base Case
% ------------------------------------------------------------------

resolve(Type, X, Class1, [Class2|Clauses], Flag) :-
   resolve(Type, X, Class1, Clauses, Flag).


% ==================================================================
% search_antecedent_space/4
% ==================================================================

search_antecedent_space(Type, _, [], '-').


% ------------------------------------------------------------------
% Definite noun phrase
% ------------------------------------------------------------------

search_antecedent_space(def, [class(X, CName), data_prop(X, CNum, cardinal)], [Ante|Antes], '+') :-
   % is_list(Ante),
   member_list(def, [class(X, CName), data_prop(X, CNum, cardinal)], Ante).

search_antecedent_space(def, [class(X, CName), prop(X, PName)], [Ante|Antes], '+') :-
   % is_list(Ante),
   member_list(def, [class(X, CName), prop(X, PName)], Ante).

search_antecedent_space(def, [named(X, PName), class(X, CName)], [Ante|Antes], '+') :-
   % is_list(Ante),
   member_list(def, [named(X, PName), class(X, CName)], Ante).  

search_antecedent_space(def, [variable(X, Var), class(X, CName)], [Ante|Antes], '+') :-
   % is_list(Ante),
   member_list(def, [variable(X, Var), class(X, CName)], Ante).  

search_antecedent_space(def, [class(X, CName)], [Ante|Antes], '+') :-
   % is_list(Ante),
   member_list(def, [class(X, CName)], Ante).  


% ------------------------------------------------------------------
% Proper name
% ------------------------------------------------------------------

search_antecedent_space(pname, [named(X, Name)], [Ante|Antes], '+') :-
%   is_list(Ante),
   member_list(pname, [named(X, Name)], Ante).  

% search_antecedent_space(pname, [named(X, Name)], [Ante|Antes], '+') :-
   % is_list(Ante),
%    member(named(X, Name), Ante).  

% search_antecedent_space(pname, [named(X, Name)], Ante, '+') :-
   % is_list(Ante),
 %   member(named(X, Name), Ante).




% ------------------------------------------------------------------
% Numeric variable
% ------------------------------------------------------------------

search_antecedent_space(nvar, [variable(X, VName), data_prop(X, CNum, cardinal)], [Ante|Antes], '+') :-
   % is_list(Ante),
   member_list(nvar, [variable(X, VName), data_prop(X, CNum, cardinal)], Ante).


% ------------------------------------------------------------------
% Number
% ------------------------------------------------------------------

search_antecedent_space(number, [data_prop(X, Number, Type), class(X, CName)], [Ante|Antes], '+') :-
   % is_list(Ante),
   member_list(number, [data_prop(X, Number, Type), class(X, CName)], Ante).  


% ------------------------------------------------------------------
% Base case
% ------------------------------------------------------------------

search_antecedent_space(Type, Ana, [Ante|Antes], Flag) :-
   search_antecedent_space(Type, Ana, Antes, Flag).


% ------------------------------------------------------------------

member_list(def, [class(X1, CName), data_prop(X1, CNum, cardinal)],
	         [class(X2, CName), data_prop(X2, CNum, cardinal)|Ante]) :- X1 == X2.

member_list(def, [class(X1, CName), prop(X1, PName)],
	         [class(X2, CName), prop(X2, PName)|Ante]) :- X1 == X2.

member_list(def, [named(X1, Var), class(X1, CName)],
	         [named(X2, Var), class(X2, CName)|Ante]) :- X1 == X2.

member_list(def, [variable(X1, VName), class(X1, CName)],
	         [variable(X2, VName), class(X2, CName)|Ante]) :- X1 == X2.

member_list(def, [class(X1, CName)],
	         [class(X2, CName)|Ante]) :-  X1 == X2.


% ------------------------------------------------------------------

member_list(pname, [named(X1, Var)],
	           [named(X2, Var)|Ante]) :- X1 == X2.


% ------------------------------------------------------------------

member_list(nvar, [variable(X1, Var), data_prop(X1, CNum, cardinal)],
	          [variable(X2, Var), data_prop(X2, CNum, cardinal)|Ante]) :- X1 == X2.


% ------------------------------------------------------------------

member_list(number, [data_prop(X1, Number, Type), class(X1, CName)],
	            [data_prop(X2, Number, Type), class(X2, CName)|Ante]) :- X1 == X2.


% ------------------------------------------------------------------

member_list(Type, Anaphor, [Ante|Antes]) :-
   member_list(Type, Anaphor, Antes).