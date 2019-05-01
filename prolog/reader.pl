% ====================================================
% Project: PENG ASP				
% Module:  reader.pl
% Author:  Rolf Schwitter
% Date:    2019-04-03
% ====================================================	
  
:- module(reader, [reader/3]).

:- style_check([-singleton, -discontiguous]). 

% ----------------------------------------------------
% reader/3
% ----------------------------------------------------	

reader(FileName, Chars2, ClauseList2) :-
   read_file_to_chars(FileName, Chars),
   strip_off_background_theory(Chars, Chars2),
   chars_to_clauses_chars(Chars2, ClausesChars),
   clauses_chars_to_clauses_asp(ClausesChars, ClausesASP),
   complete_clauses(ClausesASP, ClauseList2).


% ----------------------------------------------------
% read_file_to_chars/2
% ----------------------------------------------------	
 
read_file_to_chars(FileName, Chars) :-
   read_file_to_codes(FileName, Codes, [encoding(utf8)]),
   codes_to_chars(Codes, Chars).


% ----------------------------------------------------
% strip_off_background_theory/2
% -- split on: clipped, needs to be fixed
% ----------------------------------------------------

strip_off_background_theory(Chars1, Chars2) :-
   append(Chars2, [c, l, i, p, p, e, d|Rest], Chars1).
   
strip_off_background_theory(Chars, Chars).

% ----------------------------------------------------
% codes_to_chars/2
% ----------------------------------------------------				

codes_to_chars([], []).

codes_to_chars([110, 111, 116, 32|Codes], ['n', 'o', 't', ' '|Chars]) :-
   codes_to_chars(Codes, Chars).

codes_to_chars([32|Codes], Chars) :-
   codes_to_chars(Codes, Chars).

% Filter out comments in ASP file
codes_to_chars([37|Codes1], Chars) :-
   append(_CodesComment, [10|Codes2], Codes1),
   codes_to_chars(Codes2, Chars).

% Filter out comments in ASP file
codes_to_chars([47, 42|Codes1], Chars) :-
   append(_CodesComment, [42, 47|Codes2], Codes1),
   codes_to_chars(Codes2, Chars).

codes_to_chars([10|Codes], Chars) :-
   codes_to_chars(Codes, Chars).

codes_to_chars([Code|Codes], [Char|Chars]) :-
   char_code(Char, Code),
   codes_to_chars(Codes, Chars).


% ----------------------------------------------------
% chars_to_clauses_chars/2
% ----------------------------------------------------

chars_to_clauses_chars([], []).
		
chars_to_clauses_chars(Chars1, [Clause3|Clauses]) :-
   append(Clause, ['.'|Chars2], Chars1),
   append(Clause, ['.'], Clause3),
   chars_to_clauses_chars(Chars2, Clauses).


% ----------------------------------------------------
% clauses_chars_to_clauses_asp/2
% ----------------------------------------------------	

clauses_chars_to_clauses_asp([], []).

clauses_chars_to_clauses_asp([Clause|Clauses], [ClauseASP|ClausesASP]) :-
   build_asp_clause(Clause, ClauseASP),
   clauses_chars_to_clauses_asp(Clauses, ClausesASP).


% ----------------------------------------------------
% build_asp_clause/2
% ----------------------------------------------------	

build_asp_clause(Clause, [':-', BodyList, '.']) :-
   append([], [':', '-'|Rest], Clause),
   append(BodyChars, ['.'], Rest),
   body_chars_to_body_list(BodyChars, BodyList, BodyBindings),
   fix_variable_bindings(BodyBindings).


build_asp_clause(Clause, [HeadList, ':-', BodyList, '.']) :-
   append(HeadChars, [':', '-'|Rest], Clause),
   append(BodyChars, ['.'], Rest),
   head_chars_to_head_list(HeadChars, HeadList, HeadBindings),
   body_chars_to_body_list(BodyChars, BodyList, BodyBindings),
   append(HeadBindings, BodyBindings, Bindings1),
   sort(Bindings1, Bindings2),
   fix_variable_bindings(Bindings2).


build_asp_clause(Clause, [Literal, '.']) :-
   append(Chars, ['.'], Clause),
   fact_chars_to_fact(Chars, Literal, Bindings),
   fix_variable_bindings(Bindings).

   
% ----------------------------------------------------
% head_chars_to_head_list/3
% ----------------------------------------------------

head_chars_to_head_list(Chars1, List3, Bindings) :-
   append([Char1, '{'|Chars2], ['}', Char2], Chars1),
   member(Char1, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   member(Char2, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   number_chars(Num1, [Char1]),
   number_chars(Num2, [Char2]),
   aggregate_elements(Chars2, List1, Bindings),
   append([Num1, '{'], List1, List2),
   append(List2, ['}', Num2], List3).


head_chars_to_head_list(Chars1, List3, Bindings) :-
   append([Char1, '{'|Chars2], ['}'], Chars1),
   member(Char1, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   number_chars(Num1, [Char1]),
   aggregate_elements(Chars2, List1, Bindings),
   append([Num1, '{'], List1, List2),
   append(List2, ['}'], List3).


head_chars_to_head_list(Chars1, List3, Bindings) :-
   append(['{'|Chars2], ['}', Char2], Chars1),
   member(Char2, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   number_chars(Num2, [Char2]),
   aggregate_elements(Chars2, List1, Bindings),
   append(['{'], List1, List2),
   append(List2, ['}', Num2], List3).


head_chars_to_head_list(Chars1, List3, Bindings) :-
   append(['{'|Chars2], ['}'], Chars1),
   aggregate_elements(Chars2, List1, Bindings),
   append(['{'], List1, List2),
   append(List2, ['}'], List3).

  
head_chars_to_head_list(Chars1, Term3, Bindings3) :-
   append(Chars3, [';'|Chars2], Chars1),
   atom_chars(Atom, Chars3),
   atom_to_term(Atom, Literal, Bindings1),
   head_chars_to_head_list(Chars2, Term2, Bindings2),
   append([[Literal]], [';'], Term1),
   append(Term1, [Term2], Term3),
   append(Bindings1, Bindings2, Bindings3).


head_chars_to_head_list(Chars, [Literal], Bindings) :-
   atom_chars(Atom, Chars),
   atom_to_term(Atom, Literal, Bindings).


% ----------------------------------------------------
% body_chars_to_body_list/3
% ----------------------------------------------------	

body_chars_to_body_list(Chars1, [Literal|Literals], Bindings3) :-
   append(Chars3, [')', ','|Chars2], Chars1),
   append(Chars3, [')'], Chars4),
   balanced_round_brackets(Chars4),
   atom_chars(Atom, Chars4),
   atom_to_term(Atom, Literal, Bindings1),
   body_chars_to_body_list(Chars2, Literals, Bindings2),
   append(Bindings1, Bindings2, Bindings3).

body_chars_to_body_list(Chars, [Literal], Bindings) :-
   atom_chars(Atom, Chars),
   atom_to_term(Atom, Literal, Bindings).


% ----------------------------------------------------
% fact_chars_to_fact/3
% ----------------------------------------------------	

fact_chars_to_fact(Chars1, List3, Bindings) :-
   append(['#', 'm', 'i', 'n', 'i', 'm', 'i', 'z', 'e', '{'|Chars2], ['}'], Chars1),
   aggregate_elements(Chars2, List1, Bindings),
   append(['#minimize', '{'], List1, List2),
   append(List2, ['}'], List3).


fact_chars_to_fact(Chars1, List3, Bindings) :-
   append(['#', 'm', 'a', 'x', 'i', 'm', 'i', 'z', 'e', '{'|Chars2], ['}'], Chars1),
   aggregate_elements(Chars2, List1, Bindings),
   append(['#maximize', '{'], List1, List2),
   append(List2, ['}'], List3).


fact_chars_to_fact(Chars1, List3, Bindings) :-
   append([Char1, '{'|Chars2], ['}', Char2], Chars1),
   member(Char1, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   member(Char2, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   number_chars(Num1, [Char1]),
   number_chars(Num2, [Char2]),
   aggregate_elements(Chars2, List1, Bindings),
   append([Num1, '{'], List1, List2),
   append(List2, ['}', Num2], List3).


fact_chars_to_fact(Chars1, List3, Bindings) :-
   append([Char1, '{'|Chars2], ['}'], Chars1),
   member(Char1, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   number_chars(Num1, [Char1]),
   aggregate_elements(Chars2, List1, Bindings),
   append([Num1, '{'], List1, List2),
   append(List2, ['}'], List3).


fact_chars_to_fact(Chars1, List3, Bindings) :-
   append(['{'|Chars2], ['}', Char2], Chars1),
   member(Char2, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
   number_chars(Num2, [Char2]),
   aggregate_elements(Chars2, List1, Bindings),
   append(['{'], List1, List2),
   append(List2, ['}', Num2], List3).


fact_chars_to_fact(Chars1, List3, Bindings) :-
   append(['{'|Chars2], ['}'], Chars1),
   aggregate_elements(Chars2, List1, Bindings),
   append(['{'], List1, List2),
   append(List2, ['}'], List3).


fact_chars_to_fact(Chars1, ['-', [Literal]], Bindings) :-
   append(['-'], Chars2, Chars1),
   atom_chars(Atom, Chars2),
   atom_to_term(Atom, Literal, Bindings).


fact_chars_to_fact(Chars, Literal, Bindings) :-
   atom_chars(Atom, Chars),
   atom_to_term(Atom, Literal, Bindings).
   

% ----------------------------------------------------
% aggregate_elements/3
% ----------------------------------------------------

aggregate_elements(Chars1, List3, Bindings3) :-
   append(Chars3, [';'|Chars2], Chars1),
   member(':', Chars2),
   member(':', Chars3),
   aggregate_element(Chars3, List1, Bindings1),
   aggregate_elements(Chars2, List2, Bindings2),
   append(List1, [';'|List2], List3),
   append(Bindings1, Bindings2, Bindings3).
  

aggregate_elements(Chars1, [List2], Bindings5) :-  
   append(Chars3, [':'|Chars2], Chars1),
   append(Chars5, [','|Chars4], Chars3),
   append(Chars7, ['@'|Chars6], Chars5),
   append(['@', '('], Chars7, Chars8),         % Number checking???
   append(Chars8, [','], Chars9),
   append(Chars9, Chars6, Chars10),
   append(Chars10, [')'], Chars11),
   atom_chars(Atom1, Chars11),
   atom_chars(Atom2, Chars4),
   atom_to_term(Atom1, Term1, Bindings1),
   atom_to_term(Atom2, Term2, Bindings2),
   aggregate_element_list(Chars2, List1, Bindings3),  
   append([[Term1, Term2]], [':'|[List1]], List2),
   append(Bindings1, Bindings2, Bindings4),
   append(Bindings4, Bindings3, Bindings5).


aggregate_elements(Chars1, [List2], Bindings3) :-  
   append(Chars3, [':'|Chars2], Chars1),
   atom_chars(Atom, Chars3),
   atom_to_term(Atom, Term, Bindings1),
   aggregate_element_list(Chars2, List1, Bindings2),  
   append([[Term]], [':'|[List1]], List2),
   append(Bindings1, Bindings2, Bindings3).


aggregate_element(Chars1, [List2], Bindings3) :-   
   append(Chars3, [':'|Chars2], Chars1),
   atom_chars(Atom, Chars3),
   atom_to_term(Atom, Term, Bindings1),
   aggregate_element_list(Chars2, List1, Bindings2),  
   append([[Term]], [':'|[List1]], List2),
   append(Bindings1, Bindings2, Bindings3).


aggregate_element_list(Chars1, [Term|Terms], Bindings3) :-
   append(Chars3, [')', ','|Chars2], Chars1),
   append(Chars3, [')'], Chars4),
   balanced_round_brackets(Chars4),
   atom_chars(Atom, Chars4),
   atom_to_term(Atom, Term, Bindings1), 
   aggregate_element_list(Chars2, Terms, Bindings2),
   append(Bindings1, Bindings2, Bindings3).


aggregate_element_list(Chars, [Term], Bindings) :-
   atom_chars(Atom, Chars),
   atom_to_term(Atom, Term, Bindings).   


% ----------------------------------------------------
% balanced_round_brackets/1
% ----------------------------------------------------

balanced_round_brackets(List) :-
   balanced_round_brackets(List, 0).

balanced_round_brackets([], 0).

balanced_round_brackets(['('|Cs], K) :-
   !,
   K1 is K+1,
   balanced_round_brackets(Cs, K1).

balanced_round_brackets([')'|Cs], K) :-
   !,
   K1 is K-1,
   K1 >= 0,
   balanced_round_brackets(Cs, K1).

balanced_round_brackets([C|Cs], K) :-
   balanced_round_brackets(Cs, K).


% ----------------------------------------------------
% fix_variable_bindings/1
% ----------------------------------------------------

fix_variable_bindings(Bindings) :-
   fix_bindings(Bindings, Bindings).

fix_bindings([], Bindings).
fix_bindings([(A=Var)|BindingsRest], Bindings) :-
   member((A=Var), Bindings),
   fix_bindings(BindingsRest, Bindings).


% ----------------------------------------------------
% complete_clauses/2
% ----------------------------------------------------

complete_clauses(ClausesASP1, ClausesASP2) :-
   collect_nomina(ClausesASP1, Nomina),
   complete_clauses(ClausesASP1, Nomina, ClausesASP2, Variables), 
   fix_variable_bindings(Variables).


% ----------------------------------------------------
% collect_nomina/2
% ----------------------------------------------------

collect_nomina([], []).

collect_nomina([[named(X, PName), '.']|Clauses1], [named(X, PName)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([[class(X, CName), '.'], [data_prop(X, DProp, Type), '.']|Clauses1],
	        [class(X, CName), data_prop(X, DProp, Type)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([[pred(_, X, PredName), '.'], [data_prop(X, DProp, Type), '.'], [class(X, CName), '.']|Clauses1],
                [data_prop(X, DProp, Type), class(X, CName)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([[prop(_, X, PropName), '.'], [data_prop(X, DProp, Type), '.'], [class(X, CName), '.']|Clauses1],
                [data_prop(X, DProp, Type), class(X, CName)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([[data_prop(X, DProp, Type), '.'], [class(X, CName), '.'], [pred(X, _, PredName), '.']|Clauses1],
                [data_prop(X, DProp, Type), class(X, CName)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([[data_prop(X, DProp, Type), '.'], [class(X, CName), '.'], [prop(X, _, PropName), '.']|Clauses1],
                [data_prop(X, DProp, Type), class(X, CName)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([[data_prop(X, DProp, Type), '.'], [class(X, CName), '.']|Clauses1],
	        [data_prop(X, DProp, Type), class(X, CName)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([[class(X, CName), '.']|Clauses1], [class(X, CName)|Clauses2]) :-
   collect_nomina(Clauses1, Clauses2).

collect_nomina([Clause|Clauses1], Clauses2) :-
   collect_nomina(Clauses1, Clauses2).


% ----------------------------------------------------
% complete_clauses/4
% ----------------------------------------------------

complete_clauses([], Nomina, [], []).


complete_clauses([[Head, ':-', Body1, '.']|Rest1], Nomina, [[Head2, :-, Body2, '.']|Rest2], Bindings3) :-
   add_missing_literals_body(Body1, Body2, Bindings1),
   add_missing_literals_head(Head, Body2, Head2),
   complete_clauses(Rest1, Nomina, Rest2, Bindings2),
   append(Bindings1, Bindings2, Bindings3).


complete_clauses([[':-', Body1, '.']|Rest1], Nomina, [[':-', Body2, '.']|Rest2], Bindings3) :-
   add_missing_literals_body(Body1, Body2, Bindings1),
   complete_clauses(Rest1, Nomina, Rest2, Bindings2),
   append(Bindings1, Bindings2, Bindings3).


complete_clauses([[[Num1, '{', [Lit, ':', [named(X, PooledNames)]], '}', Num2], '.']|Rest1], 
                 Nomina, 
                 [[[Num1, '{', [Lit, ':', Conds2], '}', Num2], '.']|Rest2], Bindings3) :-
   unpool_names(X, PooledNames, Conds2),
   complete_clauses(Rest1, Nomina, Rest2, Bindings3).
   %%% append(Bindings1, Bindings2, Bindings3).

% ----------------------------------------------------
%%% Added 2018-10-11
% ----------------------------------------------------

complete_clauses([[['#minimize', '{', [Lit, ':', Conds1], '}'], '.']|Rest1], 
                 Nomina, 
                 [[['#minimize', '{', [Lit, ':', Conds2], '}'], '.']|Rest2], Bindings) :-
   add_missing_literals_conds(Conds1, Conds2),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[['#maximize', '{', [Lit, ':', Conds1], '}'], '.']|Rest1], 
                 Nomina, 
                 [[['#maximize', '{', [Lit, ':', Conds2], '}'], '.']|Rest2], Bindings) :-
   add_missing_literals_conds(Conds1, Conds2),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


% ----------------------------------------------------
% add_missing_literals_conds/2
% ----------------------------------------------------

add_missing_literals_conds([], []).

add_missing_literals_conds([prop(X, PName), class(X, CName), data_prop(X, DProp, nominal)|Rest1],
                           [prop(X, PName), class(X, CName), variable(X, VName), data_prop(X, DProp, nominal)|Rest2]) :-
   atom_codes(PName, [Code|Codes]),
   VCode is Code - 32,
   atom_codes(VName, [VCode]),
   add_missing_literals_conds(Rest1, Rest2).

add_missing_literals_conds([Lit|Lits1], [Lit|Lits2]) :-
   add_missing_literals_conds(Lits1, Lits2).


% ----------------------------------------------------
% unpool_names/3
% ----------------------------------------------------

unpool_names(X, (Name ; Names), [named(X, Name), ';'|Rest]) :-
   unpool_names(X, Names, Rest).

unpool_names(X, Name, [named(X, Name)]).


% ----------------------------------------------------

complete_clauses([[data_prop(N1, DProp1, Type1), '.'], [class(N1, CName), '.'], [data_prop(N1, DProp2, Type2), '.']|Rest1],
		  Nomina,
		  [[data_prop(X1, DProp1, Type1), class(X1, CName), data_prop(N1, DProp2, Type2), '.']|Rest2],
		  [N1=X1|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


% ----------------------------------------------------

complete_clauses([[named(N, PName), '.'], [class(N, CName), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName), pred(X1, X2, isa), class(X2, CName), '.']|Rest2],
		 [N=X1|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[['-', [pred(N, PredName)]], '.']|Rest1],
		 Nomina,
		 [[named(X, PName), '-', [pred(X, PredName)], '.']|Rest2],
		 [N=X|Bindings]) :-
   member(named(N, PName), Nomina),		      
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[class(N1, CName1), '.'],
		  [named(N1, PName), '.'],
		  [pred(N1, N2, PredName), '.'],
		  [data_prop(N2, DProp, Type), '.'],
		  [class(N2, CName2), '.']|Rest1],
		  Nomina,
		 [[class(X1, CName1),
		   named(X1, PName),
		   pred(X1, X2, PredName),
		   data_prop(X2, DProp, Type),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.'],
		  [data_prop(N2, DProp, Type), '.'],
		  [class(N2, CName2), '.']|Rest1],
		  Nomina,
		 [[class(X1, CName1),
		   named(X1, PName),
		   pred(X1, X2, PredName),
		   data_prop(X2, DProp, Type),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina),
   member(named(N1, PName), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[class(N1, CName1), '.'],
		  [named(N1, PName), '.'],
		  [pred(N1, N2, PredName), '.']|Rest1],
		  Nomina,
		 [[class(X1, CName1),
		   named(X1, PName),
		   pred(X1, X2, PredName),
		   data_prop(X2, DProp, Type),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(data_prop(N2, DProp2, Type2), Nomina),
   member(class(N2, CName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
		   named(X1, PName1),
		   pred(X1, X2, PredName),
                   data_prop(X2, DProp, Type),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina),
   member(named(N1, PName1), Nomina),
   member(data_prop(N2, DProp, Type), Nomina),
   member(class(N2, CName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.']|Rest1],  
		 Nomina,
		 [[named(X1, PName1),
		   pred(X1, X2, PredName),
                   data_prop(X2, DProp, Type),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName1), Nomina),
   member(data_prop(N2, DProp, Type), Nomina),
   member(class(N2, CName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).



%%% new: 2018-12-17

complete_clauses([[named(X, PName), '.'], [pred(N, PredName), '.']|Rest1],
		 Nomina,
		 [[named(X, PName), pred(X, PredName), '.']|Rest2],
		 [N=X|Bindings]) :-		      
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N, PredName), '.']|Rest1],
		 Nomina,
		 [[named(X, PName), pred(X, PredName), '.']|Rest2],
		 [N=X|Bindings]) :-
   member(named(N, PName), Nomina),		      
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[named(N1, PName), '.'], 
                  [pred(N1, N2, PredName), '.'], 
                  [class(N2, CName), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName), pred(X1, X2, PredName), class(X2, CName), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.'], 
                  [class(N2, CName), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName), pred(X1, X2, PredName), class(X2, CName), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.'], 
                  [class(N2, CName2), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1), pred(X1, X2, PredName), class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina), 
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.'], [named(N2, PName2), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName1), pred(X1, X2, PredName), named(X2, PName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName1), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName1), pred(X1, X2, PredName), named(X2, PName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName1), Nomina),
   member(named(N2, PName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName1), pred(X1, X2, PredName), class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName1), Nomina),
   member(class(N2, CName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[pred(N1, N2, PredName), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1), pred(X1, X2, PredName), class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina),
   member(class(N2, CName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).

% -------------------------------------------------------------------


complete_clauses([[prop(N, PropName), '.']|Rest1],
		 Nomina,
		  [[named(X, PName), prop(X, PropName), '.']|Rest2],
		 [N=X|Bindings]) :-
   member(named(N, PName), Nomina),		      
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[named(N1, PName1), '.'], [prop(N1, N2, PropName), '.'], [named(N2, PName2), '.']|Rest1],
		  Nomina,
		  [[named(X1, PName1), prop(X1, X2, PropName), named(X2, PName2), '.']|Rest2],
		  [N1=X1, N2=X2|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N1, N2, PropName), '.'], [class(N2, CName2), '.']|Rest1],
		  Nomina,
		  [[named(X1, PName1), prop(X1, X2, PropName), class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName1), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[class(N1, CName1), '.'], [named(N1, PName), '.'], [prop(N1, N2, PropName), '.'], [class(N2, CName2), '.']|Rest1],
		  Nomina,
		 [[class(X1, CName1), named(X1, PName), prop(X1, X2, PropName), class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[class(N1, CName1), '.'], [prop(N1, N2, PropName), '.'], [class(N2, CName2), '.']|Rest1],
		  Nomina,
		 [[class(X1, CName1), prop(X1, X2, PropName), class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N1, N2, PropName), '.'], [named(N2, PName2), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName1), prop(X1, X2, PropName), named(X2, PName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName1), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).



% -------------------------------------------------------------------

complete_clauses([[class(N1, CName1), '.'],
		  [data_prop(N1, DProp1, Type1), '.'],
		  [prop(N1, N2, PropName), '.'],
                  [class(N2, CName2), '.'],
		  [data_prop(N2, DProp2, Type2), '.']|Rest1],
		 Nomina,
		  [[class(X1, CName1),
	            data_prop(X1, DProp1, Type1),
		    prop(X1, X2, PropName),
		    class(X2, CName2),
                    data_prop(X2, DProp2, Type2), '.']|Rest2],
		  [N1=X1, N2=X2|Bindings]) :-		     
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[class(N1, CName1), '.'],
		  [data_prop(N1, DProp1, Type1), '.'],
		  [prop(N1, N2, PropName), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
	           data_prop(X1, DProp1, Type1),
		   prop(X1, X2, PropName),
		   class(X2, CName2),
                   data_prop(X2, DProp2, Type2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N2, CName2), Nomina),
   member(data_prop(N2, DProp2, Type2), Nomina),		     
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[class(N1, CName1), '.'],
		  [prop(N1, N2, PropName), '.'],
		  [class(N2, CName2), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
		   prop(X1, X2, PropName),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[class(N1, CName1), '.'],
		  [prop(N1, N2, PropName), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
		   prop(X1, X2, PropName),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N2, CName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N, PropName), '.'],
		  [class(N, CName), '.'],
		  [data_prop(N, DProp, nominal), '.']|Rest1],
		 Nomina,
		 [[prop(X,PropName),
		   class(X, CName),
		   data_prop(X, DProp, nominal), '.']|Rest2],
		 [N=X|Bindings]) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N1, N2, PropName), '.'],
		  [class(N2, CName2), '.'],
		  [data_prop(N2, DProp2, Type2), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
		   data_prop(X1, DProp1, Type1),
		   prop(X1, X2, PropName),
		   class(X2, CName2),
		   data_prop(X2, DProp2, Type2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina),
   member(data_prop(N1, DProp1, Type1), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N1, N2, PropName), '.'], [class(N2, CName2), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
		   prop(X1, X2, PropName),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N1, N2, PropName), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName1),
		   prop(X1, X2, PropName),
		   named(X2, PName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(named(N1, PName1), Nomina),
   member(named(N2, PName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N1, N2, PropName), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
		   data_prop(X1, DProp1, Type1),
		   prop(X1, X2, PropName),
		   class(X2, CName2),
		   data_prop(X2, DProp2, Type2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina),
   member(data_prop(N1, DProp1, Type1), Nomina),
   member(class(N2, CName2), Nomina),
   member(data_prop(N2, DProp2, Type2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


complete_clauses([[prop(N1, N2, PropName), '.']|Rest1],
		 Nomina,
		 [[class(X1, CName1),
		   prop(X1, X2, PropName),
		   class(X2, CName2), '.']|Rest2],
		 [N1=X1, N2=X2|Bindings]) :-
   member(class(N1, CName1), Nomina),
   member(class(N2, CName2), Nomina),
   complete_clauses(Rest1, Nomina, Rest2, Bindings).




complete_clauses([[class(N1, CName), '.']|Rest1],
		 Nomina,
		 [[named(X1, PName), pred(X1, X2, isa), class(X2, CName), '.']|Rest2],
		 [N1=X2|Bindings]) :-
   member(named(N2, PName), Nomina),
   N1 == N2,
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


% complete_clauses([[['-', [pred(N, PredName)]], '.']|Rest1],
%		 Nomina,
%		 [[named(X, PName), '-', [pred(X, PredName)], '.']|Rest2],
%		 [N=X|Bindings]) :-
%   member(named(N, PName), Nomina),		      
%   complete_clauses(Rest1, Nomina, Rest2, Bindings).





complete_clauses([Clause|Rest1], Nomina, [Clause|Rest2], Bindings) :-
   complete_clauses(Rest1, Nomina, Rest2, Bindings).


% ----------------------------------------------------

add_missing_literals_body(Body1, Body3, Bindings) :-
   add_missing_literals_body(Body1, Body1, Body2, Bindings),
   associate_variables(Body2, Body3).


add_missing_literals_body([], Body, [], []).

% think about isa
% add_missing_literals_body([named(X1, PName), class(X1, CName1), named(Y, CName2)|Rest1],
%			  Body,
%			  [named(X1, PName), class(X1, CName1), named(Y, CName2)|Rest2],
%			  Bindings) :-
%   CName1 = location,
%   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([named(X1, PName), class(X1, CName)|Rest1],
			  Body,
			  [named(X1, PName), pred(X1, X2, isa), class(X2, CName)|Rest2],
			  Bindings) :-
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([class(X1, CName1), prop(X1, X2, PName), class(X2, CName2)|Rest1],
			  Body,
			  [class(X1, CName1), prop(X1, X2, PName), class(X2, CName2)|Rest2],
			  Bindings) :-
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([class(X1, CName1), prop(X1, X2, PName)| Rest1],
			  Body,
			  [class(X1, CName1), prop(X1, X2, PName), class(X2, CName2)|Rest2],
			  Bindings) :-
   member(class(N2, CName2), Body),
   N2 == X2,
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([prop(X1, X2, PropName), class(X2, CName2)|Rest1],
			  Body,
			  [named(X1, PName1), prop(X1, X2, PropName), class(X2, CName2)|Rest2],
			  Bindings) :-
   member(named(N1, PName1), Body),
   N1 == X1,
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([prop(X1, X2, PName), class(X2, CName2)|Rest1],
			  Body,
			  [class(X1, CName1), prop(X1, X2, PName), class(X2, CName2)|Rest2],
			  Bindings) :-
   member(class(N1, CName1), Body),
   N1 == X1,
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([class(X1, CName1), pred(X1, X2, PName)|Rest1],
			  Body,
			  [class(X1, CName1), pred(X1, X2, PName)|Rest2],
			  Bindings) :-
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([pred(X1, X2, PName), class(X2, CName2)|Rest1],
			  Body,
			  [class(X1, CName1), pred(X1, X2, PName), class(X2, CName2)|Rest2],
			  Bindings) :-
   member(class(N1, CName1), Body),
   N1 == X1,
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([prop(X1, X2, PName)| Rest1],
			  Body,
			  [class(X1, CName1), prop(X1, X2, PName), class(X2, CName2)|Rest2],
			  Bindings) :-
   member(class(N1, CName1), Body),
   N1 == X1,
   member(class(N2, CName2), Body),
   N2 == X2, 
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([pred(X1, X2, PredName)| Rest1],
			  Body,
			  [class(X1, CName1), pred(X1, X2, PredName), class(X2, CName2)|Rest2],
			  Bindings) :-
   member(class(N1, CName1), Body),
   N1 == X1,
   member(class(N2, CName2), Body),
   N2 == X2, 
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


add_missing_literals_body([Literal|Rest1], Body, [Literal|Rest2], Bindings) :-
   add_missing_literals_body(Rest1, Body, Rest2, Bindings).


% ----------------------------------------------------

add_missing_literals_head([], _, []).
 

add_missing_literals_head([class(X, CName1)|Rest1], Body, [pred(X, Z, isa), class(Z, CName1)|Rest2]) :-
   member(class(Y, CName2), Body),
   CName1 \== CName2,
   X == Y,
   add_missing_literals_head(Rest1, Body, Rest2).


add_missing_literals_head([rel(A1, B1, RName)|Rest1],
			  Body,
			  [variable(C1, VName),
			   data_prop(C1, pos_int(A1), Type),
			   rel(A1, B1, RName),
			   class(B1, CName)|Rest2]) :-
   var(A1),
   member(variable(C1, VName), Body),
   member(data_prop(C2, pos_int(A2), Type), Body),
   C1 == C2,			  
   A1 == A2,
   member(class(B2, CName), Body),
   B1 == B2,			  
   add_missing_literals_head(Rest1, Body, Rest2).


add_missing_literals_head([rel(A1/A3, B1, RName)|Rest1],
			  Body,
			  [variable(C1, VName1),
			   data_prop(C1, pos_int(A1), Type),
			   variable(C3, VName2),
			   data_prop(C3, pos_int(A3), Type),
			   rel(A1/A3, B1, RName),
			   class(B1, CName)|Rest2]) :-
   member(variable(C1, VName1), Body),
   member(data_prop(C2, pos_int(A2), Type), Body),
   C1 == C2,			  
   member(variable(C3, VName2), Body),
   VName1 \= VName2,
   member(data_prop(C4, pos_int(A3), Type), Body),
   C3 == C4,			  
   member(class(B2, CName), Body),
   B1 == B2,			  
   add_missing_literals_head(Rest1, Body, Rest2).

  
add_missing_literals_head([pred(X1, Y1, PredName)|Rest1], 
                          Body, 
                          [class(X1, CName1), pred(X1, Y1, PredName), class(Y1, CName2)|Rest2]) :-
   member(class(X2, CName1), Body),
   X1 == X2,
   member(class(Y2, CName2), Body),
   Y1 == Y2,
   add_missing_literals_head(Rest1, Body, Rest2).


add_missing_literals_head([prop(X1, PropName)|Rest1], 
                          Body, 
                          [named(X1, Name), prop(X1, PropName)|Rest2]) :-
   member(named(X2, Name), Body),
   X1 == X2,
   add_missing_literals_head(Rest1, Body, Rest2).


add_missing_literals_head([prop(X1, PropName)|Rest1], 
                          Body, 
                          [class(X1, CName), prop(X1, PropName)|Rest2]) :-
   member(class(X2, CName), Body),
   X1 == X2,
   add_missing_literals_head(Rest1, Body, Rest2).


add_missing_literals_head([Literal|Rest1], Body, [Literal|Rest2]) :-
   add_missing_literals_head(Rest1, Body, Rest2).


% ----------------------------------------------------
% associate_variables/2
% ----------------------------------------------------
  
associate_variables(Body1, Body3) :-
   collect_same_classes(Body1, Body1, Classes1),
   sort(Classes1, Classes2),
   findall(VName, lexicon([cat:var, wform:_, arg:_, lit:variable(X, VName)]), VNameList),
   insert_variables(Classes2, VNameList, Body1, Body2),
   findall(NName, lexicon([cat:nvar, wform:_, num:_, type:_, arg:X, lit:[_, variable(X, NName)]]), NNameList),
   insert_numeric_variables(NNameList, Body2, Body3).


% ----------------------------------------------------
% collect_same_classes/3
% ----------------------------------------------------

collect_same_classes([], _, []).

collect_same_classes([class(X, CName)|Rest1], Body, [class(X, CName)|Rest2]) :-
   member(class(Y, CName), Body),
   X \== Y,
   collect_same_classes(Rest1, Body, Rest2).

collect_same_classes([Literal|Rest1], Body,  Rest2) :-
   collect_same_classes(Rest1, Body, Rest2).


% ----------------------------------------------------
% insert_variables/4
% ----------------------------------------------------

insert_variables([], _, Result, Result).

insert_variables([class(X, CName)|Rest1], [VName|Rest2], Body1, Body3) :-
   insert_variable(class(X, CName), VName, Body1, Body2),
   insert_variables(Rest1, Rest2, Body2, Body3).

insert_variable(class(X1, CName), VName, [], []).

insert_variable(class(X1, CName), VName, [class(X2, CName)|Rest1], [class(X1, CName), variable(X1, VName)|Rest2]) :-
   X1 == X2, 
   insert_variable(class(X1, CName), VName, Rest1, Rest2).

insert_variable(class(X, CName), VName, [Literal|Rest1], [Literal|Rest2]) :-
   insert_variable(class(X, CName), VName, Rest1, Rest2).


% ----------------------------------------------------
% insert_numeric_variables/3
% ----------------------------------------------------

insert_numeric_variables(_, [], []).

insert_numeric_variables([NName|NNameList],
			 [data_prop(X, pos_int(Num), Type)|Body2],
			 [variable(X, NName), data_prop(X, pos_int(Num), Type)|Body3]) :-
   var(Num),
   insert_numeric_variables(NNameList, Body2, Body3).

insert_numeric_variables(NNameList, [Literal|Body2], [Literal|Body3]) :-
   insert_numeric_variables(NNameList, Body2, Body3).


% ----------------------------------------------------
% pretty_print/1
% ----------------------------------------------------

pretty_print([]).

pretty_print([List|Lists]) :-
   write(List), nl, nl,
   pretty_print(Lists).
