% ===================================================================
% Project: PENG ASP
% Module:  writer.pl
% Author:  Rolf Schwitter
% Date:    2019-01-02
% ===================================================================

:- module(writer, [writer/2, writer/3, write_clauses/2]).

:- style_check([-discontiguous, -singleton]).

% -------------------------------------------------------------------
% writer/2
% writer/3
% -------------------------------------------------------------------

writer(FileName, [Clauses1]) :-
   reorder_clauses(Clauses1, Clauses2),
   reset_gensym(''),
   format_clauses(Clauses2, Clauses3),
   numbervars(Clauses3),
   open(FileName, write, Stream),
   write_clauses(Stream, Clauses3),
   close(Stream).


writer(FileName, [Clauses1], Clauses3) :-
   reorder_clauses(Clauses1, Clauses2),
   reset_gensym(''),
   format_clauses(Clauses2, Clauses3),
   numbervars(Clauses3),
   open(FileName, write, Stream),
   write_clauses(Stream, Clauses3),
   close(Stream).


% -------------------------------------------------------------------
% reorder_clauses/2
% -------------------------------------------------------------------

reorder_clauses(Clauses1, Clauses3) :-
   group_clauses(Clauses1, Clauses2),
   reverse(Clauses2, Clauses3).


% -------------------------------------------------------------------
% group_clauses/2
% -------------------------------------------------------------------

group_clauses(Clauses1, [['.'|Clauses3]|RClauses2]) :-
   append(['.'|Clauses3], ['.'|Clauses2], Clauses1),
   group_clauses(['.'|Clauses2], RClauses2).
  
 group_clauses(Clauses, [Clauses]) :-
   append(['.'|_], [], Clauses).


% -------------------------------------------------------------------
% format_clauses/2
% -------------------------------------------------------------------

format_clauses([], []).

format_clauses([Clause1|Clauses1], [Clause2|Clauses2]) :-   
   (
      Clause1 = ['.', [1, '}', [[named(X, PName1), ';', named(X, PName2)|Conds1], ':', Lit], '{', 1]]
      ->
      pooling_names([named(X, PName1), ';', named(X, PName2)|Conds1], Conds2),
      Clause2 = [[1, '{', [Lit, ':', Conds2], '}', 1], '.']
   ;
      Clause1 = ['.', Body, '-:', Head]
      ->
      flatten(Body, FBody),
      flatten(Head, FHead),
      reverse_and_format(FBody, RBody),
      reverse_and_format(FHead, RHead),
      copy_term([RBody, RHead], [CRBody, CRHead]),   %%% check copy_term/2
      Clause2 = [CRHead, ':-', CRBody, '.']
   ;
      Clause1 = ['.', Body, '-:']
      ->
      flatten(Body, FBody),
      reverse_and_format(FBody, RBody),
      copy_term(RBody, CRBody),                      %%% check copy_term/2
      Clause2 = [':-', CRBody, '.']
   ;
      Clause1 = ['.', ['}', [Cond1, ':', Lit1], '{', '#minimize']],
      reverse_and_format(Cond1, Cond2),
      reverse(Lit1, Lit2),
      Clause2 = [['#minimize', '{', [Lit2, ':', Cond2], '}'], '.']
   ;
      Clause1 = ['.', ['}', [Cond1, ':', Lit1], '{', '#maximize']],
      reverse_and_format(Cond1, Cond2),
      reverse(Lit1, Lit2),
      Clause2 = [['#maximize', '{', [Lit2, ':', Cond2], '}'], '.']
   ;
      flatten(Clause1, FClause),
      reverse_and_format(FClause, RevFClause),
      %% nl, nl, write('RevFClause: '), write(RevFClause), nl, nl,
      skolemize(RevFClause, Clause2)
   ),
   format_clauses(Clauses1, Clauses2).


% -------------------------------------------------------------------
% pooling_names/2
% -------------------------------------------------------------------

pooling_names([named(X, PName1), ';'|Rest], [named(X, Atom3)]) :-
   reverse([named(X, PName1), ';'|Rest], RevLiterals),
   extract_names_for_pooling(RevLiterals, ListOfNames),
   atomic_list_concat(ListOfNames, ' ; ', Atom1),
   atom_concat('(', Atom1, Atom2),
   atom_concat(Atom2, ')', Atom3).
	   
extract_names_for_pooling([named(X, PName)], [PName]).
	  
extract_names_for_pooling([named(X, PName), ';'|Rest1], [PName|Rest2]) :-
   extract_names_for_pooling(Rest1, Rest2).	  


% -------------------------------------------------------------------
% skolemize/2
% -------------------------------------------------------------------

skolemize([], []).

skolemize([class(Y, ClassName), pred(X, Y, isa)|Lits1], [class(X, ClassName), pred(X, Y, isa)|Lits2]) :-
   skolemize(Lits1, Lits2).

skolemize([number(X, Num), class(X, ClassName)|Lits1], [number(X, Num), class(X, ClassName)|Lits2]) :-
   skolemize(Lits1, Lits2).


skolemize([named(X, PName), class(X, CName)|Lits1], [named(SK, PName), class(SK, CName)|Lits2]) :-
   gensym('', SK),
   X = SK,	  
   skolemize(Lits1, Lits2).


skolemize([class(X, CName), named(X, PName)|Lits1], [class(SK, CName), named(SK, PName)|Lits2]) :-
   gensym('', SK),
   X = SK,	  
   skolemize(Lits1, Lits2).


skolemize([named(X, PName)|Lits1], [named(SK, PName)|Lits2]) :-
   gensym('', SK),
   X = SK,	  
   skolemize(Lits1, Lits2).

skolemize([class(X, ClassName)|Lits1], [class(SK, ClassName)|Lits2]) :-
   gensym('', SK),
   X = SK,	  
   skolemize(Lits1, Lits2).

skolemize([Lit|Lits1], [Lit|Lits2]) :-
   skolemize(Lits1, Lits2).


% -------------------------------------------------------------------
% reverse_and_format/2
% -------------------------------------------------------------------

reverse_and_format(List, RList) :-
   reverse_and_format(List, RList, []).

reverse_and_format([], W, W).

% reverse_and_format([named(X, PName), ';', named(X, Name)|T], W, Acc) :-  % ???
%   X = Y,
%   NegClass = -class(X, ClassName),
%   Name = named(X, Name),
%   reverse_and_format(T, W, [Name, NegClass|Acc]).


reverse_and_format([class(Y, ClassName), pred(X, Y, isa), '-', named(X, Name)|T], W, Acc) :-
   X = Y,
   NegClass = -class(X, ClassName),
   Name = named(X, Name),
   reverse_and_format(T, W, [Name, NegClass|Acc]).

reverse_and_format([class(Y, ClassName), pred(X, Y, isa), named(X, Name)|T], W, Acc) :-
   X = Y,
   Class = [ClassName, X],
   Name  = named(X, Name),
   reverse_and_format(T, W, [Name, Class|Acc]).  

reverse_and_format([number(X, Num), class(X, ClassName)|T], W, Acc) :-
   X = Num,
   Class = class(X, ClassName),
   reverse_and_format(T, W, [Class|Acc]).

reverse_and_format([pred(X, Y, Z, PredName), '-'|T], W, Acc) :-
   NegPred = -pred(X, Y, Z, PredName),
   reverse_and_format(T, W, [NegPred|Acc]).

reverse_and_format([pred(X, Y, PredName), '-'|T], W, Acc) :-
   NegPred = -pred(X, Y, PredName),
   reverse_and_format(T, W, [NegPred|Acc]).

reverse_and_format([pred(X, PredName), '-'|T], W, Acc) :-
   NegPred = -pred(X, PredName),
   reverse_and_format(T, W, [NegPred|Acc]).

reverse_and_format([prop(X, Y, Z, PropName), '-'|T], W, Acc) :-
   NegProp = -prop(X, Y, Z, PropName),
   reverse_and_format(T, W, [NegProp|Acc]).

reverse_and_format([prop(X, Y, PropName), '-'|T], W, Acc) :-
   NegProp = -prop(X, Y, PropName),
   reverse_and_format(T, W, [NegProp|Acc]).

reverse_and_format([prop(X, PropName), '-'|T], W, Acc) :-
   NegProp = -prop(X, PropName),
   reverse_and_format(T, W, [NegProp|Acc]).

reverse_and_format([pred(X, Y, isa)|T], W, Acc) :-
   X = Y,
   reverse_and_format(T, W, Acc).

reverse_and_format([variable(X, VarName)|T], W, Acc) :-
   reverse_and_format(T, W, Acc).

reverse_and_format([H|T], W, Acc) :-
   reverse_and_format(T, W, [H|Acc]).


% -------------------------------------------------------------------
% reverse_and_format_conds/3
% -------------------------------------------------------------------

reverse_and_format_conds(List, RList) :-
   reverse_and_format_conds(List, RList, []).

reverse_and_format_conds([], W, W).

reverse_and_format_conds([variable(X, VarName)|T], W, Acc) :-
   reverse_and_format_conds(T, W, Acc).

reverse_and_format_conds([H|T], W, Acc) :-
   reverse_and_format_conds(T, W, [H|Acc]).

% -------------------------------------------------------------------
% write_clauses/2
% -------------------------------------------------------------------

write_clauses(Stream, []).

write_clauses(Stream, [[Head, ':-', Body, '.']|Clauses]) :-
   write_head_literals(Stream, Head),
   write(Stream, ':- '),
   write_body_literals(Stream, Body),
   write(Stream, '.'),
   nl(Stream),
   nl(Stream),
   write_clauses(Stream, Clauses).

write_clauses(Stream, [[':-', Body, '.']|Clauses]) :-
   write(Stream, ':- '),
   write_body_literals(Stream, Body),
   write(Stream, '.'),
   nl(Stream),
   nl(Stream),
   write_clauses(Stream, Clauses).

write_clauses(Stream, [[[1, '{', [Lit, ':', Conds], '}', 1], '.']|Clauses]) :-
   write(Stream, '1 { '),
   write_list(Stream, Lit),
   write(Stream, ' : '),
   write_list(Stream, Conds),
   write(Stream, ' } 1.'),
   nl(Stream),
   nl(Stream),
   write_clauses(Stream, Clauses).

write_clauses(Stream, [[['#minimize', '{', [Lit, ':', Cond], '}'], '.']|Clauses]) :-
   write(Stream, '#minimize'),
   write(Stream, ' { '),
   write_list(Stream, Lit),
   write(Stream, ' : '),
   write_list(Stream, Cond),
   write(Stream, ' }'),
   write(Stream, '.'),
   nl(Stream),
   nl(Stream),
   write_clauses(Stream, Clauses).

write_clauses(Stream, [[['#maximize', '{', [Lit, ':', Cond], '}'], '.']|Clauses]) :-
   write(Stream, '#maximize'),
   write(Stream, ' { '),
   write_list(Stream, Lit),
   write(Stream, ' : '),
   write_list(Stream, Cond),
   write(Stream, ' }'),
   write(Stream, '.'),
   nl(Stream),
   nl(Stream),
   write_clauses(Stream, Clauses).


write_list(Stream, [Lit]) :-
   write(Stream, Lit).

write_list(Stream, ['@'('/'(W2, W1), Lit1), Lit2|Lits]) :-
   write(Stream, W2),
   write(Stream, '/'),
   write(Stream, W1),
   write(Stream, '@'),
   write(Stream, Lit1),
   write(Stream, ', '),
   write(Stream, Lit2).
  %  write_list(Stream, Lits).

write_list(Stream, ['@'(Lit1, Lit2)|Lits]) :-
   write(Stream, Lit1),
   write(Stream, '@'),
   write(Stream, Lit2),
   write(Stream, ', '),
   write_list(Stream, Lits).

write_list(Stream, [Lit, ';'|Lits]) :-
   write(Stream, Lit),
   write(Stream, ' ; '),
   write_list(Stream, Lits).

write_list(Stream, [Lit, '.'|Lits]) :-
   write(Stream, Lit),
   write(Stream, '.').

write_list(Stream, [Lit|Lits]) :-
   write(Stream, Lit),
   write(Stream, ', '),
   write_list(Stream, Lits).

write_clauses(Stream, [Clause|Clauses]) :-
   write_clause(Stream, Clause),
   write_clauses(Stream, Clauses).

% -------------------------------------------------------------------

write_clause(Stream, [Literal, '.']) :-
   write(Stream, Literal),
   write(Stream, '.'),
   nl(Stream),
   nl(Stream).

write_clause(Stream, [Literal|Literals]) :-
   write(Stream, Literal),
   write(Stream, '.'),
   nl(Stream),
   nl(Stream),
   write_clause(Stream, Literals).

% -------------------------------------------------------------------

write_head_literals(Stream, [Literal]) :-
   write(Stream, Literal),
   write(Stream, ' ').

write_head_literals(Stream, [Literal|Literals]) :-
   write(Stream, Literal),
   write(Stream, ' '),
   write_head_literals(Stream, Literals).

% -------------------------------------------------------------------

write_body_literals(Stream, [Literal]) :-
   write(Stream, Literal).

write_body_literals(Stream, [Literal|Literals]) :-
   write(Stream, Literal),
   write(Stream, ', '),
   write_body_literals(Stream, Literals).




