% ====================================================
% Project: PENG ASP
% Module:  planner.pl
% Author:  Rolf Schwitter
% Created: 2019-01-02
% ====================================================	

:- module(planner, [planner/2]).


% ------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).


% ----------------------------------------------------
% planner/2
% ----------------------------------------------------
  
planner(ClauseList1, ClauseList3) :-
   apply_strategy(ClauseList1, ClauseList2),
   flatten_clause_list(ClauseList2, [], ClauseList3).


% ----------------------------------------------------
% apply_strategy/2
% ----------------------------------------------------

apply_strategy(ClauseList1, ClauseList3) :-
   enumeration(ClauseList1, ClauseList2),
   verb_phrase_coordination(ClauseList2, ClauseList3).


% ----------------------------------------------------
% strategy: enumeration/2
%				
% ----------------------------------------------------

enumeration([], []).

enumeration([[class(A, ClassName), data_prop(A, Num, Type), prop(A, B, PropName)|Rest]|Clauses1],
	    [[class(A, ClassName), data_prop(A, Num, Type), prop(A, B, PropName)|Objects3]|Clauses3]) :-
   same_subject_role([class(A, ClassName), data_prop(A, Num, Type), prop(A, B, PropName)],
		      Clauses1, Clauses2, [], Objects1),
   append(Object, ['.'], Rest),
   append(Object, Objects1, Objects2),
   append(Objects2, ['.'], Objects3),
   enumeration(Clauses2, Clauses3).

enumeration([ClauseList1|ClauseLists1], [ClauseList1|ClauseLists2]) :-
   enumeration(ClauseLists1, ClauseLists2).

same_subject_role(_, [], Clauses, Objects, Objects).

same_subject_role([ class(A1, ClassName1), data_prop(A1, Num1, Type1), prop(A1, B1, PropName1)],
		  [[class(A2, ClassName2), data_prop(A2, Num2, Type2), prop(A2, B2, PropName2)|Rest]|Clauses1],
                  Clauses2, Objects1, Objects3) :-			      
  A1 == A2,
  ClassName1 == ClassName2,
  Num1 == Num2,
  Type1 == Type2,
  PropName1 == PropName2,
  append(Object, ['.'], [prop(A2, B2, PropName2)|Rest]),
  append(Objects1, Object, Objects2), 
  same_subject_role([class(A1, ClassName1), data_prop(A1, Num1, Type1), prop(A1, B1, PropName1)], Clauses1, Clauses2, Objects2, Objects3).

same_subject_role(ClauseList1, [ClauseList2|ClauseLists2], [ClauseList2|ClauseLists3], Objects1, Objects2) :-	     
  same_subject_role(ClauseList1, ClauseLists2, ClauseLists3, Objects1, Objects2).

		  
% ----------------------------------------------------
% strategy: verb_phrase_coordination/2
%				
% If two binary relations with the same subject
% immediately follow each other, then prepare for
% aggregation (= verb phrase coordination).
% ----------------------------------------------------

verb_phrase_coordination(ClauseList1, ClauseList2) :-
   collect_names_in_subject_position(ClauseList1, Names),
   %% sort(Names, SortedNames), 
   aggregate_verb_phrases(Names, ClauseList1, ClauseList2).

verb_phrase_coordination(ClauseList, ClauseList).  

% ----------------------------------------------------

collect_names_in_subject_position([], []).

collect_names_in_subject_position([[class(X, CName), named(X, Name), pred(X, Y, PredName)|Lits1]|Rest],
				   [class(X, CName), named(X, Name)|Lits2]) :-
   collect_names_in_subject_position(Rest, Lits2).

collect_names_in_subject_position([[named(X, Name), pred(X, Y, PredName)|Lits1]|Rest], [named(X, Name)|Lits2]) :-
   collect_names_in_subject_position(Rest, Lits2).

collect_names_in_subject_position([[named(X, Name), pred(X, PredName)|Lits1]|Rest], [named(X, Name)|Lits2]) :-
   collect_names_in_subject_position(Rest, Lits2).

collect_names_in_subject_position([[named(X, Name), prop(X, Y, PropName)|Lits1]|Rest], [named(X, Name)|Lits2]) :-
   collect_names_in_subject_position(Rest, Lits2).

collect_names_in_subject_position([[named(X, Name), prop(X, PropName)|Lits1]|Rest], [named(X, Name)|Lits2]) :-
   collect_names_in_subject_position(Rest, Lits2).

collect_names_in_subject_position([Lit|Lits1], Lits2) :-
   collect_names_in_subject_position(Lits1, Lits2).


% ----------------------------------------------------

aggregate_verb_phrases([], ClauseList, ClauseList).

aggregate_verb_phrases([class(X, CName), named(X, PName)|Rest], ClauseList1, ClauseList3) :-
   aggregate_two_verb_phrases([class(X, CName), named(X, PName)], ClauseList1, ClauseList2),
   aggregate_verb_phrases(Rest, ClauseList2, ClauseList3).

aggregate_verb_phrases([named(X, Name)|Rest], ClauseList1, ClauseList3) :-
   aggregate_two_verb_phrases([named(X, Name)], ClauseList1, ClauseList2),
   aggregate_verb_phrases(Rest, ClauseList2, ClauseList3).


% ----------------------------------------------------

aggregate_two_verb_phrases(_, [], []).

aggregate_two_verb_phrases([named(X0, Name)],
		    [[named(X1, Name), pred(X1, Y1, PredName1)|Rest1],
		     [named(X1, Name), pred(X1, Y2, PredName2)|Rest2]|Rest4],
		    [LiteralList|LiteralLists]) :-
   X0 == X1,
   PredName1 \= isa,
   PredName2 \= isa,
   append(Rest3, ['.'], Rest1), 
   append([named(X1, Name), pred(X1, Y1, PredName1)|Rest3],
	  [pred(X1, Y2, PredName2)|Rest2], LiteralList),
   aggregate_two_verb_phrases([named(X0, Name)], Rest4, LiteralLists).


aggregate_two_verb_phrases([class(X0, CName), named(X0, Name)],
		    [[class(X1, CName), named(X1, Name), pred(X1, Y1, PredName1)|Rest1],
		     [class(X1, CName), named(X1, Name), pred(X1, Y2, PredName2)|Rest2]|Rest4],
		    [LiteralList|LiteralLists]) :-
   X0 == X1,
   PredName1 \= isa,
   PredName2 \= isa,
   append(Rest3, ['.'], Rest1), 
   append([class(X1, CName), named(X1, Name), pred(X1, Y1, PredName1)|Rest3],
	  [pred(X1, Y2, PredName2)|Rest2], LiteralList),
   aggregate_two_verb_phrases([class(X0, CName), named(X0, Name)], Rest4, LiteralLists).



aggregate_two_verb_phrases([named(X0, Name)],
		    [[named(X1, Name), pred(X1, Y1, PredName1)|Rest1],
		     [named(X1, Name), prop(X1, Y2, PropName2)|Rest2]|Rest4],
		    [LiteralList|LiteralLists]) :-
   X0 == X1,
   PredName1 \= isa,
   append(Rest3, ['.'], Rest1),
   append([named(X1, Name), pred(X1, Y1, PredName1)|Rest3],
	  [prop(X1, Y2, PropName2)|Rest2], LiteralList),
   aggregate_two_verb_phrases([named(X0, Name)], Rest4, LiteralLists).


aggregate_two_verb_phrases([named(X0, Name)],
		    [[named(X1, Name), prop(X1, Y1, PropName1)|Rest1],
		     [named(X1, Name), pred(X1, Y2, PredName2)|Rest2]|Rest4],
		    [LiteralList|LiteralLists]) :-
   X0 == X1,
   PredName2 \= isa,
   append(Rest3, ['.'], Rest1),
   append([named(X1, Name), prop(X1, Y1, PropName1)|Rest3],
	  [pred(X1, Y2, PredName2)|Rest2], LiteralList),
   aggregate_two_verb_phrases([named(X0, Name)], Rest4, LiteralLists).


aggregate_two_verb_phrases([named(X0, Name)],
		    [[named(X1, Name), prop(X1, Y1, PropName1)|Rest1],
		     [named(X1, Name), prop(X1, Y2, PropName2)|Rest2]|Rest4],
			   [LiteralList|LiteralLists]) :-
   X0 == X1,
   append(Rest3, ['.'], Rest1),
   append([named(X1, Name), prop(X1, Y1, PropName1)|Rest3],
	  [prop(X1, Y2, PropName2)|Rest2], LiteralList),
   aggregate_two_verb_phrases([named(X0, Name)], Rest4, LiteralLists).


% ----------------------------------------------------------------
% Experimental

aggregate_two_verb_phrases([class(X0, ClassName), data_prop(X0, Number, Type)],
		    [[class(X1, ClassName), data_prop(X1, Number, Type), prop(X1, Y1, PropName)|Rest1],
		     [class(X1, ClassName), data_prop(X1, Number, Type), prop(X1, Y2, PropName)|Rest2]|Rest4],
			   [LiteralList|LiteralLists]) :-
   X0 == X1,
   append(Rest3, ['.'], Rest1),
   append([class(X1, ClassName), data_prop(X1, Number, Type),  prop(X1, Y1, PropName)|Rest3],
	  [prop(X1, Y2, PropName)|Rest2], LiteralList),
   aggregate_two_verb_phrases([class(X0, ClassName), data_prop(X0, Number, Type)], Rest4, LiteralLists).   


aggregate_two_verb_phrases(Names, [ClauseList|Rest], [ClauseList|ClauseLists]) :-
   aggregate_two_verb_phrases(Names, Rest, ClauseLists).


% ----------------------------------------------------
% flatten_clause_list/3
% ----------------------------------------------------

flatten_clause_list([], List, List).

flatten_clause_list([Clause|Clauses], List1, List3) :-
   append(List1, Clause, List2), 
   flatten_clause_list(Clauses, List2, List3).				
