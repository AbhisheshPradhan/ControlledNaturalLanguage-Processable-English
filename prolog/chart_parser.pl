% ================================================================
% Project: PENG ASP
% Module:  chart_parser.pl
% Author:  Rolf Schwitter
% Date:    2019-01-02
% =================================================================

% -----------------------------------------------------------------------
% Style checking
% -----------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).


% -----------------------------------------------------------------------
% Dynamic
% -----------------------------------------------------------------------

:- dynamic edge/5.

/*
% -----------------------------------------------------------------------
% term_expansion/2
% transfors a definite clause gammar into a format that can be processed
% by the chart parser
% -----------------------------------------------------------------------

term_expansion((LHS --> RHS), Format) :-
  (
     RHS    = ( { lexicon(List) }, TokenList )
     ->
     Format = ( rule(LHS, [ lexicon(List) ]) )
  ;
     RHS    = ( TokenList, { lexicon(List) } )
     ->
     Format = ( rule(LHS, [ lexicon(List) ]) )
  ;
     conjunction_to_list_rhs(RHS, RHSList)
     ->
     Format = ( rule(LHS, RHSList) )
  ;
     Format = ( rule(LHS, RHS ) )
  ).

conjunction_to_list_rhs(RHS, RHSList) :-
  \+ is_list(RHS), 
  conjunction_to_list(RHS, RHSList).

conjunction_to_list((Term, Terms), [Term|ListofTerms]) :- !, 
  nonvar(Term), 
  conjunction_to_list(Terms, ListofTerms).

conjunction_to_list(Term, [Term]).

*/

% ----------------------------------------------------------------
% call_chart_parser/3
% allows to process test sentences with the chart parser.
% ----------------------------------------------------------------

call_chart_parser(SNum, s([mode:M, cl:C1-C2, ante:A1-A2, tree:T]), TokenList) :-
   initialise_chart_parser(SNum, V0, s([mode:M, cl:C1-C2, ante:A1-A2, tree:T])),
   chart_parser(SNum, V0, Vn, TokenList),
   (
      edge(SNum, V0, Vn, s([mode:M, cl:C1-C2, ante:A1-A2, tree:T]), _)
   ;
      true
   ),
   nl, nl, write('Parse: '), write(T), nl, nl.


call_chart_parser_2(SNum, s([mode:M, cl:C1-C2, ante:A1-A2, tree:T]), TokenList) :-
   initialise_chart_parser(SNum, V0, s([mode:M, cl:C1-C2, ante:A1-A2, tree:T])),
   chart_parser(SNum, V0, Vn, TokenList),
   (
      edge(SNum, V0, Vn, s([mode:M, cl:C1-C2, ante:A1-A2, tree:T]), _)
   ;
      true
   ).


% ----------------------------------------------------------------
% initialise_chart_parser/3
% retracts all existing edges if SNum = 1 and starts active edges
% at vertex V0.
% ----------------------------------------------------------------

initialise_chart_parser(SNum, V0, LHS) :-
   (
      SNum = 1
      ->
      retractall(edge(_, _, _, _, _))
   ;
      SNum > 1
      ->
      true
   ),
   V0 = 0,
   start_active(SNum, V0, LHS).


% ----------------------------------------------------------------
% chart_parser/4
% processes sentence on a token level and starts the chart for
% each token
% ----------------------------------------------------------------

chart_parser(SNum, Vn, Vn, []).

chart_parser(SNum, V0, Vn, [Token|Tokens]) :-
   start_chart(SNum, V0, V1, [Token]),
   chart_parser(SNum, V1, Vn, Tokens).


% ----------------------------------------------------------------
% chart_parser_cmdline/4
% starts the chart for a single token
% ----------------------------------------------------------------

chart_parser_cmdline(SNum, V0, V1, [Token]) :-
   start_chart(SNum, V0, V1, [Token]).


% ----------------------------------------------------------------
% start_chart_parser/4
% calls the chart parser via the json interface; uses add_edge/5
% to insert inactive edges for the token (and its corresponding
% category) into the chart.
% Note there is no need to increase V0 here, since the client
% takes care of this.
% ----------------------------------------------------------------				

start_chart_parser(SNum, V0, V1, Token) :-
   foreach(token(SNum, V0, V1, Category, Token), add_edge(SNum, V0, V1, Category, [])).


% ----------------------------------------------------------------
% start_chart/4
% uses add_edge/5 to insert inactive edges for the token (and its
% corresponding category) into the chart
% ----------------------------------------------------------------				

start_chart(SNum, V0, V1, Token) :-
   V1 is V0 + 1,
   foreach(token(SNum, V0, V1, Category, Token), add_edge(SNum, V0, V1, Category, [])).


% ----------------------------------------------------------------
% Add active edges of type Category at vertex V0 by looking up
% the rules which expand Category in the grammar
% ----------------------------------------------------------------

% For every rule that expands Category, start an empty active edge
% based on the rule at Vertex V0.
start_active(SNum, V0, Category) :-
   foreach(rule(Category, Categories), add_edge(SNum, V0, V0, Category, Categories)).


% Call Prolog goal in curly brackets, for example for anaphora resolution
add_edge(SNum, V1, V2, Category, [{Goal}|Categories]) :-
   nonvar(Goal),
   call(Goal), 
   add_edge(SNum, V1, V2, Category, Categories).


% Check if the edge already exists
add_edge(SNum, V1, V2, Category, Categories) :-
   edge(SNum, V1, V2, Category, Categories), !.


% For an inactive edge, we need only to worry about the fundamental rule:
add_edge(SNum, V1, V2, Category1, []) :-
   assert_edge(SNum, V1, V2, Category1, []), 
   foreach(edge(SNum, V0, V1, Category2, [Category1|Categories]), add_edge(SNum, V0, V2, Category2, Categories)).


% For an active edge, we need to worry about both the fundamental and the top-down rule:
add_edge(SNum, V1, V2, Category1, [Category2|Categories]) :-
   assert_edge(SNum, V1, V2, Category1, [Category2|Categories]), 
   foreach(edge(SNum, V2, V3, Category2, []), add_edge(SNum, V1, V3, Category1, Categories)), 
   start_active(SNum, V2, Category2).


% ----------------------------------------------------------------
% assert_edge/5
% asserta(edge(...)), but gives option of displaying nature of
% the created edge
% ----------------------------------------------------------------

assert_edge(SNum, V1, V2, Category1, []) :-
   asserta(edge(SNum, V1, V2, Category1, [])).
   % nl,
   % dbgwrite(inactive(SNum, V1, V2, Category1, [])).

assert_edge(SNum, V1, V2, Category1, [Category2|Categories]) :-
   asserta(edge(SNum, V1, V2, Category1, [Category2|Categories])).
   % nl,
   % dbgwrite(active(SNum, V1, V2, Category1, [Category2|Categories])).

   
% ----------------------------------------------------------------
% token/6
% ----------------------------------------------------------------

token(compound, SNum, V1, V2, Category, [Token]) :-
   edge(SNum, V0, V1, Category, [ lexicon([cat:Cat, wform:[Token], struc:compound]) ]),
   add_edge(SNum, V0, V2, Category, []).

token(compound, SNum, V1, V2, Category, [Token]) :-
   edge(SNum, V0, V1, Category, [ lexicon([cat:Cat, wform:[Token|Tokens], struc:compound]) ]),
   assert_edge(SNum, V0, V2, Category, [ lexicon([cat:Cat, wform:Tokens, struc:compound]) ] ).

token(compound, SNum, V1, V2, Category, [Token]) :-
   call( lexicon([cat:Cat, wform:[Token|Tokens]|Rest]) ),
   call( rule(Category, [ lexicon([cat:Cat, wform:[Token|Tokens]|Rest]) ]) ),
   assert_edge(SNum, V1, V1, Category, [ lexicon([cat:Cat, wform:[Token|Tokens], struc:compound]) ]),
   assert_edge(SNum, V1, V2, Category, [ lexicon([cat:Cat, wform:Tokens, struc:compound]) ]).

token(simple, SNum, V1, V2, Category, [Token]) :-
   call( lexicon([cat:Cat, wform:[Token]|Rest]) ),
   call( rule(Category, [ lexicon([cat:Cat, wform:[Token]|Rest]) ]) ).

token(simple, SNum, V1, V2, Category, [Token]) :-
   call( rule(Category, [Token]) ).


% ----------------------------------------------------------------
% foreach - for each X do Y
% ----------------------------------------------------------------

foreach(token(SNum, V0, V1, Category, Token), Y) :-
   (
      (
         call(token(simple, SNum, V0, V1, Category, Token)),
         do(Y)
      ;
         call(token(compound, SNum, V0, V1, Category, Token))
      ),
      fail
   ;
      true
   ).
   
foreach(rule(LHS, RHS), Y) :-
   rule(LHS, RHS),
   do(Y), 
   fail.

foreach(edge(SNum, V0, V1, Category, Categories), Y) :-
   edge(SNum, V0, V1, Category, Categories),
   do(Y), 
   fail.
  
foreach(X, Y) :-
   %  nl, nl, write('Foreach: '), write(X), nl, nl,
   true.
  
do(Y) :- Y, !.


% ----------------------------------------------------------------
% dbgwrite/1 - a switchable tracing predicate
% ----------------------------------------------------------------

dbgwrite(Term) :-
   dbgon,
   write(Term),
   nl, !.

dbgwrite(Term).

dbgwrite(Term, Var) :-
   dbgon,
   integer(Var),
   tab(3 * (Var - 1)),
   write(Term),
   nl, !.

dbgwrite(Term, Var) :-
   dbgon,
   write(Term), write(" "), write(Var),
   nl, !.

dbgwrite(Term, Var).

dbgon.  % retract this to switch dbg tracing off





