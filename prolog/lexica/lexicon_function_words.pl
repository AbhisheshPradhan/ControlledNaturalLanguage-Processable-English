% ==================================================================
% Project: PENG ASP				
% Module:  lexicon_function_words.pl
% Author:  Rolf Schwitter
% Date:    2019-10-30
% ==================================================================

% ------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).

:- multifile lexicon/1.


% ===================================================================
% Function words
% ===================================================================

lexicon([cat:nadj, wform:[first], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(1), ordinal)]).

lexicon([cat:nadj, wform:[second], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(2), ordinal)]).

lexicon([cat:nadj, wform:[third], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(3), ordinal)]).

lexicon([cat:nadj, wform:[fourth], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(4), ordinal)]).

lexicon([cat:nadj, wform:[fifth], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(5), ordinal)]).

lexicon([cat:nadj, wform:[sixth], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(6), ordinal)]).

lexicon([cat:nadj, wform:[seventh], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(7), ordinal)]).

lexicon([cat:nadj, wform:[eight], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(8), ordinal)]).

lexicon([cat:nadj, wform:[nineth], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(9), ordinal)]).

lexicon([cat:nadj, wform:[tenth], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(10), ordinal)]).

lexicon([cat:nadj, wform:[eleventh], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(11), ordinal)]).

lexicon([cat:nadj, wform:[twelfth], num:sg, type:ordinal, arg:X, lit:data_prop(X, pos_int(12), ordinal)]).


lexicon([cat:nadj, wform:[one], num:sg, type:cardinal, arg:X, lit:data_prop(X, pos_int(1), cardinal)]).

lexicon([cat:nadj, wform:[two], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(2), cardinal)]).

lexicon([cat:nadj, wform:[three], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(3), cardinal)]).

lexicon([cat:nadj, wform:[four], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(4), cardinal)]).

lexicon([cat:nadj, wform:[five], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(5), cardinal)]).

lexicon([cat:nadj, wform:[six], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(6), cardinal)]).

lexicon([cat:nadj, wform:[seven], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(7), cardinal)]).

lexicon([cat:nadj, wform:[eight], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(8), cardinal)]).

lexicon([cat:nadj, wform:[nine], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(9), cardinal)]).

lexicon([cat:nadj, wform:[ten], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(10), cardinal)]).

lexicon([cat:nadj, wform:[eleven], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(11), cardinal)]).

lexicon([cat:nadj, wform:[twelve], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(12), cardinal)]).



lexicon([cat:nadj, wform:[Num], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(Num), cardinal)]) :-
   number(Num),
   Num > 1.


lexicon([cat:nadj, wform:[Num], num:_, type:nom, arg:X, lit:data_prop(X, pos_int(Num), nominal)]) :-
   number(Num),
   Num > 0.


% -------------------------------------------------------------------
% Number
% -------------------------------------------------------------------

lexicon([cat:number, wform:[1], arg:X, lit:data_prop(X, pos_int(1), cardinal)]).

lexicon([cat:number, wform:[2], arg:X, lit:data_prop(X, pos_int(2), cardinal)]).

lexicon([cat:number, wform:[3], arg:X, lit:data_prop(X, pos_int(3), cardinal)]).

lexicon([cat:number, wform:[4], arg:X, lit:data_prop(X, pos_int(4), cardinal)]).

lexicon([cat:number, wform:[5], arg:X, lit:data_prop(X, pos_int(5), cardinal)]).

lexicon([cat:number, wform:[6], arg:X, lit:data_prop(X, pos_int(6), cardinal)]).

% lexicon([cat:number, wform:[Num], arg:X, lit:data_prop(X, pos_int(Num), cardinal)]) :- 
%   number(Num).


% -------------------------------------------------------------------
% Adverb
% -------------------------------------------------------------------

lexicon([cat:adv, wform:[then]]).

lexicon([cat:adv, wform:[either]]).


% -------------------------------------------------------------------
% Arithmetic operator
% -------------------------------------------------------------------

lexicon([cat:aop, wform:['+'], arg:X1, arg:X2, lit:'+'(X1, X2)]).

lexicon([cat:aop, wform:['-'], arg:X1, arg:X2, lit:'-'(X1, X2)]).

lexicon([cat:aop, wform:['/'], arg:X1, arg:X2, lit:'/'(X1, X2)]).

lexicon([cat:aop, wform:['*'], arg:X1, arg:X2, lit:'*'(X1, X2)]).


% -------------------------------------------------------------------
% Auxiliary + negation
% -------------------------------------------------------------------

lexicon([cat:aux_neg, wform:[does, not], num:sg, vform:fin]).

lexicon([cat:aux, wform:[does], num:sg, vform:fin]).

lexicon([cat:aux, wform:['Does'], num:sg, vform:fin]).


% -------------------------------------------------------------------
% Copula
% -------------------------------------------------------------------

lexicon([cat:cop, wform:[is], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, isa)]).

lexicon([cat:cop, wform:[is], num:sg, vform:fin]).

lexicon([cat:cop, wform:[are], num:pl, vform:fin]).

lexicon([cat:cop_neg, wform:[is, not], num:sg, vform:fin]).


% -------------------------------------------------------------------
% Strong constraint
% -------------------------------------------------------------------

lexicon([cat:cst, wform:['It', is, not, the , case, that]]).

lexicon([cat:cst, wform:['It', is, false, that]]).


% -------------------------------------------------------------------
% Weak constraint
% -------------------------------------------------------------------

lexicon([cat:wcst, wform:['Minimizing']]).

lexicon([cat:wcst, wform:['Maximizing']]).


% -------------------------------------------------------------------
% Coordination
% -------------------------------------------------------------------

lexicon([cat:crd, wform:[','], op:na]).

lexicon([cat:crd, wform:[and], op:na]).

lexicon([cat:crd, wform:[or], op:';']).


% -------------------------------------------------------------------
% Determiner
% -------------------------------------------------------------------

lexicon([cat:det, wform:[a], num:sg, def:'-']).

lexicon([cat:det, wform:[an], num:sg, def:'-']).

lexicon([cat:det, wform:['A'], num:sg, def:'-']).

lexicon([cat:det, wform:['An'], num:sg, def:'-']).

lexicon([cat:det, wform:[the], num:_, def:'+']).

lexicon([cat:det, wform:['The'], num:_, def:'+']).


% -------------------------------------------------------------------
% Quantifier
% -------------------------------------------------------------------

lexicon([cat:qnt, wform:['Every'], num:sg]).

% lexicon([cat:qnt, wform:['Each'], num:sg]).


% -------------------------------------------------------------------
% Cardinality quantifier
% -------------------------------------------------------------------

lexicon([cat:cqnt, wform:[exactly, Ord], mode:proc, num:sg, lit:Lit, cond:Cond, cl:[Num, '}', [Cond, ':', Lit], '{', Num]]) :-
   member([Ord, Num], [[one, 1], [two, 2],
		       [three, 3], [four, 4],
		       [five, 5], [six, 6],
		       [seven, 7], [eight, 8],
		       [nine, 9], [ten, 10],
		       [eleven, 11], [twelve, 12]]).

lexicon([cat:cqnt, wform:[exactly, Num], mode:proc, num:sg, lit:Lit, cond:Cond, cl:[Num, '}', [Cond, ':', Lit], '{', Num]]) :-
   number(Num).

lexicon([cat:cqnt, wform:[at, most, Ord], mode:proc, num:sg, lit:Lit, cond:Cond, cl:[Num, '}', [Cond, ':', Lit], '{']]) :-
   member([Ord, Num], [[one, 1], [two, 2],
		       [three, 3], [four, 4],
		       [five, 5], [six, 6],
		       [seven, 7], [eight, 8],
		       [nine, 9], [ten, 10],
		       [eleven, 11], [twelve, 12]]).

lexicon([cat:cqnt, wform:[at, most, Num], mode:proc, num:sg, lit:Lit, cond:Cond, cl:[Num, '}', [Cond, ':', Lit], '{']]) :-
   number(Num).

lexicon([cat:cqnt, wform:[at, least, Ord], mode:proc, num:sg, lit:Lit, cond:Cond, cl:['}', [Cond, ':', Lit], '{', Num]]) :-
   member([Ord, Num], [[one, 1], [two, 2],
		       [three, 3], [four, 4],
		       [five, 5], [six, 6],
		       [seven, 7], [eight, 8],
		       [nine, 9], [ten, 10],
		       [eleven, 11], [twelve, 12]]).


% -------------------------------------------------------------------

lexicon([cat:cqnt, wform:[exactly, Ord], mode:gen, num:sg, lit:Lit, cond:Cond, cl:[Num, '{', [Lit, ':', Cond], '}', Num]]) :-
   member([Ord, Num], [[one, 1], [two, 2],
		       [three, 3], [four, 4],
		       [five, 5], [six, 6],
		       [seven, 7], [eight, 8],
		       [nine, 9], [ten, 10],
		       [eleven, 11], [twelve, 12]]).

lexicon([cat:cqnt, wform:[exactly, Num], mode:gen, num:sg, lit:Lit, cond:Cond, cl:[Num, '{', [Lit, ':', Cond], '}', Num]]) :-
   number(Num).

lexicon([cat:cqnt, wform:[at, most, Ord], mode:gen, num:sg, lit:Lit, cond:Cond, cl:['{', [Lit, ':', Cond], '}', Num]]) :-
   member([Ord, Num], [[one, 1], [two, 2],
		       [three, 3], [four, 4],
		       [five, 5], [six, 6],
		       [seven, 7], [eight, 8],
		       [nine, 9], [ten, 10],
		       [eleven, 11], [twelve, 12]]).

lexicon([cat:cqnt, wform:[at, most, Num], mode:gen, num:sg, lit:Lit, cond:Cond, cl:['{', [Lit, ':', Cond], '}', Num]]) :-
   number(Num).

lexicon([cat:cqnt, wform:[at, least, Ord], mode:gen, num:sg, lit:Lit, cond:Cond, cl:[Num, '{', [Lit, ':', Cond], '}']]) :-
   member([Ord, Num], [[one, 1], [two, 2],
		       [three, 3], [four, 4],
		       [five, 5], [six, 6],
		       [seven, 7], [eight, 8],
		       [nine, 9], [ten, 10],
		       [eleven, 11], [twelve, 12]]).

lexicon([cat:cqnt, wform:[at, least, Num], mode:gen, num:sg, lit:Lit, cond:Cond, cl:[Num, '{', [Lit, ':', Cond], '}']]) :-
   number(Num).


% -------------------------------------------------------------------
% Number
% -------------------------------------------------------------------

lexicon([cat:number, wform:[Num], arg:X, lit:data_prop(X, pos_int(Num), cardinal)]) :- 
   number(Num).


% -------------------------------------------------------------------
% Preposition
% -------------------------------------------------------------------

lexicon([cat:prep, wform:['If']]).

lexicon([cat:prep, wform:[of]]).


% -------------------------------------------------------------------
% Imperative
% -------------------------------------------------------------------

lexicon([cat:tv, wform:['Choose'], vform:bse]).


% -------------------------------------------------------------------
% Relative pronoun
% -------------------------------------------------------------------

lexicon([cat:rpn, wform:[that]]).

lexicon([cat:rpn, wform:[who]]).


% -------------------------------------------------------------------
% String variable
% -------------------------------------------------------------------

lexicon([cat:var, wform:['X'], arg:X, lit:variable(X, 'X')]).

lexicon([cat:var, wform:['Y'], arg:X, lit:variable(X, 'Y')]).

lexicon([cat:var, wform:['Z'], arg:X, lit:variable(X, 'Z')]).


% -------------------------------------------------------------------
% Numeric variable
% -------------------------------------------------------------------

lexicon([cat:nvar, wform:['N'], num:Num, type:cardinal, arg:X, lit:[data_prop(X, pos_int(I), cardinal), variable(X, 'N')]]).

lexicon([cat:nvar, wform:['M'], num:Num, type:cardinal, arg:X, lit:[data_prop(X, pos_int(I), cardinal), variable(X, 'M')]]).


% -------------------------------------------------------------------
% Expletive construction: There exists/is
% -------------------------------------------------------------------

lexicon([cat:expl, wform:['There', exists], num:sg]).

lexicon([cat:expl, wform:['There', is], num:sg]).


% -------------------------------------------------------------------
% Query word
% -------------------------------------------------------------------

lexicon([cat:wh, wform:['Who']]).

lexicon([cat:wh, wform:['Where']]).


% -------------------------------------------------------------------
% Full stop
% -------------------------------------------------------------------

lexicon([cat:fs, wform:['.']]).


% -------------------------------------------------------------------
% Question mark
% -------------------------------------------------------------------

lexicon([cat:qm, wform:['?']]).


% -------------------------------------------------------------------
% Full Verb phrase: (experimental)
% -------------------------------------------------------------------

lexicon([cat:vp, wform:[has, the, high, priority, 'H'], var:'H', lit:high]).

lexicon([cat:vp, wform:[has, the, medium, priority, 'M'], var:'M', lit:medium]).

lexicon([cat:vp, wform:[has, the, low, priority, 'L'], var:'L', lit:low]).



