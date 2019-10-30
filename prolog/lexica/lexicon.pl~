% ==================================================================
% Project: PENG ASP				
% Module:  lexicon.pl
% Author:  Rolf Schwitter
% Date:    2019-04-01
% ==================================================================

% ------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).

% :- multifile lexicon/1.
:- dynamic lexicon/1.


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

%% lexicon([cat:nadj, wform:[1], num:sg, type:cardinal, arg:X, lit:data_prop(X, pos_int(1), cardinal)]).

lexicon([cat:nadj, wform:[Num], num:pl, type:cardinal, arg:X, lit:data_prop(X, pos_int(Num), cardinal)]) :-
   number(Num),
   Num > 1.

lexicon([cat:nadj, wform:[Num], num:_, type:nom, arg:X, lit:data_prop(X, pos_int(Num), nominal)]) :-
   number(Num),
   Num > 0.


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



% ===================================================================
% Content words
% ===================================================================

% -------------------------------------------------------------------
% Adjective
% -------------------------------------------------------------------

lexicon([cat:adj, wform:[blue], arg:X, lit:prop(X, blue)]).

lexicon([cat:adj, wform:[green], arg:X, lit:prop(X, green)]).

lexicon([cat:adj, wform:[high], arg:X, lit:prop(X, high)]).

lexicon([cat:adj, wform:[low], arg:X, lit:prop(X, low)]).

lexicon([cat:adj, wform:[medium], arg:X, lit:prop(X, medium)]).

lexicon([cat:adj, wform:[noisy], arg:X, lit:prop(X, noisy)]).

lexicon([cat:adj, wform:[red], arg:X, lit:prop(X, red)]).

lexicon([cat:adj, wform:[successful], arg:X, lit:prop(X, successful)]).


% -------------------------------------------------------------------
% Relational adjective
% -------------------------------------------------------------------

lexicon([cat:radj, wform:[assigned, to], arg:X, arg:Y, lit:prop(X, Y, assigned_to)]).

lexicon([cat:radj, wform:[connected, to], arg:X, arg:Y, lit:prop(X, Y, connected_to)]).

lexicon([cat:radj, wform:[enrolled, in], arg:X, arg:Y, lit:prop(X, Y, enrolled_in)]).

lexicon([cat:radj, wform:[located, on], arg:X, arg:Y, lit:prop(X, Y, located_on)]).

lexicon([cat:radj, wform:[included, in], arg:X, arg:Y, lit:prop(X, Y, included_in)]).


% -------------------------------------------------------------------
% Noun
% -------------------------------------------------------------------

lexicon([cat:noun, wform:[accommodation], num:sg, arg:X, lit:class(X, accommodation)]).

lexicon([cat:noun, wform:[accommodations], num:pl, arg:X, lit:class(X, accommodation)]).

lexicon([cat:noun, wform:[colour], num:sg, arg:X, lit:class(X, colour)]).

lexicon([cat:noun, wform:[dollar], num:sg, arg:X, lit:class(X, dollar)]).

lexicon([cat:noun, wform:[dollars], num:pl, arg:X, lit:class(X, dollar)]).

lexicon([cat:noun, wform:[guesthouse], num:sg, arg:X, lit:class(X, guesthouse)]).

lexicon([cat:noun, wform:[guesthouses], num:pl, arg:X, lit:class(X, guesthouse)]).

lexicon([cat:noun, wform:[hotel], num:sg, arg:X, lit:class(X, hotel)]).

lexicon([cat:noun, wform:[hotels], num:pl, arg:X, lit:class(X, hotel)]).

lexicon([cat:noun, wform:[main, street], num:sg, arg:X, lit:class(X, main_street)]).

lexicon([cat:noun, wform:[man], num:sg, arg:X, lit:class(X, man)]).

lexicon([cat:noun, wform:[motel], num:sg, arg:X, lit:class(X, motel)]).

lexicon([cat:noun, wform:[motels], num:pl, arg:X, lit:class(X, motel)]).

lexicon([cat:noun, wform:[node], num:sg, arg:X, lit:class(X, node)]).

lexicon([cat:noun, wform:[nodes], num:pl, arg:X, lit:class(X, node)]).

lexicon([cat:noun, wform:[password], num:sg, arg:X, lit:class(X, password)]).

lexicon([cat:noun, wform:[priority], num:sg, arg:X, lit:class(X, priority)]).

lexicon([cat:noun, wform:[star], num:sg, arg:X, lit:class(X, star)]).

lexicon([cat:noun, wform:[stars], num:pl, arg:X, lit:class(X, star)]).

lexicon([cat:noun, wform:[student], num:sg, arg:X, lit:class(X, student)]).

lexicon([cat:noun, wform:[students], num:pl, arg:X, lit:class(X, student)]).



% -------------------------------------------------------------------
% SDLT: Noun
% -------------------------------------------------------------------

lexicon([cat:noun, wform:['Agreement', of, 'Purchase', and, 'Sell'],
	 num:sg, arg:X, lit:class(X, agreement_purchase_sell)]).

lexicon([cat:noun, wform:[agreement], num:sg, arg:X, lit:class(X, agreement)]).

lexicon([cat:noun, wform:[brokerage], num:sg, arg:X, lit:class(X, brokerage)]).

lexicon([cat:noun, wform:[buyer, brokerage], num:sg, arg:X, lit:class(X, buyer_brokerage)]).

lexicon([cat:noun, wform:[buyer], num:sg, arg:X, lit:class(X, buyer)]).

lexicon([cat:noun, wform:[listing, brokerage], num:sg, arg:X, lit:class(X, listing_brokerage)]).

lexicon([cat:noun, wform:[notice], num:sg, arg:X, lit:class(X, notice)]).

lexicon([cat:noun, wform:[written, notice], num:sg, arg:X, lit:class(X, written_notice)]).

lexicon([cat:noun, wform:[seller], num:sg, arg:X, lit:class(X, seller)]).


% -------------------------------------------------------------------
% Relational noun
% -------------------------------------------------------------------

lexicon([cat:rnoun, wform:[cost, per, star], num:sg, arg:X, arg:Y, lit:rel(X, Y, cost_per_star)]).

lexicon([cat:rnoun, wform:[star, rating], num:sg, arg:X, arg:Y, lit:rel(X, Y, star_rating)]).


% -------------------------------------------------------------------
% Proper name
% -------------------------------------------------------------------

lexicon([cat:pname, wform:['Adina'], num:sg, arg:X, lit:named(X, adina)]).

lexicon([cat:pname, wform:['Amora'], num:sg, arg:X, lit:named(X, amora)]).

lexicon([cat:pname, wform:['Bob'], num:sg, arg:X, lit:named(X, bob)]).

lexicon([cat:pname, wform:['Blue'], num:sg, arg:X, lit:named(X, blue)]).

lexicon([cat:pname, wform:['Grace'], num:sg, arg:X, lit:named(X, grace)]).

lexicon([cat:pname, wform:['Green'], num:sg, arg:X, lit:named(X, green)]).

lexicon([cat:pname, wform:['Information', 'Technology'], num:sg, arg:X, lit:named(X, information_technology)]).

lexicon([cat:pname, wform:['John'], num:sg, arg:X, lit:named(X, john)]).

lexicon([cat:pname, wform:['Macquarie', 'University'], num:sg, arg:X, lit:named(X, macquarie_university)]).

lexicon([cat:pname, wform:['Macquarie'], num:sg, arg:X, lit:named(X, macquarie_university)]).

lexicon([cat:pname, wform:['Metro'], num:sg, arg:X, lit:named(X, metro)]).

lexicon([cat:pname, wform:['Oscar'], num:sg, arg:X, lit:named(X, oscar)]).

lexicon([cat:pname, wform:['Posh'], num:sg, arg:X, lit:named(X, posh)]).

lexicon([cat:pname, wform:['Red'], num:sg, arg:X, lit:named(X, red)]).

lexicon([cat:pname, wform:['Sue'], num:sg, arg:X, lit:named(X, sue)]).

lexicon([cat:pname, wform:['Tom'], num:sg, arg:X, lit:named(X, tom)]).


% -------------------------------------------------------------------
% SDLT: Proper name
% -------------------------------------------------------------------

lexicon([cat:pname, wform:['Arun', 'Majumdar'], num:sg, arg:X, lit:named(X, arun_majumdar)]).

lexicon([cat:pname, wform:['McGrath'], num:sg, arg:X, lit:named(X, mc_grath)]).

lexicon([cat:pname, wform:['Rolf', 'Schwitter'], num:sg, arg:X, lit:named(X, rolf_schwitter)]).

lexicon([cat:pname, wform:['Tracy', 'Yap', 'Realty'], num:sg, arg:X, lit:named(X, tracy_yap_realty)]).


% -------------------------------------------------------------------
% Intransitive verb
% -------------------------------------------------------------------

lexicon([cat:iv, wform:[parties], num:sg, vform:fin, arg:X, lit:pred(X, party)]).

lexicon([cat:iv, wform:[party], num:_, vform:bse, arg:X, lit:pred(X, party)]).

lexicon([cat:iv, wform:[sleeps], num:sg, vform:fin, arg:X, lit:pred(X, sleep)]).

lexicon([cat:iv, wform:[sleep], num:_, vform:bse, arg:X, lit:pred(X, sleep)]).

lexicon([cat:iv, wform:[works], num:sg, vform:fin, arg:X, lit:pred(X, work)]).

lexicon([cat:iv, wform:[work], num:_, vform:bse, arg:X, lit:pred(X, work)]).


% -------------------------------------------------------------------
% Transitive verb
% -------------------------------------------------------------------

lexicon([cat:tv, wform:[costs], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, cost)]).

lexicon([cat:tv, wform:[cost], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, cost)]).

lexicon([cat:tv, wform:[has], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, poss)]).

lexicon([cat:tv, wform:[have], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, poss)]).

lexicon([cat:tv, wform:[owns], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, own)]).

lexicon([cat:tv, wform:[own], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, own)]).

lexicon([cat:tv, wform:[studies, at], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, study_at)]).

lexicon([cat:tv, wform:[study, at], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, study_at)]).


% -------------------------------------------------------------------
% SLDT: Verb
% -------------------------------------------------------------------

lexicon([cat:tv, wform:[appoints], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, appoint)]).

lexicon([cat:tv, wform:[belongs, to], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, belong_to)]).

lexicon([cat:tv, wform:[deals, with], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, deal_with)]).

lexicon([cat:tv, wform:[gives], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, give)]).

lexicon([cat:tv, wform:[receives], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, receive)]).

lexicon([cat:tv, wform:[represents], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, represent)]).

