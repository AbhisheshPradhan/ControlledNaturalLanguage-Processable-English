% ==================================================================
% Project: PENG ASP				%
% Module:  lexicon_content_words_3.pl
% Author:  Rolf Schwitter
% Date:    2019-01-03
% ==================================================================

% ------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).

:- multifile lexicon/1.


% ===================================================================
% Content words
% ===================================================================

% -------------------------------------------------------------------
% Adjective
% -------------------------------------------------------------------

lexicon([cat:adj, wform:[high], arg:X, lit:prop(X, high)]).

lexicon([cat:adj, wform:[low], arg:X, lit:prop(X, low)]).

lexicon([cat:adj, wform:[medium], arg:X, lit:prop(X, medium)]).

lexicon([cat:adj, wform:[noisy], arg:X, lit:prop(X, noisy)]).


% -------------------------------------------------------------------
% Relational adjective
% -------------------------------------------------------------------

lexicon([cat:radj, wform:[located, on], arg:X, arg:Y, lit:prop(X, Y, located_on)]).


% -------------------------------------------------------------------
% Noun
% -------------------------------------------------------------------

lexicon([cat:noun, wform:[accommodation], num:sg, arg:X, lit:class(X, accommodation)]).

lexicon([cat:noun, wform:[accommodations], num:pl, arg:X, lit:class(X, accommodation)]).

lexicon([cat:noun, wform:[dollar], num:sg, arg:X, lit:class(X, dollar)]).

lexicon([cat:noun, wform:[dollars], num:pl, arg:X, lit:class(X, dollar)]).

lexicon([cat:noun, wform:[guesthouse], num:sg, arg:X, lit:class(X, guesthouse)]).

lexicon([cat:noun, wform:[guesthouses], num:pl, arg:X, lit:class(X, guesthouse)]).

lexicon([cat:noun, wform:[hotel], num:sg, arg:X, lit:class(X, hotel)]).

lexicon([cat:noun, wform:[hotels], num:pl, arg:X, lit:class(X, hotel)]).

lexicon([cat:noun, wform:[main, street], num:sg, arg:X, lit:class(X, main_street)]).

lexicon([cat:noun, wform:[motel], num:sg, arg:X, lit:class(X, motel)]).

lexicon([cat:noun, wform:[motels], num:pl, arg:X, lit:class(X, motel)]).

lexicon([cat:noun, wform:[priority], num:sg, arg:X, lit:class(X, priority)]).

lexicon([cat:noun, wform:[star], num:sg, arg:X, lit:class(X, star)]).

lexicon([cat:noun, wform:[stars], num:pl, arg:X, lit:class(X, star)]).


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

lexicon([cat:pname, wform:['Grace'], num:sg, arg:X, lit:named(X, grace)]).

lexicon([cat:pname, wform:['Metro'], num:sg, arg:X, lit:named(X, metro)]).

lexicon([cat:pname, wform:['Posh'], num:sg, arg:X, lit:named(X, posh)]).


% -------------------------------------------------------------------
% Intransitive verb
% -------------------------------------------------------------------


% -------------------------------------------------------------------
% Transitive verb
% -------------------------------------------------------------------

lexicon([cat:tv, wform:[costs], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, cost)]).

lexicon([cat:tv, wform:[cost], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, cost)]).

lexicon([cat:tv, wform:[has], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, poss)]).

lexicon([cat:tv, wform:[have], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, poss)]).

lexicon([cat:tv, wform:[owns], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, own)]).

lexicon([cat:tv, wform:[own], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, own)]).

