% ==================================================================
% Project: PENG ASP				%
% Module:  lexicon_content_words_4.pl
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


% -------------------------------------------------------------------
% Relational adjective
% -------------------------------------------------------------------

lexicon([cat:radj, wform:[included, in], arg:X, arg:Y, lit:prop(X, Y, included_in)]).


% -------------------------------------------------------------------
% Noun
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


% -------------------------------------------------------------------
% Proper name
% -------------------------------------------------------------------

lexicon([cat:pname, wform:['Arun', 'Majumdar'], num:sg, arg:X, lit:named(X, arun_majumdar)]).

lexicon([cat:pname, wform:['McGrath'], num:sg, arg:X, lit:named(X, mc_grath)]).

lexicon([cat:pname, wform:['Rolf', 'Schwitter'], num:sg, arg:X, lit:named(X, rolf_schwitter)]).

lexicon([cat:pname, wform:['Tracy', 'Yap', 'Realty'], num:sg, arg:X, lit:named(X, tracy_yap_realty)]).


% -------------------------------------------------------------------
% Tansitive verb
% -------------------------------------------------------------------

lexicon([cat:tv, wform:[appoints], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, appoint)]).

lexicon([cat:tv, wform:[belongs, to], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, belong_to)]).

lexicon([cat:tv, wform:[deals, with], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, deal_with)]).

lexicon([cat:tv, wform:[gives], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, give)]).

lexicon([cat:tv, wform:[receives], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, receive)]).

lexicon([cat:tv, wform:[represents], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, represent)]).

