% ==================================================================
% Project: PENG ASP				
% Module:  lexicon_content_words.pl
% Author:  Rolf Schwitter
% Date:    2019-01-04
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

