% ==================================================================
% Project: PENG ASP				
% Module:  lexicon_content_words_1.pl
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

lexicon([cat:adj, wform:[successful], arg:X, lit:prop(X, successful)]).


% -------------------------------------------------------------------
% Relational adjective
% -------------------------------------------------------------------

lexicon([cat:radj, wform:[enrolled, in], arg:X, arg:Y, lit:prop(X, Y, enrolled_in)]).


% -------------------------------------------------------------------
% Noun
% -------------------------------------------------------------------

lexicon([cat:noun, wform:[student], num:sg, arg:X, lit:class(X, student)]).

lexicon([cat:noun, wform:[students], num:pl, arg:X, lit:class(X, student)]).

lexicon([cat:noun, wform:[location], num:sg, arg:X, lit:class(X, location)]).

lexicon([cat:noun, wform:[locations], num:pl, arg:X, lit:class(X, location)]).


% -------------------------------------------------------------------
% Relational noun
% -------------------------------------------------------------------


% -------------------------------------------------------------------
% Proper name
% -------------------------------------------------------------------

lexicon([cat:pname, wform:['Bob'], num:sg, arg:X, lit:named(X, bob)]).

lexicon([cat:pname, wform:['Information', 'Technology'], num:sg, arg:X, lit:named(X, information_technology)]).

lexicon([cat:pname, wform:['Macquarie', 'University'], num:sg, arg:X, lit:named(X, macquarie_university)]).

lexicon([cat:pname, wform:['Macquarie'], num:sg, arg:X, lit:named(X, macquarie_university)]).

lexicon([cat:pname, wform:['Tom'], num:sg, arg:X, lit:named(X, tom)]).


% -------------------------------------------------------------------
% Intransitive verb
% -------------------------------------------------------------------

lexicon([cat:iv, wform:[parties], num:sg, vform:fin, arg:X, lit:pred(X, party)]).

lexicon([cat:iv, wform:[party], num:_, vform:bse, arg:X, lit:pred(X, party)]).


lexicon([cat:iv, wform:[works], num:sg, vform:fin, arg:X, lit:pred(X, work)]).

lexicon([cat:iv, wform:[work], num:_, vform:bse, arg:X, lit:pred(X, work)]).


% -------------------------------------------------------------------
% Transitive verb
% -------------------------------------------------------------------

lexicon([cat:tv, wform:[studies, at], num:sg, vform:fin, arg:X, arg:Y, lit:pred(X, Y, study_at)]).

lexicon([cat:tv, wform:[study, at], num:_, vform:bse, arg:X, arg:Y, lit:pred(X, Y, study_at)]).


