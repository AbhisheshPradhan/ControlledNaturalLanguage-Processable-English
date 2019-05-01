% ==================================================================
% Project: PENG ASP				%
% Module:  lexicon_content_words_2.pl
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

lexicon([cat:adj, wform:[blue], arg:X, lit:prop(X, blue)]).

lexicon([cat:adj, wform:[green], arg:X, lit:prop(X, green)]).

lexicon([cat:adj, wform:[red], arg:X, lit:prop(X, red)]).


% -------------------------------------------------------------------
% Relational adjective
% -------------------------------------------------------------------

lexicon([cat:radj, wform:[assigned, to], arg:X, arg:Y, lit:prop(X, Y, assigned_to)]).

lexicon([cat:radj, wform:[connected, to], arg:X, arg:Y, lit:prop(X, Y, connected_to)]).


% -------------------------------------------------------------------
% Noun
% -------------------------------------------------------------------

lexicon([cat:noun, wform:[colour], num:sg, arg:X, lit:class(X, colour)]).

lexicon([cat:noun, wform:[colours], num:pl, arg:X, lit:class(X, colour)]).

lexicon([cat:noun, wform:[node], num:sg, arg:X, lit:class(X, node)]).

lexicon([cat:noun, wform:[nodes], num:pl, arg:X, lit:class(X, node)]).


% -------------------------------------------------------------------
% Proper name
% -------------------------------------------------------------------

lexicon([cat:pname, wform:['Blue'], num:sg, arg:X, lit:named(X, blue)]).

lexicon([cat:pname, wform:['Green'], num:sg, arg:X, lit:named(X, green)]).

lexicon([cat:pname, wform:['Red'], num:sg, arg:X, lit:named(X, red)]).
