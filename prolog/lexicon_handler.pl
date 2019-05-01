% =====================================================================
% Project:   PENG ASP
% Module:    lexicon_handling.pl
% Author:    Rolf Schwitter
% Created:   2019-01-04
% Copyright: Macquarie University, 2019
% =====================================================================

:- style_check([-discontiguous, -singleton]).


% -------------------------------------------------------------------
% load_complete_lexicon/0
% -------------------------------------------------------------------

load_complete_lexicon :-
   retractall(lexicon(_)),
   lexicon_function_words(0, Path1),
   consult(Path1),
   lexicon_content_words(0, Path2),
   consult(Path2).


% -------------------------------------------------------------------
% load_specific_lexicon/1
% -------------------------------------------------------------------

load_specific_lexicon(FileNumber) :-
   retractall(lexicon(_)),
   lexicon_function_words(0, Path1),
   consult(Path1),
   lexicon_content_words(FileNumber, Path2),
   consult(Path2).


% -------------------------------------------------------------------
% lexicon_function_words/2
% -------------------------------------------------------------------

lexicon_function_words(0, 'prolog/lexica/lexicon_function_words.pl').


% -------------------------------------------------------------------
% lexicon_content_words/2
% -------------------------------------------------------------------

lexicon_content_words(0,  'prolog/lexica/lexicon_content_words.pl').

lexicon_content_words(1, 'prolog/lexica/lexicon_content_words_1.pl').
lexicon_content_words(2, 'prolog/lexica/lexicon_content_words_2.pl').
lexicon_content_words(3, 'prolog/lexica/lexicon_content_words_3.pl').
lexicon_content_words(4, 'prolog/lexica/lexicon_content_words_4.pl').


% -------------------------------------------------------------------
% CNL file names
% -------------------------------------------------------------------

cnl_file(0, 'texts/test.txt').
cnl_file(1, 'texts/successful_student.txt').
cnl_file(2, 'texts/graph_colouring.txt').
cnl_file(3, 'texts/optimal_accommodation.txt').
cnl_file(4, 'texts/contract.txt').


% -------------------------------------------------------------------
% call load_complete_lexicon at start up.
% -------------------------------------------------------------------

:- load_complete_lexicon.