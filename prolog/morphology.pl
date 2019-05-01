% ====================================================
% Project: PENG ASP				%
% Module:  morphology.pl
% Author:  Rolf Schwitter
% Created: 2019-01-02
% ====================================================	
  
:- module(morphology, [morphology/2]).


% ------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).


% -------------------------------------------------------------------
% morphology/2
% -------------------------------------------------------------------

morphology([a|Sentence1], ['The'|Sentence2]) :-
   inspect_tokens(Sentence1, Sentence2).
   
morphology([the|Sentence], ['The'|Sentence2]) :-
   inspect_tokens(Sentence, Sentence2).

morphology(Sentence1, Sentence2) :-
   inspect_tokens(Sentence1, Sentence2).

inspect_tokens([], []).


inspect_tokens([Token, that|Sentence1], [Token, who|Sentence2]) :-
   atom_chars(Token, Chars),
   reverse(Chars, RChars),
   (
     RChars = ['t', 'n', 'e'|_]
   ;
     RChars = ['t', 's', 'i'|_]
   ;
     RChars = ['r', 'a'|_]
   ;
     RChars = ['r', 'e'|_]
   ;
     RChars = ['r', 'o'|_]
   ),
   inspect_tokens(Sentence1, Sentence2).

   
inspect_tokens([Token, who|Sentence1], [Token, that|Sentence2]) :-
   inspect_tokens(Sentence1, Sentence2).


inspect_tokens([a, Token|Sentence1], [an, Token|Sentence2]) :-
   atom_chars(Token, [Char|_Chars]),
   member(Char, [a, e, i, o, u, 'A', 'E', 'I', 'O', 'U']),
   inspect_tokens(Sentence1, Sentence2).

inspect_tokens([Token|Sentence1], [Token|Sentence2]) :-
   inspect_tokens(Sentence1, Sentence2).