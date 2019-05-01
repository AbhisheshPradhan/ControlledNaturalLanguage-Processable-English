% ====================================================
% Project: PENG ASP				%
% Module:  tokeniser.pl
% Author:  Rolf Schwitter
% Date:    2019-01-02
% ====================================================
  
:- module(tokeniser, [tokeniser/2, tokeniser/3]).

:- style_check([-discontiguous, -singleton]).

% -----------------------------------------------------
% tokeniser/2
% -----------------------------------------------------

tokeniser(FileName, Sentences) :-
  read_file_to_chars(FileName, Chars1),
  remove_comments(Chars1, Chars2),
  parse_chars_to_tokens(Tokens, Chars2, []),
  generate_sentence_list(Tokens, Sentences).


% -----------------------------------------------------
% tokeniser/3
% -----------------------------------------------------

tokeniser(FileName, Sentences, Atom) :-
  read_file_to_chars(FileName, Chars1, Atom),
  remove_comments(Chars1, Chars2),
  parse_chars_to_tokens(Tokens, Chars2, []),
  generate_sentence_list(Tokens, Sentences).


% -----------------------------------------------------
% read_file_to_chars/2
% -----------------------------------------------------

read_file_to_chars(FileName, Chars) :-
   read_file_to_codes(FileName, Codes, [encoding(utf8)]),
   transform_codes_to_chars(Codes, Chars). 


% -----------------------------------------------------
% read_file_to_chars/3
% -----------------------------------------------------

read_file_to_chars(FileName, Chars, Atom) :-
   read_file_to_codes(FileName, Codes, [encoding(utf8)]),
   atom_codes(Atom, Codes),
   transform_codes_to_chars(Codes, Chars).


filter_all_codes([], []).

filter_all_codes([47|Codes1], Codes) :-
   filter_all_codes(Codes1, Codes).

filter_all_codes([Code|Codes1], [Code|Codes2]) :-
   filter_all_codes(Codes1, Codes2).


% -----------------------------------------------------
% transform_codes_to_chars/2
% -----------------------------------------------------

transform_codes_to_chars([], []).

transform_codes_to_chars([Code|Codes], [Char|Chars]) :-
  char_code(Char, Code),
  transform_codes_to_chars(Codes, Chars).


% -----------------------------------------------------
% remove_comments/2
% -----------------------------------------------------

remove_comments([], []).

remove_comments(['%'|Chars1], Chars3) :-         
   append(Comment, ['\n'|Chars2], Chars1),
   remove_comments(Chars2, Chars3).

% Prolog multi-line comments
remove_comments(['/', '*'|Chars1], Chars3) :-
   append(Comment, ['*', '/'|Chars2], Chars1),
   remove_comments(Chars2, Chars3).

% ASP multi-line comments
remove_comments(['%', '*'|Chars1], Chars3) :-
   append(Comment, ['*', '%'|Chars2], Chars1),
   remove_comments(Chars2, Chars3).

remove_comments([Char|Chars1], [Char|Chars2]) :-
   remove_comments(Chars1, Chars2).


% -----------------------------------------------------
% DCG for tokenising
%
% parse_chars_to_tokens/1 
% -----------------------------------------------------

parse_chars_to_tokens([T|Ts]) -->
  blank,
  token(T), !,
  parse_chars_to_tokens(Ts).

parse_chars_to_tokens([]) -->
  blank.


% -----------------------------------------------------
% blank/0
%
% cntrl: Char is an ASCII control character (0..31),
%        ASCII DEL character (127), or non-ASCII
%        character in the range 128..159 or 8232..8233.
% -----------------------------------------------------

blank --> [Char],
  { Char = ' ' ; char_type(Char, cntrl) },
  !, 
  blank.

blank --> [].


% -----------------------------------------------------
% token/1
% -----------------------------------------------------

token(T) -->
  timex(Chars),
  { atom_chars(T, Chars) }.

token(T) -->
  word(Chars),
  { atom_chars(T, Chars) }.

token(T) -->
  numeral(Chars),
  { number_chars(T, Chars) }.

token(T) -->
  [Char],
  { atom_chars(T, [Char]) }.


% -----------------------------------------------------
% timex/1
% -----------------------------------------------------

timex([Char1, Char2, ':', Char3, Char4]) -->
  digit(Char1),
  digit(Char2),
  [':'],
  digit(Char3),
  digit(Char4).


% -----------------------------------------------------
% word/1
% -----------------------------------------------------

word([L|Rest]) -->
  letter(L),
  word_next(Rest).

word([L]) -->
   letter(L).

% Special rule to deal with slashes: e.g., it excludes N/M but accepts n/M 
word_next([L1, L2|Rest]) -->
  letter(L1),
  slash(L2),
  { char_type(L1, lower) }, 
  word_next(Rest).

word_next([L|Rest]) -->
  ( letter(L) ; digit(L) ; hyphen(L) ),
  word_next(Rest).

word_next([L]) -->
  ( letter(L) ; digit(L) ).


% -----------------------------------------------------
% numeral/1
% -----------------------------------------------------

numeral([C|N]) -->
  digit(C),
  numeral(N).

numeral([C]) -->
  digit(C).


% -----------------------------------------------------
% letter/1
% -----------------------------------------------------

letter(Char) -->
  [Char],
  { char_type(Char, alpha) }.


% -----------------------------------------------------
% digit/1
% -----------------------------------------------------

digit(Char) -->
  [Char],
  { char_type(Char, digit) }.


% -----------------------------------------------------
% Special symbols
% -----------------------------------------------------

hyphen('-') --> ['-'].

slash('/') --> ['/'].


% -----------------------------------------------------
%  generate_sentence_list/2
% -----------------------------------------------------

generate_sentence_list([], []).

generate_sentence_list(TokenList1, [Sentence|Sentences]) :-
   append(TokenList3, ['.'|TokenList2], TokenList1),
   \+ member('?', TokenList3),
   append(TokenList3, ['.'], Sentence),
   generate_sentence_list(TokenList2, Sentences).

generate_sentence_list(TokenList1, [Sentence|Sentences]) :-
   append(TokenList3, ['?'|TokenList2], TokenList1),
   append(TokenList3, ['?'], Sentence),
   generate_sentence_list(TokenList2, Sentences).
