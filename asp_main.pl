% =====================================================================
% Project:   PENG ASP
% Module:    asp_main.pl
% Author:    Rolf Schwitter
% Created:   2019-01-09
% Copyright: Macquarie University, 2019
% =====================================================================

:- style_check([-discontiguous, -singleton]).


% -----------------------------------------------------------------------
% Load Modules
% -----------------------------------------------------------------------

:- use_module(prolog/tokeniser, [tokeniser/2, tokeniser/3]).
:- use_module(prolog/writer, [writer/2, writer/3, write_clauses/2]).
:- use_module(prolog/reader, [reader/3]).
:- use_module(prolog/planner, [planner/2]).
:- use_module(prolog/morphology, [morphology/2]).


% -----------------------------------------------------------------------
% Consult Files
% -----------------------------------------------------------------------

:- consult('prolog/term_expansion.pl').
:- consult('prolog/chart_parser.pl').
:- consult('prolog/grammar.pl').
:- consult('prolog/lexicon_handler.pl').
:- consult('prolog/json_interface.pl').


% -----------------------------------------------------------------------
% Dynamic Predicates
% -----------------------------------------------------------------------

:- dynamic lexicon/1.
   

% -------------------------------------------------------------------
% ASP Solver and ASP File
% -------------------------------------------------------------------

asp_solver('clingo/clingo').
asp_file('asp.lp').


% -----------------------------------------------------------------------
% main_process/2
%
% This is the main process that is called by the Prolog server.
% -----------------------------------------------------------------------

main_process(JSTNIn, JSON) :-
  % incoming_data(JSTNIn),
  json_interface(JSTNIn, JSTNOut),
  atom_json_term(JSON, JSTNOut, [as(atom)]).
  % outgoing_data(JSON).


%---------------------------------------------------------------
% incoming_data/1
%---------------------------------------------------------------

incoming_data(JSTN) :-
  open('data_in.txt', append, Stream),
  writeq(Stream, JSTN),
  nl(Stream),
  nl(Stream),close(Stream).


%---------------------------------------------------------------
% outgoing_data/1
%---------------------------------------------------------------

outgoing_data(JSON) :-
  open('data_out.txt', append, Stream),
  writeq(Stream, JSON),
  nl(Stream),
  nl(Stream),
  close(Stream). 


% -------------------------------------------------------------------
% Load parser
% The chart parser is loaded by default for processing,
% and the definite clause grammar is used for generating.
% -------------------------------------------------------------------

load_parser(chart) :-
   unload_file('prolog/grammar.pl'),
   consult('prolog/term_expansion.pl'),
   consult('prolog/grammar.pl').

load_parser(dcg) :-
   unload_file('prolog/term_expansion'),  
   consult('prolog/grammar.pl').


% -------------------------------------------------------------------
% process/2
% This predicate is used for testing the grammar, for example:
% ?- process(chart, 1).
% ?- process(dcg, 1).
% -------------------------------------------------------------------

process(ParsingStrategy, Number) :-
   load_parser(ParsingStrategy),
   cnl_file(Number, FileName1),
   asp_file(FileName2),
   tokeniser(FileName1, Sentences), !,
   SNum = 0,
   profile(process_sentences(ParsingStrategy, SNum, Sentences, [[]], [Clauses], [[]], [Ante])),
   reverse(Ante, RAnte),
   copy_term(RAnte, RAnteCopy),
   numbervars(RAnteCopy),
   nl,
   write('% ----------------------------'),
   nl,
   write('% Accessible Antecedents '),
   nl,
   write('% ----------------------------'),
   nl, nl,
   write_antecedents(RAnteCopy),
   nl, nl,
   write('% ----------------------------'),
   nl, 
   write('% Generated Answer Set Program '),
   nl,
   write('% ----------------------------'),
   nl, nl,
   writer(FileName2, [Clauses], ASPProgram),
   write_clauses(user, ASPProgram).

  
% -------------------------------------------------------------------
% execute/0
% execute/1
% execute/3
% -------------------------------------------------------------------

execute :-
   asp_file(FileName),
   asp_solver(Solver),
   process_create(Solver, [0, FileName], [stdout(pipe(Stream))] ),
   read_stream_to_codes(Stream, Codes),
   nl,
   print_to_terminal(Codes, Chars),
   extract_answer_sets(Chars, AnswerSets).

execute(AnswerSets) :-
   asp_file(FileName),
   asp_solver(Solver),
   process_create(Solver, [0, FileName], [stdout(pipe(Stream))] ),
   read_stream_to_codes(Stream, Codes),
   nl,
   print_to_terminal(Codes, Chars),
   extract_answer_sets(Chars, AnswerSets). 

execute(FileName, AnswerSetProgram, AnswerSets)   :-
   asp_solver(Solver),
   process_create(Solver, [0, FileName], [stdout(pipe(Stream))] ),
   read_stream_to_codes(Stream, Codes),
   atom_codes(AnswerSetProgram, Codes),
   codes_to_chars(Codes, Chars),
   extract_answer_sets(Chars, AnswerSetsChars),
   atom_chars(AnswerSets, AnswerSetsChars).	
   

% -------------------------------------------------------------------
% print_to_terminal/2
% -------------------------------------------------------------------

print_to_terminal([], []).

print_to_terminal([Code|Codes], [Char|Chars]) :-
   char_code(Char, Code),
   write(Char),
   print_to_terminal(Codes, Chars).


% -------------------------------------------------------------------
% codes_to_chars/2
% -------------------------------------------------------------------

codes_to_chars([], []).

codes_to_chars([Code|Codes], [Char|Chars]) :-
   char_code(Char, Code),
   codes_to_chars(Codes, Chars).


% -------------------------------------------------------------------
% extract_answer_sets/2
% -------------------------------------------------------------------

extract_answer_sets(Chars1, AnswerSets) :-
  append(_Prefix, ['A',n,s,w,e,r,:,' ','1'|Rest1], Chars1),
  (
    append(AnswerSets, ['S','A','T','I','S','F','I','A','B','L','E'|Rest2], ['A',n,s,w,e,r,:,' ','1'|Rest1])
  ;
    append(AnswerSets, ['O',P,T,I,M,U,M,' ','F','O','U','N','D' |Rest2], ['A',n,s,w,e,r,:,' ','1'|Rest1])
  ), nl.


% -------------------------------------------------------------------
% generate/0
% -------------------------------------------------------------------

generate :-
  load_parser(dcg),
  asp_file(FileName),
  reader(FileName, _AnswerSetProgram, Clauses1), !, 
  planner(Clauses1, Clauses2),
  build_list_of_lists(Clauses2, Clauses3),
  profile(generate_sentences(Clauses3, [[]], [Ante], Sentences)),
  reverse(Ante, RAnte),
  numbervars(RAnte),
  nl,
  write('% ----------------------------'),
  nl,
  write('% Accessible Antecedents      '), 
  nl,
  write('% ----------------------------'),
  nl, nl,
  write_antecedents(RAnte),
  nl, nl,
  write('% ----------------------------'),
  nl, 
  write('% Generated Sentences         '),
  nl,
  write('% ----------------------------'),
  nl, nl,
  write_generated_sentences(Sentences).


% -------------------------------------------------------------------
% round_trip/1
% -------------------------------------------------------------------

round_trip(ParsingStrategy, Number) :-
   nl, write('% ----------------------- PROCESS 1 ------------------'), nl, nl,
   load_parser(ParsingStrategy),
   cnl_file(Number, FileName1),
   asp_file(FileName2),
   tokeniser(FileName1, Sentences1), !,
   process_sentences(ParsingStrategy, 0, Sentences1, [[]], [Clauses1], [[]], [Ante1]),
   writer(FileName2, [Clauses1], _),
   nl, write('% ----------------------- EXECUTE 1 ------------------'), nl, nl,
   execute(AnswerSets1),
   nl, write('% ----------------------- GENERATE -------------------'), nl, nl,
   load_parser(dcg),
   reader(FileName2, AnswerSetProgram1, Clauses2),
   planner(Clauses2, Clauses3),
   build_list_of_lists(Clauses3, Clauses4),
   unload_file('prolog/chart_parser.pl'),   % chart parser
   consult('prolog/grammar.pl'),            % chart parser
   generate_sentences(Clauses4, [[]], [Ante2], Sentences2),
   write_generated_sentences(Sentences2),
   nl, write('% ---------------------- PROCESS 2 -------------------'), nl, nl,
   load_parser(ParsingStrategy),
   process_sentences(ParsingStrategy, 0, Sentences2, [[]], [Clauses5], [[]], [Ante3]),
   writer(FileName2, [Clauses5], _),
   nl, write('% ---------------------- EXECUTE 2 -------------------'), nl, nl,
   execute(AnswerSets2),
   nl, write('% ----------------------------------------------------'), nl,
   reader(FileName2, AnswerSetProgram2, _),
   AnswerSetProgram1 == AnswerSetProgram2,
   AnswerSets1 == AnswerSets2,
   write('% Same Answer Set(s) found: round tripping successful.'),
   nl, write('% ----------------------------------------------------'), nl, nl.


% -------------------------------------------------------------------
% build_list_of_lists
% -------------------------------------------------------------------

build_list_of_lists([], []).

build_list_of_lists(ClauseList, [List2|ListofLists]) :-
   append(List, ['.'|Rest], ClauseList),
   append(List, ['.'], List2),
   build_list_of_lists(Rest, ListofLists).


% -------------------------------------------------------------------
% process_sentences/6
% -------------------------------------------------------------------

process_sentences(ParsingStrategy, SNum, [], Clauses, Clauses, Ante, Ante).

process_sentences(ParsingStrategy, SNum1, [Sentence|Sentences], Clauses1, Clauses3, Ante1, Ante3) :-
   write(user, 'Input Sentence:     '),
   writeq(user, Sentence),
   %% write_sentence(Sentence),
   (
      ParsingStrategy = chart,
      SNum2 is SNum1 + 1
      ->
      call_chart_parser(SNum2, s([mode:proc, cl:Clauses1-Clauses2, ante:Ante1-Ante2, tree:Tree]), Sentence)
   ;
      ParsingStrategy = dcg
      ->
      s([mode:proc, cl:Clauses1-Clauses2, ante:Ante1-Ante2, tree:Tree], Sentence, [])
   ),
   nl,
   write(user, 'Syntax Tree:        '),
   write(user, Tree), nl, nl,
   copy_term(Clauses2, CopyClauses2),
   numbervars(CopyClauses2),
   write(user, 'Clause as List:     '),
   writeq(user, CopyClauses2), nl, nl,
   process_sentences(ParsingStrategy, SNum2, Sentences, Clauses2, Clauses3, Ante2, Ante3).


% -------------------------------------------------------------------
% generate_sentences/4
% -------------------------------------------------------------------

/*
generate_sentences([[]], Ante, Ante, []).

generate_sentences(Clauses1, Ante1, Ante3, [Sentence|Sentences]) :-
   write('Clauses as List:    '),
   writeq(Clauses1), nl,
   s(mode:gen, cl:Clauses1-Clauses2, ante:Ante1-Ante2, tree:Tree, Sentence, []),
   nl, 
   write('Syntax Tree:        '),
   write(Tree), nl, nl,
   write('Generated Sentence: '),
   writeq(Sentence),
   nl, nl,
   generate_sentences(Clauses2, Ante2, Ante3, Sentences).
*/


generate_sentences([], Ante, Ante, []).

generate_sentences([Clause|Clauses], Ante1, Ante3, [Sentence2|Sentences]) :-
   s([mode:gen, cl:[Clause]-[[]], ante:Ante1-Ante2, tree:Tree], Sentence1, []),
   morphology(Sentence1, Sentence2),
   nl, 
   write('Syntax Tree:        '),
   write(Tree),
   nl, nl,
   write('Generated Sentence: '),
   write_sentence(Sentence2),
   nl, nl,
   generate_sentences(Clauses, Ante2, Ante3, Sentences).


% -------------------------------------------------------------------
% write_generated_sentences/1
% -------------------------------------------------------------------

write_generated_sentences(Sentences) :-
   %% nl, nl,
   %% write('Generated Sentences:'),
   %% nl, nl,
   write_default_order(Sentences).
   
write_default_order([]).

write_default_order([Sentence|Sentences]) :-
   write_sentence(Sentence),
   %% writeq(Sentence),
   nl, nl,
   write_default_order(Sentences).


% -------------------------------------------------------------------
% write_sentence/1
% -------------------------------------------------------------------

write_sentence([]).

write_sentence([','|Tokens]) :-
   write(','),
   write(' '),
   write_sentence(Tokens).

write_sentence(['.'|Tokens]) :-
   write('.'),
   write(' '),
   write_sentence(Tokens).

write_sentence(['?'|Tokens]) :-
   write('?'),
   write(' '),
   write_sentence(Tokens).

write_sentence([Token1, Token2|Tokens]) :-
   (
      member(Token2, [',', '.', '?'])
      ->
      write(Token1)
   ;
      write(Token1),
      write(' ')
   ),
   write_sentence([Token2|Tokens]).

% -------------------------------------------------------------------
% write_antecedents/1
% -------------------------------------------------------------------

write_antecedents([]).

write_antecedents([Ante|Antes]) :-
   write(Ante), nl,
   write_antecedents(Antes).
 
 
% -------------------------------------------------------------------
% start_cli/0
%
% starts the command line interface of PENG
% -------------------------------------------------------------------

start_cli :-
   unload_file('prolog/lexicon.pl'),
   consult('prolog/lexicon_2.pl'),
   write('Welcome to the PENG command line interface.'),
   nl,
   write('Type: \\exit to quit the interface'),
   nl,
   load_parser(chart),
   SNum = 0,
   process_cmd_line_sentences(SNum, V0, [[]], [[]]),
   write('Generate? \\yes or \\no'),
   nl, nl,
   read_line_to_codes(user_input, Codes),
   atom_codes(Input, Codes),
   (
      Input = '\\yes'
      ->
      generate 
   ;
      Input = '\\no'
      ->
      true
   ).


% -------------------------------------------------------------------
% process_cmd_line_sentences/4
% -------------------------------------------------------------------

process_cmd_line_sentences(SNum1, V0, C1, A1) :-
    SNum2 is SNum1 + 1,
    init_chart_parser(SNum2, V0, C1, A1, LAHCats),
    tab(3),
    write('Lookahead: '),
    display_lookahead_information(LAHCats),
    process_sentence_token_by_token(SNum2, V0, C2, A2),
    process_next_cmd_line_sentence(SNum2, V0, C2, A2).

process_next_cmd_line_sentence(SNum, V0, C2, A2) :-
    nl,
    write('Next sentence? \\yes or \\no'),
    nl, nl,
    read_line_to_codes(user_input, Codes),
    atom_codes(Input, Codes),
    (
       Input = '\\yes'
       ->
       process_cmd_line_sentences(SNum, V0, C2, A2)
    ;
       Input = '\\no'
       ->
       nl
    ;
       write('Invalid input: '),
       write(Input),
       nl,
       process_next_cmd_line_sentence(SNum, V0, C2, A2)
    ).


% -------------------------------------------------------------------
% init_chart_parser/5
% -------------------------------------------------------------------

init_chart_parser(SNum, V0, C1, A1, LAHCats2) :-
    initialise_chart_parser(SNum, V0, s([mode:M, cl:C1-C2, ante:A1-A2, tree:T])),
    findall(Cat:WF, ( edge(SNum, _V1, V2, LHS, [lexicon([cat:Cat, wform:WF|Rest])|Categories]),
                      (  lexicon([cat:Cat, wform:WF|Rest]) ; true ),
		      WF = [Token|Tokens],
		      nonvar(Token),
		      atom_codes(Token, [Code|Codes]),
		      \+ member(Code, [97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
		       	               111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122]) ),
	    LAHCats1),
    nl,
    sort(LAHCats1, LAHCats2).
  

% -------------------------------------------------------------------
% process_sentence_token_by_token/4
% -------------------------------------------------------------------

process_sentence_token_by_token(SNum, V1, C2, A2) :-
  read_line_to_codes(user_input, Codes1),
  (
     member(Codes1, [[44], [46], [63]])
     ->
     Codes1 = Codes2
  ;
     append(Codes2, [32], Codes1)
  ;
     append(Codes2, [32|Rest], Codes1)
  ;
     append(Codes2, [44|Rest], Codes1)
  ;
     append(Codes2, [46|Rest], Codes1)
  ;
     append(Codes2, [63|Rest], Codes1)
  ; 
     Codes1 = Codes2
  ),
  atom_codes(Token, Codes2),
  (
     Token = '\\exit'
     ->
     tab(3),
     write('Bye.'),
     abort
  ;
     ( Token = '.' ; Token = '?' )
     ->
     chart_parser_cmdline(SNum, V1, V2, [Token]),
     edge(SNum, V0, V2, s([mode:proc, cl:C1-C2, ante:A1-A2, tree:T]), _),
     A2 = [A2E],
     reverse(A2E, RA2),
     copy_term(RA2, RA2Copy),
     numbervars(RA2Copy),
     nl,
     write('% ----------------------------'),
     nl,
     write('% Accessible Antecedents '),
     nl,
     write('% ----------------------------'),
     nl, nl,
     write_antecedents(RA2Copy),
     nl, nl,
     write('% ----------------------------'),
     nl, 
     write('% Generated Answer Set Program '),
     nl,
     write('% ----------------------------'),
     nl, nl,
     writer('asp.lp', C2, ASPProgram),
     write_clauses(user, ASPProgram),
     execute
  ;
     chart_parser_cmdline(SNum, V1, V2, [Token]),
     findall(Cat:WF, ( edge(SNum, _V1, V2, LHS, [lexicon([cat:Cat, wform:WF|Rest])|Categories]),
                       (  lexicon([cat:Cat, wform:WF|Rest]) ; true ),
		       WF = [Token1|Tokens],
		       nonvar(Token1),
		       \+ member(Token1, ['The', 'A', 'An'])
		     ),
	     LAHCats1),
     (
        LAHCats1 = []
        ->
        tab(3),
        write('Invalid input: '),
	write(Token),
	write(' -- try again: '),
	nl,
        retractall(edge(SNum, V1, V2, _, _)),
        process_sentence_token_by_token(SNum, V1, C2, A2)
     ;
        tab(3),
        write('Processed: '),
        writeq(Token),
        nl,
        sort(LAHCats1, LAHCats2),
        tab(3),
        write('Lookahead: '),
        display_lookahead_information(LAHCats2),
        process_sentence_token_by_token(SNum, V2, C2, A2)
      )
  ).


% -------------------------------------------------------------------
% display_lookahead_information/1
% -------------------------------------------------------------------

display_lookahead_information([]).

display_lookahead_information([LAH|LAHs]) :-
   write(LAH), nl,
   display_lookahead_information_next(LAHs).

display_lookahead_information_next([]).

display_lookahead_information_next([LAH|LAHs]) :-
   tab(14),
   write(LAH), nl,
   display_lookahead_information_next(LAHs).


% -------------------------------------------------------------------
% process_sentence_2/7 -- without formatting clutter
% -------------------------------------------------------------------

process_sentences_2(ParsingStrategy, SNum, [], Clauses, Clauses, Ante, Ante).

process_sentences_2(ParsingStrategy, SNum1, [Sentence|Sentences], Clauses1, Clauses3, Ante1, Ante3) :-
   (
      ParsingStrategy = chart,
      SNum2 is SNum1 + 1
      ->
      call_chart_parser_2(SNum2, s([mode:proc, cl:Clauses1-Clauses2, ante:Ante1-Ante2, tree:Tree]), Sentence)
   ;
      ParsingStrategy = dcg
      ->
      s([mode:proc, cl:Clauses1-Clauses2, ante:Ante1-Ante2, tree:Tree], Sentence, [])
   ),
   process_sentences_2(ParsingStrategy, SNum2, Sentences, Clauses2, Clauses3, Ante2, Ante3).


% -------------------------------------------------------------------
% generate/2 -- without clutter
% -------------------------------------------------------------------

generate(SentencesAtomic, [RAnte]) :-
  load_parser(dcg),
  asp_file(FileName),
  reader(FileName, _AnswerSetProgram, Clauses1), !, 
  planner(Clauses1, Clauses2),
  build_list_of_lists(Clauses2, Clauses3),
  generate_sentences_2(Clauses3, [[]], [Ante], Sentences),
  reverse(Ante, RAnte),
  numbervars(RAnte),
  sentence_lists_to_atomic(Sentences, SentencesAtomic).


generate_sentences_2([], Ante, Ante, []).

generate_sentences_2([Clause|Clauses], Ante1, Ante3, [Sentence2|Sentences]) :-
   s([mode:gen, cl:[Clause]-[[]], ante:Ante1-Ante2, tree:Tree], Sentence1, []),
   morphology(Sentence1, Sentence2),
   generate_sentences_2(Clauses, Ante2, Ante3, Sentences).


sentence_lists_to_atomic(Sentences1, SentencesAtomic) :-
   flatten(Sentences1, Sentences2),
   atomic_list_concat(Sentences2, ' ', SentencesAtomic).
