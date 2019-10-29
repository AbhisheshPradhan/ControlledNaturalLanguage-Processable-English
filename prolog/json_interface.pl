% ========================================================================
% Projct: PENG ASP
% Module: json_interface.pl
% Author: Rolf Schwitter
% Date:   2019-09-25
% ========================================================================

  
% ------------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).


% ------------------------------------------------------------------------
% json_interface/2
%
% - adds new content words and resumes parsing
%
% ------------------------------------------------------------------------

json_interface(json([id=Id, inputmode=IMode, editmode=add, token=TokenAtom,
		     featurestructure=FS, filename=FName, spectext=Text,
		     snum=SNumAtom, spos=SPosAtom, reasoner=Flag, reasonermode=RMode]), Output) :-
   atom_json_term(FS, JSONTerm, [as(string)]),
   (
     JSONTerm = json([cat='verb: intransitive', wform=WF, vform=fin, num=sg]),
     lexicon_entry_preprocessor(iv, remove_suffix, WF, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:iv, wform:WFList,   num:sg, vform:fin, arg:X, lit:pred(X, BaseSem)]),
              lexicon([cat:iv, wform:BaseList, num:_,  vform:bse, arg:X, lit:pred(X, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='verb: intransitive', wform=Base, vform=bse, num=Num]),
     lexicon_entry_preprocessor(iv, add_suffix, Base, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:iv, wform:BaseList, num:_,  vform:bse, arg:X, lit:pred(X, BaseSem)]),
               lexicon([cat:iv, wform:WFList,   num:sg, vform:fin, arg:X, lit:pred(X, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='verb: transitive', wform=WF, vform=fin, num=sg]),
     lexicon_entry_preprocessor(tv, remove_suffix, WF, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:tv, wform:WFList,   num:sg, vform:fin, arg:X, arg:Y,  lit:pred(X, Y, BaseSem)]),
              lexicon([cat:tv, wform:BaseList, num:_,  vform:bse, arg:X, arg:Y,  lit:pred(X, Y, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='verb: transitive', wform=Base, vform=bse, num=Num]),
     lexicon_entry_preprocessor(tv, add_suffix, Base, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:tv, wform:BaseList, num:_, vform:bse, arg:X, arg:Y,  lit:pred(X, Y, BaseSem)]),
              lexicon([cat:tv, wform:WFList,   num:sg,  vform:fin, arg:X, arg:Y,  lit:pred(X, Y, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat=name, wform=WF, num=Num]),
     lexicon_entry_preprocessor(pname, WF, WFList, Base),
     % downcase_atom(WF, Base),
     Entry = [lexicon([cat:pname, wform:WFList, num:Num, arg:X, lit:named(X, Base)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='noun: common', wform=WF, num=sg]),
     lexicon_entry_preprocessor(noun, remove_suffix, WF, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:noun, wform:WFList, num:sg, arg:X, lit:class(X, BaseSem)]),
	      lexicon([cat:noun, wform:BaseList, num:pl, arg:X, lit:class(X, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='noun: common', wform=Base, num=pl]),
     lexicon_entry_preprocessor(noun, add_suffix, Base, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:noun, wform:BaseList, num:pl, arg:X, lit:class(X, BaseSem)]),
	      lexicon([cat:noun, wform:WFList,   num:sg, arg:X, lit:class(X, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='noun: relational', wform=WF, num=sg]),
     lexicon_entry_preprocessor(rnoun, remove_suffix, WF, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:rnoun, wform:WFList,   num:sg, arg:X, arg:Y, lit:rel(X, Y, BaseSem)]),
	      lexicon([cat:rnoun, wform:BaseList, num:pl, arg:X, arg:Y, lit:rel(X, Y, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='noun: relational', wform=Base, num=pl]),
     lexicon_entry_preprocessor(rnoun, add_suffix, Base, WFList, BaseList, BaseSem),
     Entry = [lexicon([cat:rnoun, wform:BaseList, num:pl, arg:X, arg:Y, lit:rel(X, Y, BaseSem)]),
	      lexicon([cat:rnoun, wform:WFList,   num:sg, arg:X, arg:Y, lit:rel(X, Y, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ;
     JSONTerm = json([cat='adjective: relational', wform=WF]),
     lexicon_entry_preprocessor(radj, _, WF, WFList, _, BaseSem),
     Entry = [lexicon([cat:radj, wform:WFList, arg:X, arg:Y, lit:prop(X, Y, BaseSem)])],
     add_lexical_entries_to_system(Entry)
   ; 
     JSONTerm = json([cat=adjective, wform=WF]),
     Entry = [lexicon([cat:adj, wform:[WF], arg:X, lit:prop(X, WF)])],
     add_lexical_entries_to_system(Entry)
   ),
   (
     atom_number(TokenAtom, Token)
   ;
     TokenAtom = Token
   ),
   atom_number(SNumAtom, SNum),
   atom_number(SPosAtom, SPos),
   chart_handler(Id, IMode, Token, SNum, SPos, Flag, RMode, Output).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
% intransitive verb: remove_suffix
% ------------------------------------------------------------------------

lexicon_entry_preprocessor(iv, remove_suffix, WF1, WFList, BaseList, BaseSem) :-
   atom_codes(WF1, WFChars),
   once(build_token_list(WFChars, WFList)),
   (
     reverse(WFList, [WF3, WF2|Rest]),
     member(WF3, [back, by, down, on, over, out, through, up]), 
     stemmer(remove_suffix, WF2, Base),
     reverse(BaseList, [WF3, Base|Rest])
   ;
     reverse(WFList, [WF2|Rest]),
     stemmer(remove_suffix, WF2, Base),
     reverse(BaseList, [Base|Rest])
   ), 
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
% intransitive verb: add_suffix
% ------------------------------------------------------------------------

lexicon_entry_preprocessor(iv, add_suffix, Base1, WFList, BaseList, BaseSem) :-
   atom_codes(Base1, BaseChars),
   once(build_token_list(BaseChars, BaseList)),
   (
     reverse(BaseList, [Base3, Base2|Rest]),
     member(Base3, [back, by, down, on, over, out, through, up]), 
     stemmer(add_suffix, Base2, WF),
     reverse(WFList, [Base3, WF|Rest])
   ;
     reverse(BaseList, [Base2|Rest]),
     stemmer(add_suffix, Base2, WF),
     reverse(WFList, [WF|Rest])
   ),
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
% transitive verb: remove_suffix
% ------------------------------------------------------------------------

lexicon_entry_preprocessor(tv, remove_suffix, WF1, WFList, BaseList, BaseSem) :-
   atom_codes(WF1, WFChars),
   once(build_token_list(WFChars, WFList)),
   (
     reverse(WFList, [WF3, WF2|Rest]),
     member(Base3, [about, across, against, along, away, back, by, down, for, forward,
		    from, in, into, of, off, on, out, to, through, up, with, without]), 
     stemmer(remove_suffix, WF2, Base),
     reverse(BaseList, [WF3, Base|Rest])
   ;
     reverse(WFList, [WF2|Rest]),
     stemmer(remove_suffix, WF2, Base),
     reverse(BaseList, [Base|Rest])
   ), 
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
%  transitive verb: add_suffix
% ------------------------------------------------------------------------

lexicon_entry_preprocessor(tv, add_suffix, Base1, WFList, BaseList, BaseSem) :-
   atom_codes(Base1, BaseChars),
   once(build_token_list(BaseChars, BaseList)),
   (
     reverse(BaseList, [Base3, Base2|Rest]),
     member(Base3, [about, across, against, along, away, back, by, down, for, forward,
		    from, in, into, of, off, on, out, to, through, up, with, without]), 
     stemmer(add_suffix, Base2, WF1),
     reverse(WFList, [Base3, WF1|Rest])
   ;
     reverse(BaseList, [Base2|Rest]),
     stemmer(add_suffix, Base2, WF1),
     reverse(WFList, [WF1|Rest])
   ),
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
%   noun: remove_suffix
% ------------------------------------------------------------------------

lexicon_entry_preprocessor(noun, remove_suffix, WF1, WFList, BaseList, BaseSem) :-
   atom_codes(WF1, WFChars),
   once(build_token_list(WFChars, WFList)),
   reverse(WFList, [WF2|Rest]),
   stemmer(remove_suffix, WF2, Base),
   reverse(BaseList, [Base|Rest]),
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
%   noun: add_suffix
% ------------------------------------------------------------------------

lexicon_entry_preprocessor(noun, add_suffix, Base1, WFList, BaseList, BaseSem) :-
   atom_codes(Base1, BaseChars),
   once(build_token_list(BaseChars, BaseList)),
   reverse(BaseList1, [Base2|Rest]),
   stemmer(add_suffix, Base2, WF1),
   reverse(WFList, [WF1|Rest]),
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
%   rnoun: remove_suffix
% ------------------------------------------------------------------------

/* can be removed
lexicon_entry_preprocessor(rnoun, remove_suffix, WF1, WFList, BaseList, BaseSem) :-
   atom_codes(WF1, WFChars),
   once(build_token_list(WFChars, WFList)),
   (
     reverse(WFList, [WF3, WF2|Rest]),
     member(Base3, [of]), 
     stemmer(remove_suffix, WF2, Base),
     reverse(BaseList, [WF3, Base|Rest])
   ;
     reverse(WFList, [WF2|Rest]),
     stemmer(remove_suffix, WF2, Base),
     reverse(BaseList, [Base|Rest])
   ), 
   atomic_list_concat(BaseList, '_', BaseSem).
*/

lexicon_entry_preprocessor(rnoun, remove_suffix, WF1, WFList, BaseList, BaseSem) :-
   atom_codes(WF1, WFChars),
   once(build_token_list(WFChars, WFList)),
   reverse(WFList, [WF3, WF2|Rest]),
   member(Base3, [of]), 
   stemmer(remove_suffix, WF2, Base),
   reverse(BaseList, [WF3, Base|Rest]),
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
%   rnoun: add_suffix
% ------------------------------------------------------------------------

/* can be removed
lexicon_entry_preprocessor(rnoun, add_suffix, Base1, WFList, BaseList, BaseSem) :-
   atom_codes(Base1, BaseChars),
   once(build_token_list(BaseChars, BaseList)),
   (
     reverse(BaseList, [Base3, Base2|Rest]),
     member(Base3, [of]), 
     stemmer(add_suffix, Base2, WF1),
     reverse(WFList, [Base3, WF1|Rest])
   ;
     reverse(BaseList, [Base2|Rest]),
     stemmer(add_suffix, Base2, WF1),
     reverse(WFList, [WF1|Rest])
   ),
   atomic_list_concat(BaseList, '_', BaseSem).
*/

lexicon_entry_preprocessor(rnoun, add_suffix, Base1, WFList, BaseList, BaseSem) :-
   atom_codes(Base1, BaseChars),
   once(build_token_list(BaseChars, BaseList)),
   reverse(BaseList, [Base3, Base2|Rest]),
   member(Base3, [of]), 
   stemmer(add_suffix, Base2, WF1),
   reverse(WFList, [Base3, WF1|Rest]),
   atomic_list_concat(BaseList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/6
%   radj
% ------------------------------------------------------------------------

% can be removed
% lexicon_entry_preprocessor(radj, _, WF, WFList, _, BaseSem) :-
%   atom_codes(WF, WFChars),
%   once(build_token_list(WFChars, WFList)),
%   atomic_list_concat(WFList, '_', BaseSem).

lexicon_entry_preprocessor(radj, _, WF1, WFList, _, BaseSem) :-
   atom_codes(WF1, WFChars),
   once(build_token_list(WFChars, WFList)),
   reverse(WFList, [WF3, WF2|Rest]),
   member(WF3, [about, across, against, along, away, back, by, down, for, forward,
		from, in, into, of, off, on, out, to, through, up, with, without]), 
   reverse(WFList, [WF3, WF2|Rest]),
   atomic_list_concat(WFList, '_', BaseSem).


% ------------------------------------------------------------------------
% lexicon_entry_preprocessor/4
%  name
% ------------------------------------------------------------------------

lexicon_entry_preprocessor(pname, WF, WFList1, Base) :-
   atom_codes(WF, WFChars),
   once(build_token_list(WFChars, WFList1)),
   downcase_token_list(WFList1, WFList2),
   atomic_list_concat(WFList2, '_', Base).


% ------------------------------------------------------------------------
% build_token_list/2
% ------------------------------------------------------------------------

build_token_list(WFChars1, [Token|Tokens]) :-
   append(WFChars3, [32|WFChars2], WFChars1),
   atom_codes(Token, WFChars3),
   build_token_list(WFChars2, Tokens).

build_token_list(WFChars, [Token]) :-
   atom_codes(Token, WFChars).


% ------------------------------------------------------------------------
% downcase_token_list/2
% ------------------------------------------------------------------------

downcase_token_list([], []).

downcase_token_list([Token1|Tokens1], [Token2|Tokens2]) :-
   downcase_atom(Token1, Token2),
   downcase_token_list(Tokens1, Tokens2).
   

% ------------------------------------------------------------------------
% json_interface/2
%
% - save: text.tmp to top-level
% ------------------------------------------------------------------------

json_interface(json([id=Id, inputmode=IMode, editmode=save, token=TokenAtom, 
		     featurestructure=FS, filename=FName, spectext=Text,
		     snum=SNumAtom, spos=SPosAtom, reasoner=Flag, reasonermode=RMode]), Output) :-
  (
     % FName = 'text.tmp',
     nonvar(Text),
     atom_concat('', FName, Path),
     atom_chars(Text, CharList),
     open(Path, write, Stream),
     write_char_list(CharList, Stream),
     close(Stream),
     Output = json([id=Id, saved=yes])
  ;
     Output = json([id=Id, saved=no])
  ).


% ------------------------------------------------------------------------
% json_interface/2
%
% - save 
%   Example: FName = 'test.txt'
%            Text  = 'Roberta and Thelma are female.'
% ------------------------------------------------------------------------

json_interface(json([id=Id, inputmode=IMode, editmode=save, token=TokenAtom, 
		     featurestructure=FS, filename=FName, spectext=Text,
		     snum=SNumAtom, spos=SPosAtom, reasoner=Flag, reasonermode=RMode]), Output) :-
  (
     nonvar(FName),
     nonvar(Text),
     atom_concat('texts/', FName, Path),
     atom_chars(Text, CharList),
     open(Path, write, Stream),
     write_char_list(CharList, Stream),
     close(Stream),
     Output = json([id=Id, saved=yes])
  ;
     Output = json([id=Id, saved=no])
  ).

write_char_list([], Stream).

write_char_list([Char|Chars], Stream) :-
  atom_chars(Atom, [Char]),
  write(Stream, Atom),
  write_char_list(Chars, Stream).


% ------------------------------------------------------------------------
% json_interface/2
%
% - 'load' returns names of all available specifications; note FName = ' '
% ------------------------------------------------------------------------

json_interface(json([id=Id, inputmode=IMode, editmode=load, token=TokenAtom, 
		     featurestructure=FS, filename=FName, spectext=Text,
		     snum=SNumAtom, spos=SPosAtom, reasoner=Flag, reasonermode=RMode]), Output) :-
  FName = ' '
  ->
  directory_files('texts/', Entries),
  filter_entries(Entries, FilteredEntries),
  sort(FilteredEntries, SFilteredEntries),
  Output = json([id=Id, filenames=SFilteredEntries]).

filter_entries([], []).

filter_entries(['.'|Entries], FilteredEntries) :-
  filter_entries(Entries, FilteredEntries).

filter_entries([..|Entries], FilteredEntries) :-
  filter_entries(Entries, FilteredEntries).

filter_entries([Entry|Entries], [Entry|FilteredEntries]) :-
  filter_entries(Entries, FilteredEntries).

 
% ------------------------------------------------------------------------
% json_interface/2
%
% - mode=generate, text is generated using the current ASP file and
%   the re-processed using 'text.tmp"; since the text might have been
%   extended.
% ------------------------------------------------------------------------


json_interface(json([id=Id, inputmode=IMode, editmode=generate, token=TokenAtom, 
		     featurestructure=FS, filename=FName, spectext=Text,
		     snum=SNumAtom, spos=SPosAtom, reasoner=Flag, reasonermode=RMode]), Output) :-

  generate(GenSentences, _AnaList),			
  load_parser(chart),
  (
       current_cnl_file(''),
       load_complete_lexicon
  ;  
       current_cnl_file(Path),
       cnl_file(FileNumber, Path),
       load_specific_lexicon(FileNumber)
  ;
       current_cnl_file(Path),
       load_complete_lexicon
  ),
  asp_file(ASPFileName),
  tokeniser('text.tmp', Sentences, Atom), !,
  delete_file('text.tmp'),
  SNum = 0,
  process_sentences_2(chart, SNum, Sentences, [[]], [Clauses], [[]], [Ante]), !,
  writer(ASPFileName, [Clauses]),
  read_answerset_program(ASPFileName, AnswerSetProgram),
  execute_answerset_program(ASPFileName, Answers, AnswerSets),
  Output = json([id=Id, spectext=Atom, gentext=GenSentences, lookahead=[], ana=[],
		   answer=Answers, asp=AnswerSetProgram, reasoner=AnswerSets]).
   

% ------------------------------------------------------------------------
% json_interface/2
%
% - mode=load; this loads a specific filename (for example: 'test.txt')
% ------------------------------------------------------------------------

json_interface(json([id=Id, inputmode=IMode, editmode=load, token=TokenAtom, 
		     featurestructure=FS, filename=FName, spectext=Text,
		     snum=SNumAtom, spos=SPosAtom, reasoner=Flag, reasonermode=RMode]), Output) :-
  FName \= ' '
  ->
  clean_up_knowledge_base,
  atom_concat('texts/', FName, Path),
  asserta(current_cnl_file(Path)),
  write(user, bla),
  (
     cnl_file(FileNumber, Path),
     load_parser(chart),
     load_specific_lexicon(FileNumber)
  ;
     load_parser(chart),
     load_complete_lexicon
  ),
  write(user, bla2),
  asp_file(ASPFileName),
  tokeniser(Path, Sentences, Atom), !,
  SNum = 0,
  process_sentences_2(chart, SNum, Sentences, [[]], [Clauses], [[]], [Ante]), !,
  writer(ASPFileName, [Clauses]),
  read_answerset_program(ASPFileName, AnswerSetProgram),
  execute_answerset_program(ASPFileName, Answers, AnswerSets),
  Output = json([id=Id, spectext=Atom, lookahead=[], ana=[], answer=Answers, asp=AnswerSetProgram, reasoner=AnswerSets]).


% ------------------------------------------------------------------------
% json_interface/2
%
% ------------------------------------------------------------------------

json_interface(json([id=Id, inputmode=IMode, editmode=parse, token=TokenAtom, 
		     featurestructure=FS, filename=FName, spectext=Text,
		     snum=SNumAtom, spos=SPosAtom, reasoner=Flag, reasonermode=RMode]), Output) :-
  (
     atom_number(TokenAtom, Token)
  ;
     TokenAtom = Token
  ),
  atom_number(SNumAtom, SNum),
  atom_number(SPosAtom, SPos),
  chart_handler(Id, IMode, Token, SNum, SPos, Flag, RMode, Output).


% ------------------------------------------------------------------------
% json_lexicon/1
%
% ------------------------------------------------------------------------

json_lexicon(json([lexicon=JSONTerms])) :-
  findall(JSONTerm, collect_categories(JSONTerm), JSONTerms).


% ------------------------------------------------------------------------
% chart_handler/9
%
% ------------------------------------------------------------------------

% ------------------------------------------------------------------------
% chart_handler
% initialise first sentence
% ------------------------------------------------------------------------

chart_handler(Id, IMode, Token, SNum, SPos, Flag, RMode, Output) :-
  Token = ' ',
  SNum  = 1,
  SPos  = 0,
  Flag  = off
  ->
  clean_up_knowledge_base,
  asserta(current_cnl_file('')),
  load_complete_lexicon,
  LHS  = s([mode:proc, cl:[[]]-C2, ante:[[]]-A2, tree:T]),
  initialise_chart_parser(SNum, SPos, LHS),
  collect_lookahead_categories(SNum, SPos, _EPos, LAHCatsJSON),
  Output = json([id=Id, lookahead=LAHCatsJSON, ana=[]]).


% ------------------------------------------------------------------------
% initialise subsequent sentences
% ------------------------------------------------------------------------

chart_handler(Id, IMode, Token, SNum, SPos, Flag, RMode, Output) :-
  Token = ' ',
  SNum  > 1,
  SPos  = 0,
  Flag  = off
  ->
  edge_handler(SNum, SPos, _SNumPrev, _EPosPrev),   % check if more sentences can be deleted
  SNumPrev is SNum - 1,
  LHS1 = s([mode:proc, cl:C1-C2, ante:A1-A2, tree:T1]), 
  edge(SNumPrev, 0, _EPos, LHS1, _),                   
  LHS2 = s([mode:proc, cl:C2-C3, ante:A2-A3, tree:T2]), 
  initialise_chart_parser(SNum, SPos, LHS2),           
  collect_lookahead_categories(SNum, SPos, _EPos, LAHCatsJSON),
  collect_anaphoric_expressions(SPos, A2, AnaList),
  Output = json([id=Id, lookahead=LAHCatsJSON, ana=AnaList]).


% ------------------------------------------------------------------------
% all other cases
% ------------------------------------------------------------------------

chart_handler(Id, IMode, Token, SNum, SPos, Flag, RMode, Output) :-
  EPos is SPos + 1,
  edge_handler(SNum, EPos, _, _),
  (
     % Process full stop
     Token = '.',
     start_chart_parser(SNum, SPos, EPos, [Token]),                       
     inspect_chart(SNum, SPos, EPos, Clause, Ana, Tree), 
     call_reasoner(Flag, RMode, Token, Clause, Answer, AnswerSetProgram, AnswerSets), 
     LAHCats = [],
     AnaEx = [],
     Output = json([id=Id, lookahead=LAHCats, ana=AnaEx, answer=Answer, para=[],
		    tree=[], asp=AnswerSetProgram, reasoner=AnswerSets])
     % nl(user), nl(user), writeq(user,Output), nl(user), nl(user)
  ;
     % Process question mark
     Token = '?',
     start_chart_parser(SNum, SPos, EPos, [Token]),
     inspect_chart(SNum, SPos, EPos, Clause, Ana, Tree),
     call_reasoner(Flag, RMode, Token, Clause, Answers, AnswerSetProgram, AnswerSets),
     LAHCats = [],
     AnaEx   = [],
     Output = json([id=Id, lookahead=LAHCats, ana=AnaEx, answer=Answers, para=[],
		    tree=[], asp=AnswerSetProgram, reasoner=AnswerSets])
  ;
     % Process text token
     IMode == text,
     Flag = off,
     (
        start_chart_parser(SNum, SPos, EPos, [Token]),
        collect_lookahead_categories_x(SNum, SPos, EPos, LAHCatsJSON),
        collect_anaphoric_expressions_x(SNum, SPos, AnaList),
        Output =  json([id=Id, lookahead=LAHCatsJSON, ana=AnaList])
    ;
       % spelling_checker(text, SNum, SPos, EPos, [Token], Solution),
       Output = json([id=Id, 'spelling suggestions'=Solution])
       % nl(user), write(user, 'Spelling checker text: '), write(user, Solution), nl(user), nl(user)
     )
  ).


% ------------------------------------------------------------------------
% collect_lookahead_categories/4
% ------------------------------------------------------------------------

% check: LHS1 and LHS2

collect_lookahead_categories(SNum, SPos, _EPos, LAHCatsJSON2) :-
    SPos = 0,
    findall([cat:Cat, wform:WF|Rest], ( edge(SNum, _SPos, SPos, LHS, [lexicon([cat:Cat, wform:WF|Rest])|Categories]),
                      ( lexicon([cat:Cat, wform:WF|Rest]) ; true ),
		      WF = [Token|Tokens],
		      nonvar(Token),
		      atom_codes(Token, [Code|Codes]),
		      \+ member(Code, [97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
		       	               111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122]) ),
	    LAHCats1),
    sort(LAHCats1, LAHCats2),
    lexical_categories(LAHCats2, LexCats1),
    sort(LexCats1, LexCats2),
    compact_lookahead_categories(LexCats2, LAHCats2, LAHCatsJSON1),
    sort(0, @<, LAHCatsJSON1, LAHCatsJSON2).


collect_lookahead_categories_x(SNum, SPos, EPos, LAHCatsJSON2) :-
    findall([cat:Cat, wform:WF|Rest], ( edge(SNum, _SPos, EPos, D, [lexicon([cat:Cat, wform:WF|Rest])]),
                      ( lexicon([cat:Cat, wform:WF|Rest]) ; true ),
		        WF = [Token1|Tokens],
		        nonvar(Token1),
		        \+ member(Token1, ['A', 'An', 'The']) ), LAHCats1),
        sort(LAHCats1, LAHCats2),
        lexical_categories(LAHCats2, LexCats1),
        sort(LexCats1, LexCats2),
        compact_lookahead_categories(LexCats2, LAHCats2, LAHCatsJSON1),
	sort(0, @<, LAHCatsJSON1, LAHCatsJSON2).


collect_lookahead_categories_xx(SNum, SPos, EPos, LHS, LAHCatsJSON2) :-
    findall([cat:Cat, wform:WF|Rest], ( edge(SNum, _SPos, EPos, LHS, [lexicon([cat:Cat, wform:WF|Rest])]),
                      ( lexicon([cat:Cat, wform:WF|Rest]) ; true ),
		        WF = [Token1|Tokens],
		        nonvar(Token1),
		        \+ member(Token1, ['A', 'An', 'The']) ), LAHCats1),
        sort(LAHCats1, LAHCats2),
        lexical_categories(LAHCats2, LexCats1),
        sort(LexCats1, LexCats2),
        compact_lookahead_categories(LexCats2, LAHCats2, LAHCatsJSON1),
	sort(0, @<, LAHCatsJSON1, LAHCatsJSON2).


% ------------------------------------------------------------------------
% lexical_categories/2
% ------------------------------------------------------------------------

lexical_categories([], []).

lexical_categories([[cat:Cat|Rest1]|Rest2], [cat:Cat|Rest3]) :-
   lexical_categories(Rest2, Rest3).


% ------------------------------------------------------------------------
% compact_lookahead_categories/3
% ------------------------------------------------------------------------

compact_lookahead_categories(LexCats, [], []).

compact_lookahead_categories([cat:adj|LexCats], LAHCats1, [json([cat='adjective', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:adj], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:nadj|LexCats], LAHCats1, [json([cat='adjective: numerical', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:nadj], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:radj|LexCats], [[cat:radj, wform:WF, struc:compound]|LAHCats1], [json([cat='compound', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:radj], [[cat:radj, wform:WF, struc:compound]|LAHCats1], LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:radj|LexCats], LAHCats1, [json([cat='adjective: relational', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:radj], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:adv|LexCats], LAHCats1, [json([cat='adverb', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:adv], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:aop|LexCats], LAHCats1, [json([cat='operator: arithmetic', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:aop], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:aux_neg|LexCats], LAHCats1, [json([cat='verb: negation', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:aux_neg], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:cop_neg|LexCats], LAHCats1, [json([cat='copula: negation', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:cop_neg], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:cop|LexCats], LAHCats1, [json([cat='copula', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:cop], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:cst|LexCats], LAHCats1, [json([cat='constraint: strong', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:cst], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:wcst|LexCats], LAHCats1, [json([cat='constraint: weak', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:wcst], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:crd|LexCats], LAHCats1, [json([cat='coordination', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:crd], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:det|LexCats], LAHCats1, [json([cat='determiner', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:det], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:qnt|LexCats], LAHCats1, [json([cat='quantifier: universal', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:qnt], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:cqnt|LexCats], LAHCats1, [json([cat='quantifier: cardinal', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:cqnt], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:noun|LexCats], [[cat:noun, wform:WF, struc:compound]|LAHCats1], [json([cat='compound', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:noun], [[cat:noun, wform:WF, struc:compound]|LAHCats1], LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:noun|LexCats], LAHCats1, [json([cat='noun: common', wform=WF2, num=N])|JSONs]) :-
   compact_lookahead_category([cat:noun, num:N], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:rnoun|LexCats], [[cat:rnoun, wform:WF, struc:compound]|LAHCats1], [json([cat='compound', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:rnoun], [[cat:rnoun, wform:WF, struc:compound]|LAHCats1], LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:rnoun|LexCats], LAHCats1, [json([cat='noun: relational', wform=WF2, num=N])|JSONs]) :-
   compact_lookahead_category([cat:rnoun, num:N], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:number|LexCats], LAHCats1, [json([cat='number', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:number], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:prep|LexCats], LAHCats1, [json([cat='preposition', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:prep], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:pname|LexCats], [[cat:pname, wform:WF, struc:compound]|LAHCats1], [json([cat='compound', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:pname], [[cat:pname, wform:WF, struc:compound]|LAHCats1], LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).


compact_lookahead_categories([cat:pname|LexCats], LAHCats1, [json([cat='name', wform=WF2, num=N])|JSONs]) :-
   compact_lookahead_category([cat:pname, num:N], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:iv|LexCats], [[cat:iv, wform:WF, struc:compound]|LAHCats1], [json([cat='compound', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:iv], [[cat:iv, wform:WF, struc:compound]|LAHCats1], LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:iv|LexCats], LAHCats1, [json([cat='verb: intransitive', wform=WF2, vform=V, num=N])|JSONs]) :-
   compact_lookahead_category([cat:iv, num:N, vform:V], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:tv|LexCats], [[cat:tv, wform:WF, struc:compound]|LAHCats1], [json([cat='compound', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:tv], [[cat:tv, wform:WF, struc:compound]|LAHCats1], LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:tv|LexCats], LAHCats1, [json([cat='verb: transitive', wform=WF2, vform=V, num=N])|JSONs]) :-
   compact_lookahead_category([cat:tv, num:N, vform:V], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

% ------

compact_lookahead_categories([cat:tv|LexCats], LAHCats1, [json([cat='imperative', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:tv], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:rpn|LexCats], LAHCats1, [json([cat='pronoun: relative', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:rpn], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:var|LexCats], LAHCats1, [json([cat='variable: string', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:var], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:nvar|LexCats], LAHCats1, [json([cat='variable: numeric', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:nvar], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:expl|LexCats], LAHCats1, [json([cat='expletive', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:expl], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:wh|LexCats], LAHCats1, [json([cat='question word', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:wh], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:fs|LexCats], LAHCats1, [json([cat='full stop', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:fs], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:qm|LexCats], LAHCats1, [json([cat='question mark', wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:qm], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:vp|LexCats], LAHCats1, [json([cat='keyphrase', wform=WF2])|JSONs]) :-
   compact_lookahead_category([cat:vp], LAHCats1, LAHCats2, WF1),
   sort(WF1, WF2),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:Cat|LexCats], LAHCats1, [json([cat=Cat, wform=WF])|JSONs]) :-
   compact_lookahead_category([cat:Cat], LAHCats1, LAHCats2, WF),
   compact_lookahead_categories(LexCats, LAHCats2, JSONs).

compact_lookahead_categories([cat:Cat|LexCats], LAHCats, JSONs) :-
   compact_lookahead_categories(LexCats, LAHCats, JSONs).


% ------------------------------------------------------------------------
% compact_lookahead_category/4
% ------------------------------------------------------------------------

compact_lookahead_category([cat:Cat|Rest1], [], [], []).

compact_lookahead_category([cat:Cat|Rest1], [[cat:Cat, wform:WF|Rest2]|LAHCats1], LAHCats2, [WF|WFs]) :-
   subset([cat:Cat|Rest1], [cat:Cat, wform:WF|Rest2]), 
   compact_lookahead_category([cat:Cat|Rest1], LAHCats1, LAHCats2, WFs).

compact_lookahead_category([cat:Cat1|Rest1], [[cat:Cat2, wform:WF|Rest2]|LAHCats1], [[cat:Cat2, wform:WF|Rest2]|LAHCats2], WFs) :-
   Cat1 \= Cat2,
   compact_lookahead_category([cat:Cat1|Rest1], LAHCats1, LAHCats2, WFs).


% ------------------------------------------------------------------------
% collect_anaphoric_expressions_x/3
% ------------------------------------------------------------------------

collect_anaphoric_expressions_x(SNum, SPos, AnaList) :-
    findall(A2, ( edge(SNum, SPos, _EPos, np(List), _),
		      member(ante:A2-A3, List),
		      A2 \= [[]], A2 \= [[], []] ),
	              AnteList1),
    sort(AnteList1, AnteList2),
    collect_anaphoric_expressions(SPos, AnteList2, AnaList).


% ------------------------------------------------------------------------
% collect_anaphoric_expressions/3
% ------------------------------------------------------------------------
  
collect_anaphoric_expressions(SPos, [], []).

collect_anaphoric_expressions(SPos, [A2|Rest], Anaphora4) :-
   compact_anaphoric_expressions(SPos, A2, Anaphora1),
   collect_anaphoric_expressions(SPos, Rest, Anaphora2),
   append(Anaphora1, Anaphora2, Anaphora3),
   sort(Anaphora3, Anaphora4).

compact_anaphoric_expressions(SPos, [], []).

compact_anaphoric_expressions(SPos, [named(X, PName)|Literals], [WForm|AnaEx]) :-
   lexicon([cat:pname, wform:WForm, num:Num, arg:X, lit:named(X, PName)]),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).


% Preliminary needs to be fixed
compact_anaphoric_expressions(SPos, [class(X, CName), data_prop(X, pos_int(Int), Type)|Literals], AnaEx) :-
   lexicon([cat:nadj, wform:WForm1, num:Num, type:Type, arg:X, lit:data_prop(X, pos_int(Int), Type)]),
   lexicon([cat:noun, wform:WForm2, num:Num, arg:X, lit:class(X, CName)]),
   append(WForm1, WForm2, NPList),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).


/**
compact_anaphoric_expressions(SPos, [class(X, CName), data_prop(X, pos_int(Int), Type)|Literals], [DefNPList|AnaEx]) :-
   SPos = 0,
   lexicon([cat:nadj, wform:WForm1, num:Num, type:Type, arg:X, lit:data_prop(X, pos_int(Int), Type)]),
   lexicon([cat:noun, wform:WForm2, num:Num, arg:X, lit:class(X, CName)]),
   append(WForm1, WForm2, WForm3),
   append(['The'], WForm3, DefNPList),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).

compact_anaphoric_expressions(SPos, [class(X, CName),  data_prop(X, pos_int(Int), Type)|Literals], [DefNPList|AnaEx]) :-
   SPos > 1,
   lexicon([cat:nadj, wform:WForm1, num:Num, type:Type, arg:X, lit:data_prop(X, pos_int(Int), Type)]),
   lexicon([cat:noun, wform:WForm2, num:Num, arg:X, lit:class(X, CName)]),
   append(WForm1, WForm2, WForm3),
   append([the], WForm3, DefNPList),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).
**/

compact_anaphoric_expressions(SPos, [class(X, CName), prop(X, PName)|Literals], [DefNPList|AnaEx]) :-
   SPos = 0,
   lexicon([cat:adj,  wform:WForm1, arg:X, lit:prop(X, PName)]),
   lexicon([cat:noun, wform:WForm2, num:Num, arg:X, lit:class(X, CName)]),
   append(WForm1, WForm2, WForm3),
   append(['The'], WForm3, DefNPList),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).

compact_anaphoric_expressions(SPos, [class(X, CName), prop(X, PName)|Literals], [DefNPList|AnaEx]) :-
   SPos > 1,
   lexicon([cat:adj,  wform:WForm1, arg:X, lit:prop(X, PName)]),
   lexicon([cat:noun, wform:WForm2, num:Num, arg:X, lit:class(X, CName)]),
   append(WForm1, WForm2, WForm3),
   append([the], WForm3, DefNPList),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).

compact_anaphoric_expressions(SPos, [class(X, CName)|Literals], [DefNPList|AnaEx]) :-
   SPos = 0,
   lexicon([cat:noun, wform:WForm, num:Num, arg:X, lit:class(X, CName)]),
   append(['The'], WForm, DefNPList),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).

compact_anaphoric_expressions(SPos, [class(X, CName)|Literals], [DefNPList|AnaEx]) :-
   SPos > 1,
   lexicon([cat:noun, wform:WForm, num:Num, arg:X, lit:class(X, CName)]),
   append([the], WForm, DefNPList),
   compact_anaphoric_expressions(SPos, Literals, AnaEx).

compact_anaphoric_expressions(SPos, [Literal|Literals], AnaEx) :-
   compact_anaphoric_expressions(SPos, Literals, AnaEx).


% -----------------------------------------------------------------------
% timex_checker/1
%
% -----------------------------------------------------------------------

timex_checker(Token) :-
   atom_codes(Token, [H1, H2, 58, M1, M2]),
   (
      H1 = 48, H2 = 48, M1 = 48, M2 = 48
   ;
      H1 = 48, H2 = 48, M1 = 48, M2 > 48, M2 =< 57
   ;
      H1 = 48, H2 = 48,  M1 > 48, M2 >= 48, M2 =< 57
   ;
      H1 = 48, H2 > 48, H2 =< 57,  M1 >= 48, M1 =< 57, M2 >= 48, M2 =< 57
   ;
      H1 > 48, H1 =< 57, H2 >= 48, H2 =< 57, M1 >= 48, M1 =<57, M2>=48, M2 =< 57
   ).


% -----------------------------------------------------------------------
% edge_handler/4
%
% -----------------------------------------------------------------------

edge_handler(SNum1, EPos1, SNumP, EPosP) :-
  EPos1 = 0,
  (
    find_max_sent_pos(SNum1, EPos1, [SNum2, EPos2]),
    SNum2 >= SNum1,
    check_edges(SNum1, EPos1, SNum2, EPos2)
  ;
    true
  ),
  SNumP is SNum1 - 1,
  find_max_pos(SNumP, EPosP).


edge_handler(SNum1, EPos1, _, _) :-
  EPos1 > 0,
  find_max_sent_pos(SNum1, EPos1, [SNum2, EPos2]),
  check_edges(SNum1, EPos1, SNum2, EPos2).


% -----------------------------------------------------------------------
% find_max_sent_pos/3
%
% -----------------------------------------------------------------------

find_max_sent_pos(SNum1, EPos1, [SNum2, EPos2]) :-
  findall([SNum1, EPos1], edge(SNum1, _, EPos1, _, _), Lists),
  (
     Lists = []
     ->
     SNum1 = SNum2,
     EPos1 = EPos2
  ;
     max_list_of_lists(Lists, [SNum2, EPos2])
  ).


% -----------------------------------------------------------------------
% find_max_pos/2
%
% -----------------------------------------------------------------------

find_max_pos(SNum, MaxPos) :-
  findall(EPos, edge(SNum, _, EPos, _, _), List),
  max_list(List, MaxPos).


% -----------------------------------------------------------------------
% check_edges/4
%
% -----------------------------------------------------------------------

check_edges(SNum1, EPos1, SNum2, EPos2) :-
  (
     SNum1 =  SNum2,
     EPos2 >= EPos1
     ->
     delete_edges_within_sentence(SNum1, EPos1, EPos2)
  ;
     SNum1 = SNum2
     ->
     true
  ;
     SNum2 > SNum1
     ->
     delete_edges_outside_sentence(SNum1, EPos1, SNum2)
  ).


% -----------------------------------------------------------------------
% delete_edges_within_sentence/3
%
% -----------------------------------------------------------------------

delete_edges_within_sentence(SNum, EPos, EPos) :-
   retractall(edge(SNum, _, EPos, _, _)).

delete_edges_within_sentence(SNum, EPos1, EPos3) :-
   EPos2 is EPos3 - 1,
   retractall(edge(SNum, _, EPos3, _, _)),
   delete_edges_within_sentence(SNum, EPos1, EPos2).


% -----------------------------------------------------------------------
% delete_edges_outside_sentence/3
%
% -----------------------------------------------------------------------

delete_edges_outside_sentence(SNum, EPos1, SNum) :-
  find_max_pos(SNum, EPos2),
  Pos2 >= EPos1,
  delete_edges_within_sentence(SNum, EPos1, EPos2).

delete_edges_outside_sentence(SNum1, EPos1, SNum3) :-
  SNum2 is SNum3 - 1,
  retractall(edge(SNum3, _, EPos3, _, _)),
  delete_edges_outside_sentence(SNum1, EPos1, SNum2).


% -----------------------------------------------------------------------
% inspect_chart/6
%
% -----------------------------------------------------------------------

inspect_chart(SNum, SPos, EPos, C2, A2, Tree) :-
  LHS = s([mode:proc, cl:C1-C2, ante:A1-A2, tree:Tree]),
  edge(SNum, 0, _EPos, LHS, _).


% -----------------------------------------------------------------------
% show_edges/1
%
% -----------------------------------------------------------------------

show_edges(SNum) :-
  findall(edge(SNum, B, C, D, E), edge(SNum, B, C, D, E), Edges),
  display_edges(Edges).

display_edges([]).

display_edges([edge(A, B, C, D, E)|Edges]) :-
  E =.. [E1|R],
  write(user, [edge(A, B, C, D, E)]),
  nl(user), nl(user),
  retract(edge(A, B, C, D, E)),
  display_edges(Edges).


% -----------------------------------------------------------------------
% max_list/2
%
%   - returns the largest integer of a list of integers
% -----------------------------------------------------------------------

max_list([H|T], M) :-
  max_list(T, H, M).

max_list([], C, C).
max_list([H|T], C, M) :-
  C2 is max(C, H),
  max_list(T, C2, M).


% -----------------------------------------------------------------------
% max_list_of_lists/2
%
%   - returns the the max list of lists
% -----------------------------------------------------------------------

max_list_of_lists([H|T], M) :-
  max_list_of_lists(T, H, M).

max_list_of_lists([], C, C).
max_list_of_lists([[H1, H2]|T], [C1, C2], M) :-
  (
    H1 > C1
    ->
    C3 = H1, C4 = H2
  ;
    H1 < C1
    ->
    C3 = C1, C4 = C2
  ;
    H1 = C1, H2 >= C2
    ->
    C3 = H1, C4 = H2
  ;
    H1 = C1, H1 < C2
    ->
    C3 = C1, C4 = C2
  ),
  max_list_of_lists(T, [C3, C4], M).



% ------------------------------------------------------------------------
% clean_up_knowledge_base/0
%
% ------------------------------------------------------------------------

clean_up_knowledge_base :-
   retractall(current_cnl_file(_)),
   retractall(edge(_, _, _, _, _)).


% -----------------------------------------------------------------------
% call_reasoner/6
%
% -----------------------------------------------------------------------

/*
call_reasoner(Flag, RMode, Token, Clauses, Answer, AnswerSetProgram, AnswerSets) :-
   Flag = on,
   Token = '.',
   asp_file(FileName),
   writer(FileName, Clauses),
   read_answerset_program(FileName, AnswerSetProgram),
   execute_answerset_program(FileName, AnswerSets).

execute_answerset_program(FileName, AnswerSets)   :-
   asp_solver(Solver),
   process_create(Solver, [0, FileName], [stdout(pipe(Stream))] ),
   read_stream_to_codes(Stream, Codes),
   atom_codes(AnswerSets, Codes).
*/

call_reasoner(Flag, RMode, Token, Clauses, Answers, AnswerSetProgram, AnswerSets) :-
   Flag = on,
   ( Token = '.' ; Token = '?'),
   asp_file(FileName),
   writer(FileName, Clauses),
   read_answerset_program(FileName, AnswerSetProgram),
   execute_answerset_program(FileName, Answers, AnswerSets).

execute_answerset_program(FileName, Answers, AnswerSets)   :-
   asp_solver(Solver),
   process_create(Solver, [0, FileName], [stdout(pipe(Stream))] ),
   read_stream_to_codes(Stream, Codes),
   atom_codes(AnswerSets, Codes),
   extract_answers_from_answersets(Codes, Answers).


% -----------------------------------------------------------------------
% read_answerset_program/2
%
% -----------------------------------------------------------------------

read_answerset_program(Path, AnswerSetProgram) :-
  open(Path, read, Stream),
  read_answerset_program_x(Stream, CodeList),
  close(Stream),
  atom_codes(AnswerSetProgram, CodeList).

read_answerset_program_x(Stream, []) :-
  at_end_of_stream(Stream).

read_answerset_program_x(Stream, [Code|Codes]) :-
  \+ at_end_of_stream(Stream),
  get_code(Stream, Code),
  read_answerset_program_x(Stream, Codes).


% -----------------------------------------------------------------------
% extract_answers_from_answersets/2
%
% -----------------------------------------------------------------------

extract_answers_from_answersets(Codes, Answers) :-
   extract_answer_literals(Codes, Answers).

extract_answer_literals([], []).

extract_answer_literals([97, 110, 115, 119, 101, 114|Rest1], [AnswerLiteral|Answers]) :-
   append([40|Arguments], [41, Code|Rest2], Rest1),
   ( Code = 32 ; Code = 10 ),
   append([97, 110, 115, 119, 101, 114], [40|Arguments], Codes1),
   append(Codes1, [41], Codes2),
   atom_codes(AnswerAtom, Codes2),
   atom_to_term(AnswerAtom, AnswerTerm, _),
   (
      AnswerTerm = answer(named(X, PName))
      ->
      lexicon([cat:pname, wform:WF, num:_, arg:X, lit:named(X, PName)]),
      atomic_list_concat(WF, ' ', AnswerLiteral)
   ;
      AnswerTerm = answer(class(X, CName))
      ->
      lexicon([cat:noun, wform:WF, num:_, arg:X, lit:class(X, CName)]),
      atomic_list_concat(WF, ' ', AnswerLiteral)
   ;
      AnswerAtom = AnswerLiteral
   ),
   extract_answer_literals(Rest2, Answers).

extract_answer_literals([Code|Codes], Answers) :-
   extract_answer_literals(Codes, Answers).


/*
extract_answers_from_answersets(Codes, Answers) :-
   %% nl(user), nl(user), write(user, Codes), nl(user), nl(user),
   extract_answer(Codes, Answers).

extract_answer([], []).

extract_answer([97, 110, 115, 119, 101, 114|Rest1], [Answer|Answers]) :-
   append([40|Arguments], [41, 32|Rest2], Rest1),
   append([97, 110, 115, 119, 101, 114], [40|Arguments], Codes1),
   append(Codes1, [41], Codes2),
   atom_codes(Answer, Codes2),
   extract_answer(Rest2, Answers).

extract_answer([97, 110, 115, 119, 101, 114|Rest1], [Answer|Answers]) :-
   append([40|Arguments], [41, 10|Rest2], Rest1),
   append([97, 110, 115, 119, 101, 114], [40|Arguments], Codes1),
   append(Codes1, [41], Codes2),
   atom_codes(Answer, Codes2),
   extract_answer(Rest2, Answers).

extract_answer([Code|Codes], Answers) :-
   extract_answer(Codes, Answers).
*/

% ------------------------------------------------------------------------
% add_lexical_entries_to_system/1
%
% ------------------------------------------------------------------------

add_lexical_entries_to_system([]).

add_lexical_entries_to_system([Entry|Entries]) :-
  assert(Entry),               % assert entry to knowledge base
  add_lexical_entries_to_system(Entries).


/*
add_lexical_entries_to_system(Entries) :-
  absolute_file_name('prolog/lexicon.pl', Absolute),
  open(Absolute, append, Stream),
  nl(Stream),
  nl(Stream),
  add_lexical_entries(Entries, Stream),
  close(Stream).

add_lexical_entries([], Stream).

add_lexical_entries([Entry|Entries], Stream) :-
  assert(Entry),               % assert entry to knowledge base
  numbervars(Entry),
  writeq(Stream, Entry),       % add entry to user lexicon
  write(Stream, '.'),
  nl(Stream),
  nl(Stream),
  add_lexical_entries(Entries, Stream).
*/

% ------------------------------------------------------------------------
% stemmer/3
%
% ------------------------------------------------------------------------

stemmer(remove_suffix, WF, Base) :-
  atom_chars(WF, Chars1),
  (
     append(Chars2, [z, z, e, s], Chars1),
     append(Chars2, [z], CharsBase)
  ;
     append(Chars2, [s, s, e, s], Chars1),
     append(Chars2, [s], CharsBase)
  ;
     append(Chars2, [z, e, s], Chars1),
     append(Chars2, [z], CharsBase)
  ;
     append(Chars2, [c, h, e, s], Chars1),
     append(Chars2, [c, h], CharsBase)
  ;
     append(Chars2, [s, h, e, s], Chars1),
     append(Chars2, [s, h], CharsBase)
  ;
     append(Chars2, [C, i, e, s], Chars1),
     \+ vowel(C),
     append(Chars2, [C, y], CharsBase)
  ;
     append(Chars2, [s], Chars1),
     append(Chars2, [], CharsBase)
  ;
     Chars1 = CharsBase
  ),
  atom_chars(Base, CharsBase).


stemmer(add_suffix, Base, WF) :-
  atom_chars(Base, Chars1),
  (
     append(Chars2, [z, z], Chars1),
     append(Chars2, [z, z, e, s], CharsBase)
  ;
     append(Chars2, [s, s], Chars1),
     append(Chars2, [s, s, e, s], CharsBase)
  ;
     append(Chars2, [z], Chars1),
     append(Chars2, [z, e, s], CharsBase)
  ;
     append(Chars2, [c, h], Chars1),
     append(Chars2, [c, h, e, s], CharsBase)
  ;
     append(Chars2, [s, h], Chars1),
     append(Chars2, [s, h, e, s], CharsBase)
  ;
     append(Chars2, [C, y], Chars1),
     \+ vowel(C),
     append(Chars2, [C, i, e, s], CharsBase)
  ;
     append(Chars2, [], Chars1),
     append(Chars2, [s], CharsBase)
  ;
     Chars1 = CharsBase
  ),
  atom_chars(WF, CharsBase).


stemmer(_, WF, Base) :-
  WF = Base.


vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).
vowel(y).

