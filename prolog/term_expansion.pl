% ================================================================
% Project: PENG ASP
% Module:  term_expansion.pl
% Author:  Rolf Schwitter
% Date:    2019-01-06
% =================================================================

% -----------------------------------------------------------------------
% Style checking
% -----------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).


% -----------------------------------------------------------------------
% term_expansion/2
% transfors a definite clause gammar into a format that can be processed
% by the chart parser
% -----------------------------------------------------------------------

term_expansion((LHS --> RHS), Format) :-
  (
     RHS    = ( { lexicon(List) }, TokenList )
     ->
     Format = ( rule(LHS, [ lexicon(List) ]) )
  ;
     RHS    = ( TokenList, { lexicon(List) } )
     ->
     Format = ( rule(LHS, [ lexicon(List) ]) )
  ;
     conjunction_to_list_rhs(RHS, RHSList)
     ->
     Format = ( rule(LHS, RHSList) )
  ;
     Format = ( rule(LHS, RHS ) )
  ).

conjunction_to_list_rhs(RHS, RHSList) :-
  \+ is_list(RHS), 
  conjunction_to_list(RHS, RHSList).

conjunction_to_list((Term, Terms), [Term|ListofTerms]) :- !, 
  nonvar(Term), 
  conjunction_to_list(Terms, ListofTerms).

conjunction_to_list(Term, [Term]).