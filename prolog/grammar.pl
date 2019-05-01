% ==================================================================
% Project: PENG ASP				
% Module:  grammar.pl
% Author:  Rolf Schwitter
% Date:    2019-04-01
% ==================================================================
  
% ==================================================================
% Modules
% ==================================================================

% :- module(grammar, [s/3]).

:- use_module(anaphora_resolution, [anaphora_resolution/1]).

% ------------------------------------------------------------------
% Style checking
% ------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).



% ------------------------------------------------------------------
% Question
% ------------------------------------------------------------------

s([mode:proc, cl:[C1|Sup]-[['.', C2, '-:', [answer(named(X, PName))]|C1]|Sup], ante:A1-A2, tree:[s(q1), [np(1), [who]],  VP]]) -->
   wh([mode:proc, ct:query, wform:['Who'], tree:WH]),
   vp([mode:proc, ct:query, crd:'-', inv:'-', num:N, arg:X, cl:[[named(X, PName)]|C1]-[C2|C1], ante:A1-A2, tree:VP]),
   qm([mode:proc, cl:_-_]).


s([mode:gen, cl:[[[answer(named(X, PName))], ':-', [named(X, PName)|C2], '.'|C1]|Sup]-[C1|Sup], ante:A1-A2, tree:[s(q2), [np, [who]], VP]]) -->
   wh([mode:gen, ct:query, wform:['Who'], tree:WH]),
   vp([mode:gen, ct:query, crd:'-', inv:'-', num:N, arg:X, cl:[C2|C1]-[[]|C1], ante:A1-A2, tree:VP]),
   qm([mode:gen, cl:_-_]).


s([mode:proc, cl:[C1|Sup]-[['.', C3, '-:', [answer(named(Y, PName))]|C4]|Sup], ante:A1-A3, tree:[s(q3), [np, [where]], Aux, NP, VP]]) -->
   wh([mode:proc, ct:query, wform:['Where'], tree:WH]),
   aux([mode:proc, num:N, vform:fin, tree:Aux]),
   np([mode:proc, ct:query, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:[[named(Y, PName), class(Y, location)]|C1]-C2, ante:A1-A2, tree:NP]),
   vp([mode:proc, ct:query, crd:'-', inv:'+', num:_N, arg:X, arg:Y, cl:C2-[C3|C4], ante:A2-A3, tree:VP]),
   qm([mode:proc, cl:_-_]).


s([mode:gen, cl:[[[answer(named(Y, PName))], ':-', [class(Y, location), named(Y, PName)|C2], '.'|C1]|Sup]-[C1|Sup], ante:A1-A3, tree:[s(q4), [np, [where]], Aux, NP, VP]]) -->
   wh([mode:gen, ct:query, wform:['Where'], tree:WH]),
   aux([mode:gen, num:N, vform:fin, tree:Aux]),
   np([mode:gen, ct:query, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:[C2|C3]-C4, ante:A1-A2, tree:NP]),
   vp([mode:gen, ct:query, crd:'-', inv:'+', num:_N, arg:X, arg:Y, cl:C4-[[]|C1], ante:A2-A3, tree:VP]),
   qm([mode:gen, cl:_-_]).


% ------------------------------------------------------------------
% If John works then John is successful.
% ------------------------------------------------------------------

s([mode:M, cl:C1-C3, ante:A1-A3, tree:[s(3), Prep, S1, Adv, S2]]) -->
   prep([mode:M, wform:['If'], head:H, body:B, cl:C1-C2, tree:Prep]),
   s1([mode:M, ct:body, crd:'+', cl:B, ante:[[]|A1]-A2, tree:S1]),
   adv([mode:M, wform:[then], tree:Adv]),
   s([mode:M, ct:head, cl:H, ante:A2-A3, tree:S2]),
   fs([mode:M, cl:C2-C3]).


% ------------------------------------------------------------------

s1([mode:M, ct:body, crd:'+', cl:C, ante:A, tree:[s(4), S]]) -->
   s1([mode:M, ct:body, crd:'-', cl:C, ante:A, tree:S]).


s1([mode:M, ct:body, crd:'-', cl:C1-C3, ante:A1-A3, tree:[s(5), NP, VP]]) -->
   np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:NP]),
   vp([mode:M, ct:body, crd:'+', num:N, arg:X, cl:C2-C3, ante:A2-A3, tree:VP]).


s1([mode:M, ct:body, crd:'+', cl:C1-C4, ante:A1-A3, tree:[s(6), S1, CRD, S2]]) -->
   s1([mode:M, ct:body, crd:'-', cl:C1-C2, ante:A1-A2, tree:S1]),
   crd([mode:M, wform:[and], cl:C2-C3, tree:CRD]),
   s1([mode:M, ct:body, crd:'+', cl:C3-C4, ante:A2-A3, tree:S2]).


s([mode:M, ct:head, cl:C1-C3, ante:A1-A4, tree:[s(7), NP, VP]]) --> 
   np([mode:M, ct:head, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:NP]),
   vp([mode:M, ct:head, crd:'+', num:N, arg:X, cl:C2-C3, ante:A2-[A3|A4], tree:VP]).


% ------------------------------------------------------------------
% Every student who studies at Macquarie University work or parties.
% Every student has exactly one supervisor.
% Note: crd:'+'
% ------------------------------------------------------------------

s([mode:M, cl:C1-C3, ante:A1-A4, tree:[s(8), NP, VP]]) -->
   np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, head:H, cl:C1-C2, ante:[[]|A1]-A2, tree:NP]),
   vp([mode:M, ct:head, crd:'+', num:N, arg:X, cl:H, ante:A2-[A3|A4], tree:VP]),
   fs([mode:M, cl:C2-C3]).


% -------------------------------------------------------------------
% Every student is a <class>.
% Note: crd:'-'
% -------------------------------------------------------------------

s([mode:M, cl:C1-C3, ante:A1-A4, tree:[s(9), NP, VP]]) -->
   np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, head:H, cl:C1-C2, ante:[[]|A1]-A2, tree:NP]),
   vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:H, ante:A2-[A3|A4], tree:VP]),
   fs([mode:M, cl:C2-C3]).


% -------------------------------------------------------------------
% It is not the case that ...
% -------------------------------------------------------------------

s([mode:M, cl:C1-C3, ante:A1-A3, tree:[s(10), CST, Sent]]) -->
   cst([mode:M, scope:S, cl:C1-C2, tree:CST]),
   s([mode:M, ct:body, crd:'+', cl:S, ante:[[]|A1]-[A2|A3], tree:Sent]),
   fs([mode:M, cl:C2-C3]).


% % -------------------------------------------------------------------

s([mode:M, ct:body, crd:'+', cl:C, ante:A, tree:[s(11), S]]) -->
   s([mode:M, ct:body, crd:'-', cl:C, ante:A, tree:S]).

s([mode:M, ct:body, crd:'+', cl:C1-C4, ante:A1-A3, tree:[s(12), S1, CRD, S2]]) -->
   s([mode:M, ct:body, crd:'-', cl:C1-C2, ante:A1-A2, tree:S1]),
   crd([mode:M, wform:[and], cl:C2-C3, tree:CRD]),
   s([mode:M, ct:body, crd:'+', cl:C3-C4, ante:A2-A3, tree:S2]).

s([mode:M, ct:body, crd:'-', cl:C1-C3, ante:A1-A3, tree:[s(13), NP, VP]]) -->
   np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:NP]),
   vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C2-C3, ante:A2-A3, tree:VP]).


% -------------------------------------------------------------------
% Choose either the accommodation Amora or Grace or Metro or Posh or Adina.
% -------------------------------------------------------------------

s([mode:proc,
   cl:[C1]-[['.', [1, '}', [Cond, ':', Lit], '{', 1]|C1]], 
   ante:A-A,
   tree:[s(14), Tv, Adv, Det, Noun, NP]]) -->
  tv([mode:proc, wform:['Choose'], vform:bse, tree:Tv]),
  adv([mode:proc, wform:[either], tree:Adv]), 
  det([mode:proc, num:N, def:'+', cl:C1-C2, tree:Det]),
  noun([mode:proc, num:N, arg:X, cl:[[]]-[Lit], ante:A1-A2, tree:Noun]), 
  np([mode:proc, ct:choice, crd:'+', fcn:obj, def:_D, num:N, arg:X, head:S, cl:[[]]-[Cond], ante:A1-A4, tree:NP]),
  fs([mode:proc, cl:[[]]-[['.']]]).


s([mode:gen,
  cl:[[[1, '{', [Lit, ':', Cond], '}', 1], '.'|C1]]-[C1],
  ante:A-A,
  tree:[s(15), Tv, Adv, Det, Noun, NP]]) -->
  % ['Choose'], [either],
  tv([mode:gen, wform:['Choose'], vform:bse, tree:Tv]),
  adv([mode:gen, wform:[either], tree:Adv]),  
  det([mode:gen, num:N, def:'+', cl:_C1-_C2, tree:Det]),
  noun([mode:gen, num:N, arg:X, cl:[Lit]-[[]], ante:A1-A2, tree:Noun]), 
  np([mode:gen, ct:choice, crd:'+', fcn:obj, def:_D, num:N, arg:X, head:S, cl:[Cond]-[[]], ante:A1-A4, tree:NP]),
  fs([mode:gen, cl:[['.']]-[[]]]).


np([mode:M, ct:choice, crd:'+', fcn:obj, def:_D, num:N, arg:X, head:S, cl:C1-C4, ante:A1-A3, tree:[np(choice), PName, CRD, NP]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   crd([mode:M, wform:[or], cl:C2-C3, tree:CRD]),
   np([mode:M, ct:choice, crd:'+', fcn:obj, def:_D, num:N, arg:X, head:S, cl:C3-C4, ante:A2-A3, tree:NP]).  


np([mode:M, ct:choice, crd:'+', fcn:obj, def:_D, num:N, arg:X, head:S, cl:C1-C4, ante:A1-A3, tree:[np(choice), PName, CRD, NP]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   crd([mode:M, wform:[or], cl:C2-C3, tree:CRD]),
   np([mode:M, ct:choice, crd:'-', fcn:obj, def:_D, num:N, arg:X, head:S, cl:C3-C4, ante:A2-A3, tree:NP]).  


np([mode:M, ct:choice, crd:'-', fcn:obj, def:_D, num:N, arg:X, head:S, cl:C1-C2, ante:A1-A2, tree:[np(choice), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]).                                             


% -------------------------------------------------------------------
% Minimizing that an accommodation is noisy has the high priority 3.
% -------------------------------------------------------------------

s([mode:proc,
   cl:[C1]-[['.', ['}', [C3, ':', [X, '@'(1,P)]], '{', '#minimize']|C1]], 
   ante:A1-A1,
   tree:[s(16), WCst, RC, VP]]) -->
  wcst([mode:proc, wform:['Minimizing'], tree:WCst]), 
  rc([mode:proc, ct:wcst, num:N, wgt:1, arg:X, cl:[[]]-C2, ante:A1-A2, tree:RC]),
  vp([mode:proc, ct:wcst, crd:'-', num:N, lev:P, arg:X, cl:C2-[C3], ante:A2-A3, tree:VP]),
  fs([mode:proc, cl:[[]]-[['.']]]).

s([mode:gen,
   cl:[[['#minimize', '{', [['@'(1, P), X], ':', C3], '}'], '.']|C1]-[C1],
   ante:A1-A1,
   tree:[s(17), [WCst, ['Minimizing']], RC, VP]]) -->
  wcst([mode:gen, wform:['Minimizing'], tree:WCst]), 
  rc([mode:gen, ct:wcst, num:N, wgt:1, arg:X, cl:[C3]-C2, ante:A1-A2, tree:RC]),
  vp([mode:gen, ct:wcst, crd:'-', num:N, lev:P, arg:X, cl:C2-[[]], ante:A2-A3, tree:VP]),
  fs([mode:gen, cl:[['.']]-[[]]]).

rc([mode:M, ct:wcst, num:N, wgt:W, arg:X, cl:C1-C3, ante:A1-A3, tree:[rc(2), RPN, NP, VP]]) -->
  rpn([mode:M, wform:[that], tree:RPN]),
  np([mode:M, ct:wcst, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:NP]),
  vp([mode:M, ct:wcst, crd:'-', num:N, wgt:W, arg:X, cl:C2-C3, ante:A2-A3, tree:VP]).

np([mode:M, ct:wcst, crd:'-', fcn:subj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[np(28), Det, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]).

vp([mode:M, ct:wcst, crd:'-', num:N, wgt:1, arg:X, cl:C, ante:A-A, tree:[vp(13), Cop, Adj]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   adj([mode:M, arg:X, cl:C, ante:_A1-_A2, tree:Adj]).


% -------------------------------------------------------------------
% Minimizing the star per cost of an accommodation 
% has the medium priority 2.
% -------------------------------------------------------------------				

s([mode:proc,
   cl:[C1]-[['.', ['}', [C3, ':', [X, '@'(W, P)]], '{', '#minimize']|C1]], 
   ante:A1-A1,
   tree:[s(18), WCst, RC, VP]]) -->
  wcst([mode:proc, wform:['Minimizing'], tree:WCst]),  
  np([mode:proc, ct:wcst, def:_D, num:N, wgt:W, arg:X, cl:[[]]-C2, ante:A1-A2, tree:RC]),
  vp([mode:proc, ct:wcst, crd:'-', num:N, lev:P, arg:X, cl:C2-[C3], ante:A2-A3, tree:VP]),
  fs([mode:proc, cl:[[]]-[['.']]]).


s([mode:gen,
   cl:[[['#minimize', '{', [['@'(W, P), X], ':', C3], '}'], '.']|C1]-[C1],
   ante:A1-A1,
   tree:[s(19), WCst, RC, VP]]) -->
  wcst([mode:gen, wform:['Minimizing'], tree:WCst]), 
  np([mode:gen, ct:wcst, def:_D, num:N, wgt:W, arg:X, cl:[C3]-C2, ante:A1-A2, tree:RC]),
  vp([mode:gen, ct:wcst, crd:'-', num:N, lev:P, arg:X, cl:C2-[[]], ante:A2-A3, tree:VP]),
  fs([mode:gen, cl:[['.']]-[[]]]).


% -------------------------------------------------------------------
% Maximizing the star rating of an accommodation has the low priority 1.
% -------------------------------------------------------------------

s([mode:proc,
   cl:[C1]-[['.', ['}', [C3, ':', [X, '@'(W, P)]], '{', '#maximize']|C1]], 
   ante:A1-A1,
   tree:[s(20), WCst, RC, VP]]) -->
  wcst([mode:proc, wform:['Maximizing'], tree:WCst]), 
  np([mode:proc, ct:wcst, def:_D, num:N, wgt:W, arg:X, cl:[[]]-C2, ante:A1-A2, tree:RC]),
  vp([mode:proc, ct:wcst, crd:'-', num:N, lev:P, arg:X, cl:C2-[C3], ante:A2-A3, tree:VP]),
  fs([mode:proc, cl:[[]]-[['.']]]).


s([mode:gen,
   cl:[[['#maximize', '{', [['@'(W, P), X], ':', C3], '}'], '.']|C1]-[C1],
   ante:A1-A1,
   tree:[s(21), WCst, RC, VP]]) -->
  wcst([mode:gen, wform:['Maximizing'], tree:WCst]), 
  np([mode:gen, ct:wcst, def:_D, num:N, wgt:W, arg:X, cl:[C3]-C2, ante:A1-A2, tree:RC]),
  vp([mode:gen, ct:wcst, crd:'-', num:N, lev:P, arg:X, cl:C2-[[]], ante:A2-A3, tree:VP]),
  fs([mode:gen, cl:[['.']]-[[]]]).


% -------------------------------------------------------------------
% Special NP rules for: Minimizing/Maximizing NP ...
% -------------------------------------------------------------------

np([mode:M, ct:wcst, def:_D, num:N, wgt:W, arg:Y, cl:C1-C5, ante:A1-A2, tree:[np(1), Det1, RNoun, Prep, Det2, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det1]),
   rnoun([mode:M, num:N, arg:W, arg:Y, cl:C2-C3, tree:RNoun]),
   prep([mode:M, wform:[of], tree:Prep]),
   det([mode:M, num:N, def:'-', cl:C3-C4, tree:Det2]),
   noun([mode:M, num:N, arg:Y, cl:C4-C5, ante:A1-A2, tree:Noun]).


% -------------------------------------------------------------------
% Special VP rules for: Minimizing/Maximizing
% -------------------------------------------------------------------

vp([mode:proc, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[C1|C2]-[[data_prop(L, pos_int(V), nominal), 
                 variable(L, 'H'),
	         class(L, priority), 
	         prop(L, high)|C1]|C2],
    ante:A2-A3, tree:[vp, WForm]]) -->
    { lexicon([cat:vp, wform:WForm, var:'H', lit:high]) },
    WForm.  


vp([mode:gen, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[[prop(L, high),
         class(L, priority),
         variable(L, 'H'),
         data_prop(L, pos_int(V), nominal)|C1]|C2]-[C1|C2],
    ante:A2-A3, tree:[vp, WForm]]) -->
    { lexicon([cat:vp, wform:WForm, var:'H', lit:high]) },
    WForm.  


vp([mode:proc, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[C1|C2]-[[data_prop(L, pos_int(V), nominal), 
                 variable(L, 'M'),
	         class(L, priority), 
	         prop(L, medium)|C1]|C2],
    ante:A2-A3, tree:[vp, WForm]]) -->
    { lexicon([cat:vp, wform:WForm, var:'M', lit:medium]) },
    WForm.  


vp([mode:gen, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[[prop(L, medium),
         class(L, priority),
         variable(L, 'M'),
         data_prop(L, pos_int(V), nominal)|C1]|C2]-[C1|C2],
    ante:A2-A3, tree:[vp, WForm]]) -->
    { lexicon([cat:vp, wform:WForm, var:'M', lit:medium]) },
    WForm.


vp([mode:proc, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[C1|C2]-[[data_prop(L, pos_int(V), nominal), 
                 variable(L, 'L'),
	         class(L, priority), 
	         prop(L, low)|C1]|C2],
    ante:A2-A3, tree:[vp, WForm]]) -->
    { lexicon([cat:vp, wform:WForm, var:'L', lit:low]) },
    WForm.


vp([mode:gen, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[[prop(L, low),
         class(L, priority),
         variable(L, 'L'),
         data_prop(L, pos_int(V), nominal)|C1]|C2]-[C1|C2],
    ante:A2-A3, tree:[vp, WForm]]) -->
   { lexicon([cat:vp, wform:WForm, var:'L', lit:low]) },
   WForm.


/*
vp([mode:proc, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[C1|C2]-[[data_prop(L, pos_int(V), nominal), 
                 variable(L, VName),
	         class(L, priority), 
	         prop(L, AName)|C1]|C2],
    ante:A2-A3, tree:VP],
   [has, the, AName, priority, VName, '.'], []).


vp([mode:gen, ct:wcst, crd:'-', num:N, lev:V, arg:X,
    cl:[[prop(L, AName),
         class(L, priority),
         variable(L, VName),
         data_prop(L, pos_int(V), nominal)|C1]|C2]-[C1|C2],
    ante:A2-A3, tree:VP],
   [has, the, AName, priority, VName, '.'], []).
*/    

% -------------------------------------------------------------------
% There is a student.
% -------------------------------------------------------------------

s([mode:M, cl:C1-C4, ante:A1-A3, tree:[s(22), Expl, NP]])
  -->
  expl([mode:M, ct:fact, num:N, cl:C1-C2, ante:A1-A2, tree:Expl]),
  np([mode:M, ct:fact, crd:'-', fcn:nsubj, def:'-', num:N, arg:X, cl:C2-C3, ante:A2-A3, tree:NP]),
  fs([mode:M, cl:C3-C4]).


np([mode:M, ct:fact, crd:'-', fcn:nsubj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[np(2), Det, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]).


% -------------------------------------------------------------------
% Bob is a student.
% -------------------------------------------------------------------

s([mode:M, cl:C1-C4, ante:A1-A3, tree:[s(22), PName, VP]]) --> 
   pname([mode:M, ct:fact, fcn:subj, def:'+', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),              
   vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C2-C3, ante:A2-A3, tree:VP]),
   fs([mode:M, cl:C3-C4]).


% -------------------------------------------------------------------
% John does not work.
% John works.
% John does not invite Mary.
% John invites Mary.
% -------------------------------------------------------------------
% Do we need def:D in subject position?

s([mode:M, cl:C1-C4, ante:A1-A3, tree:[s(23), NP, VP]]) --> 
   np([mode:M, ct:fact, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:NP]),
   vp([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C2-C3, ante:A2-A3, tree:VP]),
   fs([mode:M, cl:C3-C4]).


% ===================================================================
% Noun phrases: body
% ===================================================================

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, head:H, cl:C, ante:A, tree:[np(3), Qnt, Noun]]) -->
   qnt([mode:M, num:N, head:H, body:B, cl:C, tree:Qnt]),
   noun([mode:M, num:N, arg:X, cl:B, ante:A, tree:Noun]).

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, head:H, cl:C, ante:A1-A3, tree:[np(4), Qnt, Noun, RC]]) -->
   qnt([mode:M, num:N, head:H, body:B1-B3, cl:C, tree:Qnt]),
   noun([mode:M, num:N, arg:X, cl:B1-B2, ante:A1-A2, tree:Noun]),
   rc([mode:M, ct:body, num:N, arg:X, cl:B2-B3, ante:A2-A3, tree:RC]).

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(5), Det, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   { anaphora_resolution([def, M, X, C1, C3, C4, A1, A2, A3]) }.

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C3, ante:A, tree:[np(6), Det, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A, tree:Noun]).

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(7), Det, Noun, Var]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   var([mode:M, arg:X, cl:C3-C4, ante:A2-A3, tree:Var]),
   { anaphora_resolution([def, M, X, C1, C4, C5, A1, A3, A4]) }.

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(8), Det, Noun, Var]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   var([mode:M, arg:X, cl:C3-C4, ante:A2-A3, tree:Var]).

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(9), Det, Noun, RC]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   rc([mode:M, ct:body, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:RC]).

np([mode:M, ct:body, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(10), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.          

% --------------------------------------------------------------------

np([mode:M, ct:body, crd:'-', fcn:obj, def:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(11), Det, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   { anaphora_resolution([def, M, X, C1, C3, C4, A1, A2, A3]) }.

np([mode:M, ct:body, crd:'-', fcn:obj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[np(12), Det, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]).

np([mode:M, ct:body, crd:'-', fcn:obj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(13), NVar, Noun]]) -->
   nvar([mode:M, num:N, type:cardinal, arg:X, cl:C1-C2, ante:A1-A2, tree:NVar]),
   noun([mode:M, num:pl, arg:X, cl:C2-C3, ante:A2-A3, tree:Noun]).
  
np([mode:M, ct:body, crd:'-', fcn:obj, def:'+', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(14), Det, Noun, Var]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   var([mode:M, arg:X, cl:C3-C4, ante:A2-A3, tree:Var]),
   { anaphora_resolution([def, M, X, C1, C4, C5, A1, A3, A4]) }.

np([mode:M, ct:body, crd:'-', fcn:obj, def:'-', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(15), Det, Noun, Var]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   var([mode:M, arg:X, cl:C3-C4, ante:A2-A3, tree:Var]).

np([mode:M, ct:body, crd:'-', fcn:obj, def:'-', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(16), Det, Noun, RC]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   rc([mode:M, ct:body, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:RC]).

np([mode:M, ct:body, crd:'-', fcn:obj, def:'+', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(17), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.     


% ===================================================================
% Noun phrases: head
% ===================================================================

np([mode:M, ct:head, crd:'-', fcn:subj, def:'+', num:_N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(18), Det, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   { anaphora_resolution([def, M,  X, C1, C3, C4, A1, A2, A3]) }.

np([mode:M, ct:head, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(19), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.    

np([mode:M, ct:head, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(20), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.    

np([mode:M, ct:head, crd:'-', fcn:subj, def:'+', num:N, arg:APred, cl:C1-C5, ante:A1-A1, tree:[np(21), NVar1, AOp, NVar2]]) -->
   nvar([mode:M, num:N, type:cardinal, arg:X1, cl:C1-C2, ante:A1-A2, tree:NVar1]),
   { anaphora_resolution([nvar, M, X1, C1, C2, C3, A1, A2, A3]) },
   aop([mode:M, arg:X1, arg:X2, lit:APred, tree:AOp]),
   nvar([mode:M, num:N, type:cardinal, arg:X2, cl:C3-C4, ante:_A1-_A2, tree:NVar2]),
   { anaphora_resolution([nvar, M, X2, C1, C4, C5, A1, _A2, _A3]) }.

np([mode:M, ct:head, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(22), NVar]]) -->
   nvar([mode:M, num:N, type:cardinal, arg:X, cl:C1-C2, ante:A1-A2, tree:NVar]),
   { anaphora_resolution([nvar, M, X, C1, C2, C3, A1, A2, A3]) }.


% --------------------------------------------------------------------

np([mode:M, ct:head, crd:'-', fcn:obj, def:'-', num:N, arg:X, lit:L, cl:Cl, ante:A1-A2, tree:[np(23), Det, Noun]]) -->
   cqnt([mode:M, num:N, lit:L, cond:Cd, cl:Cl, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:Cd, ante:A1-A2, tree:Noun]).

np([mode:M, ct:head, crd:'-', fcn:obj, def:'+', num:_N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(24), Det, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   { anaphora_resolution([def, M,  X, C1, C3, C4, A1, A2, A3]) }.

%% 2018-10-14: introduced robj to deal with "the"
np([mode:M, ct:head, crd:'-', fcn:robj, def:'+', num:N, arg:X, cl:C1-C6, ante:A1-A2, tree:[np(25), Det1, RNoun, Prep, Det2, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det1]),
   rnoun([mode:M, num:N, arg:X, arg:Y, cl:C2-C3, tree:RNoun]),
   prep([mode:M, wform:[of], tree:Prep]),
   det([mode:M, num:N, def:'+', cl:C3-C4, tree:Det2]),
   noun([mode:M, num:N, arg:Y, cl:C4-C5, ante:A1-A2, tree:Noun]),
   { anaphora_resolution([def, M, Y, C4, C5, C6, A1, A2, A3]) }.

%% 2018-10-14: added nsubj
np([mode:M, ct:head, crd:'-', fcn:nsubj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[np(26), Det, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]).

np([mode:M, ct:head, crd:'-', fcn:obj, def:'+', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(27), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.    

np([mode:M, ct:head, crd:'+', fcn:obj, def:D, num:N, arg:X, cl:C, ante:A1-A2, tree:[np(28), NP]]) -->
   np([mode:M, ct:head, crd:'-', fcn:obj, def:D, num:N, arg:X, cl:C, ante:A1-A2, tree:NP]).

np([mode:M, ct:head, crd:'+', fcn:obj, def:D, num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(29), NP1, CRD, NP2]]) -->
   np([mode:M, ct:head, crd:'-', fcn:obj, def:D, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:NP1]),
   crd([mode:M, wform:[or], cl:C2-C3, tree:CRD]),
   np([mode:M, ct:head, crd:'+', fcn:obj, def:D, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:NP2]).


% ===================================================================
% Noun phrases: fact
% ===================================================================

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(30), Det, Noun, Num]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   number([mode:M, arg:X, cl:C3-C4, ante:A2-A3, tree:Num]),
   { anaphora_resolution([number, M, subj, X, C1, C4, C5, A1, A3, A4]) }.

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(31), Det, Noun, PName]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   pname([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:PName]),
   { anaphora_resolution([def, M, X, C1, C4, C5, A1, A3, A4]) }.

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'-', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(32), Det, Noun, PName]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   pname([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:PName]).

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C6, ante:A1-A5, tree:[np(33), Det, Noun, PName, RC]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   pname([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:PName]),
   { anaphora_resolution([def, M, X, C1, C4, C5, A1, A3, A4]) },
   rc([mode:M, ct:fact, num:N, arg:X, cl:C5-C6, ante:A4-A5, tree:RC]).

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'-', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(34), Det, Noun, PName, RC]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   pname([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:PName]),
   rc([mode:M, ct:fact, num:N, arg:X, cl:C4-C5, ante:A3-A4, tree:RC]).


% -------------------------------------------------------------------

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(35), Det, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   { anaphora_resolution([def, M, X, C1, C3, C4, A1, A2, A3]) }.

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[np(36), Det, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]).

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(37), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.     

% Added: 2018-10-14 (special goes with expl)
np([mode:M, ct:fact, crd:'-', fcn:nsubj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[np(38), Det, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]).

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(39), Det, NAdj, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   nadj([mode:M, num:N, type:ordinal, arg:X, cl:C2-C3, ante:A1-A2, tree:NAdj]),
   noun([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:Noun]),
   { anaphora_resolution([def, M, X, C1, C4, C5, A1, A3, A4]) }.

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'-', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(40), Det, NAdj, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   nadj([mode:M, num:N, type:ordinal, arg:X, cl:C2-C3, ante:A1-A2, tree:NAdj]),
   noun([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:Noun]).

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'+', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(41), Det, Adj, Noun]]) -->
   det([mode:M, num:N, def:'+', cl:C1-C2, tree:Det]),
   adj([mode:M, arg:X, cl:C2-C3, ante:A1-A2, tree:Adj]),
   noun([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:Noun]),
   { anaphora_resolution([def, M, X, C1, C4, C5, A1, A3, A4]) }.

np([mode:M, ct:fact, crd:'-', fcn:subj, def:'-', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(42), Det, Adj, Noun]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   adj([mode:M, arg:X, cl:C2-C3, ante:A1-A2, tree:Adj]),
   noun([mode:M, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:Noun]).

np([mode:M, ct:fact, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(43), Det, Noun, RC]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   rc([mode:M, ct:fact, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:RC]).


% --------------------------------------------------------------------

np([mode:proc, ct:fact, crd:'-', fcn:obj, def:'+', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(44), Det, Noun, Num]]) -->
   det([mode:proc, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:proc, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   number([mode:proc, arg:X, cl:C3-C4, ante:A2-A3, tree:Num]),
   { anaphora_resolution([number, proc, obj, X, C1, C4, C5, A1, A3, A4]) }.

np([mode:gen, ct:fact, crd:'-', fcn:obj, def:'+', num:N, arg:X, cl:C1-C5, ante:A1-A4, tree:[np(45), Det, Noun, Num]]) -->
   det([mode:gen, num:N, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:gen, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   number([mode:gen, arg:X, cl:C3-C4, ante:A2-A3, tree:Num]),
   { anaphora_resolution([number, gen, obj, X, C1, C4, C5, A1, A3, A4]) }.  

np([mode:M, ct:fact, crd:'-', fcn:obj, def:'+', num:sg, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(46), Det, Noun]]) -->
   det([mode:M, num:sg, def:'+', cl:C1-C2, tree:Det]),
   noun([mode:M, num:sg, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   { anaphora_resolution([def, M, X, C1, C3, C4, A1, A2, A3]) }.

np([mode:M, ct:fact, crd:'-', fcn:obj, def:'-', num:sg, arg:X, cl:C1-C3, ante:A1-A2, tree:[np(47), Det, Noun]]) -->
   det([mode:M, num:sg, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:sg, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]).

np([mode:M, ct:fact, crd:'-', fcn:obj, def:'+', num:sg, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(48), PName]]) -->
   pname([mode:M, num:sg, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.


%% -------------------------------------------------------

%% 2018-09-05 -- def:'+' ???
np([mode:M, ct:fact, crd:'-', fcn:obj, def:'+', num:N, arg:X, cl:C1-C4, ante:A1-A4, tree:[np(49), NAdj, Noun]]) -->
   nadj([mode:M, num:N, type:cardinal, arg:X, cl:C1-C2, ante:A1-A2, tree:NAdj]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A2-A3, tree:Noun]),
   { anaphora_resolution([def, M, X, C1, C3, C4, A1, A3, A4]) }.

np([mode:M, ct:fact, crd:'-', fcn:obj, def:'-', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[np(50), NAdj, Noun]]) -->
   nadj([mode:M, num:N, type:cardinal, arg:X, cl:C1-C2, ante:A1-A2, tree:NAdj]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A2-A3, tree:Noun]).

np([mode:M, ct:fact, crd:'-', fcn:obj, def:_D, num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[np(51), Det, Noun, RC]]) -->
   det([mode:M, num:N, def:'-', cl:C1-C2, tree:Det]),
   noun([mode:M, num:N, arg:X, cl:C2-C3, ante:A1-A2, tree:Noun]),
   rc([mode:M, ct:fact, num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:RC]).


% ===================================================================
% Proper name
% ===================================================================

% Special case
pname([mode:M, ct:fact, fcn:subj, def:'+', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[pname(1), PName]]) -->         
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]),
   { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.


% ===================================================================
% Grammar rules for questions
% ===================================================================

np([mode:M, ct:query, crd:'-', fcn:subj, def:_D, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:[np(q1), PName]]) -->
   pname([mode:M, num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:PName]).
   % { anaphora_resolution([pname, M, X, C1, C2, C3, A1, A2, A3]) }.      

vp([mode:M, ct:query, crd:'-', inv:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(q1), Cop, Adj]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   adj([mode:M, arg:X, cl:C, ante:_A1-_A2, tree:Adj]).

vp([mode:M, ct:query, crd:'-', inv:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(q2), AuxNeg, IV]])  -->
   aux_neg([mode:M, num:N, vform:fin, scope:S, cl:C, tree:AuxNeg]), 
   iv([mode:M, num:N, vform:bse, arg:X, cl:S, tree:IV]).

vp([mode:M, ct:query, crd:'-', inv:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(q3), IV]]) -->
   iv([mode:M, num:N, vform:fin, arg:X, cl:C, tree:IV]).

vp([mode:M, ct:query, crd:'-', inv:'+', num:_N, arg:X, arg:Y, cl:C1-C2, ante:A-A, tree:[vp(q4), TV]])  -->
   tv([mode:M, num:_N, vform:bse, arg:X, arg:Y, cl:C1-C2, tree:TV]).


% ===================================================================
% Verb phrase: body
% ===================================================================

vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(2), AuxNeg, IV]])  -->
   aux_neg([mode:M, num:N, vform:fin, scope:S, cl:C, tree:AuxNeg]), 
   iv([mode:M, num:N, vform:bse, arg:X, cl:S, tree:IV]).

vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(3), IV]]) -->
   iv([mode:M, num:N, vform:fin, arg:X, cl:C, tree:IV]).

vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(4), AuxNeg, TV, NP]])  -->
   aux_neg([mode:M, num:N, vform:fin, scope:S1-S2, cl:C1-C2, tree:AuxNeg]),
   tv([mode:M, num:N, vform:bse, arg:X, arg:Y, cl:S1-S2, tree:TV]),
   np([mode:M, ct:body, crd:'-', fcn:obj, def:_, num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(5), TV, NP]])  -->
   tv([mode:M, num:N, vform:fin, arg:X, arg:Y, cl:C1-C2, tree:TV]),
   np([mode:M, ct:body, crd:'-', fcn:obj, def:_, num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(6), CopNeg, RAdj, NP]])  -->
   cop_neg([mode:M, num:N, vform:fin, scope:S1-S2, cl:C1-C2, tree:AuxNeg]),
   radj([mode:M, arg:X, arg:Y, cl:S1-S2, tree:RAdj]),
   np([mode:M, ct:body, crd:'-', fcn:obj, def:_, num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(7), Cop, RAdj, NP]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   radj([mode:M, arg:X, arg:Y, cl:C1-C2, tree:RAdj]),
   np([mode:M, ct:body, crd:'-', fcn:obj, def:_, num:N, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:body, crd:'+', num:N, arg:X, cl:C, ante:A, tree:[vp(8), VP]]) -->
   vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C, ante:A, tree:VP]).

vp([mode:M, ct:body, crd:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[vp(9), VP1, CRD, VP2]]) -->
   vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:VP1]),
   crd([mode:M, wform:[and], cl:C2-C3, tree:CRD]),
   vp([mode:M, ct:body, crd:'+', num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:VP2]).


% ===================================================================
% Verb phrase: head
% ===================================================================

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C1-C2, ante:A-A, tree:[vp(10), AuxNeg, IV]])  -->
   aux_neg([mode:M, num:N, vform:fin, scope:S1-S2, cl:C1-C2, tree:AuxNeg]), 
   iv([mode:M, num:_, vform:bse, arg:X, cl:S1-S2, tree:IV]).

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C1-C2, ante:A-A, tree:[vp(11), IV]])  -->
   iv([mode:M, num:N, vform:fin, arg:X, cl:C1-C2, tree:IV]).

% 2018-10-14
vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C, ante:A1-A2, tree:[vp(12), Cop, NP]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   np([mode:M, ct:head, crd:'-', fcn:robj, def:'+', num:_, arg:X, cl:C, ante:A1-A2, tree:NP]).

% 2018-10-14
vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[vp(12), Cop, NP]])  -->
   cop([mode:M, num:N, vform:fin, arg:X, arg:Y, cl:C1-C2, ante:A1-A2, tree:Cop]),
   np([mode:M, ct:head, crd:'-', fcn:nsubj, def:'-', num:_, arg:Y, cl:C2-C3, ante:A2-A3, tree:NP]).

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(13), Cop, Adj]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   adj([mode:M, arg:X, cl:C, ante:_A1-_A2, tree:Adj]).

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C, ante:A1-A2, tree:[vp(14), Cop, NP]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   np([mode:M, ct:head, crd:'+', fcn:obj, def:'-', num:N, arg:X, cl:C, ante:A1-A2, tree:NP]).

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(15), CopNeg, RAdj, NP]])  -->
   cop_neg([mode:M, num:N, vform:fin, scope:S1-S2, cl:C1-C2, tree:AuxNeg]),
   radj([mode:M, arg:X, arg:Y, cl:S1-S2, tree:RAdj]),
   np([mode:M, ct:head, crd:'_', fcn:obj, def:'+', num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(16), Cop, RAdj, NP]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   radj([mode:M, arg:X, arg:Y, cl:C1-C2, tree:RAdj]),
   np([mode:M, ct:head, crd:'-', fcn:obj, def:'+', num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C, ante:A1-A2, tree:[vp(17), Cop, RAdj, NP]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   radj([mode:M, arg:X, arg:Y, cl:L, tree:RAdj]),
   np([mode:M, ct:head, crd:'-', fcn:obj, def:_D, num:_, arg:Y, lit:L, cl:C, ante:A1-A2, tree:NP]). 

vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C, ante:A1-A2, tree:[vp(18), TV, NP]])  -->
   tv([mode:M, num:N, vform:fin, arg:X, arg:Y, cl:L, tree:TV]),
   np([mode:M, ct:head, crd:'-', fcn:obj, def:_D, num:_, arg:Y, lit:L, cl:C, ante:A1-A2, tree:NP]).

%% Added: 2018-10-14
vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(181), TV, NP]])  -->
   tv([mode:M, num:N, vform:fin, arg:X, arg:Y, cl:C1-C2, tree:TV]),
   np([mode:M, ct:head, crd:'-', fcn:obj, def:'+', num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]). 

vp([mode:M, ct:head, crd:'+', num:N, arg:X, cl:C1-C2, ante:A, tree:[vp(19), VP]]) -->
   vp([mode:M, ct:head, crd:'-', num:N, arg:X, cl:C1-C2, ante:A, tree:VP]).

vp([mode:proc, ct:head, crd:'+', num:N, arg:X, cl:[[]|C1]-[[C4, OP, C2]|C5], ante:A1-A3, tree:[vp(20), VP1, CRD, VP2]]) -->
   vp([mode:proc, ct:head, crd:'-', num:N,  arg:X, cl:[[]|C1]-[C2|C3], ante:A1-A2, tree:VP1]),
   crd([mode:proc, wform:[or], cl:[[]|C3]-[[OP]|C3], tree:CRD]),
   vp([mode:proc, ct:head, crd:'+', num:N, arg:X, cl:[[]|C3]-[C4|C5], ante:A2-A3, tree:VP2]).

vp([mode:gen, ct:head, crd:'+', num:N, arg:X, cl:[[C4, OP, C2]|C5]-[[]|C1], ante:A1-A3, tree:[vp(21), VP1, CRD, VP2]]) -->
   vp([mode:gen, ct:head, crd:'-', num:N,  arg:X, cl:[C4|C5]-[[]|C3], ante:A1-A2, tree:VP1]),
   crd([mode:gen, wform:[or], cl:[[OP]|C3]-[[]|C3], tree:CRD]),
   vp([mode:gen, ct:head, crd:'+', num:N, arg:X, cl:[C2|C3]-[[]|C1], ante:A2-A3, tree:VP2]).


% ===================================================================
% Verb phrase: fact
% ===================================================================

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C2, ante:A-A, tree:[vp(22), AuxNeg, IV]])  -->
   aux_neg([mode:M, num:N, vform:fin, scope:S1-S2, cl:C1-C2, tree:AuxNeg]), 
   iv([mode:M, num:N, vform:bse, arg:X, cl:S1-S2, tree:IV]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(23), IV]])  -->
   iv([mode:M, num:N, vform:fin, arg:X, cl:C, tree:IV]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:[vp(24), Cop, NAdj]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   nadj([mode:M, num:_, type:nom, arg:X, cl:C1-C2, ante:A1-A2, tree:NAdj]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C, ante:A-A, tree:[vp(241), Cop, Adj]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   adj([mode:M, arg:X, cl:C, ante:_A1-_A2, tree:Adj]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A3, tree:[vp(25), Cop, NP]])  -->
   cop([mode:M, num:N, vform:fin, arg:X, arg:Y, cl:C1-C2, ante:A1-A2, tree:Cop]),
   np([mode:M, ct:fact, crd:'-', fcn:obj, def:'-', num:_, arg:Y, cl:C2-C3, ante:A2-A3, tree:NP]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(26), AuxNeg, TV, NP]])  -->
   aux_neg([mode:M, num:N, vform:fin, scope:S1-S2, cl:C1-C2, tree:AuxNeg]),
   tv([mode:M, num:N, vform:bse, arg:X, arg:Y, cl:S1-S2, tree:TV]),
   np([mode:M, ct:fact, crd:'-', fcn:obj, def:_, num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(27), TV, NP]])  -->
   tv([mode:M, num:N, vform:fin, arg:X, arg:Y, cl:C1-C2, tree:TV]),
   np([mode:M, ct:fact, crd:'-', fcn:obj, def:_, num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(28), CopNeg, RAdj, NP]])  -->
   cop_neg([mode:M, num:N, vform:fin, scope:S1-S2, cl:C1-C2, tree:AuxNeg]),
   radj([mode:M, arg:X, arg:Y, cl:S1-S2, tree:RAdj]),
   np([mode:M, ct:fact, crd:'-', fcn:obj, def:_, num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(29), Cop, RAdj, NP]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   radj([mode:M, arg:X, arg:Y, cl:C1-C2, tree:RAdj]),
   np([mode:M, ct:fact, crd:'-', fcn:obj, def:_, num:_, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

% ----------------


vpi([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C3, ante:A1-A2, tree:[vp(30), Cop, RAdj, NP]])  -->
   cop([mode:M, num:N, vform:fin, tree:Cop]),
   radj([mode:M, arg:X, arg:Y, cl:C1-C2, tree:RAdj]),
   np([mode:M, ct:fact, crd:'-', fcn:obj, def:'+', num:pl, arg:Y, cl:C2-C3, ante:A1-A2, tree:NP]).

vp([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[vp(31), VP1, CRD, VP2]]) -->
   vpi([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:VP1]),
   crd([mode:M, wform:[and], cl:C2-C3, tree:CRD]),
   vpc([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:VP2]).

vp([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[vp(32), VP1, CRD, VP2]]) -->
   vpi([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:VP1]),
   crd([mode:M, wform:[','], cl:C2-C3, tree:CRD]),
   vpc([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:VP2]).


% ----------------

vpc([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[vp(33), VP1, CRD, VP2]]) -->
   vpc([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:VP1]),
   crd([mode:M, wform:[and], cl:C2-C3, tree:CRD]),
   vpc([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:VP2]).

vpc([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[vp(34), VP1, CRD, VP2]]) -->
   vpc([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:VP1]),
   crd([mode:M, wform:[','], cl:C2-C3, tree:CRD]),
   vpc([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:VP2]).

vpc([mode:proc, ct:fact, crd:_, num:N, arg:X0, cl:C1-C3, ante:A1-A3, tree:[vp(35), Num]]) -->
   number([mode:proc, arg:X, cl:C1-C2, ante:A1-A2, tree:Num]),
   { anaphora_resolution([number, proc, obj, X0, C1, C2, C3, A1, A2, A3]) }.

vpc([mode:gen, ct:fact, crd:_, num:N, arg:X0, cl:[[P, class(X, ClassName)|C1]|Sup]-C2, ante:A1-A3, tree:[vp(36), Num]]) -->
   number([mode:gen, arg:X, cl:[C1|Sup]-C2, ante:A1-A2, tree:Num]),
   { anaphora_resolution([number, gen, obj, X, [[class(X, ClassName)|C1]|Sup], C2, C3, A1, A2, A3]) }.


% ----------------

vp([mode:proc, ct:fact, crd:_, num:N, arg:X0, cl:C1-C3, ante:A1-A3, tree:[vp(37), Num]]) -->
   number([mode:proc, arg:X, cl:C1-C2, ante:A1-A2, tree:Num]),
   { anaphora_resolution([number, proc, obj, X0, C1, C2, C3, A1, A2, A3]) }.


% ----------------

vp([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C, ante:A, tree:[vp(38), VP]]) -->
   vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C, ante:A, tree:VP]).

vp([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C1-C4, ante:A1-A3, tree:[vp(39), VP1, CRD, VP2]]) -->
   vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C1-C2, ante:A1-A2, tree:VP1]),
   crd([mode:M, wform:[and], cl:C2-C3, tree:CRD]),
   vp([mode:M, ct:fact, crd:'+', num:N, arg:X, cl:C3-C4, ante:A2-A3, tree:VP2]).


% ===================================================================
% Relative clauses: body + fact
% ===================================================================

rc([mode:M, ct:body, num:N, arg:X, cl:C, ante:A1-A2, tree:[rc(1), RPN, VP]]) -->
  rpn([mode:M, wform:_R, tree:RPN]),
  vp([mode:M, ct:body, crd:'-', num:N, arg:X, cl:C, ante:A1-A2, tree:VP]).

rc([mode:M, ct:fact, num:N, arg:X, cl:C, ante:A1-A2, tree:[rc(2), RPN, VP]]) -->
  rpn([mode:M, wform:_R, tree:RPN]),
  vp([mode:M, ct:fact, crd:'-', num:N, arg:X, cl:C, ante:A1-A2, tree:VP]).


% ===================================================================
% Preterminal rules
% ===================================================================

% -------------------------------------------------------------------
% Adjective
% -------------------------------------------------------------------

adj([mode:proc, arg:X, cl:[In|Sup]-[[Prop|In]|Sup], ante:[A1|A2]-[[Prop|A1]|A2], tree:[adj(1), WForm]]) -->
   { lexicon([cat:adj, wform:WForm, arg:X, lit:Prop]) },
   WForm.

adj([mode:gen, arg:X, cl:[[Prop|Out]|Sup]-[Out|Sup], ante:[A1|A2]-[[Prop|A1]|A2], tree:[adj(2), WForm]]) -->
   { lexicon([cat:adj, wform:WForm, arg:X, lit:Prop]) },
   WForm.


% -------------------------------------------------------------------
% Numerical Adjective
% -------------------------------------------------------------------

nadj([mode:proc, num:N, type:T, arg:X, cl:[In|Sup]-[[DataProp|In]|Sup], ante:[A1|A2]-[[DataProp|A1]|A2], tree:[nadj(1), [WForm]]]) -->
   [WForm],
   { lexicon([cat:nadj, wform:[WForm], num:N, type:T, arg:X, lit:DataProp]) }.


nadj([mode:gen, num:N, type:T, arg:X, cl:[[DataProp|Out]|Sup]-[Out|Sup], ante:[A1|A2]-[[DataProp|A1]|A2], tree:[nadj(2), [WForm]]]) -->
   [WForm],
   { lexicon([cat:nadj, wform:[WForm], num:N, type:T, arg:X, lit:DataProp]) }.

% -------------------------------------------------------------------
% Relational Adjective
% -------------------------------------------------------------------

radj([mode:proc, arg:X, arg:Y, cl:[C1|C2]-[[Prop|C1]|C2], tree:[radj(1), WForm]]) -->
   { lexicon([cat:radj, wform:WForm, arg:X, arg:Y, lit:Prop]) },
   WForm.

radj([mode:gen, arg:X, arg:Y, cl:[[Prop|C1]|C2]-[C1|C2], tree:[radj(2), WForm]]) -->
   {  lexicon([cat:radj, wform:WForm, arg:X, arg:Y, lit:Prop]) },
   WForm.


% -------------------------------------------------------------------
% Adverb
% -------------------------------------------------------------------

adv([mode:M, wform:WForm, tree:[adv(1), WForm]]) -->
  { lexicon([cat:adv, wform:WForm]) },
  WForm.


% -------------------------------------------------------------------
% Arithmetic operator
% -------------------------------------------------------------------

aop([mode:proc, arg:X1, arg:X2, lit:APred, tree:[arop(1), [AOp]]]) -->
   [AOp],
   { lexicon([cat:aop, wform:[AOp], arg:X1, arg:X2, lit:APred]) }.

aop([mode:gen, arg:X1, arg:X2, lit:APred, tree:[arop(2), [AOp]]]) -->
   [AOp],
   { lexicon([cat:aop, wform:[AOp], arg:X1, arg:X2, lit:APred]) }.


% -------------------------------------------------------------------
% Auxilary 
% -------------------------------------------------------------------

aux([mode:M, num:N, vform:V, tree:[aux(1), WForm]]) -->
   { lexicon([cat:aux, wform:WForm, num:N, vform:V]) },   
   WForm.

aux_neg([mode:proc, num:N, vform:V, scope:[[]|C1]-[C2, C3|C4], cl:C1-[[C2, '-'|C3]|C4], tree:[aux_neg(1), WForm]])  -->
   { lexicon([cat:aux_neg, wform:WForm, num:N, vform:V]) },
   WForm.

aux_neg([mode:gen, num:N, vform:V, scope:[C2, C3|C4]-[[]|C1], cl:[['-', C2|C3]|C4]-[C3|C4], tree:[aux_neg(2), WForm]])  -->
   { lexicon([cat:aux_neg, wform:WForm, num:N, vform:V]) },
   WForm.


% -------------------------------------------------------------------
% Copula
% -------------------------------------------------------------------

cop([mode:proc, num:N, vform:V, arg:X, arg:Y, cl:[C1|C2]-[[Pred|C1]|C2], ante:[A1|A2]-[[Pred|A1]|A2], tree:[cop(1), WForm]]) -->
   { lexicon([cat:cop, wform:WForm, num:N, vform:V, arg:X, arg:Y, lit:Pred]) },
   WForm.
  
cop([mode:gen, num:N, vform:V, arg:X, arg:Y, cl:[[Pred|C1]|C2]-[C1|C2], ante:[A1|A2]-[[Pred|A1]|A2], tree:[cop(2), WForm]]) -->
   { lexicon([cat:cop, wform:WForm, num:N, vform:V, arg:X, arg:Y, lit:Pred]) },
   WForm.


% -------------------------------------------------------------------

cop([mode:M, num:N, vform:V, tree:[cop(3), WForm]]) -->
   { lexicon([cat:cop, wform:WForm, num:N, vform:V]) },   
   WForm.


% -------------------------------------------------------------------

cop_neg([mode:proc, num:N, vform:V, scope:[[]|C1]-[C2, C3|C4], cl:C1-[[C2, '-'|C3]|C4], tree:[cop_neg(1), WForm]])  -->
   { lexicon([cat:cop_neg, wform:WForm, num:N, vform:V]) },
   WForm.

cop_neg([mode:gen, num:N, vform:V, scope:[C2, C3|C4]-[[]|C1], cl:[['-', C2|C3]|C4]-[C3|C4], tree:[cop_neg(2), WForm]])  -->
   { lexicon([cat:cop_neg, wform:WForm, num:N, vform:V]) },
   WForm.


% -------------------------------------------------------------------
% Constraint
% -------------------------------------------------------------------

cst([mode:proc, scope:[[]|Sup1]-[Body, Lits|Sup2], cl:Sup1-[[Body, '-:'|Lits]|Sup2], tree:[cst(1), WForm]]) -->
   { lexicon([cat:cst, wform:WForm]) },
   WForm.

cst([mode:gen, scope:[Body|Sup1]-[[]|Sup1], cl:[[':-', Body|Rest]|Sup1]-[Rest|Sup1], tree:[cst(2), WForm]]) -->
   { lexicon([cat:cst, wform:WForm]) },
   WForm.


% -------------------------------------------------------------------
% Weak constraint				
% -------------------------------------------------------------------

wcst([mode:proc, wform:WForm, tree:[wcst(3), WForm]]) -->  
   { lexicon([cat:wcst, wform:WForm]) },
   WForm.

wcst([mode:gen, wform:WForm, tree:[wcst(4), WForm]]) -->  
   { lexicon([cat:wcst, wform:WForm]) },
   WForm.


% -------------------------------------------------------------------
% Coordination
%--------------------------------------------------------------------

crd([mode:proc, wform:WForm, cl:[In|Sup]-[[';'|In]|Sup], tree:[crd(1), WForm]])  -->
   { lexicon([cat:crd, wform:WForm, op:';']) },
   WForm.

crd([mode:gen, wform:WForm, cl:[[';'|Out]|Sup]-[Out|Sup], tree:[crd(2), WForm]])  -->
   { lexicon([cat:crd, wform:WForm, op:';']) },
   WForm.

crd([mode:proc, wform:WForm, cl:C-C, tree:[crd(3), WForm]])  -->
   { lexicon([cat:crd, wform:WForm, op:na]) },
   WForm.

crd([mode:gen, wform:WForm, cl:C-C, tree:[crd(4), WForm]])  -->
   { lexicon([cat:crd, wform:WForm, op:na]) },
   WForm.


% -------------------------------------------------------------------
% Determiner
% -------------------------------------------------------------------

det([mode:M, num:N, def:'-', cl:C-C, tree:[det(1), WForm]]) -->  
   { lexicon([cat:det, wform:WForm, num:N, def:'-']) },
   WForm.

det([mode:M, num:N, def:'+', cl:C-C, tree:[det(2), WForm]]) -->  
   { lexicon([cat:det, wform:WForm, num:N, def:'+']) },
   WForm.


% -------------------------------------------------------------------
% Quantifier (cardinality quantifier)
% -------------------------------------------------------------------

cqnt([mode:proc, num:N, lit:[[]|Sup]-[Lit|Sup], cond:[[]|Sup]-[Conds|Sup], cl:[[]|Sup]-[[Head]|Sup], tree:[det(1), WForm]]) -->
   { lexicon([cat:cqnt, wform:WForm, mode:proc, num:N, lit:Lit, cond:Conds, cl:Head]) },
   WForm.

cqnt([mode:gen, num:N, lit:[Lit|Sup]-[[]|Sup], cond:[Conds|Sup]-[[]|Sup], cl:[Head|Sup]-[[]|Sup], tree:[det(2), WForm]]) -->
   { lexicon([cat:cqnt, wform:WForm, mode:gen, num:N, lit:Lit, cond:Conds, cl:Head]) },
   WForm.


% -------------------------------------------------------------------

qnt([mode:proc, num:N, head:[[]|C2]-[Head, Body, C3|C4], body:[[]|C1]-C2, cl:C1-[[Body, '-:', Head|C3]|C4], tree:[qnt(1), WForm]]) -->
   { lexicon([cat:qnt, wform:WForm, num:N]) },
   WForm.

qnt([mode:gen, num:N, head:C2-[[]|C1], body:[Body, Head, C3|C4]-[[]|C2], cl:[[Head, ':-', Body|C3]|C4]-C1, tree:[qnt(2), WForm]]) -->  
   { lexicon([cat:qnt, wform:WForm, num:N]) },
   WForm.


% -------------------------------------------------------------------
% Expletive there
% -------------------------------------------------------------------

expl([mode:proc, ct:fact, num:N, cl:C-C, ante:A-A, tree:[expl(1), WForm]]) -->
   { lexicon([cat:expl, wform:WForm, num:N]) },
   WForm.

expl([mode:gen, ct:fact, num:N, cl:C-C, ante:A-A, tree:[expl(2), WForm]]) -->
   { lexicon([cat:expl, wform:WForm, num:N]) },
   WForm.


% -------------------------------------------------------------------
% Noun
% -------------------------------------------------------------------

noun([mode:proc, num:N, arg:X, cl:[C1|C2]-[[Class|C1]|C2], ante:[A1|A2]-[[Class|A1]|A2], tree:[noun(1), WForm]]) -->
   { lexicon([cat:noun, wform:WForm, num:N, arg:X, lit:Class]) },
   WForm.

noun([mode:gen, num:N, arg:X, cl:[[Class|C1]|C2]-[C1|C2], ante:[A1|A2]-[[Class|A1]|A2], tree:[noun(2), WForm]]) -->
   { lexicon([cat:noun, wform:WForm, num:N, arg:X, lit:Class]) },
   WForm.


% -------------------------------------------------------------------
% Relational Noun				
% -------------------------------------------------------------------

rnoun([mode:proc, num:N, arg:X, arg:Y, cl:[C1|C2]-[[Rel|C1]|C2], tree:[rnoun(1), WForm]]) -->
   { lexicon([cat:rnoun, wform:WForm, num:N, arg:X, arg:Y, lit:Rel]) },
   WForm.

rnoun([mode:gen, num:N, arg:X, arg:Y, cl:[[Rel|C1]|C2]-[C1|C2], tree:[rnoun(2), WForm]]) -->
   { lexicon([cat:rnoun, wform:WForm, num:N, arg:X, arg:Y, lit:Rel]) },
   WForm.


% -------------------------------------------------------------------
% Noun Gerund				
% -------------------------------------------------------------------

noun([mode:proc, wform:WForm, nform:gerund, tree:[noun(3), WForm]]) -->  
   { lexicon([cat:noun, wform:WForm, nform:gerund]) },
   WForm.

noun([mode:gen, wform:WForm, nform:gerund, tree:[noun(4), WForm]]) -->  
   { lexicon([cat:noun, wform:WForm, nform:gerund]) },
   WForm.


% -------------------------------------------------------------------
% Proper Name
% -------------------------------------------------------------------

pname([mode:proc, num:N, arg:X, cl:[C1|C2]-[[Name|C1]|C2], ante:[A1|A2]-[[Name|A1]|A2], tree:[pname(1), WForm]]) -->
   { lexicon([cat:pname, wform:WForm, num:N, arg:X, lit:Name]) },
   WForm.

pname([mode:gen, num:_N, arg:X, cl:[[Name|Out]|Sup]-[Out|Sup], ante:[A1|A2]-[[Name|A1]|A2], tree:[pname(2), WForm]]) -->
   { lexicon([cat:pname, wform:WForm, num:_N, arg:X, lit:Name]) },
   WForm.


% -------------------------------------------------------------------
% Verb: intransitive, transitive
% -------------------------------------------------------------------

iv([mode:proc, num:N, vform:V, arg:X, cl:[C1|C2]-[[Pred|C1]|C2], tree:[iv(1), WForm]]) -->
   { lexicon([cat:iv, wform:WForm, num:N, vform:V, arg:X, lit:Pred]) },
   WForm.

iv([mode:gen, num:N, vform:V, arg:X, cl:[[Pred|C1]|C2]-[C1|C2], tree:[iv(2), WForm]]) -->
   { lexicon([cat:iv, wform:WForm, num:N, vform:V, arg:X, lit:Pred]) },
   WForm.


% -------------------------------------------------------------------

tv([mode:proc, num:N, vform:V, arg:X, arg:Y, cl:[C1|C2]-[[Pred|C1]|C2], tree:[tv(1), WForm]]) -->
   { lexicon([cat:tv, wform:WForm, num:N, vform:V, arg:X, arg:Y, lit:Pred]) },
   WForm.

tv([mode:gen, num:N, vform:V, arg:X, arg:Y, cl:[[Pred|C1]|C2]-[C1|C2], tree:[tv(2), WForm]]) -->
   { lexicon([cat:tv, wform:WForm, num:N, vform:V, arg:X, arg:Y, lit:Pred]) },
   WForm.

% -------------------------------------------------------------------

tv([mode:proc, wform:WForm, vform:bse, tree:[tv(3), WForm]]) -->
   { lexicon([cat:tv, wform:WForm, vform:bse]) },
   WForm.

tv([mode:gen, wform:WForm, vform:bse, tree:[tv(4), WForm]]) -->
   { lexicon([cat:tv, wform:WForm, vform:bse]) },
   WForm.


% -------------------------------------------------------------------
% Number
% -------------------------------------------------------------------

number([mode:proc, arg:X, cl:[In|Sup]-[[Number|In]|Sup], ante:[A|As]-[[Number|A]|As], tree:[number(1), [Num]]]) -->
   [Num],
   { lexicon([cat:number, wform:[Num], arg:X, lit:Number]) }.

number([mode:gen, arg:X, cl:[[Number|In]|Sup]-[In|Sup], ante:[A|As]-[[Number|A]|As], tree:[number(2), [Num]]]) -->
   [Num],
   { lexicon([cat:number, wform:[Num], arg:X, lit:Number]) }.


% -------------------------------------------------------------------
% Preposition (special)
% -------------------------------------------------------------------

prep([mode:proc, wform:WForm, head:[[]|C2]-[Head, Body, C3|C4], body:[[]|C1]-C2, cl:C1-[[Body, '-:', Head|C3]|C4], tree:[qprep(1), WForm]]) -->
  { lexicon([cat:prep, wform:WForm]) },
  WForm.

prep([mode:gen, wform:WForm, head:C2-[[]|C1], body:[Body, Head, C3|C4]-[[]|C2], cl:[[Head, ':-', Body|C3]|C4]-C1, tree:[qprep(2), WForm]]) -->
  { lexicon([cat:prep, wform:WForm]) },
  WForm.


% -------------------------------------------------------------------

prep([mode:proc, wform:WForm, tree:[prep(3), WForm]]) -->
  { lexicon([cat:prep, wform:WForm]) },
  WForm.

prep([mode:gen, wform:WForm, tree:[prep(2), WForm]]) -->
  { lexicon([cat:prep, wform:WForm]) },
  WForm.


% -------------------------------------------------------------------
% Relative pronoun
% -------------------------------------------------------------------

rpn([mode:M, wform:WForm, tree:[rpn(1), WForm]]) -->
   { lexicon([cat:rpn, wform:WForm]) },   
   WForm.


% -------------------------------------------------------------------
% Variable
% -------------------------------------------------------------------

var([mode:proc, arg:X, cl:[In|Sup]-[[Var|In]|Sup], ante:[A|As]-[[Var|A]|As], tree:[var(1), [WForm]]]) -->
   [WForm],
   { lexicon([cat:var, wform:[WForm], arg:X, lit:Var]) }.

var([mode:gen, arg:X, cl:[[Var|Out]|Sup]-[Out|Sup], ante:[A|As]-[[Var|A]|As], tree:[var(2), [WForm]]]) -->
   [WForm],
   { lexicon([cat:var, wform:[WForm], arg:X, lit:Var]) }.


% -------------------------------------------------------------------
% Numerical variable
% -------------------------------------------------------------------

nvar([mode:proc, num:N, type:T, arg:X, cl:[In|Sup]-[[DP1, DP2|In]|Sup], ante:[A1|A2]-[[DP1, DP2|A1]|A2], tree:[nvar(1), [WForm]]]) -->
   [WForm],
   { lexicon([cat:nvar, wform:[WForm], num:N, type:T, arg:X, lit:[DP1, DP2]]) }.

nvar([mode:gen, num:N, type:T, arg:X, cl:[[DP2, DP1|Out]|Sup]-[Out|Sup], ante:[A1|A2]-[[DP2, DP1|A1]|A2], tree:[nvar(2), [WForm]]]) -->
   [WForm],
   { lexicon([cat:nvar, wform:[WForm], num:N, type:T, arg:X, lit:[DP1, DP2]]) }.


% -------------------------------------------------------------------
% Who question
% -------------------------------------------------------------------

wh([mode:proc, ct:query, wform:[WForm], tree:[wh, [WForm]]]) -->
   [WForm],
   { lexicon([cat:wh, wform:[WForm]]) }.


wh([mode:gen, ct:query, wform:[WForm], tree:[wh, [WForm]]]) --> 
   [WForm],
   { lexicon([cat:wh, wform:[WForm]]) }.

% -------------------------------------------------------------------
% Full stop
% -------------------------------------------------------------------

fs([mode:proc, cl:[In|Sup]-[[WForm|In]|Sup]]) -->
   [WForm],
   { lexicon([cat:fs, wform:[WForm]]) }.
   

fs([mode:gen, cl:[['.'|In]|Sup]-[In|Sup]]) -->
   [WForm],
   { lexicon([cat:fs, wform:[WForm]]) }.


% -------------------------------------------------------------------
% Question mark
% -------------------------------------------------------------------

qm([mode:proc, cl:[In|Sup]-[[WForm|In]|Sup]]) -->
   [WForm],
   { lexicon([cat:qm, wform:[WForm]]) }.

qm([mode:gen, cl:[[WForm|In]|Sup]-[In|Sup]]) -->  
   [WForm],
   { lexicon([cat:qm, wform:[WForm]]) }.