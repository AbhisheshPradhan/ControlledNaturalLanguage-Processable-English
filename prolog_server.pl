%===============================================================
% Project:  PENG ASP
% Module:   prolog_server.pl
% Author:   Rolf Schwitter
% Date:     2019-02-03
%===============================================================

% --------------------------------------------------------------
% Style checking
% --------------------------------------------------------------

:- no_style_check(singleton).
:- no_style_check(discontiguous).


%---------------------------------------------------------------
% Modules
%---------------------------------------------------------------

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_client')).
:- use_module(library('http/json')).
:- use_module(library('http/http_json')).
:- use_module(library('http/http_log')).
:- use_module(library('http/http_parameters')).
:- ensure_loaded('asp_main.pl').


%---------------------------------------------------------------
% set_setting/2
%
%   - set http logfile
%---------------------------------------------------------------

:- set_setting(http:logfile, 'log.txt').


%---------------------------------------------------------------
% server/1
%    - defines an http server at a specific port
%---------------------------------------------------------------

server(Port) :-
  http_server(http_dispatch, [port(Port)]).


%---------------------------------------------------------------
% http_handler/3
%
%   - General HTTP handler
%---------------------------------------------------------------

:- http_handler('/peng/', handle, []).

%---------------------------------------------------------------
% http_handler/3
%
%   - CSS handler
%---------------------------------------------------------------

:- http_handler('/src/css/styles.css', css_styles, []).

:- http_handler('/lib/bootstrap/bootstrap.css', css_bootstrap, []).

:- http_handler('/lib/bootstrap/bootstrap-toggle.css', css_bootstrap_toggle, []).

:- http_handler('/lib/superfish-master/dist/css/superfish.css', css_superfish, []).

:- http_handler('/lib/bootstrap/bootstrap-switch.min.css', css_bootstrap_switch, []).

%---------------------------------------------------------------
% http_handler/3
%
%   - JavaScript handler
%---------------------------------------------------------------

:- http_handler('/lib/jquery/jquery.min.js', js_jquery, []).

:- http_handler('/lib/jquery/jquery-ui.js', js_jquery_ui, []).

:- http_handler('/lib/bootstrap/popper.min.js', js_popper, []).

:- http_handler('/lib/bootstrap/bootstrap.min.js', js_bootstrap, []).

:- http_handler('/lib/bootstrap/bootstrap-toggle.js', js_bootstrap_toggle, []).

:- http_handler('/lib/bootstrap/bootstrap-switch.min.js', js_bootstrap_switch, []).

:- http_handler('/lib/knockout/knockout.js', js_knockout, []).

:- http_handler('/lib/superfish-master/dist/js/hoverIntent.js', js_hover, []).

:- http_handler('/lib/superfish-master/dist/js/superfish.js', js_superfish, []).

:- http_handler('/src/js/Autocomplete.js', js_autocomplete, []).

:- http_handler('/src/js/ClickHelper.js', js_click_helper, []).

:- http_handler('/src/js/FeatureStructure.js', js_feature_struct, []).

:- http_handler('/src/js/KeyEventHelper.js', js_key_helper, []).

:- http_handler('/src/js/LookaheadObject.js', js_lookahead, []).

:- http_handler('/src/js/SuccessHelper.js', js_success_helper, []).

:- http_handler('/src/js/superfish_modules.js', js_superfish_mod, []).

:- http_handler('/src/js/TextArea.js', js_text_area, []).

:- http_handler('/src/js/ViewModel.js', js_view_model, []).

:- http_handler('/src/js/Dropdown.js', js_dropdown, []).


%---------------------------------------------------------------
% HTTP Reply
%---------------------------------------------------------------


handle(Request) :-
   (
      member(method(post), Request), !,
      http_parameters(Request, [id(Id, []),
				inputmode(InputMode, []),
                                editmode(Mode, []),
                                token(Token, []),
                                featurestructure(FS, []),
                                filename(FName, []),
                                spectext(Text, []),
                                snum(SNum, []),
                                spos(SPos, []),
                                reasoner(Flag, []),
                                reasonermode(RMode, [])
                                ]),
     %  write(user, 'Token: '), writeq(user, Token), nl(user), nl(user),
      JSTN = json([id=Id,
		   inputmode=InputMode,
                   editmode=Mode,
                   token=Token,
                   featurestructure=FS,
                   filename=FName,
                   spectext=Text,
                   snum=SNum,
                   spos=SPos,
                   reasoner=Flag,
                   reasonermode=RMode]),
      % display_json_in(JSTN),                  
      main_process(JSTN, JSON),         % defined in asp_main.pl
      reply_json(JSON)
      % display_json_out(JSON)
   ;
      http_reply_file('./index.html', [mime_type('text/html')], Request)
   ).

display_json_in(JSTN) :-
   (
      atom_json_term(Atom, JSTN, [as(string)]),
      write(user, 'Input: '),
      write(user, Atom),
      nl(user),
      nl(user)
   ;
      true
   ).

display_json_out(JSON) :-
   write(user, 'Output: '),
   write(user, JSON),
   nl(user),
   nl(user). 

%---------------------------------------------------------------
% CSS closure
%---------------------------------------------------------------

css_styles(Request) :-
  http_reply_file('src/css/styles.css', [mime_type('text/css')], Request).

css_bootstrap(Request) :-
  http_reply_file('lib/bootstrap/bootstrap.css', [mime_type('text/css')], Request).

css_bootstrap_toggle(Request) :-
  http_reply_file('lib/bootstrap/bootstrap-toggle.css', [mime_type('text/css')], Request).

css_bootstrap_switch(Request) :-
  http_reply_file('lib/bootstrap/bootstrap-switch.min.css', [mime_type('text/css')], Request).

css_superfish(Request) :-
  http_reply_file('lib/superfish-master/dist/css/superfish.css', [mime_type('text/css')], Request).

%---------------------------------------------------------------
% JavaScript closure
%---------------------------------------------------------------

js_jquery(Request) :-
  http_reply_file('lib/jquery/jquery.min.js', [mime_type('text/javascript')], Request).

js_jquery_ui(Request) :-
  http_reply_file('lib/jquery/jquery-ui.js', [mime_type('text/javascript')], Request).

js_popper(Request) :-
  http_reply_file('lib/bootstrap/popper.min.js', [mime_type('text/javascript')], Request).

js_bootstrap_toggle(Request) :-
  http_reply_file('lib/bootstrap/bootstrap-toggle.js', [mime_type('text/javascript')], Request).

js_bootstrap(Request) :-
  http_reply_file('lib/bootstrap/bootstrap.min.js', [mime_type('text/javascript')], Request).

js_bootstrap_switch(Request) :-
  http_reply_file('lib/bootstrap/bootstrap-switch.min.js', [mime_type('text/javascript')], Request).

js_knockout(Request) :-
  http_reply_file('lib/knockout/knockout.js', [mime_type('text/javascript')], Request).

js_hover(Request) :-
  http_reply_file('lib/superfish-master/dist/js/hoverIntent.js', [mime_type('text/javascript')], Request).

js_superfish(Request) :-
  http_reply_file('lib/superfish-master/dist/js/superfish.js', [mime_type('text/javascript')], Request).

js_view_model(Request) :-
  http_reply_file('src/js/ViewModel.js', [mime_type('text/javascript')], Request).

js_autocomplete(Request) :-
  http_reply_file('src/js/Autocomplete.js', [mime_type('text/javascript')], Request).

js_key_helper(Request) :-
  http_reply_file('src/js/KeyEventHelper.js', [mime_type('text/javascript')], Request).

js_text_area(Request) :-
  http_reply_file('src/js/TextArea.js', [mime_type('text/javascript')], Request).

js_success_helper(Request) :-
  http_reply_file('src/js/SuccessHelper.js', [mime_type('text/javascript')], Request).

js_click_helper(Request) :-
  http_reply_file('src/js/ClickHelper.js', [mime_type('text/javascript')], Request).

js_feature_struct(Request) :-
  http_reply_file('src/js/FeatureStructure.js', [mime_type('text/javascript')], Request).

js_lookahead(Request) :-
  http_reply_file('src/js/LookaheadObject.js', [mime_type('text/javascript')], Request).

js_superfish_mod(Request) :-
  http_reply_file('src/js/superfish_modules.js', [mime_type('text/javascript')], Request).
  
js_dropdown(Request) :-
  http_reply_file('src/js/Dropdown.js', [mime_type('text/javascript')], Request).

% ---------------------------------------------------------------
% Starts the server on port 8085
% You can connect to the server via http://localhost:8085/peng/
% ---------------------------------------------------------------

:- server(8085),
   nl, nl,
   write('*** Prolog Server is listening on port: 8085  ***'), nl,
   write('*** Connect via: http://localhost:8085/peng/  ***'),
   nl, nl.
