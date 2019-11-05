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

:- http_handler('/peng', handle, []).

%---------------------------------------------------------------
% http_handler/3
%
%   - CSS handler
%---------------------------------------------------------------

:- http_handler('/src/styles/styles.css', css_styles, []).

:- http_handler('/src/styles/loader.css', css_loader, []).

:- http_handler('/libs/bootstrap/bootstrap.css', css_bootstrap, []).

:- http_handler('/libs/bootstrap/bootstrap-toggle.css', css_bootstrap_toggle, []).

:- http_handler('/libs/superfish-master/dist/css/superfish.css', css_superfish, []).

:- http_handler('/libs/bootstrap/bootstrap-switch.min.css', css_bootstrap_switch, []).

%---------------------------------------------------------------
% http_handler/3
%
%   - JavaScript handler
%---------------------------------------------------------------

:- http_handler('/libs/jquery/jquery.min.js', js_jquery, []).

:- http_handler('/libs/jquery/jquery-ui.js', js_jquery_ui, []).

:- http_handler('/libs/bootstrap/popper.min.js', js_popper, []).

:- http_handler('/libs/bootstrap/bootstrap.min.js', js_bootstrap, []).

:- http_handler('/libs/bootstrap/bootstrap-toggle.js', js_bootstrap_toggle, []).

:- http_handler('/libs/bootstrap/bootstrap-switch.min.js', js_bootstrap_switch, []).

:- http_handler('/libs/knockout/knockout.js', js_knockout, []).

:- http_handler('/libs/superfish-master/dist/js/hoverIntent.js', js_hover, []).

:- http_handler('/libs/superfish-master/dist/js/superfish.js', js_superfish, []).

:- http_handler('/src/scripts/Autocomplete.js', js_autocomplete, []).

:- http_handler('/src/scripts/EventHandler.js', js_event_handler, []).

:- http_handler('/src/scripts/LookaheadObject.js', js_lookahead, []).

:- http_handler('/src/scripts/superfish_modules.js', js_superfish_mod, []).

:- http_handler('/src/scripts/TextArea.js', js_text_area, []).

:- http_handler('/src/scripts/ViewModel.js', js_view_model, []).

:- http_handler('/src/scripts/Dropdown.js', js_dropdown, []).

:- http_handler('/src/scripts/Navbar.js', js_navbar, []).

:- http_handler('/src/scripts/GlobalHelper.js', js_global_helper, []).

:- http_handler('/src/scripts/Results.js', js_results, []).

:- http_handler('/src/scripts/ExpressionLoader.js', js_expression_loader, []).

%---------------------------------------------------------------
% http_handler/3
%
%   - Assets handler
%---------------------------------------------------------------

:- http_handler('/src/styles/icon-plus.png', icon_plus, []).

:- http_handler('/src/styles/icon-minus.png', icon_minus, []).

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
      display_json_in(JSTN),                  
      main_process(JSTN, JSON),         % defined in asp_main.pl
      reply_json(JSON),
      display_json_out(JSON)
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
  http_reply_file('./src/styles/styles.css', [mime_type('text/css')], Request).

css_loader(Request) :-
  http_reply_file('./src/styles/loader.css', [mime_type('text/css')], Request).

css_bootstrap(Request) :-
  http_reply_file('./libs/bootstrap/bootstrap.css', [mime_type('text/css')], Request).

css_bootstrap_toggle(Request) :-
  http_reply_file('./libs/bootstrap/bootstrap-toggle.css', [mime_type('text/css')], Request).

css_bootstrap_switch(Request) :-
  http_reply_file('./libs/bootstrap/bootstrap-switch.min.css', [mime_type('text/css')], Request).

css_superfish(Request) :-
  http_reply_file('./libs/superfish-master/dist/css/superfish.css', [mime_type('text/css')], Request).

%---------------------------------------------------------------
% JavaScript closure
%---------------------------------------------------------------

js_jquery(Request) :-
  http_reply_file('./libs/jquery/jquery.min.js', [mime_type('text/javascript')], Request).

js_jquery_ui(Request) :-
  http_reply_file('./libs/jquery/jquery-ui.js', [mime_type('text/javascript')], Request).

js_popper(Request) :-
  http_reply_file('./libs/bootstrap/popper.min.js', [mime_type('text/javascript')], Request).

js_bootstrap_toggle(Request) :-
  http_reply_file('./libs/bootstrap/bootstrap-toggle.js', [mime_type('text/javascript')], Request).

js_bootstrap(Request) :-
  http_reply_file('./libs/bootstrap/bootstrap.min.js', [mime_type('text/javascript')], Request).

js_bootstrap_switch(Request) :-
  http_reply_file('./libs/bootstrap/bootstrap-switch.min.js', [mime_type('text/javascript')], Request).

js_knockout(Request) :-
  http_reply_file('./libs/knockout/knockout.js', [mime_type('text/javascript')], Request).

js_hover(Request) :-
  http_reply_file('./libs/superfish-master/dist/js/hoverIntent.js', [mime_type('text/javascript')], Request).

js_superfish(Request) :-
  http_reply_file('./libs/superfish-master/dist/js/superfish.js', [mime_type('text/javascript')], Request).

js_view_model(Request) :-
  http_reply_file('./src/scripts/ViewModel.js', [mime_type('text/javascript')], Request).

js_autocomplete(Request) :-
  http_reply_file('./src/scripts/Autocomplete.js', [mime_type('text/javascript')], Request).

js_event_handler(Request) :-
  http_reply_file('./src/scripts/EventHandler.js', [mime_type('text/javascript')], Request).

js_text_area(Request) :-
  http_reply_file('./src/scripts/TextArea.js', [mime_type('text/javascript')], Request).

js_lookahead(Request) :-
  http_reply_file('./src/scripts/LookaheadObject.js', [mime_type('text/javascript')], Request).

js_superfish_mod(Request) :-
  http_reply_file('./src/scripts/superfish_modules.js', [mime_type('text/javascript')], Request).
  
js_dropdown(Request) :-
  http_reply_file('./src/scripts/Dropdown.js', [mime_type('text/javascript')], Request).

js_navbar(Request) :-
  http_reply_file('./src/scripts/Navbar.js', [mime_type('text/javascript')], Request).

js_global_helper(Request) :-
  http_reply_file('./src/scripts/GlobalHelper.js', [mime_type('text/javascript')], Request).
  
js_results(Request) :-
  http_reply_file('./src/scripts/Results.js', [mime_type('text/javascript')], Request).
    
js_expression_loader(Request) :-
  http_reply_file('./src/scripts/ExpressionLoader.js', [mime_type('text/javascript')], Request).

%---------------------------------------------------------------
% Assets closure
%---------------------------------------------------------------

icon_plus(Request) :-
  http_reply_file('./src/styles/icon-plus.png', [mime_type('image/png')], Request).

icon_minus(Request) :-
  http_reply_file('./src/styles/icon-minus.png', [mime_type('image/png')], Request).

% ---------------------------------------------------------------
% Starts the server on port 8085
% You can connect to the server via http://localhost:8085/peng
% ---------------------------------------------------------------

:- server(8085),
   nl, nl,
   write('*** Prolog Server is listening on port: 8085  ***'), nl,
   write('*** Connect via: http://localhost:8085/peng  ***'),
   nl, nl.
