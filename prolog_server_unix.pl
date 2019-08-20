%===============================================================
% Project:  PENG Engine
% Module:   prolog_server_unix.pl
% Author:   Rolf Schwitter
% Date:     2019-01-10
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

% Only relevant for UNIX
:- use_module(library(http/http_unix_daemon)).

:- ensure_loaded('asp_main.pl').


%---------------------------------------------------------------
% start_server/0
% starts the HTTP server as a daemon process at port 8085
%
% c:>swipl prolog_server.pl --port=8085
%
% Only relevant for UNIX
%---------------------------------------------------------------

start_server :-
  http_daemon.


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

:- http_handler('./src/css/styles.css', css_styles, []).

:- http_handler('./lib/boostrap/bootstrap.css', css_bootstrap, []).

:- http_handler('./lib/boostrap/bootstrap-toggle.css', css_bootstrap_toggle, []).

%---------------------------------------------------------------
% http_handler/3
%
%   - JavaScript handler
%---------------------------------------------------------------


:- http_handler('./lib/boostrap/bootstrap-toggle.js', js_bootstrap_toggle, []).

:- http_handler('./src/js/ClickHelper.js', js_click_helper, []).

:- http_handler('./lib/jquery/jquery.min.js', js_jquery, []).

:- http_handler('./lib/jquery/bootstrap.min.js', js_bootstrap, []).

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
  http_reply_file('./src/css/styles.css', [mime_type('text/css')], Request).

css_bootstrap(Request) :-
  http_reply_file('./lib/bootstrap/bootstrap.css', [mime_type('text/css')], Request).

css_bootstrap_toggle(Request) :-
  http_reply_file('./lib/bootstrap/bootstrap-toggle.css', [mime_type('text/css')], Request).

%---------------------------------------------------------------
% JavaScript closure
%---------------------------------------------------------------

js_bootstrap_toggle(Request) :-
  http_reply_file('./lib/bootstrap/bootstrap-toggle.js', [mime_type('text/javascript')], Request).

js_click_helper(Request) :-
  http_reply_file('./src/js/ClickHelper.js', [mime_type('text/javascript')], Request).

js_jquery(Request) :-
  http_reply_file('./lib/jquery/jquery.min.js', [mime_type('text/javascript')], Request).

js_bootstrap(Request) :-
  http_reply_file('./lib/bootstrap/bootstrap.min.js', [mime_type('text/javascript')], Request).

% ---------------------------------------------------------------
% Starts the server on port 8085
% ---------------------------------------------------------------

:- start_server,
   nl, nl,
   write('*** Prolog Server is listening on port: 8085     ***'), nl,
   write('*** Connect via: http://130.56.243.30:8085/peng/ ***'), 
   nl, nl.
