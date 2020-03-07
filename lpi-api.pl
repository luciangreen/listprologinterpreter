:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

% we need this module from the HTTP client library for http_read_data
:- use_module(library(http/http_client)).
:- http_handler('/', web_form, []).

:- include('files/listprolog.pl').

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

	/*
	browse http://127.0.0.1:8000/
	This demonstrates handling POST requests
	   */

	   web_form(_Request) :-
	   	reply_html_page(
			    title('List Prolog Interpreter'),
			    	    [
				    	     form([action='/landing', method='POST'], [
				    	     /**
					     		p([], [
									  label([for=debug],'Debug (on/off):'),
									  		  input([name=debug, type=textarea])
											  		      ]),
											  		      **/
													      		p([], [
																	  label([for=query],'Query (e.g. [[n,reverse],[[1,2,3],[],[v,l]]]):'),
																	  		  input([name=query, type=textarea])
																			  		      ]),
													      		p([], [
																	  label([for=functions],'Functions (e.g. [
[[n,reverse],[[],[v,l],[v,l]]],
[[n,reverse],[[v,l],[v,m],[v,n]],":-",
[[[n,head],[[v,l],[v,h]]],
[[n,tail],[[v,l],[v,t]]],
[[n,wrap],[[v,h],[v,h1]]],
[[n,append],[[v,h1],[v,m],[v,o]]],
[[n,reverse],[[v,t],[v,o],[v,n]]]
]
]
]):'),
																	  		  input([name=functions, type=textarea])
																			  		      ]),
																					      		p([], input([name=submit, type=submit, value='Submit'], []))
																								      ])]).

																								      :- http_handler('/landing', landing_pad, []).

																								      landing_pad(Request) :-
																								              member(method(post), Request), !,
																									              http_read_data(Request, Data, []),
																										              format('Content-type: text/html~n~n', []),
																											      	format('<p>', []),
																												        %%portray_clause(Data),
																												        
																												        %%term_to_atom(Term,Data),
																												        
Data=[%%debug='off',%%Debug1,
query=Query1,functions=Functions1,submit=_],

term_to_atom(Debug2,'off'),
term_to_atom(Query2,Query1),
term_to_atom(Functions2,Functions1),

interpret(Debug2,Query2,Functions2,Result),
																														%%format('</p><p>========~n', []),
																															%%portray_clause
																															portray_clause(Result),
																																																															%%writeln1(Data),

format('</p>').