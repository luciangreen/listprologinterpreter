% Test to understand LPI cut behavior

:- use_module(library(date)).

:- dynamic debug/1.
:- dynamic starlog/1.
:- dynamic cut/1.
:- dynamic leash1/1.
:- dynamic types/1.
:- dynamic typestatements/1.
:- dynamic modestatements/1.
:- dynamic sys/1.
:- dynamic equals4/1.
:- dynamic query_box_n/1.
:- dynamic save_debug/1.
:- dynamic saved_debug/1.
:- dynamic lang/1.
:- dynamic retry_back/1.
:- dynamic retry_back_stack/1.
:- dynamic retry_back_stack_n/1.
:- dynamic cumulative_or_current_text/1.
:- dynamic number_of_current_text/1.
:- dynamic html_api_maker_or_terminal/1.
:- dynamic occurs_check/1.

% Initialize
init :- 
    retractall(debug(_)), assertz(debug(off)),
    retractall(cut(_)), assertz(cut(off)),
    retractall(lang(_)), assertz(lang("en")).

% Test what Result is returned by a cut
test_cut_result :-
    init,
    % Test a simple cut statement
    Body1 = [[[n,cut]]],
    interpretbody([],[], [], Vars1, Body1, Result1),
    writeln('Cut statement result:'), writeln(Result1),
    writeln('Vars:'), writeln(Vars1),
    
    % Test a sequence with cut in the middle
    Body2 = [[[n,true]], [[n,cut]], [[n,true]]],
    interpretbody([],[], [], Vars2, Body2, Result2),
    writeln('True-Cut-True sequence result:'), writeln(Result2),
    writeln('Vars:'), writeln(Vars2).

% Minimal interpretbody for cut testing
interpretbody(_Functions1,_Functions2,Vars,Vars,[],true) :- !.

interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
    Body=[Statement|Statements],
    not(predicate_or_rule_name(Statement)),
    interpretstatement1(_,Functions0,Functions,Statement,Vars1,Vars3,Result2,Cut),
    ((not(Cut=cut))->(Functions2=Functions);(!,turncut(on))),
    interpretbody(Functions0,Functions2,Vars3,Vars2,Statements,Result3),
    logicalconjunction(Result1,Result2,Result3), !.

% Minimal interpretstatement1 for cut
interpretstatement1(ssi,_F0,_Functions,[[n,cut]|_],Vars,Vars,true,cut) :- !.
interpretstatement1(ssi,_F0,_Functions,[[n,true]|_],Vars,Vars,true,nocut) :- !.

predicate_or_rule_name(_) :- fail.

turncut(State1) :-
    cut(State2),
    retract(cut(State2)),
    assertz(cut(State1)).

logicalconjunction(true,true,true) :- !.
logicalconjunction(false,false,true) :- !.
logicalconjunction(false,true,false) :- !.
logicalconjunction(false,false,false) :- !.

% Simple language word lookup
get_lang_word("n", n).
get_lang_word("cut", cut).
get_lang_word("true", true).

run_test :-
    test_cut_result.