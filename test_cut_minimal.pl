% Minimal test for cut in findall issue

% Copy the essential dynamic declarations
:- dynamic debug/1.
:- dynamic cut/1.
:- dynamic lang/1.

% Initialize essential predicates
init_test :- 
    retractall(debug(_)), assertz(debug(off)),
    retractall(cut(_)), assertz(cut(off)),
    retractall(lang(_)), assertz(lang("en")).

% Simple test data
member_cut_test(1, [1,2,3]).
member_cut_test(2, [1,2,3]).
member_cut_test(3, [1,2,3]).

% Test predicate that should stop at first solution due to cut
member2a(X, List) :- 
    member_cut_test(X, List),
    !.  % This cut should prevent backtracking in findall

% Test findall with cut
test_findall_cut(Result) :-
    findall(X, member2a(X, [1,2,3]), Result).

% Run test
run_test :-
    init_test,
    test_findall_cut(Result),
    writeln('Result with native findall and cut:'),
    writeln(Result).