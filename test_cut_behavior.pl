% Simple test for the cut fix
% This will test if a simple cut works in a minimal context

:- dynamic cut/1.

test_cut_behavior :-
    % Initialize cut state
    retractall(cut(_)), assertz(cut(off)),
    
    % Test 1: Basic cut behavior
    writeln('Test 1: Basic cut with findall'),
    findall(X, (
        retractall(cut(_)), assertz(cut(off)),
        member(X, [1,2,3]),
        retractall(cut(_)), assertz(cut(on)),  % Simulate cut execution
        cut(Cut_state),
        (Cut_state = on -> ! ; true)
    ), L1),
    writeln('Result 1: '), writeln(L1),
    
    % Test 2: No cut behavior
    writeln('Test 2: No cut with findall'),
    findall(Y, (
        retractall(cut(_)), assertz(cut(off)),
        member(Y, [1,2,3]),
        cut(Cut_state),
        (Cut_state = on -> ! ; true)
    ), L2),
    writeln('Result 2: '), writeln(L2).

run_cut_test :-
    test_cut_behavior.