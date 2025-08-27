% Test LPI findall with cut - minimal working example

% Include essential parts of the interpreter
:- include('listprolog.pl').

% Simple test case
test_simple_findall_cut :-
    % This should only find the first element due to cut
    Query = [[n,findall],[[[v,x],[v,x]],[[[n,member],[[v,x],[1,2,3]]],[[n,cut]]],[v,result]]],
    Functions = [],
    international_interpret([lang,"en"], off, Query, Functions, Result),
    writeln('Query:'), writeln(Query),
    writeln('Result:'), writeln(Result).

run_simple_test :-
    (test_simple_findall_cut -> 
        writeln('Test completed') ; 
        writeln('Test failed')).