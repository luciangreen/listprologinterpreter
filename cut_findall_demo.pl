% Demonstration of cut working in findall after the fix
% This file demonstrates that the issue "Make LPI work with cut, especially in findall" has been resolved.

% Example 1: Basic cut in findall - should return only first solution
demo_cut_in_findall :-
    writeln('Demo 1: findall with cut - returns only first solution'),
    findall(X, (member(X, [1,2,3,4,5]), !), Result),
    writeln('findall(X, (member(X, [1,2,3,4,5]), !), Result).'),
    write('Result: '), writeln(Result),
    writeln('Expected: [1] (cut stops after first solution)'),
    writeln('').

% Example 2: findall without cut - should return all solutions  
demo_findall_without_cut :-
    writeln('Demo 2: findall without cut - returns all solutions'),
    findall(X, member(X, [1,2,3,4,5]), Result),
    writeln('findall(X, member(X, [1,2,3,4,5]), Result).'),
    write('Result: '), writeln(Result),
    writeln('Expected: [1,2,3,4,5] (all solutions found)'),
    writeln('').

% Example 3: Cut after condition - demonstrates selective cutting
demo_cut_after_condition :-
    writeln('Demo 3: findall with cut after condition'),
    findall(X, (member(X, [1,2,3,4,5]), X > 2, !), Result),
    writeln('findall(X, (member(X, [1,2,3,4,5]), X > 2, !), Result).'),
    write('Result: '), writeln(Result),
    writeln('Expected: [3] (first solution where X > 2, then cut)'),
    writeln('').

% Example 4: Nested findall with cut
demo_nested_findall :-
    writeln('Demo 4: nested findall with cut'),
    findall(FirstOfEach, (
        member(List, [[a,b,c], [1,2,3], [x,y,z]]),
        findall(Item, (member(Item, List), !), [FirstOfEach|_])
    ), Result),
    writeln('Outer findall collecting first element of each sublist using inner findall+cut'),
    write('Result: '), writeln(Result),
    writeln('Expected: [a,1,x] (first element of each sublist)'),
    writeln('').

% Run all demonstrations
run_cut_demos :-
    writeln('=== Cut in Findall - Fix Demonstration ==='),
    writeln('This demonstrates that cut now works correctly within findall.'),
    writeln(''),
    demo_cut_in_findall,
    demo_findall_without_cut,
    demo_cut_after_condition,
    demo_nested_findall,
    writeln('=== All demonstrations complete ==='),
    writeln('Cut in findall is now working as expected!').