%% test_types(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

test_types(Debug,NTotal,Score) :- test_types(Debug,0,NTotal,0,Score),!.
test_types(_Debug,NTotal,NTotal,Score,Score) :- NTotal=6, !.
test_types(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test_types_cases(NTotal3,Query,Types,Functions,Result),
	(interpret(Debug,Query,Types,Functions,Result)->(Score3 is Score1+1,writeln([test_types,NTotal3,passed]));(Score3=Score1,writeln([test_types,NTotal3,failed]))),
	writeln(""),
	test_types(Debug,NTotal3,NTotal2,Score3,Score2),!.

test_types1(Debug,N,Passed) :-
	test_types_cases(N,Query,Types,Functions,Result),
	((interpret(Debug,Query,Types,Functions,Result1),%%writeln(Result1),
	Result=Result1)->(Passed=passed,writeln([test_types,N,passed]));(Passed=failed,writeln([test_types,N,failed]))),!.


%%writeln([eg1]),
test_types_cases(1,[[n,function],[1,1,[v,c]]],
[[[n,function],[[[t,list],[[t,number]]]]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,[[[[v,c], 2]]]).

test_types_cases(2,[[n,function],[1,"a",[n,a]]],
[[[n,function],[[t,number],[t,string],[t,predicatename]]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,=],[[v,a],1]],
                [[n,=],[[v,b],"a"]],
                [[n,=],[[v,c],[n,a]]]
        ]]
]
,[[]]).

test_types_cases(3,[[n,function],[[v,a]]],
[[[n,function],[[[t,brackets],[[t,number]]]]]],
[
        [[n,function],[[1]]]
]
,[[[[v,a], [1]]]]).

test_types_cases(4,[[n,f],[1,"a",2,"b"]],
[[[n,f],[[[t,list],[[t,number],[t,string]]]]]],
[
        [[n,f],[1,"a",2,"b"]]
]
,[[]]).

test_types_cases(5,[[n,f],[1,"a"]],
[
        [[n,f],[[t,a],[t,b]]],
        [[t,a],[[t,number]]],
        [[t,b],[[t,string]]]
],
[
        [[n,f],[1,"a"]]
]
,[[]]).

test_types_cases(6,[[n,f],["a"]],
[
        [[n,f],[[t,a],[t,b]]],
        [[t,a],[[t,number]]],
        [[t,a],[[t,string]]]
],
[
        [[n,f],["a"]]
]
,[[]]).
