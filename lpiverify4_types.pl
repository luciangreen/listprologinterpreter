%% test_types(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

test_types(Debug,NTotal,Score) :- test_types(Debug,0,NTotal,0,Score),!.
test_types(_Debug,NTotal,NTotal,Score,Score) :- NTotal=8, !.
test_types(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test_types_cases(NTotal3,Query,Types,Functions,Result),
	(interpret(Debug,Query,Types,Functions,Result)->(Score3 is Score1+1,writeln([test_types,NTotal3,passed]));(Score3=Score1,writeln([test_types,NTotal3,failed]))),
	writeln(""),
	test_types(Debug,NTotal3,NTotal2,Score3,Score2),!.

test_types1(Debug,N,Passed) :-
	test_types_cases(N,Query,Types,Functions,Result),
	((interpret(Debug,Query,Types,Functions,Result)%%writeln(Result1),
	%%Result=Result1
	)->(Passed=passed,writeln([test_types,N,passed]));(Passed=failed,writeln([test_types,N,failed]))),!.


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

test_types_cases(2,[[n,function],[[v,a],[v,b],[v,c]]],
[[[n,function],[[t,number],[t,string],[t,predicatename]]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,=],[[v,a],1]],
                [[n,=],[[v,b],"a"]],
                [[n,=],[[v,c],[n,a]]]
        ]]
]
,[[[[v,a], 1],[[v,b], "a"],[[v,c], [n,a]]]]).

test_types_cases(3,[[n,function],[[v,a]]],
[[[n,function],[[[t,brackets],[[t,number]]]]]],
[
        [[n,function],[[1]]]
]
,[[[[v,a], [1]]]]).

test_types_cases(4,[[n,f],[[v,a],[v,b],[v,c],[v,d]]],
[[[n,f],[[[t,list],[[t,number],[t,string]]]]]],
[
        [[n,f],[1,"a",2,"b"]]
]
,[[[[v,a], 1],[[v,b], "a"],[[v,c], 2],[[v,d], "b"]]]).

test_types_cases(5,[[n,f],[[v,a],[v,b]]],
[
        [[n,f],[[t,a],[t,b]]],
        [[t,a],[[t,number]]],
        [[t,b],[[t,string]]]
],
[
        [[n,f],[1,"a"]]
]
,[[[[v,a], 1],[[v,b], "a"]]]).

test_types_cases(6,[[n,f],[[v,a]]],
[
        [[n,f],[[t,a]]],
        [[t,a],[[t,number]]],
        [[t,a],[[t,string]]]
],
[
        [[n,f],["a"]]
]
,[[[[v,a], "a"]]]).

%%test_types_cases(7,[[n,getitemn],[1,[1,2,3],[v,bb]]],
test_types_cases(7,[[n,map],[[[n,add],[[[n,add],[[[n,add],[1]]]]]],0,[v,d]]],
[
        [[n,map],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,number]]]]],
        [t,number],[t,number]]],
        
        [[n,map],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,any]]]]],
        [t,number],[t,number]]],
        
        [[n,add],[[t,number],[t,number],[t,number]]],
        
        [[n,getitemn],[[t,number],[[t,list],[[t,any]]],[t,any]]]

        %%[[n,getitemn],[[t,number],
        %%[[t,brackets],[[[t,list],[[t,any]]]]],[t,any]]]
],
[
        [[n,map],[[v,f1],[v,l],[v,n]],":-",
        [        
                [[n,equals1],[[v,f1],[[v,f11],[v,f12]]]],
                [[n,=],[[v,f11],[n,add]]],
                [[n,getitemn],[1,[v,f12],[v,bb]]],
                [[n,number],[[v,bb]]],
                [[v,f11],[[v,l],[v,bb],[v,n]]]
        ]
        ],       
        [[n,map],[[v,f1],[v,l],[v,n]],":-",
        [        
                [[n,equals1],[[v,f1],[[v,f11],[v,f12]]]],
                [[n,=],[[v,f11],[n,add]]],
                [[n,getitemn],[1,[v,f12],[v,bb]]],
                [[v,f11],[[v,l],1,[v,l2]]],
                [[n,map],[[v,bb],[v,l2],[v,n]]]
        ]
        ],

        [[n,add],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]
        ]],

        [[n,getitemn],[1,[v,b],[v,c]],":-",
        [       [[n,head],[[v,b],[v,c]]]
        ]],
        [[n,getitemn],[[v,a],[v,b],[v,c]],":-",
        [       [[n,not],[[[n,=],[[v,a],0]]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,getitemn],[[v,d],[v,t],[v,c]]]
        ]]

]

,[[[[v,d], 3]]]).
%%,[[[[v,bb], 1]]]).


test_types_cases(8,[[n,f],[[v,d],[v,a],[v,c]]],
[[[n,f],[[t,number],[[t,list],[[t,number],[t,string]]],[t,number]]]],
[
        [[n,f],[1,[1,"a",2,"b"],1]]
]
,[[[[v,d], 1],[[v,a], [1,"a",2,"b"]],[[v,c], 1]]]).
