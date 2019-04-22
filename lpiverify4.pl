
%% test(Debug[on/off],Total,Score).

test(Debug,NTotal,Score) :- test(Debug,0,NTotal,0,Score),!.
test(_Debug,NTotal,NTotal,Score,Score) :- NTotal=12, !.
test(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test(NTotal3,Query,Functions,Result),
	(interpret(Debug,Query,Functions,Result)->(Score3 is Score1+1,writeln([test,NTotal3,passed]));(Score3=Score1,writeln([test,NTotal3,failed]))),
	writeln(""),
	test(Debug,NTotal3,NTotal2,Score3,Score2),!.

test1(Debug,N,Passed) :-
	test(N,Query,Functions,Result),
	(interpret(Debug,Query,Functions,Result)->(Passed=passed,writeln([test,N,passed]));(Passed=failed,writeln([test,N,failed]))),!.


%%writeln([eg1]),
test(1,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,[[[v,c], 2]]).
%%writeln([eg2]),
test(2,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,+],[[v,d],1,[v,c]]]
        ]
        ]
]
,[[[v,c], 3]]).
%%writeln([eg3]),
test(3,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,function2],[[v,d],[v,f]]],
                [[n,+],[[v,a],[v,b],[v,e]]],
                [[n,+],[[v,e],[v,f],[v,g]]],
                [[n,+],[[v,g],[v,d],[v,c]]]
        ]
        ],
        [[n,function2],[[v,a],[v,f]],":-",
        [
                [[n,is],[[v,a],2]],
                [[n,is],[[v,f],1]]
        ]
        ]
]
,[[[v,c], 5]]).
%%writeln([eg4]),
test(4,[[n,append1],[[v,a]]],
[
        [[n,append1],[[v,a]],":-",
        [
                [[n,b],[[v,b]]],
                [[n,c],[[v,c]]],
                [[n,append],[[v,b],[v,c],[v,a]]]
        ]
        ],
        [[n,b],[["b"]]],
        [[n,c],[["c"]]]
]
,[[[v,a], ["b", "c"]]]).

%%writeln([eg5]),
test(5,[[n,count],[1,[v,n]]],
[
        [[n,count],[1,2]],
        [[n,count],[[v,n],[v,p]],":-",
        [
                [[n,+],[[v,n],1,[v,m]]],
                [[n,count],[[v,m],[v,p]]]
        ]
        ]
] ,[[[v,n], 2]]).
%%writeln([eg6]),
test(6,[[n,count],[0,[v,n]]],
[
        [[n,count],[1,2]],
        [[n,count],[[v,n],[v,p]],":-",
        [
                [[n,+],[[v,n],1,[v,m]]],
                [[n,count],[[v,m],[v,p]]]
        ]
        ]
] ,[[[v,n], 2]]).
%%writeln([eg7]),
test(7,[[n,reverse],[[1,2,3],[],[v,l]]],
[
        [[n,reverse],[[],[v,l],[v,l]]],
        [[n,reverse],[[v,l],[v,m],[v,n]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,wrap],[[v,h1],[v,h]]],
                [[n,append],[[v,h1],[v,m],[v,o]]],
                [[n,reverse],[[v,t],[v,o],[v,n]]]
        ]
        ]
],[[[v,l], [3, 2, 1]]]).

test(8,[[n,grammar1],["apple"]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,grammar],[[v,s],[n,noun]]] 
		  ]
		  ],
		  
		  [[n,noun],"->",["apple"]]
],[]).

test(9,[[n,grammar1],["aaa"]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,grammar],[[v,s],[n,noun]]] 
		  ]
		  ],
		  
		  [[n,noun],"->",[""]],
		  [[n,noun],"->",["a",[[n,noun]]]]
],[]).

test(10,[[n,grammar1],["aa",[v,t]]],
[
		  [[n,grammar1],[[v,s],[v,t]],":-",
		  [
		  			 [[n,grammar],[[v,s],[n,noun],[v,t]]] 
		  ]
		  ],
		  
		  [[n,noun],["b"],"->",[""]],
		  [[n,noun],[[v,t]],"->",["a",[[n,noun],[[v,t]]]]]
],[[[v,t],"b"]]).

test(11,[[n,grammar1],["aa",[v,t],[v,u]]],
[
		  [[n,grammar1],[[v,s],[v,t],[v,u]],":-",
		  [
		  			 [[n,grammar],[[v,s],[n,noun],[v,t],[v,u]]] 
		  ]
		  ],
		  
		  [[n,noun],["b","c"],"->",[""]],
		  [[n,noun],[[v,t],[v,u]],"->",["a",[[n,noun],[[v,t],[v,u]]]]]
],[[[v,t],"b"],[[v,u],"c"]]).

test(12,[[n,grammar1],["aa"]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,grammar_part],[[n,noun],[v,s],""]]
		  ]
		  ],

		  [[n,noun],"->",[""]],
		  [[n,noun],"->",["a",[[n,noun]]]]

],[]).


