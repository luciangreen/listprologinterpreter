%% test_open_types(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test_open_types(Debug,NTotal,Score) :- test_open_types(Debug,0,NTotal,0,Score),!.
test_open_types(_Debug,NTotal,NTotal,Score,Score) :- NTotal=1, !.
test_open_types(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test_open_types_cases(NTotal3,Query,Types,Modes,Functions),
	((interpret(Debug,Query,Types,Modes,Functions,Result),not(Result=[]))->(Score3 is Score1+1,writeln([test_open_types,NTotal3,result,Result]),writeln([test_open_types,NTotal3,passed]));(Score3=Score1,writeln([test_open_types,NTotal3,failed]))),
	writeln(""),
	test_open_types(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

test_open_types1(Debug,N,Passed) :-
	test_open_types_cases(N,Query,Types,Modes,Functions),
	(((interpret(Debug,Query,Types,Modes,Functions,Result),not(Result=[]))%%writeln(Result1),
	%%Result=Result1
	)->(Passed=passed,writeln([test_open_types,N,result,Result]),writeln([test_open_types,N,passed]));(Passed=failed,writeln([test_open_types,N,failed]))),!.


test_open_types_cases(1,[[n,true_vs_good],[[v,t],[v,g]]],

[[[n,true_vs_good],
[
[[t,brackets],
	[
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]]
	]
]
,
[[t,brackets],
	[
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]]
	]
]]
],
[[n,random1],[[t,number],[t,number],[t,number]]]]

,[[[n,true_vs_good],[output,output]],
[[n,random1],[input,input,output]]],
[
        [[n,true_vs_good],[[v,t],[v,g]],":-",
        [
                [[n,random1],[0.1,4.6,[v,y1]]],
                [[n,random1],[[v,y1],4.7,[v,y2]]],
                [[n,random1],[[v,y2],4.8,[v,y3]]],
                [[n,random1],[[v,y3],4.9,[v,y4]]],
                
                [[n,equals2],[[v,c11],[0,0]]],
                [[n,wrap],[[v,c11],[v,c12]]],

                [[n,equals2],[[v,c21],[1,[v,y1]]]],
                [[n,wrap],[[v,c21],[v,c22]]],
                [[n,append],[[v,c12],[v,c22],[v,c23]]],
                
                [[n,equals2],[[v,c31],[2,[v,y2]]]],
                [[n,wrap],[[v,c31],[v,c32]]],
                [[n,append],[[v,c23],[v,c32],[v,c33]]],
                
                [[n,equals2],[[v,c41],[3,[v,y3]]]],
                [[n,wrap],[[v,c41],[v,c42]]],
                [[n,append],[[v,c33],[v,c42],[v,c43]]],
                
                [[n,equals2],[[v,c51],[4,[v,y4]]]],
                [[n,wrap],[[v,c51],[v,c52]]],
                [[n,append],[[v,c43],[v,c52],[v,c53]]],
                
                [[n,equals2],[[v,c61],[5,5]]],
                [[n,wrap],[[v,c61],[v,c62]]],
                [[n,append],[[v,c53],[v,c62],[v,g]]],
                
                [[n,equals3],[[v,t],[[0,0],[1,1],
                [2,2],[3,3],[4,4],[5,5]]]
        ]]],
        
        [[n,random1],[[v,a1],[v,a2],[v,n5]],":-",
        [
                [[n,-],[[v,a2],[v,a1],[v,a3]]],
                [[n,random],[[v,n1]]],
                [[n,*],[[v,a3],[v,n1],[v,n2]]],
                [[n,+],[[v,n2],[v,a1],[v,n21]]],
                [[n,*],[10,[v,n21],[v,n3]]],
                [[n,round],[[v,n3],[v,n4]]],
                [[n,/],[[v,n4],10,[v,n5]]]
        ]]
]).

test_open_types_cases(2,[[n,true_vs_good],[[[n,a],[1]],1,[v,g2]]],

        [[[n,true_vs_good],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,number]]]]],
        [t,number],[t,number]]]],
        
[[[n,true_vs_good],[input,input,output]]],



[
        [[n,true_vs_good],[[v,f1],[v,l],[v,n]],":-",
        [        
                [[n,equals3],[[v,n],1]
                
        ]]
]]).

test_open_types_cases(3,[[n,function],[[v,a]]],
[[[n,function],[[[t,brackets],[[t,number]]]]]],
[[[n,function],[output]]],
[
        [[n,function],[[1]]]
]
).

