%% test(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

i_test(Debug,NTotal,Score) :- i_test(Debug,0,NTotal,0,Score),!.
i_test(_Debug,NTotal,NTotal,Score,Score) :- NTotal=3, !.
i_test(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test-fr(NTotal3,Query,Functions,Result),
	(international_interpret([lang,"fr"],Debug,Query,Functions,Result)
	%%writeln1(Result2
	->(Score3 is Score1+1,writeln([test,NTotal3,passed]));(Score3=Score1,writeln([test,NTotal3,failed]))),
	writeln(""),
	i_test(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

i_test1(Debug,N,Passed) :-
	test-fr(N,Query,Functions,Result),
	((international_interpret([lang,"fr"],Debug,Query,Functions,Result1),%%writeln(Result1),
	Result=Result1
	)->(Passed=passed,writeln([test,N,passed]));(Passed=failed,writeln([test,N,failed]))),!.


%%writeln([eg1]),
test-fr(1,[["n",function],[1,1,["v",c]]],
[
        [["n",function],[["v",a],["v",b],["v",c]],":-",
        [
                [["n",+],[["v",a],["v",b],["v",c]]]
        ]
        ]
]
,[[[["v",c], 2]]]).

test-fr(2,[["n","liste de cartes 1"],[[1,2,3],["v",b]]],

[
        [["n","liste de cartes 1"],[["v",a],["v",b]],":-",
        [       [["n","liste de cartes"],[["n",+],["v",a],0,["v",b]]]
        ]]

        
],[[[["v",b],6]]]).

/**
test-fr(3,[["n",findall1],[[1,2,3],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[[v,a1],[v,a1]],[[n,member2],[[v,a],[v,a1]]],
                [v,b]]]
        ]]
        
],[[[[v,b],[[1,1],[2,2],[3,3]]]]]).
**/


test-fr(3,[["n","Trouver tout 1"],[[[1,11,111],[2,22,222],[3,33,333]],["v",b]]],

[
        [["n","Trouver tout 1"],[["v",a],["v",b]],":-",
        [       [["n","Trouver tout"],[["v",b1],[[["n","membre 2"],[["v",a],["v",a1]]],
        
        [["n","Trouver tout"],[["v",a2],[["n","membre 2"],[["v",a1],["v",a2]]],
                ["v",b1]]]],
                
                ["v",b]]]
        ]]
        
],[[[["v",b],[[1,11,111],[2,22,222],[3,33,333]]]]]).


test-fr(4,[["n","a"],[[1,2],["v",b]]],

[
        [["n","a"],[["v",a],["v",b]],":-",
        [       [["n","membre 2"],[["v",a],["v",b]]]
        ]]
        
],[[[["v",b],1]]]).
