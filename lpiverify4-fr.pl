%% test(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test(Debug,NTotal,Score) :- test(Debug,0,NTotal,0,Score),!.
test(_Debug,NTotal,NTotal,Score,Score) :- NTotal=1, !.
test(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test(NTotal3,Query,Functions,Result),
	(interpret(Debug,Query,Functions,Result)
	%%writeln1(Result2
	->(Score3 is Score1+1,writeln([test,NTotal3,passed]));(Score3=Score1,writeln([test,NTotal3,failed]))),
	writeln(""),
	test(Debug,NTotal3,NTotal2,Score3,Score2),!.

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
