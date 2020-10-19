%% test(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test(Debug,NTotal,Score) :- test(Debug,0,NTotal,0,Score),!.
test(_Debug,NTotal,NTotal,Score,Score) :- NTotal=75, !.
test(Debug,NTotal1,NTotal2,Score1,Score2) :-
%% Finds the alg given i/o
	NTotal3 is NTotal1+1,
	functions(Functions),
	test(NTotal3,Query,Function1,Result),
	((findall([Function,R],(member(Function,Functions),interpret(Debug,Query,Function,R)),Rs),member([Function1,Result],Rs))
	%%writeln1(Result2
	->(Score3 is Score1+1,writeln([test,NTotal3,passed]));(Score3=Score1,writeln([test,NTotal3,failed]))),
	writeln(""),
	test(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

test1(Debug,N,Passed) :-
	functions(Functions),
	test(N,Query,Function1,Result),
	(((findall([Function,R],(member(Function,Functions),interpret(Debug,Query,Function,R)),Rs),%%writeln(Rs),
	member([Function1,Result],Rs))
	%%Result=Result1
	)->(Passed=passed,writeln([test,N,passed]));(Passed=failed,writeln([test,N,failed]))),!.

functions([
[
[[n,reverse],[[],[v,l],[v,l]]],
[[n,reverse],[[v,l],[v,m],[v,n]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,wrap],[[v,h],[v,h1]]],[[n,append],[[v,h1],[v,m],[v,o]]],[[n,reverse],[[v,t],[v,o],[v,n]]]]]
],
[
[[n,intersection1],[[],[v,a],[v,l],[v,l]]],
[[n,intersection1],[[v,l1],[v,l2],[v,l3a],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l4]]],[[n,intersection2],[[v,i1],[v,l2],[],[v,l5]]],[[n,append],[[v,l3a],[v,l5],[v,l6]]],[[n,intersection1],[[v,l4],[v,l2],[v,l6],[v,l3]]]]],
[[n,intersection2],[[v,a],[],[v,l],[v,l]]],
[[n,intersection2],[[v,i1],[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l4]]],[[n,wrap],[[v,i1],[v,i11]]],[[n,append],[[v,l2],[v,i11],[v,l5]]],[[n,intersection2],[[v,i1],[v,l4],[v,l5],[v,l3]]]]],
[[n,intersection2],[[v,i1],[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i2]]],[[n,tail],[[v,l1],[v,l4]]],[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],[[n,intersection2],[[v,i1],[v,l4],[v,l2],[v,l3]]]]]
],
[
[[n,append1],[[v,b],[v,c],[v,a]],":-",[[[n,append],[[v,b],[v,c],[v,a]]]]]
],
[
[[n,minus1],[[v,l],[],[v,l]]],
[[n,minus1],[[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l2],[v,i1]]],[[n,tail],[[v,l2],[v,l5]]],[[n,delete2],[[v,l1],[v,i1],[],[v,l6]]],[[n,minus1],[[v,l6],[v,l5],[v,l3]]]]],
[[n,delete2],[[],[v,a],[v,l],[v,l]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l5]]],[[n,delete2],[[v,l5],[v,i1],[v,l2],[v,l3]]]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i2]]],[[n,tail],[[v,l1],[v,l5]]],[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],[[n,wrap],[[v,i2],[v,i21]]],[[n,append],[[v,l2],[v,i21],[v,l6]]],[[n,delete2],[[v,l5],[v,i1],[v,l6],[v,l3]]]]]
],
[
[[n,mutuallyexclusive],[[],[v,l]]],
[[n,mutuallyexclusive],[[v,l],[v,m]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,membera3],[[v,m],[v,h]]],[[n,mutuallyexclusive],[[v,t],[v,m]]]]],
[[n,membera3],[[],[v,l]]],
[[n,membera3],[[v,l],[v,m]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,=],[[v,m],[v,h]]]]],[[n,membera3],[[v,t],[v,m]]]]],
[[n,duplicates],[[],[v,l],[v,s],[v,s]]]
],
[
[[n,duplicates],[[v,l],[v,m],[v,s1],[v,s2]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,member],[[v,m],[v,h]]],[[n,"->"],[[[n,deletea2],[[v,m],[v,h],[v,m1]]],[[n,true]],[[n,=],[[v,m],[v,m1]]]]],[[n,wrap],[[v,h],[v,h1]]],[[n,append],[[v,s1],[v,h1],[v,s3]]],[[n,duplicates],[[v,t],[v,m1],[v,s3],[v,s2]]]]],
[[n,duplicates],[[v,l],[v,m],[v,s1],[v,s2]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,membera4],[[v,m],[v,h]]]]],[[n,duplicates],[[v,t],[v,m],[v,s1],[v,s2]]]]],
[[n,deletea2],[[],[v,l],[v,m1]],":-",[[[n,fail]]]],
[[n,deletea2],[[v,l],[v,m],[v,t]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,=],[[v,m],[v,h]]]]],
[[n,deletea2],[[v,l],[v,m],[v,m1]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,=],[[v,m],[v,h]]]]],[[n,deletea2],[[v,t],[v,m],[v,m1]]]]]
],
[
[[n,membera4],[[],[v,l]],":-",[[[n,fail]]]],
[[n,membera4],[[v,l],[v,h]],":-",[[[n,head],[[v,l],[v,h]]]]],
[[n,membera4],[[v,l],[v,m]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,=],[[v,m],[v,h]]]]],[[n,membera4],[[v,t],[v,m]]]]],
[[n,substring],[[],[]]],
[[n,substring],[[],[v,b]],":-",[[[n,not],[[[n,=],[[v,b],[]]]]],[[n,fail]]]],
%%[[n,substring],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,at]]],[[n,"->"],[[[n,listhead],[[v,a],[v,b]]],[[[n,true]]],[[[n,substring],[[v,at],[v,b]]]]]]]],
[[n,substring],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,at]]],[[n,"->"],[[[[n,listhead],[[v,a],[v,b]]]],[[[n,true]]],[[[n,substring],[[v,at],[v,b]]]]]]]]
],
[
[[n,listhead],[[v,l],[]]],
[[n,listhead],[[v,a],[v,b]],":-",[[[n,head],[[v,a],[v,ah]]],[[n,tail],[[v,a],[v,at]]],[[n,head],[[v,b],[v,ah]]],[[n,tail],[[v,b],[v,bt]]],[[n,listhead],[[v,at],[v,bt]]]]]%%,
%%[[n,listhead],[[v,a],[v,b]],":-",[[[n,head],[[v,a],[v,ah]]],[[n,tail],[[v,a],[v,at]]],[[n,head],[[v,b],[v,ah]]],[[n,tail],[[v,b],[v,bt]]],[[n,listhead],[[v,at],[v,bt]]]]]
]
]).
	

%%writeln([eg1]),
test(1,[[n,intersection1],[[["select,dash"],["neiey,person"],["neiey,person"]],[["select,dash"],["neiey,person"],["neiey,person"]],[],[v,c]]],

[
[[n,intersection1],[[],[v,a],[v,l],[v,l]]],
[[n,intersection1],[[v,l1],[v,l2],[v,l3a],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l4]]],[[n,intersection2],[[v,i1],[v,l2],[],[v,l5]]],[[n,append],[[v,l3a],[v,l5],[v,l6]]],[[n,intersection1],[[v,l4],[v,l2],[v,l6],[v,l3]]]]],
[[n,intersection2],[[v,a],[],[v,l],[v,l]]],
[[n,intersection2],[[v,i1],[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l4]]],[[n,wrap],[[v,i1],[v,i11]]],[[n,append],[[v,l2],[v,i11],[v,l5]]],[[n,intersection2],[[v,i1],[v,l4],[v,l5],[v,l3]]]]],
[[n,intersection2],[[v,i1],[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i2]]],[[n,tail],[[v,l1],[v,l4]]],[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],[[n,intersection2],[[v,i1],[v,l4],[v,l2],[v,l3]]]]]
]
,[[[[v,c], [["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]]]]]).

/**
[[n,duplicates],[[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[],[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]]]],

[[n,minus1],[[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[]]],

[[n,reverse],[[["select,dash"],["neiey,person"],["neiey,person"]],[],[["neiey,person"],["neiey,person"],["select,dash"]]]],

[[n,intersection1],[[["neiey,person"],["neiey,person"],["select,dash"]],[["hipaa,square"],["releases,up"],["hipaa,square"]],[],[]]],

[[n,append1],[[],[["hipaa,square"],["releases,up"],["hipaa,square"]],[["hipaa,square"],["releases,up"],["hipaa,square"]]]],

[[n,minus1],[[["hipaa,square"],["releases,up"],["hipaa,square"]],[["select,dash"],["neiey,person"],["neiey,person"]],[["hipaa,square"],["releases,up"],["hipaa,square"]]]]


]
,[[[[v,c], 2]]]).
**/