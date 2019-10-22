testopen(Debug,NTotal) :- testopen(Debug,0,NTotal),!.
testopen(_Debug,NTotal,NTotal) :- NTotal=1, !.
testopen(Debug,NTotal1,NTotal2) :-
	NTotal3 is NTotal1+1,
	testopen_cases(NTotal3,Query,Functions),
	(interpret(Debug,Query,Functions,Result)->(writeln([test,NTotal3,result,Result]),writeln([test,NTotal3,passed]));(writeln([test,NTotal3,failed]))),
	writeln(""),
	testopen(Debug,NTotal3,NTotal2),!.

testopen1(Debug,N) :-
	testopen_cases(N,Query,Functions),
((interpret(Debug,Query,Functions,Result))->(writeln([test,N,result,Result]),writeln([test,N,passed]));(writeln([test,N,failed]))),!.

testopen_cases(1,[[n,datetime],[[v,year],[v,month],[v,day],[v,hour],[v,minute],[v,second]]],

[
[[n,datetime],[[v,y],[v,m],[v,d],[v,h],[v,mi],[v,s]],":-",
	[[[n,date],[[v,y],[v,m],[v,d],[v,h],[v,mi],[v,s]]]]]]       
).

