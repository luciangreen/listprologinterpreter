%% test_pl(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% test_pl cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test_pl(Debug,NTotal,Score) :- test_pl(Debug,0,NTotal,0,Score),!.
test_pl(_Debug,NTotal,NTotal,Score,Score) :- NTotal=1, !.
test_pl(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test_pl(NTotal3,Query,Functions,Result),
	p2lpconverter_command([string,Query],Query1),
	p2lpconverter([string,Functions],Functions1),	
	p2lpconverter_term([string,Result],Result2),	
	((international_interpret([lang,"en"],Debug,Query1,Functions1,Result1),
	%writeln1([result1,Result1]),
	Result2=Result1	
	)->(Score3 is Score1+1,writeln0([test_pl,NTotal3,passed]));(Score3=Score1,writeln0([test_pl,NTotal3,failed]))),
	writeln0(""),
	test_pl(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% test_pl individual cases, Debug=trace=on or off, N=case number, Passed=output=result

test_pl1(Debug,N,Passed) :-
	test_pl(N,Query,Functions,Result),
	p2lpconverter_command([string,Query],Query1),
	p2lpconverter([string,Functions],Functions1),	
%trace,
	p2lpconverter_term([string,Result],Result2),	
	((international_interpret([lang,"en"],Debug,Query1,Functions1,Result1),
	%writeln1([result1,Result1]),
	Result2=Result1	
	)->(Passed=passed,writeln0([test_pl,N,passed]));(Passed=failed,writeln0([test_pl,N,failed]))),!.

p2lpconverter_command([Type,In],Out) :-
	string_concat("a:-",In,In1),
	p2lpconverter([Type,In1],Out1),
	Out1=[[[n,a],":-",Out]],!.
p2lpconverter_term([Type,In],Out) :-
	foldr(string_concat,["a:-",In,"."],In1),
	p2lpconverter([Type,In1],Out1),
	Out1=[[[n,a],":-",Out]],!.
	

test_pl(1,"string_chars(\"aabb\",A),findall(A1,(member(A2,A),atom_string(A2,A1)),A3),c(A3,A4).",
"c-->[].
c-->[\"a\"],d,c.
d-->[].
d-->[\"b\"],d.",

"[[A,[a,a,b,b]],[A3,[\"a\",\"a\",\"b\",\"b\"]],[A4,[\"a\",\"a\",\"b\",\"b\"]]]").
