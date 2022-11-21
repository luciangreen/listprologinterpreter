add_pred_numbers(Algorithm1,Algorithm2) :-
	Symbols=[":-","->"],
	findall(Algorithm3,(
	member(B,Algorithm1),
	
((B=[Name,Arguments1,Symbol1,Body1],member(Symbol1,Symbols),assert_find(pred_number,N),Algorithm3=[N,Name,Arguments1,Symbol1,Body1])->true;

((B=[Name,Arguments1],assert_find(pred_number,N),Algorithm3=[N,Name,Arguments1])->true;

((B=[Name,Symbol1,Body1],member(Symbol1,Symbols),assert_find(pred_number,N),Algorithm3=[N,Name,Symbol1,Body1])->true;

((B=[Name],assert_find(pred_number,N),Algorithm3=[N,Name])
))))),

Algorithm2).

assert_append(Rule,Value1) :-
	functor(Command,Rule,1),
	arg(1,Command,Values2),
	Command,
	append(Values2,[Value1],Values3),
	functor(Command2,Rule,1),
 	retractall(Command2),
	functor(Command3,Rule,1),
	arg(1,Command3,Values3),
 	assertz(Command3).
 	
assert_member(Value,Rule) :-
	functor(Command,Rule,1),
	arg(1,Command,Values2),
	Command,
	member(Value,Values2).

assert_get(Value,Rule) :-
	functor(Command,Rule,1),
	arg(1,Command,Values2),
	Command,
	Value=Values2.

assert_put(Value,Rule) :-
	functor(Command,Rule,1),
	arg(1,Command,_Values2),
	Command,
 	retractall(Command),
	functor(Command3,Rule,1),
	arg(1,Command3,Value),
 	assertz(Command3).

assert_find(Variable,N2) :-
	assert_get(N1,Variable),
	N2 is N1+1,
	assert_put(N2,Variable).
