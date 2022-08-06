%/*
match1(A,B) :-
 (var(A)->A1=empty1;A1=A),
 (var(B)->B1=empty1;B1=B),
 A1=B1,!.
 
replace_in_term(A,B,_,A) :- (not(var(A))->A=[];not(var(B))),!.

replace_in_term(Statement,A,B,Term2) :-
	
(match1(A,Statement)->Term2=B;	(Statement=[Statement1|Statement2],
	((match1(A,Statement1)->Statement1_a=B;
	replace_in_term(Statement1,A,B,Statement1_a)),
	replace_in_term(Statement2,A,B,Statement2_a))),
	
append([Statement1_a],Statement2_a,Term2)),!.

replace_in_term(A,B,_,A) :- not(match1(A,B)),!.


replace_empty_with_undefined(Values,Values_u) :-
	findall(Values_u1,(member(Value,Values),replace_in_term(Value,empty,_,Values_u1)),Values_u),!.
replace_undefined_with_empty(Values,Values_e) :-
	findall(Values_e1,(member(Value,Values),replace_in_term(Value,_,empty,Values_e1)),Values_e),!.
%*/