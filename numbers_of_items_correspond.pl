numbers_of_items_correspond([],[]) :- %trace,
!.

numbers_of_items_correspond([],Variables2) :-
 Variables2=[Statement1b|Statement2b],
 (Statement1b="|"->
numbers_of_items_correspond([[]],Statement2b)),!.

numbers_of_items_correspond(Variables1,[]) :-
 Variables1=[Statement1a|Statement2a],
 (Statement1a="|"->
numbers_of_items_correspond(Statement2a,[[]])),!.

numbers_of_items_correspond(Variables1,Variables2) :-

%(Variables1=Variables2->fail;

Variables1=[Statement1a|Statement2a],
Variables2=[Statement1b|Statement2b],
	%(Statement1a=Statement1b->fail;
	
((Statement1a="|",Statement1b="|")	->
numbers_of_items_correspond([Statement2a],[Statement2b]);
(Statement1a="|"->
numbers_of_items_correspond(Statement2a,[Variables2]);
(Statement1b="|"->
numbers_of_items_correspond([Variables1],Statement2b);


(
(((single_item(Statement1a)->true;
single_item(Statement1b))->true;
(
%contains_var(Statement1a,Statement1b))->true;

%((variable_name(Statement1b),
%not(variable_name(Statement1a)),
%contains_var(Statement1b,Statement1a))->true;

	
	numbers_of_items_correspond(Statement1a,Statement1b)))),
	numbers_of_items_correspond(Statement2a,Statement2b))
	))).

%[a|b]=[a]
%[a|b]=[a,c]