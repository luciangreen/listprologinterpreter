expression_not_var(Variable2) :-
	not(variable_name(Variable2)),
	expression_not_var1(Variable2,start).

expression_not_var1(Variable1,_) :-
	single_item_not_var(Variable1),!.

expression_not_var1([],_) :- !.
expression_not_var1(Variable1,Position) :-
	(Position=start->not(variable_name(Variable1));true),
	Variable1=[Variable1a|Variable1b],
	not(variable_name(Variable1a)),
	expression_not_var1(Variable1a,start),
	expression_not_var1(Variable1b,non_start),!.

single_item_not_var(A) :- predicate_or_rule_name(A),!.
single_item_not_var(A) :- string(A),!.
single_item_not_var(A) :- number(A),!.
single_item_not_var(A) :- atom(A),!.

single_item_or_var(A) :- predicate_or_rule_name(A),!.
single_item_or_var(A) :- variable_name(A),!.
single_item_or_var(A) :- string(A),!.
single_item_or_var(A) :- number(A),!.
single_item_or_var(A) :- atom(A),!.
