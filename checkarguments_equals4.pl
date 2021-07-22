checkarguments_equals4_list_of_lists0([],[],Vars,Vars,FirstArgs,FirstArgs) :- !.
checkarguments_equals4_list_of_lists0(Body_a,Body_b,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
	Body_a=[Body_a1|Body_a2],
	Body_b=[Body_b1|Body_b2],
	checkarguments_equals4_list_of_lists1([Body_a1],[Body_b1],Vars1,Vars3,FirstArgs1,FirstArgs3),
		checkarguments_equals4_list_of_lists0(Body_a2,Body_b2,Vars3,Vars2,FirstArgs3,FirstArgs2),!.


checkarguments_equals4_list_of_lists1([],[],Vars,Vars,FirstArgs,FirstArgs) :- !.

checkarguments_equals4_list_of_lists1(Body_a,Body_b,Vars1,Vars2,FirstArgs1,FirstArgs2) :-

not(variable_name(Body_a)),not(variable_name(Body_b)),

	Body_a=[[Statements1_a|Statements1a_a]|Statements2_a
	],
	Body_b=[[Statements1_b|Statements1a_b]|Statements2_b
	],
not(variable_name([Statements1_a|Statements1a_a])),
not(variable_name([Statements1_b|Statements1a_b])),

	((not(Statements1_a="|")->(	F1=0);
	([Statements11_a]=Statements1a_a,F1=1))),
	((not(Statements1_b="|")->(F2=0);
	([Statements11_b]=Statements1a_b,F2=1))),

	(((F1=1,F2=1)->checkarguments_equals4_list_of_lists1(Statements11_a,Statements11_b,Vars1,Vars3,FirstArgs1,FirstArgs3))->true;
	((F1=1,F2=0)->checkarguments_equals4_list_of_lists1(Statements11_a,[Statements1_b|Statements1a_b],Vars1,Vars3,FirstArgs1,FirstArgs3))->true;
	((F1=0,F2=1)->checkarguments_equals4_list_of_lists1([Statements1_a|Statements1a_a],Statements11_b,Vars1,Vars3,FirstArgs1,FirstArgs3))->true;
	((F1=0,F2=0)->(
	%trace,
	
((%trace,
expressionnotatom3(Statements1_a),expressionnotatom3(Statements1_b))->	checkarguments_equals4_list_of_lists3(Statements1_a,Statements1_b,Vars1,Vars4,FirstArgs1,FirstArgs4);
checkarguments_equals4_list_of_lists1(Statements1_a,Statements1_b,Vars1,Vars4,FirstArgs1,FirstArgs4)),	
	checkarguments_equals4_list_of_lists1(Statements1a_a,Statements1a_b,Vars4,Vars3,FirstArgs4,FirstArgs3)))),

checkarguments_equals4_list_of_lists1(Statements2_a,Statements2_b,Vars3,Vars2,FirstArgs3,FirstArgs2),true.


checkarguments_equals4_list_of_lists1(Body_a,Body_b,Vars1,Vars2,FirstArgs1,FirstArgs2) :-

not(variable_name(Body_a)),not(variable_name(Body_b)),

	Body_a=[Statement_a|Statements_a],
	Body_b=[Statement_b|Statements_b],
	
	((not(Statement_a="|")->(F1=0);
	([Statements2_a]=Statements_a,F1=1))),
	
	((not(Statement_b="|")->(F2=0);
	([Statements2_b]=Statements_b,F2=1))),
	
		(((F1=1,F2=1)->checkarguments_equals4_list_of_lists1(Statements2_a,Statements2_b,Vars1,Vars2,FirstArgs1,FirstArgs2))->true;	((F1=1,F2=0)->checkarguments_equals4_list_of_lists1(Statements2_a,[Statement_b|Statements_b],Vars1,Vars2,FirstArgs1,FirstArgs2))->true;
	((F1=0,F2=1)->checkarguments_equals4_list_of_lists1([Statement_a|Statements_a],Statements2_b,Vars1,Vars2,FirstArgs1,FirstArgs2))->true;
	((F1=0,F2=0)->(%trace,
	((%trace,
	expressionnotatom3(Statement_a),expressionnotatom3(Statement_b))->checkarguments_equals4_list_of_lists3(Statement_a,Statement_b,Vars1,Vars3,FirstArgs1,FirstArgs3);
	checkarguments_equals4_list_of_lists1(Statement_a,Statement_b,Vars1,Vars3,FirstArgs1,FirstArgs3)),
	checkarguments_equals4_list_of_lists1(Statements_a,Statements_b,Vars3,Vars2,FirstArgs3,FirstArgs2)))).
	
checkarguments_equals4_list_of_lists1(Statement_c,Statement_d,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
	((not(variable_name_or_expression(Statement_c)),not(variable_name_or_expression(Statement_d)))->
			checkarguments_equals4_list_of_lists1(Statement_c,Statement_d,Vars1,Vars2,FirstArgs1,FirstArgs2);
			checkarguments_equals4_list_of_lists3(Statement_c,Statement_d,Vars1,Vars2,FirstArgs1,FirstArgs2)),true.%.

	% a=1,1=a where 1 might be [1,2] or 1=1, not a=a
		%((variable_name(Statement_a),variable_name(Statement_b))->

variable_name_or_expression(Statement) :-
	(variable_name(Statement)->true),true.
	
	
checkarguments_equals4_list_of_lists3(Statement_a,Statement_b,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
%trace,
	(((variable_name(Statement_a),variable_name(Statement_b))->
	(append(FirstArgs1,[[Statement_a,Statement_b]],FirstArgs2),
	putvalue(Statement_b,empty,Vars1,Vars2)))->true;
	(((variable_name(Statement_a),not(variable_name(Statement_b)))->
	(putvalue(Statement_a,Statement_b,Vars1,Vars2)),
	append(FirstArgs1,[[Statement_a,Statement_b]],FirstArgs2)))->true;
	(((not(variable_name(Statement_a)),variable_name(Statement_b))->
	(putvalue(Statement_b,Statement_a,Vars1,Vars2)),
	FirstArgs1=FirstArgs2))->true;
	(((not(variable_name(Statement_a)),not(variable_name(Statement_b)))->
	(Statement_b=Statement_a,Vars1=Vars2,
	FirstArgs1=FirstArgs2)))),true.

putvalue(A,B,C1,D) :-
	delete(C1,[A,B],C),
	append(C,[[A,B]],D).
variable_name([v,_]).
	