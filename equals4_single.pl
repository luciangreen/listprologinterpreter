equals4_list_of_lists_single0(Body_a,Result,Vars1) :-
	equals4_list_of_lists_single1([Body_a],[],[Result],Vars1),!.%true.%.


equals4_list_of_lists_single1([],Result,Result,_Vars) :- true.

equals4_list_of_lists_single1(Body_a,Result1,Result2,Vars1) :-

not(variable_name(Body_a)),

	Body_a=[[Statements1_a|Statements1a_a]|Statements2_a
	],

	not(variable_name([Statements1_a|Statements1a_a])),

	(not(Statements1_a="|")->(F1=0)),
	%([Statements11_a]=Statements1a_a,F1=1))),
	%(((F1=1,F2=1)->equals4_list_of_lists_single1(Statements11_a,Statements11_b,Vars1,Vars3))->true;
	%((F1=1,F2=0)->equals4_list_of_lists_single1(Statements11_a,[Statements1_b|Statements1a_b],Vars1,Vars3))->true;
	%((F1=0,F2=1)->equals4_list_of_lists_single1([Statements1_a|Statements1a_a],Statements11_b,Vars1,Vars3))->true;
	((F1=0%,F2=0
	)->(equals4_list_of_lists_single1(Statements1_a,Result1,[Result4],Vars1),
	equals4_list_of_lists_single1(Statements1a_a,Result4,Result31,Vars1))),

	[Result3]=Result31,

equals4_list_of_lists_single1(Statements2_a,Result3,Result21,Vars1),
	Result2=[Result21],true.%.


equals4_list_of_lists_single1(Body_a,Result1,Result2,Vars1) :-

not(variable_name(Body_a)),

	Body_a=[Statement_a|Statements_a],
	
	(not(Statement_a="|")->(F1=0)),%;
	%([Statements2_a]=Statements_a,F1=1))),
	
	%((not(Statement_b="|")->(F2=0);
	%([Statements2_b]=Statements_b,F2=1))),
	
		%(((F1=1,F2=1)->equals4_list_of_lists_single1(Statements2_a,Statements2_b,Vars1,Vars2))->true;	((F1=1,F2=0)->equals4_list_of_lists_single1(Statements2_a,[Statement_b|Statements_b],Vars1,Vars2))->true;
	%((F1=0,F2=1)->equals4_list_of_lists_single1([Statement_a|Statements_a],Statements2_b,Vars1,Vars2))->true;
	((F1=0%,F2=0
	)->(equals4_list_of_lists_single1(Statement_a,Result1,Result3,Vars1),
	equals4_list_of_lists_single1(Statements_a,Result3,Result21,Vars1))),
	Result2=[Result21].
	
equals4_list_of_lists_single1(Statement_c,Result1,Result2,Vars1) :-
	%trace,
	((not(variable_name_or_expression(Statement_c)))->
			equals4_list_of_lists_single1(Statement_c,Result1,Result2,Vars1);
			equals4_list_of_lists_single3(Statement_c,Result1,Result2,Vars1)),true.%.

	% a=1,1=a where 1 might be [1,2] or 1=1, not a=a
		%((variable_name(Statement_a),variable_name(Statement_b))->

%variable_name_or_expression(Statement) :-
%	(variable_name(Statement)->true),true.
	
% get value
equals4_list_of_lists_single3(Statement_a,Result1,Result2,Vars1) :-
%trace,
	((variable_name(Statement_a))->
	getvalue(Statement_a,Value,Vars1)),
	append(Result1,[Value],Result2).
	%(((variable_name(Statement_a),not(variable_name(Statement_b)))->
	%get_put_value(Statement_a,Statement_b,Vars1,Vars2))->true; % this and line below - find var as above
	%(((not(variable_name(Statement_a)),variable_name(Statement_b))->
	%get_put_value(Statement_b,Statement_a,Vars1,Vars2))))),true.

/*get_put_value(Variable1,Vars1,Vars2) :-
	getvalue(Variable1,Value1,Vars1),
	%(Value2=empty->Value2a=Variable2;Value2a=Value2),
	val1emptyorvalsequal(Value1,Value2),
	putvalue(Variable1,Value2,Vars1,Vars2),!.
*/