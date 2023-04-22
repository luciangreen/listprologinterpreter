% test1(off,1,R).


match4_new_22(Variable1,Variable2,Vars1,Vars2%,Standard_or_e4
) :-
	(match4_new_220([Variable1],[Variable2],Vars1,Vars2%,Standard_or_e4
	)->true;
	match4_new_220([Variable2],[Variable1],Vars1,Vars2%,Standard_or_e4
	)),!.
	
match4_new_220(Variable1,Variable2,Vars1,Vars6%,Standard_or_e4
) :-
%trace,
	match4_new_222(Variable1,Variable2,Vars1,Vars2%,Standard_or_e4
	),
	
	((subtract(Vars1,Vars2,[]),
	subtract(Vars2,Vars1,[]))->Vars6=Vars2;

	(
	get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,

	find_sys(Sys_name1),
	match4_new_222(Variable1,[Dbw_v,Sys_name1],Vars2,Vars3%,Standard_or_e4
	),
	getvalue([Dbw_v,Sys_name1],Value3,Vars3),

	match4_new_222(Value3,Variable2,Vars2,Vars4%,Standard_or_e4
	),

	find_sys(Sys_name2),
	match4_new_222(Variable2,[Dbw_v,Sys_name2],Vars4,Vars5%,Standard_or_e4
	),
	getvalue([Dbw_v,Sys_name2],Value31,Vars5),

	match4_new_222(Variable1,Value31,Vars4,Vars7%,Standard_or_e4
	),
	
	match4_new_220(Variable1,Variable2,Vars7,Vars6%,Standard_or_e4
	)))%,!
	.


match4_new_222(Variable1,Variable2,Vars1,Vars2%,Standard_or_e4
) :-
%trace,
occurs_check(Variable1,Variable2),
%notrace,
	match4_new([Variable1],[Variable2],Vars1,Vars3%,Standard_or_e4
	),
	findall([V,Val2],(member([V,Val1],Vars3),
	simplify(Val1,Val2)),Vars2),
	!.


match4_new(S1,S2,V1,V2%,_Standard_or_e4
) :-
 match_get_vals(S1,[],S3,V1),
 simplify(S3,S5),
 match_get_vals(S2,[],S4,V1),
 simplify(S4,S6), 
 match_put_vals(S5,S6,V1,V2%,_Standard_or_e4
 ),!.
 
 
match_get_vals([],S1,S1,_) :- !.
	
match_get_vals(Statement,S1,S2,Vars) :-
	
%variable_name(Statement)->
%match_get_val(Statement,Value,Vars),
%append(S1,[Value],S2));
	(Statement=[Statement1|Statement2],
	(variable_name(Statement1)->
(match_get_val(Statement1,Value1,Vars),
append(S1,[Value1],S3));
 (single_item_or_var(Statement1)->
 (Value1=Statement1,
append(S1,[Value1],S3));
	(match_get_vals(Statement1,[],S31,Vars),
	S3=[S31]))),
	match_get_vals(Statement2,[],S4,Vars)),
	foldr(append,[S3,S4],S5),
	%trace,
	%S6=[S5],
	%(S1=[]->S2=S5;
	foldr(append,[S1,S5],S2),!.
	
match_get_val(Variable,Value,Vars)	 :-
 (member([Variable,Value],Vars)->true;
 Variable=Value),!.
 
 
 
 


match_put_vals([],[],Vars,Vars%,_Standard_or_e4
) :- %trace,
!.

match_put_vals([],Variables2,Vars1,Vars2%,Standard_or_e4
) :-
 Variables2=[Statement1b|Statement2b],
 (Statement1b="|"->
match_put_vals([[]],Statement2b,Vars1,Vars2%,Standard_or_e4
)),!.

match_put_vals(Variables1,[],Vars1,Vars2%,Standard_or_e4
) :-
 Variables1=[Statement1a|Statement2a],
 (Statement1a="|"->
match_put_vals(Statement2a,[[]],Vars1,Vars2%,Standard_or_e4
)),!.

match_put_vals(Variables1,Variables2,Vars1,Vars2%,Standard_or_e4
) :-

%(Variables1=Variables2->fail;

Variables1=[Statement1a|Statement2a],
Variables2=[Statement1b|Statement2b],
	%(Statement1a=Statement1b->fail;
	
((Statement1a="|",Statement1b="|")	->
match_put_vals([Statement2a],[Statement2b],Vars1,Vars2%,Standard_or_e4
);
(Statement1a="|"->
match_put_vals(Statement2a,[Variables2],Vars1,Vars2%,Standard_or_e4
);
(Statement1b="|"->
match_put_vals([Variables1],Statement2b,Vars1,Vars2%,Standard_or_e4
);

% v=_1 if undef
% vsys x _

% v=[multi items] - insert vals in multi items - |, simplify, what if v is defined & in following
% v=v
% v=item
% item=item

% 
% 
(%Vars1=Vars3,
((((single_item_or_var(Statement1a)%,Vars1=Vars2
)->true;
(single_item_or_var(Statement1b)%,Vars1=Vars2
))->
	match4_new1(Statement1a,Statement1b,Vars1,Vars3%,Standard_or_e4
	)
	;
(
%contains_var1(Statement1a,Statement1b))->true;

%((variable_name(Statement1b),
%not(variable_name(Statement1a)),
%contains_var1(Statement1b,Statement1a))->true;

	
	match_put_vals(Statement1a,Statement1b,Vars1,Vars3%,Standard_or_e4
	)))),
	%Vars3=Vars4,
	match_put_vals(Statement2a,Statement2b,Vars3,Vars2%,Standard_or_e4
	))
	))),!.





match4_new1(Statement1,Statement2,Vars1,Vars2%,_Standard_or_e4
) :-
 %get_lang_word("v",Dbw_v), %(variable_name(Statement1)->(find_sys(Name1),Statement1a=[Dbw_v,Name1]);%Statement1a=Statement1),

 variable_name(Statement1),
 variable_name(Statement2),

 %(Standard_or_e4=standard->
 %putvalue(Statement1,empty,Vars1,Vars2);

 %getvalue_new2(Statement1,Value1,Vars1,Standard_or_e4),
 %getvalue_new2(Statement2,Value2,Vars1,Standard_or_e4),
 (Statement1=Statement2->Vars1=Vars2;
 putvalue(Statement2,Statement1,Vars1,Vars2)),!.

match4_new1(Statement1,Statement2,Vars1,Vars2%,_Standard_or_e4
) :-
 not(variable_name(Statement1)),
 variable_name(Statement2),
 %getvalue_new(Statement1,Value1,Vars1),
 %getvalue_new2(Statement2,Value2,Vars1,Standard_or_e4),

 putvalue(Statement2,Statement1,Vars1,Vars2).
 %match4_new2(Statement1,Value2,Vars1,Vars2,Standard_or_e4).

match4_new1(Statement1,Statement2,Vars1,Vars2%,_Standard_or_e4
) :-
 variable_name(Statement1),
 not(variable_name(Statement2)),
 %getvalue_new2(Statement1,Value1,Vars1,Standard_or_e4),
 %getvalue_new(Statement2,Value2,Vars1),

 putvalue(Statement1,Statement2,Vars1,Vars2).
 %match4_new2(Value1,Statement2,Vars1,Vars2,Standard_or_e4).

match4_new1(Statement1,Statement2,Vars1,Vars1%,_Standard_or_e4
) :-
 not(variable_name(Statement1)),
 not(variable_name(Statement2)),
 %getvalue_new(Statement1,Value1,Vars1),
 %getvalue_new(Statement2,Value2,Vars1),

Statement1=Statement2. %match4_new2(Statement1,Statement2,Vars1,Vars2,Standard_or_e4).


% A=B (B=B)

/*
getvalue_new2(Variable,Value,Vars,_) :-
	(not(variable_name(Variable))->Variable=Value;
	((member([Variable,Value],Vars),
	not(variable_name(Value)))->true;% is this required in getvalue_new1
	        (%not(member([Variable,_Value],Vars)),
	        (%Standard_or_e4=standard->
	        %Value=empty;
	        Variable=Value)))),!.
*/ 