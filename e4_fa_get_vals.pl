
e4_fa_getvalues2([],Values,Values,_Vars,Flags,Flags) :- !.
e4_fa_getvalues2(VarNames1,Values1,Values2,Vars,Flags1,Flags2) :-
	VarNames1=[VarName1|VarNames2],
	(VarName1=[VarName2]->Flag1=true;VarName2=VarName1),
	e4_fa_getvalue(VarName2,Value1,Vars),
	(is_empty(Value1)->Flag2=true;(Value2=Value1,Flag2=false)),
	(Flag1=true->Value3=[Value2];Value3=Value2),
	append(Values1,Value3,Values3),
	append(Flags1,[Flag2],Flags3),
	e4_fa_getvalues2(VarNames2,Values3,Values2,Vars,Flags3,Flags2),!.

e4_fa_getvalues(Variable1,Variable2,Value1,Value2,Vars) :-
        e4_fa_getvalue(Variable1,Value1,Vars),
        e4_fa_getvalue(Variable2,Value2,Vars).
e4_fa_getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars) :-
        e4_fa_getvalue(Variable1,Value1,Vars),
        e4_fa_getvalue(Variable2,Value2,Vars),
        e4_fa_getvalue(Variable3,Value3,Vars).
        
e4_fa_getvalue(Variable,Value,Vars) :-
        ((not(isvar(Variable)),isvalstrorundef(Value),Variable=Value)->true;
        (isvar(Variable),isvalstrorundef(Value),e4_fa_getvar(Variable,Value,Vars))).
%putvalue(empty,A,Vars,Vars) :- %writeln1(A),
% not(isvar(A)),!.
putvalue(Variable,Value,Vars1,Vars2) :-
        ((not(isvar(Variable)),isvalstrorundef_or_compound(Value),Variable=Value,Vars1=Vars2)->true;
        (isvar(Variable),isvalstrorundef_or_compound(Value),updatevar(Variable,Value,Vars1,Vars2))),!. 
isvalstrorundef_or_compound(Value):-
	(compound(Value)->true;
	isvalstrorundef(Value)),!.
e4_fa_getvar(Variable,Value,Vars) :-
	((member([Variable,Value],Vars),
	not(is_empty(Value)))->true;
	        ((aggregate_all(count,member([Variable,_Value],Vars),0)->true;%%
	(member([Variable,Empty],Vars),is_empty(Empty))),Value=Variable))
.
e4_fa_getvar(undef,undef,_Vars) :-
	!.
	
e4_fa_val1emptyorvalsequal(_Empty,_Value) :- true, !.

%e4_fa_val1emptyorvalsequal(Value,Value) :-
%	not(Value=empty).
	
	
collect_vars([],Vars,Vars) :- !.
collect_vars(Term,Vars1,Vars2) :-
	not(variable_name(Term)),
	Term=[Term1|Term2],
	collect_vars(Term1,Vars1,Vars3),
	collect_vars(Term2,Vars3,Vars2),!.
collect_vars(Term,Vars1,Vars2) :-
	variable_name(Term),
	append(Vars1,[Term],Vars2),!.
collect_vars(Term,Vars,Vars) :-
	not(variable_name(Term)),!.

equals4_first_args(Variable1,Variable2,First_args2) :-
%trace,
	length(Variable1,Length),
	
equals4_first_args1(1,Length,Variable1,Variable2,[],First_args2),!.

equals4_first_args1(Length2,Length1,[],[],First_args,First_args) :-
	Length2 is Length1+1,!.
equals4_first_args1(Length0,Length1,Variable1,Variable2,First_args0,First_args01) :-
	Variable1=[Item1|Item1a],
	Variable2=[Item2|Item2a],
	%numbers(Length,1,[],N),
	%findall(First_args%First_args1
	%,(member(N1,N),
	%get_item_n(Variable1,N1,Item1),
	%get_item_n(Variable2,N1,Item2),
	%trace,
	%e4_fa_match4_2([Item1],[Item2],[],First_args1),
	match4_new_22([Item1],[Item2],[],First_args1%,e4
	),

	collect_vars(Item2,[],Vars2),
	%trace,
	findall(Value2,(member([First_args5,Value],First_args1),
	(not(member(First_args5,Vars2))->Value2=[First_args5,Value];
	(not(expression_not_var(Value)),Value2=[Value,First_args5]))),First_args6),
	
	%(First_args2=[]->First_args6=First_args2;First_args6=First_args2),
	%maplist(append,First_args6,[First_args]),
	%*append(First_args6,[First_args),
	%*),First_args3),
	%trace,
	%maplist(append,[First_args3],[First_args4]),
	%*delete(First_args3,[],First_args2),

	append(First_args0,First_args6,First_args02),
	
	Length2 is Length0+1,
	equals4_first_args1(Length2,Length1,Item1a,Item2a,First_args02,First_args01),

	!.
	/*
	collect_vars(Item1,[],Vars1),
	collect_vars(Item2,[],Vars2),
	%trace,
	findall(First_args5,(member([First_args3,Value],First_args1),
	(member(First_args3,Vars1)->First_args5=[First_args3,Value];
	((member(First_args3,Vars2),not(Vars1=[]))->First_args5=[Value,First_args3]->true;
	First_args5=[First_args3,Value]))),First_args6),
%	maplist(append,[First_args0],[First_args]),
	%trace,
	findall([First_args3,Value],(member([First_args3,Value],First_args6),
	(not(member(First_args3,Vars2))->true;
	(member(First_args3,Vars1),member(First_args3,Vars2)))
	),First_args0),
	maplist(append,[First_args0],[First_args])
	),First_args4),
	delete(First_args4,[],First_args2),
	%maplist(append,[First_args4],[First_args2]),
	!.
*/

e4_updatevars([],_,Vars2,Vars2) :- !.
e4_updatevars(FirstArgs,Vars1,Vars2,Vars3) :-
%writeln1(e4_updatevars_1(FirstArgs,Vars1,Vars2,Vars3)),
%trace,
	%get_lang_word("v",Dbw_v),
	FirstArgs=[[Orig,New]|Rest],
	(expression_not_var(New)->append(Vars2,[[Orig,New]],Vars5);
	(%trace,
	replace_vars([Orig],[],[Orig1],[],First_vars1),
	
	%member([New,Value],Vars1),
	(not(expression_not_var(Orig1))->
	(	remember_and_turn_off_debug(Debug),
%trace,
%find_sys(Sys_name),


	(interpretpart(match4,Orig1,New,Vars1,Vars4,_)->true;(turn_back_debug(Debug),fail)),
	%trace,
	collect_vars(Orig1,[],Orig2),
	findall([O1,O2],(member([O1,O2],Vars4),member(O1,Orig2)),Vars6),
	%subtract(Vars4,Vars1,Vars6),
		%getvalue(Orig,Value,Vars4),

	turn_back_debug(Debug),

		replace_first_vars1(Vars6,First_vars1,[],Vars61),

	append(Vars2,Vars61,Vars5)));
	Vars2=Vars5)),

	%append(Vars2,[[Orig,Value]],Vars4))),
	e4_updatevars(Rest,Vars1,Vars5,Vars3),!.
	
	
/*
replace_vars(Term,Vars1,Vars2,First_vars1,First_vars2) :-
	not(variable_name(Term)),
	Term=[[Term1|Term1a]|Term2],
	not(variable_name([Term1|Term1a])),
	replace_vars([Term1a],Vars1,Vars3,First_vars1,First_vars3),
	replace_vars(Term1,Vars3,Vars4,First_vars3,First_vars4),
	replace_vars(Term2,Vars4,Vars2,First_vars4,First_vars2),!.
	*/
%replace_vars(Term,Vars1,Vars2,First_vars1,First_vars2) :-
%	replace_vars0(Term,Vars1,Vars2,First_vars1,First_vars2),!.
	

replace_vars(Term,_Vars1,X,First_vars1,First_vars2) :-
	% for first vars only
	replace_vars0(Term,First_vars1,First_vars2),
	% for vars
	replace_vars01(Term,X,First_vars2),!.

replace_vars0([],Variable,Variable,First_vars,First_vars) :- !.

replace_vars0(Term,First_vars,First_vars) :-
	expression_not_var(Term),
	!.
replace_vars0(Term,First_vars1,First_vars2) :-
	not(variable_name(Term)),
	Term=[Term1|Term2],
	replace_vars0(Term1,First_vars1,First_vars3),
	%Vars5=[Vars3],
	replace_vars0(Term2,First_vars3,First_vars2),
	%Vars5=[Vars4],
	%trace,
	%append(Vars1],Vars3,Vars4],Vars2),
	%append(Vars1,Vars5,Vars6),
	%append(Vars6,Vars4,Vars2),
	%maplist(append,[[Vars1],Vars3,Vars4],Vars2),
	!.
replace_vars0(Term,First_vars1,First_vars2) :-
	get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,
	variable_name(Term),
	(member([Term,[Dbw_v,_Var_name1]],First_vars1)->
	(First_vars1=First_vars2);
	(find_sys(Var_name2),
	append(First_vars1,[[Term,[Dbw_v,Var_name2]]],First_vars2))),
	!.
	
replace_first_vars1([],_First_vars,Vars,Vars) :- !.
replace_first_vars1(Vars1,First_vars,Vars2,Vars3) :-
	%get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,
	Vars1=[[Var_name,Value]|Vars5],
	(member([Term,Var_name],First_vars)->
	append(Vars2,[[Term,Value]],Vars4);
	append(Vars2,[[Var_name,Value]],Vars4)),
	replace_first_vars1(Vars5,First_vars,Vars4,Vars3),!.

replace_first_vars2([],_First_vars,Vars,Vars) :- !.
replace_first_vars2(Vars1,First_vars,Vars2,Vars3) :-
	%get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,
	Vars1=[[Var_name1,Var_name2]|Vars5],
	%(member([Term1,Var_name1],First_vars)->
	%Term2=Term1;Term2=Var_name1),
	replace_first_vars211(Var_name1,First_vars,Term2),
	%(member([Term3,Var_name2],First_vars)->
	%Term4=Term3;Term4=Var_name2),
	replace_first_vars211(Var_name2,First_vars,Term4),
	append(Vars2,[[Term2,Term4]],Vars4),
	replace_first_vars2(Vars5,First_vars,Vars4,Vars3),!.


%*

%replace_first_vars21(Variable2,_,Vars1,X,FirstArgs1,FirstArgs2) :-
%	is_list(Variable2),
%	replace_first_vars211(Variable2,X,Vars1,FirstArgs1,FirstArgs2).

replace_first_vars211(Var_name1,First_vars,Term2) :-
	single_item_or_var(Var_name1),
	(member([Term1,Var_name1],First_vars)->
	Term2=Term1;Term2=Var_name1),
	%append(Term3,[Term2],Term4),
	!.

replace_first_vars211([],_,[]) :- !.
replace_first_vars211(Variable1,First_vars,Term) :-
	not(single_item_or_var(Variable1)),
	Variable1=[Variable1a|Variable1b],
	replace_first_vars211(Variable1a,First_vars,Value1a),
	replace_first_vars211(Variable1b,First_vars,Value1b),
	append([Value1a],Value1b,Term),!.

%*

is_single_item_or_expression_list(A) :-
	not(variable_name(A)),
	(single_item(A)->true;
	(is_list(A),findall(B,(member(B,A),expressionnotatom(B)
	%not(variable_name(B))
	),C),length(A,L),length(C,L))),
	!.

is_single_item_or_expression_list_with_atoms(A) :-
	not(variable_name(A)),
	(single_item_or_atom(A)->true;
	(is_list(A),findall(B,(member(B,A),expression_or_atom(B)
	%not(variable_name(B))
	),C),length(A,L),length(C,L))),
	!.

	
	%	replace_vars0(Term,Vars1,Vars2,First_vars1,First_vars2),!.

replace_vars01(Variable2,X,Vars1) :-
	not(variable_name(Variable2)),
	is_list(Variable2),
	getvalue_match1(Variable2,X,Vars1).

getvalue_match1(Variable1,Value1,Vars1) :-
	get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,
	single_item_or_atom(Variable1),
	(is_single_item_or_expression_list_with_atoms(Variable1)->
	Value1=Variable1;

	(member([Variable1,[Dbw_v,Var_name1]],Vars1)->
	(Value1=[Dbw_v,Var_name1]);
	(find_sys(Var_name2),
	Value1=[Dbw_v,Var_name2]))),!.


getvalue_match1([],[],_Vars1) :- !.
getvalue_match1(Variable1,Value1,Vars1) :-
	not(single_item_or_atom(Variable1)),
	Variable1=[Variable1a|Variable1b],
	getvalue_match1(Variable1a,Value1a,Vars1),
	getvalue_match1(Variable1b,Value1b,Vars1),
	append([Value1a],Value1b,Value1),!.

% get new var listing

/*
replace_vars011(Variable2,Vars1,Vars2a,Vars2b) :-
	not(variable_name(Variable2)),
	is_list(Variable2),
	getvalue_match11(Variable2,Vars1,Vars2a,Vars2b).

getvalue_match11(Variable1,Vars1,Vars2a,Vars2b) :-
	single_item(Variable1),
	(is_single_item_or_expression_list(Variable1)->
	Vars2a=Vars2b%append(Vars2a,[Variable1],Vars2b
	;
	(getvalue(Variable1,Value2,Vars1),
	append(Vars2a,[[Variable1,Value2]],Vars2b))),!.

getvalue_match11([],_Vars1,Vars2,Vars2) :- !.
getvalue_match11(Variable1,Vars1,Vars2a,Vars2b) :-
	not(single_item(Variable1)),
	Variable1=[Variable1a|Variable1b],
	getvalue_match11(Variable1a,Vars1,Vars2a,Vars2c),
	getvalue_match11(Variable1b,Vars1,Vars2c,Vars2b),
	%append([Value1a],Value1b,Value1),!.
	!.
*/

replace_vars011(Variable2,_Vars1,_Vars2a,Vars2b) :-
	get_lang_word("sys",Dbw_sys),

	findall([[A,C],C1],(member([[A,C],C1],Variable2),
	string_concat(Dbw_sys,_N1,C)),Vars2c),
	subtract(Variable2,Vars2c,Vars2b),!.


e4_substitutevarsA1(Variable2,Vars1,_,X,FirstArgs1,FirstArgs2) :-
	is_list(Variable2),
	e4_substitutevarsA2_getvalue_match1(Variable2,X,Vars1,FirstArgs1,FirstArgs2).

e4_substitutevarsA2_getvalue_match1(Variable1,Value1,Vars1,FirstArgs1,FirstArgs3) :-
	single_item_or_var(Variable1),
	getvalue(Variable1,Value,Vars1),
	((is_empty(Value)->
	((Value1=Variable1),
	(isvar(Variable1)->append(FirstArgs1,[Variable1],
	FirstArgs3);FirstArgs3=FirstArgs1));
	(getvalue(Variable1,Value,Vars1),
	Value1=Value,
	FirstArgs3=FirstArgs1))),!.

e4_substitutevarsA2_getvalue_match1([],[],_Vars1,FirstArgs1,FirstArgs1) :- !.
e4_substitutevarsA2_getvalue_match1(Variable1,Value1,Vars1,FirstArgs1,FirstArgs2) :-
	not(single_item_or_var(Variable1)),
	Variable1=[Variable1a|Variable1b],
	e4_substitutevarsA2_getvalue_match1(Variable1a,Value1a,Vars1,FirstArgs1,FirstArgs3),
	e4_substitutevarsA2_getvalue_match1(Variable1b,Value1b,Vars1,FirstArgs3,FirstArgs2),
	append([Value1a],Value1b,Value1),!.

