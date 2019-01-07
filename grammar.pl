convert_to_grammar_part1([],Grammar,Grammar) :- !.
convert_to_grammar_part1(Grammar1,Grammar2,Grammar3) :-
	Grammar1=[Grammar4|Grammar5],
	(Grammar4=[[n,Name],Variables1,"->",Body1]->true;
	(Grammar4=[[n,Name],"->",Body1],Variables1=[])),
	append([[v,vgp1],[v,vgp2]],Variables1,Variables2),
	member(Item1,Body1),call_or_terminal(Item1),	%% If not, operator expected.
	append([[n,Name]],Variables2,Variables3),
	Grammar6=[[n,grammar_part],Variables3,":-"],
	convert_to_grammar_part2(Body1,1,3,2,[],Body2),
	append(Grammar6,[Body2],Grammar7),
	append(Grammar2,[Grammar7],Grammar8),
	convert_to_grammar_part1(Grammar5,Grammar8,Grammar3),!.
convert_to_grammar_part1(Grammar1,Grammar2,Grammar3) :-
	Grammar1=[Grammar4|Grammar5],
   (((Grammar4=[_Name1,_Variables1,":-",_Body1]->true;
	Grammar4=[_Name2,":-",_Body2])->true;
	Grammar4=[_Name3,_Variables2])->true;
	Grammar4=[_Name4]),
	append(Grammar2,[Grammar4],Grammar6),
	convert_to_grammar_part1(Grammar5,Grammar6,Grammar3),!.
	convert_to_grammar_part2([],_FirstVar,_SecondVar,_SecondVarParent,Body,Body) :- !.

%% only want 2vp to be used if last call

convert_to_grammar_part2(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3) :-
	Body1=[Item|Items],
	call1(Item),
	(last_call_or_terminal2(Items)->
		convert_to_grammar_part31(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3);
		convert_to_grammar_part32(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3)), !.

%%
convert_to_grammar_part2(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3) :-
	Body1=[Item|Items],
	terminal(Item),
	(last_call_or_terminal2(Items)->
		convert_to_grammar_part31t(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3);
		convert_to_grammar_part32t(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3)), !.

convert_to_grammar_part2(Body1,FirstVar,SecondVar,SecondVarParent,Body2,Body3) :-
	Body1=[Item|Rest1],
	Item=[[n,code]|Rest2],
	append(Body2,Rest2,Body4),
	convert_to_grammar_part2(Rest1,FirstVar,SecondVar,SecondVarParent,Body4,Body3),!.
	
convert_to_grammar_part31(Body1,FirstVar,SecondVar,SecondVarParent,Body2,Body3) :-
	Body1=[Item1|Rest1],
	convert_to_grammar_part311(Item1,FirstVar,SecondVarParent,Body2,Body4),
	convert_to_grammar_part2(Rest1,FirstVar,SecondVar,SecondVarParent,Body4,Body3), !.

convert_to_grammar_part311([[n,RuleName]],FirstVar1,SecondVarParent,Body2,Body3) :-
	to_variable_name(FirstVar1,FirstVarName),
	to_variable_name(SecondVarParent,SecondVarParentName),
	Call=[[n,RuleName],FirstVarName,SecondVarParentName],
	append([[n,grammar_part]],[Call],Item),
	append(Body2,[Item],Body3),!.

convert_to_grammar_part311([[n,RuleName],Variables1],FirstVar1,SecondVarParent,Body2,Body3) :-
	variables(Variables1),
	to_variable_name(FirstVar1,FirstVarName),
	to_variable_name(SecondVarParent,SecondVarParentName),
	append([[n,RuleName],FirstVarName,SecondVarParentName],Variables1,Call),
	append([[n,grammar_part]],[Call],Item2),
	append(Body2,[Item2],Body3),!.

convert_to_grammar_part32(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3) :-
	Body1=[Item1|Rest1],
	convert_to_grammar_part321(Item1,Rest1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3),!.

convert_to_grammar_part321(Item1,Rest1,FirstVar1,SecondVar1,SecondVarParent,Body2,Body3) :-
	Item1=[[n,RuleName]],
	to_variable_name(FirstVar1,FirstVarName),
	to_variable_name(SecondVar1,SecondVarName),
	Call=[[n,RuleName],FirstVarName,SecondVarName],
	append([[n,grammar_part]],[Call],Item2),
	append(Body2,[Item2],Body4),
	FirstVar2 is SecondVar1,
	SecondVar2 is SecondVar1+1,
	convert_to_grammar_part2(Rest1,FirstVar2,SecondVar2,SecondVarParent,Body4,Body3),!.

convert_to_grammar_part321(Item1,Rest1,FirstVar1,SecondVar1,SecondVarParent,Body2,Body3) :-
	Item1=[[n,RuleName],Variables1],
	variables(Variables1),
	to_variable_name(FirstVar1,FirstVarName),
	to_variable_name(SecondVar1,SecondVarName),
	append([[n,RuleName],FirstVarName,SecondVarName],Variables1,Call),
	append([[n,grammar_part]],[Call],Item2),
	append(Body2,[Item2],Body4),
	FirstVar2 is SecondVar1,
	SecondVar2 is SecondVar1+1,
	convert_to_grammar_part2(Rest1,FirstVar2,SecondVar2,SecondVarParent,Body4,Body3),!.

%%

convert_to_grammar_part31t(Body1,FirstVar,SecondVar,SecondVarParent,Body2,Body3) :-
	Body1=[Item1|Rest1],
	convert_to_grammar_part311t(Item1,FirstVar,SecondVarParent,Body2,Body4),
	convert_to_grammar_part2(Rest1,FirstVar,SecondVar,SecondVarParent,Body4,Body3), !.

convert_to_grammar_part311t(Item1,FirstVar1,SecondVarParent,Body2,Body3) :-
	to_variable_name(FirstVar1,FirstVarName),
	to_variable_name(SecondVarParent,SecondVarParentName),
	Call=[Item1,FirstVarName,SecondVarParentName],
	append([[n,grammar_part]],[Call],Item2),
	append(Body2,[Item2],Body3),!.

convert_to_grammar_part32t(Body1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3) :-
	Body1=[Item1|Rest1],
	convert_to_grammar_part321t(Item1,Rest1,FirstVar1,SecondVar,SecondVarParent,Body2,Body3),!.

convert_to_grammar_part321t(Item1,Rest1,FirstVar1,SecondVar1,SecondVarParent,Body2,Body3) :-
	to_variable_name(FirstVar1,FirstVarName),
	to_variable_name(SecondVar1,SecondVarName),
	Call=[Item1,FirstVarName,SecondVarName],
	append([[n,grammar_part]],[Call],Item2),
	append(Body2,[Item2],Body4),
	FirstVar2 is SecondVar1,
	SecondVar2 is SecondVar1+1,
	convert_to_grammar_part2(Rest1,FirstVar2,SecondVar2,SecondVarParent,Body4,Body3),!.

last_call_or_terminal2([]) :- !.
last_call_or_terminal2(Body1) :-
	Body1=[Item|Body2],
	not(call_or_terminal(Item)),
	last_call_or_terminal2(Body2),!.
	
to_variable_name(Var,Name1) :-
	atom_concat(vgp,Var,Name2),
	Name1=[v,Name2],!.
variable_name([v,_Name]) :- !.
predicate_or_rule_name([n,_Name]) :- !.
predicate_or_rule([[n,_Name]|_Variables]) :- !.
variables([]) :- !.
variables(Variables1) :-
	Variables1=[Variable|Variables2],
	Variable=[v,_VariableName],
	variables(Variables2),!.
terminal(Item1) :-
	(Item1=[Item2]->
		terminal(Item2);
		(variable_name(Item1)->true;string(Item1))),!.
code(Item) :-
	Item=[code|_Rest],!.
call1([[n,_PredicateName]|_Variables]) :- !.
call_not_grammar([[n,PredicateName]|_Variables]) :- not(PredicateName=grammar),not(PredicateName=grammar_part),!.
call_grammar_part([[n,grammar_part]|_Variables]) :- !.

call_or_terminal(Item) :-
	terminal(Item)->true;call1(Item),!.
	
getvalues2([],Values,Values,_Vars,Flags,Flags) :- !.
getvalues2(VarNames1,Values1,Values2,Vars,Flags1,Flags2) :-
	VarNames1=[VarName1|VarNames2],
	(VarName1=[VarName2]->Flag1=true;VarName2=VarName1),
	getvalue(VarName2,Value1,Vars),
	(isvar(Value1)->(true,Flag2=true);(Value2=Value1,Flag2=false)),
	(Flag1=true->Value3=[Value2];Value3=Value2),
	append(Values1,Value3,Values3),
	append(Flags1,[Flag2],Flags3),
	getvalues2(VarNames2,Values3,Values2,Vars,Flags3,Flags2),!.
undefined_to_empty([],Values,Values) :- !.
undefined_to_empty(Values1,Values2,Values3) :-
	Values1=[Value1|Values4],
	(var(Value1)->Value2=empty;Value2=Value1),
	append(Values2,[Value2],Values5),
	undefined_to_empty(Values4,Values5,Values3),!.
	
