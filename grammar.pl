%% Replace with grammar part, write grammar part, write grammars to convert string to compound

%% Replace with grammar part

%% Make atoms into strings as soon as entered x have both, because it is annoying to enter strings as strings* - prefer atoms because they're faster, use strings only when necessary - * get rid of atoms as strings and use atoms only as names in form [n,name], v

%% ** Need compound terms in LPI
%% convert_to_grammar_part1([[n,nouns],[[v,l1],[v,l2]]],"->",[[[n,word],[[v,noun1]]],[[n,word],[[v,noun2]]],[[n,code],[[n,append],[[v,l1],[[v,noun1],[v,noun2]],[v,l2]]]]]]],[],G).
%% convert_to_grammar_part1([[[n,noun],[[v,l1],[v,l2]],"->",[[[n,word],[[v,noun1]]]]]],[],G).
%% convert_to_grammar_part1([[[n,noun],"->",[[[n,word],[[v,noun1]]]]]],[],G).
%% convert_to_grammar_part1([[[n,noun],"->",[[[v,a]],[[n,code],[[n,=],[[v,a],"apple"]]]]]],[],G).
%% convert_to_grammar_part1([[[n,noun],"->",[[v,a],[[n,code],[[n,=],[[v,a],["apple"]]]]]]],[],G).
%% convert_to_grammar_part1([[[n,noun],"->",[["apple"]]]],[],G).
%% convert_to_grammar_part1([[[n,noun],"->",["apple"]]],[],G).

%% Notes
%% Brackets around body in input v

%% base case [] v
%% calls, terminals, code v
%% if calls - 2vp or non 2vp v

%% No bc needed except [] v
%% Recurse after all except bc [] v

%% code in converted code has no brackets v

%% $->v v

%% must have a call or terminal v

%% terminals have vgps, undef g t->g p v

%% can find x as terminals, can find singletons later v

%% grammar_part,[n,noun],[v,vgp1],[v,vgp2] should be
%% grammar_part,[[n,noun],[v,vgp1],[v,vgp2]] do using list proc v

%% append [[]] not [] to [[v v]] v

%% Print [] around body v

%% append list in first query args 1<->2 v

%% ** change [v,name] to [v*,name] so new interpreter, grammar command can run them, can have multiple v, v* or groups of these used in an interpreter shell x use a marker to avoid replacing variables with values when passing algorithms to shells, or detect and leave without marker

%% keep original length recorded to add base case in case when add variables
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
	
	%% member to check all doesn't work
	((maplist(basecasecondition(Variables3,[n,Name]),Grammar2))->
((Variables1=[]->(Grammar9=[[n,grammar_part],[[n,Name],[],[v,vgp]]],Grammar10=[[n,grammar_part],[[n,Name],"",[v,vgp]]]);fail %% need more for variable
	),append(Grammar2,[Grammar9,
	Grammar10,Grammar7],Grammar8));
	append(Grammar2,[%%Grammar9,
	Grammar7],Grammar8)),
	convert_to_grammar_part1(Grammar5,Grammar8,Grammar3),!.
convert_to_grammar_part1(Grammar1,Grammar2,Grammar3) :-
	Grammar1=[Grammar4|Grammar5],
   (((Grammar4=[_Name1,_Variables1,":-",_Body1]->true;
	Grammar4=[_Name2,":-",_Body2])->true;
	Grammar4=[_Name3,_Variables2])->true;
	Grammar4=[_Name4]),
	append(Grammar2,[Grammar4],Grammar6),
	convert_to_grammar_part1(Grammar5,Grammar6,Grammar3),!.
	
basecasecondition(Variables3,[n,Name],Item1) :-
	not((Item1=[[n,grammar_part],Item2|_Rest2],Item2=[[n,Name]|_Rest3],length(Variables3,Length),length(Item2,Length))).
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
	
%% write grammar part

%% [[]] in args

%% collect code commands to write


%% Terminal known, Phrase2 known
%% Terminal unknown, Phrase2 known
%% Terminal known, Phrase2 unknown
%% Terminal unknown, Phrase2 unknown - error
%% if known or unknown, can be [v]

%% Gets val or sets prolog var to undef, puts val in undef in var

%% Doesn't support [v], use wrap(v) x - append not string concat if P1 is ["a"]

getvalues2([],Values,Values,_Vars,Flags,Flags) :- !.
getvalues2(VarNames1,Values1,Values2,Vars,Flags1,Flags2) :-
	VarNames1=[VarName1|VarNames2],
	(VarName1=[VarName2]->Flag1=true;VarName2=VarName1),
	getvalue(VarName2,Value1,Vars),
	(Value1=empty->Flag2=true;(Value2=Value1,Flag2=false)),
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
	
