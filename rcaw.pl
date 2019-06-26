:- dynamic debug/1.

caw00(Debug,PredicateName,Rules,MaxLength,TotalVars,_InputVarList,_OutputVarList,Program1,_Program2,Ps1) :-
	repeat,
	%%MaxLength2 is MaxLength + 1,
	%%TotalVars = MaxLength,
	randvars(MaxLength,MaxLength,[],RandVars),
	populatevars(RandVars,MaxLength,[],PV),
	Code is MaxLength + 1 + 97,
	char_code(Char,Code),
	OutputVarList=[[[v,Char],1]],
	retractall(debug(_)),
    	assertz(debug(Debug)),
	retractall(totalvars(_)),
    	assertz(totalvars(TotalVars)),
	caw0(PredicateName,Rules,MaxLength,PV,OutputVarList,Program1,_Program3,Ps),
	sort(Ps,Ps1),not(Ps1=[]),!.

randvars(0,_,V,V) :- !.
randvars(N,L,V1,V2) :-
	random(N1), N2A is round(97+(N1*L)), char_code(V3,N2A), V31=[v,V3], ((member(V31,V1))->randvars(N,L,V1,V2);
	(append(V1,[V31],V4),
	NA is N-1, randvars(NA,L,V4,V2))),!.
randvars2(0,_L,V,V) :- !.
randvars2(N,L,V1,V2) :-
	random(N1), N2A is round(97+(N1*L)), char_code(V3,N2A), atom_string(V3,V4), %%V41=[v,V4],
	((member(V4,V1))->randvars2(N,L,V1,V2);
	(append(V1,[V4],V5),
	NA is N-1, randvars2(NA,L,V5,V2))),!.
populatevars([],_,PV,PV) :- !.
populatevars([RV1|RVs],MaxLength2,PV1,PV2) :-
	randvars2(MaxLength2,MaxLength2,[],RV2),
	append(PV1,[[RV1,RV2]],PV3),
	populatevars(RVs,MaxLength2,PV3,PV2),!.

caw0(PredicateName,Rules,MaxLength,InputVarList,OutputVarList,Program1,Program2,Ps2) :-
	varnames(InputVarList,[],InputVars,[],InputValues),
	varnames(OutputVarList,[],OutputVars,[],_OutputValues),
	retractall(outputvars(_)),
    	assertz(outputvars(OutputVars)),
	append(InputVars,OutputVars,Vars11),
	%%Vars11=InputVars,
	%%Vars12=InputVars,
	append(InputValues,OutputVars,Vars2),
	%%append(InputValues,OutputValues,Values),
	Query=[PredicateName,Vars2],
	caw1(Query,PredicateName,Rules,MaxLength,Vars11,InputVars,InputVars,_,OutputVarList,OutputVars,Program1,Program2,[],Ps2), !.

caw1(_,_,_,0,_,_,_,_,_,_,_,_,Ps,Ps) :- !.
caw1(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program2,Programs2,Ps1) :-
	MaxLength2 is MaxLength - 1,
	addrules0(InputVars2,OutputVars,OutputVars,[],Program3),
	%%writeln([addrules(InputVars2,OutputVars,OutputVars,[],PenultimateVars,[],Program3)]),	
	%%optimise(Program1,InputVars1,InputVars2,PenultimateVars,Program4), %% IV2->3
	%%writeln([optimise(Program1,InputVars1,InputVars2,PenultimateVars,Program4)]),
	append(Program1,Program3,Program5),
	append(InputVars1,OutputVars,Vars2),
	Program2=[
        [PredicateName,Vars2,":-",
                Program5
        ]
        ],debug(Debug),

        %%writeln([interpret(Debug,Query,Program2,OutputVarList2)]),

	interpret(Debug,Query,Program2,OutputVarList2),
        %%writeln([interpret(Debug,Query,Program2,OutputVarList2)]),
	append(Programs2,[[Query,Program2,OutputVarList2]],Programs3),
	caw1a(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program2,Programs3,Ps1),!.

%%caw1(_Query,_PredicateName,_Rules,_MaxLength,_VarList,_InputVars1,_InputVars2,_InputVars3,_OutputVarList,_OutputVars,_Program1,_Program4,Ps,Ps) :- writeln(here1),!.
caw1(_Query,_PredicateName,_Rules,_MaxLength2,_VarList,_InputVars1,_InputVars2,_InputVars3,_OutputVarList,_OutputVars,[],_Program2,Programs3,Programs3) :- !.
	%%writeln([here1,	caw1(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program21,Programs3,Programs3)]),!.

caw1a(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program4,Ps1,Ps2) :-
	
	%%writeln([caw(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,OutputVarList,OutputVars,Program1,Program4)]),
	%%MaxLength2 is MaxLength - 1,
	%%writeln(["ml",MaxLength2]),
	reverse(InputVars2,InputVars5),
	random_member([RuleName,NumInputs,NumOutputs],Rules),
	%%writeln([member([RuleName,NumInputs,NumOutputs],Rules)]),
	%%writeln([rule(RuleName,NumInputs,NumOutputs,VarList,VarList2,Rule)]),
	rule(RuleName,NumInputs,NumOutputs,InputVars5,InputVars4,VarList,VarList2,Rule),
%%writeln([inputVars1,InputVars1]),
%%	writeln([rule(RuleName,NumInputs,NumOutputs,InputVars5,InputVars4,VarList,VarList2,Rule)]),
	append(Program1,[Rule],Program3),
	%%writeln([inputVars3,InputVars3]),
	%%InputVars2=InputVars3,
	%%writeln([program4,Program4]),

%%caw1a(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program4,Ps,Ps) :-
%%writeln([here,caw1a(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program4,Ps,Ps)])


caw(Query,PredicateName,Rules,MaxLength,VarList2,InputVars1,InputVars4,InputVars3,OutputVarList,OutputVars,Program3,Program4,Ps1,Ps2),!.


caw(_,_,_,0,_,_,_,_,_,_,_,_,Ps,Ps) :- !.
caw(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program2,Programs1,Programs2) :-
	MaxLength2 is MaxLength - 1,
	addrules(InputVars2,OutputVars,OutputVars,[],_PenultimateVars,[],Program3),
	%%writeln([addrules(InputVars2,OutputVars,OutputVars,[],PenultimateVars,[],Program3)]),	
	%%optimise(Program1,InputVars1,InputVars2,PenultimateVars,Program4), %% IV2->3
	%%writeln([optimise(Program1,InputVars1,InputVars2,PenultimateVars,Program4)]),
	append(Program1,Program3,Program5),
	append(InputVars1,OutputVars,Vars2),
	Program2=[
        [PredicateName,Vars2,":-",
                Program5
        ]
        ],debug(Debug),
%%***
%% () choose iv1 as args during caw, () eliminate e problem, could move forward in optimiser but don't need it v
%% should have a leading edge of 1 immediately previous (new output) as an arg in latest rule v, go backwards to choose latest possible args x, 3 x rules can have same as previous rule's output as an output x: at a time
%% chunks will solve having at least 1 rule that connects to last output
%% can optimise number of inputs

%% test member, = in caw

	%%writeln([interpret(Debug,Query,Program2,OutputVarList2)]),

	interpret(Debug,Query,Program2,OutputVarList2),
	%%writeln([interpret(Debug,Query,Program2,OutputVarList2)]),
	append(Programs1,[[Query,Program2,OutputVarList2]],Programs3),
	cawa(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program2,Programs3,Programs2),!.
%%caw(_,_,_,_,_,_,_,_,_,_,_,_,Ps,Ps) :- !.

caw(_Query,_PredicateName,_Rules,_MaxLength2,_VarList,_InputVars1,_InputVars2,_InputVars3,_OutputVarList,_OutputVars,[],_Program2,Programs3,Programs3) :-
	%%writeln([here2,	caw(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program21,Programs3,Programs3)]),
	!.


cawa(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program4,Ps1,Ps2) :-
	%%writeln([caw(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,OutputVarList,OutputVars,Program1,Program4)]),
	%%MaxLength2 is MaxLength - 1,
	%%writeln(["ml",MaxLength2]),
	random_member([RuleName,NumInputs,NumOutputs],Rules),
	%%writeln([member([RuleName,NumInputs,NumOutputs],Rules)]),
	%%writeln([rule(RuleName,NumInputs,NumOutputs,VarList,VarList2,Rule)]),
	rule(RuleName,NumInputs,NumOutputs,InputVars2,InputVars4,VarList,VarList2,Rule),
%%writeln([inputVars1,InputVars1]),
%%	writeln([rule(RuleName,NumInputs,NumOutputs,InputVars2,InputVars4,VarList,VarList2,Rule)]),
	append(Program1,[Rule],Program3),
	%%writeln([inputVars3,InputVars3]),
	%%InputVars2=InputVars3,
	%%writeln([program4,Program4]),
caw(Query,PredicateName,Rules,MaxLength,VarList2,InputVars1,InputVars4,InputVars3,OutputVarList,OutputVars,Program3,Program4,Ps1,Ps2), !.

varnames([],Vars,Vars,Values,Values) :- !.
varnames(VarList,Vars1,Vars2,Values1,Values2) :-
	VarList=[Var|Vars3],
	Var=[VarName,Value],
	append(Vars1,[VarName],Vars4),
	append(Values1,[Value],Values3),
	varnames(Vars3,Vars4,Vars2,Values3,Values2),!.

addrules0(_,_,[],Program,Program) :- !.
addrules0(VarList,OutputVars1,OutputVars2,Program1,Program2) :-
	OutputVars2=[OutputVar|OutputVars3],
	random_member(Var,VarList),
	random_member(OutputVar,OutputVars1),
	append(Program1,[[[n,=],[OutputVar,Var]]],Program3),
	addrules0(VarList,OutputVars1,OutputVars3,Program3,Program2),!.

addrules(_,_,[],PV,PV,Program,Program) :- !.
addrules(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2) :-
	restlast(VarList,[],_,Var),
	%%OutputVars2=[OutputVar|OutputVars3],
	random_member(OutputVar,OutputVars2),
	delete(OutputVars1,OutputVar,OutputVars3),
%%	member(Var,VarList),
	random_member(OutputVar,OutputVars1),
	append(Program1,[[[n,=],[OutputVar,Var]]],Program3),
	append(PenultimateVars1,[Var],PenultimateVars3),
	addrules2(VarList,OutputVars1,OutputVars3,PenultimateVars3,PenultimateVars2,Program3,Program2),!.

addrules2(_,_,[],PV,PV,Program,Program) :- !.
addrules2(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2) :-
%%	restlast(VarList,[],_,Var),
	OutputVars2=[OutputVar|OutputVars3],
	random_member(Var,VarList),
	not(member(Var,PenultimateVars1)),
	random_member(OutputVar,OutputVars1),
	append(Program1,[[[n,=],[OutputVar,Var]]],Program3),
	append(PenultimateVars1,[Var],PenultimateVars3),
	addrules2(VarList,OutputVars1,OutputVars3,PenultimateVars3,PenultimateVars2,Program3,Program2),!.

iv1flagdisjunction(A,B,true) :-
	((A=true)->true; (B=true)),!.
iv1flagdisjunction(_,_,false) :- !.


restlast([],_,_,_) :- fail, !.	
restlast([Last],Rest,Rest,Last) :-
	Last=[v,_],!.
restlast(Last,Rest,Rest,Last) :-
	length(Last,1),!.
restlast(Vars1,Rest1,Rest2,Last) :-
	Vars1=[Var|Vars2],
	append(Rest1,[Var],Rest3),
	restlast(Vars2,Rest3,Rest2,Last),!.



rule(RuleName,1,1,InputVars1,InputVars2,VarList,VarList2,Rule) :-
	random_member(Var,InputVars1),
	rule2(RuleName,Var,VarList,VarList2,Rule,Var1),
	restlast(InputVars1,[],_,Last), %% Last should be outputs - 2nd last rule?
	(Var=Last->true;Last=Var1),
	append(InputVars1,[Var1],InputVars2),!.
rule(RuleName,1,2,InputVars1,InputVars2,VarList,VarList2,Rule) :-
        random_member(Var,InputVars1),
        rule3(RuleName,Var,VarList,VarList2,Rule,Var1,Var2),
	restlast(InputVars1,[],_,Last),
	(Var=Last->true;
	(Last=Var1->true;Last=Var2)),
	append(InputVars1,[Var1,Var2],InputVars2),!.
rule(RuleName,2,0,InputVars,InputVars,VarList,VarList,Rule) :-
%%writeln([rule(RuleName,2,1,InputVars1,InputVars2,VarList,VarList2,Rule)]),
        random_member(Var,InputVars),
        random_member(Vara,InputVars),
        rule6(RuleName,Var,Vara,_VarList,_VarList2,Rule),
	restlast(InputVars,[],_,Last),
%%writeln([last,Last]),
	(Var=Last->true;Vara=Last),!.
rule(RuleName,2,1,InputVars1,InputVars2,VarList,VarList2,Rule) :-
%%writeln([rule(RuleName,2,1,InputVars1,InputVars2,VarList,VarList2,Rule)]),
        random_member(Var,InputVars1),
        random_member(Vara,InputVars1),
        rule4(RuleName,Var,Vara,VarList,VarList2,Rule,Var1),
	restlast(InputVars1,[],_,Last),
%%writeln([last,Last]),
	((Var=Last->true;Vara=Last)->true;
	(Last=Var1)),
%%writeln([var,Var,vara,Vara]),
	append(InputVars1,[Var1],InputVars2),!.
rule(RuleName,2,2,InputVars1,InputVars2,VarList,VarList2,Rule) :-
        random_member(Var,InputVars),
        random_member(Vara,InputVars),
        rule5(RuleName,Var,Vara,VarList,VarList2,Rule,Var1,Var2),
	restlast(InputVars1,[],_,Last),
	((Var=Last->true;Vara=Last)->true;
	(Last=Var1->true;Last=Var2)), %% make last var2, use different inputs from previous rule, make this line usea version of previous line as well (args from rule before that) - redo rules based on past programming
	append(InputVars1,[Var1,Var2],InputVars2),!.


rule2(RuleName,Var,VarList,VarList2,Rule,Var1) :-
	var(VarList,Var1,VarList2),
	Rule=[RuleName,[Var,Var1]],!.%%,
	%%member(Var,!.

rule3(RuleName,Var,VarList,VarList3,Rule,Var1,Var2) :-
        var(VarList,Var1,VarList2),
        var(VarList2,Var2,VarList3),
        Rule=[RuleName,[Var,Var1,Var2]],!.

rule6(RuleName,Var,Vara,_VarList,_VarList2,Rule) :-
        Rule=[RuleName,[Var,Vara]],!.

rule4(RuleName,Var,Vara,VarList,VarList2,Rule,Var1) :-
        var(VarList,Var1,VarList2),
        Rule=[RuleName,[Var,Vara,Var1]],!.

%%ae be with predicate support also
rule5(RuleName,Var,Vara,VarList,VarList3,Rule,Var1,Var2) :-
        var(VarList,Var1,VarList2),
        var(VarList2,Var2,VarList3),
        Rule=[RuleName,[Var,Vara,Var1,Var2]],!.

%%var(Item,Var,Vars,Vars) :-
%%	member([Item,Var],Vars).
var(Vars1,Var1,Vars2) :-
	length(Vars1,Vars1Length1),
	Vars1Length2 is Vars1Length1-1,
	length(Vars3,Vars1Length2),
	append(Vars3,[Var2],Vars1),
	Var2=[v,Var21],
	char_code(Var21,Var2Code1),
	Var2Code2 is Var2Code1 + 1,
	var2(Var2Code2,Var1),
	append(Vars1,[Var1],Vars2),!.

var2(Code,Var1) :-
	outputvars(OutputVars),
	totalvars(TotalVars),
	Code2 is 96+TotalVars,
	Code =< Code2, %% 122
	char_code(Var11,Code),
	Var1=[v,Var11],
	not(member(Var1,OutputVars)),!.
var2(Var2Code,Code3) :-
	Var2Code2 is Var2Code + 1,	
	totalvars(TotalVars),
	Code2 is 96+TotalVars,
	Var2Code2 =< Code2,
	var2(Var2Code2,Code31),
	Code3=[v,Code31],!.
