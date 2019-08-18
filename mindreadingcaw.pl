:- dynamic debug/1.
:- include('texttobr2qb').
:- include('mindreadtestshared').

%% Silly to mind read caw inputs, because they are mostly unused.  Use mind reading for definitely used multi choice answer

%% caw00(off,[n,f],[[[n,append],2,1],[[n,delete],2,1],[[n,head],1,1],[[n,tail],1,1],[[n,member],1,1]],2,8,_InputVarList,_OutputVarList,[],_Program2,Ps).
%% [[[[n,f],[[b,c],[b,a],[v,d]]],[[[n,f],[[v,a],[v,b],[v,d]],:-,[[[n,=],[[v,d],[v,a]]]]]],[[[v,d],[b,c]]]],[[[n,f],[[b,c],[b,a],[v,d]]],[[[n,f],[[v,a],[v,b],[v,d]],:-,[[[n,append],[[v,b],[v,a],[v,e]]],[[n,=],[[v,d],[v,e]]]]]],[[[v,d],[b,a,b,c]]]]]

/**

?- interpret(on,[[n,f],[["b","c"],["b","a"],[v,d]]],[[[n,f],[[v,a],[v,b],[v,d]],":-",[[[n,=],[[v,d],[v,a]]]]]],Result).
[call,[[n,f],[[b,c],[b,a],[v,d]]],Press c.]
[call,[[n,is],[variable,[b,c]]],Press c.]
[exit,[[n,is],[[v,d],[b,c]]],Press c.]
[exit,[[n,f],[[b,c],[b,a],[b,c]]],Press c.]
Result = [[[v, d], ["b", "c"]]].

?- interpret(on,[[n,f],[["b","c"],["b","a"],[v,d]]],[[[n,f],[[v,a],[v,b],[v,d]],":-",[[[n,append],[[v,b],[v,a],[v,e]]],[[n,=],[[v,d],[v,e]]]]]],Result).
[call,[[n,f],[[b,c],[b,a],[v,d]]],Press c.]
[call,[[n,append],[[b,a],[b,c],variable3]],Press c.]
[exit,[[n,append],[[b,a],[b,c],[b,a,b,c]]],Press c.]
[call,[[n,is],[variable,[b,a,b,c]]],Press c.]
[exit,[[n,is],[[v,d],[b,a,b,c]]],Press c.]
[exit,[[n,f],[[b,c],[b,a],[b,a,b,c]]],Press c.]
Result = [[[v, d], ["b", "a", "b", "c"]]].

**/

%% ML max 25
shell1(Command) :-
				(bash_command(Command,_)->
					true;
					(writeln(["Failed shell1 command: ",Command]),abort)
				).

bash_command(Command, Output) :-
        setup_call_cleanup(process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out)).


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

random1(N1) :-
	random2(N2),random2(N3), string_concat(N2,N3,S), number_string(N1,S).
	
random2(N) :-
	trialy2_30("0",H21),
	trialy2_30("1",H22),
	trialy2_30("2",H23),
	trialy2_30("3",H24),
	trialy2_30("4",H25),
	trialy2_30("5",H26),
	trialy2_30("6",H27),
	trialy2_30("7",H28),
	trialy2_30("8",H29),
	trialy2_30("9",H210),

	H2L=[H21,H22,H23,H24,H25,
	H26,H27,H28,H29,H210],
	sort(H2L,H2A),
	reverse(H2A,H2B),
	H2B=[[_,N]|_Rest2].

randvars(0,_,V,V) :- !.
randvars(N,L,V1,V2) :-
	
	random1(N0), N1 is N0/100, N2A is round(97+(N1*L)), char_code(V3,N2A), V31=[v,V3], ((member(V31,V1))->randvars(N,L,V1,V2);
	(append(V1,[V31],V4),
	NA is N-1, randvars(NA,L,V4,V2))),!.
randvars2(0,_L,V,V) :- !.
randvars2(N,L,V1,V2) :-
	random1(N0), N1 is N0/100, N2A is round(97+(N1*L)), char_code(V3,N2A), atom_string(V3,V4), %%V41=[v,V4],
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
	append(Programs2,[[Query,Program2,OutputVarList2]],Ps1).%% ,Programs3->Ps1
	%%caw1a(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program2,Programs3,Ps1),!.

%%caw1(_Query,_PredicateName,_Rules,_MaxLength,_VarList,_InputVars1,_InputVars2,_InputVars3,_OutputVarList,_OutputVars,_Program1,_Program4,Ps,Ps) :- writeln(here1),!.
caw1(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program2,Programs3,Programs3) :- !.
	%%writeln([here1,	caw1(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program21,Programs3,Programs3)]),!.

caw1a(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program4,Ps1,Ps2) :-
	
	%%writeln([caw(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,OutputVarList,OutputVars,Program1,Program4)]),
	%%MaxLength2 is MaxLength - 1,
	%%writeln(["ml",MaxLength2]),
	reverse(InputVars2,InputVars5),
	random1(N0), N1 is N0/100, length(Rules,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,Rules),
	List2=[[RuleName,NumInputs,NumOutputs]|_Rest]);fail),
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

caw(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program2,Programs3,Programs3) :-
	%%writeln([here2,	caw(Query,PredicateName,Rules,MaxLength2,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,[],_Program21,Programs3,Programs3)]),
	!.


cawa(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,InputVars3,OutputVarList,OutputVars,Program1,Program4,Ps1,Ps2) :-
	%%writeln([caw(Query,PredicateName,Rules,MaxLength,VarList,InputVars1,InputVars2,OutputVarList,OutputVars,Program1,Program4)]),
	%%MaxLength2 is MaxLength - 1,
	%%writeln(["ml",MaxLength2]),
	random1(N0), N1 is N0/100, length(Rules,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,Rules),
	List2=[[RuleName,NumInputs,NumOutputs]|_Rest]);fail),
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
	
	random1(N0), N1 is N0/100, length(VarList,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,VarList),
	List2=[Var|_Rest]);fail),

	random1(N01), N11 is N01/100, length(OutputVars1,L1), N21 is round(L1*N11)-1,
	(N21>=0->
	(length(List11,N21), append(List11,List21,OutputVars1),
	List21=[OutputVar|_Rest2]);fail),

	append(Program1,[[[n,=],[OutputVar,Var]]],Program3),
	addrules0(VarList,OutputVars1,OutputVars3,Program3,Program2),!.

addrules(_,_,[],PV,PV,Program,Program) :- !.
addrules(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2) :-
	restlast(VarList,[],_,Var),
	%%OutputVars2=[OutputVar|OutputVars3],

	random1(N0), N1 is N0/100, length(OutputVars2,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,OutputVars2),
	List2=[OutputVar|_Rest]);fail),

	delete(OutputVars1,OutputVar,OutputVars3),
%%	member(Var,VarList),

	random1(N01), N11 is N01/100, length(OutputVars1,L1), N21 is round(L1*N11)-1,
	(N21>=0->
	(length(List11,N21), append(List11,List21,OutputVars1),
	List21=[OutputVar|_Rest2]);fail),

	append(Program1,[[[n,=],[OutputVar,Var]]],Program3),
	append(PenultimateVars1,[Var],PenultimateVars3),
	addrules2(VarList,OutputVars1,OutputVars3,PenultimateVars3,PenultimateVars2,Program3,Program2),!.

addrules2(_,_,[],PV,PV,Program,Program) :- !.
addrules2(VarList,OutputVars1,OutputVars2,PenultimateVars1,PenultimateVars2,Program1,Program2) :-
%%	restlast(VarList,[],_,Var),
	OutputVars2=[OutputVar|OutputVars3],

	random1(N0), N1 is N0/100, length(VarList,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,VarList),
	List2=[Var|_Rest]);fail),

	not(member(Var,PenultimateVars1)),

	random1(N01), N11 is N01/100, length(OutputVars1,L1), N21 is round(L1*N11)-1,
	(N21>=0->
	(length(List11,N21), append(List11,List21,OutputVars1),
	List21=[OutputVar|_Rest2]);fail),

	append(Program1,[[[n,=],[OutputVar,Var]]],Program3),
	append(PenultimateVars1,[Var],PenultimateVars3),
	addrules2(VarList,OutputVars1,OutputVars3,PenultimateVars3,PenultimateVars2,Program3,Program2),!.

%% optimise([[append,[a,a,d]],[append,[a,a,e]],[append,[a,a,f]],[append,[a,b,g]]],[g],P).
/**
optimise(Program1,InputVars1,InputVars2,PenultimateVars,Program2) :-
	reverse(Program1,Program4),
	findrulesflowingtopv1(Program4,InputVars1,InputVars2,PenultimateVars,[],Rules,true),
	%%findrulesflowingtopv1a(Program1,_Program32,InputVars1,InputVars2,PenultimateVars,[],_Rules1),
	intersection(Program1,Rules,Program3),
	unique1(Program3,[],Program2).
findrulesflowingtopv1(_,_,_,[],Rules,Rules,false).
findrulesflowingtopv1(Program0,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	(atom(Var);length(Var,1)),
	findrulesflowingtopv20(Program0,Program0,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1).
findrulesflowingtopv1(Program0,InputVars1,InputVars2,Vars1,Rules1,Rules2,IV1Flag1) :-
	Vars1=[Var|Vars2],
	findrulesflowingtopv20(Program0,Program0,InputVars1,InputVars2,Var,Rules1,Rules3,IV1Flag2), 
	findrulesflowingtopv1(Program0,InputVars1,InputVars2,Vars2,Rules3,Rules2,IV1Flag3),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1).

%%findrulesflowingtopv2([],Program,Program,_,_,Rules,Rules).
findrulesflowingtopv20(_,[],_InputVars1,_InputVars2,_Var,Rules,Rules,false).
findrulesflowingtopv20(Program0,Rules4,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rules4=[Rule|Rules],
	(findrulesflowingtopv2(Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules3,IV1Flag2)->true;(Rules3=Rules1,IV1Flag2=false)),
	%%delete(Program0,Rule,Program1),
	findrulesflowingtopv20(Program0,Rules,InputVars1,InputVars2,Var,Rules3,Rules2,IV1Flag3),%%p1->0
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1).
%%findrulesflowingtopv2(_,[],[],_,_,_,Rules,Rules).
findrulesflowingtopv2(Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[_PredicateName,Vars],
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program2),
	%%Program2=Program1,
	%%(not(intersection(Rulesx,Rules1))-> x
	%% append, append, unique1
	%%append(Rules1,[Rule],Rules3);Rules3=Rules1),

	%%member(Var2,Rest),
	%%member(Var2,InputVars1),

	length(Rest,Length1), Length1>=1,
	subtract(Rest,InputVars1,IV3s),
	length(IV3s,Length3),
	subtract(Rest,IV3s,IV1s),
	length(IV1s,Length2), Length2>=1,
	subtract(IV3s,InputVars2,[]),

	IV1Flag2=true,

	%%delete(Program0,Rule,Program1),

	%%(delete(Program0,Rule,Program3),
	%%iv3s1(IV3s,Program3,IV3s,[]),
	(Length3>=1->
	(findrulesflowingtopv1(Program0,InputVars1,InputVars2,IV3s,[],Rules5,IV1Flag3),not(Rules5=[]));
	(Rules5=[],IV1Flag3=false)),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag4),
	%%->true; Rules5=[],IV1Flag1=IV1Flag4),
	
	((findrulesflowingtopv1(Program0,InputVars1,InputVars2,IV1s,[],Rules6,IV1Flag5), %%iv1s->rest, etc
	iv1flagdisjunction(IV1Flag4,IV1Flag5,IV1Flag1))->true;(Rules6=[],IV1Flag1=IV1Flag4)),

	append([Rule],Rules1,Rules9),
	append(Rules9,Rules5,Rules7),
	append(Rules7,Rules6,Rules8),
	unique1(Rules8,[],Rules2).

/**
findrulesflowingtopv2(_Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[_PredicateName,Vars],
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program2),
	%%Program2=Program1,
	(not(member(Rule,Rules1))->
	append(Rules1,[Rule],Rules2);Rules2=Rules1),
	subset(Rest,InputVars2),

	intersection(Rest,InputVars1,Intersection),
	length(Intersection,0),

%%	not((member(Var2,Rest),
%%	member(Var2,InputVars1))),

	IV1Flag1=false.
**/
/**
findrulesflowingtopv2(Program0,Rule,InputVars1,InputVars2,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[_PredicateName,Vars],
	restlast(Vars,[],Rest,Var),	
	%%delete(Program1,[PredicateName,Vars],Program3),
	%%Program3=Program1,
	%%append(Rules1,[Rule],Rules3),
	subset(Rest,InputVars2),
	
	intersection(Rest,InputVars1,Intersection),
	length(Intersection,0),

%%	not((member(Var2,Rest),
%%	member(Var2,InputVars1))),

%%	delete(Program0,Rule,Program1),

	IV1Flag2=false,
	findrulesflowingtopv1(Program0,InputVars1,InputVars2,Rest,[],Rules4,IV1Flag3),
	%%not(Rules4=[]),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1),

	append(Rules1,[Rule],Rules7),
	append(Rules7,Rules4,Rules8),
	unique1(Rules8,[],Rules2).
**/
/**
%%->true;(Program2=Program1,Rules2=Rules1)).
findrulesflowingtopv2(Rule,Program0,Program1,_Program2,InputVars1,InputVars,Var,Rules1,Rules2,IV1Flag1) :-
	Rule=[PredicateName,Vars],
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program4),
	%%Program4=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules3),
	findrulesflowingtopv1(Program0,Program1,_Program2,InputVars1,InputVars,Rest,Rules3,Rules2,IV1Flag3),
	iv1flagdisjunction(IV1Flag2,IV1Flag3,IV1Flag1).

	%%findrulesflowingtopv2(Program5,Program2,Rest,Rules3,Rules2).

**/
iv1flagdisjunction(A,B,true) :-
	((A=true)->true; (B=true)),!.
iv1flagdisjunction(_,_,false) :- !.
/**
iv3s0([],_,IV3s1,IV3s2).
iv3s0(IV3s,Program0,IV3s1,IV3s2).
	IV3s=[IV3|IV3s3],
	iv3s1(IV3,Program0,IV3s1,IV3s4),
	iv3s0(IV3s3,Program0,IV3s4,IV3s2).
iv3s1(_,[],IV3s,IV3s).	
iv3s1(IV3,Program0,IV3s1,IV3s2) :-
	Program0=[Rule|Rules],
	iv3s2(IV3,Rule,IV3s1,IV3s3),
	iv3s1(IV3,Rules,IV3s3,IV3s2).
iv3s2(IV3,Rule,IV3s,IV3s1,IV3s2).
	Rule=[_PredicateName,Vars],
	restlast(Vars,[],_Rest,IV3),	
	delete(IV3s1,IV3,IV3s2).


findrulesflowingtopv1a(_,_,_,_,[],Rules,Rules).
findrulesflowingtopv1a(Program1,Program2,InputVars1,InputVars2,Var,Rules1,Rules2) :-
	atom(Var),
	findrulesflowingtopv2a(Program1,Program2,InputVars1,InputVars2,Var,Rules1,Rules2).
findrulesflowingtopv1a(Program1,Program2,InputVars1,InputVars2,Vars1,Rules1,Rules2) :-
	Vars1=[Var|Vars2],
	findrulesflowingtopv2(Program1,Program3,InputVars1,InputVars2,Var,Rules1,Rules3),
	findrulesflowingtopv1a(Program3,Program2,InputVars1,InputVars2,Vars2,Rules3,Rules2).
%%findrulesflowingtopv2([],Program,Program,_,_,Rules,Rules).
findrulesflowingtopv2a([],[],_,_,_,Rules,Rules).
findrulesflowingtopv2a(Program1,Program2,_InputVars1,InputVars2,Var,Rules1,Rules2) :-
	member([PredicateName,Vars],Program1),
	restlast(Vars,[],Rest,Var),
	(
%%delete(Program1,[PredicateName,Vars],Program2),
Program2=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules2),
	subset(Rest,InputVars2)).
findrulesflowingtopv2a(Program1,Program2,InputVars1,InputVars2,Var,Rules1,Rules2) :-
	member([PredicateName,Vars],Program1),
	restlast(Vars,[],Rest,Var),
	(
%%delete(Program1,[PredicateName,Vars],Program3),
Program3=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules3),
	subset(Rest,InputVars2)),
	findrulesflowingtopv1a(Program3,Program2,InputVars1,InputVars2,Rest,Rules3,Rules2).

%%->true;(Program2=Program1,Rules2=Rules1)).
findrulesflowingtopv2a(Program1,Program2,InputVars1,InputVars,Var,Rules1,Rules2) :-
	member([PredicateName,Vars],Program1),
	restlast(Vars,[],Rest,Var),
	%%delete(Program1,[PredicateName,Vars],Program4),
	Program4=Program1,
	append(Rules1,[[PredicateName,Vars]],Rules3),
	findrulesflowingtopv1a(Program4,Program2,InputVars1,InputVars,Rest,Rules3,Rules2).
	%%findrulesflowingtopv2(Program5,Program2,Rest,Rules3,Rules2).
	**/
**/
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

	random1(N0), N1 is N0/100, length(InputVars1,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,InputVars1),
	List2=[Var|_Rest]);fail),

	rule2(RuleName,Var,VarList,VarList2,Rule,Var1),
	restlast(InputVars1,[],_,Last), %% Last should be outputs - 2nd last rule?
	(Var=Last->true;Last=Var1),
	append(InputVars1,[Var1],InputVars2),!.
rule(RuleName,1,2,InputVars1,InputVars2,VarList,VarList2,Rule) :-

	random1(N0), N1 is N0/100, length(InputVars1,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,InputVars1),
	List2=[Var|_Rest]);fail),

        rule3(RuleName,Var,VarList,VarList2,Rule,Var1,Var2),
	restlast(InputVars1,[],_,Last),
	(Var=Last->true;
	(Last=Var1->true;Last=Var2)),
	append(InputVars1,[Var1,Var2],InputVars2),!.
rule(RuleName,2,0,InputVars,InputVars,VarList,VarList,Rule) :-
%%writeln([rule(RuleName,2,1,InputVars1,InputVars2,VarList,VarList2,Rule)]),
	random1(N0), N1 is N0/100, length(InputVars,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,InputVars),
	List2=[Var|_Rest]);fail),

	random1(N01), N11 is N01/100, length(InputVars,L1), N21 is round(L1*N11)-1,
	(N21>=0->
	(length(List11,N21), append(List11,List21,InputVars),
	List21=[Vara|_Rest2]);fail),

        rule6(RuleName,Var,Vara,_VarList,_VarList2,Rule),
	restlast(InputVars,[],_,Last),
%%writeln([last,Last]),
	(Var=Last->true;Vara=Last),!.
rule(RuleName,2,1,InputVars1,InputVars2,VarList,VarList2,Rule) :-
%%writeln([rule(RuleName,2,1,InputVars1,InputVars2,VarList,VarList2,Rule)]),
	random1(N0), N1 is N0/100, length(InputVars1,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,InputVars1),
	List2=[Var|_Rest]);fail),

	random1(N01), N11 is N01/100, length(InputVars1,L1), N21 is round(L1*N11)-1,
	(N21>=0->
	(length(List11,N21), append(List11,List21,InputVars1),
	List21=[Vara|_Rest2]);fail),

        rule4(RuleName,Var,Vara,VarList,VarList2,Rule,Var1),
	restlast(InputVars1,[],_,Last),
%%writeln([last,Last]),
	((Var=Last->true;Vara=Last)->true;
	(Last=Var1)),
%%writeln([var,Var,vara,Vara]),
	append(InputVars1,[Var1],InputVars2),!.
rule(RuleName,2,2,InputVars1,InputVars2,VarList,VarList2,Rule) :-

	random1(N0), N1 is N0/100, length(InputVars,L), N2 is round(L*N1)-1,
	(N2>=0->
	(length(List1,N2), append(List1,List2,InputVars),
	List2=[Var|_Rest]);fail),

	random1(N01), N11 is N01/100, length(InputVars,L1), N21 is round(L1*N11)-1,
	(N21>=0->
	(length(List11,N21), append(List11,List21,InputVars),
	List21=[Vara|_Rest2]);fail),

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

%% this goes from e.g. c not a to TotalVars
%% skip over vars, start from a x reassign vars to abc etc

%% if returning 12 from 12345 remove 345 args

%% try lowest possible number of vars first (return shortest program first), then keep on adding number of vars
%% can elimininate same program both here and in assessment verification
