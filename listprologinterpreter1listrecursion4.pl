:- use_module(library(date)).

:- dynamic debug/1.
:- dynamic cut/1.
:- dynamic leash1/1.
:- dynamic types/1.
:- dynamic typestatements/1.
:- dynamic modestatements/1.

/** List Prolog Interpreter **/

interpret(Debug,Query,Functions1,Result) :-
	retractall(types(_)),
 	assertz(types(off)),
interpret11(Debug,Query,Functions1,Result).
interpret(Debug,Query,TypeStatements,ModeStatements,Functions1,Result) :-
	retractall(types(_)),
 	assertz(types(on)),
	retractall(typestatements(_)),
 	assertz(typestatements(TypeStatements)),
	retractall(modestatements(_)),
 	assertz(modestatements(ModeStatements)),
interpret11(Debug,Query,Functions1,Result).
interpret11(Debug,Query,Functions1,Result) :-
%%writeln1([i1]),
	%%writeln1(convert_to_grammar_part1(Functions1,[],Functions2,_)),
	convert_to_grammar_part1(Functions1,[],Functions2,_),
	%%writeln1(Functions2),
	%%pp3(Functions2),
	%%writeln1(interpret1(Debug,Query,Functions2,Functions2,Result)),
	findall(Result1,interpret1(Debug,Query,Functions2,Functions2,Result1),Result).
interpret1(Debug,Query,Functions1,Functions2,Result) :-
%%writeln1([i11]),
	retractall(debug(_)),
 	assertz(debug(Debug)),
   retractall(cut(_)),
   assertz(cut(off)),
	retractall(leash1(_)),
   assertz(leash1(off)), %% Should normally be off
	%%writeln1(member1(Query,Functions1,Functions2,Result)),
	member1(Query,Functions1,Functions2,Result).
%%member1([_,R],_,[],R).
%%member1(_,_,[],[]).
member1(_Query,_,_,[],_) :- %%writeln1(["The query",Query,"matches no predicates."]),
fail,!.
member1(Query,Functions,Functions2,Vars8) :-
%%writeln1([m1]),
	cut(off)->(
        (Query=[Function,Arguments1],
	(Functions2=[[Function,Arguments2,":-",Body]|_Functions3]),
	length(Arguments1,Length),
	length(Arguments2,Length),

checktypes_inputs(Function,Arguments1),
        
        %%writeln1(checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs)),
        checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),
        %%->ca2 
%%writeln1([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
debug_call(Skip,[Function,Arguments1]),
	(interpretbody(Functions,Functions2,Vars1,Vars2,Body,true)->debug_fail_fail(Skip);debug_fail(Skip,[Function,Arguments1])
	),
	%%writeln1(updatevars(FirstArgs,Vars2,[],Result)),
	updatevars(FirstArgs,Vars2,[],Result),
        %%reverse(Result,[],Vars7),
	((not(Result=[])->
        %%Result=[Var71|Vars72],
        %%writeln1(unique1(Result,[],Vars8)),
        unique1(Result,[],Vars8),
%%writeln1(["FirstArgs",FirstArgs,"Vars",Vars2,"Result",Result,"Vars7",Vars7,"Vars72",Vars72,"Var71",Var71,"Vars8",Vars8]),
%%writeln1(["Vars8",Vars8]),
	%%writeln1(findresult3(Arguments1,Vars8,[],Result2)),
	findresult3(Arguments1,Vars8,[],Result2)
%%writeln1([findresult3,"Arguments1",Arguments1,"Vars8",Vars8,"Result2",Result2])
	);(
%%writeln1(here1),
	Vars8=[],Result2=[])),
%%writeln1(["Arguments1",Arguments1,"Vars2",Vars2,"Result",Result]),
		debug_exit(Skip,[Function,Result2]),
		        checktypes(Function,Result2)
)
	;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2,":-",_Body]|Functions3], %% make like previous trunk?
	member11(Query,Functions,Functions2,Vars8))
	);(turncut(off)%%,Result=[]
	).
member11(Query,Functions,Functions2,Result) :-
%%writeln1([m11]),
%%writeln1(["Query",Query,"Functions",Functions,"Functions2",Functions2,"Result",Result]),
	cut(off)->(
        (Query=[Function],
        (Functions2=[[Function,":-",Body]|_Functions3]),
        debug_call(Skip,[Function]),
	Result=[],
        (interpretbody(Functions,Functions2,[],_Vars2,Body,true)->debug_fail_fail(Skip);debug_fail(Skip,[Function])),
    debug_exit(Skip,[Function])
	);
	(%%Query=[Function],
	%%Functions2=[[Function]|Functions3],
	member12(Query,Functions,Functions2,Result))
	);(turncut(off)).
member12(Query,Functions,Functions2,Vars8) :-
%%writeln1([m12]),
	cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        
        checktypes_inputs(Function,Arguments1),

        checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),
%%writeln1([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
	updatevars(FirstArgs,Vars1,[],Result),
        %%reverse(Result,[],Vars7),
        ((not(Result=[])->
        %%Result=[Var71|Vars72],
        unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
        );(
%%writeln1(here2),
	Vars8=[],Result2=[])),
        	debug_call(Skip,[Function,Arguments1]),
   debug_exit(Skip,[Function,Result2]),
	checktypes(Function,Result2)

	);
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2]|Functions3],
	member13(Query,Functions,Functions2,Vars8))
	);(turncut(off)).
member13(Query,Functions,Functions2,Result) :-
%%writeln1([m13]),
	cut(off)->(
        (Query=[Function],
        (Functions2=[[Function]|_Functions3]),
        debug_call(Skip,[Function]),
	Result=[],
        %%interpretbody(Functions,[],_Vars2,Body,true),
        debug_exit(Skip,[Function])
	);%%->true;
	(%%Query=[Function],
	Functions2=[_Function|Functions3],
	member1(Query,Functions,Functions3,Result))
	);(turncut(off)).
interpret2(Query,Functions1,Functions2,Result) :-
%%writeln1(i2),
%%writeln1(["%%interpret2 Query",Query,"Functions1",Functions1,"Functions2",Functions2]),
        member2(Query,Functions1,Functions2,Result).
%%member2([_,R],_,[],R).
%%member2(_,_,[],[]).
member2(_Query,_,_,[],_) :- %%writeln1(["The query",Query,"matches no predicates."]),
fail,!.
member2(Query,Functions,Functions2,Vars8) :-
%%writeln1([m2]),
	cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2,":-",Body]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        
        checktypes_inputs(Function,Arguments1),

        checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),
        
%%writeln1([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
debug_call(Skip,[Function,Arguments1]),
        (interpretbody(Functions,Functions2,Vars1,Vars2,Body,true)->debug_fail_fail(Skip);
        debug_fail(Skip,[Function,Arguments1])), %%**arg2 change
%%writeln1(["Functions",Functions,"Functions2",Functions2,"Vars1",Vars1,"Vars2",Vars2,"Body",Body]),
        updatevars(FirstArgs,Vars2,[],Result),
        %%reverse(Result,[],Vars7),
        ((not(Result=[])->
        %%Result=[Var71|Vars72],
        unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
%%writeln1(["Vars2",Vars2,"Result",Result]),
        );(
	%%writeln1(here3),
	Vars8=[],Result2=[])),
   debug_exit(Skip,[Function,Result2]),
   checktypes(Function,Result2)

	);%%->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2,":-",_Body]|Functions3],
	member21(Query,Functions,Functions2,Vars8))
	);(turncut(off)).
member21(Query,Functions,Functions2,Result) :-
%%writeln1([m21]),
	cut(off)->(
        (Query=[Function],
        (Functions2=[[Function,":-",Body]|_Functions3]),
        Vars1=[],
		  debug_call(Skip,[Function]),
        (interpretbody(Functions,Functions2,Vars1,_Vars2,Body,true)->debug_fail_fail(Skip);
        debug_fail(Skip,[Function])), %%**arg2 change
        debug_exit(Skip,[Function])
	);%%->true;
	(%%Query=[Function],
	%%Functions2=[[Function]|Functions3],
	member22(Query,Functions,Functions2,Result))
	);(turncut(off)).
member22(Query,Functions,Functions2,Vars8) :-
%%writeln1([m22]),
	cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        
        checktypes_inputs(Function,Arguments1),

        checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),
%%writeln1([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
        updatevars(FirstArgs,Vars1,[],Result),
        %%reverse(Result,[],Vars7),
        ((not(Result=[])->
        %%Result=[Var71|Vars72],
        unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
        );(
%%writeln1(here4),
	Vars8=[],Result2=[])),
        	debug_call(Skip,[Function,Arguments1]),
        	debug_exit(Skip,[Function,Result2]),
	checktypes(Function,Result2)

	);%%->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2]|Functions3],
	member23(Query,Functions,Functions2,Vars8))
	);(turncut(off)).
member23(Query,Functions,Functions2,Vars8) :-
%%writeln1([m23]),
	cut(off)->(
        (Query=[Function],
        (Functions2=[[Function]|_Functions3]),
        	debug_call(Skip,[Function]),
	Vars8=[],
        	debug_exit(Skip,[Function])
	);%%->true;
	(%%Query=[Function],
	Functions2=[_Function|Functions3],
	member2(Query,Functions,Functions3,Vars8))
	);(turncut(off)).
	
checkarguments([],[],Vars,Vars,FirstArgs,FirstArgs) :- !. 
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :- %%
%%writeln1(1),
	Arguments1=[Value|Arguments3], %% Value may be a number, string, list or tree
	expressionnotatom3(Value),
	Arguments2=[Variable2|Arguments4],
	not(var(Variable2)),isvar(Variable2),
	putvalue(Variable2,Value,Vars1,Vars3),
	checkarguments(Arguments3,Arguments4,Vars3,Vars2,FirstArgs1,FirstArgs2),!.
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :- %%A
%%writeln1(2),
        Arguments1=[Variable|Arguments3], %% Value may be a number, string, list or tree
        not(var(Variable)),isvar(Variable),
        Arguments2=[Value|Arguments4],
        expressionnotatom3(Value),
        putvalue(Variable,Value,Vars1,Vars3),
	append(FirstArgs1,[[Variable,Value]],FirstArgs3),
        checkarguments(Arguments3,Arguments4,Vars3,Vars2,FirstArgs3,FirstArgs2),!.
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
%%writeln1(3),
        Arguments1=[Variable1|Arguments3],
	not(var(Variable1)),isvar(Variable1),
        Arguments2=[Variable2|Arguments4],
	not(var(Variable2)),isvar(Variable2),
	(getvalue(Variable2,Value,Vars1)),%%->true);Value=empty), 
	%%((Value=empty->Value1=Variable2;Value1=Value))),
        putvalue(Variable2,Value,Vars1,Vars3),
        append(FirstArgs1,[[Variable1,Variable2]],FirstArgs3),
        checkarguments(Arguments3,Arguments4,Vars3,Vars2,FirstArgs3,FirstArgs2),!.
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
%%writeln1(4),
        Arguments1=[Value1|Arguments3],
        expressionnotatom3(Value1),
        Arguments2=[Value1|Arguments4],
        expressionnotatom3(Value1),
        checkarguments(Arguments3,Arguments4,Vars1,Vars2,FirstArgs1,FirstArgs2),!.

%% checktypes([n,f],[1,"a",[n,a]],[[[n,f],[[t,number],[t,string],[t,predicatename]]]]).
%% checktypes([n,f],[1,1,1],[[[n,f],[[[t,list],[[t,number]]]]]]).
%% checktypes([n,f],[[1]],[[[n,f],[[[t,brackets],[[t,number]]]]]]).
%% checktypes([n,f],[1,"a",2,"b"],[[[n,f],[[[t,list],[[t,number],[t,string]]]]]]).
%% checktypes([n,f],[1,"a"],[[[n,f],[[t,a],[t,b]]],[[t,a],[[t,number]]],[[t,b],[[t,string]]]]).
%% Can write your own "any" type.


checktypes_inputs(Function,Vars1):-%%,TypeStatements1) :-
%%writeln(checktypes(Function,Vars1)),
	((types(on))->(typestatements(TypeStatements1),
	modestatements(ModeStatements1),
	checktypes0_inputs(Function,Vars1,TypeStatements1,ModeStatements1);true);true),!.
checktypes0_inputs(Function,Vars1,_TypeStatements1,_ModeStatements1) :- 
	length(Vars1,L),L is 0,Vars1=[],
	(types(on)->debug_types_call([Function,/,~,L,input,type,check]);true),
	
	
	(types(on)->debug_call(Skip,[Function,Vars1]);true),
		
	(types(on)->debug_exit(Skip,[Function,Vars1]);true),
	(types(on)->debug_types_exit([Function,/,~,L,input,type,check]);true).
checktypes0_inputs(Function,Vars1,TypeStatements1,ModeStatements1) :-
	length(Vars1,L),
	(types(on)->debug_types_call([Function,/,~,L,input,type,check]);true),
	
	
	(types(on)->
	((member([Function|[TypeStatements2]],TypeStatements1),
	member([Function|[ModeStatements2]],ModeStatements1),
	extract_modes1(TypeStatements2,TypeStatements3,Vars1,Vars2,ModeStatements2),
	debug_call(Skip,[Function,Vars2]);true),
	checktypes1(Vars2,TypeStatements3,TypeStatements3,TypeStatements1))->
	(
	(types(on)->debug_exit(Skip,[Function,Vars2]);true),
	(types(on)->debug_types_exit([Function,/,~,L,input,type,check]);true))
	
;(
	(types(on)->debug_fail(Skip,[Function,Vars1]);true),

(types(on)->debug_types_fail([Function,/,~,L,input,type,check]);true))).

extract_modes1(TypeStatements1,TypeStatements3,Vars1,Vars2,ModeStatements1) :-
	%%TypeStatements1=[TypeStatements2|TypeStatements3],
	extract_modes2(TypeStatements1,[],TypeStatements3,Vars1,[],Vars2,ModeStatements1).
	%%TypeStatements3=[TypeStatements3a|TypeStatements3].
extract_modes2([],TypeStatements2a,TypeStatements2a,[],Vars,Vars,[]) :- !.
extract_modes2(TypeStatements1,TypeStatements2a,TypeStatements3,Vars1,Vars2,Vars3,ModeStatements1) :-
	ModeStatements1=[input|ModeStatements3],
	TypeStatements1=[TypeStatements2|TypeStatements3a],
	Vars1=[Vars11|Vars12],
	append(TypeStatements2a,[TypeStatements2],TypeStatements4),
	append(Vars2,[Vars11],Vars4),
	extract_modes2(TypeStatements3a,TypeStatements4,TypeStatements3,Vars12,Vars4,Vars3,ModeStatements3).
extract_modes2(TypeStatements1,TypeStatements2a,TypeStatements3,Vars1,Vars2,Vars3,ModeStatements1) :-
	ModeStatements1=[output|ModeStatements3],
	TypeStatements1=[_TypeStatements2|TypeStatements3a],
	Vars1=[_Vars11|Vars12],
	extract_modes2(TypeStatements3a,TypeStatements2a,TypeStatements3,Vars12,Vars2,Vars3,ModeStatements3).


checktypes(Function,Vars1):-%%,TypeStatements1) :-
%%writeln(checktypes(Function,Vars1)),
	((types(on))->(typestatements(TypeStatements1),
	checktypes0(Function,Vars1,TypeStatements1);true);true),!.
checktypes0(Function,Vars1,_TypeStatements1) :- 
	length(Vars1,L),L is 0,Vars1=[],
	(types(on)->debug_types_call([Function,/,L,type,check]);true),
	
	
	(types(on)->debug_call(Skip,[Function,Vars1]);true),
		
	(types(on)->debug_exit(Skip,[Function,Vars1]);true),
	(types(on)->debug_types_exit([Function,/,L,type,check]);true).
		
checktypes0(Function,Vars1,TypeStatements1) :-
	length(Vars1,L),
	(types(on)->debug_types_call([Function,/,L,type,check]);true),
	
	
	(types(on)->debug_call(Skip,[Function,Vars1]);true),
	((member([Function|[TypeStatements2]],TypeStatements1),
	checktypes1(Vars1,TypeStatements2,TypeStatements2,TypeStatements1))->
	(
	(types(on)->debug_exit(Skip,[Function,Vars1]);true),
	(types(on)->debug_types_exit([Function,/,L,type,check]);true))
	
;(
	(types(on)->debug_fail(Skip,[Function,Vars1]);true),

(types(on)->debug_types_fail([Function,/,L,type,check]);true))).

checktypes1([],[],_,_) :- !.

	checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4) :-
%%writeln(checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4)),
	Vars1=[Vars2|Vars3],
	list(Vars2,_,_),
	TypeStatements1=[[[t,list]|[TypeStatements3]]|TypeStatements4a],
(types(on)->debug_call(Skip,[[t,list],TypeStatements3]);true),

	((checktypes3(Vars2,TypeStatements3,TypeStatements2,TypeStatements4))->
		(types(on)->debug_exit(Skip,[[t,list],Vars2]);true)
;     (types(on)->debug_fail(Skip,[[t,list],Vars2]);true)
),
	checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4). %% ** in brac as well

checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4) :-
%%writeln(checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4)),
	%%Vars1=[Vars2|Vars3],
	%%list(Vars1,_,_),%%length(Vars1,1),
	TypeStatements1=[[[t,list]|[TypeStatements3]]|_TypeStatements4a],
(types(on)->debug_call(Skip,[[t,list],TypeStatements3]);true),

	((checktypes3(Vars1,TypeStatements3,TypeStatements2,TypeStatements4))->
		(types(on)->debug_exit(Skip,[[t,list],Vars1]);true)
;     (types(on)->debug_fail(Skip,[[t,list],Vars1]);true)).
	%%checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4). %% ** in brac as well
	

checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4) :-
	TypeStatements1=[[[t,brackets]|[TypeStatements3]]|TypeStatements4a],
(types(on)->debug_call(Skip,[[t,brackets],TypeStatements3]);true),
	(([Vars2|Vars3]=Vars1,
	checktypes1(Vars2,TypeStatements3,TypeStatements2,TypeStatements4))->
		(types(on)->debug_exit(Skip,[[t,brackets],Vars1]);true)
;     (types(on)->debug_fail(Skip,[[t,brackets],Vars1]);true)),
	checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4). %% ** in brac as well

/**checktypes1(Vars1,TypeStatements0,TypeStatements1,TypeStatements4) :-
	((number(Vars1)->true);string(Vars1)->true;Vars1=[n,_]),
	%%Vars1=[Vars2|Vars3],
	%%TypeStatements0=[TypeStatements2|TypeStatements3],
	checktypes2(Vars1,TypeStatements0,TypeStatements1,TypeStatements4).
	%%checktypes1(Vars3,TypeStatements3,TypeStatements1,TypeStatements4).

**/
checktypes1(Vars1,TypeStatements0,TypeStatements1,TypeStatements4) :-
	Vars1=[Vars2|Vars3],
	TypeStatements0=[TypeStatements2|TypeStatements3],
	checktypes2(Vars2,TypeStatements2,TypeStatements1,TypeStatements4),
	checktypes1(Vars3,TypeStatements3,TypeStatements1,TypeStatements4).
	
checktypes2(Vars,TypeStatements1,_TypeStatements2,_C) :-
%%writeln(checktypes2(Vars,TypeStatements1,_TypeStatements2,C)),
TypeStatements1=[t,number],
(types(on)->debug_call(Skip,[[t,number],Vars]);true),
	((number(Vars))->
		(types(on)->debug_exit(Skip,[[t,number],Vars]);true)
;     (types(on)->debug_fail(Skip,[[t,number],Vars]);true)),!.
checktypes2(Vars,TypeStatements1,_TypeStatements2,_) :-
TypeStatements1=[t,predicatename],
(types(on)->debug_call(Skip,[[t,predicatename],Vars]);true),
	((Vars=[n,_])->
		(types(on)->debug_exit(Skip,[[t,predicatename],Vars]);true)
;     (types(on)->debug_fail(Skip,[[t,predicatename],Vars]);true)),!.

checktypes2(Vars,TypeStatements1,_TypeStatements2,_) :-
TypeStatements1=[t,string],
(types(on)->debug_call(Skip,[[t,string],Vars]);true),
	((string(Vars))->
		(types(on)->debug_exit(Skip,[[t,string],Vars]);true)
;     (types(on)->debug_fail(Skip,[[t,string],Vars]);true)),!.

checktypes2(Vars,TypeStatements1,_TypeStatements2,_) :-
TypeStatements1=[t,any],
(types(on)->debug_call(Skip,[[t,any],Vars]);true),
	((true)->
		(types(on)->debug_exit(Skip,[[t,any],Vars]);true)
;     (types(on)->debug_fail(Skip,[[t,any],Vars]);true)),!.

checktypes2(Vars,TypeStatements1,TypeStatements2,TypeStatements4) :-
TypeStatements1=[t,Type],(not(Type=list),not(Type=brackets),not(Type=number),not(Type=predicatename),not(Type=string),not(Type=any)),
(types(on)->debug_call(Skip,[[t,Type],Vars]);true),
	((
	member([[t,Type]|[TypeStatements3]],TypeStatements4),
	(checktypes1(Vars,TypeStatements3,TypeStatements2,TypeStatements4)->true;
		checktypes1([Vars],TypeStatements3,TypeStatements2,TypeStatements4)))->
		(types(on)->debug_exit(Skip,[[t,Type],Vars]);true)
;     (types(on)->debug_fail(Skip,[[t,Type],Vars]);true)),!.

/**
checktypes2(Vars,TypeStatements1,TypeStatements2,TypeStatements4) :-
	TypeStatements1=[t,Type],
	member([[t,Type]|[TypeStatements3]],TypeStatements4),
	checktypes1([Vars],TypeStatements3,TypeStatements2,TypeStatements4).
**/
checktypes3([],_,_TypeStatements2,_) :- !.
checktypes3(Vars,TypeStatements3,TypeStatements2,TypeStatements6) :-
%%writeln(checktypes3(Vars,TypeStatements3,TypeStatements2,TypeStatements6)),
	length(TypeStatements3,L),
	length(L1,L),
	append(L1,L2,Vars),
	%%[L10]=L1,
	%%TypeStatements3=[TypeStatements4|TypeStatements5],
	%%findall(L10,(member(L10,L1),checktypes2(L10,TypeStatements4,TypeStatements2,TypeStatements6)),B),
	checktypes1(L1,TypeStatements3,TypeStatements2,TypeStatements6),
	checktypes3(L2,TypeStatements3,TypeStatements2,TypeStatements6).






interpretbody(_Functions1,_Functions2,Vars,Vars,[],true) :- !.



/**
interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
        Body=[[[n,not],Statement]
        ],
	
	writeln1(interpretbody(Functions0,Functions,Vars1,Vars3,Statement,Result2)),
	not(interpretbody(Functions0,Functions,Vars1,Vars3,Statement,Result2)), %% 2->1
        ((Result2=cut)->!;true).
**/

/** *** may need to uncomment
interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
        Body=[Statements1|Statements2],not(Statements1=[[n,not],_]),not(predicate_or_rule_name(Statements1)),
        interpretbody(Functions0,Functions,Vars1,Vars3,Statements1,Result2),
        interpretbody(Functions0,Functions,Vars3,Vars2,Statements2,Result3),
        %%((Result3=cut)->!;true),
        logicalconjunction(Result1,Result2,Result3),!.
**/

interpretbody(Functions0,Functions,Vars1,Vars2,Body,_Result1) :-
        Body=[[Statements1|Statements1a]|Statements2
        ],
	
		not(predicate_or_rule_name(Statements1)),
%%writeln1(interpretbody(Functions0,Functions,Vars1,Vars3,[Statement],Result2)),
	interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],_Result2), %% 2->1

	interpretbody(Functions0,Functions,Vars3,Vars4,Statements1a,_Result22), %% 2->1
        %%((Result2=cut)->!;true),
        interpretbody(Functions0,Functions,Vars4,Vars2,Statements2,_Result3),
       %% ((Result3=cut)->!;true),
  %%()      logicalnot(Result2,Result4), 
%%()	(logicalconjunction(Result1,Result4,Result3)->true;(Result1=false)),
	!.



        
interpretbody(Functions0,Functions,Vars1,Vars2,Body,_Result1) :-
        Body=[[[n,not],[Statement]]|Statements2
        ],

debug_call(Skip,[[n,not]]),
        (	(not(interpretbody(Functions0,Functions,Vars1,_Vars3,[Statement],_Result22)), %% 2->1
        ((_Result2=cut)->!;true))->
debug_exit(Skip,[[n,not]])
;     debug_fail(Skip,[[n,not]])),
	%%writeln1(interpretbody(Functions0,Functions,Vars1,Vars3,[Statement],Result2)),

        interpretbody(Functions0,Functions,Vars1,Vars2,Statements2,_Result32),
        ((_Result3=cut)->!;true),
  %%()      logicalnot(Result2,Result4), 
%%()	(logicalconjunction(Result1,Result4,Result3)->true;(Result1=false)),
	!.
	
	


interpretbody(Functions0,Functions,Vars1,Vars2,Body,_Result1) :-
        Body=[[[n,or],[Statements1,Statements2]]|Statements3],
        (interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],_Result2); %% *** changed from 1 to Result2
	%%,((Value1=cut)->!;true));
        interpretbody(Functions0,Functions,Vars1,Vars3,[Statements2],_Result2)),%%!. *** changed from 1 to Result2

        interpretbody(Functions0,Functions,Vars3,Vars2,Statements3,_Result3),
        %%((Result3=cut)->!;true),
        %%logicalconjunction(Result1,Result2,Result3),
        !.


	%%,((Value=cut)->!;true)).
	%%(logicaldisjunction(Result1,Value1,Value2)->true;(Result1=false)).


interpretbody(Functions0,Functions,Vars1,Vars2,Body,_Result1) :-
        Body=[[[n,"->"],[Statements1,Statements2]]|Statements3],
        (interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],_Result2)-> 
                interpretbody(Functions0,Functions,Vars3,Vars4,[Statements2],_Result22)),

        interpretbody(Functions0,Functions,Vars4,Vars2,Statements3,_Result3),
        !.




interpretbody(Functions0,Functions,Vars1,Vars2,Body,_Result1) :-
        Body=[[[n,"->"],[Statements1,Statements2,Statements2a]]|Statements3],
        (interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],_Result2)-> 
                interpretbody(Functions0,Functions,Vars3,Vars4,[Statements2],_Result22);
                interpretbody(Functions0,Functions,Vars1,Vars4,[Statements2a],_Result23)),

        interpretbody(Functions0,Functions,Vars4,Vars2,Statements3,_Result3),
        !.


interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
	Body=[Statement|Statements],
%%writeln1(["Functions0",Functions0,"Functions",Functions,"Statement",Statement,"Vars1",Vars1,"Vars3",Vars3,"Result2",Result2,"Cut",Cut]),
	not(predicate_or_rule_name(Statement)),
	interpretstatement1(Functions0,Functions,Statement,Vars1,Vars3,Result2,Cut),
%%writeln1(["here1"]),
	((not(Cut=cut))->(Functions2=Functions);(turncut(on))), %% cut to interpret1/2 (assertz)
%%writeln1(["here3"]),
	interpretbody(Functions0,Functions2,Vars3,Vars2,Statements,Result3),
	%%((Result3=cut)->!;true),
%%writeln1(["here4"]),
	logicalconjunction(Result1,Result2,Result3),!.
%%writeln1([Result1,Result2,Result3]).
turncut(State1) :-
	cut(State2),
	retract(cut(State2)),
	assertz(cut(State1)).
turndebug(State1) :-
	debug(State2),
	retract(debug(State2)),
	assertz(debug(State1)).
logicaldisjunction(true,Result2,Result3) :-
        true(Result2);true(Result3).
logicalconjunction(true,Result2,Result3) :-
	true(Result2),true(Result3).
logicalnot(Result1,Result2) :-
	true(Result1),false(Result2).
logicalnot(Result1,Result2) :-
        false(Result1),true(Result2).
true(true).
false(false).

%%interpretstatement1(_F0,[],_,Vars,Vars,true,nocut) :- !
%%writeln1("AND HERE!")
%%	.

interpretstatement1(_F0,_Functions,[[n,cut]],Vars,Vars,true,cut) :- !.
interpretstatement1(_F0,_Functions,[[n,true]],Vars,Vars,_,nocut).
interpretstatement1(_F0,_Functions,[[n,fail]],Vars,Vars,_,nocut) :- fail.

/**
interpretstatement1(Functions0,Functions,[[n,not],[Statements]],Vars1,Vars2,Result,nocut) :-
	not(interpretbody(Functions0,Functions,Vars1,Vars2,
		Statements,Result)).

interpretstatement1(Functions0,Functions,[[n,or],[Statement1,Statement2]],Vars1,Vars2,Result,nocut) :-
	(interpretbody(Functions0,Functions,Vars1,Vars2,
		Statement1,Result1);
		interpretbody(Functions0,Functions,Vars1,Vars2,
		Statement2,Result2)).
**/

interpretstatement1(_F0,_Functions,[[n,atom],[Variable]],Vars,Vars,true,nocut) :-
	getvalue(Variable,Value,Vars),
debug_call(Skip,[[n,atom],[Value]]),
	(atom(Value)->
debug_exit(Skip,[[n,atom],[Value]])
;     debug_fail(Skip,[[n,atom],[Value]])),!.

interpretstatement1(_F0,_Functions,[[n,string],[Variable]],Vars,Vars,true,nocut) :-
        getvalue(Variable,Value,Vars),
debug_call(Skip,[[n,string],[Value]]),
	(string(Value)->
debug_exit(Skip,[[n,string],[Value]])
;     debug_fail(Skip,[[n,string],[Value]])),!.

interpretstatement1(_F0,_Functions,[[n,number],[Variable]],Vars,Vars,true,nocut) :-
        getvalue(Variable,Value,Vars),
debug_call(Skip,[[n,number],[Value]]),
	(number(Value)->
debug_exit(Skip,[[n,number],[Value]])
;     debug_fail(Skip,[[n,number],[Value]])),!.

interpretstatement1(_F0,_Functions,[[n,letters],[Variable]],Vars,Vars,true,nocut) :-
        getvalue(Variable,Value,Vars),
debug_call(Skip,[[n,letters],[Value]]),
        ((string_codes(Value,Value1),
        phrase(word1(Value1),_))->
debug_exit(Skip,[[n,letters],[Value]])
;     debug_fail(Skip,[[n,letters],[Value]])),!.

interpretstatement1(_F0,_Functions,[[n,variable],[Variable]],Vars,Vars,true,nocut) :-
debug_call(Skip,[[n,variable],[Variable]]),
        (isvar(Variable)->
debug_exit(Skip,[[n,variable],[Variable]])
;     debug_fail(Skip,[[n,variable],[Variable]])),!.

/**interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1]],Vars1,Vars2,true,nocut) :-
	isop(Operator),
	interpretpart(is,Variable1,Vars1,Vars2),!.
**/


interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
	isop(Operator),
	interpretpart(is,Variable1,Variable2,Vars1,Vars2),!.

/**
interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(31),
        isop(Operator),
        interpretpart(is,Variable2,Variable1,Vars1,Vars2).
**/

interpretstatement1(_F0,_Functions,[[n,Operator],[Variable2,Variable3,Variable1]],Vars1,Vars2,true,nocut) :-
	operator(Operator),
%%writeln1(4),
        interpretpart(isop,Operator,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
	comparisonoperator(Operator),
%%writeln1(4),
        interpretpart(iscomparison,Operator,Variable1,Variable2,Vars1,Vars2).

%%interpretstatement1(_F0,_Functions,[Variable2+Variable3,is,Variable1],Vars1,Vars2,true,nocut) :-
%%writeln1(41),
        %%interpretpart(isplus,Variable1,Variable2,Variable3,Vars1,Vars2).

/**interpretstatement1(_F0,_Functions,[[n,=],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(5),
        interpretpart(is,Variable1,Variable2,Vars1,Vars2).
**/

interpretstatement1(_F0,_Functions,[[n,equals1],[Variable1,[Variable2,Variable3]]],Vars1,Vars2,true,nocut) :-
%%writeln1(5),
        interpretpart(match1,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,equals2],[Variable1,[Variable2,Variable3]]],Vars1,Vars2,true,nocut) :-
%%writeln1(5),
        interpretpart(match2,Variable1,Variable2,Variable3,Vars1,Vars2).

%%interpretstatement1(_F0,_Functions,[[Variable2,Variable3]=Variable1],Vars1,Vars2,true,nocut) :-
%%writeln1(51),
%%        interpretpart(match,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,wrap],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(52), wrap
%%writeln([[n,wrap],[Variable1,Variable2]]),
        interpretpart(bracket1,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,unwrap],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(53), unwrap
        interpretpart(bracket2,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,head],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(6),
        interpretpart(head,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,tail],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(61),
        interpretpart(tail,Variable1,Variable2,Vars1,Vars2).


interpretstatement1(_F0,_Functions,[[n,member],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(8),
        interpretpart(member,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,member2],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(8),
        interpretpart(member2,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,delete],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
%%writeln1(),
        interpretpart(delete,Variable1,Variable2,Variable3,Vars1,Vars2).
%%** all in form f,[1,1,etc], including + with 0,1

interpretstatement1(_F0,_Functions,[[n,append],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
%%writeln1(9),
        interpretpart(append,Variable1,Variable2,Variable3,Vars1,Vars2).


interpretstatement1(_F0,_Functions,[[n,stringconcat],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
        interpretpart(stringconcat,Variable1,Variable2,Variable3,Vars1,Vars2).

/**interpretstatement1(_F0,_Functions,[[n,grammar_part]|Variables1],Vars1,Vars2,true,nocut) :-
%%writeln1(x9),
		  [Variables2]=Variables1,
        interpretpart(grammar_part,Variables2,Vars1,Vars2),!.**/

interpretstatement1(_F0,_Functions,[[n,stringtonumber],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
        interpretpart(stringtonumber,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,random],[Variable1]],Vars1,Vars2,true,nocut) :-
        interpretpart(random,Variable1,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,length],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
        interpretpart(length,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,ceiling],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
        interpretpart(ceiling,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,date],[Year,Month,Day,Hour,Minute,Seconds]],Vars1,Vars2,true,nocut) :-
        interpretpart(date,Year,Month,Day,Hour,Minute,Seconds,Vars1,Vars2).

/***
interpretstatement1(Functions0,_Functions,Query1,Vars1,Vars8,true,nocut) :-
%%writeln1("h1/10"),
        Query1=[[n,grammar]|Arguments],
        ((Arguments=[[Grammar1,Phrase1,RuleName|Variables2]],
        %%[Variables3]=Variables2,
        name(RuleName),
		  convert_to_grammar_part1(Grammar1,[],Grammar2))->true;
		  (Grammar2=Functions0,
		  ((Arguments=[[Phrase1,RuleName|Variables2]]
		  %%([Variables3]=Variables2->true;(Variables2=[],Variables3=[]))
		  )))),

%%writeln1(["Arguments",Arguments,"Vars1",Vars1]),

%%substitutevarsA1(Phrase,Vars1,[],Vars3,[],FirstArgs1),

%%Vars3=[[[v,PhraseVarName],PhraseValue]],
%%Vars4=[[[v,vgp1],PhraseValue]],

   append([Phrase1],Variables2,Variables4), %% *** Should V3 be in [] v

substitutevarsA1(Variables4,Vars1,[],Vars2,[],FirstArgs), %%% var to value, after updatevars:  more vars to values, and select argument vars from latest vars
%%writeln1([substitutevarsA1,arguments,Arguments,vars1,Vars1,vars3,Vars3,firstargs,FirstArgs]),
		  
Vars2=[Phrase2|Vars4],
((Phrase2=[]->true;Phrase2=[_A|_B])->End=[];End=""),
		  (not(Vars4=[])->append([RuleName,Phrase2,End],Vars4,Vars5);
		  (Vars5=[RuleName,Phrase2,End])),
		  Query2=[[n,grammar_part],Vars5],
		  ((((terminal(RuleName),  
		  

		  (not(Vars4=[])->append([Phrase2,RuleName],Vars4,Vars52);
		  (Vars52=[Phrase2,RuleName])),
		  
		  (debug(on)->(writeln1([call,[[n,grammar],Vars52],"Press c."]),(leash1(on)->true;(not(get_single_char(97))->true;abort)));true),

      interpretpart(grammar_part,Vars5,[],Result1),
		  
		  	updatevars2(FirstArgs,Result1,[],Vars51),
	updatevars3(Vars2,Vars51,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
)->true;(
%%writeln1(here1),
	Vars8=[]),strip(Vars8,[],Result2))->true)),

        	(debug(on)->(writeln1([exit,[[n,grammar],Result2],"Press c."]),(leash1(on)->true;(not(get_single_char(97))->true;abort)));true)

)->true;(not(terminal(RuleName)),
         %% Bodyvars2?
		          	
		          			  (not(Vars4=[])->append([Phrase2,RuleName],Vars4,Vars52);
		  (Vars52=[Phrase2,RuleName])),

(debug(on)->(writeln1([call,[[n,grammar],Vars52],"Press c."]),(leash1(on)->true;(not(get_single_char(97))->true;abort)));true),

%%        	debug(on)->writeln1([call,[Function,[Vars3]]]),
%%writeln1(["Query2",Query2,"Functions0",Functions0]),
        interpret2(Query2,Grammar2,Grammar2,Result1), 
        	(debug(on)->(writeln1([exit,[[n,grammar],Vars52],"Press c."]),(leash1(on)->true;(not(get_single_char(97))->true;abort)));true),

	updatevars2(FirstArgs,Result1,[],Vars51),
	updatevars3(Vars2,Vars51,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
);(
%%writeln1(here1),
	Vars8=[])))),!.
***/

interpretstatement1(_Grammar,_Grammar2,[[n,grammar_part],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
%%writeln1("h1/10"),
%%trace,%%%%****
	interpretpart(grammar_part,[Variable1,Variable2,Variable3],Vars1,Vars2).

/***
        Query1=[[n,grammar_part]|Arguments],
        Arguments=[[RuleName|Variables2]],
        	%%(([Variables4|Rest]=Variables2->Variables3=Variables2;(Variables2=[],Variables3=[]))),

        ((not(terminal(RuleName)),
%%writeln1(["Arguments",Arguments,"Vars1",Vars1]),
        substitutevarsA1(Variables2,Vars1,[],Vars3,[],FirstArgs), %%% var to value, after updatevars:  more vars to values, and select argument vars from latest vars
%%writeln1([substitutevarsA1,arguments,Arguments,vars1,Vars1,vars3,Vars3,firstargs,FirstArgs]),
		  (not(Vars3=[])->(append([RuleName],Vars3,Vars4),Query2=[[n,grammar_part],Vars4]);
		  Query2=[[n,grammar_part],RuleName]), %% Bodyvars2?
%%        	debug(on)->writeln1([call,[Function,[Vars3]]]),
%%writeln1(["Query2",Query2,"Functions0",Functions0]),
        %%notrace,%%****
 interpret2(Query2,Grammar,Grammar,Result1),
 %%trace,%****
	updatevars2(FirstArgs,Result1,[],Vars5),
	updatevars3(Vars1,Vars5,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
)->true;(
%%writeln1(here1),
	Vars8=[]))->true)->true;
(terminal(RuleName),substitutevarsA1(Variables2,Vars1,[],Vars3,[],FirstArgs),
%%writeln1(here), %%****
%%Vars3=[Phrase,End],
%%Vars41=[Phrase,[v,vgp]],
append([RuleName],Vars3,Vars9),
%%writeln1([vars9,Vars9]), %%%%%*****
interpretpart(grammar_part,Vars9,[],Result1),
	updatevars2(FirstArgs,Result1,[],Vars5),
	updatevars3(Vars3,Vars5,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
	%%writeln1([vars8,Vars8]) %%%*****
)->true;(
%%writeln1(here1),
	Vars8=[]))->true)),%%notrace, %%****
	!.
**/

interpretstatement1(Functions0,_Functions,Query1,Vars1,Vars8,true,nocut) :-
%%writeln1("h1/10"),
        Query1=[Function,Arguments],%%not(Function=[n,grammar]->true;Function=[n,grammar_part]), ****
%%writeln1(["Arguments",Arguments,"Vars1",Vars1]),
        %%***writeln1(substitutevarsA1(Arguments,Vars1,[],Vars3,[],FirstArgs)),
        (Function=[v,_]->
        (append([Function],Arguments,Arguments1),
        substitutevarsA1(Arguments1,Vars1,[],Vars3,[],FirstArgs),
        Vars3=[Function1|Vars31],
        Query2=[Function1,Vars31]);
        (substitutevarsA1(Arguments,Vars1,[],Vars3,[],FirstArgs), %%% var to value, after updatevars:  more vars to values, and select argument vars from latest vars
%%writeln1([substitutevarsA1,arguments,Arguments,vars1,Vars1,vars3,Vars3,firstargs,FirstArgs]),
        Query2=[Function,Vars3])), %% Bodyvars2?
%%        	debug(on)->writeln1([call,[Function,[Vars3]]]),
%%writeln1(["Query2",Query2,"Functions0",Functions0]),
        interpret2(Query2,Functions0,Functions0,Result1), 
	updatevars2(FirstArgs,Result1,[],Vars5),
	updatevars3(Vars1,Vars5,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
);(
%%writeln1(here1),
	Vars8=[])).
%%**** reverse and take first instance of each variable.
	%%findresult3(Arguments,Vars6,[],Result2)
%%writeln1(["FirstArgs",FirstArgs,"Result1",Result1,"Vars5",Vars5,"Vars4",Vars4]),
%%writeln1(["Vars1:",Vars1,"Vars4:",Vars4]),
%%		debug(on)->writeln1([exit,[Function,[Result2]]]).
interpretstatement1(Functions0,_Functions,Query,Vars,Vars,true) :-
	Query=[_Function],
debug_call(Skip,[Function]),
        (interpret2(Query,Functions0,Functions0,_Result1)->
debug_exit(Skip,[Function])
;     debug_fail(Skip,[Function])),!.


debug_react(Status,115,true) :- Status=call, 
turndebug(off), writeln(" skip"). %% skip
debug_react(_Status,97,false) :- writeln(" abort"),abort. %% abort
debug_react(Status,A,false) :- ((Status=call,not(A=115),not(A=97))->true;
(member_exit_fail(Status),not(A=97))), writeln(" creep"). %% creep

member_exit_fail(exit).
member_exit_fail(fail).

debug_call(Skip,FunctionArguments1) :-
(debug(on)->(write1([call,FunctionArguments1,"Press c to creep, s to skip or a to abort."]),(leash1(on)->true;(get_single_char(Key),debug_react(call,Key,Skip))));Skip=false).

debug_fail_fail(Skip) :-
(debug(on)->(Skip=true->turndebug(on);true);true).

debug_fail(Skip,FunctionArguments1) :-
((Skip=true->turndebug(on);true),((debug(on)->(write1([fail,FunctionArguments1,"Press c to creep or a to abort."]),(leash1(on)->true;(get_single_char(Key),debug_react(fail,Key,_Skip))));true),fail)).

debug_exit(Skip,FunctionResult2) :-
((Skip=true->turndebug(on);true),((debug(on)->(write1([exit,FunctionResult2,"Press c to creep or a to abort."]),(leash1(on)->true;(get_single_char(Key),debug_react(exit,Key,_Skip))));true))).


debug_types_call(FunctionArguments1) :-
debug_types(call,FunctionArguments1).
debug_types(Call,FunctionArguments1) :-
(debug(on)->(writeln1([Call,FunctionArguments1]));true).

debug_types_fail(FunctionArguments1) :-
((debug(on)->(writeln1([fail,FunctionArguments1]));true),fail).

debug_types_exit(FunctionResult2) :-
debug_types(exit,FunctionResult2).


operator(+).
operator(-).
operator(*).
operator(/).

comparisonoperator(>).
comparisonoperator(>=).
comparisonoperator(<).
comparisonoperator(=<).
%%comparisonoperator(=).
comparisonoperator(=\=).


word1([])-->[].
word1([A|As]) --> [A],word1(As),{%%atom_codes(A,AC),
char_type(A,alpha)},!.
/**interpretstatement1(_Functions0, _Functions,_Query,_Vars1,_Vars2,false) :-
	writeln1([false]).
**/
/**
interpretstatement2(Value,_Vars,Value) :-
	(number(Value);atom(Value)).
interpretstatement2(Variable,Vars1,Value) :-
	getvalue(Variable,Value,Vars1).
interpretstatement3(A + B,Vars,Value1) :-
        interpretstatement2(A,Vars,Value2),
        interpretstatement2(B,Vars,Value3),
        Value1 = Value2 + Value3.
interpretstatement3(Value,_Vars,Value) :-
	(number(Value);atom(Value)).
interpretstatement3(Variable,Vars,Value) :-
        getvalue(Variable,Value,Vars).
        **/
getvalue(Variable,Value,Vars) :-
        ((not(isvar(Variable)),isvalstrorundef(Value),Variable=Value)->true;
        (isvar(Variable),isvalstrorundef(Value),getvar(Variable,Value,Vars))).
putvalue(Variable,Value,Vars1,Vars2) :-
        ((not(isvar(Variable)),isvalstrorundef(Value),Variable=Value,Vars1=Vars2)->true;
        (isvar(Variable),isvalstrorundef(Value),updatevar(Variable,Value,Vars1,Vars2))),!. 
getvar(Variable,Value,Vars) :-
	((member([Variable,Value],Vars),
	not(Value=empty))->true;
	        ((aggregate_all(count,member([Variable,_Value],Vars),0)->true;%%
	member([Variable,empty],Vars)),Value=empty))
.
getvar(undef,undef,_Vars) :-
	!.
%%getvar(Variable,empty,Vars) :-
        %%(aggregate_all(count,member([Variable,_Value],Vars),0)->true;%%;
	%%member([Variable,empty],Vars))
	%%.
updatevar(undef,_Value,Vars,Vars) :-
	!.
updatevar(Variable,Value,Vars1,Vars2) :-
	((((member([Variable,empty],Vars1),
	delete(Vars1,[Variable,empty],Vars3),
	append(Vars3,[[Variable,Value]],Vars2))->true;
	((not(member([Variable,Value1],Vars1)),
	((Value1=empty)->true;(Value1=Value)))),
        append(Vars1,[[Variable,Value]],Vars2))->true;
	(member([Variable,Value],Vars1),Vars2=Vars1))->true;
	(undef(Variable),
	append(Vars1,[[Variable,Value]],Vars2))).
/**updatevars(_FirstArgs,[],Vars,Vars).
updatevars(FirstArgs,Vars1,Vars2,Vars3) :-
        Vars1=[[Variable1,Value]|Vars4],
	((member([Variable2,Variable1],FirstArgs), %% removed brackets around firstargs here and 2 line below
	append(Vars2,[[Variable2,Value]],Vars5))->true;
	(member([Variable1,_Variable2],FirstArgs),
	append(Vars2,[[Variable1,Value]],Vars5))),
	updatevars(FirstArgs,Vars4,Vars5,Vars3),
	!.
updatevars(FirstArgs,Vars1,Vars2,Vars3) :-
	Vars1=[_Vars4|Vars5],
	updatevars(FirstArgs,Vars5,Vars2,Vars3).**/
updatevars([],_Vars1,Vars2,Vars2) :- !.
updatevars(FirstArgs,Vars1,Vars2,Vars3) :-
	FirstArgs=[[Orig,New]|Rest],
	(expressionnotatom(New)->append(Vars2,[[Orig,New]],Vars4);
	(member([New,Value],Vars1),
	append(Vars2,[[Orig,Value]],Vars4))),
	updatevars(Rest,Vars1,Vars4,Vars3),!.
updatevars2(_FirstArgs,[],Vars,Vars) :- !.
updatevars2(FirstArgs,Vars1,Vars2,Vars3) :-
        Vars1=[[Variable,Value]|Vars4],
        (member(Variable,FirstArgs), %% removed brackets around firstargs here and 2 line below, ** vars1 into arg in (10), check cond
        append(Vars2,[[Variable,Value]],Vars5)),
        updatevars2(FirstArgs,Vars4,Vars5,Vars3).
updatevars3(Vars1,[],Vars1).
updatevars3(Vars1,Vars2,Vars4) :-
	Vars2=[[Variable,Value]|Vars5],
	delete(Vars1,[Variable,empty],Vars6),
	append(Vars6,[[Variable,Value]],Vars7),
	updatevars3(Vars7,Vars5,Vars4),
	!.
updatevars3(Vars1,Vars2,Vars4) :-
	Vars2=[[Variable,Value]|Vars5],
	append(Vars1,[[Variable,Value]],Vars6),
        updatevars3(Vars6,Vars5,Vars4).
reverse([],List,List).
reverse(List1,List2,List3) :-
	List1=[Head|Tail],
	append([Head],List2,List4),
	reverse(Tail,List4,List3).
unique1([],Items,Items).
unique1([Item|Items1],Items2,Items3) :-
	delete(Items1,Item,Items4),
	append(Items2,[Item],Items5),
	unique1(Items4,Items5,Items3).
isvar([v,_Value]) :- !.
isval(Value) :-
	number(Value).
isvalstr(N) :-
	isval(N);string(N).
isvalempty(N) :-
	isval(N);(N=empty).
isempty(N) :-
	N=empty.
/**isvalstrempty(N) :-
	isval(N);(string(N);N=empty).**/
isvalstrempty(N) :-
	var(N),!.
isvalstrempty(N) :-
	isval(N),!.
isvalstrempty(N) :-
	string(N).
isvalstrempty(empty).
isvalstrempty([]).
/**isvalstrempty(N) :-
	atom(N),fail,!.
**/
isvalstrorundef(N) :- 
	var(N),!.
isvalstrorundef(N) :- 
	not(var(N)),isval(N),!.
isvalstrorundef(N) :- 
	not(var(N)),expression(N),!.
undef(N) :-
	var(N).
/**
expression(N) :-
	isval(N);(string(N);atom(N)),!.
expression([]).
expression(empty).
expression([N]) :-
	expression(N).
expression([N|Ns]):-
	expression(N),expression(Ns).
**/

expression(empty) :-
	!.
expression(N) :-
	isval(N),!.
expression(N) :-
	string(N),!.
expression(N) :-
	atom(N),!.
expression([]) :-
	!.
expression(N) :-
	not(atom(N)),
	length(N,L),L>=1,
	expression2(N).
expression2([]).
expression2([N|Ns]) :-
	%%(
	expression3(N),%%->true;expression2(N)),
	expression2(Ns).
expression3(N) :-
	isval(N),!.
expression3(N) :-
	string(N),!.
expression3(N) :-
	atom(N),!.
expression3(N) :-
	expression2(N),!.

expressionnotatom3(N) :-
	expressionnotatom(N),!.
expressionnotatom3(N) :-
	not(N=[v,_]),not(N=["v",_]),expression(N),!.

expressionnotatom(N) :-
	isvalstrempty(N),!.
expressionnotatom(N) :-
	not(atom(N)),
	length(N,L),L>=1,
	expressionnotatom2(N),!.
expressionnotatom(Name) :-
	predicate_or_rule_name(Name),!.
expressionnotatom2([]).
expressionnotatom2([N|Ns]) :-
	isvalstrempty(N),	
	expressionnotatom2(Ns).

substitutevarsA1(Arguments,Vars1,Vars2,Vars3,FirstArgs1,FirstArgs2) :-
	substitutevarsA2(Arguments,Vars1,Vars2,Vars3,FirstArgs1,FirstArgs2),!.
substitutevarsA2([],_Vars1,Vars2,Vars2,FirstArgs,FirstArgs):-!.
substitutevarsA2(Arguments,Vars1,Vars2,Vars3,FirstArgs1,FirstArgs2) :-
	Arguments=[Variable|Variables],
	((getvalue(Variable,Value,Vars1),
	Value=empty)->
	((append(Vars2,[Variable],Vars4)),
	(isvar(Variable)->append(FirstArgs1,[Variable],
	FirstArgs3);FirstArgs3=FirstArgs1));
	(getvalue(Variable,Value,Vars1),
	append(Vars2,[Value],Vars4)),
	FirstArgs3=FirstArgs1),
        substitutevarsA2(Variables,Vars1,Vars4,Vars3,FirstArgs3,FirstArgs2),!.

findresult3([],_Result,Result2,Result2):-!.
findresult3(Arguments1,Result1,Result2,Result3) :-
	Arguments1=[Value|Arguments2],
	expressionnotatom3(Value),
	append(Result2,[Value],Result4),
        findresult3(Arguments2,Result1,Result4,Result3),!.
findresult3(Arguments1,Result1,Result2,Result3) :-
        Arguments1=[Variable|Arguments2],
        isvar(Variable),
	member([Variable,Value],Result1),
        append(Result2,[Value],Result4),
        findresult3(Arguments2,Result1,Result4,Result3),!.

strip([],Result2,Result2).
strip(Arguments1,Result2,Result3) :-
	Arguments1=[[Variable,Value]|Arguments2],
        isvar(Variable),
        append(Result2,[Value],Result4),
        strip(Arguments2,Result4,Result3).
