:- use_module(library(date)).

:- dynamic debug/1.
:- dynamic cut/1.
:- dynamic leash1/1.
:- dynamic types/1.
:- dynamic typestatements/1.
:- dynamic modestatements/1.
:- dynamic findall_sys/1.

:- dynamic lang/1.

/** List Prolog Interpreter **/

interpret(Debug,Query,Functions1,Result) :-
	international_interpret([lang,"en"],
	Debug,Query,Functions1,Result).

international_interpret([lang,Lang],Debug,Query,Functions1,Result) :-
	retractall(lang(_)),
 	assertz(lang(Lang)),
	interpret_1(Debug,Query,Functions1,Result).
	
international_interpret([lang,Lang],Debug,Query,TypeStatements,ModeStatements,Functions1,Result) :-
	retractall(lang(_)),
 	assertz(lang(Lang)),
	interpret_1(Debug,Query,TypeStatements,ModeStatements,Functions1,Result).


interpret_1(Debug,Query,Functions1,Result) :-
	retractall(types(_)),
 	assertz(types(off)),
interpret11(Debug,Query,Functions1,Result).

interpret_1(Debug,Query,TypeStatements,ModeStatements,Functions1,Result) :-
	retractall(types(_)),
 	assertz(types(on)),
	retractall(typestatements(_)),
 	assertz(typestatements(TypeStatements)),
	retractall(modestatements(_)),
 	assertz(modestatements(ModeStatements)),
interpret11(Debug,Query,Functions1,Result).

interpret11(Debug,Query,Functions1,Result) :-
	((not(lang(_Lang1))
	%var(Lang1)
	)->
	(retractall(lang(_)),
 	assertz(lang("en")));
	true),
	load_lang_db,

%%writeln1([i1]),
	%%writeln1(convert_to_grammar_part1(Functions1,[],Functions2,_)),
	convert_to_grammar_part1(Functions1,[],Functions2,_),
	%trace,
	%writeln1(Functions2),
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
  	retractall(findall_sys(_)),
 	assertz(findall_sys(1)),
	%%writeln1(member1(Query,Functions1,Functions2,Result)),
	member1(Query,Functions1,Functions2,Result).
%%member1([_,R],_,[],R).
%%member1(_,_,[],[]).
member1(_Query,_,_,[],_) :- %%writeln1(["The query",Query,"matches no predicates."]),
fail,!.
member1(Query,Functions,Functions2,Vars8) :-
%%writeln1([m1]),
	(cut(off)->(
        (Query=[Function,Arguments1],
	(Functions2=[[Function,Arguments2,":-",Body]|_Functions3]),
	length(Arguments1,Length),
	length(Arguments2,Length),

checktypes_inputs(Function,Arguments1),
        
        %%writeln1(checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs)),
        %trace,
        checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),
        %%->ca2 
%%writeln1([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
debug_call(Skip,[Function,Arguments1]),
	(interpretbody(Functions,Functions2,Vars1,Vars2,Body,true)->debug_fail_fail(Skip);debug_fail(Skip,[Function,Arguments1])
	),
	%%writeln1(updatevars(FirstArgs,Vars2,[],Result)),
	updatevars(FirstArgs,Vars2,[],Result),
        %%reverse(Result,[],Vars7),
	((true->%not(Result=[])->
        %%Result=[Var71|Vars72],
        %%writeln1(unique1(Result,[],Vars8)),
        (unique1(Result,[],Vars8),
%%writeln1(["FirstArgs",FirstArgs,"Vars",Vars2,"Result",Result,"Vars7",Vars7,"Vars72",Vars72,"Var71",Var71,"Vars8",Vars8]),
%%writeln1(["Vars8",Vars8]),
	%%writeln1(findresult3(Arguments1,Vars8,[],Result2)),
	findresult3(Arguments1,Vars8,[],Result2)
%%writeln1([findresult3,"Arguments1",Arguments1,"Vars8",Vars8,"Result2",Result2])
	);(
%%writeln1(here1),
	Vars8=[],Result2=[]))),
%%writeln1(["Arguments1",Arguments1,"Vars2",Vars2,"Result",Result]),
		debug_exit(Skip,[Function,Result2]),
		        checktypes(Function,Result2)
)
	;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2,":-",_Body]|Functions3], %% make like previous trunk?
	member11(Query,Functions,Functions2,Vars8))
	);(turncut(off),fail%%,Result=[]
	)).
member11(Query,Functions,Functions2,Result) :-
%%writeln1([m11]),
%%writeln1(["Query",Query,"Functions",Functions,"Functions2",Functions2,"Result",Result]),
	(cut(off)->(
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
	);(turncut(off),fail)).
member12(Query,Functions,Functions2,Vars8) :-
%%writeln1([m12]),
	(cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        
        checktypes_inputs(Function,Arguments1),

        checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),
%%writeln1([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
	updatevars(FirstArgs,Vars1,[],Result),
        %%reverse(Result,[],Vars7),
        ((%not
        true->%(Result=[])->
        %%Result=[Var71|Vars72],
        (unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
        );(
%%writeln1(here2),
	Vars8=[],Result2=[]))),
        	debug_call(Skip,[Function,Arguments1]),
   debug_exit(Skip,[Function,Result2]),
	checktypes(Function,Result2)

	);
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2]|Functions3],
	member13(Query,Functions,Functions2,Vars8))
	);(turncut(off),fail)).
member13(Query,Functions,Functions2,Result) :-
%%writeln1([m13]),
	(cut(off)->(
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
	);(turncut(off),fail)).
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
	(cut(off)->(
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
        ((true->%not(Result=[])->
        %%Result=[Var71|Vars72],
        (unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
%%writeln1(["Vars2",Vars2,"Result",Result]),
        );(
	%%writeln1(here3),
	Vars8=[],Result2=[]))),
   debug_exit(Skip,[Function,Result2]),
   checktypes(Function,Result2)

	);%%->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2,":-",_Body]|Functions3],
	member21(Query,Functions,Functions2,Vars8))
	);(turncut(off),fail)).
member21(Query,Functions,Functions2,Result) :-
%%writeln1([m21]),
	(cut(off)->(
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
	);(turncut(off),fail)).
member22(Query,Functions,Functions2,Vars8) :-
%%writeln1([m22]),
	(cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        
        checktypes_inputs(Function,Arguments1),

        checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),
%%writeln1([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
        updatevars(FirstArgs,Vars1,[],Result),
        %%reverse(Result,[],Vars7),
        ((true->%not(Result=[])->
        %%Result=[Var71|Vars72],
        (unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
        );(
%%writeln1(here4),
	Vars8=[],Result2=[]))),
        	debug_call(Skip,[Function,Arguments1]),
        	debug_exit(Skip,[Function,Result2]),
	checktypes(Function,Result2)

	);%%->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2]|Functions3],
	member23(Query,Functions,Functions2,Vars8))
	);(turncut(off),fail)).
member23(Query,Functions,Functions2,Vars8) :-
%%writeln1([m23]),
	(cut(off)->(
        (Query=[Function],
        (Functions2=[[Function]|_Functions3]),
        	debug_call(Skip,[Function]),
	Vars8=[],
        	debug_exit(Skip,[Function])
	);%%->true;
	(%%Query=[Function],
	Functions2=[_Function|Functions3],
	member2(Query,Functions,Functions3,Vars8))
	);(turncut(off),fail)).
	
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
%%trace,
%%writeln(checktypes(Function,Vars1)),
	(types(on)->(typestatements(TypeStatements1),
	modestatements(ModeStatements1),
	checktypes0_inputs(Function,Vars1,TypeStatements1,ModeStatements1));true),!.
checktypes0_inputs(Function,Vars1,_TypeStatements1,_ModeStatements1) :- 
	length(Vars1,L),L is 0,Vars1=[],
	get_lang_word("input type check",Input_type_check),
	(types(on)->debug_types_call([Function,/,~,L,Input_type_check]);true),
	
	
	(types(on)->debug_call(Skip,[Function,Vars1]);true),
		
	(types(on)->debug_exit(Skip,[Function,Vars1]);true),
	(types(on)->(debug_types_exit([Function,/,~,L,Input_type_check]));true),!.
checktypes0_inputs(Function,Vars1,TypeStatements1,ModeStatements1) :-
	length(Vars1,L),
	get_lang_word("input type check",Input_type_check),
	(types(on)->(debug_types_call([Function,/,~,L,Input_type_check]));true),
	
	
	(member([Function|[TypeStatements2]],TypeStatements1),
	member([Function|[ModeStatements2]],ModeStatements1),
	extract_modes1(TypeStatements2,TypeStatements3,Vars1,Vars2,ModeStatements2),
	(types(on)->debug_call(Skip,[Function,Vars2]);true),
	((checktypes1(Vars2,TypeStatements3,TypeStatements3,TypeStatements1))->
	(
	(types(on)->debug_exit(Skip,[Function,Vars2]);true),
	(types(on)->(debug_types_exit([Function,/,~,L,Input_type_check]));true))
	
;(
	(types(on)->debug_fail(Skip,[Function,Vars1]);true),

(types(on)->(debug_types_fail([Function,/,~,L,Input_type_check]));true)))),!.

extract_modes1(TypeStatements1,TypeStatements3,Vars1,Vars2,ModeStatements1) :-
	%%TypeStatements1=[TypeStatements2|TypeStatements3],
	%%trace,
	%%writeln1([TypeStatements1,ModeStatements1]),
	extract_modes2(TypeStatements1,[],TypeStatements3,Vars1,[],Vars2,ModeStatements1),!.
	%%TypeStatements3=[TypeStatements3a|TypeStatements3].
extract_modes2([],TypeStatements2a,TypeStatements2a,[],Vars,Vars,[]) :- !.
%%extract_modes2(_,TypeStatements2a,TypeStatements2a,[],Vars,Vars,[]) :- !.
extract_modes2(TypeStatements1,TypeStatements2a,TypeStatements3,Vars1,Vars2,Vars3,ModeStatements1) :-
	get_lang_word("input",Input),
	ModeStatements1=[Input|ModeStatements3],
	TypeStatements1=[TypeStatements2|TypeStatements3a],
	Vars1=[Vars11|Vars12],
	append(TypeStatements2a,[TypeStatements2],TypeStatements4),
	append(Vars2,[Vars11],Vars4),
	extract_modes2(TypeStatements3a,TypeStatements4,TypeStatements3,Vars12,Vars4,Vars3,ModeStatements3),!.
extract_modes2(TypeStatements1,TypeStatements2a,TypeStatements3,Vars1,Vars2,Vars3,ModeStatements1) :-
	get_lang_word("output",Output),
	ModeStatements1=[Output|ModeStatements3],
	TypeStatements1=[_TypeStatements2|TypeStatements3a],
	Vars1=[_Vars11|Vars12],
	extract_modes2(TypeStatements3a,TypeStatements2a,TypeStatements3,Vars12,Vars2,Vars3,ModeStatements3),!.


checktypes(Function,Vars1):-%%,TypeStatements1) :-
%%writeln(checktypes(Function,Vars1)),
	(types(on)->(typestatements(TypeStatements1),
	checktypes0(Function,Vars1,TypeStatements1));true),!.
checktypes0(Function,Vars1,_TypeStatements1) :- 
	get_lang_word("Type check",Type_check),
	length(Vars1,L),L is 0,Vars1=[],
	(types(on)->(debug_types_call([Function,/,L,Type_check]));true),
	
	
	(types(on)->debug_call(Skip,[Function,Vars1]);true),
		
	(types(on)->debug_exit(Skip,[Function,Vars1]);true),
	(types(on)->(debug_types_exit([Function,/,L,Type_check]));true),!.
		
checktypes0(Function,Vars1,TypeStatements1) :-
	get_lang_word("Type check",Type_check),
	length(Vars1,L),
	(types(on)->(debug_types_call([Function,/,L,Type_check]));true),
	
	
	(types(on)->debug_call(Skip,[Function,Vars1]);true),
	((member([Function|[TypeStatements2]],TypeStatements1),
	checktypes1(Vars1,TypeStatements2,TypeStatements2,TypeStatements1))->
	(
	(types(on)->debug_exit(Skip,[Function,Vars1]);true),
	(types(on)->(debug_types_exit([Function,/,L,Type_check]));true))
	
;(
	(types(on)->debug_fail(Skip,[Function,Vars1]);true),

(types(on)->(debug_types_fail([Function,/,L,Type_check]));true))),!.

checktypes1([],[],_,_) :- !.

	checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4) :-
get_lang_word("t",T),
%trace,
get_lang_word("list",Dbw_list),
%%writeln(checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4)),
	Vars1=[Vars2|Vars3],
	list1(Vars2,_,_),
	TypeStatements1=[[[T,Dbw_list]|[TypeStatements3]]|TypeStatements4a],
(types(on)->(debug_call(Skip,[[T,Dbw_list],TypeStatements3]));true),

	((checktypes3(Vars2,TypeStatements3,TypeStatements2,TypeStatements4))->
		((types(on)->(debug_exit(Skip,[[T,Dbw_list],Vars2]));true),
		checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4))
;     (types(on)->(debug_fail(Skip,[[T,Dbw_list],Vars2]));true)
)
%%not(variable_name(Vars2)),
	. %% ** in brac as well

checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4) :-
	get_lang_word("t",T),get_lang_word("list",Dbw_list),
%%writeln(checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4)),
	%%Vars1=[Vars2|Vars3],
	%%list(Vars1,_,_),%%length(Vars1,1),
	TypeStatements1=[[[T,Dbw_list]|[TypeStatements3]]|_TypeStatements4a],
(types(on)->(debug_call(Skip,[[T,Dbw_list],TypeStatements3]));true),

	((checktypes3(Vars1,TypeStatements3,TypeStatements2,TypeStatements4))->
		(types(on)->debug_exit(Skip,[[T,Dbw_list],Vars1]);true)
;     (types(on)->debug_fail(Skip,[[T,Dbw_list],Vars1]);true)).
	%%checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4). %% ** in brac as well
	

checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4) :-
	get_lang_word("t",T),get_lang_word("brackets",Dbw_brackets),
	TypeStatements1=[[[T,Dbw_brackets]|[TypeStatements3]]|TypeStatements4a],
(types(on)->debug_call(Skip,[[T,Dbw_brackets],TypeStatements3]);true),
	(([Vars2|Vars3]=Vars1,
	checktypes1(Vars2,TypeStatements3,TypeStatements2,TypeStatements4))->
		((types(on)->debug_exit(Skip,[[T,Dbw_brackets],Vars1]);true),
		checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4))
;     (types(on)->debug_fail(Skip,[[T,Dbw_brackets],Vars1]);true))
%%not(variable_name(Vars2)),
	,!. %% ** in brac as well

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
	%%not(variable_name(Vars2)),
	checktypes1(Vars3,TypeStatements3,TypeStatements1,TypeStatements4).
	
checktypes2(Vars,TypeStatements1,_TypeStatements2,_C) :-
	get_lang_word("t",T),get_lang_word("number",Dbw_number),

%%writeln(checktypes2(Vars,TypeStatements1,_TypeStatements2,C)),
TypeStatements1=[T,Dbw_number],
(types(on)->debug_call(Skip,[[T,Dbw_number],Vars]);true),
	((number(Vars))->
		(types(on)->debug_exit(Skip,[[T,Dbw_number],Vars]);true)
;     (types(on)->debug_fail(Skip,[[T,Dbw_number],Vars]);true)).
checktypes2(Vars,TypeStatements1,_TypeStatements2,_) :-
	get_lang_word("t",T),get_lang_word("predicatename",Dbw_predicatename),
	get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,

TypeStatements1=[T,Dbw_predicatename],
(types(on)->debug_call(Skip,[[T,Dbw_predicatename],Vars]);true),
	((Vars=[Dbw_n,_])->
		(types(on)->debug_exit(Skip,[[T,Dbw_predicatename],Vars]);true)
;     (types(on)->debug_fail(Skip,[[T,Dbw_predicatename],Vars]);true)).

checktypes2(Vars,TypeStatements1,_TypeStatements2,_) :-
	get_lang_word("t",T),get_lang_word("string",Dbw_string),

TypeStatements1=[T,Dbw_string],
(types(on)->debug_call(Skip,[[T,Dbw_string],Vars]);true),
	((string(Vars))->
		(types(on)->debug_exit(Skip,[[T,Dbw_string],Vars]);true)
;     (types(on)->debug_fail(Skip,[[T,Dbw_string],Vars]);true)).

checktypes2(Vars,TypeStatements1,_TypeStatements2,_) :-
	get_lang_word("t",T),get_lang_word("any",Dbw_any),

TypeStatements1=[T,Dbw_any],
(types(on)->debug_call(Skip,[[T,Dbw_any],Vars]);true),
	((true)->
		(types(on)->debug_exit(Skip,[[T,Dbw_any],Vars]);true)
;     (types(on)->debug_fail(Skip,[[T,Dbw_any],Vars]);true)).

checktypes2(Vars,TypeStatements1,TypeStatements2,TypeStatements4) :-
	get_lang_word("t",T),
	get_lang_word("list",Dbw_list),
	get_lang_word("brackets",Dbw_brackets),
	get_lang_word("number",Dbw_number),
	get_lang_word("predicatename",Dbw_predicatename),
	get_lang_word("string",Dbw_string),
	get_lang_word("any",Dbw_any),

TypeStatements1=[T,Type],(not(Type=Dbw_list),not(Type=Dbw_brackets),not(Type=Dbw_number),not(Type=Dbw_predicatename),not(Type=Dbw_string),not(Type=Dbw_any)),
(types(on)->debug_call(Skip,[[T,Type],Vars]);true),
	((
	%%not(variable_name(Vars)),
	member([[T,Type]|[TypeStatements3]],TypeStatements4),
	(checktypes1(Vars,TypeStatements3,TypeStatements2,TypeStatements4)->true;
		checktypes1([Vars],TypeStatements3,TypeStatements2,TypeStatements4)))->
		(types(on)->debug_exit(Skip,[[T,Type],Vars]);true)
;     (types(on)->debug_fail(Skip,[[T,Type],Vars]);true)).

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
	checktypes3(L2,TypeStatements3,TypeStatements2,TypeStatements6),!.






interpretbody(_Functions1,_Functions2,Vars,Vars,[],true) :- true.%%!.



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

interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
        Body=[[Statements1|Statements1a]|Statements2
        ],
	
		not(predicate_or_rule_name(Statements1)),
%%writeln1(interpretbody(Functions0,Functions,Vars1,Vars3,[Statement],Result2)),
	interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],Result2), %% 2->1
        %%((Result2=cut)->!;true),

	interpretbody(Functions0,Functions,Vars3,Vars4,Statements1a,Result22), %% 2->1
        %%((Result22=cut)->!;true),
        interpretbody(Functions0,Functions,Vars4,Vars2,Statements2,Result3),
       %%((Result3=cut)->!;true),
  %%()      logicalnot(Result2,Result4), 
logicalconjunction(Result1a,Result2,Result22),
logicalconjunction(Result1,Result1a,Result3),
	true.%%!.



        
interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
	get_lang_word("n",Dbw_n),get_lang_word("not",Dbw_not),
        Body=[[[Dbw_n,Dbw_not],[Statement]]|Statements2
        ],

debug_call(Skip,[[Dbw_n,Dbw_not]]),
        (	(not(interpretbody(Functions0,Functions,Vars1,_Vars3,[Statement],Result22)))-> %% 2->1
        %%((Result22=cut)->!;true)),%%->
debug_exit(Skip,[[Dbw_n,Dbw_not]])
;     debug_fail(Skip,[[Dbw_n,Dbw_not]])),
	%%writeln1(interpretbody(Functions0,Functions,Vars1,Vars3,[Statement],Result2)),

        interpretbody(Functions0,Functions,Vars1,Vars2,Statements2,Result32),
        %%((Result32=cut)->!;true),
       logicalnot(Result1a,Result22), 
logicalconjunction(Result1,Result1a,Result32),
	true.%%!.
	
	


interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result0) :-
	get_lang_word("n",Dbw_n),get_lang_word("or",Dbw_or),

        Body=[[[Dbw_n,Dbw_or],[Statements1,Statements2]]|Statements3],
        (interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],Result1)
        %%((Result1=cut)->!;true)); %% *** changed from 1 to Result2
	%%,((Value1=cut)->!;true))
	;
        interpretbody(Functions0,Functions,Vars1,Vars3,[Statements2],Result2)),%%!. *** changed from 1 to Result2
        %%((Result2=cut)->!;true),

        interpretbody(Functions0,Functions,Vars3,Vars2,Statements3,Result3),
        %%((Result3=cut)->!;true),
        logicaldisjunction(Result1a,Result1,Result2),
        logicalconjunction(Result0,Result1a,Result3),
        	true.%%!.


	%%,((Value=cut)->!;true)).
	%%(logicaldisjunction(Result1,Value1,Value2)->true;(Result1=false)).


interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
	get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,


        Body=[[[Dbw_n,"->"],[Statements1,Statements2]]|Statements3],
        (interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],Result2)
                %%((Result2=cut)->!;true))
-> 
                interpretbody(Functions0,Functions,Vars3,Vars4,[Statements2],Result22)),
                 %%((Result22=cut)->!;true))),

        interpretbody(Functions0,Functions,Vars4,Vars2,Statements3,Result3),
               %%((Result3=cut)->!;true),
        logicalconjunction(Result1a,Result2,Result22),
        logicalconjunction(Result1,Result1a,Result3),

        	true.%%!.




interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
	get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,

        Body=[[[Dbw_n,"->"],[Statements1,Statements2,Statements2a]]|Statements3],
        ((interpretbody(Functions0,Functions,Vars1,Vars3,[Statements1],Result2)
           %%((Result2=cut)->!;true))
           -> 
                interpretbody(Functions0,Functions,Vars3,Vars4,[Statements2],Result22)
                %%((Result22=cut)->!;true))
                ;
                interpretbody(Functions0,Functions,Vars1,Vars4,[Statements2a],Result23))),
                %%((Result23=cut)->!;true))),

        interpretbody(Functions0,Functions,Vars4,Vars2,Statements3,Result3),
        
                logicalconjunction(Result1a,Result2,Result22),
                logicaldisjunction(Result1b,Result1a,Result23),
                logicalconjunction(Result1,Result1b,Result3),

        %%((Result3=cut)->!;true),
        	true.%%!.


interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
	Body=[Statement|Statements],
%%writeln1(["Functions0",Functions0,"Functions",Functions,"Statement",Statement,"Vars1",Vars1,"Vars3",Vars3,"Result2",Result2,"Cut",Cut]),
	not(predicate_or_rule_name(Statement)),
	interpretstatement1(Functions0,Functions,Statement,Vars1,Vars3,Result2,Cut),
%%writeln1(["here1"]),
%trace,
	((not(Cut=cut))->(Functions2=Functions);(%%trace,
	!,turncut(on))
	), %% cut to interpret1/2 (assertz)
%%writeln1(["here3"]),
	interpretbody(Functions0,Functions2,Vars3,Vars2,Statements,Result3),
	%%((Result3=cut)->!;true),
%%writeln1(["here4"]),
	logicalconjunction(Result1,Result2,Result3)	,true.%%,!.
%%writeln1([Result1,Result2,Result3]).
turncut(State1) :-
	cut(State2),
	retract(cut(State2)),
	assertz(cut(State1)).
turndebug(State1) :-
	debug(State2),
	retract(debug(State2)),
	assertz(debug(State1)).
logicaldisjunction(true,true,true) :- !.
logicaldisjunction(true,false,true) :- !.
logicaldisjunction(true,true,false) :- !.
logicaldisjunction(false,false,true) :- !.
logicalconjunction(true,true,true) :- !.
logicalconjunction(false,false,true) :- !.
logicalconjunction(false,true,false) :- !.
logicalconjunction(false,false,false) :- !.
logicalnot(Result1,Result2) :-
	true(Result1),false(Result2).
logicalnot(Result1,Result2) :-
        false(Result1),true(Result2).
true(true).
false(false).

%%interpretstatement1(_F0,[],_,Vars,Vars,true,nocut) :- !
%%writeln1("AND HERE!")
%%	.

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_trace]],Vars,Vars,true,nocut) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("trace",Dbw_trace1),Dbw_trace1=Dbw_trace,
turndebug(on),
!.

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_notrace]],Vars,Vars,true,nocut) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("notrace",Dbw_notrace1),Dbw_notrace1=Dbw_notrace,
turndebug(off),
!.

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_cut]],Vars,Vars,true,cut) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("cut",Dbw_cut1),Dbw_cut1=Dbw_cut,!.
interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_true]],Vars,Vars,_,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("true",Dbw_true1),Dbw_true1=Dbw_true,!.
interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_fail]],Vars,Vars,_,nocut) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("fail",Dbw_fail1),Dbw_fail1=Dbw_fail,
fail.

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

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_atom],[Variable]],Vars,Vars,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("atom",Dbw_atom1),Dbw_atom1=Dbw_atom,

	getvalue(Variable,Value,Vars),
debug_call(Skip,[[Dbw_n,Dbw_atom],[Value]]),
	(atom(Value)->
debug_exit(Skip,[[Dbw_n,Dbw_atom],[Value]])
;     debug_fail(Skip,[[Dbw_n,Dbw_atom],[Value]])),!.

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_string],[Variable]],Vars,Vars,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("string",Dbw_string1),Dbw_string=Dbw_string1,

        getvalue(Variable,Value,Vars),
debug_call(Skip,[[Dbw_n,Dbw_string],[Value]]),
	(string(Value)->
debug_exit(Skip,[[Dbw_n,Dbw_string],[Value]])
;     debug_fail(Skip,[[Dbw_n,Dbw_string],[Value]])),!.

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_number],[Variable]],Vars,Vars,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("number",Dbw_number1),Dbw_number1=Dbw_number,
        getvalue(Variable,Value,Vars),
debug_call(Skip,[[Dbw_n,Dbw_number],[Value]]),
	(number(Value)->
debug_exit(Skip,[[Dbw_n,Dbw_number],[Value]])
;     debug_fail(Skip,[[Dbw_n,Dbw_number],[Value]])),!.

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_letters],[Variable]],Vars,Vars,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("letters",Dbw_letters1),Dbw_letters1=Dbw_letters,

        getvalue(Variable,Value,Vars),
debug_call(Skip,[[Dbw_n,Dbw_letters],[Value]]),
        ((string_codes(Value,Value1),
        phrase(word1(Value1),_))->
debug_exit(Skip,[[Dbw_n,Dbw_letters],[Value]])
;     debug_fail(Skip,[[Dbw_n,Dbw_letters],[Value]])),!.

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_variable],[Variable]],Vars,Vars,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("variable",Dbw_variable1),Dbw_variable1=Dbw_variable,

debug_call(Skip,[[Dbw_n,Dbw_variable],[Variable]]),
        (isvar(Variable)->
debug_exit(Skip,[[Dbw_n,Dbw_variable],[Variable]])
;     debug_fail(Skip,[[Dbw_n,Dbw_variable],[Variable]])),!.

/**interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1]],Vars1,Vars2,true,nocut) :-
	isop(Operator),
	interpretpart(is,Variable1,Vars1,Vars2),!.
**/


interpretstatement1(_F0,_Functions,[[Dbw_n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
	isop(Operator),
	interpretpart(is,Variable1,Variable2,Vars1,Vars2),!.

/**
interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln1(31),
        isop(Operator),
        interpretpart(is,Variable2,Variable1,Vars1,Vars2).
**/

interpretstatement1(_F0,_Functions,[[Dbw_n,Operator],[Variable2,Variable3,Variable1]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
	operator(Operator),
%%writeln1(4),
        interpretpart(isop,Operator,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
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

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_equals1],[Variable1,[Variable2,Variable3]]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("equals1",Dbw_equals11),Dbw_equals11=Dbw_equals1,

%%writeln1(5),
        interpretpart(match1,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_equals2],[Variable1,[Variable2,Variable3]]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("equals2",Dbw_equals21),Dbw_equals21=Dbw_equals2,

%%writeln1(5),
        interpretpart(match2,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_equals3],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("equals3",Dbw_equals31),Dbw_equals31=Dbw_equals3,
%%writeln1(5),
        interpretpart(match3,Variable1,Variable2,Vars1,Vars2).


interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_equals4],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%trace,
%writeln1(interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_equals4],[Variable1,Variable2]],Vars1,Vars2,true,nocut)),
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("equals4",Dbw_equals41),Dbw_equals41=Dbw_equals4,
get_lang_word("v",Dbw_v),
get_lang_word("sys1",Dbw_sys1),
%%writeln1(5),
%trace,
         remember_and_turn_off_debug(Debug),
 	%trace,
         (interpretpart(match4,Variable1,Variable2,Vars1,Vars5,_)->true;(turn_back_debug(Debug),fail)),
         
         interpretpart(match4,Variable1,[Dbw_v,Dbw_sys1],Vars5,Vars4,_),

  	  	  getvalue([Dbw_v,Dbw_sys1],Value3,Vars4),
 	  	  
 	  	  turn_back_debug(Debug),
 
 
        interpretpart(match4,Variable1,Variable2,Vars1,Vars2,Value3),!.

%%interpretstatement1(_F0,_Functions,[[Variable2,Variable3]=Variable1],Vars1,Vars2,true,nocut) :-
%%writeln1(51),
%%        interpretpart(match,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_wrap],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("wrap",Dbw_wrap1),Dbw_wrap1=Dbw_wrap,
%%writeln1(52), wrap
%%writeln([[n,wrap],[Variable1,Variable2]]),
        interpretpart(bracket1,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_unwrap],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("unwrap",Dbw_unwrap1),Dbw_unwrap1=Dbw_unwrap,
%%writeln1(53), unwrap
        interpretpart(bracket2,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_head],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("head",Dbw_head1),Dbw_head1=Dbw_head,

%%writeln1(6),
        interpretpart(head,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_tail],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("tail",Dbw_tail1),Dbw_tail1=Dbw_tail,
%%writeln1(61),
        interpretpart(tail,Variable1,Variable2,Vars1,Vars2).


interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_member],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("member",Dbw_member1),Dbw_member1=Dbw_member,
%%writeln1(8),
        interpretpart(member,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_member2],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
%trace,
get_lang_word("member2",Dbw_member21),Dbw_member21=Dbw_member2,
%%writeln1(8),
        interpretpart(member2,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_member2],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
%trace,
get_lang_word("member3",Dbw_member21),Dbw_member21=Dbw_member2,
%%writeln1(8),
        interpretpart(member3,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_delete],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("delete",Dbw_delete1),Dbw_delete1=Dbw_delete,
%%writeln1(),
        interpretpart(delete,Variable1,Variable2,Variable3,Vars1,Vars2).
%%** all in form f,[1,1,etc], including + with 0,1

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_append],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("append",Dbw_append1),Dbw_append1=Dbw_append,
%%writeln1(9),
        interpretpart(append,Variable1,Variable2,Variable3,Vars1,Vars2).


interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_stringconcat],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("stringconcat",Dbw_stringconcat1),Dbw_stringconcat1=Dbw_stringconcat,

        interpretpart(stringconcat,Variable1,Variable2,Variable3,Vars1,Vars2).

/**
interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_stringconcat_1],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("stringconcat1",Dbw_stringconcat1),Dbw_stringconcat1=Dbw_stringconcat_1,

        interpretpart(stringconcat1,Variable1,Variable2,Variable3,Vars1,Vars2).
**/

/**interpretstatement1(_F0,_Functions,[[n,grammar_part]|Variables1],Vars1,Vars2,true,nocut) :-
%%writeln1(x9),
		  [Variables2]=Variables1,
        interpretpart(grammar_part,Variables2,Vars1,Vars2),!.**/

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_stringtonumber],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("stringtonumber",Dbw_stringtonumber1),Dbw_stringtonumber1=Dbw_stringtonumber,
        interpretpart(stringtonumber,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_random],[Variable1]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("random",Dbw_random1),Dbw_random1=Dbw_random,
        interpretpart(random,Variable1,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_length],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("length",Dbw_length1),Dbw_length1=Dbw_length,
        interpretpart(length,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_ceiling],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("ceiling",Dbw_ceiling1),Dbw_ceiling1=Dbw_ceiling,
        interpretpart(ceiling,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_date],[Year,Month,Day,Hour,Minute,Seconds]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("date",Dbw_date1),Dbw_date1=Dbw_date,
        interpretpart(date,Year,Month,Day,Hour,Minute,Seconds,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_round],[N1,N2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("round",Dbw_round1),Dbw_round1=Dbw_round,
        interpretpart(round,N1,N2,Vars1,Vars2).

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

interpretstatement1(_Grammar,_Grammar2,[[Dbw_n,grammar_part],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
%get_lang_word("grammar_part",Dbw_grammar_part1),Dbw_grammar_part1=Dbw_grammar_part,


%%writeln1("h1/10"),
%%trace,%%%%****
	interpretpart(grammar_part,[Variable1,Variable2,Variable3],Vars1,Vars2).

interpretstatement1(Functions0,Functions,[[Dbw_n,Dbw_findall],[Variable1,Body,Variable3]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("findall",Dbw_findall1),Dbw_findall1=Dbw_findall,
get_lang_word("v",Dbw_v),


%%writeln1(interpretstatement1(Functions0,Functions,[[n,findall],[Variable1,Body,Variable3]],Vars1,Vars2,true,nocut)),
%%writeln1("h1/10"),
%%trace,%%%%****
%%	trace,
	debug_call(Skip,[[Dbw_n,Dbw_findall],[Variable1,Body,Variable3]]),
((
	findall(Value3,(
	%%trace,
	%%writeln1(	interpretbody(Functions0,Functions,Vars1,Vars3,[Body],Result2)),

	interpretbody(Functions0,Functions,Vars1,Vars3,[Body],_Result2), %% 2->1
	%%((Result2=cut)->!;true),
	%%trace,
	%%(cut(on)->(%%notrace,
	%%fail);(%%trace,
	%%true)),%%notrace,
		
	 remember_and_turn_off_debug(Debug),
	%%trace,
find_findall_sys(Findall_sys_name),
        interpretpart(match4,Variable1,[Dbw_v,Findall_sys_name],Vars3,Vars2,_),
%%writeln1(        interpretpart(match4,Variable1,[v,sys1],Vars3,Vars2,_)),
%%interpretstatement1(Functions0,Functions,[[n,equals4],[Variable1,Variable3]],Vars3,Vars2,true,nocut),
	getvalue([Dbw_v,Findall_sys_name],Value3,Vars2),
	
	 turn_back_debug(Debug)

	),Value3a),
	putvalue(Variable3,Value3a,Vars1,Vars2)
        )->
debug_exit(Skip,[[Dbw_n,Dbw_findall],[Variable1,Body,Value3a]])
;     debug_fail(Skip,[[Dbw_n,Dbw_findall],[Variable1,Body,Variable3]])).


interpretstatement1(_Functions0,_Functions,[[Dbw_n,Dbw_string_from_file],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("string_from_file",Dbw_string_from_file1),Dbw_string_from_file1=Dbw_string_from_file,
        interpretpart(string_from_file,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(Functions0,Functions,[[Dbw_n,Dbw_maplist],[Variable1,Variable2,Variable3,Variable4]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("maplist",Dbw_maplist1),Dbw_maplist1=Dbw_maplist,

        interpretpart(maplist,Functions0,Functions,Variable1,Variable2,Variable3,Variable4,Vars1,Vars2).


interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_string_length],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("string_length",Dbw_string_length1),Dbw_string_length1=Dbw_string_length,
        interpretpart(string_length,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_sort],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("sort",Dbw_sort1),Dbw_sort1=Dbw_sort,
        interpretpart(sort,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_intersection],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("intersection",Dbw_intersection1),Dbw_intersection1=Dbw_intersection,
        interpretpart(intersection,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_read_string],[Variable1]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("read_string",Dbw_read_string1),Dbw_read_string1=Dbw_read_string,
        interpretpart(read_string,Variable1,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_writeln],[Variable1]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("writeln",Dbw_writeln1),Dbw_writeln1=Dbw_writeln,
        interpretpart(writeln,Variable1,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_atom_string],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("atom_string",Dbw_atom_string1),Dbw_atom_string1=Dbw_atom_string,
        interpretpart(atom_string,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[Dbw_n,Dbw_get_lang_word],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("get_lang_word",Dbw_get_lang_word1),Dbw_get_lang_word1=Dbw_get_lang_word,
        interpretpart(get_lang_word,Variable1,Variable2,Vars1,Vars2).

	%%interpretpart(findall,[Variable1,Variable3],Vars3,Vars2).

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

interpretstatement1(_Functions0,_Functions,Query1,Vars1,Vars8,true,nocut) :-
get_lang_word("v",Dbw_v),
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("call",Dbw_call1),Dbw_call1=Dbw_call,

%%writeln1("h1/10"),

%find_pred_sm(Reserved_words1),
        ((Query1=[[Dbw_n,Dbw_call],[[lang,Lang1],Debug1,[Function,Arguments],Functions%,Result
        ]],Tm=off%,
        %not(member(Dbw_call,Reserved_words1))
        )->true;
        (Query1=[[Dbw_n,Dbw_call],[[lang,Lang1],Debug1,[Function,Arguments],Types,Modes,Functions%,Result
        ]],Tm=on)),        
        
        lang(Lang2a),
        types(Types2a),
		  (Types2a=on->(typestatements(TypeStatements2a),
		  modestatements(ModeStatements2a));true),
		  
        (Lang1=same->lang(Lang2);Lang2=Lang1),
        (Debug1=same->debug(Debug2);Debug2=Debug1),
        
        %%not(Function=[n,grammar]->true;Function=[n,grammar_part]), ****
%%writeln1(["Arguments",Arguments,"Vars1",Vars1]),
        %%***writeln1(substitutevarsA1(Arguments,Vars1,[],Vars3,[],FirstArgs)),
        (Function=[Dbw_v,_]->
        (append([Function],Arguments,Arguments1),
        substitutevarsA1(Arguments1,Vars1,[],Vars3,[],FirstArgs),
        Vars3=[Function1|Vars31],
        Query2=[Function1,Vars31]);
        (substitutevarsA1(Arguments,Vars1,[],Vars3,[],FirstArgs), %%% var to value, after updatevars:  more vars to values, and select argument vars from latest vars
%%writeln1([substitutevarsA1,arguments,Arguments,vars1,Vars1,vars3,Vars3,firstargs,FirstArgs]),
        Query2=[Function,Vars3])), %% Bodyvars2?
%%        	debug(on)->writeln1([call,[Function,[Vars3]]]),
%%writeln1(["Query2",Query2,"Functions0",Functions0]),
        
        
        %interpret2(Query2,Functions0,Functions0,Result1), 
        
(Tm=off->international_interpret([lang,Lang2],Debug2,Query2,Functions,Result1a);
	international_interpret([lang,Lang2],Debug2,Query2,Types,Modes,Functions,Result1a)),
	member(Result1,Result1a),

	retractall(lang(_)),
 	assertz(lang(Lang2a)),

	retractall(types(_)),
 	assertz(types(Types2a)),

		  (Types2a=on->(
		  	retractall(typestatements(_)),
 	assertz(typestatements(TypeStatements2a)),
	retractall(modestatements(_)),
 	assertz(modestatements(ModeStatements2a)));true),


	updatevars2(FirstArgs,Result1,[],Vars5),
	updatevars3(Vars1,Vars5,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
);(
%%writeln1(here1),
	Vars8=[])).        


        
        interpretstatement1(Functions0,_Functions,Query1,Vars1,Vars8,true,nocut) :-
get_lang_word("v",Dbw_v),
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("call",Dbw_call1),Dbw_call1=Dbw_call,

%%writeln1("h1/10"),
%trace,
%writeln([Functions0,Functions0]),
find_pred_sm(Reserved_words1),

        %trace,
        ((Query1=[[Dbw_n,Dbw_call],[Function,Arguments]]%,        not(member(Dbw_call,Reserved_words1))
        )->true;
(Query1=[Function,Arguments]%,Function=[Dbw_n1,Function_a],atom_string(Function_a,Function_s),
%not(member(Function_s,Reserved_words1))
)
),

%trace,
        %%not(Function=[n,grammar]->true;Function=[n,grammar_part]), ****
%%writeln1(["Arguments",Arguments,"Vars1",Vars1]),
        %%***writeln1(substitutevarsA1(Arguments,Vars1,[],Vars3,[],FirstArgs)),
        (Function=[Dbw_v,_]->
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
interpretstatement1(Functions0,_Functions,Query,Vars,Vars,true,nocut) :-
	Query=[_Function],
debug_call(Skip,[Function]),
        (interpret2(Query,Functions0,Functions0,_Result1)->
debug_exit(Skip,[Function])
;     debug_fail(Skip,[Function])),!.


debug_react(Status,115,true) :- Status=call, 
turndebug(off), write(" "),get_lang_word("skip",Dbw_skip),writeln(Dbw_skip). %% skip
debug_react(_Status,97,false) :- write(" "),get_lang_word("abort",Dbw_abort),writeln(Dbw_abort),abort. %% abort
debug_react(Status,A,false) :- ((Status=call,not(A=115),not(A=97))->true;
(member_exit_fail(Status),not(A=97))), write(" "),get_lang_word("creep",Dbw_creep),writeln(Dbw_creep). %% creep

member_exit_fail(exit).
member_exit_fail(fail).

debug_call(Skip,FunctionArguments1) :-
get_lang_word("call",Dbw_call),
get_lang_word("Press c to creep, s to skip or a to abort.",Dbw_note1),

(debug(on)->(write1([Dbw_call,FunctionArguments1,Dbw_note1]),(leash1(on)->writeln("");(get_single_char(Key),debug_react(call,Key,Skip))));Skip=false).

debug_fail_fail(Skip) :-
(debug(on)->(Skip=true->turndebug(on);true);true).

debug_fail(Skip,FunctionArguments1) :-
get_lang_word("fail",Dbw_fail),
get_lang_word("Press c to creep or a to abort.",Dbw_note1),


((Skip=true->turndebug(on);true),((debug(on)->(write1([Dbw_fail,FunctionArguments1,Dbw_note1]),(leash1(on)->writeln("");(get_single_char(Key),debug_react(fail,Key,_Skip))));true),fail)).

debug_exit(Skip,FunctionResult2) :-
get_lang_word("exit",Dbw_exit),
get_lang_word("Press c to creep or a to abort.",Dbw_note1),
((Skip=true->turndebug(on);true),((debug(on)->(write1([Dbw_exit,FunctionResult2,Dbw_note1]),(leash1(on)->writeln("");(get_single_char(Key),debug_react(exit,Key,_Skip))));true))).


debug_types_call(FunctionArguments1) :-
get_lang_word("call",Dbw_call),
debug_types(Dbw_call,FunctionArguments1).
debug_types(Call,FunctionArguments1) :-
(debug(on)->(writeln1([Call,FunctionArguments1]));true).

debug_types_fail(FunctionArguments1) :-
get_lang_word("fail",Dbw_fail),
((debug(on)->(writeln1([Dbw_fail,FunctionArguments1]));true),fail).

debug_types_exit(FunctionResult2) :-
get_lang_word("exit",Dbw_exit),
debug_types(Dbw_exit,FunctionResult2).


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
isvar([Dbw_v,_Value]) :- 
get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,!.
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
get_lang_word("v",Dbw_v),
	not(N=Dbw_v),not(N=[Dbw_v,_]),
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
	get_lang_word("v",Dbw_v),
	not(N=[v,_]),not(N=["v",_]),not(N=[Dbw_v,_]),expression(N),!.

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


 remember_and_turn_off_debug(Debug) :-
 	debug(Debug),retractall(debug(_)),assertz(debug(off)).
 
 turn_back_debug(Debug) :-
 	retractall(debug(_)),assertz(debug(Debug)).

find_findall_sys(Name2) :-
	findall_sys(N1),
	concat_list(["findall_sys",N1],Name1),
	get_lang_word(Name1,Name2),
	%atom_string(Name2,Name1),
	N2 is N1+1,
	retractall(findall_sys(_)),
 	assertz(findall_sys(N2)).
