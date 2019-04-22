:- dynamic debug/1.
:- dynamic cut/1.

/** List Prolog Interpreter **/

interpret(Debug,Query,Functions1,Result) :-
%%writeln([i1]),
	convert_to_grammar_part1(Functions1,[],Functions2),
	%%writeln(Functions2),
%%writeln(Functions2),
	interpret1(Debug,Query,Functions2,Functions2,Result),
	!.
interpret1(Debug,Query,Functions1,Functions2,Result) :-
%%writeln([i11]),
	retractall(debug(_)),
    	assertz(debug(Debug)),
        retractall(cut(_)),
        assertz(cut(off)),
	member1(Query,Functions1,Functions2,Result).
%%member1([_,R],_,[],R).
%%member1(_,_,[],[]).
member1(_,_,[],_) :- fail,!.
member1(Query,Functions,Functions2,Vars8) :-
%%writeln([m1]),
	cut(off)->(
        (Query=[Function,Arguments1],
	(Functions2=[[Function,Arguments2,":-",Body]|_Functions3]),
	length(Arguments1,Length),
	length(Arguments2,Length),
        
        ((Function=[n,grammar]->true;Function=[n,grammar_part])->checkarguments1(Arguments1,Arguments2,[],Vars1,[],FirstArgs);checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),!),
        %%->ca2 
%%writeln([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
                (debug(on)->(writeln([call,[Function,Arguments1],"Press c."]),(not(get_single_char(97))->true;abort));true),
	interpretbody(Functions,Functions2,Vars1,Vars2,Body,true),
	updatevars(FirstArgs,Vars2,[],Result),
        %%reverse(Result,[],Vars7),
	((not(Result=[])->
        %%Result=[Var71|Vars72],
        unique1(Result,[],Vars8),
%%writeln(["FirstArgs",FirstArgs,"Vars",Vars2,"Result",Result,"Vars7",Vars7,"Vars72",Vars72,"Var71",Var71,"Vars8",Vars8]),
%%writeln(["Vars8",Vars8]),
	findresult3(Arguments1,Vars8,[],Result2)
%%writeln([findresult3,"Arguments1",Arguments1,"Vars8",Vars8,"Result2",Result2])
	);(
%%writeln(here1),
	Vars8=[],Result2=[])),
%%writeln(["Arguments1",Arguments1,"Vars2",Vars2,"Result",Result]),
		(debug(on)->(writeln([exit,[Function,Result2],"Press c."]),(not(get_single_char(97))->true;abort));true))
	->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2,":-",_Body]|Functions3], %% make like previous trunk?
	member11(Query,Functions,Functions2,Vars8))
	);(turncut(off)%%,Result=[]
	).
member11(Query,Functions,Functions2,Result) :-
%%writeln([m11]),
%%writeln(["Query",Query,"Functions",Functions,"Functions2",Functions2,"Result",Result]),
	cut(off)->(
        (Query=[Function],
        (Functions2=[[Function,":-",Body]|_Functions3]),
                (debug(on)->(writeln([call,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true),
	Result=[],
        interpretbody(Functions,Functions2,[],_Vars2,Body,true),!,
        	(debug(on)->(writeln([exit,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true)
	)->true;
	(%%Query=[Function],
	%%Functions2=[[Function]|Functions3],
	member12(Query,Functions,Functions2,Result))
	);(turncut(off)).
member12(Query,Functions,Functions2,Vars8) :-
%%writeln([m12]),
	cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        ((Function=[n,grammar]->true;Function=[n,grammar_part])->checkarguments1(Arguments1,Arguments2,[],Vars1,[],FirstArgs);checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),!),
%%writeln([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
	updatevars(FirstArgs,Vars1,[],Result),
        %%reverse(Result,[],Vars7),
        ((not(Result=[])->
        %%Result=[Var71|Vars72],
        unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
        );(
%%writeln(here2),
	Vars8=[],Result2=[])),
        	(debug(on)->(writeln([call,[Function,Arguments1],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[Function,Result2],"Press c."]),(not(get_single_char(97))->true;abort));true)
	)->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2]|Functions3],
	member13(Query,Functions,Functions2,Vars8))
	);(turncut(off)).
member13(Query,Functions,Functions2,Result) :-
%%writeln([m13]),
	cut(off)->(
        (Query=[Function],!,
        (Functions2=[[Function]|_Functions3]),
        	(debug(on)->(writeln([call,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true),
	Result=[],
        %%interpretbody(Functions,[],_Vars2,Body,true),
        	(debug(on)->(writeln([exit,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true)
	)->true;
	(%%Query=[Function],
	Functions2=[_Function|Functions3],
	member1(Query,Functions,Functions3,Result))
	);(turncut(off)).
interpret2(Query,Functions1,Functions2,Result) :-
%%writeln(i2),
%%writeln(["%%interpret2 Query",Query,"Functions1",Functions1,"Functions2",Functions2]),
        member2(Query,Functions1,Functions2,Result).
%%member2([_,R],_,[],R).
%%member2(_,_,[],[]).
member2(_,_,[],_) :- fail,!.
member2(Query,Functions,Functions2,Vars8) :-
%%writeln([m2]),
	cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2,":-",Body]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        ((Function=[n,grammar]->true;Function=[n,grammar_part])->checkarguments1(Arguments1,Arguments2,[],Vars1,[],FirstArgs);checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),!),
%%writeln([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
                (debug(on)->(writeln([call,[Function,Arguments1],"Press c."]),(not(get_single_char(97))->true;abort));true),
        interpretbody(Functions,Functions2,Vars1,Vars2,Body,true), %%**arg2 change
%%writeln(["Functions",Functions,"Functions2",Functions2,"Vars1",Vars1,"Vars2",Vars2,"Body",Body]),
        updatevars(FirstArgs,Vars2,[],Result),
        %%reverse(Result,[],Vars7),
        ((not(Result=[])->
        %%Result=[Var71|Vars72],
        unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
%%writeln(["Vars2",Vars2,"Result",Result]),
        );(
	%%writeln(here3),
	Vars8=[],Result2=[])),
        	(debug(on)->(writeln([exit,[Function,Result2],"Press c."]),(not(get_single_char(97))->true;abort));true)
	)->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2,":-",_Body]|Functions3],
	member21(Query,Functions,Functions2,Vars8))
	);(turncut(off)).
member21(Query,Functions,Functions2,Result) :-
%%writeln([m21]),
	cut(off)->(
        (Query=[Function],
        (Functions2=[[Function,":-",Body]|_Functions3]),
        Vars1=[],
                (debug(on)->(writeln([call,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true),
        interpretbody(Functions,Functions2,Vars1,_Vars2,Body,true),!, %%**arg2 change
        	(debug(on)->(writeln([exit,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true)
	)->true;
	(%%Query=[Function],
	%%Functions2=[[Function]|Functions3],
	member22(Query,Functions,Functions2,Result))
	);(turncut(off)).
member22(Query,Functions,Functions2,Vars8) :-
%%writeln([m22]),
	cut(off)->(
        (Query=[Function,Arguments1],
        (Functions2=[[Function,Arguments2]|_Functions3]),
        length(Arguments1,Length),
        length(Arguments2,Length),
        ((Function=[n,grammar]->true;Function=[n,grammar_part])->checkarguments1(Arguments1,Arguments2,[],Vars1,[],FirstArgs);checkarguments(Arguments1,Arguments2,[],Vars1,[],FirstArgs),!),
%%writeln([checkarguments,"Arguments1",Arguments1,"Arguments2",Arguments2,"Vars1",Vars1,"FirstArgs",FirstArgs]),
        updatevars(FirstArgs,Vars1,[],Result),
        %%reverse(Result,[],Vars7),
        ((not(Result=[])->
        %%Result=[Var71|Vars72],
        unique1(Result,[],Vars8),
        findresult3(Arguments1,Vars8,[],Result2)
        );(
%%writeln(here4),
	Vars8=[],Result2=[])),
        	(debug(on)->(writeln([call,[Function,Arguments1],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[Function,Result2],"Press c."]),(not(get_single_char(97))->true;abort));true)
	)->true;
	(%%Query=[Function,_Arguments1],
	%%Functions2=[[Function,_Arguments2]|Functions3],
	member23(Query,Functions,Functions2,Vars8))
	);(turncut(off)).
member23(Query,Functions,Functions2,Vars8) :-
%%writeln([m23]),
	cut(off)->(
        (Query=[Function],!,
        (Functions2=[[Function]|_Functions3]),
        	(debug(on)->(writeln([call,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true),
	Vars8=[],
        	(debug(on)->(writeln([exit,[Function],"Press c."]),(not(get_single_char(97))->true;abort));true)
	)->true;
	(%%Query=[Function],
	Functions2=[_Function|Functions3],
	member2(Query,Functions,Functions3,Vars8))
	);(turncut(off)).
checkarguments([],[],Vars,Vars,FirstArgs,FirstArgs). 
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :- %%
%%writeln(1),
	Arguments1=[Value|Arguments3], %% Value may be a number, string, list or tree
	expressionnotatom(Value),
	Arguments2=[Variable2|Arguments4],
	not(var(Variable2)),isvar(Variable2),
	putvalue(Variable2,Value,Vars1,Vars3),
	checkarguments(Arguments3,Arguments4,Vars3,Vars2,FirstArgs1,FirstArgs2).
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :- %%A
%%writeln(2),
        Arguments1=[Variable|Arguments3], %% Value may be a number, string, list or tree
        not(var(Variable)),isvar(Variable),
        Arguments2=[Value|Arguments4],
        expressionnotatom(Value),
        putvalue(Variable,Value,Vars1,Vars3),
	append(FirstArgs1,[[Variable,Value]],FirstArgs3),
        checkarguments(Arguments3,Arguments4,Vars3,Vars2,FirstArgs3,FirstArgs2).
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
%%writeln(3),
        Arguments1=[Variable1|Arguments3],
	not(var(Variable1)),isvar(Variable1),
        Arguments2=[Variable2|Arguments4],
	not(var(Variable2)),isvar(Variable2),
	(getvalue(Variable2,Value,Vars1)->true;Value=empty), 
	%%((Value=empty->Value1=Variable2;Value1=Value))),
        putvalue(Variable2,Value,Vars1,Vars3),
        append(FirstArgs1,[[Variable1,Variable2]],FirstArgs3),
        checkarguments(Arguments3,Arguments4,Vars3,Vars2,FirstArgs3,FirstArgs2).
checkarguments(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
%%writeln(4),
        Arguments1=[Value1|Arguments3],
        expressionnotatom(Value1),
        Arguments2=[Value1|Arguments4],
        expressionnotatom(Value1),
        checkarguments(Arguments3,Arguments4,Vars1,Vars2,FirstArgs1,FirstArgs2).

checkarguments1([],[],Vars,Vars,FirstArgs,FirstArgs). 
checkarguments1(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :- %%
%%writeln(1),
	Arguments1=[Value|Arguments3], %% Value may be a number, string, list or tree
	expressionnotatom(Value),
	Arguments2=[Variable2|Arguments4],
	not(var(Variable2)),isvar(Variable2),
	putvalue(Variable2,Value,Vars1,Vars3),
	checkarguments1(Arguments3,Arguments4,Vars3,Vars2,FirstArgs1,FirstArgs2).
checkarguments1(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :- %%A
%%writeln(2),
        Arguments1=[Variable|Arguments3], %% Value may be a number, string, list or tree
        not(var(Variable)),isvar(Variable),
        Arguments2=[Value|Arguments4],
        expressionnotatom(Value),
        putvalue(Variable,Value,Vars1,Vars3),
	append(FirstArgs1,[[Variable,_]],FirstArgs3),
        checkarguments1(Arguments3,Arguments4,Vars3,Vars2,FirstArgs3,FirstArgs2).
checkarguments1(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
%%writeln(3),
        Arguments1=[Variable1|Arguments3],
	not(var(Variable1)),isvar(Variable1),
        Arguments2=[Variable2|Arguments4],
	not(var(Variable2)),isvar(Variable2),
	(getvalue(Variable2,Value,Vars1)->((Value=empty->Value1=Variable2;Value1=Value))),
        putvalue(Variable2,Value1,Vars1,Vars3),
        append(FirstArgs1,[[Variable1,Variable2]],FirstArgs3),
        checkarguments1(Arguments3,Arguments4,Vars3,Vars2,FirstArgs3,FirstArgs2).
checkarguments1(Arguments1,Arguments2,Vars1,Vars2,FirstArgs1,FirstArgs2) :-
%%writeln(4),
        Arguments1=[Value1|Arguments3],
        expressionnotatom(Value1),
        Arguments2=[Value1|Arguments4],
        expressionnotatom(Value1),
        checkarguments1(Arguments3,Arguments4,Vars1,Vars2,FirstArgs1,FirstArgs2).
        
interpretbody(_Functions1,_Functions2,Vars,Vars,[],true) :- !.

interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
        Body=[Statements1|Statements2],not(predicate_or_rule_name(Statements1)),
        interpretbody(Functions0,Functions,Vars1,Vars3,Statements1,Result2),
        interpretbody(Functions0,Functions,Vars3,Vars2,Statements2,Result3),
        %%((Result3=cut)->!;true),
        logicalconjunction(Result1,Result2,Result3),!.

interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
        Body=[[not,[Statements]]|Statements2],
	interpretbody(Functions0,Functions,Vars1,Vars3,Statements,Result2),
        %%((Result2=cut)->!;true),
        interpretbody(Functions0,Functions,Vars3,Vars2,Statements2,Result3),
        %%((Result3=cut)->!;true),
        logicalnot(Result2,Result4),
	(logicalconjunction(Result1,Result4,Result3)->true;(Result1=false)),!.
interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
        Body=[[Statements1],or,[Statements2]],
        (interpretbody(Functions0,Functions,Vars1,Vars2,Statements1,Result1)->true;
	%%,((Value1=cut)->!;true));
        interpretbody(Functions0,Functions,Vars1,Vars2,Statements2,Result1)),!.
	%%,((Value=cut)->!;true)).
	%%(logicaldisjunction(Result1,Value1,Value2)->true;(Result1=false)).


interpretbody(Functions0,Functions,Vars1,Vars2,Body,Result1) :-
	Body=[Statement|Statements],
%%writeln(["Functions0",Functions0,"Functions",Functions,"Statement",Statement,"Vars1",Vars1,"Vars3",Vars3,"Result2",Result2,"Cut",Cut]),
	not(predicate_or_rule_name(Statement)),
	interpretstatement1(Functions0,Functions,Statement,Vars1,Vars3,Result2,Cut),
%%writeln(["here1"]),
	((not(Cut=cut))->(Functions2=Functions);(turncut(on))), %% cut to interpret1/2 (assertz)
%%writeln(["here3"]),
	interpretbody(Functions0,Functions2,Vars3,Vars2,Statements,Result3),
	%%((Result3=cut)->!;true),
%%writeln(["here4"]),
	logicalconjunction(Result1,Result2,Result3),!.
%%writeln([Result1,Result2,Result3]).
turncut(State1) :-
	cut(State2),
	retract(cut(State2)),
	assertz(cut(State1)).
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
%%writeln("AND HERE!")
%%	.

interpretstatement1(_F0,_Functions,[[n,cut]],Vars,Vars,true,cut) :- !.

interpretstatement1(_F0,_Functions,[[n,atom],[Variable]],Vars,Vars,true,nocut) :-
	getvalue(Variable,Value,Vars),
                (debug(on)->(writeln([call,[[n,atom],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true),
	atom(Value),
                (debug(on)->(writeln([exit,[[n,atom],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true).

interpretstatement1(_F0,_Functions,[[n,string],[Variable]],Vars,Vars,true,nocut) :-
        getvalue(Variable,Value,Vars),
                (debug(on)->(writeln([call,[[n,string],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        string(Value),
                (debug(on)->(writeln([exit,[[n,string],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true).

interpretstatement1(_F0,_Functions,[[n,number],[Variable]],Vars,Vars,true,nocut) :-
        getvalue(Variable,Value,Vars),
                (debug(on)->(writeln([call,[[n,number],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        number(Value),
                (debug(on)->(writeln([exit,[[n,number],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true).

interpretstatement1(_F0,_Functions,[[n,letters],[Variable]],Vars,Vars,true,nocut) :-
        getvalue(Variable,Value,Vars),
                (debug(on)->(writeln([call,[[n,letters],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        string_codes(Value,Value1),
        phrase(word1(Value1),_),
                (debug(on)->(writeln([exit,[[n,letters],[Value]],"Press c."]),(not(get_single_char(97))->true;abort));true).

interpretstatement1(_F0,_Functions,[[n,variable],[Variable]],Vars,Vars,true,nocut) :-
        var(Variable),
                (debug(on)->(writeln([call,[[n,variable],[Variable]],"Press c."]),(not(get_single_char(97))->true;abort));true),
                (debug(on)->(writeln([exit,[[n,variable],[Variable]],"Press c."]),(not(get_single_char(97))->true;abort));true).

interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
	isop(Operator),
	interpretpart(is,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,Operator],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln(31),
        isop(Operator),
        interpretpart(is,Variable2,Variable1,Vars1,Vars2).
	
interpretstatement1(_F0,_Functions,[[n,+],[Variable2,Variable3,Variable1]],Vars1,Vars2,true,nocut) :-
%%writeln(4),
        interpretpart(isplus,Variable1,Variable2,Variable3,Vars1,Vars2).

%%interpretstatement1(_F0,_Functions,[Variable2+Variable3,is,Variable1],Vars1,Vars2,true,nocut) :-
%%writeln(41),
        %%interpretpart(isplus,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,=],[Variable1,[Variable2,Variable3]]],Vars1,Vars2,true,nocut) :-
%%writeln(5),
        interpretpart(match,Variable1,Variable2,Variable3,Vars1,Vars2).

%%interpretstatement1(_F0,_Functions,[[Variable2,Variable3]=Variable1],Vars1,Vars2,true,nocut) :-
%%writeln(51),
%%        interpretpart(match,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,wrap],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln(52), wrap
        interpretpart(bracket1,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,unwrap],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln(53), unwrap
        interpretpart(bracket2,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,head],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln(6),
        interpretpart(head,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,tail],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln(61),
        interpretpart(tail,Variable1,Variable2,Vars1,Vars2).


interpretstatement1(_F0,_Functions,[[n,member],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln(8),
        interpretpart(member,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,delete],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
%%writeln(),
        interpretpart(delete,Variable1,Variable2,Variable3,Vars1,Vars2).
%%** all in form f,[1,1,etc], including + with 0,1

interpretstatement1(_F0,_Functions,[[n,append],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
%%writeln(9),
        interpretpart(append,Variable1,Variable2,Variable3,Vars1,Vars2).

interpretstatement1(_F0,_Functions,[[n,stringconcat],[Variable1,Variable2,Variable3]],Vars1,Vars2,true,nocut) :-
%%writeln(9),
        interpretpart(stringconcat,Variable1,Variable2,Variable3,Vars1,Vars2).

/**interpretstatement1(_F0,_Functions,[[n,grammar_part]|Variables1],Vars1,Vars2,true,nocut) :-
%%writeln(x9),
		  [Variables2]=Variables1,
        interpretpart(grammar_part,Variables2,Vars1,Vars2),!.**/

interpretstatement1(_F0,_Functions,[[n,stringtonumber],[Variable1,Variable2]],Vars1,Vars2,true,nocut) :-
%%writeln(52), wrap
        interpretpart(stringtonumber,Variable1,Variable2,Vars1,Vars2).

interpretstatement1(Functions0,_Functions,Query1,Vars1,Vars8,true,nocut) :-
%%writeln("h1/10"),
        Query1=[[n,grammar]|Arguments],
        ((Arguments=[[Grammar1,Phrase1,RuleName|Variables2]],
        %%[Variables3]=Variables2,
        name(RuleName),
		  convert_to_grammar_part1(Grammar1,[],Grammar2))->true;
		  (Grammar2=Functions0,
		  ((Arguments=[[Phrase1,RuleName|Variables2]]
		  %%([Variables3]=Variables2->true;(Variables2=[],Variables3=[]))
		  )))),

%%writeln(["Arguments",Arguments,"Vars1",Vars1]),

%%substitutevarsA1(Phrase,Vars1,[],Vars3,[],FirstArgs1),

%%Vars3=[[[v,PhraseVarName],PhraseValue]],
%%Vars4=[[[v,vgp1],PhraseValue]],

   append([Phrase1],Variables2,Variables4), %% *** Should V3 be in [] v

substitutevarsA1(Variables4,Vars1,[],Vars2,[],FirstArgs), %%% var to value, after updatevars:  more vars to values, and select argument vars from latest vars
%%writeln([substitutevarsA1,arguments,Arguments,vars1,Vars1,vars3,Vars3,firstargs,FirstArgs]),
		  
Vars2=[Phrase2|Vars4],
((Phrase2=[]->true;Phrase2=[_A|_B])->End=[];End=""),
		  (not(Vars4=[])->append([RuleName,Phrase2,End],Vars4,Vars5);
		  (Vars5=[RuleName,Phrase2,End])),
		  Query2=[[n,grammar_part],Vars5],
		  ((((terminal(RuleName),  
		  

		  (not(Vars4=[])->append([Phrase2,RuleName],Vars4,Vars52);
		  (Vars52=[Phrase2,RuleName])),
		  
		  (debug(on)->(writeln([call,[[n,grammar],Vars52],"Press c."]),(not(get_single_char(97))->true;abort));true),

      interpretpart(grammar_part,Vars5,[],Result1),
		  
		  	updatevars2(FirstArgs,Result1,[],Vars51),
	updatevars3(Vars2,Vars51,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
)->true;(
%%writeln(here1),
	Vars8=[]),strip(Vars8,[],Result2))->true)),

        	(debug(on)->(writeln([exit,[[n,grammar],Result2],"Press c."]),(not(get_single_char(97))->true;abort));true)

)->true;(not(terminal(RuleName)),
         %% Bodyvars2?
		          	
		          			  (not(Vars4=[])->append([Phrase2,RuleName],Vars4,Vars52);
		  (Vars52=[Phrase2,RuleName])),

(debug(on)->(writeln([call,[[n,grammar],Vars52],"Press c."]),(not(get_single_char(97))->true;abort));true),

%%        	debug(on)->writeln([call,[Function,[Vars3]]]),
%%writeln(["Query2",Query2,"Functions0",Functions0]),
        interpret2(Query2,Grammar2,Grammar2,Result1), 
        	(debug(on)->(writeln([exit,[[n,grammar],Vars52],"Press c."]),(not(get_single_char(97))->true;abort));true),

	updatevars2(FirstArgs,Result1,[],Vars51),
	updatevars3(Vars2,Vars51,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
);(
%%writeln(here1),
	Vars8=[])))),!.


interpretstatement1(Grammar,_Grammar2,Query1,Vars1,Vars8,true,nocut) :-
%%writeln("h1/10"),
%%trace,%%%%****
        Query1=[[n,grammar_part]|Arguments],
        Arguments=[[RuleName|Variables2]],
        	%%(([Variables4|Rest]=Variables2->Variables3=Variables2;(Variables2=[],Variables3=[]))),

        ((not(terminal(RuleName)),
%%writeln(["Arguments",Arguments,"Vars1",Vars1]),
        substitutevarsA1(Variables2,Vars1,[],Vars3,[],FirstArgs), %%% var to value, after updatevars:  more vars to values, and select argument vars from latest vars
%%writeln([substitutevarsA1,arguments,Arguments,vars1,Vars1,vars3,Vars3,firstargs,FirstArgs]),
		  (not(Vars3=[])->(append([RuleName],Vars3,Vars4),Query2=[[n,grammar_part],Vars4]);
		  Query2=[[n,grammar_part],RuleName]), %% Bodyvars2?
%%        	debug(on)->writeln([call,[Function,[Vars3]]]),
%%writeln(["Query2",Query2,"Functions0",Functions0]),
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
%%writeln(here1),
	Vars8=[]))->true)->true;
(terminal(RuleName),substitutevarsA1(Variables2,Vars1,[],Vars3,[],FirstArgs),
%%writeln(here), %%****
%%Vars3=[Phrase,End],
%%Vars41=[Phrase,[v,vgp]],
append([RuleName],Vars3,Vars9),
%%writeln([vars9,Vars9]), %%%%%*****
interpretpart(grammar_part,Vars9,[],Result1),
	updatevars2(FirstArgs,Result1,[],Vars5),
	updatevars3(Vars3,Vars5,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
	%%writeln([vars8,Vars8]) %%%*****
)->true;(
%%writeln(here1),
	Vars8=[]))->true)),%%notrace, %%****
	!.


interpretstatement1(Functions0,_Functions,Query1,Vars1,Vars8,true,nocut) :-
%%writeln("h1/10"),
        Query1=[Function,Arguments],not(Function=[n,grammar]->true;Function=[n,grammar_part]),
%%writeln(["Arguments",Arguments,"Vars1",Vars1]),
        substitutevarsA1(Arguments,Vars1,[],Vars3,[],FirstArgs), %%% var to value, after updatevars:  more vars to values, and select argument vars from latest vars
%%writeln([substitutevarsA1,arguments,Arguments,vars1,Vars1,vars3,Vars3,firstargs,FirstArgs]),
        Query2=[Function,Vars3], %% Bodyvars2?
%%        	debug(on)->writeln([call,[Function,[Vars3]]]),
%%writeln(["Query2",Query2,"Functions0",Functions0]),
        interpret2(Query2,Functions0,Functions0,Result1), 
	updatevars2(FirstArgs,Result1,[],Vars5),
	updatevars3(Vars1,Vars5,Vars6),
	reverse(Vars6,[],Vars7),
	((not(Vars7=[])->
	%%Vars7=[Var71|Vars72],
	unique1(Vars7,[],Vars8)
);(
%%writeln(here1),
	Vars8=[])).
%%**** reverse and take first instance of each variable.
	%%findresult3(Arguments,Vars6,[],Result2)
%%writeln(["FirstArgs",FirstArgs,"Result1",Result1,"Vars5",Vars5,"Vars4",Vars4]),
%%writeln(["Vars1:",Vars1,"Vars4:",Vars4]),
%%		debug(on)->writeln([exit,[Function,[Result2]]]).
interpretstatement1(Functions0,_Functions,Query,Vars,Vars,true) :-
	Query=[_Function],
        	debug(on)->writeln([call,[Function]]),
        interpret2(Query,Functions0,Functions0,_Result1),
        	debug(on)->writeln([exit,[Function]]).

word1([])-->[].
word1([A|As]) --> [A],word1(As),{%%atom_codes(A,AC),
char_type(A,alpha)},!.
/**interpretstatement1(_Functions0, _Functions,_Query,_Vars1,_Vars2,false) :-
	writeln([false]).
**/
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
getvalue(Variable,Value,Vars) :-
        ((not(isvar(Variable)),isvalstrorundef(Value),Variable=Value)->true;
        (isvar(Variable),isvalstrorundef(Value),getvar(Variable,Value,Vars))).
putvalue(Variable,Value,Vars1,Vars2) :-
        ((not(isvar(Variable)),isvalstrorundef(Value),Variable=Value,Vars1=Vars2)->true;
        (isvar(Variable),isvalstrorundef(Value),updatevar(Variable,Value,Vars1,Vars2))),!. 
getvar(Variable,Value,Vars) :-
        member([Variable,Value],Vars),
	not(Value=empty).
getvar(undef,undef,_Vars) :-
	!.
getvar(Variable,empty,Vars) :-
        not(member([Variable,_Value],Vars))->true;
	member([Variable,empty],Vars).
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
	expression3(N),
	expression2(Ns).
expression3(N) :-
	isval(N),!.
expression3(N) :-
	string(N),!.
expression3(N) :-
	atom(N),!.

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

findresult3([],_Result,Result2,Result2).
findresult3(Arguments1,Result1,Result2,Result3) :-
	Arguments1=[Value|Arguments2],
	expressionnotatom(Value),
	append(Result2,[Value],Result4),
        findresult3(Arguments2,Result1,Result4,Result3).
findresult3(Arguments1,Result1,Result2,Result3) :-
        Arguments1=[Variable|Arguments2],
        isvar(Variable),
	member([Variable,Value],Result1),
        append(Result2,[Value],Result4),
        findresult3(Arguments2,Result1,Result4,Result3).

strip([],Result2,Result2).
strip(Arguments1,Result2,Result3) :-
	Arguments1=[[Variable,Value]|Arguments2],
        isvar(Variable),
        append(Result2,[Value],Result4),
        strip(Arguments2,Result4,Result3).
