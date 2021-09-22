e4_fa_getvalue_match(Variable1,Value1,Vars1) :-
%trace,
	e4_fa_getvalue_match_pipe2(Variable1,Value1,Vars1).
	%notrace.
	%(Value1=[empty|empty]->trace;true).
e4_fa_getvalue_match(Variable1,Value1,Vars1) :-
	e4_fa_getvalue_match1(Variable1,Value1,Vars1).
	
e4_fa_getvalue_match_pipe2(Variable1,Value1,Vars1) :-
not(Variable1="|"),
	single_item(Variable1),
	e4_fa_getvalue(Variable1,Value1,Vars1),!.
e4_fa_getvalue_match_pipe2([],[],_Vars1) :- !.
e4_fa_getvalue_match_pipe2(Variable1,Value1,Vars1) :-
%not(member("|",Variable1)),
	not(single_item(Variable1)),
	Variable1=[Variable1a|Variable1b],
	((Variable1a="|"->
	(%trace,
	e4_fa_getvalue_match_pipe2(Variable1b,Value1b,Vars1),
	Value1b=[Value1]));
	(e4_fa_getvalue_match_pipe2(Variable1a,Value1a,Vars1),
	e4_fa_getvalue_match_pipe2(Variable1b,Value1b,Vars1),
	(Value1b=empty->Value1=[Value1a];
	append([Value1a],Value1b,Value1)))),!.

e4_fa_getvalue_match1(Variable1,Value1,Vars1) :-
not(Variable1="|"),
	single_item(Variable1),
	e4_fa_getvalue(Variable1,Value1,Vars1),!.
e4_fa_getvalue_match([],[],_Vars1) :- !.
e4_fa_getvalue_match(Variable1,Value1,Vars1) :-
not(member("|",Variable1)),
	not(single_item(Variable1)),
	Variable1=[Variable1a|Variable1b],
	e4_fa_getvalue_match(Variable1a,Value1a,Vars1),
	e4_fa_getvalue_match(Variable1b,Value1b,Vars1),
	append([Value1a],Value1b,Value1),!.
	
	/*
e4_fa_getvalue_match_pipe([],[],_Vars1) :- !.
e4_fa_getvalue_match_pipe(Variable1,Value1,Vars1) :-
	variable_name(Variable1),
	e4_fa_getvalue(Variable1,Value1,Vars1),
	not(Value1=empty),!.


e4_fa_getvalue_match_pipe(Variable1,Value1,Vars1) :- %%,Top_flag

	(variable_name(Variable1)->
	(e4_fa_getvalue(Variable1,Value1,Vars1),
	not(Value1=empty))),

	e4_fa_split_into_head_and_tail(Variable1,Head1a,Tail1a,Pipe1,Head_is_list_of_lists1),
	(single_item(Head1a) -> L1 = 1 ; (is_list(Head1a),length(Head1a,L1))),
	(%%trace,
	(Head_is_list_of_lists1=true)->(
	%%writeln(here1),
		Head1=Head1a,Tail1=Tail1a,
		%%notrace,
		%%trace,
	e4_fa_getvalue_match_pipe(Head1,Value11a,Vars1),
	e4_fa_getvalue_match_pipe(Tail1,Value11b,Vars1),
	not(Value11a=empty),(Value11b=empty->fail;Value11c=Value11b),
	is_list(Value11c),
	append(Value11a,Value11c,Value1)
	%%[Value3]=Value5,Value4=[Value6|Value6a],
	%%maplist(append,[[Value5,Value6,Value6a]],Value2)
	%%,notrace
	);

	((Pipe1=true)->
		(e4_fa_split_by_number_of_items(Variable1,L1,_Head2,_Tail2),
		Head1=Head1a,Tail1=Tail1a);
	%%((Pipe1=false,Pipe2=true)->
	%%	(split_by_number_of_items(Variable1,L2,Head1,Tail1),
	%%	Head2=Head2a,Tail2=Tail2a);
	(Pipe1=false)->%%,Pipe2=false,L1=L2,
		Head1=Head1a,Tail1=Tail1a),
		%%Head2=Head2a,Tail2=Tail2a))
	 % *1
	%%trace,
	%%writeln(here2),
	e4_fa_getvalue_match_pipe(Head1,Value12a,Vars1),
	e4_fa_getvalue_match_pipe(Tail1,Value12b,Vars1),
	not(Value12a=empty),(Value12b=empty->fail;Value12c=Value12b),
	is_list(Value12c),
	%%trace,
	append([Value12a],Value12c,Value1)),!.

e4_fa_getvalue_match_pipe([Variable1|Variable1b],Value1,Vars1) :-
%%variable_name(Variable1),
	(variable_name(Variable1)->
	(e4_fa_getvalue(Variable1,Value1,Vars1),
	not(Value1=empty))),
	
	(variable_name(Variable1b)->
	(e4_fa_getvalue(Variable1b,Value1b,Vars1),
	not(Value1b=empty))),

	e4_fa_getvalue_match_pipe(Variable1,Value11a,Vars1),
	e4_fa_getvalue_match_pipe(Variable1b,Value11b,Vars1),
	not(Value11a=empty),(Value11b=empty->Value11c=[];Value11c=Value11b),
	is_list(Value11c),
	append([Value11a],Value11c,Value1),!.

*/

e4_fa_match4_2(Variable1,Variable2,Vars1,Vars2) :-
	e4_fa_match4_10(Variable1,Variable2,Vars1,Vars2),!.
e4_fa_match4_2(Variable1,Variable2,Vars1,Vars2) :-
	e4_fa_match4(Variable1,Variable2,Vars1,Vars2),!.


e4_fa_match4_10(Variable1,Variable2,Vars1,Vars2) :-
	%%interpretpart(match4,Variable1,[v,sys2],Vars1,Vars3,_),
%%	getvalue([v,sys2],Value1,Vars3))),

	not(variable_name(Variable2)),
	is_list(Variable2),
	%%findall(Value2,(member(A,Variable2),getvalue(A,Value2,Vars1)),X),
	e4_fa_getvalue_match(Variable2,X,Vars1),
	
%%trace,
	e4_fa_match4(Variable1,X,Vars1,Vars2),!.
	
e4_fa_match4_10(Variable1,Variable2,Vars1,Vars2) :-
%%trace,
	not(variable_name(Variable1)),
	is_list(Variable1),
	%%findall(Value1,(
	
	%%interpretpart(match4,Variable1,[v,sys1],Vars1,Vars3,_),
	%%getvalue([v,sys1],Value1,Vars3)
	
	e4_fa_getvalue_match(Variable1,X,Vars1),

	e4_fa_match4(X,Variable2,Vars1,Vars2),!.

	

e4_fa_match4(Variable1,Variable2,Vars1,Vars2) :-
%%trace,
	variable_name(Variable2),
	e4_fa_getvalue(Variable2,Value2,Vars1),
	not(variable_name(Variable1)),
	is_list(Variable1),
	%%findall(Value1,(
	
	%%interpretpart(match4,Variable1,[v,sys1],Vars1,Vars3,_),
	%%getvalue([v,sys1],Value1,Vars3)
	
	e4_fa_getvalue_match(Variable1,X,Vars1),
	%%member(A,Variable1),getvalue(A,Value1,Vars1)
	%%),X),
	e4_fa_val1emptyorvalsequal(Value2,X),
	putvalue(Variable2,X,Vars1,Vars2),
	length(Variable1,L),length(X,L),!.
e4_fa_match4(Variable1,Variable2,Vars1,Vars2) :-
	variable_name(Variable1),
	e4_fa_getvalue(Variable1,Value1,Vars1),
	not(variable_name(Variable2)),
	is_list(Variable2),
	%%findall(Value2,(member(A,Variable2),getvalue(A,Value2,Vars1)),X),
	e4_fa_getvalue_match(Variable2,X,Vars1),
	e4_fa_val1emptyorvalsequal(Value1,X),
	putvalue(Variable1,X,Vars1,Vars2),
	length(Variable2,L),length(X,L),!.


e4_fa_match4(Variable1,Variable2,Vars1,Vars2) :-
%%trace,
	e4_fa_match4_list(Variable1,Variable2,Vars1,Vars2),!.
e4_fa_match4(Variable1,Variable2,Vars1,Vars2%%,Top_flag
) :-
	(((Variable1=[],Variable2=[[]])->true;(Variable1=[[]],Variable2=[]))->(%trace,
	fail);true),
	e4_fa_split_into_head_and_tail(Variable1,Head1a,Tail1a,Pipe1,Head_is_list_of_lists1),
	(single_item(Head1a) -> L1 = 1 ; (is_list(Head1a),length(Head1a,L1))),
	e4_fa_split_into_head_and_tail(Variable2,Head2a,Tail2a,Pipe2,Head_is_list_of_lists2),
	(single_item(Head2a) -> L2 = 1 ; (is_list(Head2a),length(Head2a,L2))),
	(%%trace,
	(Head_is_list_of_lists1=true->true;Head_is_list_of_lists2=true)->(
	%%writeln(here1),
		Head1=Head1a,Tail1=Tail1a,
		Head2=Head2a,Tail2=Tail2a,%%notrace,
		%%trace,
	e4_fa_match4_list(Head1,Head2,Vars1,Vars3),
	e4_fa_match4(Tail1,Tail2,Vars3,Vars2)
	%%[Value3]=Value5,Value4=[Value6|Value6a],
	%%maplist(append,[[Value5,Value6,Value6a]],Value2)
	%%,notrace
	);

	((Pipe1=true,Pipe2=false)->
		(e4_fa_split_by_number_of_items(Variable2,L1,Head2,Tail2),
		Head1=Head1a,Tail1=Tail1a);
	((Pipe1=false,Pipe2=true)->
		(e4_fa_split_by_number_of_items(Variable1,L2,Head1,Tail1),
		Head2=Head2a,Tail2=Tail2a);
	(Pipe1=false,Pipe2=false,L1=L2,
		Head1=Head1a,Tail1=Tail1a),
		Head2=Head2a,Tail2=Tail2a))
	, % *1
	%%trace,
	%%writeln(here2),
	e4_fa_match4_list(Head1,Head2,Vars1,Vars3),
	e4_fa_match4(Tail1,Tail2,Vars3,Vars2)
	%%,notrace
	%%(Top_flag=true->(trace,[Value3]=Value5);Value3=Value5),(Value4=[]->(Value6=[],Val6a=[]);[Value6|Val6a]=Value4),
	
	%%maplist(append,[[Value1,Value5]],[Value2a]),
	%%(Top_flag=true->(append(Value2a,Value6,Value61),
	%%append(Value61,Val6a,Value2));maplist(append,[[Value2a,Value6,Val6a]],[Value2]))%%,
	%%append(Value2a,Value4,Value2)
	)
	%%,notrace
	,!.

e4_fa_split_into_head_and_tail(Variable,Head1c,Tail1c,Pipe,Head_is_list_of_lists) :-
%writeln1(split_into_head_and_tail(Variable,Head1c,Tail1c,Pipe,Head_is_list_of_lists)),

not(variable_name(Variable)),
	(findall(_FA,member("|",Variable),FA2),length(FA2,FA3),FA3=<1),
	%%Variable=[[v, a], "|", [v, d]]->trace,%%((
	(((
	append(Head2,["|"|Tail2],Variable) %%-> notrace;notrace)
	),
	(is_list(Head2),e4_fa_head_is_list_of_lists(Head2,Head_is_list_of_lists),(length(Head2,1) -> Head2=[Head1] ; 
		Head2=Head1)),%%trace,
		Tail2=[Tail1],Pipe=true)->true;
	%%(
	((is_list(Variable),not(variable_name(Variable)),
	Variable=[Head1|Tail1],Pipe=false,e4_fa_head_is_list_of_lists(Head1,Head_is_list_of_lists))->true;
	(Head1=Variable,Tail1=[],Pipe=false,e4_fa_head_is_list_of_lists(Head1,Head_is_list_of_lists)))),
	(Head1=empty->Head1c=[];Head1=Head1c),
	(Tail1=empty->Tail1c=[];Tail1=Tail1c),!.
	%%(.%%->true;
	%%([Head]=Variable,Tail=[]))).
	
%% [a,b|c]=[A|B].
%%A = a,
%%B = [b|c].
%% not supported, in future.

%% *1 only accept multiple items in head when other is |-less list, otherwise same number of items in head
%% ?- [1,2,3]=[A,B|C].
%% A = 1,
%% B = 2,
%% C = [3].

%% what about - v
/**
?- [[A],C|B]=[[1],2,3,4].
A = 1,
C = 2,
B = [3, 4].
**/

%% need to detect if head is a compound, flag and process it
e4_fa_head_is_list_of_lists(Head2,true) :-
get_lang_word("v",Dbw_v),
	%%trace,
	([Head3]=Head2->true;(%%notrace,
	fail)),findall(A,
	(member(A,Head3),((A=[v,_] -> true; ((A=[Dbw_v,_] -> true;is_list(A)))))),B),
	%%trace,writeln(Head2).%%
	is_list(Head3),length(Head3,L),length(B,L),!.
e4_fa_head_is_list_of_lists(_,false) :- !.


e4_fa_split_by_number_of_items(List,N2,List10,List2) :-
	%%N2 is N1-1,
	length(List1,N2),
	append(List1,List2,List),
	(List1=[_] -> List1=[List10] ; List1=List10),!.
	
e4_fa_match4_list([],[],Vars,Vars) :- !.

e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
	not(variable_name(Head1)),
	not(variable_name(Head2)),
	Head1=[Head1a|Head1b],
	Head2=[Head2a|Head2b],
	not(Head1a="|"),
	not(Head2a="|"),
	e4_fa_match4(Head1a,Head2a,Vars1,Vars3%%,false
	),
	e4_fa_match4_list(Head1b,Head2b,Vars3,Vars2),!.
e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
%%trace,
	%%single_item(Head1),
	%%single_item(Head2),
	e4_fa_match4_terminal(Head1,Head2,Vars1,Vars2),!.%%,
	%%append(Value1,[Value3],Value2).


e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
	variable_name(Head1),
	not(variable_name(Head2)),
	not(Head2="|"),
	e4_fa_getvalue(Head1,Value1,Vars1),not(Value1=empty),
	e4_fa_match4(Value1,Head2,Vars1,Vars2),!.
e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
	not(variable_name(Head1)),
	not(Head1="|"),
	variable_name(Head2),
	e4_fa_getvalue(Head2,Value2,Vars1),not(Value2=empty),
	e4_fa_match4(Head1,Value2,Vars1,Vars2),!.
e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
	variable_name(Head1),
	variable_name(Head2),
	e4_fa_getvalue(Head1,Value1,Vars1),not(Value1=empty),
	e4_fa_getvalue(Head2,Value2,Vars1),not(Value2=empty),
	e4_fa_match4(Value1,Value2,Vars1,Vars2),!.
	
e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
	Head1=["|"|[Head1a]],
	Head2=["|"|[Head2a]],
	e4_fa_match4(Head1a,Head2a,Vars1,Vars2),!.
e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
	Head1=["|"|[Head1a]],
	not(Head2=["|"|_]),
	e4_fa_match4(Head1a,Head2,Vars1,Vars2),!.
e4_fa_match4_list(Head1,Head2,Vars1,Vars2) :-
	Head2=["|"|[Head2a]],
	not(Head1=["|"|_]),
	e4_fa_match4(Head1,Head2a,Vars1,Vars2),!.

e4_fa_match4_terminal([],[],Vars,Vars) :- !.
e4_fa_match4_terminal(Variable1,Variable2,Vars1,Vars2) :-
	%%is_list(Variable1),length(Variable1,1),
	%%is_list(Variable2),length(Variable2,1),
	%%trace,
	[Variable1a]=Variable1,
	[Variable2a]=Variable2,
	single_item(Variable1a),
	single_item(Variable2a),
	%%notrace,
e4_fa_match4_terminal(Variable1a,Variable2a,Vars1,Vars2),!.%%,
	%%append(Value1,[[Value3]],Value2).

e4_fa_match4_terminal(Variable1,Variable2,Vars1,Vars2) :-%%trace,
	%%single_item(Variable1), %% may be [1,2]
	%%single_item(Variable2),
   e4_fa_getvalues(Variable1,Variable2,Value1,Value2,Vars1),
   %% what if there is a var in a compound term? - may need different code in getvalues
        ((Value1A = Value2,
        e4_fa_val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2)%%bracket_if_single(Value1A,Value1A2),
        %%append11(Value1a,[Value1A],Value2a)
        )->true;
        ((Value2A = Value1,
        e4_fa_val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2)%%,%%bracket_if_single(Value2A,Value2A2),
        %%append11(Value1a,[Value2A],Value2a)
        )->true;
	     fail
	     %% assumes either or both A and B in A=B are instantiated, 
	     %% can be changed later.
	     )),!.