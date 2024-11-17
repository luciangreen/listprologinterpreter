% if arg is different from call, write (length 1 - no square brackets)


store_starlog_fa(Skip,%Skip1,
FA) :- !.
/*
%trace,
	find_sl_n(N),
 sl_fa(Sl_fa),
 append(Sl_fa,[[N,FA]],Sl_fa1),
 retractall(sl_fa(_)),
 assertz(sl_fa(Sl_fa1)),
 (Skip=[Skip1,N]->true;
 Skip=Skip1),
 writeln([N,FA]),!.
*/

convert_to_starlog(_Skip,_TF,FunctionResult2,FunctionResult21) :-
(starlog(on)->(
%trace,

FunctionResult2=[F,A1],
((F=[n,append],length(A1,3))->
(A1=[A,B,C],(C=A&B)=FunctionResult21);

((F=[n,stringconcat],length(A1,3))->
(A1=[A,B,C],(C=A:B)=FunctionResult21);

((F=[n,atom_concat],length(A1,3))->
(A1=[A,B,C],(C=(A^B))=FunctionResult21);

FunctionResult2=FunctionResult21)))
);FunctionResult2=FunctionResult21),!.

/*
)
((Skip=[_Skip1,N]->
(
	sl_fa(Sl_fa),
	(member([N,FA],Sl_fa)->true;(writeln("No Starlog skip entry."),abort)),

%trace,
	FunctionResult2=[_|
	A1]
	,
	%trace,
	FA=[_|A2],
	(((flatten(A1,A11),flatten(A2,A21),A11=[],A21=[])->true;A1=A2)->
	% assume all defined, no outputs
	FunctionResult21=true%TF
	;
	((length(A1,L),length(A2,L)->true;(writeln(["Starlog args different lengths.",A1,A2]),abort)),
	numbers(L,1,[],Ns),
	findall(T2,(member(N,Ns),get_item_n(A1,N,T1),get_item_n(A2,N,T2),not(T1=T2)),T2s),
	(T2s=[T2s1]->true;T2s=T2s1),
	FunctionResult21=T2s1))
	);
	(%false,
	FunctionResult2=FunctionResult21))),!.
	
	
*/