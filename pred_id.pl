find_pred_id(N2) :-
	pred_id(N1),
	N2 is N1+1,
	retractall(pred_id(_)),
 	assertz(pred_id(N2)).

% used in ssi

collect_connected_pred_ids(Pred_id,Pred_ids1,Pred_ids2,Predicate_number,Globals3,Pred_nums) :-

collect_connected_pred_ids1(Pred_id,Pred_ids1,Pred_ids3,Predicate_number,Globals3,Pred_nums),

%writeln1([*,Pred_ids3]),

 % uncomment for heavier cut

findall(Pred_ids6,(member(Pred_id5,Pred_ids3),collect_connected_pred_ids2(Pred_id5,[Pred_id5],Pred_ids6,Globals3,Pred_nums)),Pred_ids2),

  % don't forget to comment

%Pred_ids3=Pred_ids2,

!.

collect_connected_pred_ids1(Pred_id,Pred_ids1,Pred_ids2,Predicate_number,Globals3,Pred_nums) :-

 % starting with the current pred_id, work backwards and forwards to identify pred ids to cut that are connected by predicate number
 ((member([pred_id_chain,Prev_pred_id,Pred_id],Globals3),
 not(member(Prev_pred_id,Pred_ids1)),
 member([[pred_num,Prev_pred_id],Predicate_number],Globals3)
 %/*
 ,member([_Pred_name,_Arity,Pred_nums1],Pred_nums),
 member(Predicate_number1,Pred_nums1),
 member(Predicate_number,Pred_nums1),
 member([[pred_num,Prev_pred_id2],Predicate_number],Globals3)
 %*/
 )-> % last line added, ppi->ppi2 below x
 (%trace,
 append(Pred_ids1,[Prev_pred_id2],Pred_ids3),

  
 %findall(Pred_ids4, collect_connected_pred_ids2(Prev_pred_id,Pred_ids3,Pred_ids4,Globals3),Pred_ids5),

 Pred_ids3=Pred_ids5,
 collect_connected_pred_ids1(Prev_pred_id,Pred_ids5,Pred_ids2,Predicate_number,Globals3,Pred_nums));
 Pred_ids1=Pred_ids2).
 
 
%collect_connected_pred_ids2(Pred_id,Pred_ids1,Pred_ids1,_Globals3) :-

 %not(member([pred_id_chain,Pred_id,Next_pred_id],Globals3)).
/*
collect_connected_pred_ids2(Pred_id,Pred_ids1,Pred_ids2,Globals3) :-

 (findall([%Next_pred_id,
 Next_pred_id1],(member([pred_id_chain,Pred_id,Next_pred_id],Globals3)),Next_pred_id1),
 member(Next_pred_id2,Next_pred_id),
 collect_connected_pred_ids2(Next_pred_id2,[Next_pred_id],Pred_ids2,Globals3)).%),Pred_ids2)).
 */
 
 collect_connected_pred_ids2(Pred_id,Pred_ids1,Pred_ids2,Globals3,Pred_nums) :-
 ((findall(Next_pred_id,member([pred_id_chain,Pred_id,Next_pred_id],Globals3),Next_pred_id),
 %member(Next_pred_id,Next_pred_id1),
 %not(member(Next_pred_id,Pred_ids1))
 subtract(Next_pred_id,Pred_ids1,Next_pred_id1)
 %member([[pred_num,Prev_pred_id],Predicate_number],Globals3)
 )->
 (%trace,
 append(Pred_ids1,Next_pred_id1,Pred_ids3),
 
 
 %findall(Pred_ids4, collect_connected_pred_ids2(Prev_pred_id,Pred_ids3,Pred_ids4,Globals3),Pred_ids5),
 
 Pred_ids3=Pred_ids5,
 findall(Pred_ids6,(member(Next_pred_id2,Next_pred_id1),collect_connected_pred_ids2(Next_pred_id2,Pred_ids5,Pred_ids6,Globals3,Pred_nums)),Pred_ids7),
 append([Pred_id,Pred_ids7],Next_pred_id1,Pred_ids2));
 Pred_ids1=Pred_ids2).
 
 
