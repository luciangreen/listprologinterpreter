cut_cps_lpi(Pred_id,Predicate_number,Globals3) :-



%trace,
%pred_numbers(Pred_nums),
%trace,
%writeln1(cut_cps),
%writeln1(cut_cps(Choice_point_trail1a,Choice_point_trail2,CP_Vars1a,CP_Vars2,Pred_id,Predicate_number,Globals3)),
		% collect pred ids connected by curr pred num
		
		
% clear empty Cps

%clear_cps(Choice_point_trail1a,Choice_point_trail1,CP_Vars1a,CP_Vars1),
% (possibly not necessarily) collect connected pred ids
		
findall(Pred_ids,collect_connected_pred_ids(Pred_id,[Pred_id
],Pred_ids,Predicate_number,Globals3,_Pred_nums),Pred_ids1a),

flatten(Pred_ids1a,Pred_ids1b),
sort(Pred_ids1b,Pred_ids2a),
subtract(Pred_ids2a,[%Pred_id
],Pred_ids2),

cut_preds(Cut_preds),

append(Cut_preds,Pred_ids2,Cut_preds2),
sort(Cut_preds2,Cut_preds3),

retractall(cut_preds(_)),
assertz(cut_preds(Cut_preds3)).