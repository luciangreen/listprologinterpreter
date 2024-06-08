replace_term(A,F,R,B) :-
 sub_term_wa(F,A,In),
 findall([Ad,R],member([Ad,_],In),In2),
 foldr(put_sub_term_wa_ae,In2,A,B),!.