convert_to_lp_pipe(empty2,empty2) :- !.
convert_to_lp_pipe([Dbw_v,A],[Dbw_v,A]) :- 
 get_lang_word("v",Dbw_v),
 !.
convert_to_lp_pipe(Value1A,Value1A) :-
 foldr(append,[Value1A],[],_).

convert_to_lp_pipe(Value1A,Value1A1) :-
 command_n_sols(N),
 numbers(N,1,[],N1),
 member(N2,N1),
 length(L,N2),
 append(L,A,Value1A),
 %length(A,1),
 %foldr(append,[A],[],_),
 %var(A),
 A=empty2,
 foldr(append,[L,["|"],[A]],[],Value1A1).
 
matrix([iii,
iio,
ioi,
ioo,
oii,
oio,
ooi,
ooo]).

matrix_member([ii,oi,io,oo]).
	
