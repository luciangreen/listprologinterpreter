% la_maths.pl

%% numbers(5,1,[],N).
%% N=[1,2,3,4,5]

numbers(N2,N1,Numbers2) :-
	(N2>=N1->
	numbers(N2,N1,[],Numbers2);
	numbers(N1,N2,[],Numbers2)),!.

numbers(N2,N1,Numbers1,Numbers2):-
numbers1(N2,N1,Numbers1,Numbers2),!.
numbers1(N2,N1,Numbers,Numbers) :-
	N2 is N1-1,!.
numbers1(N2,N1,Numbers1,Numbers2) :-
	N3 is N1+1,
	append(Numbers1,[N1],Numbers3),
	numbers1(N2,N3,Numbers3,Numbers2),!.

%% get_item_n([a,b,c],3,Item).
%% Item = c

get_item_n(A,B,C) :-
	catch(get_item_n1(A,B,C),_,fail),!.

get_item_n1([],_,[]) :-!.%,fail.
get_item_n1(Exposition,Number1,Item) :-
	Number2 is Number1-1,
	length(List,Number2),
	append(List,[Item|_],Exposition),!.

put_item_n(Exposition,Number1,Item2,Exposition2) :-
	Number2 is Number1-1,
	length(List,Number2),
	append(List,[_Item1|Rest],Exposition),
	append(List,[Item2|Rest],Exposition2),!.

% put_item_ns([a,b,c,b],[[2,d],[4,e]],F).
% F = [a, d, c, e].

put_item_ns(A,[],A) :- !.
put_item_ns(A,BCs,D) :-
 BCs=[[B,C]|BCs2],
 put_item_n(A,B,C,E),
 put_item_ns(E,BCs2,D).


% get_n_item([4,6,7],6,L).
% L = 2

get_n_item(A,C,L2) :-
	append(B,[C|_],A),length(B,L),L2 is L+1.

% delete_item_n([4,5,6],2,D).
% D = [4,6]
delete_item_n(A,N,D) :-
	N1 is N-1,length(B,N1),append(B,[_|C],A),append(B,C,D),!.


sum(A,S) :-
	sum(A,0,S), !.
sum([],S,S):-!.
sum(S0,S1,S2) :-
	S0=[S3|S4],
	S5 is S1+S3,
	sum(S4,S5,S2),!.

% number_order(1000,M).
% M = 3

number_order(N1,M) :-
	log(N1,N),log(10,P),M is ceiling(N/P),!.

foldr(plus,A,B) :-
	foldr(plus,A,0,B).

maximum_length(List,Maximum_length) :-
	findall(L,(member(A2,List),length(A2,L)),L2),
	sort(L2,L3),
	append(_,[Maximum_length],L3),!.

sub_list(List,Before_list,Sub_list,After_list) :-
	append(Before_list,L1,List),
	append(Sub_list,After_list,L1).
	
sub_list_reverse(List,Before_list,Sub_list,After_list) :-
	reverse(List,L2),
	append(To_after_list,L1,L2),
	reverse(To_after_list,After_list),
	reverse(L1,L11),
	append(Before_list,Sub_list,L11).
	
all_distinct1(A) :- sort(A,B),msort(A,B).

split_list_on_item(A,Item,Lists) :-
	(is_list(Item)->Item1=Item;Item1=[Item]),
 	findall(BID,(((append(B,C,A),append(Item1,F,C),
 	split_list_on_item(F,Item1,D),delete([B,Item1],[],BI),%foldr(append,[B,Item1],BI),
 	foldr(append,[BI,D],BID))->true;(A=[]->BID=[];BID=[A]))),Lists1),foldr(append,Lists1,Lists).




% With apologies to swipl 8.3.17+

clumped1(Items, Counts) :-
    clump1(Items, Counts).

clump1([], []).
clump1([H|T0], [H-C|T]) :-
    ccount1(T0, H, T1, 1, C),
    clump1(T1, T).

ccount1([H|T0], E, T, C0, C) :-
    E == H,
    !,
    C1 is C0+1,
    ccount1(T0, E, T, C1, C).

ccount1(List, _, List, C, C).

repeat(N,C) :-
	numbers(N,1,[],Ns),findall(_,(member(_,Ns),C),_),!.
