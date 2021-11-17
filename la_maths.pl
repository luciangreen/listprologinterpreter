% la_maths.pl

%% numbers(5,1,[],N).
%% N=[1,2,3,4,5]

numbers(N2,N1,Numbers2):-
	(N2>N1->
	numbers(N2,N1,[],Numbers2);
	numbers(N1,N2,[],Numbers2)).

numbers(N2,N1,Numbers1,Numbers2):-
numbers1(N2,N1,Numbers1,Numbers2),!.
numbers1(N2,N1,Numbers,Numbers) :-
	N2 is N1-1,!.
numbers1(N2,N1,Numbers1,Numbers2) :-
	N3 is N1+1,
	append(Numbers1,[N1],Numbers3),
	numbers1(N2,N3,Numbers3,Numbers2),!.

%% get_item_n([1,2,3],3,Item).
%% Item = 3

get_item_n([],_,[]) :-!.
get_item_n(Exposition,Number1,Item) :-
	Number2 is Number1-1,
	length(List,Number2),
	append(List,[Item|_],Exposition),!.

put_item_n(Exposition,Number1,Item2,Exposition2) :-
	Number2 is Number1-1,
	length(List,Number2),
	append(List,[_Item1|Rest],Exposition),
	append(List,[Item2|Rest],Exposition2),!.

% get_n_item([4,6,7],6,L).
% L = 2

get_n_item(A,C,L2) :-
	append(B,[C|_],A),length(B,L),L2 is L+1,!.

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