/**
he should identify different writers in the exposition

- white, black, found white and found purple lists of names
- a list is e.g. [a,b,c]
- intersection, minus written by hand
**/

%% identifywriters([1,2,3],[10,11,12],[1,2,3,4,5,6,7,8,9,10],Found_white,Found_black,Found_purple).
%% Found_white = [1, 2, 3],
%% Found_black = [10],
%% Found_purple = [4, 5, 6, 7, 8, 9] ;

identifywriters(White,Black,Found,Found_white,Found_black,Found_purple) :-
	intersection1(Found,White,[],Found_white),
	intersection1(Found,Black,[],Found_black),
	minus(Found,Found_white,Found_purple1),
	minus(Found_purple1,Found_black,Found_purple).

%% intersection1([a,b,c],[c,d,e],[],A).
%% A = [c]

intersection1([], _, List, List).
intersection1(List1, List2, List3A, List3) :-
	List1=[Item1|List4],
	intersection2(Item1,List2,[],List5),
	append(List3A,List5,List6),
	intersection1(List4,List2,List6,List3).
intersection2(_, [], List, List).
intersection2(Item1, List1, List2, List3) :-
	List1=[Item1|List4],
	append(List2,[Item1],List5),
	intersection2(Item1, List4, List5, List3).
intersection2(Item1, List1, List2, List3) :-
	List1=[Item2|List4],
	not(Item1=Item2),
	intersection2(Item1, List4, List2, List3).
	
%% member1A(1,[1,2]).
%% true

member1A(Item,List) :-
	intersection2(Item,List,[],[_]).
	
%% minus([1,2,3,4],[1,2,4],A).
%% A = [3]

minus1A(List, [], List).
minus1A(List1, List2, List3) :-
	List2=[Item1|List4],
	delete(List1,Item1,List5),
	minus1A(List5, List4, List3).

%% minus1([1,2,3],[1,2],A).
%% A = [3]

minus1(List, [], List).
minus1(List1, List2, List3) :-
	List2=[Item1|List5],
	delete2(List1,Item1,[],List6),
	minus1(List6, List5, List3).
	
%% delete2([1,2],1,[],A).
%% A = [2]

delete2([], _, List, List).
delete2(List1,Item1,List2,List3) :-
	List1=[Item1|List5],
	delete2(List5,Item1,List2,List3).
delete2(List1,Item1,List2,List3) :-
	List1=[Item2|List5],
	not(Item1=Item2),
	append(List2,[Item2],List6),
	delete2(List5,Item1,List6,List3).
	
/**
- punctuation, space, return delimited x

Later
- connect with quotes from texts mentioned in bibliography

Don't
- count sentences
**/