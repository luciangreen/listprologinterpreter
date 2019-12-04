%% data_to_types.pl

/**

?- data_to_types([1,"a",[1]],[],T).
T = [[t, number], [t, string], [[t, brackets], [[[t, number]]]]].

?- data_to_types([1,["a"],1],[],T).
T = [[t, number], [[t, brackets], [[[t, string]]]], [[t, number]]].

?- data_to_types([[1],"a",1],[],T).
T = [[[t, brackets], [[[t, number]]]], [[t, string], [t, number]]].

**/

data_to_types([],Types,Types) :- !.
data_to_types(Data,Types1,Types2) :-
	number(Data),append(Types1,[[t,number]],Types2),!.
data_to_types(Data,Types1,Types2) :-
	string(Data),append(Types1,[[t,string]],Types2),!.
data_to_types(Data1,Types1,Types2) :-
	Data1=[[Data2]],
	data_to_types(Data2,[],Types4),
	Types5=[[t,brackets],[Types4]],
	append_list([Types1,Types5],Types2),!.
data_to_types(Data1,Types1,Types2) :-
	Data1=[[Data2]|Data4],
	data_to_types(Data2,[],Types4),
	Types5=[[t,brackets],[Types4]],
	data_to_types(Data4,[],Types6),
	append_list([Types1,Types5,Types6],Types2),!.
data_to_types(Data1,Types1,Types2) :-
	Data1=[[Data2|Data3]|Data4],
	data_to_types(Data2,[],Types3),
	data_to_types(Data3,Types3,Types4),
	Types5=[[t,brackets],[Types4]],
	data_to_types(Data4,[],Types6),
	append_list([Types1,Types5,Types6],Types2),!.
data_to_types(Data1,Types1,Types2) :-
	Data1=[Data2|Data3],
	data_to_types(Data2,Types1,Types3),
	data_to_types(Data3,Types3,Types2),!.
	%%Types2=[Types4].

append_list(A1,B):-
	A1=[A|List],
	append_list(A,List,B),!.

append_list(A,[],A):-!.
append_list(A,List,B) :-
	List=[Item|Items],
	append(A,[Item],C),
	append_list(C,Items,B).
