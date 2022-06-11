%% data_to_types.pl

/**

?- data_to_types([[1],"a",1],[],T).
T = [[[t, brackets], [[t, number]]], [t, string], [t, number]].

?- data_to_types([1,["a"],1],[],T).
T = [[t, number], [[t, brackets], [[t, string]]], [t, number]].

?- data_to_types([1,"a",[1]],[],T).
T = [[t, number], [t, string], [[t, brackets], [[t, number]]]].

**/

data_to_types([],Types,Types) :- !.
data_to_types(Data,Types1,Types2) :-
get_lang_word("t",T),
get_lang_word("number",Dbw_number),
	number(Data),append(Types1,[[T,Dbw_number]],Types2),!.
data_to_types(Data,Types1,Types2) :-
get_lang_word("t",T),
get_lang_word("string",Dbw_string),
	string(Data),append(Types1,[[T,Dbw_string]],Types2),!.
data_to_types(Data1,Types1,Types2) :-
get_lang_word("t",T),
get_lang_word("brackets",Dbw_brackets),
	Data1=[[Data2]],
	data_to_types(Data2,[],Types4),
	Types5=[[[T,Dbw_brackets],Types4]],
	append_list([Types1,Types5],Types2),!.
data_to_types(Data1,Types1,Types2) :-
get_lang_word("t",T),
get_lang_word("brackets",Dbw_brackets),
	Data1=[[Data2]|Data4],
	data_to_types(Data2,[],Types4),
	Types5=[[[T,Dbw_brackets],Types4]],
	data_to_types(Data4,[],Types6),
	append_list([Types1,Types5,Types6],Types2),!.
data_to_types(Data1,Types1,Types2) :-
get_lang_word("t",T),
get_lang_word("brackets",Dbw_brackets),
	Data1=[[Data2|Data3]|Data4],
	data_to_types(Data2,[],Types3),
	data_to_types(Data3,Types3,Types4),
	Types5=[[[T,Dbw_brackets],Types4]],
	data_to_types(Data4,[],Types6),
	append_list([Types1,Types5,Types6],Types2),!.
data_to_types(Data1,Types1,Types2) :-
	Data1=[Data2|Data3],
	data_to_types(Data2,Types1,Types3),
	data_to_types(Data3,Types3,Types2),!.
	%%Types2=[Types4].

append_list(A1,B):-
	append_list(A1,[],B),!.

%/*

append_list([],A,A):-!.
append_list(List,A,B) :-
	List=[Item|Items],
	append(A,Item,C),
	append_list(Items,C,B).
%*/