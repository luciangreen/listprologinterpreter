%% simplify_types.pl

/**

?- simplify_types([[[t, brackets], [[t, number]]], [t, string], [t, number]],[],T).
T = [[[t, number]], [t, string], [t, number]].

**/


simplify_types([],Types,Types) :- !.
% Simplified: Handle basic types in one clause
simplify_types(Data,Types1,Types2) :-
	get_lang_word("t",T),
	Data=[T,TypeName],
	member(TypeName, ["number", "string", "atom"]),
	append(Types1,[Data],Types2),!.
% Special case for pipe operator
simplify_types("|",Types1,Types2) :-
	append(Types1,["|"],Types2),!.
simplify_types(Data1,Types1,Types2) :-
get_lang_word("t",T),
get_lang_word("brackets",Dbw_brackets),
	Data1=[[[T,Dbw_brackets],Types4]|Types6],
	simplify_types(Types4,[],Data2),
	Types5=[Data2],
	append(Types1,Types5,Types2a),
	
simplify_types(Types6,Types2a,Types2),!.
	

simplify_types(Data1,Types1,Types2) :-
get_lang_word("t",T),
get_lang_word("list",Dbw_list),
	Data1=[[[T,Dbw_list],Types4]|Types6],
%trace,
	simplify_types(Types4,[],Data2),%[Data2|Data2a]),

(Data2=[Data2a]->Types5=[{Data2a}];
(Data2=[Data2a|Data2b]->	
(square_to_round([Data2a|Data2b],Types52),
round_to_curly(Types52,Types51),
Types5=[Types51]);false)),
	append(Types1,Types5,Types2a),
	simplify_types(Types6,Types2a,Types2),!.
	



% Generic type handling
simplify_types(Data,Types1,Types2) :-
	get_lang_word("t",T),
	Data=[T,A],
	append(Types1,[Data],Types2),!.


simplify_types(Data1,Types1,Types2) :-
	Data1=[Data2|Data3],
	simplify_types(Data2,Types1,Types3),
	simplify_types(Data3,Types3,Types2),!.
	%%Types2=[Types4].

