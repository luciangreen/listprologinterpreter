%% la_strings.pl

:- dynamic html_api_maker_or_terminal/1.

use_module(library(pio)).
use_module(library(dcg/basics)).

open_s(File,Mode,Stream) :-
	atom_string(File1,File),
	open(File1,Mode,Stream),!.

string_atom(String,Atom) :-
	atom_string(Atom,String),!.

phrase_from_file_s(string(Output), String) :-
	atom_string(String1,String),
	phrase_from_file(string(Output), String1),!.
	
writeln0(Term) :-
	%term_to_atom(Term,Atom),
%append([Term],["\n"],Term1),	
	%append_retry_back_stack([text,Term1]),!.
	(html_api_maker_or_terminal(html)->
	(%term_to_atom(Term,Atom),
	writeln(Term%Atom%,[]
	)
	,format('<br>\n',[]));
	writeln(Term)),!.
	
write0(Term) :-
	%term_to_atom(Term,Atom),
	%append_retry_back_stack([text,Term])
	(html_api_maker_or_terminal(html)->
	(%term_to_atom(Term,Atom),
	format(Term,[])%,format('\n',[])
	);
	write(Term)),!.

writeln1(Term) :-
	term_to_atom(Term,Atom),
	
%atom_concat(Atom,"\n",Atom1),
	%append_retry_back_stack([text,Atom1]),!.
	(html_api_maker_or_terminal(html)->
	(%term_to_atom(Term,Atom),
	format(Atom,[]),format('<br>\n',[]));
	writeln(Atom)),!.
	
	%writeln(Atom),!.
	
write1(Term) :-
/*
	term_to_atom(Term,Atom),
	append_retry_back_stack([text,Atom]),!.
	%write(Atom),!.
	*/
	term_to_atom(Term,Atom),
	
%atom_concat(Atom,"\n",Atom1),
	%append_retry_back_stack([text,Atom1]),!.
	(html_api_maker_or_terminal(html)->
	(%term_to_atom(Term,Atom),
	format(Atom,[])%,format('\n',[])
	);
	write(Atom)),!.
	
shell1_s(Command) :-
 	atom_string(Command1,Command),
	shell1(Command1),!.
	
shell1(Command) :-
				(bash_command(Command,_)->
					true;
					(writeln0(["Failed shell1 command: ",Command]),abort)
				),!.

bash_command(Command, Output) :-
        setup_call_cleanup(process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out)).

foldr(Function,A,L,B) :-
	reverse(A,C),
	foldl(Function,C,L,B),!.

foldr(append,A,B) :-
	foldr(append,A,[],B).
	
foldr(string_concat,A,B) :-
	foldr(string_concat,A,"",B).

concat_list([],""):-!.
concat_list(A1,B):-
	%A1=[A|List],
	concat_list("",A1,B),!.

concat_list(A,List,B) :-
	concat_list1(A,List,B).
	%string_codes(B,B1),!.

concat_list1(A,[],A):-!.
concat_list1(A,List,B) :-
	List=[Item|Items],
	%string_codes(Item,Item1),
	string_concat(A,Item,C),
	concat_list1(C,Items,B).

atom_concat_list([],''):-!.
atom_concat_list(A1,B):-
	%A1=[A|List],
	atom_concat_list([],A1,B),!.

atom_concat_list(A,List,B) :-
	atom_concat_list1(A,List,B1),
	atom_codes(B,B1),!.

atom_concat_list1(A,[],A):-!.
atom_concat_list1(A,List,B) :-
	List=[Item|Items],
	atom_codes(Item,Item1),
	append(A,Item1,C),
	atom_concat_list1(C,Items,B).

append_list(A1,B):-
	%A1=[A|List],
	append_list([],A1,B),!.

append_list(A,[],A):-!.
append_list(A,List,B) :-
	List=[Item|Items],
	append(A,[Item],C),
	append_list(C,Items,B).

append_list2([],[]):-!.
append_list2(A1,B):-
	%A1=[A|List],
	append_list2([],A1,B),!.

append_list2(A,[],A):-!.
append_list2(A,List,B) :-
	List=[Item|Items],
	append(A,Item,C),
	append_list2(C,Items,B).

list1(A,_,_) :-
	(A=[_|_]->true;A=[]),!.
	
	% splits after !,?,.

/*	
split_string17(String1,List) :-
	%string_codes(String2,String1),
	test(17,_,Code,_),
	%trace,
	%writeln1([interpret(off,[[n,grammar1],[String1,Chars,[v,t]]],
	%	Code,A)]),
	interpret(off,[[n,grammar1],[String1,[v,t]]],
		Code,[[[[v,t],List]]]),!.
*/

% split_string1(Docs,["`"],Input1) - splits and deletes on chars

split_string1(String1,Chars,List) :-
	%string_codes(String2,String1),
	test(116,_,Code,_),
	%trace,
	%writeln1([interpret(off,[[n,grammar1],[String1,Chars,[v,t]]],
	%	Code,A)]),
	interpret(off,[[n,grammar1],[String1,Chars,[v,t]]],
		Code,[[[[v,t],List]]]),!.

/**
?- split_string2("a   a",[" "],A).
A = ["a", " ", " ", " ", "a"].
**/

split_string2(String1,Chars,List) :-
	%string_codes(String2,String1),
	test(117,_,Code,_),
	%trace,
	%writeln1([interpret(off,[[n,grammar1],[String1,Chars,[v,t]]],
	%	Code,A)]),
	interpret(off,[[n,grammar1],[String1,Chars,[v,t]]],
		Code,[[[[v,t],List]]]),!.

% ?- join_chars_after(["d","e","a","c","f","b","g"],["a","b"],[],L).
% L = ["d", "ea", "c", "fb", "g"].

join_chars_after([],_Chars,List,List) :- !.
join_chars_after([List1],_Chars,List2,List3) :-
	append(List2,[List1],List3),!.
join_chars_after(List1,Chars,List5,List2) :-
	List1=[Char1,Char2|List3],
	member(Char2,Chars),
	string_concat(Char1,Char2,Char3),
	append([Char3],List3,List4),
	join_chars_after(List4,Chars,List5,List2),!.
join_chars_after(List1,Chars,List5,List2) :-
	List1=[Char1,Char2|List3],
	not(member(Char2,Chars)),
	append([Char2],List3,List4),
	append(List5,[Char1],List6),
	join_chars_after(List4,Chars,List6,List2),!.

% split_on_substring117(`AAABAAD`,`BD`,[],A). or
% ?- split_on_substring117([65,65,65,66,65,65,68],[66,68],[],A).
% A = ["AAA", "B", "AA", "D"].

split_on_substring117([],_A,E,E) :- !.
split_on_substring117(A,B2,E,C) :-
    intersection(A,B2,[]),
    string_codes(E1,E),
    string_codes(A1,A),
    concat_list([E1,A1],C2),
    append_list([C2],C),
    !.
split_on_substring117(A,B2,E,C) :-
    member(B,B2),
    append([B],D,A),
    %trace,
    split_on_substring117(D,B2,[],C1),
    string_codes(E1,E),
    string_codes(B1,[B]),
    %trace,
    (E1=""->maplist(append,[[[B1],C1]],[C]);
    (%trace,
    maplist(append,[[[E1,B1],C1]],[C]))),
    !.
split_on_substring117(A,B,E1,C) :-
    length(E,1),
    append(E,D,A),
    append(E1,E,E2),
    split_on_substring117(D,B,E2,C),!.

num_chars(Char,Num1,String) :-
	numbers(Num1,1,[],Nums),
	findall(Char,(member(_Num2,Nums)),Chars),
	concat_list(Chars,String),!.
	
/*
replace_new('0ab0','a','c',A).
A = "0cb0".
*/

replace_new(A1,Find,Replace,F) :-
string_length(Replace,Replace_l),
string_concat("%",A1,A2),
string_concat(A2,"%",A),		split_string(A,Find,Find,B),findall([C,Replace],(member(C,B)),D),maplist(append,[D],[E]),concat_list(E,F1),string_concat(F2,G,F1),string_length(G,Replace_l),
	string_concat("%",F3,F2),	
	string_concat(F,"%",F3),!.