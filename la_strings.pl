%% la_strings.pl

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
	
writeln1(Term) :-
	term_to_atom(Term,Atom),
	writeln(Atom),!.
	
write1(Term) :-
	term_to_atom(Term,Atom),
	write(Atom),!.
	
shell1_s(Command) :-
 	atom_string(Command1,Command),
	shell1(Command1),!.
	
shell1(Command) :-
				(bash_command(Command,_)->
					true;
					(writeln(["Failed shell1 command: ",Command]),abort)
				),!.

bash_command(Command, Output) :-
        setup_call_cleanup(process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out)).

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
	
% split_string1(Docs,["`"],Input1)

split_string1(String1,Chars,List) :-
	%string_codes(String2,String1),
	test(116,_,Code,_),
	%trace,
	%writeln1([interpret(off,[[n,grammar1],[String1,Chars,[v,t]]],
	%	Code,A)]),
	interpret(off,[[n,grammar1],[String1,Chars,[v,t]]],
		Code,[[[[v,t],List]]]),!.
