delete_invisibles_etc(F,G) :-
	findall(J,(member(H,F),atom_string(H,J),not(J="."),not(J=".."),not(string_concat(".",_,J))),G),!.


open_file_s(Path,File_term) :-
		phrase_from_file_s(string(File), Path),
		string_codes(File_string,File),
		term_to_atom(File_term,File_string),!.


open_string_file_s(Path,File_string) :-
		phrase_from_file_s(string(File), Path),
		string_codes(File_string,File),!.
		%term_to_atom(File_term,File_string),!.


save_file_s(Path,Content_term_or_string) :-
	((compound(Content_term_or_string)->true;Content_term_or_string=[])->term_to_atom(Content_term_or_string,String);((string(Content_term_or_string)->true;atom(Content_term_or_string))->Content_term_or_string=String;
	(concat_list(["Error: save_file_s content not in compound, atom or string format."],Notification),
	writeln0(Notification),abort))),
	(open_s(Path,write,Stream),
	write(Stream,String),
	close(Stream)),
	!.
	
exists_file_s(F1) :-
 atom_string(F2,F1),
 exists_file(F2),!.

exists_directory_s(F1) :-
 atom_string(F2,F1),
 exists_directory(F2),!.

make_directory_s(F1) :-
 atom_string(F2,F1),
 make_directory(F2),!.

working_directory1(A1,B1) :-
 (string(A1)->atom_string(A,A1);A=A1),
 (string(B1)->atom_string(B,B1);B=B1),
 term_to_atom(working_directory(A,B),Atom),
 	catch(working_directory(A,B), _, (foldr(string_concat,["Error on ",Atom%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text41),writeln1(Text41)%fail%abort
 	)),!.
