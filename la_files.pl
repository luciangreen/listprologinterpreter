delete_invisibles_etc(F,G) :-
	findall(J,(member(H,F),atom_string(H,J),not(J="."),not(J=".."),not(string_concat(".",_,J))),G),!.

open_file_s(Path,File_term) :-
		phrase_from_file_s(string(File), Path),
		string_codes(File_string,File),
		term_to_atom(File_term,File_string),!.

save_file_s(Path,Content_term_or_string) :-
	((compound(Content_term_or_string)->true;Content_term_or_string=[])->term_to_atom(Content_term_or_string,String);(string(Content_term_or_string)->Content_term_or_string=String;
	(concat_list(["Error: save_file_s content not in compound or string format."],Notification),
	writeln0(Notification),abort))),
	(open_s(Path,write,Stream),
	write(Stream,String),
	close(Stream)),
	!.