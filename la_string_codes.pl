% la_string_codes.pl

prepare_string_c(S,O) :-
	string_codes(S,O).

codes_to_string_c(C,S) :-
	string_codes(S,C).

/**
read_string_c(Stream, Chars1, Chars2, End, Result) :-
	read_string(Stream, Chars1, Chars2, End, Result1),
	prepare_string(Result1,Result).
**/

write_to_stream_c(String,I,O) :-	
	prepare_string_c(String,O2),
	append(I,O2,O).
	
