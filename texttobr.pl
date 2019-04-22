%% use_module(library(pio)). In la_strings


%% texttobr - converts file stream to list of 3D dimensions of each character

texttobr(N1,Filex1,Stringx1,M1) :-

	((number(N1),N=N1)->true;
	(N1=u,N=1)),

	((Filex1=u,Filex="file.txt")->true;
	Filex=Filex1),

	((number(M1),M=M1)->true;
	M=all), %% If m1 is undefined or all then m=all

	texttobrc1(N,Filex,Stringx1,M),!.

texttobrc1(0,_,_,_) :- !.
texttobrc1(N1,Filex,Stringx1,BreasoningLimit) :-
	texttobrc2(Filex,Stringx1,BreasoningLimit),
	N2 is N1-1,
	texttobrc1(N2,Filex,Stringx1,BreasoningLimit).	

texttobrc2(Filex,Stringx1,M) :-

	((Stringx1=u,
	phrase_from_file_s(string2(String1), Filex))->true;
	string_codes(Stringx1,String1)),
	
	((number(M),length(String,M),
	append(String,_,String1))->true;
	String=String1),

	br(String),!.
	
string2(String) --> list2(String).
list2([]) --> [].
list2([L|Ls]) --> [L], list2(Ls).

br([]) :-
	!.
br([Code|Codes]) :-
	char_code(Character,Code),
	br(Character,_X,_Y,_Z),
	brth2(Character,_Brth),
	%%writeln([Character]),%%,X,Y,Z]), %%
	%%write(' '),
	br(Codes).
brth2(_,sweetinvincibleandprayedfor).
br('A',1,1.5,0).
br('B',1,1.5,0).
br('C',1,1.5,0).
br('D',1,1.5,0).
br('E',1,1.5,0).
br('F',1,1.5,0).
br('G',1,1.5,0).
br('H',1,1.5,0).
br('I',1,1.5,0).
br('J',1,1.5,0).
br('K',1,1.5,0).
br('L',1,1.5,0).
br('M',1,1.5,0).
br('N',1,1.5,0).
br('O',1,1.5,0).
br('P',1,1.5,0).
br('Q',1,1.5,0).
br('R',1,1.5,0).
br('S',1,1.5,0).
br('T',1,1.5,0).
br('U',1,1.5,0).
br('V',1,1.5,0).
br('W',1,1.5,0).
br('X',1,1.5,0).
br('Y',1,1.5,0).
br('Z',1,1.5,0).
br('a',1,1,0).
br('b',1,1.5,0).
br('c',1,1,0).
br('d',1,1.5,0).
br('e',1,1,0).
br('f',1,2.5,0).
br('g',1,2,0).
br('h',1,1.5,0).
br('i',1,1.5,0).
br('j',1,2.5,0).
br('k',1,1.5,0).
br('l',1,1.5,0).
br('m',1,1,0).
br('n',1,1,0).
br('o',1,1,0).
br('p',1,2,0).
br('q',1,2,0).
br('r',1,1,0).
br('s',1,1,0).
br('t',1,1.5,0).
br('u',1,1,0).
br('v',1,1,0).
br('w',1,1,0).
br('x',1,1,0).
br('y',1,2,0).
br('z',1,1,0).

br('?',1,1.5,0).
br('-',1,1,0).
br(' ',1,1,0).
br(',',1,1,0).
br('(',1,1.5,0).
br(')',1,1.5,0).
br('|',1,2.5,0).
br('.',1,1,0).
br(':',1,1,0).
br('_',1,1,0).
br('\'',1,1.5,0).
br('[',1,1.5,0).
br(']',1,1.5,0).
br('<',1,1,0).
br('>',1,1,0).

br('0',1,1.5,0).
br('1',1,1.5,0).
br('2',1,1.5,0).
br('3',1,1.5,0).
br('4',1,1.5,0).
br('5',1,1.5,0).
br('6',1,1.5,0).
br('7',1,1.5,0).
br('8',1,1.5,0).
br('9',1,1.5,0).

br('{',1,1.5,0).
br('}',1,1.5,0).
br('\n',1,1,0).
br(_,1,1,0).
