%% la_strings.pl

:- dynamic html_api_maker_or_terminal/1.

use_module(library(pio)).
use_module(library(dcg/basics)).

string_strings(S,S1) :-
	string_chars(S,S2),
	maplist(atom_string,S2,S1),!.

get_until_char(S,C,Cs1,Left1) :-
	string_strings(S,S1),
	append(Cs,B,S1),(is_list(C)->C1=C;	string_strings(C,C1)),
	append(C1,Left,B),
	foldr(string_concat,Cs,Cs1),
	foldr(string_concat,Left,Left1),
	!.
get_until_char(S,_C,S,"") :- !.

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

writeln_info(Colour,A1) :-
(is_list(A1)->term_to_atom(A1,A);A=A1), atomic_list_concat(C1,"\\n",A),atomic_list_concat(C1,"\n",B),split_string(B,"\n\r","\n\r",B1),findall(_,(member(C,B1),atom_string(C2,C),ansi_format([fg(Colour)], C2, []),nl),_),!.

writeln_info(A1) :-
(is_list(A1)->term_to_atom(A1,A);A=A1), atomic_list_concat(C1,"\\n",A),atomic_list_concat(C1,"\n",B),split_string(B,"\n\r","\n\r",B1),findall(_,(member(C,B1),atom_string(C2,C),print_message(information,C2)),_),!.

n_to_br(Term,Term1) :-
	sub_term_types_wa([string,atom],Term,Instances),
	findall([Add,X],(member([Add,X1],Instances),
	atomic_list_concat(A,'\n',X1),
	atomic_list_concat(A,'<br>',X2),	
	(atom(X1)
	->X2=X;(atom_string(X2,X4)),X4=X%,term_to_atom(X4,X)
	)),X3),
	foldr(put_sub_term_wa_ae,X3,Term,Term1),!.

writeln_br(Term) :-	
	(html_api_maker_or_terminal(html)->
	(n_to_br(Term,Term1),
	%term_to_atom(Term1,Atom),
	write(Term1),write('<br>\n'));
	(%term_to_atom(Term,Atom),
	writeln(Term))),!.

write_br(Term) :-	
	(html_api_maker_or_terminal(html)->
	(n_to_br(Term,Term1),
	%term_to_atom(Term1,Atom),
	write(Term1));
	(%term_to_atom(Term,Atom),
	write(Term))),!.
	
	
shell1_s(Command,Out) :-
 	atom_string(Command1,Command),
	bash_command(Command1,Out),
	%not(Out=user:fail),
	!.

shell1_s1(Command,Out) :-
 	atom_string(Command1,Command),
	bash_command1(Command1,Out),
	%not(Out=user:fail),
	!.
		
shell1_s(Command) :-
 	atom_string(Command1,Command),
	shell1(Command1),!.

shell1_s1(Command) :-
 	atom_string(Command1,Command),
	shell11(Command1),!.
	
shell1(Command) :-
	bash_command(Command,_),!.

shell11(Command) :-
	bash_command1(Command,_),!.

bash_command(Command, Output) :-
	timeout(bash_command1(Command, Output),3,10),!.

bash_command1(Command, Output) :-
		setup_call_cleanup(process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out))
        ,!.
        
timeout(A,N,S) :-
	timeout(N,A,N,S),!.

timeout(N2,A,0,S) :- writeln_info([%A,"has"
"Failed after ",N2," unsuccessful attempts with timeouts of ",S," seconds."]),fail,%abort,
!.
timeout(N2,A,N,S) :-
	Time_limit is 5*60*60,
	(catch(call_with_time_limit(Time_limit,(writeln_info(["Trying"%,A
	]),catch(A,_,fail))),time_limit_exceeded,fail)->writeln_info([%A,
	"Successful."]);(writeln_info(["Failed."%,A%,". Please press return to retry."
	]),N1 is N-1,(N1=0->true;%true%
	sleep(S)%)
	%)
	),%writeln_info(["Failed",A%,". Please press return to retry."
	%]),%read_string(user_input,"\n","\r",_,_),
	timeout(N2,A,N1,S))),!.


foldr(string_concat,[A,B,C],D) :-
	not(var(A)),var(B),not(var(C)),
	string_concat(A,G,D),
	string_concat(B,C,G),!.
	
% aBc=abc	
foldr(string_concat1,ABC,D) :-
	findall(E,(member(E,ABC),var(E)),E1),
	length(E1,1),not(var(D)),
	member(V,ABC),var(V),!,
	sub_term_types_wa([var],ABC,[[Ad,_]]),
	(string(D)->string_strings(D,D11);atom_chars(D,D11)),
	%trace,
	findall(M,(member(M1,D11),(catch(number_string(M,M1),_,false)->true;M=M1)),M2),
	get_sub_term_wa(M2,Ad,V),!.
	
/*
foldr(string_concat2,["a","b",C,D,E,"f"],"abcdef").

C = D, D = "",
E = "cde" ;
C = "",
D = "c",
E = "de" ;
C = "",
D = "cd",
E = "e" ;
C = E, E = "",
D = "cde" ;
C = "c",
D = "",
E = "de" 
...
*/
	
foldr(string_concat2,ABC,D) :-
	findall(E,(member(E,ABC),var(E)),E1),
	length(E1,3),
	not(var(D)),
	sub_term_types_wa([var],ABC,[[Ad1,_],[Ad2,_],[Ad3,_]]),
	Ad1=[_,N1],Ad2=[_,N2],N2 is N1+1,Ad3=[_,N3],N3 is N2+1,
	(string(D)->string_strings(D,D11);atom_chars(D,D11)),
	findall(M,(member(M1,D11),(catch(number_string(M,M1),_,false)->true;M=M1)),M2),
	%findall(L,(member(L1,ABC),(var(L1)->L=L1;L=[L1])),L2),
	
	N4 is N1-1,
	length(N4L,N4),
	length(N4L1,N4),
	N5 is 3,
	length(N5L,N5),
	length(N5L1,N5),
	append(N4L,N6,M2),
	append(N5L,_,N6),

	append(N4L1,N61,ABC),
	append(N5L1,_,N61),

	append(V1,V4,N5L),
	append(V2,V3,V4),

	foldr(string_concat,V1,V11),
	foldr(string_concat,V2,V21),
	foldr(string_concat,V3,V31),	
	N5L1=[V11,V21,V31].
	

foldr(Function,A,L,B) :-
	reverse(A,C),
	foldl(Function,C,L,B),!.

foldr(append,A,B) :-
	append(A,B),!.
	
%foldr(append1,A,B) :-
%	foldr(append1,A,[],B),!.
	
foldr(string_concat,A,B) :-
	atomics_to_string(A,'',B),!.

foldr(atom_concat,A,B) :-
	atomic_list_concat(A,'',B),!.

concat_list_terms(A,B) :-
	findall(X,(member(X1,A),((string(X1)->true;(atom(X1)->true;number(X1)))->X=X1;term_to_atom(X1,X)%,term_to_atom(X2,X)
	)),A1),
	concat_list(A1,B),!.
	
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

split_string_on_non_alpha(A,B1) :-
numbers(255,1,[],Ns3),foldl(alpha_and_string,Ns3,[],B8), % ABC
% A = ABC123ABC
% Want ABC,ABC
string_strings(A,A1),

maplist(if_member_equals_else_space(B8),A1,E2),

foldr(string_concat,E2,B9),split_string(B9," "," ",B1),!.

alpha_and_string(N21,L,A22) :- ((char_code(A2,N21),char_type(A2,alpha),atom_string(A2,A21),append(L,[A21],A22))->true;L=A22),!.

if_member_equals_else_space(B8,E1,E) :-
(member(E1,B8)->E=E1;E=" "),!.

split_string(A,B,C) :-
	split_string(A,B,B,C),!.
	
split_string1a(String1,Chars,List) :-
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

% split_on_substring117a("AAABAAD","BD",[],A).
% A = ["AAA", "B", "AA", "D"].

split_on_substring117a(A,B,_,D) :-
    string_codes(A,A1),
    string_codes(B,B1),
	split_on_substring117(A1,B1,[],D),!.

% split_on_substring117a("AAABAAD","BD",A).
% A = ["AAA", "B", "AA", "D"].

split_on_substring117a(A,B,C) :-
	split_on_substring117a(A,B,_,C),!.


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


% find_first(B,(member(B,[true,false]),B),D).
% D = [true] .
find_first(A) :-
 A,!.

/*
find_first(A,B,B_condition,C) :-
 D = (B,(B_condition->!;fail)),findall(A,D,C1),
 (C1=[C|_]->true;C=[]),!.
*/

% findall_until_fail(B,member(B,[true,true,false,true]),B,D).
% D = [true,true] .

findall_until_fail(A,B,B_condition,C) :-
	D=(B,(B_condition->true;(!,fail))),
	findall(A,D,C),!.
	
find_until_passes(A)	:- (A->true;find_until_passes(A)),!.
	
find_until(A,B,C) :-
	append(C,B4,A),
	append([B],_C4,B4),!.

% repeat_until_the_same(A=1,A,B is A+1,B,C).
% A=1,B = 2.

/*
repeat_until_the_same(A,Value,B_initial,B,B_result) :-
	%copy_term(B,B1),
	A=Value,
	%A_initial_assignment,
	repeat_until_the_same1(A,B_initial,B,B_result),!.
repeat_until_the_same1(A,B_initial,B,B_result2) :-
	%copy_term(A,A1),
	copy_term(B_initial,B_initial1),
	copy_term(B,B1),
	copy_term(B,B2),
	copy_term(B_result,B_result1),
	A=B_initial,B1,(A=B_result->B_result=B_result2;(repeat_until_the_same1(A,B_initial1,B2,B_result2))),!.
*/

% repeat_until_the_same(A,1,(C=1,writeln(C)),C). 
% A = C, C = D.

% repeat_until_the_same(A,1,(random(X),C is floor(3*X),writeln(C)),C).
% 0
% 2
% 1
% A = E, E = C, C = 1,
% X = 0.5791569817112253.

/*
repeat_until_the_same(A,Value,B,B_res_var) :-
	repeat_until_the_same1(A,Value,B_res_var,B,B_res_var,B_res_var),!.

repeat_until_the_same1(A1,Value,Value,B,B_res_var,B_res_var1) :-
	copy_term(A,A1),
	A=Value,
	B,(B_res_var=B_res_var1->
	true;
	repeat_until_the_same1(A1,Value,B_res_var,B,B_res_var,B_res_var1)),!.



% repeat_until_the_same(A,1,((A=1->A2=2;A2=1),writeln(A2)),A2).
% repeat until the last two results are the same.

repeat_until_the_same(A,Value,B,B_res_var) :-
	copy_term(A,A_new),
	copy_term(B,B_new),
	A=Value,
	B,
	repeat_until_the_same1(B_new,A_new,%A,
	A,Value,B_res_var,B,B_res_var,B_res_var),!.

repeat_until_the_same1(B_new,A_new,%A,
A,Value,Value,B,B_res_var,B_res_var1) :-
	copy_term(A,A1),
	copy_term(A_new,A_new1),
	copy_term(A_new,A_new2),
	copy_term(B_new,B_new1),
	A_new=Value,
	B_new,(B_res_var=B_res_var1->
	true;
	repeat_until_the_same1(B_new1,A_new2,%A2,
	A1,Value,B_res_var,B,B_res_var,B_res_var1)),!.
*/

% repeat_until_last_two_same(generate_result(Result),Result,Result1).
		
% to do: initial values, more result vars		

%repeat_until_last_two_same(Pred,Pred_res_var,Result) :-
%   repeat_until_last_two_same(Pred,Pred_res_var, _,_, Result),!.

/*
repeat_until_last_two_same(Pred,Pred_res_var,A, B, Result) :-
	functor(Pred,Name,Arity),
	functor(Pred_new,Name,Arity),
	copy_term(Pred_res_var,Pred_res_var_new),
	%copy_term(Pred_new,Pred_new1),
	%copy_term(Pred_new,Pred_new2),
	% Generate some result
    (Pred,%generate_result(Result1),
    (%false%var(Pred_res_var)
    %->%var(B)
    
    %(
        %repeat_until_last_two_same(Pred_new1,Pred_res_var,B,Pred_res_var, Result);
        
% repeat_until_last_two_same(Result1).
		
% to do: initial values, more result vars		

* put into shell command to work with other Prolog predicate arguments

        */

repeat_until_last_two_same(Result) :-
   repeat_until_last_two_same(_, Result),!.
        
  	repeat_until_last_two_same(B, Result) :-
    % Generate some result
    generate_result(Result1),
    % Check if the last two results are the same
    (   ((var(B)->fail;true),
    Result1 = B, B = Result)->true;
    % If not, continue repeating
       repeat_until_last_two_same(Result1, Result)
    ),!.    


/*
%	repeat_until_last_two_same(generate_result(Result1),Result1,R).
    repeat_until_last_two_same( Pred, Result_var, Result) :-
	    repeat_until_last_two_same1(Pred, Result_var,_, Result),!.
    repeat_until_last_two_same1(Pred, Result_var, B, Result) :-
    %copy_term(Result_var,Result_var0),
    functor(Result_var,Name,Arity),
	functor(Result_var0,Name,Arity),

    % Generate some result
    Pred,%generate_result(Result1),
    % Check if the last two results are the same
    (   ((var(B)->fail;true),
    Result_var = B, B = Result)->true;
    % If not, continue repeating
       repeat_until_last_two_same1(Pred, Result_var0,Result_var, Result)
    ),!.    

% Predicate to generate a result (replace this with your actual computation)
*/
generate_result(Result) :-
    % For example, generating a random integer between 1 and 100
    random(1, 3, Result),
    writeln(Result).

%not(A) :- \+(A),!.

% Doesn't delete \n in "\n"
split_on_substring1(A,B,D) :-
	string_codes(A,A1),
	string_codes(B,B1),
	split_on_substring117(A1,B1,[],D),!.

% Deletes \n in "\n"
split_on_substring(A,B,_,D) :-
	split_on_substring(A,B,D),!.
		
split_on_substring(A,B,D) :-
	string_codes(A,A1),
	string_codes(B,B1),
	split_on_substring117(A1,B1,[],D1),
	(D1=[]->D2=[""];D1=D2),
	%D2=D,%delete_double_newlines(D2,[],D),
	append(_D6,B4,D2),
	append([B5],_C4,B4),
	not(B5="\n"),
	delete_newlines_after_text(B4,[],D),!.
	
split_string1(A,B,C) :-
	split_string1(A,B,_,C),!.
	
split_string1(A,B,_,C) :-
	%string_strings(B,B2),
	split_on_substring(A,B,C),
	%findall(D,(member(D1,C1),(member(D1,B2)->D="";D=D1)),C),
	!.
	
split_string1b(A,B,C) :-
	split_string1b(A,B,_,C),!.
	
split_string1b(A,B,_,C) :-
	string_strings(B,B2),
	split_on_substring(A,B,C1),
	findall(D,(member(D1,C1),(member(D1,B2)->D="";D=D1)),C),!.
	
delete_newlines_after_text([],A,A) :- !.
delete_newlines_after_text([A],B,C) :- 
	append(B,[A],C),!.
delete_newlines_after_text(D2,D1,D) :-
	D2=[T,"\n"|D4],
	append(D5,B4,D4),
	append([B],_C4,B4),
	not(B="\n"),
	length(D5,L),
	numbers(L,1,[],Ns),
	findall("",member(_,Ns),D51),
	foldr(append,[D1,[T],D51],D6),
	delete_newlines_after_text(B4,D6,D),!.
delete_newlines_after_text(D2,D1,D) :-
	D2=[T|D4],
	%append(D5,B4,D4),
	%append([B],_C4,B4),
	%not(B="\n"),
	foldr(append,[D1,[T]],D6),
	delete_newlines_after_text(D4,D6,D),!.

/*
delete_double_newlines([],D,D) :- !.
delete_double_newlines(D2,D5,D) :-
	trace,
	D2=["\n","\n"|D4],
	append(D5,["\n"],D6),
	delete_double_newlines(D4,D6,D),!.
delete_double_newlines(D2,D5,D) :-
	trace,
	D2=["\n"|D4],
	append(D5,[],D6),
	delete_double_newlines(D4,D6,D),!.
delete_double_newlines(D2,D5,D) :-
	D2=[D3|D4],
	append(D5,[D3],D6),
	delete_double_newlines(D4,D6,D),!.
*/

retract_all(A) :- retractall(A).

remove_dups([],[]) :- !.
remove_dups([Head|Tail],Result):-
 member(Head,Tail),
 remove_dups(Tail,Result),!.
remove_dups([Head|Tail],[Head|Result]):-
 remove_dups(Tail,Result),!.

remove_first_and_last_items(L1,L5) :-
 append([_L],L4,L1),
 append(L5,[_L3],L4),!.

remove_first_and_last_items(L,R,L1,L5) :-
 append([L],L4,L1),
 append(L5,[R],L4),!.

findall1(A,B,C) :- findall(A,B,C),!.

% first_n(B,member(B,([true,true,false,true,true,false]),B),D,3).
% D = [true,true,true] .

first_n(A,B,C,Limit) :-
retractall(first_n(_)),
assertz(first_n(0)),
	D=(B,first_n(First_n),First_n1 is First_n+1,retractall(first_n(_)),assertz(first_n(First_n1)),(First_n1>=Limit->!;true)),
	findall(A,D,C),!.

sub_string_strings(List,Before1,Sub_string,After1) :-
	string_strings(List,List1),
	string_strings(Sub_string,Sub_string1),
	append(Before,L1,List1),
	append(Sub_string1,After,L1),
	foldr(string_concat,Before,Before1),
	foldr(string_concat,After,After1).

reverse_string(A,B) :- string_strings(A,C), reverse(C,D), foldr(string_concat,D,B),!.
