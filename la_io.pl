% la_io.pl

read_number(Prompt,N) :-
repeat,writeln(Prompt),
read_string(user_input,"\n","\r",_,S),
number_string(N,S),!.

read_string(Prompt,S) :-
repeat,writeln(Prompt),
read_string(user_input,"\n","\r",_,S),
string(S),!.


menu_display(Menu_items,Result) :-

	length(Menu_items,Menu_items_length),
	numbers(Menu_items_length,1,[],N),
	findall([N1,"\t",Menu_item,"\n"],(member(N1,N),
	get_item_n(Menu_items,N1,Menu_item)),Menu1),
	flatten1_menu(Menu1,Menu3),
	foldr(string_concat,Menu3,Menu2),
	writeln(Menu2),
	menu_display2(Menu_items,Menu_items_length,Result).
	
menu_display2(Menu_items,Menu_items_length,Result) :-
	foldr(string_concat,["Please choose from menu items 1-",Menu_items_length,", or q to quit."],Prompt),
	writeln(Prompt),
	read_string(user_input,"\n","\r",_,Input),
	(Input="q"->Result=Input;
	(((number_string(Input_num,Input),
	Input_num>=1,Input_num=<Menu_items_length,
	get_item_n(Menu_items,Input_num,Result)))->true;
	menu_display2(Menu_items,Menu_items_length,Result))),!.

flatten1_menu(A,B) :-
 flatten2_menu(A,[],B),!.
 
flatten2_menu([],B,B) :- !.
flatten2_menu(A,B,C) :-
 (not((A=[_|_]->true;A=[]))->append(B,[A],C);
 (A=[D|E],flatten2_menu(D,B,F),
 flatten2_menu(E,F,C))),!.
