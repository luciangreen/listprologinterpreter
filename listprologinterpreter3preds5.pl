interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),

        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	expression(Value1),
	expression(Value2),
        val1emptyorvalsequal(Value1,Value2),
	%%isval(Value2),
        putvalue(Variable1,Value2,Vars1,Vars2),
		(debug(on)->(writeln([call,[[n,is],[variable,Value2]],"Press c."]),(not(get_single_char(97))->true;abort));true),
		(debug(on)->(writeln([exit,[[n,is],[Variable1,Value2]],"Press c."]),(not(get_single_char(97))->true;abort));true).
		
		interpretpart(bracket1,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
	Value1A = [Value2],
        val1emptyorvalsequal(Value1,Value1A),
        %%val1emptyorvalsequal(Value1A,Value2),
        putvalue(Variable1,Value1A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,wrap],[variable,[Value2]]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,wrap],[Variable2,[Value2]]],"Press c."]),(not(get_single_char(97))->true;abort));true).

		interpretpart(stringtonumber,Variable2,Variable1,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
	%%Value1A = [Value2],
	(Value2A=""->Value1="";
	number_string(Value1,Value2A)),
        val1emptyorvalsequal(Value2,Value2A),
        %%val1emptyorvalsequal(Value1A,Value2),
        putvalue(Variable2,Value2A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,stringtonumber],[Value1,Value2A]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,stringtonumber],[Value1,Value2A]],"Press c."]),(not(get_single_char(97))->true;abort));true).
        	
interpretpart(bracket2,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        Value1A = Value2,
        val1emptyorvalsequal(Value1,Value1A),
        %%val1emptyorvalsequal(Value2A,Value1),
        putvalue(Variable1,Value1A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,unwrap],[[variable],[Value2]]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,unwrap],[[Value2],[Value2]]],"Press c."]),(not(get_single_char(97))->true;abort));true).
        	
interpretpart(head,Variable1,Variable2,Vars1,Vars2) :-
	getvalues(Variable1,Variable2,Value1,Value2,Vars1),
	Value1=[Value1A|_Rest],
        val1emptyorvalsequal(Value2,Value1A),
        putvalue(Variable2,Value1A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,head],[Value1,variable]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,head],[Value1,Value1A]],"Press c."]),(not(get_single_char(97))->true;abort));true),	!.
        	
interpretpart(tail,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        Value1=[_Head|Value1A],
	%%removebrackets(Value1A,Value1B), 
        val1emptyorvalsequal(Value2,Value1A),
        putvalue(Variable2,Value1A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,tail],[Value1,variable]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,tail],[Value1,Value1A]],"Press c."]),(not(get_single_char(97))->true;abort));true).
        	
interpretpart(member,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
	(not(Value1=empty)->(member(Value1,Value2),Vars2=Vars1,
        	(debug(on)->(writeln([call,[[n,member],[Value1,Value2]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,member],[Value1,Value2]],"Press c."]),(not(get_single_char(97))->true;abort));true));
	(member(Value3,Value2),
	putvalue(Variable1,Value3,Vars1,Vars2),
                (debug(on)->(writeln([call,[[n,member],[variable1,Value2]],"Press c."]),(not(get_single_char(97))->true;abort));true),
                (debug(on)->(writeln([exit,[[n,member],[Value3,Value2]],"Press c."]),(not(get_single_char(97))->true;abort));true))).

interpretpart(isplus,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
	isvalempty(Value1),
	isval(Value2),
	isval(Value3),
        Value1A is Value2 + Value3,
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,+],[Value2,Value3,variable]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,+],[Value2,Value3,Value1A]],"Press c."]),(not(get_single_char(97))->true;abort));true).
        	
interpretpart(match,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        Value1 = [Value2A, Value3A],
        val1emptyorvalsequal(Value2,Value2A),
        val1emptyorvalsequal(Value3,Value3A),
        putvalue(Variable2,Value2A,Vars1,Vars3),
        putvalue(Variable3,Value3A,Vars3,Vars2),
        	(debug(on)->(writeln([call,[[n,=],[[Value2A, Value3A],[variable1,variable2]]],"Press c."]),(not(get_single_char(97))->true;abort));true),        		
		(debug(on)->(writeln([exit,[[n,=],[[Value2A, Value3A],[Value2A, Value3A]],"Press c."]]),(not(get_single_char(97))->true;abort));true).
		
interpretpart(match,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        Value1A = [Value2, Value3],
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,=],[variable,[Value2,Value3]]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,=],[Value2,Value3],[Value2,Value3]],"Press c."]),(not(get_single_char(97))->true;abort));true).

interpretpart(delete,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        delete(Value1,Value2,Value3A),
        val1emptyorvalsequal(Value3,Value3A),
        putvalue(Variable3,Value3A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,delete],[Value1,Value2,variable3]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,delete],[Value1,Value2,Value3A]],"Press c."]),(not(get_single_char(97))->true;abort));true).


interpretpart(append,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        append1(Value1,Value2,Value3A),
        val1emptyorvalsequal(Value3,Value3A),
        putvalue(Variable3,Value3A,Vars1,Vars2),
        	(debug(on)->(writeln([call,[[n,append],[Value1,Value2,variable3]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,append],[Value1,Value2,Value3A]],"Press c."]),(not(get_single_char(97))->true;abort));true).
        	
        	
interpretpart(stringconcat,Terminal,Phrase2,Phrase1,Vars1,Vars2) :-
	%%Variables1=[Terminal,Phrase1,Phrase2], %% terminal can be v or "a"
        getvalues2([Terminal,Phrase1,Phrase2],
        	[],[TerminalValue1,Phrase1Value1,Phrase2Value1],Vars1,[],[Flag1,Flag2,_Flag3]), %% prolog vars, list of vars, [v]=[prolog var]
        %%delete(Value1,Value2,Value3A),
        (Terminal=[_Value]->TerminalValue2=[TerminalValue1];TerminalValue2=TerminalValue1),
                
(Terminal=""->(TerminalValue2="",
       
string_concat(TerminalValue2,Phrase2Value1,Phrase1Value1))->true;
            ((var(TerminalValue2)->(string_concat(TerminalValue2,Phrase2Value1,Phrase1Value1)),string_length(TerminalValue2,1));string_concat(TerminalValue2,Phrase2Value1,Phrase1Value1))),
                
        putvalue(Terminal,TerminalValue2,Vars1,Vars3),
        putvalue(Phrase2,Phrase2Value1,Vars3,Vars4),
        putvalue(Phrase1,Phrase1Value1,Vars4,Vars2),
        (Flag1=true->TerminalValue3=variable1;TerminalValue3=TerminalValue1),
        (Flag2=true->Phrase1Value3=variable2;Phrase1Value3=Phrase1Value1),
        	(debug(on)->(writeln([call,[[n,stringconcat],[TerminalValue3,Phrase1Value3,Phrase2]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,stringconcat],[TerminalValue1,Phrase1Value1,Phrase2Value1]],"Press c."]),(not(get_single_char(97))->true;abort));true),!.
        	
        	        	

interpretpart(grammar_part,Variables1,Vars1,Vars2) :-
	Variables1=[Terminal,Phrase1,Phrase2], %% terminal can be v or "a"
        %%terminal(Terminal),
        getvalues2([Terminal,Phrase1,Phrase2],
        	[],[TerminalValue1,Phrase1Value1,Phrase2Value1],Vars1,[],[Flag1,Flag2,_Flag3]), %% prolog vars, list of vars, [v]=[prolog var]
        %%delete(Value1,Value2,Value3A),
        (Terminal=[_Value]->TerminalValue2=[TerminalValue1];TerminalValue2=TerminalValue1),



((string(Phrase1Value1)->Phrase1Value1=Phrase1Value11;(number(Phrase1Value1)->number_string(Phrase1Value1,Phrase1Value11);Phrase1Value1=Phrase1Value11)),

(Terminal=""->TerminalValue2="";true),
       
(((var(TerminalValue2)->(string_concat(TerminalValue2,Phrase2Value1,Phrase1Value11)),string_length(TerminalValue2,1));string_concat(TerminalValue2,Phrase2Value1,Phrase1Value11))->true;    

string_concat(TerminalValue2,Phrase2Value1,Phrase1Value11))->true;
            


((Phrase1Value1=[_ItemA|_ItemsA]),(Terminal=[]->(TerminalValue2=[],

((var(TerminalValue2)->length(TerminalValue2,1);true),(append(TerminalValue2,Phrase2Value1,Phrase1Value1))))->true;

(append(TerminalValue2,Phrase2Value1,Phrase1Value1)->true)))),

        putvalue(Terminal,TerminalValue2,Vars1,Vars3),
        putvalue(Phrase2,Phrase2Value1,Vars3,Vars4),
        putvalue(Phrase1,Phrase1Value1,Vars4,Vars2),
        (Flag1=true->TerminalValue3=variable1;TerminalValue3=TerminalValue1),
        (Flag2=true->Phrase1Value3=variable2;Phrase1Value3=Phrase1Value1),
        	(debug(on)->(writeln([call,[[n,grammar_part],[TerminalValue3,Phrase1Value3,Phrase2]],"Press c."]),(not(get_single_char(97))->true;abort));true),
        	(debug(on)->(writeln([exit,[[n,grammar_part],[TerminalValue1,Phrase1Value1,Phrase2Value1]],"Press c."]),(not(get_single_char(97))->true;abort));true),!.

getvalues(Variable1,Variable2,Value1,Value2,Vars) :-
        getvalue(Variable1,Value1,Vars),
        getvalue(Variable2,Value2,Vars).
getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars) :-
        getvalue(Variable1,Value1,Vars),
        getvalue(Variable2,Value2,Vars),
        getvalue(Variable3,Value3,Vars).
val1emptyorvalsequal(empty,_Value) :- !.
val1emptyorvalsequal(Value,Value) :-
	not(Value=empty).
isop(is).
isop(=).
stringconcat1([],Item,Item) :-
	!.
stringconcat1(Item11,Item21,Item31) :-
	
replace_empty_with_empty_set(	[Item11,Item21,Item31],[],[Item1,Item2,Item3]),
maplist(expressionnotatom,[Item1,Item2,Item3]),
	string_concat(Item1,Item2,Item3),!.

append1([],Item,Item) :-
	!.
append1(Item11,Item21,Item31) :-
	
replace_empty_with_empty_set(	[Item11,Item21,Item31],[],[Item1,Item2,Item3]),
maplist(expressionnotatom,[Item1,Item2,Item3]),
/**((isvalstr(Item1),Item1A=[Item1]);(not(isvalstr(Item1)),Item1A=Item1)),
        ((isvalstr(Item2),Item2A=[Item2]);(not(isvalstr(Item2)),Item2A=Item2)),
        %%((isvalstr(Item3),Item3A=[Item3]);(not(isvalstr(Item3)),Item3A=Item3)),
        **/
	append(Item1,Item2,Item3),!.
/**delete1(Item1,Item2,Item3) :-
	((isvalstr(Item1),Item1A=[Item1]);(not(isvalstr(Item1)),Item1A=Item1)),
        ((isvalstr(Item2),Item2A=[Item2]);(not(isvalstr(Item2)),Item2A=Item2)),
        %%((isvalstr(Item3),Item3A=[Item3]);(not(isvalstr(Item3)),Item3A=Item3)),
	delete(Item1A,Item2A,Item3).
**/
replace_empty_with_empty_set([],A,A) :-!.
replace_empty_with_empty_set(A,B,C) :-
	A=[Item1|Items],
	(var(Item1)->Item2=Item1;(Item1=empty->Item2=[];Item2=Item1)),
	append(B,[Item2],D),
	replace_empty_with_empty_set(Items,D,C),!.
removebrackets([[Value]],Value) :-!.
removebrackets(Value,Value).