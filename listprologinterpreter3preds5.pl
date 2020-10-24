interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	not(isempty(Value1)),
	expression(Value1),
	isempty(Value2),
        val1emptyorvalsequal(Value2,Value1),
	%%isval(Value2),
debug_call(Skip,[[n,is],[Value1,variable]]),
(        putvalue(Variable2,Value1,Vars1,Vars2)->
debug_exit(Skip,[[n,is],[Value1,Value1]])
;     debug_fail(Skip,[[n,is],[Value1,variable]])),!.

interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	isempty(Value1),
	not(isempty(Value2)),
	expression(Value2),
        val1emptyorvalsequal(Value1,Value2),
	%%isval(Value2),
debug_call(Skip,[[n,is],[variable,Value2]]),
(        putvalue(Variable1,Value2,Vars1,Vars2)->
debug_exit(Skip,[[n,is],[Value2,Value2]])
;     debug_fail(Skip,[[n,is],[variable,Value2]])),!.
		
interpretpart(bracket1,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,wrap],[Value1,Variable2]]),
((	Value1A = [Value1],
        val1emptyorvalsequal(Value2,Value1A),
        %%val1emptyorvalsequal(Value1A,Value2),
        putvalue(Variable2,Value1A,Vars1,Vars2))->
debug_exit(Skip,[[n,wrap],[Value1A,Value1A]])
;     debug_fail(Skip,[[n,wrap],[Value1,Variable2]])),!.

interpretpart(stringtonumber,Variable2,Variable1,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
	%%Value1A = [Value2],
debug_call(Skip,[[n,stringtonumber],[Value2,value]]),
	((((Value2=""->true;Value2=empty)->Value1="";
	number_string(Value1A,Value2)),
        val1emptyorvalsequal(Value1,Value1A),
        %%val1emptyorvalsequal(Value1A,Value2),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
debug_exit(Skip,[[n,stringtonumber],[Value2,Value1A]])
;     debug_fail(Skip,[[n,stringtonumber],[Value2,value]])),!.


interpretpart(bracket2,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,unwrap],[variable,Value2]]),
        (([Value2A] = Value1,
        val1emptyorvalsequal(Value2,Value2A),
        %%val1emptyorvalsequal(Value2A,Value1),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[n,unwrap],[Value1,Value2A]])
;     debug_fail(Skip,[[n,unwrap],[variable,Value2]])),!.
        	
interpretpart(head,Variable1,Variable2,Vars1,Vars2) :-
	getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,head],[Value1,variable]]),
	((Value1=[Value1A|_Rest],
        val1emptyorvalsequal(Value2,Value1A),
        putvalue(Variable2,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[n,head],[Value1,Value1A]])
;     debug_fail(Skip,[[n,head],[Value1,variable]])),!.
        	
interpretpart(tail,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,tail],[Value1,variable]]),
        ((Value1=[_Head|Value1A],
	%%removebrackets(Value1A,Value1B), 
        val1emptyorvalsequal(Value2,Value1A),
        putvalue(Variable2,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[n,tail],[Value1,Value1A]])
;     debug_fail(Skip,[[n,tail],[Value1,variable]])),!.
        	
interpretpart(member,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,member],[Value1,Value2]]),
  	(((not(Value2=empty)->member(Value2,Value1),
	(member(Value3,Value1),
	putvalue(Variable2,Value3,Vars1,Vars2)%%,Vars2=Vars1
	)))->
      debug_exit(Skip,[[n,member],[Value1,Value3]])
;     debug_fail(Skip,[[n,member],[Value1,Value2]])),!.

interpretpart(member2,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[n,member2],[Value1,Value2]]),
	((Value2=empty,((member(Value2a,Value1),
	putvalue(Variable2,Value2a,Vars1,Vars2)))),
      debug_exit(Skip,[[n,member2],[Value1,Value2a]])).
%%;     %%debug_fail(Skip,[[n,member2],[Value1,Value2]])),!.
%%		((debug(on)->(writeln1([fail,[[n,member2],[Value1,value]],"Press c."]),(leash1(on)->true;(not(get_single_char(97))->true;abort)));true),fail))))).

interpretpart(isop,Operator,Variable1,Variable2,Variable3,Vars1,Vars2) :-
getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        debug_call(Skip,[[n,Operator],[Value2,Value3,variable]]),
	((isvalempty(Value1),
	isval(Value2),
	isval(Value3),
	Expression=..[Operator,Value2,Value3],
        Value1A is Expression,
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[n,Operator],[Value2,Value3,Value1A]])
;     debug_fail(Skip,[[n,Operator],[Value2,Value3,variable]])),!.

interpretpart(iscomparison,Operator,Variable1,Variable2,Vars1,Vars1) :-        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[n,Operator],[Value1,Value2]]),
	((isval(Value1),
	isval(Value2),
	Expression=..[Operator,Value1,Value2],
        Expression)->
      debug_exit(Skip,[[n,Operator],[Value1,Value2]])
;     debug_fail(Skip,[[n,Operator],[Value1,Value2]])),!.

interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        	not(isempty(Value1)),
        	not(isempty(Value2)),
        debug_call(Skip,[[n,=],[Value1,Value2]]),
        ((Value1A = Value2,
		val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[n,=],[Value1A,Value2]])
;     debug_fail(Skip,[[n,=],[Value1,Value2]])),!.                        	

     	
interpretpart(match1,Variable1,Variable2,Variable3,Vars1,Vars2) :-
getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        Value1 = [Value2A, Value3A],
        debug_call(Skip,[[n,=],[[Value2A,Value3A],[variable1,variable2]]]),
        ((val1emptyorvalsequal(Value2,Value2A),
        val1emptyorvalsequal(Value3,Value3A),
        putvalue(Variable2,Value2A,Vars1,Vars3),
        putvalue(Variable3,Value3A,Vars3,Vars2))->
      debug_exit(Skip,[[n,=],[[Value2A, Value3A],[Value2A, Value3A]]])
;     debug_fail(Skip,[[n,=],[[Value2A,Value3A],[variable1,variable2]]])),!.                        	
		
interpretpart(match2,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        Value1A = [Value2, Value3],
        debug_call(Skip,[[n,=],[variable,[Value2,Value3]]]),
        ((val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      (debug_exit(Skip,[[n,=],[[Value2,Value3],[Value2,Value3]]])
;     debug_fail(Skip,[[n,=],[variable,[Value2,Value3]]]))),!.                        	

interpretpart(match3,Variable1,Variable2,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        Value1A = Value2,
        debug_call(Skip,[[n,=],[variable,Value2]]),
        ((val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      (debug_exit(Skip,[[n,=],[Value2,Value2]])
;     debug_fail(Skip,[[n,=],[variable,Value2]]))),!.                        	

interpretpart(delete,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        debug_call(Skip,[[n,delete],[Value1,Value2,variable3]]),
        ((delete(Value1,Value2,Value3A),
        val1emptyorvalsequal(Value3,Value3A),
        putvalue(Variable3,Value3A,Vars1,Vars2))->
      debug_exit(Skip,[[n,delete],[Value1,Value2,Value3A]])
;     debug_fail(Skip,[[n,delete],[Value1,Value2,variable3]])),!.                        	

interpretpart(append,Variable1,Variable2,Variable3,Vars1,Vars2) :-
        	
        getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        debug_call(Skip,[[n,append],[Value1,Value2,variable3]]),
        ((append1(Value1,Value2,Value3A),
        val1emptyorvalsequal(Value3,Value3A),
        putvalue(Variable3,Value3A,Vars1,Vars2))->
      debug_exit(Skip,[[n,append],[Value1,Value2,Value3A]])
;     debug_fail(Skip,[[n,append],[Value1,Value2,variable3]])),!.                        	

interpretpart(date,Year,Month,Day,Hour,Minute,Seconds,Vars1,Vars2) :-
        	
        getvalues(Year,Month,Day,YearValueA,MonthValueA,DayValueA,Vars1),
        getvalues(Hour,Minute,Seconds,HourValueA,MinuteValueA,SecondsValueA,Vars1),
        debug_call(Skip,[[n,date],[[variable1,variable2,variable3,variable4,variable5,variable6]]]),
        ((get_time(TS),stamp_date_time(TS,date(YearValueB,MonthValueB,DayValueB,HourValueB,MinuteValueB,SecondsValueB,_A,_TZ,_False),local),
        val1emptyorvalsequal(YearValueA,YearValueB),
        val1emptyorvalsequal(MonthValueA,MonthValueB),
        val1emptyorvalsequal(DayValueA,DayValueB),
        val1emptyorvalsequal(HourValueA,HourValueB),
        val1emptyorvalsequal(MinuteValueA,MinuteValueB),
        val1emptyorvalsequal(SecondsValueA,SecondsValueB),
        putvalue(Year,YearValueB,Vars1,Vars3),      	
        putvalue(Month,MonthValueB,Vars3,Vars4),      	
        putvalue(Day,DayValueB,Vars4,Vars5),      	
        putvalue(Hour,HourValueB,Vars5,Vars6),      	
        putvalue(Minute,MinuteValueB,Vars6,Vars7),      	
        putvalue(Seconds,SecondsValueB,Vars7,Vars2))->
      debug_exit(Skip,[[n,date],[YearValueB,MonthValueB,DayValueB,HourValueB,MinuteValueB,SecondsValueB]])
;     debug_fail(Skip,[[n,date],[variable1,variable2,variable3,variable4,variable5,variable6]])),!.

interpretpart(random,Variable1,Vars1,Vars2) :-        getvalue(Variable1,Value1,Vars1),
        debug_call(Skip,[[n,random],[variable]]),
   ((random(Value1A),
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->      	
      debug_exit(Skip,[[n,random],[Value1A]])
;     debug_fail(Skip,[[n,random],[variable]])),!.

interpretpart(length,Variable1,Variable2,Vars1,Vars2) :-        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[n,length],[Value1,variable]]),
	((islist(Value1),
   length(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[n,length],[Value1,Value2A]])
;     debug_fail(Skip,[[n,length],[Value1,variable]])),!.

interpretpart(ceiling,Variable1,Variable2,Vars1,Vars2) :-        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[n,ceiling],[Value1,variable]]),
	((isval(Value1),
   ceiling(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[n,ceiling],[Value1,Value2A]])
;     debug_fail(Skip,[[n,ceiling],[Value1,variable]])),!.

interpretpart(round,Variable1,Variable2,Vars1,Vars2) :-  
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[n,round],[Value1,variable2]]),
        ((Value2A is round(Value1),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[n,round],[Value1,Value2A]])
;     debug_fail(Skip,[[n,round],[Value1,variable2]])),!.                        	

interpretpart(stringconcat,Terminal,Phrase2,Phrase1,Vars1,Vars2) :-
	%%Variables1=[Terminal,Phrase1,Phrase2], %% terminal can be v or "a"
        ((getvalues2([Terminal,Phrase1,Phrase2],
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
        (Flag2=true->Phrase1Value3=variable2;Phrase1Value3=Phrase1Value1))->

(debug_call(Skip,[[n,stringconcat],[TerminalValue3,Phrase1Value3,Phrase2]]),	

debug_exit(Skip,[[n,stringconcat],[TerminalValue1,Phrase1Value1,Phrase2Value1]])
        	);
        	
        	(debug_call(Skip,[[n,stringconcat],[variable1,variable2,variable3]]),
        	debug_fail(Skip,[[n,stringconcat],[variable1,variable2,variable3]])
        	)),!.
        	
interpretpart(grammar_part,Variables1,Vars1,Vars2) :-
	((Variables1=[Terminal,Phrase1,Phrase2], %% terminal can be v or "a"
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
        (Flag2=true->Phrase1Value3=variable2;Phrase1Value3=Phrase1Value1))->
        	(debug_call(Skip,[[n,grammar_part],[TerminalValue3,Phrase1Value3,Phrase2]]),
        	debug_exit(Skip,[[n,grammar_part],[TerminalValue1,Phrase1Value1,Phrase2Value1]]));

        	(debug_call(Skip,[[n,grammar_part],[variable1,variable2,variable3]]),
        (debug_fail(Skip,[[n,grammar_part],[variable1,variable2,variable3]])))),!.
        	
        	
        	

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
maplist(expression,[Item1,Item2,Item3]),
	string_concat(Item1,Item2,Item3),!.

append1([],Item,Item) :-
	!.
append1(Item11,Item21,Item31) :-
	
replace_empty_with_empty_set(	[Item11,Item21,Item31],[],[Item1,Item2,Item3]),
%%maplist(expression,[Item1,Item2,Item3]), %% commented out 21 8 19
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