interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("is",Dbw_is),
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	not(isempty(Value1)),
	expression(Value1),
	isempty(Value2),
        val1emptyorvalsequal(Value2,Value1),
	%%isval(Value2),
debug_call(Skip,[[Dbw_n,Dbw_is],[Value1,variable]]),
(        putvalue(Variable2,Value1,Vars1,Vars2)->
debug_exit(Skip,[[Dbw_n,Dbw_is],[Value1,Value1]])
;     debug_fail(Skip,[[Dbw_n,Dbw_is],[Value1,variable]])),!.

interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("is",Dbw_is),
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	isempty(Value1),
	not(isempty(Value2)),
	expression(Value2),
        val1emptyorvalsequal(Value1,Value2),
	%%isval(Value2),
debug_call(Skip,[[Dbw_n,Dbw_is],[variable,Value2]]),
(        putvalue(Variable1,Value2,Vars1,Vars2)->
debug_exit(Skip,[[Dbw_n,Dbw_is],[Value2,Value2]])
;     debug_fail(Skip,[[Dbw_n,Dbw_is],[variable,Value2]])),!.
		
interpretpart(bracket1,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("wrap",Dbw_wrap),
        getvalues_equals4(Variable1,Variable2,Value1,_Value2,Vars1),
debug_call(Skip,[[Dbw_n,Dbw_wrap],[Value1,Variable2]]),
((	Value1A = [Value1],
        %val1emptyorvalsequal(Value2,Value1A),
        %%val1emptyorvalsequal(Value1A,Value2),
        putvalue_equals4(Variable2,Value1A,Vars1,Vars2))->
debug_exit(Skip,[[Dbw_n,Dbw_wrap],[Value1A,Value1A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_wrap],[Value1,Variable2]])),!.

interpretpart(stringtonumber,Variable2,Variable1,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("stringtonumber",Dbw_stringtonumber),
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
	%%Value1A = [Value2],
debug_call(Skip,[[Dbw_n,Dbw_stringtonumber],[Value2,value]]),
	((((Value2=""->true;Value2=empty)->Value1="";
	number_string(Value1A,Value2)),
        val1emptyorvalsequal(Value1,Value1A),
        %%val1emptyorvalsequal(Value1A,Value2),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
debug_exit(Skip,[[Dbw_n,Dbw_stringtonumber],[Value2,Value1A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_stringtonumber],[Value2,value]])),!.


interpretpart(bracket2,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("unwrap",Dbw_unwrap),
        getvalues_equals4(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[Dbw_n,Dbw_unwrap],[variable,Value2]]),
        (([Value2A] = Value1,
        %val1emptyorvalsequal(Value2,Value2A),
        %%val1emptyorvalsequal(Value2A,Value1),
        putvalue_equals4(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_unwrap],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_unwrap],[variable,Value2]])),!.
        	
interpretpart(head,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("head",Dbw_head),
	getvalues_equals4(Variable1,Variable2,Value1,_Value2,Vars1),
debug_call(Skip,[[Dbw_n,Dbw_head],[Value1,variable]]),
	((Value1=[Value1A|_Rest],
        %val1emptyorvalsequal(Value2,Value1A),
        putvalue_equals4(Variable2,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_head],[Value1,Value1A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_head],[Value1,variable]])),!.
        	
interpretpart(tail,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("tail",Dbw_tail),
        getvalues_equals4(Variable1,Variable2,Value1,_Value2,Vars1),
debug_call(Skip,[[Dbw_n,Dbw_tail],[Value1,variable]]),
        ((Value1=[_Head|Value1A],
	%%removebrackets(Value1A,Value1B), 
        %val1emptyorvalsequal(Value2,Value1A),
        putvalue_equals4(Variable2,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_tail],[Value1,Value1A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_tail],[Value1,variable]])),!.
        	
interpretpart(member,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("member",Dbw_member),
        getvalues_equals4(Variable1,Variable2,Value1,Value2,Vars1),
debug_call(Skip,[[Dbw_n,Dbw_member],[Value1,Value2]]),
  	%(((not(Value2=empty)->member(Value2,Value1),
	((member(Value3,Value1),
	putvalue_equals4(Variable2,Value3,Vars1,Vars2)%%,Vars2=Vars1
	)->
      debug_exit(Skip,[[Dbw_n,Dbw_member],[Value1,Value3]])
;     debug_fail(Skip,[[Dbw_n,Dbw_member],[Value1,Value2]])),!.

interpretpart(member2,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("member2",Dbw_member2),
        getvalues_equals4(Variable1,Variable2,Value1,Value2,Vars1),
	((%Value2=empty,
	((member(Value2a,Value1),
	debug_call(Skip,[[Dbw_n,Dbw_member2],[Value1,Value2]]),
	putvalue_equals4(Variable2,Value2a,Vars1,Vars2)))),
      debug_exit(Skip,[[Dbw_n,Dbw_member2],[Value1,Value2a]])).
%%;     %%debug_fail(Skip,[[n,member2],[Value1,Value2]])),!.
%%		((debug(on)->(writeln1([fail,[[n,member2],[Value1,value]],"Press c."]),(leash1(on)->true;(not(get_single_char(97))->true;abort)));true),fail))))).

interpretpart(member3,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("member3",Dbw_member2),
        getvalues_equals4(Variable1,Variable2,Value1,Value2,Vars1),
        %trace,
	((%Value2=empty,
	%trace,
	((member(Value1a,Value2),

	remember_and_turn_off_debug(Debug),

	(interpretpart(match4,Variable1,Value1a,Vars1,Vars2,_)->true;(turn_back_debug(Debug),fail)),
	
	turn_back_debug(Debug),

	
	debug_call(Skip,[[Dbw_n,Dbw_member2],[Value1,Value2]])
	%putvalue(Variable1,Value1a,Vars1,Vars2)
	))),
      debug_exit(Skip,[[Dbw_n,Dbw_member2],[Value1a,Value2]])).

interpretpart(isop,Operator,Variable1,Variable2,Variable3,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
        debug_call(Skip,[[Dbw_n,Operator],[Value2,Value3,variable]]),
	((isvalempty(Value1),
	isval(Value2),
	isval(Value3),
	Expression=..[Operator,Value2,Value3],
        Value1A is Expression,
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Operator],[Value2,Value3,Value1A]])
;     debug_fail(Skip,[[Dbw_n,Operator],[Value2,Value3,variable]])),!.

interpretpart(iscomparison,Operator,Variable1,Variable2,Vars1,Vars1) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[Dbw_n,Operator],[Value1,Value2]]),
	((isval(Value1),
	isval(Value2),
	Expression=..[Operator,Value1,Value2],
        Expression)->
      debug_exit(Skip,[[Dbw_n,Operator],[Value1,Value2]])
;     debug_fail(Skip,[[Dbw_n,Operator],[Value1,Value2]])),!.

interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
        getvalues_equals4(Variable1,Variable2,Value1,Value2,Vars1),
        %	not(isempty(Value1)),
        %	not(isempty(Value2)),
        debug_call(Skip,[[Dbw_n,=],[Value1,Value2]]),
        ((Value1A = Value2,
		%val1emptyorvalsequal(Value1,Value1A),
        putvalue_equals4(Variable1,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,=],[Value1A,Value2]])
;     debug_fail(Skip,[[Dbw_n,=],[Value1,Value2]])),!.                        	

     	
interpretpart(match1,Variable1,Variable2,Variable3,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
getvalues_equals4(Variable1,Variable2,Variable3,Value1,_Value2,_Value3,Vars1),
        Value1 = [Value2A, Value3A],
        debug_call(Skip,[[Dbw_n,=],[[Value2A,Value3A],[variable1,variable2]]]),
        ((%val1emptyorvalsequal(Value2,Value2A),
        %val1emptyorvalsequal(Value3,Value3A),
        putvalue_equals4(Variable2,Value2A,Vars1,Vars3),
        putvalue_equals4(Variable3,Value3A,Vars3,Vars2))->
      debug_exit(Skip,[[Dbw_n,=],[[Value2A, Value3A],[Value2A, Value3A]]])
;     debug_fail(Skip,[[Dbw_n,=],[[Value2A,Value3A],[variable1,variable2]]])),!.                        	
		
interpretpart(match2,Variable1,Variable2,Variable3,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
        getvalues_equals4(Variable1,Variable2,Variable3,_Value1,Value2,Value3,Vars1),
        Value1A = [Value2, Value3],
        debug_call(Skip,[[Dbw_n,=],[variable,[Value2,Value3]]]),
        ((%val1emptyorvalsequal(Value1,Value1A),
        putvalue_equals4(Variable1,Value1A,Vars1,Vars2))->
      (debug_exit(Skip,[[Dbw_n,=],[[Value2,Value3],[Value2,Value3]]])
;     debug_fail(Skip,[[Dbw_n,=],[variable,[Value2,Value3]]]))),!.                        	

interpretpart(match3,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
        getvalues_equals4(Variable1,Variable2,_Value1,Value2,Vars1),
        Value1A = Value2,
        debug_call(Skip,[[Dbw_n,=],[variable,Value2]]),
        ((%val1emptyorvalsequal(Value1,Value1A),
        putvalue_equals4(Variable1,Value1A,Vars1,Vars2))->
      (debug_exit(Skip,[[Dbw_n,=],[Value2,Value2]])
;     debug_fail(Skip,[[Dbw_n,=],[variable,Value2]]))),!.                        	

interpretpart(match4,Variable1,Variable2,Vars1,Vars2,_Note) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("equals4",Dbw_equals4),
%trace,
        debug_call(Skip,[[Dbw_n,Dbw_equals4],[Variable1,Variable2]]),
        %trace,
        (match4_2(Variable1,Variable2,Vars1,Vars2)
        
        %%Value1A = Value2,
        %%((val1emptyorvalsequal(Value1,Value1A),
        %%putvalue(Variable1,Value1A,Vars1,Vars2))
        ->
      (debug_exit(Skip,[[Dbw_n,Dbw_equals4],[Variable1,Variable2]])
;     debug_fail(Skip,[[Dbw_n,Dbw_equals4],[Variable1,Variable2]]))),!.                        	


interpretpart(delete,Variable1,Variable2,Variable3,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("delete",Dbw_delete),
        getvalues_equals4(Variable1,Variable2,Variable3,Value1,Value2,_Value3,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_delete],[Value1,Value2,variable3]]),
        ((delete(Value1,Value2,Value3A),
        %val1emptyorvalsequal(Value3,Value3A),
        putvalue_equals4(Variable3,Value3A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_delete],[Value1,Value2,Value3A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_delete],[Value1,Value2,variable3]])),!.                        	

interpretpart(append,Variable1,Variable2,Variable3,Vars1,Vars2) :-
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("append",Dbw_append),
        	%trace,
        %getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
	getvalue_equals4(Variable1,Value1,Vars1),
	getvalue_equals4(Variable2,Value2,Vars1),
	getvalue_equals4(Variable3,_Value3,Vars1),

debug_call(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,variable3]]),
        ((append1(Value1,Value2,Value3A),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable3,Value3A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,Value3A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,variable3]])),!.                        	

interpretpart(date,Year,Month,Day,Hour,Minute,Seconds,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("date",Dbw_date),
        	
        getvalues(Year,Month,Day,YearValueA,MonthValueA,DayValueA,Vars1),
        getvalues(Hour,Minute,Seconds,HourValueA,MinuteValueA,SecondsValueA,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_date],[[variable1,variable2,variable3,variable4,variable5,variable6]]]),
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
      debug_exit(Skip,[[Dbw_n,Dbw_date],[YearValueB,MonthValueB,DayValueB,HourValueB,MinuteValueB,SecondsValueB]])
;     debug_fail(Skip,[[Dbw_n,Dbw_date],[variable1,variable2,variable3,variable4,variable5,variable6]])),!.

interpretpart(random,Variable1,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("random",Dbw_random),
        getvalue(Variable1,Value1,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_random],[variable]]),
   ((random(Value1A),
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->      	
      debug_exit(Skip,[[Dbw_n,Dbw_random],[Value1A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_random],[variable]])),!.

interpretpart(length,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("length",Dbw_length),
       getvalues_equals4(Variable1,Variable2,Value1,_Value2,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_length],[Value1,variable]]),
	((islist(Value1),
   length(Value1,Value2A),
        %val1emptyorvalsequal(Value2,Value2A),
        putvalue_equals4(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_length],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_length],[Value1,variable]])),!.

interpretpart(ceiling,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("ceiling",Dbw_ceiling),
       getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_ceiling],[Value1,variable]]),
	((isval(Value1),
   ceiling(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_ceiling],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_ceiling],[Value1,variable]])),!.

interpretpart(round,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("round",Dbw_round),
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_round],[Value1,variable2]]),
        ((Value2A is round(Value1),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_round],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_round],[Value1,variable2]])),!.                        	

interpretpart(string_from_file,Variable1,Variable2,Vars1,Vars2) :-  
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("string_from_file",Dbw_string_from_file),

        getvalues(Variable1,Variable2,Value1,Value2,Vars1),

        debug_call(Skip,[[Dbw_n,Dbw_string_from_file],[variable,Value2]]),
	%%A=..[a,1]
	((phrase_from_file_s(string_g(String00a),Value2),
	string_codes(Value1A,String00a),
%%interpretstatement1(Functions0,Functions,[[Value1,Value2]],Vars1,Vars2,true,nocut),
        
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable2,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_string_from_file],[Value1A,Value2]])
;     debug_fail(Skip,[[Dbw_n,Dbw_string_from_file],[variable,Value2]])),!.                        	


interpretpart(maplist,Functions0,Functions,Variable1,Variable2,Variable3,Variable4,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("maplist",Dbw_maplist),

        getvalue(Variable1,Value1,Vars1),
        getvalues_equals4(Variable2,Variable3,Variable4,Value2,Value3,_Value4,Vars1),

        debug_call(Skip,[[Dbw_n,Dbw_maplist],[Value1,Value2,Value3,variable]]),
	%%A=..[a,1]
	((
	map(Functions0,Functions,Value1,Value2,Value3,Value4A,Vars1),

%%interpretstatement1(Functions0,Functions,[[Value1,Value2]],Vars1,Vars2,true,nocut),
        
        %val1emptyorvalsequal(Value4,Value4A),
        putvalue_equals4(Variable4,Value4A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_maplist],[Value1,Value2,Value3,Value4A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_maplist],[Value1,Value2,Value3,variable]])),!.                        	

interpretpart(string_length,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("string_length",Dbw_string_length),
       getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_string_length],[Value1,variable]]),
	((string(Value1),
   string_length(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_string_length],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_string_length],[Value1,variable]])),!.

interpretpart(sort,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("sort",Dbw_sort),
       getvalues_equals4(Variable1,Variable2,Value1,_Value2,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_sort],[Value1,variable]]),
	((is_list(Value1),
   sort(Value1,Value2A),
        %val1emptyorvalsequal(Value2,Value2A),
        putvalue_equals4(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_sort],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_sort],[Value1,variable]])),!.

interpretpart(intersection,Variable1,Variable2,Variable3,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("intersection",Dbw_intersection),
       getvalues_equals4(Variable1,Variable2,Variable3,Value1,Value2,_Value3,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_intersection],[Value1,Value2,variable]]),
	((is_list(Value1),is_list(Value2),
   intersection(Value1,Value2,Value3A),
        %val1emptyorvalsequal(Value3,Value3A),
        putvalue_equals4(Variable3,Value3A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_intersection],[Value1,Value2,Value3A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_intersection],[Value1,Value2,variable]])),!.


interpretpart(read_string,Variable1,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("read_string",Dbw_read_string),
       getvalue(Variable1,Value1,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_read_string],[variable]]),
	((read_string(user_input, "\n", "\r", _End1, Value1A),
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_read_string],[Value1A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_read_string],[variable]])),!.

interpretpart(writeln,Variable1,Vars1,Vars1) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("writeln",Dbw_writeln),
       %getvalue(Variable1,Value1,Vars1),
get_lang_word("v",Dbw_v),
        
        
debug_call(Skip,[[Dbw_n,Dbw_writeln],[variable]]),
	((%writeln(Value1)
	
	remember_and_turn_off_debug(Debug),
	%%trace,
find_findall_sys(Findall_sys_name),
        interpretpart(match4,Variable1,[Dbw_v,Findall_sys_name],Vars1,Vars3,_),
%%writeln1(        interpretpart(match4,Variable1,[v,sys1],Vars3,Vars2,_)),
%%interpretstatement1(Functions0,Functions,[[n,equals4],[Variable1,Variable3]],Vars3,Vars2,true,nocut),
	getvalue([Dbw_v,Findall_sys_name],Value3,Vars3),
	
	 turn_back_debug(Debug),
	
 writeln(Value3)
	
        %val1emptyorvalsequal(Value1,Value1A),
        %putvalue(Variable1,Value1A,Vars1,Vars2)
        )->
      debug_exit(Skip,[[Dbw_n,Dbw_writeln],[Value3]])
;     debug_fail(Skip,[[Dbw_n,Dbw_writeln],[variable]])),!.


interpretpart(atom_string,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("atom_string",Dbw_atom_string),
       getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        (isvar(Variable1)->
        (debug_call(Skip,[[Dbw_n,Dbw_atom_string],[variable,Value2]]),
	((string(Value2),
   atom_string(Value1A,Value2),
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_atom_string],[Value1A,Value2]])
;     debug_fail(Skip,[[Dbw_n,Dbw_atom_string],[variable,Value2]])));

        (debug_call(Skip,[[Dbw_n,Dbw_atom_string],[Value1,variable]]),
	((atom(Value1),
   atom_string(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_atom_string],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_atom_string],[Value1,variable]]))))
,!.

interpretpart(get_lang_word,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("get_lang_word",Dbw_get_lang_word),
       getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_get_lang_word],[Value1,variable]]),
	((%is_list(Value1),
	get_lang_word(Value1,Value2A1),
	string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_get_lang_word],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_get_lang_word],[Value1,variable]])),!.

/**
A,B,x*
A,x,B
x,A,B
A,x,y
x,A,y
x,y,A
x,y,z
A,B,C
**/

/**
interpretpart(stringconcat1,Terminal,Phrase2,Phrase1,Vars1,Vars2) :-
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("stringconcat1",Dbw_stringconcat),
isvar(Terminal),
isvar(Phrase2),
       getvalues(Terminal,Phrase2,Phrase1,Value1,Value2,Value3,Vars1),

debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[variable1,variable2,Value2]]),
	((string(Value3),
   string_concat(Value1A,Value2A,Value3),
        val1emptyorvalsequal(Value1,Value1A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Terminal,Value1A,Vars1,Vars3),
        putvalue(Phrase2,Value2A,Vars3,Vars2)),
      debug_exit(Skip,[[Dbw_n,Dbw_stringconcat],[Value1A,Value2A,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_stringconcat],[variable1,variable2,Value3]])
).%,!.
**/

interpretpart(stringconcat,Terminal,Phrase2,Phrase1,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("stringconcat",Dbw_stringconcat),
	%%Variables1=[Terminal,Phrase1,Phrase2], %% terminal can be v or "a"
        ((getvalues2([Terminal,Phrase1,Phrase2],
        	[],[TerminalValue1,Phrase1Value1,Phrase2Value1],Vars1,[],[Flag1,Flag2,_Flag3]), %% prolog vars, list of vars, [v]=[prolog var]
        %%delete(Value1,Value2,Value3A),
        (Terminal=[_Value]->TerminalValue2=[TerminalValue1];TerminalValue2=TerminalValue1),
                
(Terminal=""->(TerminalValue2="",
       
string_concat(TerminalValue2,Phrase2Value1,Phrase1Value1))->true;
            ((var(TerminalValue2)->(string_concat(TerminalValue2,Phrase2Value1,Phrase1Value1)),string_length(TerminalValue2,1)
            );string_concat(TerminalValue2,Phrase2Value1,Phrase1Value1))),
                
        putvalue(Terminal,TerminalValue2,Vars1,Vars3),
        putvalue(Phrase2,Phrase2Value1,Vars3,Vars4),
        putvalue(Phrase1,Phrase1Value1,Vars4,Vars2),
        (Flag1=true->TerminalValue3=variable1;TerminalValue3=TerminalValue1),
        (Flag2=true->Phrase1Value3=variable2;Phrase1Value3=Phrase1Value1))->

(debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[TerminalValue3,Phrase1Value3,Phrase2]]),	

debug_exit(Skip,[[Dbw_n,Dbw_stringconcat],[TerminalValue1,Phrase1Value1,Phrase2Value1]])
        	);
        	
        	(debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[variable1,variable2,variable3]]),
        	debug_fail(Skip,[[Dbw_n,Dbw_stringconcat],[variable1,variable2,variable3]])
        	)).%!.

        	

interpretpart(grammar_part,Variables1,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
%get_lang_word("grammar_part",Dbw_grammar_part),

	Variables1=[Terminal,Phrase1,Phrase2], %% terminal can be v or "a"
        %%terminal(Terminal),
        getvalues2([Terminal,Phrase1,Phrase2],
        	[],[TerminalValue1,Phrase1Value1,Phrase2Value1],Vars1,[],[Flag1,Flag2,_Flag3]), %% prolog vars, list of vars, [v]=[prolog var]
        %%delete(Value1,Value2,Value3A),
    ((    (Terminal=[_Value]->TerminalValue2=[TerminalValue1];TerminalValue2=TerminalValue1),



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
        	(debug_call(Skip,[[Dbw_n,grammar_part],[TerminalValue3,Phrase1Value3,Phrase2]]),
        	debug_exit(Skip,[[Dbw_n,grammar_part],[TerminalValue1,Phrase1Value1,Phrase2Value1]]));

% CAW requires input,input,output with "a","ab",[v,a] where [v,a]="b"
        	(debug_call(Skip,[[Dbw_n,grammar_part],[Terminal,Phrase1,Phrase2]]),
        (debug_fail(Skip,[[Dbw_n,grammar_part],[Terminal,Phrase1,Phrase2]])))),!.
        	

        	

getvalues(Variable1,Variable2,Value1,Value2,Vars) :-
        getvalue(Variable1,Value1,Vars),
        getvalue(Variable2,Value2,Vars).
getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars) :-
        getvalue(Variable1,Value1,Vars),
        getvalue(Variable2,Value2,Vars),
        getvalue(Variable3,Value3,Vars).
        /**
getvalues2(Variable1,Variable2,Value1,Value2,Vars) :-
        getvalue2(Variable1,Value1,Vars),
        getvalue2(Variable2,Value2,Vars).
getvalues2(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars) :-
        getvalue2(Variable1,Value1,Vars),
        getvalue2(Variable2,Value2,Vars),
        getvalue2(Variable3,Value3,Vars).
**/
%val1emptyorvalsequal([],_Value) :- !.
val1emptyorvalsequal(empty,_Value) :- !.
val1emptyorvalsequal(Value,Value) :-
	not(Value=empty).
val1emptyorvalsequal([Value1|Value1a],[Value2|Value2a]) :-
	val1emptyorvalsequal(Value1,Value2),
	val1emptyorvalsequal(Value1a,Value2a),!.
isop(Is):-get_lang_word("is",Is1),Is1=Is,!.
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


%% bc in a=bc
%%: if doesn't contain "|" in first level, then match4 list x, terminal


getvalue_match(Variable1,Value1,Vars1) :-
not(Variable1="|"),
	single_item(Variable1),
	getvalue(Variable1,Value1,Vars1),!.
getvalue_match([],[],_Vars1) :- !.
getvalue_match(Variable1,Value1,Vars1) :-
not(member("|",Variable1)),
	not(single_item(Variable1)),
	Variable1=[Variable1a|Variable1b],
	getvalue_match(Variable1a,Value1a,Vars1),
	getvalue_match(Variable1b,Value1b,Vars1),
	append([Value1a],Value1b,Value1),!.
	
getvalue_match_pipe([],[],_Vars1) :- !.
getvalue_match_pipe(Variable1,Value1,Vars1) :-
	variable_name(Variable1),
	getvalue(Variable1,Value1,Vars1),
	not(Value1=empty),!.


getvalue_match_pipe(Variable1,Value1,Vars1) :- %%,Top_flag

	(variable_name(Variable1)->
	(getvalue(Variable1,Value1,Vars1),
	not(Value1=empty))),

	split_into_head_and_tail(Variable1,Head1a,Tail1a,Pipe1,Head_is_list_of_lists1),
	(single_item(Head1a) -> L1 = 1 ; length(Head1a,L1)),
	(%%trace,
	(Head_is_list_of_lists1=true)->(
	%%writeln(here1),
		Head1=Head1a,Tail1=Tail1a,
		%%notrace,
		%%trace,
	getvalue_match_pipe(Head1,Value11a,Vars1),
	getvalue_match_pipe(Tail1,Value11b,Vars1),
	not(Value11a=empty),(Value11b=empty->fail;Value11c=Value11b),
	is_list(Value11c),
	append(Value11a,Value11c,Value1)
	%%[Value3]=Value5,Value4=[Value6|Value6a],
	%%maplist(append,[[Value5,Value6,Value6a]],Value2)
	%%,notrace
	);

	((Pipe1=true)->
		(split_by_number_of_items(Variable1,L1,_Head2,_Tail2),
		Head1=Head1a,Tail1=Tail1a);
	%%((Pipe1=false,Pipe2=true)->
	%%	(split_by_number_of_items(Variable1,L2,Head1,Tail1),
	%%	Head2=Head2a,Tail2=Tail2a);
	(Pipe1=false)->%%,Pipe2=false,L1=L2,
		Head1=Head1a,Tail1=Tail1a),
		%%Head2=Head2a,Tail2=Tail2a))
	 % *1
	%%trace,
	%%writeln(here2),
	getvalue_match_pipe(Head1,Value12a,Vars1),
	getvalue_match_pipe(Tail1,Value12b,Vars1),
	not(Value12a=empty),(Value12b=empty->fail;Value12c=Value12b),
	is_list(Value12c),
	%%trace,
	append([Value12a],Value12c,Value1)),!.

getvalue_match_pipe([Variable1|Variable1b],Value1,Vars1) :-
%%variable_name(Variable1),
	(variable_name(Variable1)->
	(getvalue(Variable1,Value1,Vars1),
	not(Value1=empty))),
	
	(variable_name(Variable1b)->
	(getvalue(Variable1b,Value1b,Vars1),
	not(Value1b=empty))),

	getvalue_match_pipe(Variable1,Value11a,Vars1),
	getvalue_match_pipe(Variable1b,Value11b,Vars1),
	not(Value11a=empty),(Value11b=empty->Value11c=[];Value11c=Value11b),
	is_list(Value11c),
	append([Value11a],Value11c,Value1),!.

% for check arguments

match4_21(Variable1,Variable2,Vars1,Vars2) :-
	%replace_vars(Variable1,[],Variable1a,[],First_vars1),
	%replace_vars(Variable2,[],Variable2a,[],_First_vars2),
	%append(First_vars1,First_vars2,First_vars3),
	match4_22(Variable1,Variable2,Vars1,Vars2),
	%replace_first_vars1(Vars3,First_vars1,[],Vars2),
	!.

% for intra-predicate equals4

match4_2(Variable1,Variable2,Vars1,Vars2) :-
	match4_22(Variable1,Variable2,Vars1,Vars2),!.

match4_22(Variable1,Variable2,Vars1,Vars2) :-
	match4_10(Variable1,Variable2,Vars1,Vars2),!.
match4_22(Variable1,Variable2,Vars1,Vars2) :-
	match4(Variable1,Variable2,Vars1,Vars2),!.


match4_10(Variable1,Variable2,Vars1,Vars2) :-
	%%interpretpart(match4,Variable1,[v,sys2],Vars1,Vars3,_),
%%	getvalue([v,sys2],Value1,Vars3))),

	not(variable_name(Variable2)),
	is_list(Variable2),
	%%findall(Value2,(member(A,Variable2),getvalue(A,Value2,Vars1)),X),
	getvalue_match(Variable2,X,Vars1),
	
%%trace,
	match4(Variable1,X,Vars1,Vars2),!.
	
match4_10(Variable1,Variable2,Vars1,Vars2) :-
%%trace,
	not(variable_name(Variable1)),
	is_list(Variable1),
	%%findall(Value1,(
	
	%%interpretpart(match4,Variable1,[v,sys1],Vars1,Vars3,_),
	%%getvalue([v,sys1],Value1,Vars3)
	
	getvalue_match(Variable1,X,Vars1),

	match4(X,Variable2,Vars1,Vars2),!.

	

match4(Variable1,Variable2,Vars1,Vars2) :-
%%trace,
	variable_name(Variable2),
	getvalue(Variable2,Value2,Vars1),
	not(variable_name(Variable1)),
	is_list(Variable1),
	%%findall(Value1,(
	
	%%interpretpart(match4,Variable1,[v,sys1],Vars1,Vars3,_),
	%%getvalue([v,sys1],Value1,Vars3)
	
	getvalue_match(Variable1,X,Vars1),
	%%member(A,Variable1),getvalue(A,Value1,Vars1)
	%%),X),
	val1emptyorvalsequal(Value2,X),
	putvalue(Variable2,X,Vars1,Vars2),
	length(Variable1,L),length(X,L),!.
match4(Variable1,Variable2,Vars1,Vars2) :-
	variable_name(Variable1),
	getvalue(Variable1,Value1,Vars1),
	not(variable_name(Variable2)),
	is_list(Variable2),
	%%findall(Value2,(member(A,Variable2),getvalue(A,Value2,Vars1)),X),
	getvalue_match(Variable2,X,Vars1),
	val1emptyorvalsequal(Value1,X),
	putvalue(Variable1,X,Vars1,Vars2),
	length(Variable2,L),length(X,L),!.


match4(Variable1,Variable2,Vars1,Vars2) :-
%%trace,
	match4_list(Variable1,Variable2,Vars1,Vars2),!.
match4(Variable1,Variable2,Vars1,Vars2%%,Top_flag
) :-
	split_into_head_and_tail(Variable1,Head1a,Tail1a,Pipe1,Head_is_list_of_lists1),
	(single_item(Head1a) -> L1 = 1 ; length(Head1a,L1)),
	split_into_head_and_tail(Variable2,Head2a,Tail2a,Pipe2,Head_is_list_of_lists2),
	(single_item(Head2a) -> L2 = 1 ; length(Head2a,L2)),
	(%%trace,
	(Head_is_list_of_lists1=true->true;Head_is_list_of_lists2=true)->(
	%%writeln(here1),
		Head1=Head1a,Tail1=Tail1a,
		Head2=Head2a,Tail2=Tail2a,%%notrace,
		%%trace,
	match4_list(Head1,Head2,Vars1,Vars3),
	match4(Tail1,Tail2,Vars3,Vars2)
	%%[Value3]=Value5,Value4=[Value6|Value6a],
	%%maplist(append,[[Value5,Value6,Value6a]],Value2)
	%%,notrace
	);

	((Pipe1=true,Pipe2=false)->
		(split_by_number_of_items(Variable2,L1,Head2,Tail2),
		Head1=Head1a,Tail1=Tail1a);
	((Pipe1=false,Pipe2=true)->
		(split_by_number_of_items(Variable1,L2,Head1,Tail1),
		Head2=Head2a,Tail2=Tail2a);
	(Pipe1=false,Pipe2=false,L1=L2,
		Head1=Head1a,Tail1=Tail1a),
		Head2=Head2a,Tail2=Tail2a))
	, % *1
	%%trace,
	%%writeln(here2),
	match4_list(Head1,Head2,Vars1,Vars3),
	match4(Tail1,Tail2,Vars3,Vars2)
	%%,notrace
	%%(Top_flag=true->(trace,[Value3]=Value5);Value3=Value5),(Value4=[]->(Value6=[],Val6a=[]);[Value6|Val6a]=Value4),
	
	%%maplist(append,[[Value1,Value5]],[Value2a]),
	%%(Top_flag=true->(append(Value2a,Value6,Value61),
	%%append(Value61,Val6a,Value2));maplist(append,[[Value2a,Value6,Val6a]],[Value2]))%%,
	%%append(Value2a,Value4,Value2)
	)
	%%,notrace
	,!.

split_into_head_and_tail(Variable,Head1c,Tail1c,Pipe,Head_is_list_of_lists) :-
%writeln1(split_into_head_and_tail(Variable,Head1c,Tail1c,Pipe,Head_is_list_of_lists)),

not(variable_name(Variable)),
	(findall(_FA,member("|",Variable),FA2),length(FA2,FA3),FA3=<1),
	%%Variable=[[v, a], "|", [v, d]]->trace,%%((
	(((
	append(Head2,["|"|Tail2],Variable) %%-> notrace;notrace)
	),
	(is_list(Head2),head_is_list_of_lists(Head2,Head_is_list_of_lists),(length(Head2,1) -> Head2=[Head1] ; 
		Head2=Head1)),%%trace,
		Tail2=[Tail1],Pipe=true)->true;
	%%(
	((is_list(Variable),not(variable_name(Variable)),
	Variable=[Head1|Tail1],Pipe=false,head_is_list_of_lists(Head1,Head_is_list_of_lists))->true;
	(Head1=Variable,Tail1=[],Pipe=false,head_is_list_of_lists(Head1,Head_is_list_of_lists)))),
	(Head1=empty->Head1c=[];Head1=Head1c),
	(Tail1=empty->Tail1c=[];Tail1=Tail1c),!.
	%%(.%%->true;
	%%([Head]=Variable,Tail=[]))).
	
%% [a,b|c]=[A|B].
%%A = a,
%%B = [b|c].
%% not supported, in future.

%% *1 only accept multiple items in head when other is |-less list, otherwise same number of items in head
%% ?- [1,2,3]=[A,B|C].
%% A = 1,
%% B = 2,
%% C = [3].

%% what about - v
/**
?- [[A],C|B]=[[1],2,3,4].
A = 1,
C = 2,
B = [3, 4].
**/

%% need to detect if head is a compound, flag and process it
head_is_list_of_lists(Head2,true) :-
get_lang_word("v",Dbw_v),
	%%trace,
	([Head3]=Head2->true;(%%notrace,
	fail)),findall(A,
	(member(A,Head3),((A=[v,_] -> true; ((A=[Dbw_v,_] -> true;is_list(A)))))),B),
	%%trace,writeln(Head2).%%
	is_list(Head3),length(Head3,L),length(B,L),!.
head_is_list_of_lists(_,false) :- !.


split_by_number_of_items(List,N2,List10,List2) :-
	%%N2 is N1-1,
	length(List1,N2),
	append(List1,List2,List),
	(List1=[_] -> List1=[List10] ; List1=List10),!.
	
match4_list([],[],Vars,Vars) :- !.

match4_list(Head1,Head2,Vars1,Vars2) :-
	not(variable_name(Head1)),
	not(variable_name(Head2)),
	Head1=[Head1a|Head1b],
	Head2=[Head2a|Head2b],
	not(Head1a="|"),
	not(Head2a="|"),
	match4(Head1a,Head2a,Vars1,Vars3%%,false
	),
	match4_list(Head1b,Head2b,Vars3,Vars2),!.
match4_list(Head1,Head2,Vars1,Vars2) :-
%%trace,
	%%single_item(Head1),
	%%single_item(Head2),
	match4_terminal(Head1,Head2,Vars1,Vars2),!.%%,
	%%append(Value1,[Value3],Value2).


match4_list(Head1,Head2,Vars1,Vars2) :-
	variable_name(Head1),
	not(variable_name(Head2)),
	not(Head2="|"),
	getvalue(Head1,Value1,Vars1),not(Value1=empty),
	match4(Value1,Head2,Vars1,Vars2),!.
match4_list(Head1,Head2,Vars1,Vars2) :-
	not(variable_name(Head1)),
	not(Head1="|"),
	variable_name(Head2),
	getvalue(Head2,Value2,Vars1),not(Value2=empty),
	match4(Head1,Value2,Vars1,Vars2),!.
match4_list(Head1,Head2,Vars1,Vars2) :-
	variable_name(Head1),
	variable_name(Head2),
	getvalue(Head1,Value1,Vars1),not(Value1=empty),
	getvalue(Head2,Value2,Vars1),not(Value2=empty),
	match4(Value1,Value2,Vars1,Vars2),!.
	
match4_list(Head1,Head2,Vars1,Vars2) :-
	Head1=["|"|[Head1a]],
	Head2=["|"|[Head2a]],
	match4(Head1a,Head2a,Vars1,Vars2),!.
match4_list(Head1,Head2,Vars1,Vars2) :-
	Head1=["|"|[Head1a]],
	not(Head2=["|"|_]),
	match4(Head1a,Head2,Vars1,Vars2),!.
match4_list(Head1,Head2,Vars1,Vars2) :-
	Head2=["|"|[Head2a]],
	not(Head1=["|"|_]),
	match4(Head1,Head2a,Vars1,Vars2),!.

match4_terminal([],[],Vars,Vars) :- !.
match4_terminal(Variable1,Variable2,Vars1,Vars2) :-
	%%is_list(Variable1),length(Variable1,1),
	%%is_list(Variable2),length(Variable2,1),
	%%trace,
	[Variable1a]=Variable1,
	[Variable2a]=Variable2,
	single_item(Variable1a),
	single_item(Variable2a),
	%%notrace,
match4_terminal(Variable1a,Variable2a,Vars1,Vars2),!.%%,
	%%append(Value1,[[Value3]],Value2).

match4_terminal(Variable1,Variable2,Vars1,Vars2) :-%%trace,
	%%single_item(Variable1), %% may be [1,2]
	%%single_item(Variable2),
   getvalues(Variable1,Variable2,Value1,Value2,Vars1),
   %% what if there is a var in a compound term? - may need different code in getvalues
        ((Value1A = Value2,
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2)%%bracket_if_single(Value1A,Value1A2),
        %%append11(Value1a,[Value1A],Value2a)
        )->true;
        ((Value2A = Value1,
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2)%%,%%bracket_if_single(Value2A,Value2A2),
        %%append11(Value1a,[Value2A],Value2a)
        )->true;
	     fail
	     %% assumes either or both A and B in A=B are instantiated, 
	     %% can be changed later.
	     )),!.

bracket_if_single(Value1A,Value1A) :-
	is_list(Value1A),!.
bracket_if_single(Value1A,[Value1A]) :-
	single_item(Value1A),!.
	
single_item(A) :- predicate_or_rule_name(A),!.
single_item(A) :- variable_name(A),!.
single_item(A) :- A="|",fail,!.
single_item(A) :- string(A),!.
single_item(A) :- number(A),!.
%single_item(A) :- atom(A),!.
%single_item([A,B]) :- atom(A),atom(b),!.

is_value_match(A) :- predicate_or_rule_name(A),!.
is_value_match(A) :- A="|",fail,!.
is_value_match(A) :- string(A),!.
is_value_match(A) :- number(A),!.
%is_value_match(A) :- atom(A),!.
%is_value_match([A,B]) :- atom(A),atom(b),!.

append11(empty,A,A) :- !.
append11(A,B,C) :- append(A,B,C).

/** match4

from bracketed head:

match4([[[v,a],[v,c]],"|",[v,b]],[[1,2],3,4],[],V).          V = [[[v, a], 1], [[v, c], 2], [[v, b], [3, 4]]].

                                                              match4([[v,a],"|",[v,b]],[1,2,3,4],[],V).
V = [[[v, a], 1], [[v, b], [2, 3, 4]]].

match4([[v,a],[v,c],"|",[v,b],[v,d]],[1,2,3,4],[],V).
should be false

match4([[[v,a]],[v,c],"|",[v,b]],[[1],2,3,4],[],V).
V = [[[v, a], 1], [[v, c], 2], [[v, b], [3, 4]]].


                                                              match4([[v,a],"|",[v,b]],[[1,2],3,4],[],V).
V = [[[v, a], [1, 2]], [[v, b], [3, 4]]].

match4([[[v,a],"|",[v,d]],[v,c],"|",[v,b]],[[1,5],2,3,4],[],V).
V = [[[v, a], 1], [[v, d], [5]], [[v, c], 2], [[v, b], [3, 4]]].

match4([[v,a],"|",[v,b]],[[1,2],3,4],[],V).
V = [[[v, a], [1, 2]], [[v, b], [3, 4]]].

match4([[v,a],"|",[[v,b]]],[1,2],[],V).                             V = [[[v, a], 1], [[v, b], 2]].

match4([[v,a]],[1],[],V).                                    
V = [[[v, a], 1]].

match4([[v,a],[v,b]],[1,2],[],V).                            
V = [[[v, a], 1], [[v, b], 2]].
                                                              match4([[v,a],[v,b]],[[1,3],2],[],V).
V = [[[v, a], [1, 3]], [[v, b], 2]] 

**/

map(_,_,_F,[],L,L,_).
map(Functions0,Functions,F,L,M1,N,Vars1):-
get_lang_word("v",Dbw_v),
get_lang_word("sys1",Dbw_sys1),
not((L=[])),L=[H|T],

	interpretstatement1(Functions0,Functions,[F,[M1,H,[Dbw_v,Dbw_sys1]]],Vars1,Vars2,true,nocut),
	getvalue([Dbw_v,Dbw_sys1],M2,Vars2),
	
%%(F,(M1,H,M2)),
map(Functions0,Functions,F,T,M2,N,Vars1).
