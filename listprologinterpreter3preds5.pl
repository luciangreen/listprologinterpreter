interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("is",Dbw_is),
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	not(is_empty(Value1)),
	expression(Value1),
	is_empty(Value2),
        val1emptyorvalsequal(Value2,Value1),
	%%isval(Value2),
debug_call(Skip,[[Dbw_n,Dbw_is],[Value1,variable]]),
(        putvalue(Variable2,Value1,Vars1,Vars2)->
debug_exit(Skip,[[Dbw_n,Dbw_is],[Value1,Value1]])
;     debug_fail(Skip,[[Dbw_n,Dbw_is],[Value1,variable]])),!.

interpretpart(is,Variable1,Variable2,Vars1,Vars2) :-
%writeln(here),
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("is",Dbw_is),
        getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        %%getvalue(Value1,Value1A,Vars1),
	%%isvalstr(Value1),
	%%isvalstr(Value1A),
	is_empty(Value1),
	not(is_empty(Value2)),
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
	((((Value2=""->true;is_empty(Value2))->Value1="";
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
        	
/*interpretpart(member,Variable1,Variable2,Vars1,Vars2) :-
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
*/
interpretpart(member2,Variable1,Variable2,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("member2",Dbw_member2),
get_lang_word("v",Dbw_v),

%trace,
        getvalues_equals4(Variable1,Variable2,Value1,Value2,Vars1),


	matrix_member(Matrix),findall(X,(member(Y,[Value1,Value2]),(contains_var([Dbw_v,_],Y)->X=o;X=i)),Z),
foldr(atom_concat,Z,'',W),(member(W,Matrix)->true;(writeln([incorrect,member2,modes,W]),abort)),

((W=ii->true;W=io)->	((%Value2=empty,
	((member(Value2a,Value1),
	debug_call(Skip,[[Dbw_n,Dbw_member2],[Value2,Value1]]),
	putvalue_equals4(Variable2,Value2a,Vars1,Vars2)))),
      debug_exit(Skip,[[Dbw_n,Dbw_member2],[Value2a,Value1]]));
      
      
      

(W=oi,%trace,	
%replace_in_term([Value2,],_%'$VAR'(_)
%        ,empty,Value1A1),
             command_n_sols(N),
%trace,
%writeln(Value2),
	findnsols(N,Value1A1,(member(Value2,Value1A),
        
        %Value1A=[Value3A2|Value3A3],
        %ValueIA1=[Value3A2,"|",Value3A3],
        
        find_v_sys(V_sys),
        	        	
%trace,

        replace_in_term(Value1A,_%'$VAR'(_)
        ,empty2,Value1A2),

        convert_to_lp_pipe(Value1A2,Value1A3),

        replace_in_term(Value1A3,empty2%'$VAR'(_)
        ,V_sys,Value1A1)
        
        )
        ,ValueA),!,
        
        
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        %Vars1=Vars2,
        member(Value1a,ValueA),
        putvalue_equals4(Variable1,Value1a,Vars1,Vars2),
        
        
        	debug_call(Skip,[[Dbw_n,Dbw_member2],[Value2,Value1]]),
      debug_exit(Skip,[[Dbw_n,Dbw_member2],[Value2,Value1a]]));
      
      
(W=oo->%**** change this


(command_n_sols(N),
	%findall([Vars2b,[Value1a,Value3a],Value1a,Value3a],(
	findnsols(N,%[Value1A2,
	Value2A2%]
	,(member(_Value1A,Value2A),
        %replace_in_term(Value1A,_%'$VAR'(_)
        %,empty,Value1A1),

        find_v_sys(V_sys),

        replace_in_term(Value2A,_%'$VAR'(_)
        ,empty2,Value2A4),
        
        %convert_to_lp_pipe(Value1A1,Value1A2),
        convert_to_lp_pipe(Value2A4,Value2A3),
        
        replace_in_term(Value2A3,empty2%'$VAR'(_)
        ,V_sys,Value2A2)
        
        )
        ,ValueA),!,
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        %Vars1=Vars2,
        member(%[Value1a,
        Value2a%]
        ,ValueA),
        %putvalue_equals4(Variable1,Value1a,Vars1,Vars3),%)->
        putvalue_equals4(Variable2,Value2a,Vars1,Vars2)
	,%,Vars2a),Vars2a=[[Vars2,_,Value1a,Value3a]|Vars2d],
		%findall([Vars2e,Vals2g],member([Vars2e,Vals2g,_,_],Vars2d),Vars2c1),
		
					%Vars2c=[[Dbw_n,Dbw_member2],[Value1,Value3],_,_,%,%Value2a
			%_,_,%[Value1,Value2a]
			%Vars2c1],
        	 debug_call(Skip,[[Dbw_n,Dbw_member2],[Value1,Value1]]),

      debug_exit(Skip,[[Dbw_n,Dbw_member2],[Value2a,Value1]]))))           
      .
%%;     %%debug_fail(Skip,[[n,member],[Value1,Value2]])),!.
%%		((debug(on)->(writeln1([fail,[[n,member],[Value1,value]],"Press c."]),(leash1(on)->true;(not(get_single_char(97))->true;abort)));true),fail))))).

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

	
	debug_call(Skip,[[Dbw_n,Dbw_member2],[Value2,Value1]])
	%putvalue(Variable1,Value1a,Vars1,Vars2)
	))),
      debug_exit(Skip,[[Dbw_n,Dbw_member2],[Value2,Value1a]])).

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
get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("equals4",Dbw_equals4),
%trace,
        debug_call(Skip,[[Dbw_n,Dbw_equals4],[Variable1,Variable2]]),
        %trace,
        remember_and_turn_off_debug(Debug),
        
        ((match4_new_22(Variable1,Variable2,Vars1,Vars2%,standard
        ), 

%match4_2(Variable1,Variable2,Vars1,Vars2),
        
        
        
	%trace,
find_sys(Sys_name),
        match4_new_22(Variable1,[Dbw_v,Sys_name],Vars2,Vars3%,standard
        ),
%%writeln1(        interpretpart(match4,Variable1,[v,sys1],Vars3,Vars2,_)),
%%interpretstatement1(ssi,Functions0,Functions,[[n,equals4],[Variable1,Variable3]],Vars3,Vars2,true,nocut),
	getvalue([Dbw_v,Sys_name],Value3,Vars3),
	
	 turn_back_debug(Debug))


        %%Value1A = Value2,
        %%((val1emptyorvalsequal(Value1,Value1A),
        %%putvalue(Variable1,Value1A,Vars1,Vars2))
        ->
      debug_exit(Skip,[[Dbw_n,Dbw_equals4],[Value3,Value3]])
;     (turn_back_debug(Debug),
debug_fail(Skip,[[Dbw_n,Dbw_equals4],[Variable1,Variable2]]))),!.                        	


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
%trace,
	getvalue_equals4(Variable1,Value11,Vars1),
	getvalue_equals4(Variable2,Value21,Vars1),
	getvalue_equals4(Variable3,Value31,Vars1),
	
append2(Dbw_n,Dbw_append,Variable1,Variable2,Variable3,Value11,Value21,Value31,Vars1,Vars2).	

append2(Dbw_n,Dbw_append,_Variable1,_Variable2,Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(1),
(contains_empty(Value31),not(contains_empty(Value11)),not(contains_empty(Value21))),
(
Value11=Value1,Value21=Value2,
debug_call(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,variable3]]),
        ((append1(Value1,Value2,Value3A),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable3,Value3A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,Value3A]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,variable3]])
)
)
).

append2(Dbw_n,Dbw_append,Variable1,Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(2),
%trace,
	(contains_empty(Value11),contains_empty(Value21),not(contains_empty(Value31))),
(
Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_append],[variable1,variable2,Value3]]),
        ((append1(Value1A,Value2A,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable1,Value1A,Vars1,Vars3),
        putvalue_equals4(Variable2,Value2A,Vars3,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1A,Value2A,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1A,Value2A,variable3]]))
%);
))).

append2(Dbw_n,Dbw_append,Variable1,_Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(3),
(contains_empty(Value11),not(contains_empty(Value21)),not(contains_empty(Value31))),
(
Value21=Value2,Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_append],[variable1,Value2,Value3]]),
        ((append1(Value1A,Value2,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable1,Value1A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1A,Value2,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[variable1,Value2,Value3]]))
%);
))).

append2(Dbw_n,Dbw_append,_Variable1,Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(4),
(contains_empty(Value21),not(contains_empty(Value11)),not(contains_empty(Value31))),
(
Value11=Value1,Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_append],[Value1,variable2,Value3]]),
        ((append1(Value1,Value2A,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable2,Value2A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1,Value2A,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1,variable2,Value3]]))
))).                        	

append2(Dbw_n,Dbw_append,_Variable1,_Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(5),
(not(contains_empty(Value21)),not(contains_empty(Value11)),not(contains_empty(Value31))),
(
Value11=Value1,Value21=Value2,Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,Value3]]),
        ((append1(Value1,Value2,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        Vars1=Vars2,
        %putvalue_equals4(Variable2,Value2A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1,variable2,Value3]]))
))).                        	

%oio
append2(Dbw_n,Dbw_append,Variable1,_Variable2,Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(5),
%trace,
(contains_empty(Value11),not(contains_empty(Value21)),contains_empty(Value31)),
(
%trace,
%Value11=Value1,
Value21=Value2,%Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_append],[_Value1,Value2,_Value3]]),
command_n_sols(N),
%trace,
        ((findnsols(N,[Value1A1,Value3A1],(append1(Value1A,Value2,Value3A),
        find_v_sys(V_sys1),
        replace_in_term(Value1A,_%'$VAR'(_)
        ,V_sys1,Value1A1),
        find_v_sys(V_sys2),
        replace_in_term(Value3A,_%'$VAR'(_)
        ,V_sys2,Value3A1))
        ,ValueA),!,
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        %Vars1=Vars2,
        member([Value1A,Value3A],ValueA),
        putvalue_equals4(Variable1,Value1A,Vars1,Vars3),%)->
        putvalue_equals4(Variable3,Value3A,Vars3,Vars2),%)->
        
        %putvalue_equals4(Variable2,Value2A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1A,Value2,Value3A]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1,variable2,Value3]]))
))).                        	

%ioo
append2(Dbw_n,Dbw_append,_Variable1,_Variable2,Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(5),
(not(contains_empty(Value11)),contains_empty(Value21),contains_empty(Value31)),
(
%trace,
%Value11=Value1,
Value11=Value1,%Value31=Value3,
Value21=Value2,
debug_call(Skip,[[Dbw_n,Dbw_append],[Value1,Value21,Value31]]),
%command_n_sols(N),
        ((%findnsols(N,[Value1A,Value3A],
        find_v_sys(V_sys),
        append1(Value1,_Value2A,Value3A),%ValueA),
        replace_in_term(Value3A,_%'$VAR'(_)
        ,V_sys,Value3A1),
        Value3A1=[Value3A2|Value3A3],
        Value3A4=[Value3A2,"|",Value3A3],
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        %Vars1=Vars2,
        %member([Value1A,Value3A],ValueA),
        putvalue_equals4(Variable3,Value3A4,Vars1,Vars2),%)->
        
        %putvalue_equals4(Variable2,Value2A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,Value3A4]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1,variable2,Value3]]))
))).                        	

%ooo
append2(Dbw_n,Dbw_append,Variable1,_Variable2,Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
get_lang_word("v",Dbw_v),
%writeln(5),
(contains_empty(Value11),contains_empty(Value21),contains_empty(Value31)),
(
%trace,
%Value11=Value1,
Value11=Value1,Value31=Value3,
Value21=Value2,
debug_call(Skip,[[Dbw_n,Dbw_append],[Value1,Value2,Value3]]),
command_n_sols(N),
%N=3,
%find_v_sys(V_sys),
replace_in_term(Value2,[Dbw_v,_],%'$VAR'(_)
        _,Value22),
        ((findnsols(N,[Value1A1,Value3A1],
(
        append1(Value1A,Value22,Value3A),
        find_v_sys(V_sys1),
        replace_in_term(Value1A,_%'$VAR'(_)
        ,empty2,Value1A2),
        %replace_in_term(Value2A,_%'$VAR'(_)
        %,empty,Value2A1),
        find_v_sys(V_sys2),
        replace_in_term(Value3A,_%'$VAR'(_)
        ,empty2,Value3A2),
        convert_to_lp_pipe(Value1A2,Value1A3),
        %convert_to_lp_pipe(Value2A2,Value2A1),
        convert_to_lp_pipe(Value3A2,Value3A3),
        
        replace_in_term(Value1A3,empty2%'$VAR'(_)
        ,V_sys1,Value1A1),

        replace_in_term(Value3A3,empty2%'$VAR'(_)
        ,V_sys2,Value3A1)

        ),ValueA),!,
        member([Value1a,Value3a],ValueA),

        %Value1A1=Value1a,
        %Value3A1=Value3a,
        %trace,
        putvalue_equals4(Variable1,Value1a,Vars1,Vars3b),%)->
        %putvalue_equals4(Variable2,Value2a,Vars3b,Vars3),%)->
        %trace,
        putvalue_equals4(Variable3,Value3a,Vars3b,Vars2),
        
      debug_exit(Skip,[[Dbw_n,Dbw_append],[Value1a,Value2,Value3a]]))
%;     debug_fail(Skip,[[Dbw_n,Dbw_append],[Value1,variable2,Value3]]))
)).      



      
      
      

interpretpart(stringconcat,Variable1,Variable2,Variable3,Vars1,Vars2) :-
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("stringconcat",Dbw_stringconcat),
        	%trace,
        %getvalues(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
	getvalue_equals4(Variable1,Value11,Vars1),
	getvalue_equals4(Variable2,Value21,Vars1),
	getvalue_equals4(Variable3,Value31,Vars1),
	
stringconcat2(Dbw_n,Dbw_stringconcat,Variable1,Variable2,Variable3,Value11,Value21,Value31,Vars1,Vars2).	

stringconcat2(Dbw_n,Dbw_stringconcat,_Variable1,_Variable2,Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(1),
(contains_empty(Value31),not(contains_empty(Value11)),not(contains_empty(Value21))),
(
Value11=Value1,Value21=Value2,
debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,Value2,variable3]]),
        ((string_concat(Value1,Value2,Value3A),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable3,Value3A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,Value2,Value3A]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,Value2,variable3]])
)
)
).

stringconcat2(Dbw_n,Dbw_stringconcat,Variable1,Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(2),
%trace,
	(contains_empty(Value11),contains_empty(Value21),not(contains_empty(Value31))),
(
Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[variable1,variable2,Value3]]),
        ((string_concat(Value1A,Value2A,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable1,Value1A,Vars1,Vars3),
        putvalue_equals4(Variable2,Value2A,Vars3,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_stringconcat],[Value1A,Value2A,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_stringconcat],[Value1A,Value2A,variable3]]))
%);
))).

stringconcat2(Dbw_n,Dbw_stringconcat,Variable1,_Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(3),
(contains_empty(Value11),not(contains_empty(Value21)),not(contains_empty(Value31))),
(
Value21=Value2,Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[variable1,Value2,Value3]]),
        ((string_concat(Value1A,Value2,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable1,Value1A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_stringconcat],[Value1A,Value2,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_stringconcat],[variable1,Value2,Value3]]))
%);
))).

stringconcat2(Dbw_n,Dbw_stringconcat,_Variable1,Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(4),
(contains_empty(Value21),not(contains_empty(Value11)),not(contains_empty(Value31))),
(
Value11=Value1,Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,variable2,Value3]]),
        ((string_concat(Value1,Value2A,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        putvalue_equals4(Variable2,Value2A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,Value2A,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,variable2,Value3]]))
))).                        	

stringconcat2(Dbw_n,Dbw_stringconcat,_Variable1,_Variable2,_Variable3,Value11,Value21,Value31,Vars1,Vars2) :-
%writeln(5),
(not(contains_empty(Value21)),not(contains_empty(Value11)),not(contains_empty(Value31))),
(
Value11=Value1,Value21=Value2,Value31=Value3,
debug_call(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,Value2,Value3]]),
        ((string_concat(Value1,Value2,Value3),
        %val1emptyorvalsequal(Value3,Value3A),
        %trace,
        Vars1=Vars2,
        %putvalue_equals4(Variable2,Value2A,Vars1,Vars2),%)->
      debug_exit(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,Value2,Value3]])
%;     debug_fail(Skip,[[Dbw_n,Dbw_stringconcat],[Value1,variable2,Value3]]))
))).                        	



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
%%interpretstatement1(ssi,Functions0,Functions,[[Value1,Value2]],Vars1,Vars2,true,nocut),
        
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable2,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_string_from_file],[Value1A,Value2]])
;     debug_fail(Skip,[[Dbw_n,Dbw_string_from_file],[variable,Value2]])),!.                        	

interpretpart(word1,Variable1,Vars1) :-  
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("word1",Dbw_word1),

        getvalue(Variable1,Value1,Vars1),

        debug_call(Skip,[[Dbw_n,Dbw_word1],[variable]]),
	%%A=..[a,1]
	((phrase(word1(Value1),_))->
	%phrase_from_file_s(string_g(String00a),Value2),
	%string_codes(Value1A,String00a),
%%interpretstatement1(ssi,Functions0,Functions,[[Value1,Value2]],Vars1,Vars2,true,nocut),
        
        %val1emptyorvalsequal(Value1,Value1A),
        %putvalue(Variable2,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_word1],[Value1]])
;     debug_fail(Skip,[[Dbw_n,Dbw_word1],[variable]])),!.                        	


interpretpart(maplist,Functions0,Functions,Variable1,Variable2,Variable3,Variable4,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("maplist",Dbw_maplist),

        getvalue(Variable1,Value1,Vars1),
        getvalues_equals4(Variable2,Variable3,Variable4,Value2,Value3,_Value4,Vars1),

        debug_call(Skip,[[Dbw_n,Dbw_maplist],[Value1,Value2,Value3,variable]]),
	%%A=..[a,1]
	((
	map(Functions0,Functions,Value1,Value2,Value3,Value4A,Vars1),

%%interpretstatement1(ssi,Functions0,Functions,[[Value1,Value2]],Vars1,Vars2,true,nocut),
        
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


interpretpart(text_area,Variable1,Variable2,Variable3,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("text_area",Dbw_text_area),
       getvalue(Variable1,Value1,Vars1),
       getvalue(Variable2,Value2,Vars1),
       getvalue(Variable3,Value3,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_text_area],[Value1,Value2,variable]]),
	((writeln(Value2),
	read_string(user_input, "\n", "\r", _End1, Value3A),
        val1emptyorvalsequal(Value3,Value3A),
        putvalue(Variable3,Value3A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_text_area],[Value3A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_text_area],[variable]])),!.

interpretpart(writeln,Variable1,Vars1,Vars1) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("writeln",Dbw_writeln),
       %getvalue(Variable1,Value1,Vars1),
get_lang_word("v",Dbw_v),
        
        
debug_call(Skip,[[Dbw_n,Dbw_writeln],[variable]]),
	((%writeln(Value1)
	
	remember_and_turn_off_debug(Debug),
	%%trace,
find_sys(Sys_name),
        interpretpart(match4,Variable1,[Dbw_v,Sys_name],Vars1,Vars3,_),
%%writeln1(        interpretpart(match4,Variable1,[v,sys1],Vars3,Vars2,_)),
%%interpretstatement1(ssi,Functions0,Functions,[[n,equals4],[Variable1,Variable3]],Vars3,Vars2,true,nocut),
	getvalue([Dbw_v,Sys_name],Value3,Vars3),
	
	 turn_back_debug(Debug),
	
 writeln0(Value3)
	
        %val1emptyorvalsequal(Value1,Value1A),
        %putvalue(Variable1,Value1A,Vars1,Vars2)
        )->
      debug_exit(Skip,[[Dbw_n,Dbw_writeln],[Value3]])
;     debug_fail(Skip,[[Dbw_n,Dbw_writeln],[variable]])),!.


interpretpart(atom_string,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("atom_string",Dbw_atom_string),
       getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        (contains_empty(Value1)->
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
	Value2A=Value2A1,
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_get_lang_word],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_get_lang_word],[Value1,variable]])),!.



interpretpart(shell,Variable1,Vars1,Vars1) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("shell",Dbw_shell),
       getvalue(Variable1,Value1,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_shell],[Value1]]),
	((%is_list(Value1),
	shell1_s(Value1)
	%Value2A=Value2A1,
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        %val1emptyorvalsequal(Value2,Value2A),
        %putvalue(Variable2,Value2A,Vars1,Vars2)
        )->
      debug_exit(Skip,[[Dbw_n,Dbw_shell],[Value1]])
;     debug_fail(Skip,[[Dbw_n,Dbw_shell],[Value1]])),!.



interpretpart(date_time_stamp,Y,M,D,Ho,Mi,Se,Se2,Variable,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("date_time_stamp",Dbw_date_time_stamp),

        getvalues(Y,M,D,YearValueA,MonthValueA,DayValueA,Vars1),
        getvalues(Ho,Mi,Se,HourValueA,MinuteValueA,SecondsValueA,Vars1),
        getvalues(Se2,Variable,Se2ValueA,ValueA,Vars1),
        
       %getvalue(Variable1,Value1,Vars1),
        debug_call(Skip,[[Dbw_n,Dbw_date_time_stamp],[YearValueA,MonthValueA,DayValueA,HourValueA,MinuteValueA,SecondsValueA,Se2ValueA,variable]]),
	((%is_list(Value1),
	date_time_stamp(date(YearValueA,MonthValueA,DayValueA,HourValueA,MinuteValueA,SecondsValueA,Se2ValueA,-,-),Value1A),
	%Value2A=Value2A1,
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(ValueA,Value1A),
        putvalue(Variable,Value1A,Vars1,Vars2)
        )->
      debug_exit(Skip,[[Dbw_n,Dbw_date_time_stamp],[YearValueA,MonthValueA,DayValueA,HourValueA,MinuteValueA,SecondsValueA,Se2ValueA,Value1A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_date_time_stamp],[YearValueA,MonthValueA,DayValueA,HourValueA,MinuteValueA,SecondsValueA,Se2ValueA,variable]])),!.

interpretpart(phrase_from_file,Variable1,In,Vars1,Vars2) :- 
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("phrase_from_file",Dbw_phrase_from_file),
       getvalue(Variable1,Value1,Vars1),
       getvalue(In,Value2,Vars1),
        %trace,

%trace,
        debug_call(Skip,[[Dbw_n,Dbw_phrase_from_file],[variable,Value2]]),
	((%is_list(Value1),
	phrase_from_file(string(Out),Value2),
	%shell1_s(Value1)
	%Value2A=Value2A1,
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(Value1,Out),
        putvalue(Variable1,Out,Vars1,Vars2)
        )->
      debug_exit(Skip,[[Dbw_n,Dbw_phrase_from_file],[Out,Value2]])
;     debug_fail(Skip,[[Dbw_n,Dbw_phrase_from_file],[variable,Value2]])),!.

/*
interpretpart(term_to_atom,Variable1,Variable2,Vars1,Vars2) :- 
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("term_to_atom",Dbw_term_to_atom),
       getvalues(Variable1,Variable2,Value1,Value2,Vars1),
        
((contains_empty(Value1),not(contains_empty(Value2)))->       
        
        (debug_call(Skip,[[Dbw_n,Dbw_term_to_atom],[variable,Value2]]),
	((%is_list(Value1),
	term_to_atom(Value1A,Value2),
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(Value1,Value1A),
        putvalue(Variable1,Value1A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_term_to_atom],[Value1A,Value2]])
;     debug_fail(Skip,[[Dbw_n,Dbw_term_to_atom],[variable,Value2]])));

(        (debug_call(Skip,[[Dbw_n,Dbw_term_to_atom],[Value1,variable]]),
	((%is_list(Value1),
	term_to_atom(Value1,Value2A),
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value2A),
        putvalue(Variable2,Value2A,Vars1,Vars2))->
      debug_exit(Skip,[[Dbw_n,Dbw_term_to_atom],[Value1,Value2A]])
;     debug_fail(Skip,[[Dbw_n,Dbw_term_to_atom],[Value1,variable]])))
))
,!.


interpretpart(split_on_substring117a,Variable1,Variable2,Variable3,Variable4,Vars1,Vars2) :- 
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("split_on_substring117a",Dbw_split_on_substring117a),
       getvalue(Variable1,Value1,Vars1),
       getvalue(Variable2,Value2,Vars1),
       getvalue(Variable3,Value3,Vars1),
       getvalue(Variable4,Value4,Vars1),
        %trace,

%trace,
        debug_call(Skip,[[Dbw_n,Dbw_split_on_substring117a],[Value1,Value2,Value3,variable]]),
	((%is_list(Value1),
	split_on_substring117a(Value1,Value2,Value3,Value41),
	%shell1_s(Value1)
	%Value2A=Value2A1,
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(Value4,Value41),
        putvalue(Variable4,Value41,Vars1,Vars2)
        )->
      debug_exit(Skip,[[Dbw_n,Dbw_split_on_substring117a],[Value1,Value2,Value3,Value41]])
;     debug_fail(Skip,[[Dbw_n,Dbw_split_on_substring117a],[Value1,Value2,Value3,variable]])),!.


interpretpart(string_strings,Variable1,Variable2,Vars1,Vars2) :- 
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("string_strings",Dbw_string_strings),
       getvalue(Variable1,Value1,Vars1),
       getvalue(Variable2,Value2,Vars1),
        %trace,

%trace,
        debug_call(Skip,[[Dbw_n,Dbw_string_strings],[Value1,variable]]),
	((%is_list(Value1),
	string_strings(Value1,Value21),
	%shell1_s(Value1)
	%Value2A=Value2A1,
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),
        val1emptyorvalsequal(Value2,Value21),
        putvalue(Variable2,Value21,Vars1,Vars2)
        )->
      debug_exit(Skip,[[Dbw_n,Dbw_string_strings],[Value1,Value21]])
;     debug_fail(Skip,[[Dbw_n,Dbw_string_strings],[Value1,variable]])),!.
*/

interpretpart(shell_pl,I0,QP0,QV0,P0,OVar0,Vars1,Vars2) :-
%trace,
% eg [I,QP,QV,P,OVar]=[[1,1],"a","A,A1","B is A+A1,B1 is A1-A1,write([B,B1]).",[v,o]]
 find_v_sys(V_sys),
 	remember_and_turn_off_debug(Debug),

 interpretpart(match4,I0,V_sys,Vars1,Vars3,_),
 
 	turn_back_debug(Debug),

 getvalue(V_sys,I1,Vars3),
 term_to_atom(I1,I),

       %getvalue(I0,I,Vars1),
       getvalue(QP0,QP,Vars1),
       getvalue(QV0,QV,Vars1),
       getvalue(P0,P,Vars1),
       getvalue(OVar0,OVar,Vars1),


get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("shell_pl",Dbw_shell_pl1),Dbw_shell_pl1=Dbw_shell_pl,

debug_call(Skip,[[Dbw_n,Dbw_shell_pl],[I,QP,QV,P,variable]]),
       
((QV="",I1=[])->Mid=    [
    QP,",\n\t",
    "halt.\n\n","main :- halt(1).\n",
	QP, " :-","\n\t"];
	
	Mid=[    "[",QV,"]=",I,",","\n\t",
    QP,"(",QV,"),","\n\t",
    

	"halt.\n\n","main :- halt(1).\n",
	QP,"(",QV,") :-","\n\t"]),
   
flatten(["#!/usr/bin/swipl -f -q\n\n",%":-include('",Go_path5,File,"').\n",
":- initialization(catch(main, Err, handle_error(Err))).\n\nhandle_error(_Err):-\n  halt(1).\n\n","main :-\n\t",

    %"opt_arguments([], _, Args),","\n\t",
Mid,P],String1),

foldr(string_concat,String1,String),

foldr(string_concat,[%"../private2/luciancicd-testing/",Repository1b,"/",Go_path5,
"main.pl"],GP),
open_s(GP,write,S1),
write(S1,String),close(S1),

findall([" ",I1],member(I1,I),I2),flatten(I2,I3),foldr(string_concat,I3,I4),
foldr(string_concat,["chmod +x ",GP],S31),%,
%trace,
catch(bash_command(S31,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text41),%writeln1(Text4),
	fail%abort
 	)),
 foldr(string_concat,["swipl -f -q ./",GP,%" ",
I4],S3),%,
%trace,
((catch(bash_command(S3,VO), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text4),%writeln1(Text4),
	fail%abort
 	)),	term_to_atom(OVar1,VO))->
 	(        val1emptyorvalsequal(OVar,OVar1),
        
        
         	remember_and_turn_off_debug(Debug1),

find_sys(Sys_name),        interpretpart(match4,OVar0,[Dbw_v,Sys_name],Vars1,Vars2fa,_),
	getvalue([Dbw_v,Sys_name],Value3,Vars2fa),
	interpretpart(match4,OVar1,Value3,[]%Vars1
	,Vars2,_),
	 turn_back_debug(Debug1),
	 
        %putvalue(OVar0,OVar1,Vars1,Vars2),
	 
	         
debug_exit(Skip,[[Dbw_n,Dbw_shell_pl],[I,QP,QV,P,OVar1]]));
(debug_fail(Skip,[[Dbw_n,Dbw_shell_pl],[I,QP,QV,P,variable]]),fail)),!.

interpretpart(shell_c,I0,P0,OVar0,Vars1,Vars2) :-
%trace,
% eg [I,QP,QV,P,OVar]=[[1,1],"a","A,A1","B is A+A1,B1 is A1-A1,write([B,B1]).",[v,o]]
 %find_v_sys(V_sys),
 %interpretpart(match4,I0,V_sys,Vars1,Vars3,_),
 getvalue(I0,I1,Vars1),
 term_to_atom(I1,I),
 
open_s("input.txt",write,S1T),
write(S1T,I),close(S1T),
 

       %getvalue(I0,I,Vars1),
       %getvalue(QP0,QP,Vars1),
       %getvalue(QV0,QV,Vars1),
       %trace,
       getvalue(P0,P1,Vars1),
       %trace,
%atomic_list_concat(P1,'\n',P2),
%atomic_list_concat(P1,'\\n',P3),
%atomic_list_concat(P4,'\"',P3),
%atomic_list_concat(P4,'"',P),
atom_string(P,P1),
open_s("program.txt",write,S1C),
write(S1C,P),close(S1C),

foldr(string_concat,["cp program.txt program.c"],S32),%,
%trace,
catch(bash_command(S32,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42),%writeln1(Text4),
	fail%abort
 	)),

       
       getvalue(OVar0,OVar,Vars1),


get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("shell_c",Dbw_shell_c1),Dbw_shell_c1=Dbw_shell_c,

debug_call(Skip,[[Dbw_n,Dbw_shell_c],[I,P,variable]]),
       

foldr(string_concat,["gcc program.c"],S31),%,
%trace,
catch(bash_command(S31,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text43),%writeln1(Text4),
	fail%abort
 	)),
 foldr(string_concat,["./a.out<input.txt"],S3),%,
%trace,
((catch(bash_command(S3,VO), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text44),%writeln1(Text4),
	fail%abort
 	)),	term_to_atom(OVar1,VO))->
 	(        val1emptyorvalsequal(OVar,OVar1),
        putvalue(OVar0,OVar1,Vars1,Vars2),
debug_exit(Skip,[[Dbw_n,Dbw_shell_c],[I,P,OVar1]]));
(debug_fail(Skip,[[Dbw_n,Dbw_shell_c],[I,P,variable]]),fail)),!.

/*
 	bash_command1(Command, Output) :-
        process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out).
*/

interpretpart(command,Command1,Args,Variables,Vars1,Vars2) :- 
%trace,
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("v",Dbw_v1),Dbw_v1=Dbw_v,
get_lang_word(Command1,Dbw_command),
%trace,

findall(Value,(member(Variable,Variables),
getvalue(Variable,Value,Vars1)),Values),

/*
(length(Variables,0)->
(Variables=[],
       Values=[]);
(length(Variables,1)->
(Variables=[Variable1],
       getvalue(Variable1,Value1,Vars1),Values=[Value1]);
(length(Variables,2)->
(Variables=[Variable1,Variable2],
       getvalues(Variable1,Variable2,Value1,Value2,Vars1),
       Values=[Value1,Value2]);
(length(Variables,3)->
(Variables=[Variable1,Variable2,Variable3],
       getvalue(Variable1,Variable2,Variable3,Value1,Value2,Value3,Vars1),
    	Values=[Value1,Value2,Value3]))))),
*/
       
length(Variables,VL),
length(Args,VL),
numbers(VL,1,[],VLN),

% check modes of arguments
forall(member(VLN1,VLN),(get_item_n(Args,VLN1,Arg),
get_item_n(Values,VLN1,Val),
(Arg=o->contains_var([Dbw_v,_],Val);
not(contains_var([Dbw_v,_],Val))))),

findall(Debug_variable,(member(VLN1,VLN),
 get_item_n(Args,VLN1,Arg),get_item_n(Values,VLN1,Value),
 (Arg=i->Debug_variable=Value;Debug_variable=variable)),
 Debug_variables),
 
 length(Command_vars,VL),%=[A,B,C],
 
findall(Command_variable,(member(VLN1,VLN),
 get_item_n(Args,VLN1,Arg),get_item_n(Values,VLN1,Value),
 get_item_n(Command_vars,VLN1,Command_vars_val),
 (Arg=i->Command_variable=Value;
 Command_variable=Command_vars_val)),
 Command_variables),
  
         debug_call(Skip,[[Dbw_n,Dbw_command],Debug_variables]),
	((%is_list(Value1),
	string_atom(Command1,Command1_atom),
	%trace,
	functor(Command2,Command1_atom,VL),
	(length(Command_variables,0)->Command3=Command2;
	(arg2(VLN,Command2,Command_variables),Command2=Command3)),
	Command3,
	%string_atom(Value2A,Value2A1), % *** LPI only takes strings
   %sort(Value1,Value2A),

forall(member(VLN1,VLN),(get_item_n(Args,VLN1,Arg),
get_item_n(Command_vars,VLN1,Command_vars_n),
get_item_n(Values,VLN1,Value),
(Arg=o->val1emptyorvalsequal(Value,Command_vars_n);true))),

putvalues2(Args,Variables,Command_variables,Vars1,Vars2)
        %putvalue(Variable2,Value2A,Vars1,Vars2)
        ),%->
      debug_exit(Skip,[[Dbw_n,Dbw_command],Command_variables])
      ).
%;     debug_fail(Skip,[[Dbw_n,Dbw_command],Debug_variables])).

arg2([],_Command,_Args) :- !.
arg2([Arg_n|Arg_n2],Command,[Arg|Args]) :-
 arg(Arg_n,Command,Arg),
 arg2(Arg_n2,Command,Args).

putvalues2([],_Variables,_Command_vars,Vars,Vars) :- !.
putvalues2([Arg|Args],[Variable|Variables],[Command_var|Command_vars],Vars1,Vars2) :-
 (Arg=o->match4_new_22(Variable,Command_var,Vars1,Vars3);
 Vars1=Vars3),
 putvalues2(Args,Variables,Command_vars,Vars3,Vars2).
 

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

/*
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

        */	

interpretpart(grammar_part,Variables1,Vars1,Vars2) :-
get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
get_lang_word("grammar_part",Dbw_grammar_part),

	Variables1=[Terminal,Phrase1,Phrase2], %% terminal can be v or "a"
        %%terminal(Terminal),
        %trace,
grammar_part2(Dbw_n,Dbw_grammar_part,Terminal,Phrase1,Phrase2,Vars1,Vars2).


grammar_part2(_Dbw_n,_Dbw_grammar_part,Terminal,Phrase1,Phrase2,Vars1,Vars2) :-
%trace,

        getvalues2([Terminal,Phrase1,Phrase2],
        	[],[TerminalValue1,Phrase1Value1,Phrase2Value1],Vars1,[],[_Flag1,_Flag2,_Flag3]),
        	
(%(not(contains_var1(Terminal)),
is_list(TerminalValue1)->true;
(%(not(contains_var1(Phrase1)),
is_list(Phrase1Value1)->true;
(%(not(contains_var1(Phrase2)),
is_list(Phrase2Value1)))),

%getvalues(Terminal,Phrase1,Phrase2,TerminalValue1,Phrase1Value1,Phrase2Value1,Vars1),

interpretpart(append,Terminal,Phrase2,Phrase1,Vars1,Vars2).


grammar_part2(Dbw_n,_Dbw_grammar_part,Terminal,Phrase1,Phrase2,Vars1,Vars2) :-

        getvalues2([Terminal,Phrase1,Phrase2],
        	[],[TerminalValue1,Phrase1Value1,Phrase2Value1],Vars1,[],[Flag1,Flag2,_Flag3]), %% prolog vars, list of vars, [v]=[prolog var]
        %%delete(Value1,Value2,Value3A),

(string(TerminalValue1)->true;
(string(Phrase1Value1)->true;
(string(Phrase2Value1)))),
        
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
val1emptyorvalsequal(Empty,_Value) :- is_empty(Empty),!.
val1emptyorvalsequal(Value,Value) :-
	not(is_empty(Value)).
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

%append1([],Item,Item).
append1(Item11,Item21,Item31) :-
	
replace_empty_with_empty_set(	[Item11,Item21,Item31],[],[Item1,Item2,Item3]),
%%maplist(expression,[Item1,Item2,Item3]), %% commented out 21 8 19
/**((isvalstr(Item1),Item1A=[Item1]);(not(isvalstr(Item1)),Item1A=Item1)),
        ((isvalstr(Item2),Item2A=[Item2]);(not(isvalstr(Item2)),Item2A=Item2)),
        %%((isvalstr(Item3),Item3A=[Item3]);(not(isvalstr(Item3)),Item3A=Item3)),
        **/
	append(Item1,Item2,Item3).
/**delete1(Item1,Item2,Item3) :-
	((isvalstr(Item1),Item1A=[Item1]);(not(isvalstr(Item1)),Item1A=Item1)),
        ((isvalstr(Item2),Item2A=[Item2]);(not(isvalstr(Item2)),Item2A=Item2)),
        %%((isvalstr(Item3),Item3A=[Item3]);(not(isvalstr(Item3)),Item3A=Item3)),
	delete(Item1A,Item2A,Item3).
**/
replace_empty_with_empty_set([],A,A).
replace_empty_with_empty_set(A,B,C) :-
	A=[Item1|Items],
	(var(Item1)->Item2=Item1;(is_empty(Item1)->Item2=[];Item2=Item1)),
	append(B,[Item2],D),
	replace_empty_with_empty_set(Items,D,C).
removebrackets([[Value]],Value) :-!.
removebrackets(Value,Value).


%% bc in a=bc
%%: if doesn't contain "|" in first level, then match4 list x, terminal


	
single_item(A) :- predicate_or_rule_name(A),!.
single_item(A) :- variable_name(A),!.
single_item(A) :- A="|",fail,!.
single_item(A) :- string(A),!.
single_item(A) :- number(A),!.
single_item(A) :- blob(A,stream),!.

%single_item(A) :- atom(A),!.
%single_item([A,B]) :- atom(A),atom(b),!.

single_item_or_atom(A) :- predicate_or_rule_name(A),!.
single_item_or_atom(A) :- variable_name(A),!.
%single_item_or_atom(A) :- A="|",fail,!.
single_item_or_atom(A) :- string(A),!.
single_item_or_atom(A) :- number(A),!.
single_item_or_atom(A) :- atom(A),!.
single_item_or_atom(A) :- blob(A,stream),!.

is_value_match(A) :- predicate_or_rule_name(A),!.
is_value_match(A) :- A="|",fail,!.
is_value_match(A) :- string(A),!.
is_value_match(A) :- number(A),!.
is_value_match(A) :- blob(A,stream),!.
%is_value_match(A) :- atom(A),!.
%is_value_match([A,B]) :- atom(A),atom(b),!.

append11(Empty,A,A) :- is_empty(Empty),!.
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

	interpretstatement1(_,Functions0,Functions,[F,[M1,H,[Dbw_v,Dbw_sys1]]],Vars1,Vars2,true,nocut),
	getvalue([Dbw_v,Dbw_sys1],M2,Vars2),
	
%%(F,(M1,H,M2)),
map(Functions0,Functions,F,T,M2,N,Vars1).

