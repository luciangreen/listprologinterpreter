%symbol(Symbol,Symbol) :-!.

%%slp2lp_variables(Name1,[v,Name1]) :- predicate_or_rule_name(Name1),!.
%slp2lp_variables(Name,Name) :- !.

/**
collect_arguments_body(Body1,Body2) :-
	findall(*,(member(Statement1,Body1
collect_arguments_body(Body1,[],Body2) :-

**/

%%predicate_or_rule_name([A,B]) :- atom(A),is_list(B),!.
%predicate_or_rule_name([V_or_n,_Name]) :- (V_or_n=v->true;V_or_n=n),!.%%,atom(Name),!.
%% x: predicate_or_rule_name(V_or_n) :- (V_or_n=v->true;V_or_n=n),fail,!.

collect_arguments_body2([],N,N):-!.%%,Body3


%%collect_arguments_body2([],Body,Body) :- !.
collect_arguments_body2(Body1,Body2,Body3) :-
        Body1=[[Statements1|Statements1a]|Statements2
        ],
	
		not(predicate_or_rule_name(Statements1)),
			  %Number1a is Number1+1,
collect_arguments_body2([Statements1],Body2,Body4), %% 2->1

	collect_arguments_body2(Statements1a,Body4,Body5),
        collect_arguments_body2(Statements2,Body5,Body3),
    
%%	append([Body3,Body4],Body6),
%%	append([[Body6],Body5],Body2),
	

	!.



        
collect_arguments_body2(Body1,Body2,Body3) :-
get_lang_word("n",Dbw_n),
get_lang_word("not",Dbw_not),

        Body1=[[[Dbw_n,Dbw_not],Statement]|Statements2 %% [] removed from Statement
        ],
		  %Number1a is Number1+1,
        collect_arguments_body2([Statement],Body2,Body4),
        collect_arguments_body2(Statements2,Body4,Body3),
		  %append([Number1,%%*,
		  %[n,not]],Body3,Body5),
		  %append([Body5],Body4
     %   ,Body2),

	!.
	
	


collect_arguments_body2(Body1,Body2,Body3) :-
get_lang_word("n",Dbw_n),
get_lang_word("or",Dbw_or),
        Body1=[[[Dbw_n,Dbw_or],[Statements1,Statements2]]|Statements3],
		  %Number1a is Number1+1,
        collect_arguments_body2([Statements1],Body2,Body4),
        collect_arguments_body2([Statements2],Body4,Body5),
        collect_arguments_body2(Statements3,Body5,Body3),
        %append(Body3,Body4,Body34),
        %Body6=[Number1,[n,or],Body34
        %],
        %append([Body6],Body5,Body2),
        !.


collect_arguments_body2(Body1,Body2,Body3) :-
get_lang_word("n",Dbw_n),

        Body1=[[[Dbw_n,"->"],[Statements1,Statements2]]|Statements3],
		  %Number1a is Number1+1,
        collect_arguments_body2([Statements1],Body2,Body4), 
    	  collect_arguments_body2([Statements2],Body4,Body5),

        collect_arguments_body2(Statements3,Body5,Body3),
        %append(Body3,Body4,Body34),
        %Body6=[Number1,[n,"->"],Body34
        %],
        %append([Body6],Body5,Body2),

        !.




collect_arguments_body2(Body1,Body2,Body3) :-
get_lang_word("n",Dbw_n),
        Body1=[[[Dbw_n,"->"],[Statements1,Statements2,Statements2a]]|Statements3],
		  %Number1a is Number1+1,
        collect_arguments_body2([Statements1],Body2,Body4),
        collect_arguments_body2([Statements2],Body4,Body5),
                %%trace,
                collect_arguments_body2([Statements2a],Body5,Body6),
        collect_arguments_body2(Statements3,Body6,Body3),
        %append_list2([Body3,Body4,Body5],Body345),
        %Body7=[Number1,[n,"->"],Body345],        
        %append([Body7],Body6,Body2),
        !.


collect_arguments_body2(Body1,Body2,Body3) :-
	Body1=[Statement|Statements],
	not(predicate_or_rule_name(Statement)),
	collect_arguments_statement1(Statement,Body2,Body4),
	collect_arguments_body2(Statements,Body4,Body3),
   %append_list2([Result1,Result2],Body2),
   !.
   
collect_arguments_statement1(Statement,Arguments1,Arguments2) :-
get_lang_word("n",Dbw_n),

	((Statement=[[Dbw_n,_Name],Arguments],
	%trace,
	recursive_collect_arguments(Arguments,Arguments1,Arguments2)

%findall(Argument,(member(Argument,Arguments),variable_name(Argument)),Arguments3),
	%append(Arguments1,Arguments3,Arguments2)
	%Arguments=Result2,
	%findall(Argument,(member(Argument,Arguments),(predicate_or_rule_name(Argument))),Result2),
	%Result1=[[Number1,[n,Name],Result2]]
	)->true;
	(Statement=[[Dbw_n,_Name]],
	Arguments1=Arguments2)).
	
recursive_collect_arguments([],Arguments,Arguments) :- !.
recursive_collect_arguments(Statement,Arguments1,Arguments2) :-
	Statement=[Statement1|Statement2],
	(variable_name(Statement1)->append(Arguments1,[Statement1],Arguments3);(expression_not_var(Statement1)->Arguments1=Arguments3;
	recursive_collect_arguments(Statement1,Arguments1,Arguments3))),
	recursive_collect_arguments(Statement2,Arguments3,Arguments2).
%recursive_collect_arguments(Statement,Arguments1,Arguments2) :-
	%variable_name(Statement)->
	
%/*
occurs_check(Variable1,Variables2) :-
%trace,
 (occurs_check(on)->
 ((Variable1=Variables2->true;
 (occurs_check2([Variable1],[Variables2])->fail;true)));
 true).

 

	contains_var1(_,[]) :- fail.
	
contains_var1(Var,Statement) :-
	
(Var=Statement->true;	(Statement=[Statement1|Statement2],
	(Var=Statement1->true;
	(contains_var1(Var,Statement1)->true;
	contains_var1(Var,Statement2))))).
%*/

% if a=b(a) then fail
	occurs_check2([],[]) :- true.

occurs_check2(Variables1,Variables2) :-

(Variables1=Variables2->fail;

/*
((variable_name(Variables1),
not(variable_name(Variables2)),
contains_var1(Variables1,Variables2))->true;

(variable_name(Variables2),
not(variable_name(Variables1)),
contains_var1(Variables2,Variables1)))),
*/
	((Variables1=[Statement1a|Statement2a],
Variables2=[Statement1b|Statement2b]),
	(Statement1a=Statement1b->fail;
	
((variable_name(Statement1a),
not(variable_name(Statement1b)),
contains_var1(Statement1a,Statement1b))->true;

((variable_name(Statement1b),
not(variable_name(Statement1a)),
contains_var1(Statement1b,Statement1a))->true;

	
	occurs_check2(Statement1a,Statement1b)))),
	occurs_check2(Statement2a,Statement2b))).




contains_empty([]) :- fail.
contains_empty(Statement) :-
	
(is_empty(Statement)->true;	(Statement=[Statement1|Statement2],
	(is_empty(Statement1)->true;
	(contains_empty(Statement1)->true;
	contains_empty(Statement2))))).
	
	length_is_list(A,B) :-
 is_list(A),length(A,B),!.
 

