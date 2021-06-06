# List Prolog Documentation

* Load List Prolog by downloading the <a href="https://github.com/luciangreen/listprologinterpreter">repository from GitHub</a>.

* Please download and install <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine.

* Load the List Prolog Interpreter by typing:
`['listprolog'].`

* The interpreter is called in the form:
`interpret(Debug,Query,Functions,Result).`

Where:
Debug - on or off for trace,
Query - the query,
Functions - the algorithm,
Result - the result.

* For example:
```
interpret(off,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,Result).
```

The result of the query is:
`Result=[[[[v,c], 2]]]`  This is the list of non-deterministic results (i.e. ones that SWI-Prolog would return after pressing `";"`) containing the list of variable values. [] is returned if the predicate is false and [[]] is returned if the predicate is true, but there are no results.


# Documentation of Body Structures

In:
```
interpret(off,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                THE BODY
        ]
        ]
]
,Result).
```

* Commands may be in brackets, e.g.
`[[[n,+],[[v,a],[v,b],[v,c]]]]`


* Statements may be negated in the form:

`[[n,not],[Statement]]`

For example:

`[[n,not],[[[n,=],[[v,n],1]]]]`


* Statements may be connected by the disjunctive (or):

`[[n,or],[Statements1,Statements2]]`

For example:

```
[[n,or],[[[n,is],[[v,a],1]],
[[n,is],[[v,a],2]]]]
```

A limitation of List Prolog is that multiple clauses should be used rather than "or" to give non-deterministic results.

* If-then statements may either be in the form:

`[[n,"->"],[Statements1,Statements2]]`

This means "If Statements1 then Statements2".

E.g. 
```
[[n,"->"],[[[n,>],[[v,a],[v,c122]]],
[[n,downpipe],[[v,c122],[v,b],[v,c]]]]]
```

* Or, if-then statements may be in the form:

`[[n,"->"],[Statements1,Statements2,Statements2a]]`

This means "If Statements1 then Statements2, else Statements2a".

For example:

```
[[n,"->"],[[[n,deletea2],[[v,m],[v,h],[v,m1]]],
[[n,true]],
[[n,=],[[v,m],[v,m1]]]]]
```


# Documentation of Commands


* `[[n,IsOp],[Variable1,Variable2,Variable3]]` e.g. `[[n,+],[1,2,[v,b]]]]` returns `[v,b]=3` where `IsOp` may be `+`,`-`,`*` or `/`.

* `[[n,cut]]` - behaves like swipl's ! (doesn't allow backtracking forward or back past it)

* `[[n,true]]` - behaves like true

* `[[n,fail]]` - fails the current predicate

* `[[n,atom],[Variable]]`, e.g. `[[n,atom],[[v,a]]]` - returns true if `[v,a]=`e.g. `'a'`, an atom

* `[[n,string],[Variable]]`, e.g. `[[n,string],[[v,a]]]` - returns true if `[v,a]=`e.g. `"a"`, a string

* `[[n,number],[Variable]]`, e.g. `[[n,number],[14]]` - returns true where `14` is a number

* `[[n,letters],[Variable]]`, e.g. `[[n,letters],["abc"]]` - returns true where `"abc"` is letters

* `[[n,IsOperator],[Variable1,Variable2]]`, where `IsOperator=is` or `IsOperator="="`, e.g. `[[n,=],[[v,a],1]]` - returns true if `[v,a]=1`

* `[[n,ComparisonOperator],[Variable1,Variable2]]`, where `ComparisonOperator=">",">=","<", "=<", "=" or "=\="` e.g. `[[n,=\=],[1,2]]` - returns `not(1=2)=true`.

* `[[n,equals1],[Variable1,[Variable2,Variable3]]]` e.g. `[[n,equals1],[["a","b"],[[v,a],[v,b]]]]` returns `[v,a]="a"` and `[v,b]="b"`

* `[[n,equals2],[Variable1,[Variable2,Variable3]]]` e.g. `[[n,equals2],[[v,a],["a","b"]]]` returns `[v,a]=["a","b"]`

* `[[n,equals3],[Variable1,Variable2]]` e.g. `[[n,equals3],[[v,a],[1,2,3]]]` returns `[v,a]=[1,2,3]`

* `[[n,wrap],[Variable1,Variable2]]` e.g. `[[n,wrap],["a",[v,b]]]` returns `[v,b]=["a"]`

* `[[n,unwrap],[Variable1,Variable2]]` e.g. `[[n,wrap],[["a"],[v,b]]]` returns `[v,b]="a"`

* `[[n,head],[Variable1,Variable2]]` e.g. `[[n,head],[["a","b","c"],[v,b]]]` returns `[v,b]="a"`

* `[[n,tail],[Variable1,Variable2]]` e.g. `[[n,tail],[["a","b","c"],[v,b]]]` returns `[v,b]=["b","c"]`

* `[[n,member],[Variable1,Variable2]]` e.g. `[[n,member],[["a","b","c"],[v,b]]]` returns `[v,b]="a"`, `[v,b]="b"` or `[v,b]="c"`, or e.g. `[[n,member],[["a","b","c"],"c"]]` returns true.

* `[[n,member2],[Variable1,Variable2]]` e.g. `[[n,member2],[["a","b","c"],[v,b]]]` returns `[v,b]="a"`, `[v,b]="b"` or `[v,b]="c"`

* `[[n,delete],[Variable1,Variable2,Variable3]]` e.g. `[[n,delete],[["a","b","b","c"],"b",[v,c]]]` returns `[v,c]=["a","c"]`

* `[[n,append],[Variable1,Variable2,Variable3]]` e.g. `[[n,append],[["a","b"],["c"],[v,d]]]` returns `[v,d]=["a","b","c"]`.  Note: Variable2 must be in list form, not e.g. `"c"`, or the command will fail.  Wrap may wrap in `[]`.

* `[[n,stringconcat],[Variable1,Variable2,Variable3]]` e.g. `[[n,stringconcat],["hello ","john",[v,c]]]` returns `[v,c]="hello john"`.

* `[[n,stringtonumber],[Variable1,Variable2]]` e.g. `[[n,stringtonumber],["3",[v,b]]]` returns `[v,b]=3`

* `[[n,random],[Variable1]]` e.g. `[[n,random],[[v,r]]]` returns e.g. `[v,r]=0.19232608946956326`

* `[[n,length],[Variable1,Variable2]]` e.g. `[[n,length],[[1,2,3],[v,l]]]` returns `[v,l]=3`

* `[[n,ceiling],[Variable1,Variable2]]` e.g. `[[n,ceiling],[0.19232608946956326,[v,c]]]` returns `[v,c]=1`

* `[[n,date],[Year,Month,Day,Hour,Minute,Seconds]]` e.g. `[[n,date],[[v,year],[v,month],[v,day],[v,hour],[v,minute],[v,second]]]` returns e.g. `[v,year]=2019`, `[v,month]=11`, `[v,day]=6`, `[v,hour]=12`, `[v,minute]=15`, `[v,second]=20.23353409767151`.

* `[[n,sqrt],[Variable1,Variable2]]` e.g. `[[n,ceiling],[4,[v,c]]]` returns `[v,c]=2`

* `[[n,round],[Variable1,Variable2]]` e.g. `[[n,round],[1.5,[v,c]]]` returns `[v,c]=2`

* `[[n,equals4],[Variable1,Variable2]]` e.g. `[[n,equals4],[[[v,c],"|",[v,d]],[1,2,3]]]` returns `[v,c]=1` and `[v,d]=[2,3]`.  You may use either order (i.e. a=1 or 1=a).  Multiple items are allowed in the head of the list, there may be lists within lists, and lists with pipes must have the same number of items in the head in each list, or no pipe in the other list.

* `[[n,findall],[Variable1,Variable2,Variable3]]` e.g. `[[n,equals3],[[v,a],[1,2,3]]],[[n,findall],[[v,a1],[[n,member2],[[v,a],[v,a1]]],[v,b]]]` returns `[v,b]=[1,2,3]`

* `[[n,string_from_file],[Variable1,Variable2]]` e.g. `[[n,string_from_file],[[v,a],"file.txt"]]` returns `[v,a]="Hello World"`

* `[[n,maplist],[Variable1,Variable2,Variable3,Variable4]]` e.g. `[[n,maplist],[[n,+],[1,2,3],0,[v,b]]]` returns `[v,b]=6`

* `[[n,string_length],[Variable1,Variable2]]` e.g. `[[n,string_length],["abc",[v,b]]]` returns `[v,b]=3`

* `[[n,sort],[Variable1,Variable2]]` e.g. `[[n,sort],[[1,3,2],[v,b]]]` returns `[v,b]=[1,2,3]`

* `[[n,intersection],[Variable1,Variable2]]` e.g. `[[n,intersection],[[1,3,2],[3,4,5],[v,b]]]` returns `[v,b]=[3]`

* `[[n,read_string],[Variable1]]` e.g. `[[n,read_string],[[v,a]]]` asks for input and returns `[v,a]="hello"`

* `[[n,writeln],[Variable1]]` e.g. `[[n,writeln],[[v,a]]]` writes `[v,a]` which is `"hello"`

* `[[n,atom_string],[Variable1,Variable2]]` e.g. `[[n,atom_string],[a,[v,b]]]` returns `[v,b]="a"` or `[[n,atom_string],[[v,b],"a"]]` returns `[v,b]=a`

* (1) `[[n,call],[Function,Arguments]]` e.g. `[[n,call],[[n,member2a],[["a","b","c"],[v,b]]]]]` returns `[v,b]=a`

* (2) `[[n,call],[[lang,Lang],Debug,[Function,Arguments],Functions]]` e.g. `[[n,call],[[lang,same],same,[[n,member2a],[["a","b","c"],[v,b]]],
[[[n,member2a],[[v,a],[v,b]],":-",
	[[[n,member2],[[v,a],[v,b]]]]]]]]` returns `[v,b]="a"`, where `same` means the same lang or debug as the parent predicate.

* (3) `[[n,call],[[lang,Lang],Debug,[Function,Arguments],Types,Modes,Functions]]` e.g. `[[n,call],[[lang,same],same,[[n,member2a],[["a","b","c"],[v,b]]],  [[[n,member2a],[[[t,brackets],[[t,number],[t,number],[t,number]]],[t,number]]]],
        [[[n,member2a],[input,output]]],
[[[n,member2a],[[v,a],[v,b]],":-",
        [       [[n,member2],[[v,a],[v,b]]]]
        ]]]]` returns `[v,b]="a"`, where `same` means the same lang or debug as the parent predicate.
                
* `[[n,trace]]` switches on trace (debug) mode.

* `[[n,notrace]]` switches off trace (debug) mode.

* See lpiverify4.pl for examples of rules (predicates without bodies) and calls to predicates.


# Grammars

* List Prolog supports grammars, for example:

* Grammars may be recursive (see test 9 in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">lpiverify4.pl</a>), i.e. they may repeat until triggering the base case:

```
test(9,[[n,grammar1],["aaa"]],
[
	[[n,grammar1],[[v,s]],":-",[[[n,noun],[[v,s],""]]]],
	[[n,noun],"->",[""]],
	[[n,noun],"->",["a",[[n,noun]]]]
	],
[[]]).
```

* And:

```
test(8,[[n,grammar1],["apple"]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],""]] 
		  ]
		  ],
		  
		  [[n,noun],"->",["apple"]]
],[[]]).
```

* In `[[n,noun],[[v,s],""]]`, the argument `[v,s]` is the entry string and `""` is the exit string.

* In the above example, the word `"apple"` is parsed.  Grammars use `"->"`, not `":-"`.

* Grammars may be recursive (see test 9 in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">lpiverify4.pl</a>), i.e. they may repeat until triggering the base case.

* Grammars may have extra arguments, placed after the other arguments.  The entry and exit string arguments are only used outside the grammar, and can be accessed, e.g. `b` in:
```
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]
```		  
Note: `":-"`, not `"->"`  
		
		
* Base case 1 for recursive grammars, which requires e.g.
```
		  [[n,compound212],["","",[v,t],[v,t]]],
```
which is triggered at the end of the string (`""`), taking and returning the extra argument `t`.

and a "bottom case" (base case 2) in case it is not at the end of the string, e.g.:
```
		  [[n,compound212],[[v,x],[v,x],[v,t],[v,t]]],
```
which is triggered if the first base case is not matched.  It takes and returns `x` (the entry and exit strings) and the extra argument `t`.

given the clause:
```
  [
    [n,compound21],[[v,t],[v,u]],"->",
    [
      [[n,a]],
      [[n,code],[[n,wrap],["a",[v,itemname1]]],
        [[n,append],[[v,t],[v,itemname1],[v,v]]]
      ],
      [[n,compound212],[[v,v],[v,u]]]
    ]
  ],
```

In it, `[[n,a]]` calls a grammar predicate called `"a"`.  `[[n,code],...]` is similar to `{}` in SWI-Prolog (it allows commands to be called within a grammar).  The code wraps a string and appends it to a list, before exiting code and calling the grammar predicate `compound212`.  `v` and `u` are not entry and exit strings, they are extra arguments, handled with `t` in the base cases 1 and 2 above.  The start of the entry string is matched with strings when [[n,a]] is called and any grammar predicates (outside `[[n,code],...]` i.e. `[[n,compound212],[[v,v],[v,u]]]` are given the rest of the entry string (which is an exit string), and this continues until the string ends at base case 1 or the string doesn't end at base case 2 and is processed in a later clause.

* Sometimes there is another recursive clause, which calls itself:
```
  [
    [n,compound21],[[v,t],[v,u]],"->",
    [
      [[n,a]],",",
      [[n,compound21],[[],[v,compound1name]]],
      [[n,code],[[n,wrap],["a",[v,itemname1]]],
        [[n,append],[[v,t],[v,itemname1],[v,v]]],
        [[n,append],[[v,v],[v,compound1name],[v,u]]]
      ]
    ]
  ],
```
		  
`[[n,a]]`, a call, could be substituted with `[v,b]`, however `[[n,a]]` would call a grammar predicate and `[v,b]` would return a character.

* When we need to find out what the next character is but parse the character somewhere else, we use lookahead.

E.g.:
```
  [
    [n,word21],[[v,t],[v,u]],"->",
    [
      [v,a],[[n,commaorrightbracketnext]],
      [[n,code],[[n,letters],[[v,a]]],
        [[n,stringconcat],[[v,t],[v,a],[v,v]]]
      ],
      [[n,word212],[[v,v],[v,u]]]
    ]
  ],
```

With `commaorrightbracketnext` (which looks ahead for a comma or `"]"`), it doesn't return true in `"a"` of `"ab,c"`
when it is run and goes to `"b"` instead as wanted on another run.

* Note, we can call `lookahead` as a grammar predicate:
`[[n,lookahead],["ate"]]`

even though the predicate itself is not a grammar predicate:
```
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]
```
where `[v,b]="ate"`.

* Predicates or predicates to modify to provide the function of string to list (test 15), split on character(s) (test 17), `intersection` and `minus` are in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">lpiverify4.pl</a>.

# Type Statements

* Functions may have strong types, which check inputted values when calling a function and check all values when exiting a function.  So far, any type statement with the name and arity of the function may match data for a call to that function.

* The user may optionally enter types after the query. The following type statement tests number, string and predicate name types.

* Note: Mode statements, described in the next section, are required after Type Statements.

* Type Statements may be recursive (see test 23 in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_types.pl">lpiverify4_types.pl</a>), i.e. they may repeat until triggering the base case:

```
test_types_cases(23,[[n,connect_cliques],[[["a",1],[1,2],[2,"b"]],[["a",3],[3,4],[4,"b"]],[v,output]]],
[
	[[n,connect_cliques],[[t,list2],[t,list2],[t,list2]]],
	[[t,item],[[t,number]]],
	[[t,item],[[t,string]]],
	[[t,list2],[[[t,list],[[t,item]]]]],
	[[t,list2],[[[t,list],[[t,list2]]]]]
],
	[[[n,connect_cliques],[input,input,output]]],
	[[[n,connect_cliques],[[v,a],[v,b],[v,c]],":-",
		[[[n,append],[[v,a],[v,b],[v,c]]]]]],
[[[[v,output],[["a",1],[1,2],[2,"b"],["a",3],[3,4],[4,"b"]]]]]).
```

* Also:

```
test_types_cases(2,[[n,function],[[v,a],[v,b],[v,c]]],
[[[n,function],[[t,number],[t,string],[t,predicatename]]]],
[[[n,function],[output,output,output]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,=],[[v,a],1]],
                [[n,=],[[v,b],"a"]],
                [[n,=],[[v,c],[n,a]]]
        ]]
]
,[[[[v,a], 1],[[v,b], "a"],[[v,c], [n,a]]]]).
```

* The following type statement tests number types.
```
test_types_cases(1,[[n,function],[1,1,[v,c]]],
[[[n,function],[[t,number],[t,number],[t,number]]]],
[[[n,function],[input,input,output]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,[[[[v,c], 2]]]).
```

* The following type statement tests number and bracket types.
```
test_types_cases(3,[[n,function],[[v,a]]],
[[[n,function],[[[t,brackets],[[t,number]]]]]],
[[[n,function],[output]]],
[
        [[n,function],[[1]]]
]
,[[[[v,a], [1]]]]).
```

* The following type statement tests number and string types.
```
test_types_cases(4,[[n,f],[[v,a],[v,b],[v,c],[v,d]]],
[[[n,f],[[t,number],[t,string],[t,number],[t,string]]]],
[[[n,f],[output,output,output,output]]],
[
        [[n,f],[1,"a",2,"b"]]
]
,[[[[v,a], 1],[[v,b], "a"],[[v,c], 2],[[v,d], "b"]]]).
```

* The following type statement tests unique types, number and string types.
```
test_types_cases(5,[[n,f],[[v,a],[v,b]]],
[
        [[n,f],[[t,a],[t,b]]],
        [[t,a],[[t,number]]],
        [[t,b],[[t,string]]]
],
[
        [[n,f],[output,output]]
],
[
        [[n,f],[1,"a"]]
]
,[[[[v,a], 1],[[v,b], "a"]]]).
```

* The following type statement tests any of number and string types.
```
test_types_cases(6,[[n,f],[[v,a]]],
[
        [[n,f],[[t,a]]],
        [[t,a],[[t,number]]],
        [[t,a],[[t,string]]]
],
[
        [[n,f],[output]]
],
[
        [[n,f],["a"]]
]
,[[[[v,a], "a"]]]).
```

* The following type statements test any types (including multiple types).
```
[
        [[n,map],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,any]]]]],
        [t,number],[t,number]]],
        
        [[n,getitemn],[[t,number],[[t,list],[[t,any]]],[t,any]]]
],
```

# Mode Statements

In the following,
```
test_types_cases(2,[[n,function],[[v,a],[v,b],[v,c]]],
[[[n,function],[[t,number],[t,string],[t,predicatename]]]],
[[[n,function],[output,output,output]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,=],[[v,a],1]],
                [[n,=],[[v,b],"a"]],
                [[n,=],[[v,c],[n,a]]]
        ]]
]
,[[[[v,a], 1],[[v,b], "a"],[[v,c], [n,a]]]]).
```

`[[[n,function],[output,output,output]]],` is the mode statement, which must follow the type statement (although type and mode statements together are optional).  The Mode Statement specifies whether each of the variables takes input or gives output.

# Functional List Prolog (FLP)

* List Prolog has an optional functional mode.  In FLP, function calls may be passed as variables and functions may have strong types.

* Functional algorithms may be recursive (see test 7 in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_types.pl">lpiverify4_types.pl</a>), i.e. they may repeat until triggering the base case:

```
test_types_cases(7,[[n,map],[[[n,add],[[[n,add],[[[n,add],1]]]]],0,[v,d]]],
[
        [[n,map],[[t,number],
        [t,number],[t,number]]],
        
        [[n,map],[[[t,brackets],[[t,predicatename],
        [t,number]]],
        [t,number],[t,number]]],
        
        [[n,map],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,any]]]]],
        [t,number],[t,number]]],
        
        [[n,add],[[t,number],[t,number],[t,number]]],
        
        [[n,getitemn],[[t,number],[[t,list],[[t,any]]],[t,any]]]
],
[
        [[n,map],[input,input,output]],
                
        [[n,add],[input,input,output]],
        
        [[n,getitemn],[input,input,output]]
],
[
        [[n,map],[[v,f1],[v,n1],[v,n]],":-",
        [        
                [[n,number],[[v,f1]]],
                [[n,add],[[v,n1],[v,f1],[v,n]]]
        ]
        ],       
        [[n,map],[[v,f1],[v,l],[v,n]],":-",
        [        
                [[n,equals1],[[v,f1],[[v,f11],[v,bb]]]],
                [[n,=],[[v,f11],[n,add]]],
                [[n,number],[[v,bb]]],
                [[v,f11],[[v,l],[v,bb],[v,n1]]],
                [[v,f11],[[v,n1],[v,bb],[v,n]]]
        ]
        ],       
        [[n,map],[[v,f1],[v,l],[v,n]],":-",
        [        
                [[n,equals1],[[v,f1],[[v,f11],[v,f12]]]],
                [[n,=],[[v,f11],[n,add]]],
                [[n,getitemn],[1,[v,f12],[v,bb]]],
                [[v,f11],[[v,l],1,[v,l2]]],
                [[n,map],[[v,bb],[v,l2],[v,n]]]
        ]
        ],

        [[n,add],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]
        ]],

        [[n,getitemn],[1,[v,b],[v,c]],":-",
        [       [[n,head],[[v,b],[v,c]]]
        ]],
        [[n,getitemn],[[v,a],[v,b],[v,c]],":-",
        [       [[n,not],[[[n,=],[[v,a],1]]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,getitemn],[[v,d],[v,t],[v,c]]]
        ]]

]

,[[[[v,d], 4]]]).
```

* In the following, `[[n,function],[[[n,function2],[2]],1,1,[v,c]]]` function2 is passed as a variable.  `[v,f11]` is replaced with the function name.
```
%% c=f(g(2), 1, 1)
test(53,[[n,function],[[[n,function2],[2]],1,1,[v,c]]],
[
        [[n,function],[[v,f1],[v,a],[v,b],[v,c]],":-",
        [
                [[n,equals1],[[v,f1],[[v,f11],[v,f12]]]],
                [[n,getitemn],[1,[v,f12],[v,bb]]],
                [[v,f11],[[v,bb],[v,d],[v,f]]],
                [[n,+],[[v,a],[v,b],[v,e]]],
                [[n,+],[[v,e],[v,f],[v,g]]],
                [[n,+],[[v,g],[v,d],[v,c]]]
        ]
        ],
        [[n,function2],[[v,bb],[v,a],[v,f]],":-",
        [
                [[n,is],[[v,a],[v,bb]]],
                [[n,is],[[v,f],1]]
        ]
        ],

        [[n,getitemn],[1,[v,b],[v,c]],":-",
        [       [[n,head],[[v,b],[v,c]]]
        ]],
        [[n,getitemn],[[v,a],[v,b],[v,c]],":-",
        [       [[n,not],[[[n,=],[[v,a],0]]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,getitemn],[[v,d],[v,t],[v,c]]]
        ]]
]

,[[[[v,c], 5]]]).
```

* For other examples, please see <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">lpiverify4.pl</a>, <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_types.pl">lpiverify4_types.pl</a> (for examples with types, including the list type), <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_open.pl">lpiverify4_open.pl</a> (for examples with open-ended results) and <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4_open_types.pl">lpiverify4_open_types.pl</a> (for examples with open-ended results with types).
