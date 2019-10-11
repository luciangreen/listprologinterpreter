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
`Result=[[[[v,c], 2]]]`  This is the list of non-deterministic results (i.e. ones that SWI-Prolog would return after pressing `";"`) containing the list of variable values.


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


* See lpiverify4.pl for examples of rules (predicates without bodies) and calls to predicates.


# Grammars

* List Prolog supports grammars, for example:

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

* Grammars may have extra arguments, placed after the other arguments.  The entry and exit string arguments are only used outside the grammar, and can be accessed, e.g.:
```
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]
```		  
Note: `":-"`, not `"->"`  
		
		
* Base cases for recursive grammars require e.g.
```
		  [[n,compound212],["","",[v,t],[v,t]]],
```

and a "bottom case" in case it is not at the end of the string, e.g.:
```
		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],
```

given the clause:
```
		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,a]],
		  [[n,code],[[n,wrap],["a",[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],
```

In it, `[[n,a]]` calls a grammar predicate called `"a"`.  `[[n,code],...]` is similar to `{}` in SWI-Prolog (it allows commands to be called within a grammar).  The code wraps a string and appends it to a list, before exiting code and calling the grammar predicate `compound212`.  `v` and `u` are not entry and exit strings, they are extra arguments, handled with `t` in the base cases above.

* Sometimes there is another recursive clause, which calls itself:
```
		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,a]],",",
		  [[n,compound21],[[],[v,compound1name]]],
		  [[n,code],[[n,wrap],["a",[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]]]],
```
		  
`[[n,a]]`, a call, could be substituted with `[v,b]`, however `[[n,a]]` would call a grammar predicate and `[v,b]` would return a character.

* When we need to find out what the next character is but parse the character somewhere else, we use lookahead.

E.g.:
```
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,commaorrightbracketnext]],
		  [[n,code],[[n,letters],[[v,a]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],
```

With `commaorrightbracketnext` (which looks ahead for a comma or `"]"`), it doesn't return true in `"a"` of `"ab,c"`
and goes to `"b"` instead as wanted.

* Note, we can call `lookahead` as a grammar predicate:
`[[n,lookahead],["ate"]]`

even though the predicate itself is not a grammar predicate:
```
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]
```

* Predicates or predicates to modify to provide the function of string to list (test 15), split on character(s) (test 17), `intersection` and `minus` are in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">lpiverify4.pl</a> or in <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/philosophy/Put%20in%20lpiverify/he%20should%20identify%20different%20writers%20in%20the%20exposition.pl">plans</a>.