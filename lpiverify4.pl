%% test(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test(Debug,NTotal,Score) :- test(Debug,0,NTotal,0,Score),!.
test(_Debug,NTotal,NTotal,Score,Score) :- NTotal=251, !.
test(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test(NTotal3,Query,Functions,Result),
	((international_interpret([lang,"en"],Debug,Query,Functions,Result1),
	%writeln1([result1,Result1]),
	Result=Result1	
	)->(Score3 is Score1+1,writeln0([test,NTotal3,passed]));(Score3=Score1,writeln0([test,NTotal3,failed]))),
	writeln0(""),
	test(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

test1(Debug,N,Passed) :-
	test(N,Query,Functions,Result),
	((international_interpret([lang,"en"],Debug,Query,Functions,Result1),
	%writeln1([result1,Result1]),
	Result=Result1
	)->(Passed=passed,writeln0([test,N,passed]));(Passed=failed,writeln0([test,N,failed]))),!.


%%writeln([eg1]),
test(1,[[n,function]],
[
        [[n,function],":-",
        [
                [[n,equals4_on]]
        ]
        ]
]
,[[]]).
%%writeln([eg2]),
test(2,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,+],[[v,d],1,[v,c]]]
        ]
        ]
]
,[[[[v,c], 3]]]).
%%writeln([eg3]),
test(3,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,function2],[[v,d],[v,f]]],
                [[n,+],[[v,a],[v,b],[v,e]]],
                [[n,+],[[v,e],[v,f],[v,g]]],
                [[n,+],[[v,g],[v,d],[v,c]]]
        ]
        ],
        [[n,function2],[[v,a],[v,f]],":-",
        [
                [[n,is],[[v,a],2]],
                [[n,is],[[v,f],1]]
        ]
        ]
]
,[[[[v,c], 5]]]).
%%writeln([eg4]),
test(4,[[n,append1],[[v,a]]],
[
        [[n,append1],[[v,a]],":-",
        [
                [[n,b],[[v,b]]],
                [[n,c],[[v,c]]],
                [[n,append],[[v,b],[v,c],[v,a]]]
        ]
        ],
        [[n,b],[["b"]]],
        [[n,c],[["c"]]]
]
,[[[[v,a], ["b", "c"]]]]).

%%writeln([eg5]),
test(5,[[n,count],[1,[v,n]]],
[
        [[n,count],[1,2]]
        
] ,[[[[v,n], 2]]]).
%%writeln([eg6]),
test(6,[[n,count],[0,[v,n]]],
[
        [[n,count],[1,2]],
        [[n,count],[[v,n],[v,p]],":-",
        [
                [[n,+],[[v,n],1,[v,m]]],
                [[n,count],[[v,m],[v,p]]]
        ]
        ]/*
        ,
        
        [[n,p1],[[v,n],[v,a],[v,p]],":-",
        [
                [[n,+],[[v,n],[v,a],[v,p]]]
        ]
        ]*/
        
] ,[[[[v,n], 2]]]).
%%writeln([eg7]),
test(7,[[n,reverse],[[1,2,3],[],[v,l]]],
[
        [[n,reverse],[[],[v,l],[v,l]]],
        [[n,reverse],[[v,l],[v,m],[v,n]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,wrap],[[v,h],[v,h1]]],
                [[n,append],[[v,h1],[v,m],[v,o]]],
                [[n,reverse],[[v,t],[v,o],[v,n]]]
        ]
        ]
],[[[[v,l], [3, 2, 1]]]]).

test(8,[[n,grammar1],["apple"]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],""]] 
		  ]
		  ],
		  
		  [[n,noun],"->",["apple"]]
],[[]]).

test(9,[[n,grammar1],["aaa"]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],""]] 
		  ]
		  ],
		  
		  [[n,noun],"->",[""]],
		  [[n,noun],"->",["a",[[n,noun]]]]
],[[]]).

test(10,[[n,grammar1],["aa",[v,t]]],
[
		  [[n,grammar1],[[v,s],[v,t]],":-",
		  [
		  			 [[n,noun],[[v,s],"",[v,t]]] 
		  ]
		  ],
		  
		  [[n,noun],["b"],"->",[""]],
		  [[n,noun],[[v,t]],"->",["a",[[n,noun],[[v,t]]]]]
],[[[[v,t],"b"]]]).

test(11,[[n,grammar1],["aa",[v,t],[v,u]]],
[
		  [[n,grammar1],[[v,s],[v,t],[v,u]],":-",
		  [
		  			 [[n,noun],[[v,s],"",[v,t],[v,u]]] 
		  ]
		  ],
		  
		  [[n,noun],["b","c"],"->",[""]],
		  [[n,noun],[[v,t],[v,u]],"->",["a",[[n,noun],[[v,t],[v,u]]]]]
],[[[[v,t],"b"],[[v,u],"c"]]]).

test(12,[[n,grammar1],["aa"]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],""]]
		  ]
		  ],

		  [[n,noun],"->",[""]],
		  [[n,noun],"->",["a",[[n,noun]]]]

],[[]]).

test(13,[[n,grammar1],["[a,a]",[v,t]]],
[
		  [[n,grammar1],[[v,u],[v,t]],":-",
		  [
		  			 [[n,compound],[[v,u],"",[],[v,t]]]
		  ]
		  ],

		  %[[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound],[[v,t],[v,u]],"->",
		  ["[",[[n,compound21],[[v,t],[v,v]]],"]",
		  [[n,compound213],[[v,v],[v,u]]]]],

		  %[[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,a]],[[n,rightbracketnext]],
		  [[n,code],[[n,wrap],["a",[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,a]],",",
		  [[n,compound21],[[],[v,compound1name]]],
		  [[n,code],[[n,wrap],["a",[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]]]],

		  %[[n,compound212],[[v,a],[v,a],[v,t],[v,t]]],

		  %[[n,a],["",""]],

		  [[n,a],"->",["a"]],

		  [[n,rightbracketnext],"->",
		  [[[n,lookahead],["]"]]]],
		  
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]		  
		  %[[n,a],[[v,a],[v,a]]]

],[[[[v,t],["a","a"]]]]).

test(14,[[n,grammar1],["[a]",[v,t]]],
[
		  [[n,grammar1],[[v,u],[v,t]],":-",
		  [
		  			 [[n,compound],[[v,u],"",[],[v,t]]]
		  ]
		  ],

		  [[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,a],[v,a],[v,t],[v,t]]],

		  [[n,compound],[[v,t],[v,u]],"->",
		  ["[",[[n,compound21],[[v,t],[v,v]]],"]",
		  [[n,compound213],[[v,v],[v,u]]]]],
		  
		  [[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,a],[v,a],[v,t],[v,t]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,a]],
		  [[n,code],[[n,wrap],["a",[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],
		  
		  [[n,a],["",""]],

		  [[n,a],"->",["a"]],

		  [[n,a],[[v,a],[v,a]]]

],[[[[v,t],["a"]]]]).

% String to List (Term)

% run with swipl --stack-limit=1G

% n,letters needs to include chars except quote

%test(15,[[n,grammar1],["[[\"aa,]\",\"b\",a],1]",[v,t]]],
test(15,

[[n,grammar1],[%"[[1],1]"
"[[\"aa,]\",b,\"c\",[]],1]"
,[v,t]]],

%[[n,item],["b,""c"",[]],1]",[v,vgp3],[v,i]]],

%test(15,[[n,grammar1],["[]"]],
%est(15,[[n,item],["\"aa,\"","",[v,t]]],
%test(15,[[n,item],["a","",[v,t]]],
%test(15,[[n,grammar1],["[a]",[v,t]]],
%%test(15,[[n,compound213],["","",[["a"],1],[v,t]]],

[
		  [[n,grammar1],[[v,u],[v,t]],":-",
		  [
		  			 [[n,compound],[[v,u],"",[],[v,t]]]
		  %,[[n,cut]]
		  			 %%[[n,number21],[[v,u],"","",[v,t]]]
		  			 %%[[n,compound213],["","",[["a"],1],[v,t]]]
		  ]
		  ],

		  %[[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]], %% swapped these

		  [[n,compound],[[v,t],[v,u]],"->",
		  ["[","]",
		  [[n,compound213],[[v,t],[v,u]]]]],

		  [[n,compound],[[v,t],[v,u]],"->",
		  ["[",[[n,compound21],[[v,t],[v,v]]],"]",%[[n,code],[[n,trace2]]],
		  [[n,compound213],[[v,v],[v,u]]]]],

		  %[[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]],
		  [[n,lookahead],["]"]],
		  [[n,code],[[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]
		  %,[[n,code],[[n,cut]]]
		  ]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]],",",
		  
		  %[[n,code],[[n,trace]]],
		  
		  [[n,compound21],[[],[v,compound1name]]],
		  [[n,code],[[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]
		  		  %,[[n,cut]]
			]]],

		  [[n,item],[[v,t]],"->",["\"",[[n,word21],["",[v,t]]],
		    "\""
		    %[[n,code],[[n,cut]]]
		    ]],

		  [[n,item],[[v,t]],"->",
		  [[[n,number21],["",[v,u]]],[[n,code],
		  [[n,stringtonumber],[[v,u],[v,t]]]
		  %,[[n,cut]]
		  ]]],

%/*
		  [[n,item],[[v,t]],"->",[[[n,word21_atom],["",[v,t1]]],
		  [[n,code],[[n,atom_string],[[v,t],[v,t1]]]
		  %,[[n,cut]]
		  ]]], % atoms
%*/
		  [[n,item],[[v,t]],"->",[[[n,compound],[[],[v,t]]]
		  		    %,[[n,code],[[n,cut]]]
			]],

		  %[[n,number212],["","",[v,t],[v,t]]],

		  [[n,number212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,commaorrightbracketnext]],
		  [[n,code],
		  [[n,"->"],[[
		  [[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]
		  ]],
		  [[n,true]],
		  [[n,"->"],[
		  [[n,equals4],[[v,a],"."]],
		  [[n,true]],
		  [[n,equals4],[[v,a],"-"]]
		  ]]
		  ]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,number212],[[v,v],[v,u]]]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],
		  [[n,"->"],[[
		  [[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]
		  ]],
		  [[n,true]],
		  [[n,"->"],[
		  [[n,equals4],[[v,a],"."]],
		  [[n,true]],
		  [[n,equals4],[[v,a],"-"]]
		  ]]
		  ]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,number21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]],

		  %[[n,word212],["","",[v,t],[v,t]]],

		  [[n,word212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,quote_next]],
		  [[n,code],%[[n,letters],[[v,a]]],
		  [[n,not],[[[n,=],[[v,a],"\""]]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],

		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],%[[n,letters],[[v,a]]],
		  [[n,not],[[[n,=],[[v,a],"\""]]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word21],["",[v,wordstring]]],
		  [[n,code],
		  [[n,stringconcat],[[v,v],[v,wordstring],[v,u]]]]]],

%/*
		  %[[n,word212_atom],["","",[v,t],[v,t]]],

		  [[n,word212_atom],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,word21_atom],[[v,t],[v,u]],"->",
		  [[v,a],[[n,commaorrightbracketnext]],
		  [[n,code],%[[n,letters],[[v,a]]],
		  [[n,not],[[[n,=],[[v,a],"\""]]]],
		  [[n,not],[[[n,=],[[v,a],"["]]]],
		  [[n,not],[[[n,=],[[v,a],"]"]]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212_atom],[[v,v],[v,u]]]]],

		  [[n,word21_atom],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],%[[n,letters],[[v,a]]],
		  [[n,not],[[[n,=],[[v,a],"\""]]]],
		  [[n,not],[[[n,=],[[v,a],"["]]]],
		  [[n,not],[[[n,=],[[v,a],"]"]]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word21_atom],["",[v,wordstring]]],
		  [[n,code],
		  [[n,stringconcat],[[v,v],[v,wordstring],[v,u]]]]]],
		  %*/
		  [[n,commaorrightbracketnext],"->",
		  [[[n,lookahead],[","]]]],

		  [[n,commaorrightbracketnext],"->",
		  [[[n,lookahead],["]"]]]],
		  
		  
		  [[n,quote_next],"->",
		  [[[n,lookahead],["\""]]]],

		  
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]

%()%],[[[v,t],[["a"],1]]]).
%],[[[[v,t],[["aa,]","b",a],1]]]]).

],[[[[v,t],[["aa,]",b,"c",[]],1]]]]).
%],[[[[v,i],b]]]).

%],[[[[v,t],[[1],1]]]]).

%],[[[[v,t],[[]]]]]).
%],[[[[v,t],"aa,"]]]).
%],[[[[v,t],[a]]]]).



%% Dukel goes to the grammar example

test(16,[[n,grammar1],["john ate the apple"]],
[
		  [[n,grammar1],[[v,u]],":-",
		  [
		  			 [[n,sentence],[[v,u],""]]
		  ]
		  ],
		  
		  [[n,sentence],"->",
		  [[[n,subject]],[[n,verbphrase]]]],

		  [[n,verbphrase],"->",
		  [[[n,verb]],[[n,object]]]],
		  
		  [[n,subject],["",""]],

		  [[n,subject],"->",["john"," "]],

		  [[n,subject],[[v,a],[v,a]]],
		  
		  [[n,verb],["",""]],

		  [[n,verb],"->",["ate"," "]],

		  [[n,verb],[[v,a],[v,a]]],

		  [[n,object],["",""]],

		  [[n,object],"->",["the"," ","apple"]],

		  [[n,object],[[v,a],[v,a]]]
],[[]]).

% Split string on ".", "?", "!"

%% Blackl loves the grammar

test(17,[[n,grammar1],["aaa1 ,-'! a? b! b.",[v,t]]],
%test(17,[[n,grammar1],["a? b!",[v,t]]],
%%test(15,[[n,compound213],["","",[["a"],1],[v,t]]],

[
		  [[n,grammar1],[[v,u],[v,t]],":-",
		  [
		  			 [[n,compound21],[[v,u],"",[],[v,t]]]
		  			 ,[[n,cut]]
		  			 %%[[n,number21],[[v,u],"","",[v,t]]]
		  			 %%[[n,compound213],["","",[["a"],1],[v,t]]]
		  ]
		  ],

		  [[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]], %% swapped these

		  [[n,compound],[[v,t],[v,u]],"->",
		  [[[n,compound21],[[v,t],[v,v]]],
		  [[n,compound213],[[v,v],[v,u]]]]],

		  [[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]
		  ,[[n,code],[[n,cut]]]
		  ]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]]," ",
		  [[n,compound21],[[],[v,compound1name]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]
		  ,[[n,cut]]
		  ]]],
%/**
		  [[n,item],[[v,t]],"->",
		  [[[n,number21],["",[v,t]]],
		  [[n,code],[[n,cut]]]]],
%**/
		  [[n,item],[[v,t]],"->",[[[n,word21],["",[v,t]]]%,
		  %[[n,code],[[n,cut]]]]],
		  ]],

		  [[n,item],[[v,t]],"->",[[[n,compound],[[],[v,t]]]%,
		  %[[n,code],[[n,cut]]]]],
		  ]],
%/**
		  [[n,number212],["","",[v,t],[v,t]]],

		  [[n,number212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,number212],[[v,v],[v,u]]]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,number21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]],
%**/
		  [[n,word212],["","",[v,t],[v,t]]],

		  [[n,word212],[[v,u],[v,u],[v,t],[v,t]]],

/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],
**/
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[v,b],
		  [[n,code],[[n,sentencechars],[[v,a]]],
		  [[n,finalchar],[[v,b]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v1]]],
		  [[n,stringconcat],[[v,v1],[v,b],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],

/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,word21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]]

**/
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,sentencechars],[[v,a]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word21],["",[v,wordstring]]],
		  [[n,code],
		  [[n,stringconcat],[[v,v],[v,wordstring],[v,u]]]]]],
		  
		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,letters],[[v,c]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[[n,stringtonumber],[[v,c],[v,n]]],
		  [[n,number],[[v,n]]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c]," "]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],","]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"-"]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"'"]]]],
		  
		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"."]]]],

		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"!"]]]],

		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"?"]]]]

%%],[[[v,t],[["a"],1]]]).
],[[[[v,t],["aaa1 ,-'!","a?","b!","b."]]]]).

%% Adye is Venan

test(18,[[n,grammar1],["what is 1+11",[v,c]]],
[
		  [[n,grammar1],[[v,u],[v,c]],":-",
		  [
		  			 [[n,sentence],[[v,u],"",[v,c]]]
		  ]
		  ],
		  
		  [[n,sentence],[[v,c]],"->",
		  [[[n,subject]],[[n,verbphrase],[[v,c]]]]],

		  [[n,verbphrase],[[v,c]],"->",
		  [[[n,verb]],[[n,object],[[v,c]]]]],
		  
		  [[n,subject],["",""]],

		  [[n,subject],"->",["what"," "]],

		  [[n,subject],[[v,a],[v,a]]],
		  
		  [[n,verb],["",""]],

		  [[n,verb],"->",["is"," "]],

		  [[n,verb],[[v,a],[v,a]]],

		  [[n,object],["","",[v,c]]],

		  [[n,object],[[v,c]],"->",[[[n,item],[[v,a]]],
		  "+",
		  [[n,item],[[v,b]]],
		  [[n,code],[[n,+],[[v,a],[v,b],[v,c]]]]]],

		  [[n,object],[[v,a],[v,a]]],
		  
		  [[n,item],[[v,t]],"->",
		  [[[n,number21],["",[v,u]]],[[n,code],
		  [[n,stringtonumber],[[v,u],[v,t]]]]]],

		  [[n,number212],["","",[v,t],[v,t]]],

		  [[n,number212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,number212],[[v,v],[v,u]]]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,number21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]]

],[[[[v,c],12]]]).


%% Inky Classic 2

test(19,[[n,positivityscore],[["not","you","like","a","walk"]
,["would","you","like","a","walk"
],0,[v,s]]],

/**
test(19,[[n,positivityscore],["would1"%%,"you","like","a","walk"
,["would"%%,"you","like","a","walk"
]]],
**/
[
/**
        [[n,positivityscore],[[v,l],[v,m]],":-",
        [      	 [[n,not],[
        				[[n,member],[[v,l],[v,m]]]]]
        			 
        ]]
**/        

        [[n,positivityscore],[[],[v,l],[v,s],[v,s]]],
        [[n,positivityscore],[[v,l],[v,m],[v,s1],[v,s2]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,member],[[v,h],[v,m]]],
                [[n,+],[[v,s1],1,[v,s3]]],
                [[n,positivityscore],[[v,t],[v,m],[v,s3],
                	[v,s2]]]
        ]],
        
        [[n,positivityscore],[[v,l],[v,m],[v,s1],[v,s2]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,not],[[[n,member],[[v,h],[v,m]]]]],
                [[n,positivityscore],[[v,t],[v,m],[v,s1],
                	[v,s2]]]]]

        
]
        
,[[[[v,s], 4]]]).
%%,[]).

test(20,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[[n,+],[[v,a],[v,b],[v,c]]]]
        ]
        ]
]
,[[[[v,c], 2]]]).

%%test(21,[[n,grammar1],["john ate"]],
test(21,[[n,grammar1],["ate",[v,t]]],
[


		  [[n,grammar1],[[v,u],[v,t]],":-",
		  [
		  			 [[n,lookahead],[[v,u],[v,t],"ate"]] %% 2 is endstring, 3 is what lookahead checks for
		  ]
		  ],
/**		  
		  [[n,sentence],"->",
		  [[[n,subject]],
		  [[n,lookahead],["ate"]],
		  [[n,verb]]
		  ]],

		  [[n,verbphrase],"->",
		  [[[n,verb]]]],
		  
		  [[n,subject],["",""]],

		  [[n,subject],"->",["john"," "]],

		  [[n,subject],[[v,a],[v,a]]],

		  [[n,object],["",""]],

		  [[n,object],"->",["apples"]],

		  [[n,object],[[v,a],[v,a]]],
**/
		  
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]

]
,[[[[v,t],"ate"]]]).


test(22,[[n,grammar1],["peter cut the pear"]],
[
		  [[n,grammar1],[[v,u]],":-",
		  [
		  			 [[n,sentence],[[v,u],""]]
		  ]
		  ],
		  
		  [[n,sentence],"->",
		  [[[n,subject]],[[n,verbphrase]]]],

		  [[n,verbphrase],"->",
		  [[[n,verb]],[[n,object]]]],
		  
		  [[n,subject],["",""]],

		  [[n,subject],"->",["john"," "]],

		  [[n,subject],"->",["peter"," "]],

		  [[n,subject],[[v,a],[v,a]]],
		  
		  [[n,verb],["",""]],

		  [[n,verb],"->",["ate"," "]],

		  [[n,verb],"->",["bought"," "]],

		  [[n,verb],"->",["cut"," "]],

		  [[n,verb],[[v,a],[v,a]]],

		  [[n,object],"->",
		  ["the"," ",[[n,noun]]]],

		  [[n,noun],["",""]],

		  [[n,noun],"->",["apple"]],

		  [[n,noun],"->",["pear"]],

		  [[n,noun],"->",["peach"]],

		  [[n,noun],[[v,a],[v,a]]]
],[[]]).

%% Two Uses - PhD algorithm - agree with all of only one of two sides and give opinion

%%do you agree with either abc (list j1) or def (list j2) given you agree with abcg (list k)?
%%what is your opinion of a given opinion of a is b?

test(23,[[n,agree],[["a","b","c"]
,["d","e","f"],["a","b","c","g"],[v,s]]],

[

%% test whether each item of jn is a member of k
%% test whether each item of jn is not a member of k

        [[n,agree],[[v,j1],[v,j2],[v,k],"You agree with j1."],":-",
        [       [[n,membera1],[[v,j1],[v,k]]],
                [[n,membera2],[[v,j2],[v,k]]]
        ]],

        [[n,agree],[[v,j1],[v,j2],[v,k],"You agree with j2."],":-",
        [       [[n,membera1],[[v,j2],[v,k]]],
                [[n,membera2],[[v,j1],[v,k]]]
        ]],

        [[n,membera1],[[],[v,l]]],
        [[n,membera1],[[v,l],[v,m]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,member],[[v,h],[v,m]]],
                [[n,membera1],[[v,t],[v,m]]]
        ]],

        [[n,membera2],[[],[v,l]]],
        [[n,membera2],[[v,l],[v,m]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,membera3],[[v,m],[v,h]]],
                [[n,membera2],[[v,t],[v,m]]]]],
                
        [[n,membera3],[[],[v,l]]],
        [[n,membera3],[[v,l],[v,m]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,not],[[[n,=],[[v,m],[v,h]]]]],
                [[n,membera3],[[v,t],[v,m]]]
        ]]

        
],[[[[v,s],"You agree with j1."]]]).
        
test(24,[[n,modus_ponens],["a",[["a","b"],["c","d"],["e","f"]],[v,s]]],

[
        [[n,modus_ponens],[[v,a],[v,ab],[v,b]],":-",
        [       [[n,member],[[v,ab1],[v,ab]]],
                [[n,equals1],[[v,ab1],[[v,a],[v,b]]]]
        ]]
        
],[[[[v,s],"b"]]]).

%% Two Uses - original argument and algorithm (file)
%% splits on \n, removes 1 duplicate per line, returns score of duplicates

test(25,[[n,grammar1],["aaa1 ,-'\na\nb\nb\n",
"aaa1 ,-'\na\nb\na",
[v,s]]],
%%()test(15,[[n,compound213],["","",[["a"],1],[v,t]]],
%%test(25,[[n,word21],["a\n","","",[v,t]]],
%%test(25,[[n,deletea2],[["a","b"],"a",[v,m1]]],
%%test(25,[[n,deletea2],[["a","a","b"],"a",[v,m1]]],
%%test(25,[[n,membera3],[["a","b"],"c"]],
%%test(25,[[n,positivityscore],[["a","b"],["a","b"],0,[v,m1]]],

[
		  [[n,grammar1],[[v,u],[v,t],[v,s]],":-",
		  [
		  			 [[n,compound21],[[v,u],"",[],[v,u1]]],
		  			 [[n,compound21],[[v,t],"",[],[v,t1]]],
		  			 [[n,positivityscore],[[v,u1],[v,t1],0,[v,s]]]
		  			 %%[[n,membera3],[["a","b"],"a"]]
		  			 %%[[n,number21],[[v,u],"","",[v,t]]]
		  			 %%[[n,compound213],["","",[["a"],1],[v,t]]]
		  ]
		  ],

		  [[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]], %% swapped these

		  [[n,compound],[[v,t],[v,u]],"->",
		  [[[n,compound21],[[v,t],[v,v]]],
		  [[n,compound213],[[v,v],[v,u]]]]],

		  [[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]],
		  [[n,compound21],[[],[v,compound1name]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]]]],
/**
		  [[n,item],[[v,t]],"->",
		  [[[n,number21],["",[v,t]]]]],
**/
		  [[n,item],[[v,t]],"->",[[[n,word21],["",[v,t]]]]],

		  [[n,item],[[v,t]],"->",[[[n,compound],[[],[v,t]]]]],
/**
		  [[n,number212],["","",[v,t],[v,t]]],

		  [[n,number212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,number212],[[v,v],[v,u]]]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,number21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]],
**/
		  [[n,word212],["","",[v,t],[v,t]]],

		  [[n,word212],[[v,u],[v,u],[v,t],[v,t]]],

/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],
**/
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,b],
		  [[n,code],%%[[n,sentencechars],[[v,a]]],
		  [[n,finalchar],[[v,b]]]
		  %%[[n,stringconcat],[[v,t],[v,a],[v,v1]]],
		  %%[[n,stringconcat],[[v,t],[v,b],[v,v]
		  ],
		  [[n,word212],[[v,t],[v,u]]]]],

/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,word21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]]

**/
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],%%[[n,not_return_next]],
		  [[n,code],[[n,not],[[[n,=],[[v,a],"\n"]]]],
		  %%[[n,sentencechars],[[v,a]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word21],["",[v,wordstring]]],
		  [[n,code],
		  [[n,stringconcat],[[v,v],[v,wordstring],[v,u]]]]]],

/**		  
		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,letters],[[v,c]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[[n,stringtonumber],[[v,c],[v,n]]],
		  [[n,number],[[v,n]]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c]," "]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],","]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"-"]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"'"]]]],
**/		  
		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"\n"]]]],
		  
		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],""]]]],
		  
%%		  [[n,not_return_next],[[v,a],[v,a]],":-",
%%		  [[[n,not],[[[n,stringconcat],["\n",[v,d],[v,a]]]]]]]

        [[n,positivityscore],[[],[v,l],[v,s],[v,s]]],
        [[n,positivityscore],[[v,l],[v,m],[v,s1],[v,s2]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,member],[[v,h],[v,m]]],
                [[n,"->"],[[[n,deletea2],[[v,m],[v,h],[v,m1]]],
                [[n,true]],
                [[n,=],[[v,m],[v,m1]]]]],
                [[n,+],[[v,s1],1,[v,s3]]],
                [[n,positivityscore],[[v,t],[v,m1],[v,s3],
                	[v,s2]]]
        ]],
        
        [[n,positivityscore],[[v,l],[v,m],[v,s1],[v,s2]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,not],[[[n,membera3],[[v,m],[v,h]]]]],
                [[n,positivityscore],[[v,t],[v,m],[v,s1],
                	[v,s2]]]]],
                	
        [[n,deletea2],[[],[v,l],[v,m1]],":-",[[[n,fail]]]], %%%**
        [[n,deletea2],[[v,l],[v,m],[v,t]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,=],[[v,m],[v,h]]]
                %%[[n,delete],[[v,m],[v,h],[v,m1]]]]],
                ]],
                
        [[n,deletea2],[[v,l],[v,m],[v,m1]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,not],[[[n,=],[[v,m],[v,h]]]]],
                %%[[n,not],[[[n,membera3],[[v,m],[v,h]]]]],
                [[n,deletea2],[[v,t],[v,m],[v,m1]]]
        ]],
        
        [[n,membera3],[[],[v,l]],":-",[[[n,fail]]]],
        [[n,membera3],[[v,l],[v,m]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[[n,=],[[v,m],[v,h]]]]
        ]],
        
        [[n,membera3],[[v,l],[v,m]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,not],[[[[n,=],[[v,m],[v,h]]]]]],
                [[n,membera3],[[v,t],[v,m]]]
        ]]



%%()],[[[v,t],[["a"],1]]]).
],[[[[v,s],3]]]).
%%		],[[[[v,m1],2]]]).
%%],[]).
%%()],[[[[v,t],"a"]]]).

%% Pedagogy X - write 15*2=30 As in Honours and 50*2=100 As in Masters and 250 As in PhD. - use cawp to find algorithms with input and output in same pattern as a past algorithm - question answer for cawp specs, run cawp in post order

/**
%% - simple x spec with data - for alg, and simple x spec with data for grammars
- in text file

- instruct to call only previously found predicates (x call first) - then constructs next predicate up as head predicate - new feature: substitutes a call x either tries existing tree or writes fresh code
**/

%% Two Uses - See philosophy/he should identify different writers in the exposition.pl

test(26,[[n,append1],[["a"],["b"],[v,s]]],

[
        [[n,append1],[[v,a],[v,b],[v,s]],":-",
        [       [[n,append],[[v,a],[v,b],[v,s]]]
        ]]
        
],[[[[v,s],["a","b"]]]]).


test(27,[[n,equals11],["a","a"]],

[
        [[n,equals11],[[v,a],[v,a]]]
        
],[[]]).

test(28,[[n,number11],[1]],

[
        [[n,number11],[[v,a]],":-",
        [       [[n,number],[[v,a]]]
        ]]
        
],[[]]).

test(29,[[n,minus11],[[1,2,3],[3],[v,c]]],

[
        [[n,minus11],[[v,a],[],[v,a]]],
        [[n,minus11],[[v,a],[v,b],[v,c]],":-",
        [       [[n,head],[[v,b],[v,h]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,delete],[[v,a],[v,h],[v,c]]],
                [[n,minus11],[[v,c],[v,t],[v,c]]]
        ]]
        
],[[[[v,c],[1,2]]]]).

test(30,[[n,if11],[1,[v,b]]],

[
        [[n,if11],[[v,a],[v,b]],":-",
        [       [[n,"->"],[[[n,is],[[v,a],1]],
                [[n,is],[[v,b],2]],
                [[n,is],[[v,b],3]]]]
        ]]
        
],[[[[v,b],2]]]).


test(31,[[n,not11],[1]],

[
        [[n,not11],[[v,a]],":-",
        [       [[n,not],[[[n,=],[[v,a],2]]]]
        ]]
        
],[[]]).

test(32,[[n,or11],[1]],

[
        [[n,or11],[[v,a]],":-",
        [       [[n,or],[[[n,is],[[v,a],1]],
                [[n,is],[[v,a],2]]]]
        ]]
        
],[[]]).

%% Starts at 3, decreases given the lesser of A or B until reaching 1.

test(33,[[n,downpipe],[3,1,[[3,[4,2]],[2,[3,1]]]]],

[
        [[n,downpipe],[[v,a],[v,a],[v,b]]],
        [[n,downpipe],[[v,a],[v,b],[v,c]],":-",
        [       [[n,member],[[v,c1],[v,c]]],
                [[n,equals4],[[v,c1],[[v,a],[v,c12]]]],
                [[n,equals4],[[v,c12],[[v,c121],[v,c122]]]],
                [[n,"->"],[[[n,>],[[v,a],[v,c121]]],
                [[n,downpipe],[[v,c121],[v,b],[v,c]]],
                [[n,"->"],[[[n,>],[[v,a],[v,c122]]],
                        [[n,downpipe],[[v,c122],[v,b],[v,c]]],
                        [[n,fail]]]]]]
        ]]
        
],[[]]).

%% Get item n, copies it

test(34,[[n,getitemn],[3,[1,2,3],[v,c]]],

[
        [[n,getitemn],[1,[v,b],[v,c]],":-",
        [       [[n,head],[[v,b],[v,c]]]
        ]],
        [[n,getitemn],[[v,a],[v,b],[v,c]],":-",
        [       [[n,not],[[[n,=],[[v,a],1]]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,getitemn],[[v,d],[v,t],[v,c]]]
        ]]
        
],[[[[v,c],3]]]).

%% A shell of LPI allows manipulation of variable order, testing for e.g. identical inverse

%% commutative not identical

test(35,[[n,identical],[1,2]],

[
        [[n,identical],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]],
                [[n,+],[[v,b],[v,a],[v,c]]]
        ]]
        
],[[]]).

test(36,[[n,associative],[1,2,3]],

[
        [[n,associative],[[v,a],[v,b],[v,c]],":-",
        [       [[n,*],[[v,a],[v,b],[v,d]]],
                [[n,*],[[v,d],[v,c],[v,e]]],
                [[n,*],[[v,b],[v,c],[v,f]]],
                [[n,*],[[v,f],[v,a],[v,e]]]
        ]]
        
],[[]]).

%% audience size

test(37,[[n,length1],[[1],0,[v,l]]],
[
        [[n,length1],[[],[v,l],[v,l]]],
        [[n,length1],[[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,+],[[v,m1],1,[v,m2]]],
                [[n,length1],[[v,t],[v,m2],[v,n]]]
        ]
        ]
],[[[[v,l], 1]]]).


%% Are their differences equal?

test(38,[[n,optimise1],[[[5,4],[3,2],[1,0]],[v,d]]],

[
        [[n,optimise1],[[v,a],[v,b]],":-",
        [       [[n,head],[[v,a],[v,h]]],
                [[n,tail],[[v,a],[v,t]]],
                [[n,equals1],[[v,h],[[v,h1],[v,h2]]]],
                [[n,-],[[v,h1],[v,h2],[v,b]]],
                [[n,"->"],[[[n,not],[[[n,=],[[v,t],[]]]]],
                [[n,optimise1],[[v,t],[v,b]]],
                [[n,true]]]]
        ]]
        
],[[[[v,d], 1]]]).


test(39,[[n,member1a],[1,[1,2]]],

[[[n,member1a],[[v,i1],[v,l]], ":-",
	[[[n,intersection2],[[v,i1],[v,l],[],[v,m]]]]],
[[n,intersection2],[[v,a], [], [v,l], [v,l]]],
[[n,intersection2],[[v,i1], [v,l1], [v,l2], [v,l3]], ":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,wrap],[[v,i1],[v,i11]]],
	[[n,append],[[v,l2],[v,i11],[v,l3]]]]],
	%%[[n,intersection2],[[v,i1], [v,l4], [v,l5], [v,l3]]]]],
[[n,intersection2],[[v,i1], [v,l1], [v,l2], [v,l3]], ":-",
	[[[n,head],[[v,l1],[v,i2]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],
	[[n,intersection2],[[v,i1], [v,l4], [v,l2], [v,l3]]]]]]
,[[]]).

test(40,[[n,minus1],[[1,2,3],[1,2],[v,a]]],

[[[n,minus1],[[v,l], [], [v,l]]],
[[n,minus1],[[v,l1], [v,l2], [v,l3]],":-",
	[[[n,head],[[v,l2],[v,i1]]],
	[[n,tail],[[v,l2],[v,l5]]],
	[[n,delete2],[[v,l1],[v,i1],[],[v,l6]]],
	[[n,minus1],[[v,l6], [v,l5], [v,l3]]]]],
[[n,delete2],[[], [v,a], [v,l], [v,l]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l5]]],
	[[n,delete2],[[v,l5],[v,i1],[v,l2],[v,l3]]]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",
	[[[n,head],[[v,l1],[v,i2]]],
	[[n,tail],[[v,l1],[v,l5]]],
	[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],
	[[n,wrap],[[v,i2],[v,i21]]],
	[[n,append],[[v,l2],[v,i21],[v,l6]]],
	[[n,delete2],[[v,l5],[v,i1],[v,l6],[v,l3]]]]]]
	
,[[[[v,a], [3]]]]).

test(41,[[n,part_of_string],[[1,2,3,4],[2,3]]],

[
        [[n,part_of_string],[[], []]],
        [[n,part_of_string],[[],[v,b]],":-",
	             [[[n,not],[[[n,=],[[v,b],[]]]]],
	             [[n,fail]]]],
        [[n,part_of_string],[[v,a],[v,b]],":-",
                [[[n,tail],[[v,a],[v,at]]],
                [[n,"->"],[[[n,listhead],[[v,a],[v,b]]],
                        [[n,true]],
                        [[n,part_of_string],[[v,at],[v,b]]]]]]],

        [[n,listhead],[[v,l],[]]],
        [[n,listhead],[[v,a],[v,b]],":-",
                [[[n,head],[[v,a],[v,ah]]],
                [[n,tail],[[v,a],[v,at]]],
                [[n,head],[[v,b],[v,ah]]],
                [[n,tail],[[v,b],[v,bt]]],
                [[n,listhead],[[v,at],[v,bt]]]
        ]]
        
],[[]]).

test(42,%[[n,or12],[[v,a]]],
[[n,findall],[[v,b],[[[n,or12],[[v,b]]]],[v,a]]],
[
        [[n,or12],[1]],
        [[n,or12],[2]]        
        
],[[[[v,a],[1,2]]]]).

test(43,[[n,intersection1],[[1,2,3],[3,4,5],[],[v,a]]],

[[[n,intersection1],[[], [v,a], [v,l], [v,l]]],
[[n,intersection1],[[v,l1], [v,l2], [v,l3a], [v,l3]],":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,intersection2],[[v,i1],[v,l2],[],[v,l5]]],
	[[n,append],[[v,l3a],[v,l5],[v,l6]]],
	[[n,intersection1],[[v,l4],[v,l2],[v,l6],[v,l3]]]]],
[[n,intersection2],[[v,a], [], [v,l], [v,l]]],
[[n,intersection2],[[v,i1], [v,l1], [v,l2], [v,l3]], ":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,wrap],[[v,i1],[v,i11]]],
	[[n,append],[[v,l2],[v,i11],[v,l3]]]]],
	%%[[n,intersection2],[[v,i1], [v,l4], [v,l5], [v,l3]]]]],
[[n,intersection2],[[v,i1], [v,l1], [v,l2], [v,l3]], ":-",
	[[[n,head],[[v,l1],[v,i2]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],
	[[n,intersection2],[[v,i1], [v,l4], [v,l2], [v,l3]]]]]]

,[[[[v,a], [3]]]]).

test(44,[[n,delete2],[[1,1,2],1,[],[v,a]]],

[[[n,delete2],[[], [v,a], [v,l], [v,l]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l5]]],
	[[n,delete2],[[v,l5],[v,i1],[v,l2],[v,l3]]]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",
	[[[n,head],[[v,l1],[v,i2]]],
	[[n,tail],[[v,l1],[v,l5]]],
	[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],
	[[n,wrap],[[v,i2],[v,i21]]],
	[[n,append],[[v,l2],[v,i21],[v,l6]]],
	[[n,delete2],[[v,l5],[v,i1],[v,l6],[v,l3]]]]]]
	
,[[[[v,a], [2]]]]).

%% confidence - when a person produces a certain amount of work, they will be fulfilled

test(45,[[n,greaterthan],[3,2]],

[
[[n,greaterthan],[[v,a],[v,b]],":-",
        [[[n,>],[[v,a],[v,b]]]]]
        
],[[]]).

%% did - check a box true

test(46,[[n,conjunction],["true","false",[v,c]]],

[
[[n,conjunction],["true","true","true"]],
[[n,conjunction],[[v,a],[v,b],"false"],":-",
	[[[n,not],[[[[n,=],[[v,a],"true"]],
	[[n,=],[[v,b],"true"]]]]]]]]
        
,[[[[v,c], "false"]]]).

%% have - I had the collection of 1D items

test(47,[[n,sum],[[3,1,2],0,[v,l]]],
[
        [[n,sum],[[],[v,l],[v,l]]],
        [[n,sum],[[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,+],[[v,m1],[v,h],[v,m2]]],
                [[n,sum],[[v,t],[v,m2],[v,n]]]
        ]
        ]
],[[[[v,l], 6]]]).

%% I see to sort

test(48,[[n,sort0],[[9,4,8,2,1,5,7,6,3,10],[v,l]]],
[
        [[n,sort0],[[v,l],[v,n]],":-",
        [       [[n,sort1],[[v,l],[],[v,n]]]
        ]
        ],
        [[n,sort1],[[],[v,l],[v,l]]],
        [[n,sort1],[[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,maximum],[[v,t],[v,h],[v,m2],[],[v,r]]],
                [[n,wrap],[[v,m2],[v,m3]]],
                [[n,append],[[v,m1],[v,m3],[v,m4]]],
                [[n,sort1],[[v,r],[v,m4],[v,n]]]
        ]
        ],
        [[n,maximum],[[],[v,l],[v,l],[v,r],[v,r]]],
        [[n,maximum],[[v,l],[v,m1],[v,n],[v,r1],[v,r2]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,"->"],[[[n,>=],[[v,m1],[v,h]]],
                        [[[n,=],[[v,m2],[v,m1]]],
                        [[n,wrap],[[v,h],[v,h2]]],
                        [[n,append],[[v,r1],[v,h2],[v,r3]]]],
                        [[[n,=],[[v,m2],[v,h]]],
                        [[n,wrap],[[v,m1],[v,m12]]],
                        [[n,append],[[v,r1],[v,m12],[v,r3]]]]]],
                [[n,maximum],[[v,t],[v,m2],[v,n],[v,r3],[v,r2]]]
        ]
        ]
],[[[[v,l], [10,9,8,7,6,5,4,3,2,1]]]]).

%% the aim of going to a place is reaching local maximum height

test(49,[[n,maximum0],[[2,1,3,5,-1],[v,m]]],
[
        [[n,maximum0],[[v,l],[v,m]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,maximum],[[v,t],[v,h],[v,m],[],[v,r]]]
        ]
        ],
        [[n,maximum],[[],[v,l],[v,l],[v,r],[v,r]]],
        [[n,maximum],[[v,l],[v,m1],[v,n],[v,r1],[v,r2]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,"->"],[[[n,>=],[[v,m1],[v,h]]],
                        [[[n,=],[[v,m2],[v,m1]]],
                        [[n,wrap],[[v,h],[v,h2]]],
                        [[n,append],[[v,r1],[v,h2],[v,r3]]]],
                        [[[[n,=],[[v,m2],[v,h]]]],
                        [[n,wrap],[[v,m1],[v,m12]]],
                        [[n,append],[[v,r1],[v,m12],[v,r3]]]]]],
                [[n,maximum],[[v,t],[v,m2],[v,n],[v,r3],[v,r2]]]
        ]
        ]
],[[[[v,m], 5]]]).

%% the tutor gave the mark for either answer

test(50,[[n,disjunction],["true","false",[v,c]]],

[
[[n,disjunction],["false","false","false"]],
[[n,disjunction],[[v,a],[v,b],"true"],":-",
	[[[n,not],[[[[n,=],[[v,a],"false"]],
	[[n,=],[[v,b],"false"]]]]]]]]
        
,[[[[v,c], "true"]]]).

test(51,[[n,expressionnotheadache],["true",[v,c]]],

[
[[n,expressionnotheadache],["true","true"]],
[[n,expressionnotheadache],[[v,a],"false"],":-",
	[[[n,not],[[[[n,=],[[v,a],"true"]]]]]]] ]       
,[[[[v,c], "true"]]]).

test(52,[[n,mainrole],[7,[v,c]]],

[
[[n,mainrole],[7,"mainrole"]],
[[n,mainrole],[[v,shortcourses],"false"],":-",
	[[[n,not],[[[[n,=],[[v,shortcourses],7]]]]]]] ]       
,[[[[v,c], "mainrole"]]]).

%% c=f(g(2), 1, 1)
test(53,[[n,function],[[[n,function2],[2]],1,1,[v,c]]],
%%test(53,[[n,getitemn],[1,[1,2,3],[v,bb]]],
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
        [       [[n,not],[[[n,=],[[v,a],1]]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,getitemn],[[v,d],[v,t],[v,c]]]
        ]]
]

,[[[[v,c], 5]]]).
%%,[[[[v,bb], 1]]]).

test(54,[[n,function],[[[n,function2],[2]],1,1,[v,c]]],
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
        [       [[n,not],[[[n,=],[[v,a],1]]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,getitemn],[[v,d],[v,t],[v,c]]]
        ]]
]

,[[[[v,c], 5]]]).

test(55,[[n,test1],[[v,c]]],

[
[[n,test1],[1]],
[[n,test2],[2]]]
,[[[[v, c], 1]]]).

test(56,[[n,map],[[n,add],[1,2,3],0,[v,d]]],
[
        [[n,map],[[v,f],[],[v,l],[v,l]]],
        [[n,map],[[v,f],[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[v,f],[[v,m1],[v,h],[v,m2]]],
                [[n,map],[[v,f],[v,t],[v,m2],[v,n]]]
        ]
        ],

        [[n,add],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]

,[[[[v,d], 6]]]).

%% later: (test 58) omit if [v,f] fails

test(57,[[n,findall1],[[n,plusone],[1,2,3],[],[v,d]]],
[
        [[n,findall1],[[v,f],[],[v,l],[v,l]]],
        [[n,findall1],[[v,f],[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[v,f],[[v,h],[v,m2]]],
                [[n,wrap],[[v,m2],[v,m3]]],
                [[n,append],[[v,m1],[v,m3],[v,m4]]],
                [[n,findall1],[[v,f],[v,t],[v,m4],[v,n]]]
        ]
        ],

        [[n,plusone],[[v,a],[v,c]],":-",
        [       [[n,+],[[v,a],1,[v,c]]]
        ]
        ]
]

,[[[[v,d], [2,3,4]]]]).


test(58,[[n,findall1],[[n,a_to_c],["a","b","a"],[],[v,d]]],
[
        [[n,findall1],[[v,f],[],[v,l],[v,l]]],
        [[n,findall1],[[v,f],[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                %[[n,trace2]],
                [[n,"->"],[[[v,f],[[v,h],[v,m2]]],
                [       [[n,wrap],[[v,m2],[v,m3]]],
                        [[n,append],[[v,m1],[v,m3],[v,m4]]]
                ],
                [
                        [[n,=],[[v,m1],[v,m4]]]
                ]]],
                [[n,findall1],[[v,f],[v,t],[v,m4],[v,n]]]

        ]
        ],

        [[n,a_to_c],["a","c"]
        ]
]

,[[[[v,d], ["c","c"]]]]).


test(59,[[n,count],[1,[v,n]]],
[
        [[n,count],[1,2],":-",[[[n,cut]]]],
        [[n,count],[1,3]]

        
] ,[[[[v,n], 2]]]).

test(60,[[n,a]],
[
[[n,a],":-",
[

[[n,interweaving1],[[["select,dash"],["neiey,person"],["neiey,person"]],[["select,dash"],["neiey,person"],["neiey,person"]],[],[["select,dash"],["neiey,person"],["neiey,person"]]]],

[[n,duplicates],[[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[],[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]]]],

[[n,minus1],[[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[["select,dash"],["neiey,person"],["neiey,person"],["neiey,person"],["neiey,person"]],[]]],

[[n,reverse],[[["select,dash"],["neiey,person"],["neiey,person"]],[],[["neiey,person"],["neiey,person"],["select,dash"]]]],

[[n,interweaving1],[[["neiey,person"],["neiey,person"],["select,dash"]],[["hipaa,square"],["releases,up"],["hipaa,square"]],[],[]]],

[[n,append1],[[],[["hipaa,square"],["releases,up"],["hipaa,square"]],[["hipaa,square"],["releases,up"],["hipaa,square"]]]],

[[n,minus1],[[["hipaa,square"],["releases,up"],["hipaa,square"]],[["select,dash"],["neiey,person"],["neiey,person"]],[["hipaa,square"],["releases,up"],["hipaa,square"]]]]

]],
[[n,reverse],[[],[v,l],[v,l]]],
[[n,reverse],[[v,l],[v,m],[v,n]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,wrap],[[v,h],[v,h1]]],[[n,append],[[v,h1],[v,m],[v,o]]],[[n,reverse],[[v,t],[v,o],[v,n]]]]],
[[n,interweaving1],[[],[v,a],[v,l],[v,l]]],
[[n,interweaving1],[[v,l1],[v,l2],[v,l3a],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l4]]],[[n,interweaving2],[[v,i1],[v,l2],[],[v,l5]]],[[n,append],[[v,l3a],[v,l5],[v,l6]]],[[n,interweaving1],[[v,l4],[v,l2],[v,l6],[v,l3]]]]],
[[n,interweaving2],[[v,a],[],[v,l],[v,l]]],
[[n,interweaving2],[[v,i1],[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l4]]],[[n,wrap],[[v,i1],[v,i11]]],[[n,append],[[v,l2],[v,i11],[v,l3]]]]],%%[[n,interweaving2],[[v,i1],[v,l4],[v,l5],[v,l3]]]]],
[[n,interweaving2],[[v,i1],[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i2]]],[[n,tail],[[v,l1],[v,l4]]],[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],[[n,interweaving2],[[v,i1],[v,l4],[v,l2],[v,l3]]]]],
[[n,append1],[[v,b],[v,c],[v,a]],":-",[[[n,append],[[v,b],[v,c],[v,a]]]]],
[[n,minus1],[[v,l],[],[v,l]]],
[[n,minus1],[[v,l1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l2],[v,i1]]],[[n,tail],[[v,l2],[v,l5]]],[[n,delete2],[[v,l1],[v,i1],[],[v,l6]]],[[n,minus1],[[v,l6],[v,l5],[v,l3]]]]],
[[n,delete2],[[],[v,a],[v,l],[v,l]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i1]]],[[n,tail],[[v,l1],[v,l5]]],[[n,delete2],[[v,l5],[v,i1],[v,l2],[v,l3]]]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",[[[n,head],[[v,l1],[v,i2]]],[[n,tail],[[v,l1],[v,l5]]],[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],[[n,wrap],[[v,i2],[v,i21]]],[[n,append],[[v,l2],[v,i21],[v,l6]]],[[n,delete2],[[v,l5],[v,i1],[v,l6],[v,l3]]]]],
[[n,mutuallyexclusive],[[],[v,l]]],
[[n,mutuallyexclusive],[[v,l],[v,m]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,membera3],[[v,m],[v,h]]],[[n,mutuallyexclusive],[[v,t],[v,m]]]]],
[[n,membera3],[[],[v,l]]],
[[n,membera3],[[v,l],[v,m]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,=],[[v,m],[v,h]]]]],[[n,membera3],[[v,t],[v,m]]]]],
[[n,duplicates],[[],[v,l],[v,s],[v,s]]],
[[n,duplicates],[[v,l],[v,m],[v,s1],[v,s2]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,member],[[v,h],[v,m]]],[[n,"->"],[[[n,deletea2],[[v,m],[v,h],[v,m1]]],[[n,true]],[[n,=],[[v,m],[v,m1]]]]],[[n,wrap],[[v,h],[v,h1]]],[[n,append],[[v,s1],[v,h1],[v,s3]]],[[n,duplicates],[[v,t],[v,m1],[v,s3],[v,s2]]]]],
[[n,duplicates],[[v,l],[v,m],[v,s1],[v,s2]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,membera4],[[v,m],[v,h]]]]],[[n,duplicates],[[v,t],[v,m],[v,s1],[v,s2]]]]],
[[n,deletea2],[[],[v,l],[v,m1]],":-",[[[n,fail]]]],
[[n,deletea2],[[v,l],[v,m],[v,t]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,=],[[v,m],[v,h]]]]],
[[n,deletea2],[[v,l],[v,m],[v,m1]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,=],[[v,m],[v,h]]]]],[[n,deletea2],[[v,t],[v,m],[v,m1]]]]],
[[n,membera4],[[],[v,l]],":-",[[[n,fail]]]],
[[n,membera4],[[v,l],[v,h]],":-",[[[n,head],[[v,l],[v,h]]]]],
[[n,membera4],[[v,l],[v,m]],":-",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,not],[[[n,=],[[v,m],[v,h]]]]],[[n,membera4],[[v,t],[v,m]]]]],
[[n,part_of_string],[[],[]]],
[[n,part_of_string],[[],[v,b]],":-",[[[n,not],[[[n,=],[[v,b],[]]]]],[[n,fail]]]],
%%[[n,part_of_string],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,at]]],[[n,"->"],[[[n,listhead],[[v,a],[v,b]]],[[[n,true]]],[[[n,part_of_string],[[v,at],[v,b]]]]]]]],
[[n,part_of_string],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,at]]],[[n,"->"],[[[[n,listhead],[[v,a],[v,b]]]],[[[n,true]]],[[[n,part_of_string],[[v,at],[v,b]]]]]]]],
[[n,listhead],[[v,l],[]]],
[[n,listhead],[[v,a],[v,b]],":-",[[[n,head],[[v,a],[v,ah]]],[[n,tail],[[v,a],[v,at]]],[[n,head],[[v,b],[v,ah]]],[[n,tail],[[v,b],[v,bt]]],[[n,listhead],[[v,at],[v,bt]]]]],
[[n,listhead],[[v,a],[v,b]],":-",[[[n,head],[[v,a],[v,ah]]],[[n,tail],[[v,a],[v,at]]],[[n,head],[[v,b],[v,ah]]],[[n,tail],[[v,b],[v,bt]]],[[n,listhead],[[v,at],[v,bt]]]]]
],[[]]).

test(61,[[n,add],[[1,2,3],3,[],[v,l]]],
[
        [[n,add],[[],[v,th],[v,l],[v,l]]],
        [[n,add],[[v,l],[v,th],[v,m],[v,n]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,+],[[v,h],[v,th],[v,h0]]],
                [[n,wrap],[[v,h0],[v,h1]]],
                [[n,append],[[v,m],[v,h1],[v,o]]],
                [[n,add],[[v,t],[v,th],[v,o],[v,n]]]
        ]
        ]
],[[[[v,l], [4,5,6]]]]).

test(62,[[n,add],[[1],[2,3],[v,l]]],
[
        [[n,add],[[],[v,l],[v,l]]],
        [[n,add],[[v,l],[v,m],[v,n]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,wrap],[[v,h],[v,h1]]],
                [[n,append],[[v,h1],[v,m],[v,o]]],
                [[n,add],[[v,t],[v,o],[v,n]]]
        ]
        ]
],[[[[v, l], [1,2,3]]]]).

test(63,[[n,add],[1,[v,b]]],
[
        [[n,add],[2,3]],
        [[n,add],[1,[v,b]],":-",
        [       [[n,add],[2,[v,b]]]]]
],[[[[v, b], 3]]]).


test(64,[[n,add0],[[1,2],[v,b]]],
[
	     [[n,add2],[[v,a],[v,b]],":-",
        [       [[n,=],[[v,a],[]]],
                [[n,=],[[v,b],[]]]]],
        [[n,add3],[[v,a],[v,b]],":-",
        [       [[n,tail],[[v,a],[v,b]]]]],
        
        [[n,add0],[[v,a],[v,b]],":-",
        [       [[n,1],[[v,a],[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],
        
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,add2],[[v,a],[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,add3],[[v,a],[v,c]]],
                [[n,1],[[v,c],[v,d]]],
                [[n,=],[[v,d],[v,b]]]]]
],[[[[v, b], []]]]).

test(65,[[n,add0],[[1],[v,b]]],
[[[n,add3],[[v,a],[v,b]],":-",[[[n,tail],[[v,a],[v,b]]]]],[[n,add0],[[v,a],[v,b]],":-",[[[n,add3],[[v,a],[v,c]]],[[n,=],[[v,c],[v,b]]]]]]
,[[[[v, b], []]]]).

/**
%%[[1],[2,3],[1,2,3]]],[[],[1,2,3],[1,2,3]]]

test(63,[[n,add],[1,2,[v,l]]],
[
        [[n,add],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]]],
        [[n,add],[[v,a],[v,b],[v,c]],":-",
        [       [[n,-],[[v,a],[v,b],[v,c]]]]]
],[[[[v, l], 3]], [[[v, l], -1]]]).

test(64,[[n,add],[[1,2,3],3,[],[v,l],[v,t],[v,t],[v,th],[v,th],[v,o],[v,o]]],
[
        [[n,add],[[],[v,th],[v,l],[v,l],[v,t],[v,t],[v,th],[v,th],[v,o],[v,o]]],
        [[n,add],[[v,l],[v,th],[v,m],[v,n],[v,t],[v,th],[v,o]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,+],[[v,h],[v,th],[v,h0]]],
                [[n,wrap],[[v,h0],[v,h1]]],
                [[n,append],[[v,m],[v,h1],[v,o]]],
                [[n,add],[[v,t],[v,th],[v,o],[v,n],[v,t],[v,t],[v,th],[v,th],[v,o],[v,o]]]
        ]
        ]
],[[[[v,l], [4,5,6]],[[v,t],888],[[v,th],888],[[v,o],888]]]).

%% do separate i,o to group of last 3 vars
%% separate i,o


test(65,[[n,add3],[[v,a],[v,b]]],
[
[[n,add1],[1]],
[[n,add2],[[v,a],[v,b]],":-",
[       [[n,+],[[v,a],1,[v,b]]]]],
[[n,add3],[[v,a],[v,b]],":-",
[       [[n,+],[[v,a],1,[v,b]]]]],


%% give functional function base case name as arg, which it can move around using cawp not cawmp

%% fibonacci


%% change back lpi, cawp verify
**/

test(66,[[n,addorsubtract1],[2,1,1]],
[
        [[n,addorsubtract1],[[v,a],[v,b],[v,c]],":-",
        [       %%[[n,or],[[[n,addorsubtract2],[[v,a],[v,b],[v,c]]],
        						%%[[n,true]],
        						[[n,addorsubtract2],[[v,a],[v,b],[v,d]]],%%]
        						[[n,=],[[v,d],[v,c]]]
        ]
        ],
        [[n,addorsubtract2],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,d]]],%%]
        						[[n,=],[[v,d],[v,c]]]
        ]
        ],
        [[n,addorsubtract2],[[v,a],[v,b],[v,c]],":-",
        [       [[n,-],[[v,a],[v,b],[v,d]]],%%]
        						[[n,=],[[v,d],[v,c]]]
        ]
        ]        
],[[]]).

test(67,[[n,addorsubtract1],[2,1,1]],
[
        [[n,addorsubtract1],[[v,a],[v,b],[v,c]],":-",
        [       [[n,or],[[[n,addorsubtract2],[[v,a],[v,b],[v,c]]],
        						%%[[n,true]],
        						[[n,addorsubtract3],[[v,a],[v,b],[v,c]]]]]
        ]
        ],
        [[n,addorsubtract2],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ],
        [[n,addorsubtract3],[[v,a],[v,b],[v,c]],":-",
        [       [[n,-],[[v,a],[v,b],[v,c]]]
        ]
        ]        
],[[]]).


test(68,[[n,addorsubtract1],[2,1,1]],
[
        [[n,addorsubtract1],[[v,a],[v,b],[v,c]],":-",
        [       [[n,"->"],[[[n,addorsubtract2],[[v,a],[v,b],[v,c]]],
        						[[n,true]],
        						[[n,addorsubtract3],[[v,a],[v,b],[v,c]]]]]
        ]
        ],
        [[n,addorsubtract2],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ],
        [[n,addorsubtract3],[[v,a],[v,b],[v,c]],":-",
        [       [[n,-],[[v,a],[v,b],[v,c]]]
        ]
        ]        
],[[]]).

test(69,[[n,add0],[2,1]],
[        
	[[n,add0],[[v,a],[v,b]],":-",
		[[[n,1],[[v,a],[v,c]]],
		[[n,=],[[v,c],[v,b]]]]],
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,-],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
],[[]]).

test(70,[[n,add0],[1,2]],
[        
        [[n,a2],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[n,a3],[[v,a],[v,b]],":-",
        [       [[n,-],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[n,add0],[[v,a],[v,b]],":-",
        [       [[n,1],[[v,a],[v,b]]]]],
        
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,a2],[[v,a],[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],

        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,a3],[[v,a],[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
],[[]]).

test(71,[[n,add0],[1,2]],
[       
        [[n,add0],[[v,a],[v,b]],":-",
        [       [[n,1],[[v,a],[v,b]]]]],
        
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
],[[]]).

test(72,[[n,add0],[1,[v,b]]],
[       
        [[n,add0],[[v,a],[v,b]],":-",
        [       [[n,1],[[v,a],[v,b]]]]],
        
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,+],[[v,a],1,[v,c]]],
                [[n,=],[[v,c],[v,b]]]]]
],[[[[v,b],2]]]).

test(73,[[n,add0],[1,1,[v,c]]],
[       
        [[n,add0],[[v,a],[v,b],[v,c]],":-",
        [       [[n,1],[[v,a],[v,b],[v,c]]]]],
        
        [[n,1],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,d]]],
                [[n,=],[[v,d],[v,c]]]]]
],[[[[v,c],2]]]).

test(74,[[n,add0],[[1,2],[v,c]]],
[ %% Result
[[n,add2],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[n,add3],[[v,a],[v,b]],":-",
	[[[n,tail],[[v,a],[v,b]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,add2],[[v,a],[v,c]]],
	[[n,=],[[v,c],[v,b]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,add3],[[v,a],[v,c]]],
	[[n,add0],[[v,c],[v,d]]],
	[[n,=],[[v,d],[v,b]]]]]]
,[[[[v,c],[]]]]).

test(75,[[n,add0],[[],[v,c]]],
[[[n,add2],[[v,a],[v,b]],":-",
	[[[n,=],[[v,a],[]]],
	[[n,=],[[v,b],[]]]]],
[[n,add0],[[v,a],[v,b]],":-",
	[[[n,add2],[[v,a],[v,c]]],
	[[n,=],[[v,a],[v,b]]]]]],
[[[[v,c],[]]]]).

test(76,[[n,implies2],[1,[v,b]]],

[
        [[n,implies2],[[v,a],[v,b]],":-",
        [       [[n,"->"],[[[n,is],[[v,a],1]],
                [[n,is],[[v,b],2]]]]
        ]]
        
],[[[[v,b],2]]]).

test(77,[[n,findall1],[[1,2,3],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[v,a1],[[n,member],[[v,a1],[v,a]]],
                [v,b]]]
        ]]
        
],[[[[v,b],[1,2,3]]]]).

/**
test(77a,[[n,member2a],[[1,2,3],[v,b]]],

[
        [[n,member2a],[[v,a],[v,b]],":-",
        [[[n,member],[[v,a],[v,b]]]
        ]]
        
        
],[[[[v,b],1]],[[[v,b],2]],[[[v,b],3]]]).

**/

test(78,[[n,maplist1],[[1,2,3],[v,b]]],

[
        [[n,maplist1],[[v,a],[v,b]],":-",
        [       [[n,maplist],[[n,+],[v,a],0,[v,b]]]
        ]]

        
],[[[[v,b],6]]]).


test(79,[[n,equals41],[[1,2,3],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[v,a],[[v,b],"|",[v,c]]]]
        ]]
        
],[[[[v,b],1]]]).

test(80,[[n,equals41],[[v,a],[v,d],[v,c],[v,b]]],

[
        [[n,equals41],[[v,a],[v,d],[v,c],[v,b]],":-",
        [       [[n,equals4],[[[1,5],2,3,4],[[[v,a],"|",[v,d]],[v,c],"|",[v,b]]]]
        ]]
        
],[[[[v,a],1],[[v,b],[3,4]],[[v,c],2],[[v,d],[5]]]]).


test(81,[[n,equals41],[[v,a],[v,c],[v,b]]],

[
        [[n,equals41],[[v,a],[v,c],[v,b]],":-",
        [       [[n,equals4],[[[[v,a],[v,c]],"|",[v,b]],[[1,2],3,4]]]
        ]]
        
],[[[[v,a],1],[[v,b],[3,4]],[[v,c],2]]]).

test(82,[[n,equals41],[[v,a],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[[v,a],"|",[v,b]],[1,2,3,4]]]
        ]]
        
],[[[[v, a], 1], [[v, b], [2, 3, 4]]]]).

test(83,[[n,equals41]],

[
        [[n,equals41],":-",
        [       [[n,equals4],[[[v,a],[v,c],"|",[v,b],[v,d]],[1,2,3,4]]]
        ]]
        
],[]).

test(84,[[n,equals41],[[v,a],[v,c],[v,b]]],

[
        [[n,equals41],[[v,a],[v,c],[v,b]],":-",
        [       [[n,equals4],[[[[v,a]],[v,c],"|",[v,b]],[[1],2,3,4]]]
        ]]
        
],[[[[v,a],1],[[v,b],[3,4]],[[v,c],2]]]).

test(85,[[n,equals41],[[v,a],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[[v,a],"|",[v,b]],[[1,2],3,4]]]
        ]]
        
],[[[[v, a], [1, 2]], [[v, b], [3, 4]]]]).

test(86,[[n,equals41],[[v,a],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[[v,a],"|",[[v,b]]],[1,2]]]
        ]]
        
],[[[[v, a], 1], [[v, b], 2]]]).

test(87,[[n,equals41],[[v,a]]],

[
        [[n,equals41],[[v,a]],":-",
        [       [[n,equals4],[[[v,a]],[1]]]
        ]]
        
],[[[[v, a], 1]]]).

test(88,[[n,equals41],[[v,a],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[[v,a],[v,b]],[1,2]]]
        ]]
        
],[[[[v, a], 1], [[v, b], 2]]]).

test(89,[[n,equals41],[[v,a],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[[v,a],[v,b]],[[1,3],2]]]
        ]]
        
],[[[[v, a], [1, 3]], [[v, b], 2]]]).


test(90,[[n,equals41]],

[
        [[n,equals41],":-",
        [       [[n,equals4],[[[v,a],[v,c],"|",[v,b],"|",[v,d]],[1,2,3,4]]]
        ]]
        
],[]).

test(91,[[n,equals41],[[1,2,3]]],

[
        [[n,equals41],[[v,a]],":-",
        [       [[n,equals4],[[v,a],[1,2,3]]]
        ]]
        
],[[]]).

test(92,[[n,equals41],[[v,a],[v,b],[v,d]]],

[
        [[n,equals41],[[v,a],[v,b],[v,d]],":-",
        [       [[n,equals4],[[[v,a],"|",[[v,b],"|",[v,d]]],[1,2,3,4]]]
        ]]
        
],[[[[v, a], 1], [[v, b], 2],[[v, d], [3,4]]]]).

test(93,[[n,maplist1],[[[1],[2],[3]],[v,b]]],

[
        [[n,maplist1],[[v,a],[v,b]],":-",
        [       [[n,maplist],[[n,append],[v,a],[],[v,b]]]
        ]]

        
],[[[[v,b],[1,2,3]]]]).

test(94,[[n,maplist1],[[[[1]],[[2]],[[3]]],[v,b]]],

[
        [[n,maplist1],[[v,a],[v,b]],":-",
        [       [[n,maplist],[[n,append],[v,a],[],[v,b]]]
        ]]

        
],[[[[v,b],[[1],[2],[3]]]]]).

test(95,[[n,findall1],[[1,2,3],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[[v,a1],[v,a1]],[[n,member],[[v,a1],[v,a]]],
                [v,b]]]
        ]]
        
],[[[[v,b],[[1,1],[2,2],[3,3]]]]]).


test(96,[[n,equals41],[1,[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[v,b],[[v,a],[v,a]]]]
        ]]
        
],[[[[v, b], [1,1]]]]).


test(97,[[n,equals41],[[v,a]]],

[
        [[n,equals41],[[v,a]],":-",
        [       [[n,equals4],[[v,a],[1,2,3]]]
        ]]
        
],[[[[v,a],[1,2,3]]]]).

test(98,[[n,equals41],[[[1,2],3,4],[v,a],[v,b]]],

[
        [[n,equals41],[[v,c],[v,a],[v,b]],":-",
        [       [[n,equals4],[[[v,a],"|",[v,b]],[v,c]]]
        ]]
        
],[[[[v, a], [1, 2]], [[v, b], [3, 4]]]]).

test(99,[[n,equals41],[1,[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,equals4],[[v,b],[[[v,a],[v,a]],[v,a]]]]
        ]]
        
],[[[[v, b], [[1,1],1]]]]).


test(100,[[n,equals41],[1,[v,c],[v,b]]],

[
        [[n,equals41],[[v,a],[v,c],[v,b]],":-",
        [       [[n,equals4],[[[v,c],"|",[v,b]],[[[v,a],[v,a]],[v,a]]]]
        ]]
        
],[[[[v,b],[1]],[[v,c],[1,1]]]]).


test(101,[[n,equals41],[1,[v,c],[v,b]]],

[
        [[n,equals41],[[v,a],[v,c],[v,b]],":-",
        [       [[n,equals4],[[[[v,a],[v,a]],[v,a]],[[v,c],"|",[v,b]]]]
        ]]
        
],[[[[v,b],[1]],[[v,c],[1,1]]]]).


test(102,[[n,equals41],[1,[2,3],[v,b1],[v,b2],[v,b3]]],

[
        [[n,equals41],[[v,a],[v,d],[v,b1],[v,b2],[v,b3]],":-",
        [       [[n,equals4],[[[v,a],"|",[v,d]],[[v,b1],[v,b2],[v,b3]]]]
        ]]
        
],[[[[v, b1], 1],[[v,b2],2],[[v,b3],3]]]).

test(103,[[n,equals41],[1,[2,3],[v,b1],[v,b2],[v,b3]]],

[
        [[n,equals41],[[v,a],[v,d],[v,b1],[v,b2],[v,b3]],":-",
        [       [[n,equals4],[[[v,b1],[v,b2],[v,b3]],[[v,a],"|",[v,d]]]]
        ]]
        
],[[[[v, b1], 1],[[v,b2],2],[[v,b3],3]]]).

test(104,[[n,findall1],[[[1,2],[3,4]],[v,b]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[v,b1],[[[n,member],[[v,a1],[v,a]]],
        
        [[n,findall],[[v,a3],[[[n,member],[[v,a2],[v,a1]]],
        [[n,+],[[v,a2],5,[v,a3]]]],
                [v,b1]]]],
                
                [v,b]]]	
        ]]
        
],[[[[v,b],[[6,7],[8,9]]
%[[1,11,111],[2,22,222],[3,33,333]]
]]]).


test(105,[[n,member2a],[[v,b],[1,11,111]]],

[
        [[n,member2a],[[v,b],[v,a]],":-",
        [       [[n,member],[[v,b],[v,a]]],[[n,cut]]]
        ]
        
],[[[[v,b],1]]]).

/**

%% Need to analyse body, test whether cut is after a statement, cut results

test(105a,[[n,findall1],[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[v,b1],[[[n,member],[[v,a],[v,a1]]],
                [[n,cut]],
        
        [[n,findall],[[v,a2],[[n,member],[[v,a1],[v,a2]]],
                [v,b1]]]
                ],
                
                [v,b]]]
        ]]
        
],[[[[v,b],[[1,11,111]]]]]).

test(105b,[[n,findall1],[[1,2,3],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[[v,a1],[v,a1]],[[[n,member],[[v,a],[v,a1]]],[[n,cut]]],
                [v,b]]]
        ]]
        
],[[[[v,b],[[1,1]]]]]).


**/

test(106,[[n,call1a],[[v,b],[1,11,111]]],

[
        [[n,call1a],[[v,b],[v,a]],":-",
        [       [[n,call],[[n,member2a],[[v,b],[v,a]]]]]
        ],
        
        [[n,member2a],[[v,b],[v,a]],":-",
        [       [[n,member],[[v,b],[v,a]]],[[n,cut]]]
        ]
        
],[[[[v,b],1]]]).

test(107,[[n,call1b],[[1,11,111],[v,b]]],

[
        [[n,call1b],[[v,a],[v,b]],":-",
        [
        
        		  %[[n,equals4_on]], called algorithm isn't treated with equals4 anyway
        		  
       [[n,call],[[lang,same],same,[[n,member2a],[[v,a],[v,b]]],
[[[n,member2a],[[v,a],[v,b]],":-",
        [       [[n,member],[[v,b],[v,a]]],[[n,cut]]]
        ]]]],
        
            [[n,equals4_on]]
]]       
        
],[[[[v,b],1]]]).



test(108,[[n,call1b],[[1,11,111],[v,b]]],
        %[[[n,call1b],[[[t,brackets],[[t,number],[t,number],[t,number]]],[t,number]]]],
        %[[[n,call1b],[input,output]]],

[
        %[[n,equals4_off]],
        [[n,call1b],[[v,a],[v,b]],":-",
        [       [[n,call],[[lang,same],same,[[n,member2a],[[v,a],[v,b]]],
        [[[n,member2a],[[[t,number],[t,number],[t,number]],[t,number]]]],
        [[[n,member2a],[input,output]]],

[[[n,member2a],[[v,a],[v,b]],":-",
        [       [[n,member],[[v,b],[v,a]]],[[n,cut]]]
        ]]]],
        [[n,cut]],
        [[n,equals4_on]]
        ]]       
        
],[[[[v,b],1]]]).


test(109,[[n,middle],[2,[v,b]]],
[
        [[n,middle],[[v,a],[v,b]],":-",
        [       [[n,/],[[v,a],2,[v,b]]]
        ]]       
        
],[[[[v,b],1]]]).

test(110,[[n,level_with],[170,[v,b]]],
[
        [[n,level_with],[[v,a],[v,a]]]      
        
],[[[[v,b],170]]]).

test(111,[[n,tra_las],[5,[v,a]]],
[
        [[n,tra_las],[[v,n],[v,a]],":-",
        [       [[n,las],[[v,n],[],[v,b]]],
                [[n,append],[["tra"],[v,b],[v,a]]]
        ]],       

        [[n,las],[0,[v,l],[v,l]]],
        [[n,las],[[v,l],[v,m],[v,n]],":-",
        [       [[n,-],[[v,l],1,[v,h]]],
                [[n,append],[[v,m],["la"],[v,o]]],
                [[n,las],[[v,h],[v,o],[v,n]]]
        ]
        ]
        
],[[[[v,a],["tra","la","la","la","la","la"]]]]).

test(112,[[n,final_gong],[5,[v,a]]],
[
        [[n,final_gong],[[v,n],[v,a]],":-",
        [       [[n,-],[[v,n],1,[v,n1]]],
                [[n,dashes],[[v,n1],[],[v,b]]],
                [[n,append],[[v,b],["gong"],[v,a]]]
        ]],       

        [[n,dashes],[0,[v,l],[v,l]]],
        [[n,dashes],[[v,l],[v,m],[v,n]],":-",
        [       [[n,-],[[v,l],1,[v,h]]],
                [[n,append],[[v,m],["-"],[v,o]]],
                [[n,dashes],[[v,h],[v,o],[v,n]]]
        ]
        ]
        
],[[[[v,a],["-","-","-","-","gong"]]]]).

test(113,[[n,bedroom_to_garden],["bedroom",[v,b]]],
[
        [[n,bedroom_to_garden],["bedroom","garden"]]      
        
],[[[[v,b],"garden"]]]).

test(114,[[n,stop_at_top],[["-","-","-","top"],[v,a]]],
[
        [[n,stop_at_top],[[],"fail"]],
        [[n,stop_at_top],[[v,l],"success"],":-",
        [       [[n,head],[[v,l],"top"]]
        ]],
                [[n,stop_at_top],[[v,l],[v,n]],":-",
        [       [[n,head],[[v,l],"-"]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,stop_at_top],[[v,t],[v,n]]]
        ]
        ]
        
],[[[[v,a],"success"]]]).


% Program finder 6 3 21

test(115,[[n,function],[[["n1","a"]],[["a",5]],[],[v,result]]],
[[[n,function],[[],[v,inputs2],[v,output],[v,output]]],[[n,function],[[v,input1],[v,inputs2],[v,inputs3],[v,output]],":-",[[[n,head],[[v,input1],[v,head]]],[[n,tail],[[v,input1],[v,tail]]],[[n,equals1],[[v,head],[[v,a],[v,b]]]],[[[n,string],[[v,a]]],[[n,string],[[v,b]]]],[[n,head],[[v,inputs2],[v,head1]]],[[n,tail],[[v,inputs2],[v,tail1]]],[[n,equals1],[[v,head1],[[v,b],[v,c]]]],[[[n,number],[[v,c]]]],[[n,equals2],[[v,item1],[[v,a],[v,c]]]],[[n,wrap],[[v,item1],[v,item1a]]],[[n,append],[[v,inputs3],[v,item1a],[v,item2]]],[[n,function],[[v,tail],[v,tail1],[v,item2],[v,output]]]]]]

,[[[[v,result],[["n1", 5]]]]]).

% Split after ".","!","?", producing "" if one of these characters is at the start

test(116,[[n,grammar1],[".aaa.bbb.",[".","?"],[v,t]]],
%test(17,[[n,grammar1],["aaa1 ,-'! a? b! b.",[v,t]]],
%%test(15,[[n,compound213],["","",[["a"],1],[v,t]]],

[
		  [[n,grammar1],[[v,u],[v,cs],[v,t]],":-",
		  [
		  			 [[n,compound21],[[v,u],"",[v,cs],[],[v,t]]]
		  			 %%[[n,number21],[[v,u],"","",[v,t]]]
		  			 %%[[n,compound213],["","",[["a"],1],[v,t]]]
		  ]
		  ],

		  [[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]], %% swapped these

		  [[n,compound],[[v,cs],[v,t],[v,u]],"->",
		  [[[n,compound21],[[v,cs],[v,t],[v,v]]],
		  [[n,compound213],[[v,v],[v,u]]]]],

		  [[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],["","",[v,cs],[],[""]]],

		  [[n,compound21],[[v,cs],[v,t],[v,u]],"->",
		  [[[n,item],[[v,i],[v,cs]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],

		  [[n,compound21],[[v,cs],[v,t],[v,u]],"->",
		  [[[n,item],[[v,i],[v,cs]]],%" ",
		  [[n,compound21],[[v,cs],[],[v,compound1name]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]]]],
/**
		  [[n,item],[[v,t]],"->",
		  [[[n,number21],["",[v,t]]]]],
**/
		  [[n,item],[[v,t],[v,cs]],"->",[[[n,word21],[[v,cs],"",[v,t]]]]],

		  [[n,item],[[v,t],[v,cs]],"->",[[[n,compound],[[v,cs],[],[v,t]]]]],
/**
		  [[n,number212],["","",[v,t],[v,t]]],

		  [[n,number212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,number212],[[v,v],[v,u]]]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,number21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]],
**/
		  [[n,word212],["","",[v,t],[v,t]]],

		  [[n,word212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,word213],["","",[v,t],[v,t]]],

/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],
**/
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  [v,b],%[[n,lookahead1],[[v,cs]]],
		  [[n,code],%[[n,sentencechars],[[v,a]]],
		  [[n,finalchar],[[v,b],[v,cs]]]
		  %[[n,stringconcat],[[v,t],[v,a],[v,v1]]],
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  ],
		  [[n,word212],[[v,t],[v,u]]]]],

		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  [v,b],%[[n,lookahead1],[[v,cs]]],
		  [[n,code],%[[n,sentencechars],[[v,a]]],
		  %[[n,trace]],
		  [[n,sentencechars],[[v,b],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,b],[v,v1]]]
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  ],
		  [[n,word213],[[v,v1],[v,u]]]]],

/** nothing in string
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  "",%[[n,lookahead],[[v,c]]],
		  %[[n,code],%[[n,sentencechars],[[v,a]]],
		  %[[n,finalchar],[[v,b],[v,cs]]]
		  %[[n,stringconcat],[[v,t],[v,a],[v,v1]]],
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  %],
		  %[[n,code],%[[n,sentencechars],[[v,b],[v,cs]]],
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]],
		  %[[n,not],[[n,finalchar],[[v,c],[v,cs]]]]],
		  [[n,word212],[[v,t],[v,u]]]]],
**/
/**
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  %[[n,code],[[n,trace]]],
		  [v,b],[[n,lookahead],[[v,c]]],
		  [[n,code],[[n,sentencechars],[[v,b],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,b],[v,v]]],
		  [[n,not],[[n,finalchar],[[v,c],[v,cs]]]]]
		  %[[n,finalchar],[[v,b],[v,cs]]]
		  %[[n,stringconcat],[[v,t],[v,a],[v,v1]]],
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  ,
		  [[n,word212],[[v,v],[v,u]]]]],
**/
/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,word21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]]

**/
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,sentencechars],[[v,a],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word21],[[v,cs],"",[v,wordstring]]],
		  [[n,code],
		  [[n,stringconcat],[[v,v],[v,wordstring],[v,u]]]]]],
		  
		  [[n,sentencechars],[[v,c],[v,cs]],":-",
		  [[[n,not],[[[n,member],[[v,c],[v,cs]]]]]]],

/**		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,letters],[[v,c]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[[n,stringtonumber],[[v,c],[v,n]]],
		  [[n,number],[[v,n]]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c]," "]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],","]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"-"]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"'"]]]],
		  
		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"."]]]],

**/
		  [[n,finalchar],[[v,c],[v,cs]],":-",
		  [[[n,member],[[v,c],[v,cs]]]]],

/**
		  [[n,lookahead1],[[v,c],[v,cs]],":-", %?
		  [[[n,member],[[v,cs],[v,c]]],
		  [[n,lookahead],[[v,c]]]]],

		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"!"]]]],

		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"?"]]]]
		  **/

		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]

%%],[[[v,t],[["a"],1]]]).
%],[[[[v,t],["aaa1 ,-'!","a?","b!","b."]]]]).
],[[[[v,t],["","aaa","bbb"]]]]).

test(117,[[n,grammar1],["a   a. a ",[" ","."],[v,t]]],
%test(17,[[n,grammar1],["aaa1 ,-'! a? b! b.",[v,t]]],
%%test(15,[[n,compound213],["","",[["a"],1],[v,t]]],

[
		  [[n,grammar1],[[v,u],[v,cs],[v,t]],":-",
		  [
		  			 [[n,compound21],[[v,u],"",[v,cs],[],[v,t]]]
		  			 %%[[n,number21],[[v,u],"","",[v,t]]]
		  			 %%[[n,compound213],["","",[["a"],1],[v,t]]]
		  ]
		  ],

		  %[[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]], %% swapped these

		  [[n,compound],[[v,cs],[v,t],[v,u]],"->",
		  [[[n,compound21],[[v,cs],[v,t],[v,v]]],
		  [[n,compound213],[[v,v],[v,u]]]]],

		  %[[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],["","",[v,cs],[],[""]]],

		  [[n,compound21],[[v,cs],[v,t],[v,u]],"->",
		  [[[n,item],[[v,i],[v,cs]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],

		  [[n,compound21],[[v,cs],[v,t],[v,u]],"->",
		  [[[n,item],[[v,i],[v,cs]]],%" ",
		  [[n,compound21],[[v,cs],[],[v,compound1name]]],
		  [[n,code],%%[[n,stringconcat],[[v,i],".",[v,i2]]],
		  [[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]]]],
/**
		  [[n,item],[[v,t]],"->",
		  [[[n,number21],["",[v,t]]]]],
**/
		  [[n,item],[[v,t],[v,cs]],"->",[[[n,word21],[[v,cs],"",[v,t]]]]],

		  [[n,item],[[v,t],[v,cs]],"->",[[[n,compound],[[v,cs],[],[v,t]]]]],
/**
		  [[n,number212],["","",[v,t],[v,t]]],

		  [[n,number212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,number212],[[v,v],[v,u]]]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,number21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]],
**/
		  [[n,word212],["","",[v,t],[v,t]]],

		  [[n,word212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,word213],["","",[v,t],[v,t]]],

/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],
**/
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  [v,b],%[[n,lookahead1],[[v,cs]]],
		  [[n,code],%[[n,sentencechars],[[v,a]]],
		  [[n,finalchar],[[v,b],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,b],[v,v1]]]
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  ],
		  [[n,word212],[[v,v1],[v,u]]]]],

		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  [v,b],[[n,lookahead],[[v,c]]],
		  [[n,code],
		  %[[n,trace]],
		  [[n,finalchar],[[v,c],[v,cs]]],
		  %
		  [[n,sentencechars],[[v,b],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,b],[v,v1]]]
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  ],
		  [[n,word212],[[v,v1],[v,u]]]]],

		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  [v,b],%[[n,lookahead],[[v,c]]],
		  [[n,code],
		  %[[n,trace]],
		  %[[n,sentencechars],[[v,c],[v,cs]]],
		  %
		  [[n,sentencechars],[[v,b],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,b],[v,v1]]]
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  ],
		  [[n,word213],[[v,v1],[v,u]]]]],

/** nothing in string
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  "",%[[n,lookahead],[[v,c]]],
		  %[[n,code],%[[n,sentencechars],[[v,a]]],
		  %[[n,finalchar],[[v,b],[v,cs]]]
		  %[[n,stringconcat],[[v,t],[v,a],[v,v1]]],
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  %],
		  %[[n,code],%[[n,sentencechars],[[v,b],[v,cs]]],
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]],
		  %[[n,not],[[n,finalchar],[[v,c],[v,cs]]]]],
		  [[n,word212],[[v,t],[v,u]]]]],
**/
/**
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [%[v,a],
		  %[[n,code],[[n,trace]]],
		  [v,b],[[n,lookahead],[[v,c]]],
		  [[n,code],[[n,sentencechars],[[v,b],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,b],[v,v]]],
		  [[n,not],[[n,finalchar],[[v,c],[v,cs]]]]]
		  %[[n,finalchar],[[v,b],[v,cs]]]
		  %[[n,stringconcat],[[v,t],[v,a],[v,v1]]],
		  %[[n,stringconcat],[[v,t],[v,b],[v,v]]]
		  ,
		  [[n,word212],[[v,v],[v,u]]]]],
**/
/**
		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
		  [[n,number],[[v,a1]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
	 	  [[n,word21],["",[v,numberstring]]],
		  [[n,code],[[n,stringconcat],
		  [[v,v],[v,numberstring],[v,u]]]]]]

**/
		  [[n,word21],[[v,cs],[v,t],[v,u]],"->",
		  [[v,a],
		  % lookahead sent
		   %[[n,lookahead],[[v,c]]],
		  [[n,code],[[n,sentencechars],[[v,a],[v,cs]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word21],[[v,cs],"",[v,wordstring]]],
		  [[n,code],
		  [[n,stringconcat],[[v,v],[v,wordstring],[v,u]]]]]],
		  
		  [[n,sentencechars],[[v,c],[v,cs]],":-",
		  [[[n,not],[[[n,member],[[v,c],[v,cs]]]]]]],

/**		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,letters],[[v,c]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[[n,stringtonumber],[[v,c],[v,n]]],
		  [[n,number],[[v,n]]]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c]," "]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],","]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"-"]]]],

		  [[n,sentencechars],[[v,c]],":-",
		  [[[n,=],[[v,c],"'"]]]],
		  
		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"."]]]],

**/
		  [[n,finalchar],[[v,c],[v,cs]],":-",
		  [[[n,member],[[v,c],[v,cs]]]]],

/**
		  [[n,lookahead1],[[v,c],[v,cs]],":-", %?
		  [[[n,member],[[v,cs],[v,c]]],
		  [[n,lookahead],[[v,c]]]]],

		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"!"]]]],

		  [[n,finalchar],[[v,c]],":-",
		  [[[n,=],[[v,c],"?"]]]]
		  **/

		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [%[[n,trace]],
		  [[n,stringconcat],[[v,b],[v,d],[v,a]]],
		  
[[n,string_length],[[v,b],1]]
		  
		  %[[n,trace]],
		  %[[n,string_length],[[v,b],1]]]]
		  ]]

%%],[[[v,t],[["a"],1]]]).
%],[[[[v,t],["aaa1 ,-'!","a?","b!","b."]]]]).
],[[[[v,t],["a", " ", " ", " ", "a", ".", " ", "a", " "]]]]).

% a types state machine

% v1 checked by type1 etc

/**
        [[[n,connect_cliques],[[t,list2],[t,list2],[t,list2]]],
        [[t,item],[[t,number]]],
        [[t,item],[[t,string]]],
        [[t,list2],[[[t,list],[[t,item]]]]],
        [[t,list2],[[[t,list],[[t,list2]]]]]],
        [[[n,connect_cliques],[input,input,output]]],

**/

% list type accepts whole not partial repeats of types

/**
test(118,[[n,connect_cliques_types],[[["a",1],[1,2],[2,"b"]],[["a",3],[3,4],[4,"b"]],[["a",1],[1,2],[2,"b"],["a",3],[3,4],[4,"b"]]]],

[
		  [[n,connect_cliques_types],[[v,vtp1],[v,vtp2],[v,vtp3]],":-",
[	[[n,list2_types],[[v,vtp1]]],
	[[n,list2_types],[[v,vtp2]]],
	[[n,list2_types],[[v,vtp3]]]]], % not in check types on entry, just exit

% t->n

[[[n,types],[[v,vt],[[v,vtp1]]]],":-",

% like curr type checker
% put up with non (what would appear in lpi) type checking code in trace, will also affect findall, maplist, forall, intersection, etc.
% - i.e. don't use trace1, notrace1
% x only trace1s wanted type checking trace statements x trace all if on x trace1 switched off whenever unnecessary

***


[[n,item_types],[[v,vtp1]],":-",
[	[[n,number_types],[[v,vtp1]]]]],

[[n,item_types],[[v,vtp1]],":-",
[	[[n,string_types],[[v,vtp1]]]]],

[[n,list2_types],[[v,vtp1]],":-",
[	[[n,list_types],[[v,vtp1]]]]],


[[n,list_types],[[v,vtp1]],":-",
[	%[[n,brackets_type],[[[[t,list_types],[[t,item_types]]]],[[v,vtp1]]]] ** % make types args rather than in sm
[[n,list_type],[[[t,item_types]]]],[[v,vtp1]]]] ** % make types args rather than in sm

[[n,brackets_type],[[[v,vt1]],[[v,vtp1]]],":-", % could be vt1 vt2 vtp1 vtp2

[	[[n,notrace1]],
% repeats items until finished
	[[n,list_types_1],[[v,vt1],[v,vtp1]]], % ["a",1]
	[[n,trace1]],
	[[n,list_types],[[n,list_types],[[v,vtp1]]]]]],

[[n,list_types_1],[[]]],
[[n,list_types_1],[[v,vtp1]],":-",
[	[[n,equals4],[[v,vtp1],[[[v,vtp2],"|",[v,vtp3]]]]],
	[[n,trace1]],
	[[n,item_types],[[v,vtp2]]],
	[[n,notrace1]],
	[[n,list_types_1],[[v,vtp3]]]]],
	
[[n,list2_types],[[v,vtp1]],":-",
[	[[n,list2_types],[[v,vtp1]]]]],

[[n,trace1],":-",
[	[[n,trace]]]],

[[n,notrace1],":-",
[	[[n,notrace]]]]

],[[]]).
**/

% trace on for writelns



%test(118,[[n,extract_modes2],[[[t,string],[t,string]],[],[v,typestatements3],["yes","yes"],[],[v,vars3],[input,output]]],
test(118,%[[n,types],["on"]],
[[n,query_pred]],
%[[n,is_list],[[1,2,3]]],
%[[n,extractmodes2],[[[t,string]],[],[v,typestatements3],["yes"],[],[v,vars3],[out]]],

/*[[n,checktypes_inputs],

[[n,pred],["yes","yes"],

[[[n,pred],[[t,string],[t,string]]]],

[[[n,pred],[in,out]]]]],


[[n,checktypes_inputs],

[["n","want_baby"],["yes","yes","yes","yes"],

[[["n","want_baby"],[["t","string"],["t","string"],["t","string"],["t","string"]]]],

[[["n","want_baby"],["in","in","in","out"]]]]],
*/
%/** *** THESE
%/*
[
		  [[n,query_pred],":-",
		  [[[n,equals4_on]],

[[n,checktypes_inputs],

[[n,want_baby],["yes"],

[[[n,want_baby],[[t,string]]]],

[[[n,want_baby],[in]]]]],

		  [[n,equals4_on]]

]],
%*/
%**/

% the type checker sm is better than the type command anyway because it will work with skip and retry in trace
% - use normal trace, notrace on checktypes, works with skip, retry (trace before checktypes, if exits or fails, turns off trace)
% later: $ trace status to display


[[n,types],["off"]], % need assertz command in ssi, not in lpi

[[n,checktypes_inputs],[[v,function],[v,vars1],[v,typestatements1],[v,modestatements1]],":-", % need these last 2 vars for out check as well
[
	[[n,"->"],[[[n,types],["on"]],
	%[[n,typestatements],[[v,typestatements1]]],[[n,modestatements],[[v,modestatements1]]],
	[[n,checktypes0_inputs],[[v,function],[v,vars1],[v,typestatements1],[v,modestatements1]]],[[n,true]]]],
	[[n,cut]]
]],
[[n,checktypes0_inputs],[[v,function],[v,vars1],[v,typestatements1],[v,modestatements1]],":-",
[
	[[n,length],[[v,vars1],[v,l]]],
	[[n,is],[[v,l],0]],
	[[n,equals4],[[v,vars1],[]]],
	[[n,get_lang_word],["in type check",[v,input_type_check]]], % need this command
	[[n,"->"],[[[n,types],["on"]],[[n,debug_types_call],[[[v,function],"/","~",[v,l],[v,input_type_check]]]],[[n,true]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],
	[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_exit],[[[v,function],"/","~",[v,l],[v,input_type_check]]]]],[[n,true]]]],
	[[n,cut]]
]],
[[n,checktypes0_inputs],[[v,function],[v,vars1],[v,typestatements1],[v,modestatements1]],":-",
[
	[[n,length],[[v,vars1],[v,l]]],
	[[n,get_lang_word],["in type check",[v,input_type_check]]],
	[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_call],[[[v,function],"/","~",[v,l],[v,input_type_check]]]]],[[n,true]]]],
	
	%[[n,trace]],
	[[[[n,member3],[[[v,function],"|",[[v,typestatements2]]],[v,typestatements1]]],[[n,member3],[[[v,function],"|",[[v,modestatements2]]],[v,modestatements1]]],[[n,extractmodes1],[[v,typestatements2],[v,typestatements3],[v,vars1],[v,vars2],[v,modestatements2]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[v,function],[v,vars2]]]],[[n,true]]]],[[n,"->"],[[[[n,checktypes1],[[v,vars2],[v,typestatements3],[v,typestatements3],[v,typestatements1]]]],[[[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[v,function],[v,vars2]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_exit],[[[v,function],"/","~",[v,l],[v,input_type_check]]]]],[[n,true]]]]]],[[[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_fail],[[[v,function],"/","~",[v,l],[v,input_type_check]]]]],[[n,true]]]]]]]]]],
	[[n,cut]]
]],
[[n,extractmodes1],[[v,typestatements1],[v,typestatements3],[v,vars1],[v,vars2],[v,modestatements1]],":-",
[
	[[n,extractmodes2],[[v,typestatements1],[],[v,typestatements3],[v,vars1],[],[v,vars2],[v,modestatements1]]],
	[[n,cut]]
]],

[[n,extractmodes2],[[],[v,typestatements2a],[v,typestatements2a],[],[v,vars],[v,vars],[]],":-",
[
	[[n,cut]]
]],
[[n,extractmodes2],[[v,typestatements1],[v,typestatements2a],[v,typestatements3],[v,vars1],[v,vars2],[v,vars3],[v,modestatements1]],":-",
[
	[[n,get_lang_word],["in",[v,in]]],
	
		%[[n,trace]],
	%[[n,writeln],[[[v,typestatements1],[v,typestatements2a],[v,typestatements3],[v,vars1],[v,vars2],[v,vars3],[v,modestatements1]]]],

	%[[n,writeln],[[v,modestatements1]]],
	
	[[n,equals4],[[v,modestatements1],[[v,in],"|",[v,modestatements3]]]],
	[[n,equals4],[[v,typestatements1],[[v,typestatements2],"|",[v,typestatements3a]]]],
	[[n,equals4],[[v,vars1],[[v,vars11],"|",[v,vars12]]]],
	
	%[[n,trace]],
	[[n,append],[[v,typestatements2a],[[v,typestatements2]],[v,typestatements4]]],
	[[n,append],[[v,vars2],[[v,vars11]],[v,vars4]]],
	[[n,extractmodes2],[[v,typestatements3a],[v,typestatements4],[v,typestatements3],[v,vars12],[v,vars4],[v,vars3],[v,modestatements3]]]
	,[[n,cut]] %**
]],
[[n,extractmodes2],[[v,typestatements1],[v,typestatements2a],[v,typestatements3],[v,vars1],[v,vars2],[v,vars3],[v,modestatements1]],":-",
[
	[[n,get_lang_word],["out",[v,out]]],
	[[n,equals4],[[v,modestatements1],[[v,out],"|",[v,modestatements3]]]],
	[[n,equals4],[[v,typestatements1],[[v,typestatements2],"|",[v,typestatements3a]]]],
	[[n,equals4],[[v,vars1],[[v,vars11],"|",[v,vars12]]]],
	[[n,extractmodes2],[[v,typestatements3a],[v,typestatements2a],[v,typestatements3],[v,vars12],[v,vars2],[v,vars3],[v,modestatements3]]],
	[[n,cut]]
]],
[[n,checktypes],[[v,function],[v,vars1],[v,typestatements1],[v,modestatements1]],":-",
[
	[[n,"->"],[[[n,types],["on"]],[[[[n,typestatements],[[v,typestatements1]]],[[n,checktypes0],[[v,function],[v,vars1],[v,typestatements1]]]]],[[n,true]]]],
	[[n,cut]]
]],
[[n,checktypes0],[[v,function],[v,vars1],[v,typestatements1]],":-",
[
	[[n,get_lang_word],["Type check",[v,type_check]]],
	[[n,length],[[v,vars1],[v,l]]],
	[[n,is],[[v,l],0]],
	[[n,equals4],[[v,vars1],[]]],
	[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_call],[[[v,function],"/",[v,l],[v,type_check]]]]],[[n,true]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],
	[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_exit],[[[v,function],"/",[v,l],[v,type_check]]]]],[[n,true]]]],
	[[n,cut]]
]],
[[n,checktypes0],[[v,function],[v,vars1],[v,typestatements1]],":-",
[
	[[n,get_lang_word],["Type check",[v,type_check]]],
	[[n,length],[[v,vars1],[v,l]]],
	[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_call],[[[v,function],"/",[v,l],[v,type_check]]]]],[[n,true]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],
	[[n,"->"],[[[[[n,member3],[[[v,function],"|",[[v,typestatements2]]],[v,typestatements1]]],[[n,checktypes1],[[v,vars1],[v,typestatements2],[v,typestatements2],[v,typestatements1]]]]],[[[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_exit],[[[v,function],"/",[v,l],[v,type_check]]]]],[[n,true]]]]]],[[[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[v,function],[v,vars1]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[[n,debug_types_fail],[[[v,function],"/",[v,l],[v,type_check]]]]],[[n,true]]]]]]]],
	[[n,cut]]
]],
[[n,checktypes1],[[],[],[v,u1],[v,u2]],":-",
[
	[[n,cut]]
]],
[[n,checktypes1],[[v,vars1],[v,typestatements1],[v,typestatements2],[v,typestatements4]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["list",[v,dbw_list]]],
	[[n,equals4],[[v,vars1],[[v,vars2],"|",[v,vars3]]]],
	
	%[[n,trace]],
	
	[[n,is_list],[[v,vars2]]],
	
	%[[n,writeln],[[[v,typestatements1]]]],
	[[n,equals4],[[v,typestatements1],[[[[v,t],[v,dbw_list]],"|",[[v,typestatements3]]],"|",[v,typestatements4a]]]], 
	[[n,"->"],[[[n,types],["on"]],[[[n,debug_call],[[v,skip],[[[v,t],[v,dbw_list]],[v,typestatements3]]]]],[[n,true]]]],
	[[n,"->"],[[[[n,checktypes3],[[v,vars2],[v,typestatements3],[v,typestatements2],[v,typestatements4]]]],[[[[n,"->"],[[[n,types],["on"]],[[[n,debug_exit],[[v,skip],[[[v,t],[v,dbw_list]],[v,vars2]]]]],[[n,true]]]],[[n,checktypes1],[[v,vars3],[v,typestatements4a],[v,typestatements2],[v,typestatements4]]]]],[[n,"->"],[[[n,types],["on"]],[[[n,debug_fail],[[v,skip],[[[v,t],[v,dbw_list]],[v,vars2]]]]],[[n,true]]]]]]
]],
[[n,checktypes1],[[v,vars1],[v,typestatements1],[v,typestatements2],[v,typestatements4]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["list",[v,dbw_list]]],
	[[n,equals4],[[v,typestatements1],[[[[v,t],[v,dbw_list]],"|",[[v,typestatements3]]],"|",[v,typestatements4a]]]],
	[[n,"->"],[[[n,types],["on"]],[[[n,debug_call],[[v,skip],[[[v,t],[v,dbw_list]],[v,typestatements3]]]]],[[n,true]]]],
	[[n,"->"],[[[[n,checktypes3],[[v,vars1],[v,typestatements3],[v,typestatements2],[v,typestatements4]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[[v,t],[v,dbw_list]],[v,vars1]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[[v,t],[v,dbw_list]],[v,vars1]]]],[[n,true]]]]]]
]],
[[n,checktypes1],[[v,vars1],[v,typestatements1],[v,typestatements2],[v,typestatements4]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["brackets",[v,dbw_brackets]]],
	[[n,equals4],[[v,typestatements1],[[[[v,t],[v,dbw_brackets]],"|",[[v,typestatements3]]],"|",[v,typestatements4a]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[[v,t],[v,dbw_brackets]],[v,typestatements3]]]],[[n,true]]]],
	[[n,"->"],[[[[[n,equals4],[[v,vars1],[[v,vars2],"|",[v,vars3]]]],[[n,checktypes1],[[v,vars2],[v,typestatements3],[v,typestatements2],[v,typestatements4]]]]],[[[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[[v,t],[v,dbw_brackets]],[v,vars1]]]],[[n,true]]]],[[n,checktypes1],[[v,vars3],[v,typestatements4a],[v,typestatements2],[v,typestatements4]]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[[v,t],[v,dbw_brackets]],[v,vars1]]]],[[n,true]]]]]],
	[[n,cut]]
]],
[[n,checktypes1],[[v,vars1],[v,typestatements0],[v,typestatements1],[v,typestatements4]],":-",
[
	[[n,equals4],[[v,vars1],[[v,vars2],"|",[v,vars3]]]],
	[[n,equals4],[[v,typestatements0],[[v,typestatements2],"|",[v,typestatements3]]]],
	[[n,checktypes2],[[v,vars2],[v,typestatements2],[v,typestatements1],[v,typestatements4]]],
	[[n,checktypes1],[[v,vars3],[v,typestatements3],[v,typestatements1],[v,typestatements4]]]
]],
[[n,checktypes2],[[v,vars],[v,typestatements1],[v,typestatements2],[v,c]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["number",[v,dbw_number]]],
	[[n,equals4],[[v,typestatements1],[[v,t],[v,dbw_number]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[[v,t],[v,dbw_number]],[v,vars]]]],[[n,true]]]],
	[[n,"->"],[[[[n,number],[[v,vars]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[[v,t],[v,dbw_number]],[v,vars]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[[v,t],[v,dbw_number]],[v,vars]]]],[[n,true]]]]]]
]],
[[n,checktypes2],[[v,vars],[v,typestatements1],[v,typestatements2],[v,u1]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["predicatename",[v,dbw_predicatename]]],
	[[n,get_lang_word],["n",[v,dbw_n1]]],
	[[n,=],[[v,dbw_n1],[v,dbw_n]]],
	[[n,equals4],[[v,typestatements1],[[v,t],[v,dbw_predicatename]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[[v,t],[v,dbw_predicatename]],[v,vars]]]],[[n,true]]]],
	[[n,"->"],[[[[n,equals4],[[v,vars],[[[v,dbw_n],[v,u2]]]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[[v,t],[v,dbw_predicatename]],[v,vars]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[v,t],[v,dbw_predicatename]],[v,vars]]],[[n,true]]]]]]
]],
[[n,checktypes2],[[v,vars],[v,typestatements1],[v,typestatements2],[v,u1]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["string",[v,dbw_string]]],
	
	%[[n,trace]], ****
	
	[[n,equals4],[[v,typestatements1],[[v,t],[v,dbw_string]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[[v,t],[v,dbw_string]],[v,vars]]]],[[n,true]]]],
	[[n,"->"],[[[[n,string],[[v,vars]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[[v,t],[v,dbw_string]],[v,vars]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[[v,t],[v,dbw_string]],[v,vars]]]],[[n,true]]]]]]
]],
[[n,checktypes2],[[v,vars],[v,typestatements1],[v,typestatements2],[v,u1]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["any",[v,dbw_any]]],
	[[n,equals4],[[v,typestatements1],[[v,t],[v,dbw_any]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[[v,t],[v,dbw_any]],[v,vars]]]],[[n,true]]]],
	[[n,"->"],[[[[n,true]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[[v,t],[v,dbw_any]],[v,vars]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[[v,t],[v,dbw_any]],[v,vars]]]],[[n,true]]]]]]
]],
[[n,checktypes2],[[v,vars],[v,typestatements1],[v,typestatements2],[v,typestatements4]],":-",
[
	[[n,get_lang_word],["t",[v,t]]],
	[[n,get_lang_word],["list",[v,dbw_list]]],
	[[n,get_lang_word],["brackets",[v,dbw_brackets]]],
	[[n,get_lang_word],["number",[v,dbw_number]]],
	[[n,get_lang_word],["predicatename",[v,dbw_predicatename]]],
	[[n,get_lang_word],["string",[v,dbw_string]]],
	[[n,get_lang_word],["any",[v,dbw_any]]],
	[[n,equals4],[[v,typestatements1],[[v,t],[v,type]]]],
	[[[[n,not],[[[n,=],[[v,type],[v,dbw_list]]]]],[[n,not],[[[n,=],[[v,type],[v,dbw_brackets]]]]],[[n,not],[[[n,=],[[v,type],[v,dbw_number]]]]],[[n,not],[[[n,=],[[v,type],[v,dbw_predicatename]]]]],[[n,not],[[[n,=],[[v,type],[v,dbw_string]]]]],[[n,not],[[[n,=],[[v,type],[v,dbw_any]]]]]]],
	[[n,"->"],[[[n,types],["on"]],[[n,debug_call],[[v,skip],[[[v,t],[v,type]],[v,vars]]]],[[n,true]]]],
	[[n,"->"],[[[[[n,member3],[[[[v,t],[v,type]],"|",[[v,typestatements3]]],[v,typestatements4]]],[[n,"->"],[[[n,checktypes1],[[v,vars],[v,typestatements3],[v,typestatements2],[v,typestatements4]]],[[n,true]],[[n,checktypes1],[[[v,vars]],[v,typestatements3],[v,typestatements2],[v,typestatements4]]]]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_exit],[[v,skip],[[[v,t],[v,type]],[v,vars]]]],[[n,true]]]],[[n,"->"],[[[n,types],["on"]],[[n,debug_fail],[[v,skip],[[[v,t],[v,type]],[v,vars]]]],[[n,true]]]]]]
]],
[[n,checktypes3],[[],[v,u1],[v,typestatements2],[v,u2]],":-",
[
	[[n,cut]]
]],
[[n,checktypes3],[[v,vars],[v,typestatements3],[v,typestatements2],[v,typestatements6]],":-",
[
	[[n,length],[[v,typestatements3],[v,l]]],
	[[n,length],[[v,l1],[v,l]]],
	[[n,append],[[v,l1],[v,l2],[v,vars]]],
	[[n,checktypes1],[[v,l1],[v,typestatements3],[v,typestatements2],[v,typestatements6]]],
	[[n,checktypes3],[[v,l2],[v,typestatements3],[v,typestatements2],[v,typestatements6]]],
	[[n,cut]]
]],

[[n,debug_call],[[v,skip],[[v,function],[v,vars1]]],":-",[
[[n,writeln],[["debug_call",[v,function],[v,vars1]]]]
]],
[[n,debug_exit],[[v,skip],[[v,function],[v,vars1]]],":-",[
[[n,writeln],[["debug_exit",[v,function],[v,vars1]]]]
]],
[[n,debug_fail],[[v,skip],[[v,function],[v,vars1]]],":-",[
[[n,writeln],[["debug_fail",[v,function],[v,vars1]]]],
[[n,fail]]
]],
[[n,debug_types_call],[[v,function]],":-",[
[[n,writeln],[["debug_types_call",[v,function]]]]
]],
[[n,debug_types_exit],[[v,function]],":-",[
[[n,writeln],[["debug_types_exit",[v,function]]]]
]],
[[n,debug_types_fail],[[v,function]],":-",[
[[n,writeln],[["debug_types_fail",[v,function]]]],
[[n,fail]]
]],

	[[n,is_list],[[v,var]],":-",
	[[[n,=],[[v,var],[]]]]],

	[[n,is_list],[[v,var]],":-",
	[[[n,equals4],[[v,var],[[v,v1],"|",[v,v2]]]]]]


],
[[]]).

test(119,[[n,count],[2]],
[
        [[n,count],[[v,n]],":-",
        [
                [[n,=],[[v,n],1]]
        ]
        ],
        [[n,count],[[v,n]],":-",
        [
                [[n,=],[[v,n],2]]
        ]
        ],
        [[n,count],[[v,n]],":-",
        [
                [[n,=],[[v,n],3]]
        ]
        ]
] ,[[]]).


test(120,[[n,function],[1,[v,b],2,[v,a]]],
[
        [[n,function],[[v,a],[v,a],[v,b],[v,b]],":-",
        [
                [[n,true]]
        ]
        ]
]
, [[[[v,a],2],[[v,b],1]]]).

test(121,[[n,append1],[[v,a]]],
[
        [[n,append1],[[v,a]],":-",
        [
                [[n,a],[[v,a]]]
        ]
        ],
        [[n,a],[["a"]],":-",
        [
                [[n,true]]
        ]]
]
,[[[[v,a], ["a"]]]]).


test(122,[[n,equals4_on1]],
%test(122,[[n,compound],["[],1]",[v,u],["aa,]",b,"c",[]],[v,t]]],

[
/*
		  [[n,compound],["[]","",[],[v,t]],":-",
		  [
		  			 [[n,compound],[[v,u],"",[],[v,t]]]
		  ]
		  ],
*/
/* 
		  [[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]],
		  [[n,a],[[v,u],[v,u],[v,t],[v,t]]],
		  
		  [[n,compound],[[v,t],[v,u]],"->",
		  ["[",[[n,a],[[v,t],[v,v]]],
		  "]",
		  [[n,compound213],[[v,v],[v,u]]]]]

],
*/
		  [[n,equals4_on1],":-",
		  [[[n,equals4_on]]]]
],
		  
%[[[[v,u], ",1]"],[[v,t], ["aa,]",b,"c",[]]]]]).
[[]]).

		  

test(123,[[n,equals41],[[[v,b],"|",[v,c]]]],

[
        [[n,equals41],[[1,2,3]]]
        
],[[[[v,b],1],[[v,c],[2,3]]]]).


test(124,[[n,equals41],[[[[v,a],"|",[v,d]],[v,c],"|",[v,b]]]],

[
        [[n,equals41],[[[1,5],2,3,4]]]

],[[[[v,a],1],[[v,b],[3,4]],[[v,c],2],[[v,d],[5]]]]).


test(125,[[n,equals41],[[[[v,a],[v,c]],"|",[v,b]]]],

[
        [[n,equals41],[[[1,2],3,4]]]
        
],[[[[v,a],1],[[v,b],[3,4]],[[v,c],2]]]).

test(126,[[n,equals41],[[[v,a],"|",[v,b]]]],

[
        [[n,equals41],[[1,2,3,4]]]
        
],[[[[v, a], 1], [[v, b], [2, 3, 4]]]]).

test(127,[[n,equals41],[[[v,a],[v,c],"|",[v,b],[v,d]]]],

[
        [[n,equals41],[[1,2,3,4]]]
        
],[]).

test(128,[[n,equals41],[[[[v,a]],[v,c],"|",[v,b]]]],

[
        [[n,equals41],[[[1],2,3,4]]]
        
],[[[[v,a],1],[[v,b],[3,4]],[[v,c],2]]]).

test(129,[[n,equals41],[[[v,a],"|",[v,b]]]],

[
        [[n,equals41],[[[1,2],3,4]]]
        
],[[[[v, a], [1, 2]], [[v, b], [3, 4]]]]).

test(130,[[n,equals41],[[[v,a],"|",[[v,b]]]]],

[
        [[n,equals41],[[1,2]]]
        
],[[[[v, a], 1], [[v, b], 2]]]).

test(131,[[n,equals41],[[[v,a]]]],

[
        [[n,equals41],[[1]]]
        
],[[[[v, a], 1]]]).

test(132,[[n,equals41],[[[v,a],[v,b]]]],

[
        [[n,equals41],[[1,2]]]
        
],[[[[v, a], 1], [[v, b], 2]]]).

test(133,[[n,equals41],[[[v,a],[v,b]]]],

[
        [[n,equals41],[[[1,3],2]]]
        
],[[[[v, a], [1, 3]], [[v, b], 2]]]).


test(134,[[n,equals41],[[[v,a],[v,c],"|",[v,b],"|",[v,d]]]],

[
        [[n,equals41],[[1,2,3,4]]]
        
],[]).

test(135,[[n,equals41],[[1,2,3]]],

[
        [[n,equals41],[[1,2,3]]]
        
],[[]]).

test(136,[[n,equals41],[[[v,a],"|",[[v,b],"|",[v,d]]]]],

[
        [[n,equals41],[[1,2,3,4]]]
        
],[[[[v, a], 1], [[v, b], 2],[[v, d], [3,4]]]]).




test(137,[[n,equals41],[[v,b]]],

[
        [[n,equals41],[[v,b]],":-",
        [       [[n,equals42],[[[v,b],"|",[v,c]]]]
%        [       [[n,equals42],[[v,b]]]
        ]],

        [[n,equals42],[[1,2,3]]]

        
],[[[[v,b],1]]]).

test(138,[[n,equals41]],
[
		  [[n,equals41],":-",
		  [[[n,equals4_on]]]]
],
		  
[[]]).

test(139,[[n,append1],[[v,a],[v,d]]],
[
        [[n,append1],[[v,a],[v,d]],":-",
        [
                %[[n,equals4_on]],
                [[n,b],[[v,b]]],
                [[n,c],[[v,c]]],
                [[n,append],[[[v,b],[v,c]],[[v,c]],[[v,a],"|",[v,d]]]]
                %[[n,equals4_off]]
        ]
        ],
        [[n,b],["b"]],
        [[n,c],["c"]]
]
,[[[[v,a], "b"],[[v,d], ["c", "c"]]]]).

test(140,[[n,equals41],[[1,2,3],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,wrap],[[[v,a]],[v,b]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,b],[[[1,2,3]]]]]]).

test(141,[[n,equals41],[[1,2,3],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,head],[[[v,a]],[v,b]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,b],[1,2,3]]]]).

test(142,[[n,equals41],[[1,2,3],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,tail],[[[v,a]],[v,b]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,b],[]]]]).

test(143,[[n,equals41],[[1,2,3],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,member],[[v,b],[[v,a]]]],
                [[n,member],[[1,2,3],[[v,a]]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,b],[1,2,3]]]]).

test(144,[[n,equals41],[[1,2,3],[v,b]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,member],[[v,b],[[v,a]]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,b],[1,2,3]]]]).

test(145,[[n,equals41],[[[1,2,3]],[v,c]]],

[
        [[n,equals41],[[v,a],[v,c]],":-",
        [       [[n,member3],[[1,"|",[v,c]],[v,a]]
%        [       [[n,equals42],[[v,b]]]
        ]]]
        
],[[[[v,c],[2,3]]]]).

test(146,[[n,equals41],[[[1,2,3],4,5],[v,c]]],

[
        [[n,equals41],[[v,a],[v,c]],":-",
        [       %[[n,=],[[v,a],[[1,2,3],"|",[v,c]]]],
				          %[[n,is],[[v,a],[[1,2,3],"|",[v,c]]]],
				          [[n,equals3],[[[1,2,3],"|",[v,c]],[v,a]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,c],[4,5]]]]).

test(147,[[n,equals41],[[v,c]]],

[
        [[n,equals41],[[v,c]],":-",
        [       [[n,equals2],[[v,c],[1,[4,5]]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,c],[1,[4,5]]]]]).

test(148,[[n,equals41],[[v,c]]],

[
        [[n,equals41],[[v,a]],":-",
        [       [[n,equals3],[[[v,b],"|",[v,a]],[4,5,6]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,c],[5,6]]]]).

test(149,[[n,equals41],[[[1,2,3]],[v,b],[v,c]]],

[
        [[n,equals41],[[v,a],[v,b],[v,c]],":-",
        [       [[n,delete],[[v,a],[1],[[v,b],"|",[v,c]]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,b],[1,2,3]],[[v,c],[]]]]).


test(150,[[n,equals41],[[[4,5,6]],[v,c]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,maplist],[[n,append],[v,a],[1,2,3],[[v,b],"|",[v,d]]]]
        ]]
        
],[[[[v,c],1]]]).

test(151,[[n,equals41],[[[6,2,3],[5]],[v,c]]],

[
        [[n,equals41],[[v,a],[v,c]],":-",
        [       [[n,sort],[[v,a],[[5],"|",[v,c]]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,c],[[6,2,3]]]]]).

test(152,[[n,equals41],[[6,2,3],[1,2,3],[v,c]]],

[
        [[n,equals41],[[v,a],[v,b],[v,c]],":-",
        [       [[n,intersection],[[v,a],[v,b],[[v,c],"|",[v,d]]]]
%        [       [[n,equals42],[[v,b]]]
        ]]
        
],[[[[v,c],2]]]).

test(153,[[n,equals41],[[[4,5,6]],[v,c]]],

[
        [[n,equals41],[[v,a],[v,b]],":-",
        [       [[n,maplist],[[n,append],[v,a],[1,2,3],[[v,b],"|",[2,3,4,5,6]]]]
        ]]
        
],[[[[v,c],1]]]).


test(154,[[n,equals41],[[[4,5,6]],[v,c]]],

[
        [[n,equals41],[[v,a],[v,d]],":-",
        [       [[n,maplist],[[n,append],[v,a],[1,2,3],[1,"|",[v,d]]]]
        ]]
        
],[[[[v,c],[2,3,4,5,6]]]]).

test(155,[[n,equals41],[[v,a]]],

[
        [[n,equals41],[[v,a]],":-",
        [       [[n,equals4],[[v,a],[1,"|",[2,3]]]]
        ]]
        
],[[[[v,a],[1,2,3]]]]).


test(156,[[n,equals42],[[v,a],[[v,b],[v,c]]]],

[
        [[n,equals42],[[[v,d],[v,e]],[v,f]],":-",
        [       
        						[[n,equals4],[[[v,d],[v,e],[v,f]],[1,2,[3,4]]]]
        ]]
               
],[[[[v,a],[1,2]],[[v,b],3],[[v,c],4]]]).

test(157,[[n,equals41],[[v,a]]],

[
        [[n,equals41],[[[n,b],c]]]
        
],[[[[v,a],[[n,b],c]]]]).


test(158,[[n,equals41],[[v,a]]],

[
        [[n,equals41],[[v,a]],":-",
        [       [[n,equals42],[[v,a]]]
        ]],
        
        [[n,equals41],[1]]
        
],[[[[v,a],1]]]).


test(159,[[n,equals41]],
[
		  [[n,equals41],":-",
		  [[[n,equals4_on]]]]
],
		  
[[]]).

% formerly test 1

test(160,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                %[[n,equals4_off]],
                [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,[[[[v,c], 2]]]).

test(161,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
        %[[n,trace2]],
                [[n,function2],[[v,a],[v,b],[v,f]]],
                [[n,function2],[[v,f],[v,f],[v,c]]]
                
        ]
        ],
        [[n,function2],[[v,a],[v,f],[v,h]],":-",
        [
                [[n,+],[[v,a],[v,f],[v,h]]]
        ]
        ]
]
,[[[[v,c], 4]]]).


test(162,[[n,equals41],[[v,a],[v,b],[v,c]]],

[
        [[n,equals41],[[v,a],[v,b],[v,c]],":-",
        [       [[n,equals42],[[v,a],[[v,b],[v,c]]]]
        ]],

        [[n,equals42],[[[v,d],[v,e]],[v,f]],":-",
        [       %[[n,trace2]],
        						[[n,equals4],[[[v,d],[v,e],[v,f]],[1,2,[3,4]]]]
        ]]

        %[[n,equals42],[[1,2,[3,4]]]]
               
],[[[[v,a],[1,2]],[[v,b],3],[[v,c],4]]]).

test(163,[[n,function1],[[v,a],[v,a],[v,b],[v,b]]],
[
        [[n,function1],[[v,b],[v,b],[v,a],[v,a]],":-",
        [
                [[n,function],[1,[v,b],2,[v,a]]]
        ]
        ],
        
        [[n,function],[[v,a],[v,a],[v,b],[v,b]],":-",
        [
                [[n,true]]
        ]
        ]

]
,[[[[v,a],1],[[v,b],2]]]).


test(164,[[n,function1],[[v,a],[v,a],[v,b],[v,b]]],
[
        [[n,function1],[[v,b],[v,a],[v,a],[v,b]],":-", % should fail
        [
                [[n,function],[1,[v,b],2,[v,a]]]
        ]
        ],
        
        [[n,function],[[v,a],[v,a],[v,b],[v,b]],":-",
        [
                [[n,true]]
        ]
        ]

]
,[]).

test(165,[[[n,equals4],[[v,a],[1,2]]],[[n,equals4],[[v,b],[0,"|",[v,a]]]]],
[],
[[[[v,a],[1,2]],[[v,b],[0,1,2]]]]).

test(166,[[[n,equals4],[[v,a],0]],[[n,equals4],[[v,b],[[v,a],"|",[1,2]]]]],
[],
[[[[v,a],0],[[v,b],[0,1,2]]]]).

test(167,[[n,equals4],[[[v, a], "|", [v, b]], [1, "|", 2]]],
[],
[[[[v, a], 1], [[v, b], 2]]]).

test(168,[[n,function1],[[v,a],"|",[v,b]]],
[
        [[n,function1],[1,"|",2]]
]
,[[[[v, a], 1], [[v, b], 2]]]).

test(169,[[n,function1],[[v,a],[v,b]]],
[
        [[n,function1],[[v,a],[v,b]],":-",
        [
        	[[n,equals4],[[[v,a],[v,a]],[[1,[v,b]],[1,1]]]]
        ]]
]
,[[[[v, a], [1,1]], [[v, b], 1]]]).

test(170,[[n,function1],[[v,a],[v,a]]],
[
        [[n,function1],[[1,[v,b]],[1,1]]]
]
,[[[[v, a], [1,1]]]]).

test(171,[[n,function1],[[[]]]],
[
        [[n,function1],[[]]]
]
,[]).

test(172,[[n,equals4],[[],[[]]]],
[],
[]).

test(173,[[n,equals4],[[[v,d],[v,d],[v,a],[v,b],[v,a]],
[[v,e],[v,c],1,[v,c],[v,b]]]],
[],
[[[[v, a], 1], [[v, b], 1], [[v, c], 1], [[v, d], 1], [[v, e], 1]]]).


test(174,[[n,function1],[[v,e],[v,c],[v,a],[v,c],[v,a]]],
[
        [[n,function1],[[v,d],[v,d],1,[v,b],[v,b]]]
]
,[[[[v, a], 1], [[v, c], 1], [[v, e], 1]]]).

test(175,[[n,equals4],
	[
	[[[v,e1],[v,e2]]	,[v,c],[v,a],[v,c],[v,a],[v,e1]],
	[[v,d],[v,d],[[1,1],[1,1]],[v,b],[v,b],[[v,f],[v,f2]]]
	]],
[],
[[[[v,a],[[1,1],[1,1]]],[[v,b],[[1,1],[1,1]]],[[v,c],[[1,1],[1,1]]],[[v,d],[[1,1],[1,1]]],[[v,e1],[1,1]],[[v,e2],[1,1]],[[v,f],1],[[v,f2],1]]]).



test(176,[[n,function1],
	[[[v,e1],[v,e2]]	,[v,c],[v,a],[v,c],[v,a],[v,e1]]],

[
        [[n,function1],
	[[v,d]           ,[v,d],[[1,1],[1,1]],[v,b],[v,b],[[v,f],[v,f2]]]]
],
[[[[v, a], [[1,1],[1,1]]], [[v, c], [[1,1],[1,1]]], [[v, e1], [1,1]],[[v, e2], [1,1]]]]).

test(177,[[n,equals4],
	[[[[v,e1],[v,e2]]	,"|",[[v,c],[v,a],[v,c],[v,a],[v,e1]]],
	[[v,d]          ,"|",[[v,d],[[1,1],[1,1]],[v,b],[v,b],[[v,f],[v,f2]]]]]],
[],
[[[[v,a],[[1,1],[1,1]]],[[v,b],[[1,1],[1,1]]],[[v,c],[[1,1],[1,1]]],[[v,d],[[1,1],[1,1]]],[[v,e1],[1,1]],[[v,e2],[1,1]],[[v,f],1],[[v,f2],1]]]).


test(178,[[n,function1],
	[[[v,e1],[v,e2]]	,"|",[[v,c],[v,a],      [v,c],[v,a],[v,e1]]]],

[
        [[n,function1],
	[[v,d],          "|",[[v,d],[[1,1],[1,1]],[v,b],[v,b],[[v,f],[v,f2]]]]]
],
[[[[v, a], [[1,1],[1,1]]], [[v, c], [[1,1],[1,1]]], [[v, e1], [1,1]],[[v, e2], [1,1]]]]).

test(179,[[n,add0]],
[
/* 
        [[n,add0],":-",
        [       [[n,add1],[[v,a]]],
        						[[n,writeln],["a"]]]],
                
        [[n,add1],[[v,a]],":-",
        [       [[n,add2],[[v,a]]]]],        
        */
        [[n,add0],":-",
        [       %[[n,trace2]],
        						[[n,fail]]]]
]        
,[]).

test(180,[[n,add0]],
[
/* 
        [[n,add0],":-",
        [       [[n,add1],[[v,a]]],
        						[[n,writeln],["a"]]]],
                
        [[n,add1],[[v,a]],":-",
        [       [[n,add2],[[v,a]]]]],        
        */
        [[n,add0],":-",
        [       %[[n,trace2]],
        						[[n,true]]]]
]        
,[[]]).


test(181,[[n,1],[[1,2],[v,b]]],
[
        %[[n,query_box],[[v,a],[v,b]],":-",
        %[       [[n,1],[[v,a],[v,b]]]]],

	     [[n,add2],[[v,a],[v,b]],":-",
        [       [[n,=],[[v,a],[]]],
                [[n,=],[[v,b],[]]]]],
        [[n,add3],[[v,a],[v,b]],":-",
        [       [[n,tail],[[v,a],[v,b]]]]],
        
        %[[n,add0],[[v,a],[v,b]],":-",
        %[       [[n,1],[[v,a],[v,c]]],
        %        [[n,=],[[v,c],[v,b]]]]],
        
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,add2],[[v,a],[v,c]]],
                [[n,=],[[v,c],[v,b]]]]],
        [[n,1],[[v,a],[v,b]],":-",
        [       [[n,add3],[[v,a],[v,c]]],
                [[n,1],[[v,c],[v,d]]],
                [[n,=],[[v,d],[v,b]]]]]
],[[[[v, b], []]]]).

test(182,[[n,member_try],[[1,2,3],[v,b]]],
[
	[[n,member_try],[[v,a],[v,b]],":-",
	[	
		[[n,member],[[v,b],[v,a]]],
		[[n,equals4],[[v,b],2]]]
	]
]
      
,[[[[v, b], 2]]]).


test(183,[[n,not1]],
[
	[[n,not1],":-",
	[	
		[[n,not],[[[n,equals4],[3,2]]]]%,
		%[[n,true]]
	]]
]
      
,[[]]).
      
      
test(184,[[n,not1]],
[
	[[n,not1],":-",
	[	
		[[n,not],[[[[n,equals4],[3,2]],[[n,equals4],[3,2]]]]],
		[[n,true]]
	]]
]
      
,[[]]).
      
      
test(185,[[n,not1]],
[
	[[n,not1],":-",
	[	
		[[n,not],[[[[n,equals4],[2,2]],[[n,equals4],[2,2]]]]],
		[[n,true]]
	]]
]
      
,[]).

test(186,[[n,brackets1]],
[
	[[n,brackets1],":-",
	[	
		%[[n,true]],
		%[[[n,true]],[[n,true]]]
		%[[n,true]],
		[[[n,true]],%[[n,true]],
		[[[n,true]],[[n,true]]]]
	]]
]
      
,[[]]).

test(187,[[n,brackets1]],
[
	[[n,brackets1],":-",
	[	
		%[[n,true]],
		%[[[n,true]],[[n,true]]]
		%[[n,true]],
		%*[[[n,writeln],["1"]],%[[n,true]],
		%*[[[n,writeln],["2"]],[[n,writeln],["3"]]],
		%[[n,writeln],["3.5"]],
		[[[n,writeln],["4"]]%,[[n,writeln],["5"]]
		],
		[[[n,writeln],["6"]],[[n,writeln],["7"]]]
	]]
]
      
,[[]]).

test(188,[[n,brackets1]],
[
	[[n,brackets1],":-",
	[	
		%[[n,true]],
		%[[[n,true]],[[n,true]]]
		%[[n,true]],
		[[[n,true]],%[[n,true]],
		[[[n,true]]%,[[n,trace2]]
		]]
	]]
]
      
,[[]]).

test(189,[[n,brackets1]],
[
	[[n,brackets1],":-",
	[		[[n,true]]
	]]
]
      
,[[]]).

test(190,[[n,true]],
[]
      
,[[]]).

test(191,[[n,not1]],
[
	[[n,not1],":-",
	[		[[n,not],[[[n,member],["not",["would","you","like","a","walk"]]]]]
	]]
]
      
,[[]]).

/*
test(192,[[n,a]],
[
	[[n,a],":-",
	[		[[[n,writeln],["1"]],

	[[n,writeln],["2"]]],
	
	
	[[n,writeln],["3"]],
	[[n,false]]
	]]
]
      
,[]).

*/

test(192,[[n,findall1],[[1,2,3],[v,b]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[v,b1],[[[n,member],[[v,a1],[v,a]]],
        
        %[[n,findall],[[v,a3],[[[n,member],[[v,a1],[v,a2]]],
        [[n,+],[[v,a1],2,[v,b1]]]],
                %[v,b1]]]],
                
                [v,b]]]	
        ]]
        
],[[[[v,b],[3,4,5]
%[[1,11,111],[2,22,222],[3,33,333]]
]]]).

test(193,[[n,findall1],[[[[1,2,3,4]]],[v,b]]],%[[[[1],[2]],[[3],[4]]],[v,b]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,true]],
        [[n,findall],[[v,b1],[[[n,member],[[v,a1],[v,a]]],
        
        [[n,findall],[[v,b2],[[[n,member],[[v,a2],[v,a1]]],
        
        [[n,findall],[[v,a4],[[[n,member],[[v,a3],[v,a2]]],
        [[n,+],[[v,a3],4,[v,a4]]]],
                [v,b2]]]],
                [v,b1]]]],
                
                [v,b]]]	
        ]]
        
],[[[[v,b],[[[5,6,7,8]]]%[[[5],[6]],[[7],[8]]]
%[[1,11,111],[2,22,222],[3,33,333]]
]]]).


test(194,[[n,cut1],[[v,a]]],%[[[[1],[2]],[[3],[4]]],[v,b]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
		  [[n,cut1],[[v,a]],":-",
		  [[[n,findall],[[v,a1],[[[n,member],[[v,a1],[1,2]]]%,
		  %,[[n,cut]]
		  ],[v,a]]]]]
        
],[[[[v,a],[1,2]%[[[5],[6]],[[7],[8]]]
%[[1,11,111],[2,22,222],[3,33,333]]
]]]).


test(195,%[[n,or12],[[v,a]]],
[[n,findall],[[v,b],[[[n,or12],[[v,b]]]],[v,a]]],
[
        [[n,or12],[1],":-",[[[n,true]]]],
        [[n,or12],[2],":-",[[[n,fail]]]],
        [[n,or12],[3],":-",[[[n,fail]]]]
        
],[[[[v,a],[1]]]]).

test(196,%[[n,or12],[[v,a]]],
[[n,findall],[[v,b],[[[n,or12],[[v,b]]]],[v,a]]],
[
        [[n,or12],[1]]        
        
],[[[[v,a],[1]]]]).

test(197,%[[n,or12],[[v,a]]],
[[n,findall],[[v,b],[[[n,member],[[v,b],[1]]]],[v,a]]],
[
        
],[[[[v,a],[1]]]]).

test(198,
[[n,function]],

[[[n,function],":-",[[[n,not],[[[[n,false]],[[n,false]]]]],[[n,true]]]]],

[[]]).

test(199,
[[n,function],[1,1,[v,a]]],
[[[n,function],[[v,a],[v,b],[v,c]],":-",[[[[n,+],[[v,a],[v,b],[v,d]]],[[n,+],[[v,d],[v,d],[v,e]]]],[[n,-],[[v,e],[v,e],[v,c]]]]]]
,
[[[[v,a],0]]]).

test(200,
[[n,function]],
[[[n,function],":-",[[[n,or],[[[n,false]],[[n,true]]]],[[n,true]]]]],
[[]]).

test(201,
[[n,function]],
[[[n,function],":-",[[[n,"->"],[[[n,true]],[[n,true]]]],[[n,true]]]]],
[[]]).

test(202,
[[n,function]],
[[[n,function],":-",[[[n,"->"],[[[n,true]],[[n,true]],[[n,true]]]],[[n,true]]]]],
[[]]).

test(203,

%a(
%[[n,apply_all_to_all],[["m","vine"],[v,inventory4]]],%

[[n,traverse],[2,6]], %2,8
%[[n,apply_all_to_all],[["vine","c","k"],[v,inventory4]]],
[
%[[n,rainforest],[[[1,9,["*"]],[2,9,["*"]],[3,9,["*"]],[4,9,["*"]],[5,9,["*"]],[6,9,["*"]],[1,8,["*"]],[2,8,["m"]],[3,8,["vine"]],[4,8,[]],[5,8,[]],[6,8,["*"]],[1,7,["*"]],[2,7,["*"]],[3,7,["*"]],[4,7,["*"]],[5,7,["*"]],[6,7,["*"]]]]],

[[n,rainforest],[[[1,9,[     ]],[2,9,[     ]],[3,9,["*"    ]],[4,9,["*"    ]],[5,9,["*"    ]],
 [1,8,[     ]],[2,8,["*"    ]],[3,8,[     ]],[4,8,[     ]],[5,8,["*"    ]],
 [1,7,[     ]],[2,7,["*"    ]],[3,7,[     ]],[4,7,["*"    ]],[5,7,[     ]],
 [1,6,["*"    ]],[2,6,[     ]],[3,6,["k"    ]],[4,6,["*"    ]],[5,6,["*"    ]],
 [1,5,["*"    ]],[2,5,[     ]],[3,5,[     ]],[4,5,["c"    ]],[5,5,["*"    ]],
 [1,4,[     ]],[2,4,["*"    ]],[3,4,[     ]],[4,4,[     ]],[5,4,["*"    ]],
 [1,3,["*"    ]],[2,3,[     ]],[3,3,[     ]],[4,3,["*"    ]],[5,3,[     ]],
 [1,2,["*"    ]],[2,2,[     ]],[3,2,["vine"    ]],[4,2,["*"    ]],[5,2,[     ]],
 [1,1,[     ]],[2,1,["*"    ]],[3,1,["*"    ]],[4,1,["*"    ]],[5,1,[     ]]]]],

[[n,traverse],[[v,x],[v,y]],":-",
[
	[[n,"->"],
	[
		[[n,traverse],[[v,x],[v,y],[],[v,c],[],[v,a],"no",[v,b]]],

		[
		[[n,true]]
		],

		[
		[[n,false]]
		]
	]]%,
	%,[[n,cut]]
]],
[[n,traverse],[[v,'_x'],[v,'_y'],[v,explored],[v,explored],[v,inventory],[v,inventory],[v,e],[v,e]],":-",
[
	[[n,"->"],
	[
		[[n,equals4],[[v,e],"e"]],

		[[n,true]],

		[[n,fail]]
	]]%,
	%,[[n,cut]]
]],
[[n,traverse],[[v,x],[v,y],[v,explored1],[v,explored2],[v,inventory1],[v,inventory2],[v,e1],[v,e2]],":-",
[
	[[n,rainforest],[[v,map]]],
	[[n,member],[[[v,x],[v,y],[v,cell]],[v,map]]],
	[[n,"->"],
	[
		[[n,"->"],
		[
			[[n,equals4],[[v,cell],["*"]]],

			[
			[[n,true]]
			],

			[
			[[n,member],[[[v,x],[v,y]],[v,explored1]]]
			]
		]],

		[
		[
		[[n,equals4],[[v,explored1],[v,explored2]]],
		[[n,equals4],[[v,inventory1],[v,inventory2]]],
		[[n,=],[[v,e2],[v,e1]]]
		%,[[n,cut]] speeds up in ssi but crashes lpi
		]
		],

		[
		[
		[[n,writeln],[[[v,x],[v,y]]]],
		[[n,"->"],
		[
			[[n,equals4],[[v,cell],[]]],

			[
			[[n,equals4],[[v,inventory4a],[v,inventory1]]]
			],

			[
			[
			[[n,equals4],[[v,cell],[[v,item]]]],
			[[n,append],[[v,inventory1],[[v,item]],[v,inventory3]]],
			[[n,apply_all_to_all],[[v,inventory3],[v,inventory4]]],
			[[n,equals4],[[v,inventory4a],[v,inventory4]]]
			]
			]
		]],
		[[n,writeln],[[v,inventory4a]]],
		[[n,"->"],
		[
			[[n,member],["e",[v,inventory4a]]],

			[
			[
			[[n,writeln],["Game Over"]],
			[[n,equals4],[[v,explored1],[v,explored2]]],
			[[n,equals4],[[v,inventory2],[v,inventory4a]]],
			[[n,=],[[v,e2],"e"]]%,
			%,[[n,cut]]
			%,[[n,trace]]
			]
			],

			[
			[
			[[n,append],[[v,explored1],[[[v,x],[v,y]]],[v,explored3]]],
			[[n,-],[[v,x],1,[v,xm1]]],
			[[n,-],[[v,y],1,[v,ym1]]],
			[[n,+],[[v,x],1,[v,xp1]]],
			[[n,+],[[v,y],1,[v,yp1]]],
			[[n,traverse],[[v,xm1],[v,y],[v,explored3],[v,explored4],[v,inventory4a],[v,inventory5],[v,e1],[v,e3]]],
			[[n,traverse],[[v,x],[v,ym1],[v,explored4],[v,explored5],[v,inventory5],[v,inventory6],[v,e3],[v,e4]]],
			[[n,traverse],[[v,xp1],[v,y],[v,explored5],[v,explored6],[v,inventory6],[v,inventory7],[v,e4],[v,e5]]],
			[[n,traverse],[[v,x],[v,yp1],[v,explored6],[v,explored2],[v,inventory7],[v,inventory2],[v,e5],[v,e2]]]
			]
			]
		]]
		]
		]
	]]
%,[[n,cut]]
]],
[[n,apply_all_to_all],[[v,inventory1],[v,inventory2]],":-",
[
%[[n,trace2]],
	[[n,findall],
	[
		[v,item3],

		[
		[[n,member],[[v,item1],[v,inventory1]]],
		[[n,member],[[v,item2],[v,inventory1]]],
		[[n,not],[[[[n,equals4],[[v,item1],[v,item2]]]]]],
		[[n,apply],[[v,item1],[v,item2],[v,item3]]],
		[[n,not],[[[[n,member],[[v,item3],[v,inventory1]]]]]]
		],

		[v,addeditems]
	]],
	[[n,"->"],
	[
		[[n,equals4],[[v,addeditems],[]]],

		[
		[[n,equals4],[[v,inventory1],[v,inventory2]]]
		],

		[
		[
		[[n,append],[[v,inventory1],[v,addeditems],[v,inventory3]]],
		[[n,apply_all_to_all],[[v,inventory3],[v,inventory2]]]
		]
		]
	]]
%,[[n,cut]]
]],
[[n,apply],["k","c","m"]],
[[n,apply],["m","vine","e"]]
],
[[]]).
%[[[[v,inventory4],["vine","c","k","m","e"]]]]).

test(204,
[[n,function]],
[[[n,function],":-",[[[n,writeln],["Hello World!"]]]]],
[[]]).

test(205,[[n,findall1],[[[1,2],[1,4]],[v,b]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[v,b1],[[[n,member],[[v,a1],[v,a]]],
        
        [[n,findall],[[v,a2],[[[n,member],[[v,a2],[v,a1]]],
        %[[n,+],[[v,a2],5,[v,a3]]],
        %[[n,/],[[v,a3],2,[v,a4]]],
        %[[n,ceiling],[[v,a4],[v,a5]]],
        [[n,equals4],[[v,a2],1]]
        ],
                [v,b1]]]],
                
                [v,b]]]	
        ]]
        
],[[[[v,b],[[1],[1]]
%[[1,11,111],[2,22,222],[3,33,333]]
]]]).

test(206,%[[n,or12],[[v,a]]],
[[n,findall],[[v,b],[[[n,or12],[1,[v,b]]]],[v,a]]],
[
        [[n,or12],[1,1]],
        [[n,or12],[2,2]],
        [[n,or12],[1,3]]
        
],[[[[v,a],[1,3]]]]).


test(207,[[n,findall1],[[1,2],[v,b1]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b1]],":-",
        [       %[[n,findall],[[v,b1],[[[n,member],[[v,a],[v,a1]]],
        
        [[n,findall],[[v,a3],[[[n,member],[[v,a2],[v,a]]],
        [[n,+],[[v,a2],5,[v,a3]]],
        [[n,/],[[v,a3],2,[v,a4]]],
        [[n,ceiling],[[v,a4],[v,a5]]],
        [[n,equals4],[[v,a4],[v,a5]]]]
        ,
                [v,b1]]]]
                
                %[v,b]]]	
        ]
        
],[[[[v,b1],[6]
%[[1,11,111],[2,22,222],[3,33,333]]
]]]).

%/*
test(208,[[n,reverse1],[[1,2,3],[],[v,c],[v,b],[v,g]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],
[[[n,reverse1],[[],[v,a],[v,c],[v,b],[v,a]]],
[[n,reverse1],[[v,a],[v,b],[v,c],[v,f],[v,g]],":-",[[[n,head],[[v,a],[v,b1]]],[[n,tail],[[v,a],[v,c1]]],[[n,wrap],[[v,b1],[v,d]]],[[n,append],[[v,d],[v,b],[v,f]]],[[n,reverse1],[[v,c1],[v,f],[v,h],[v,j],[v,g]]]]]]
,[[[[v,b],[1]],[[v,g],[3,2,1]]]]).
%*/

test(209,[[n,a],[["a","b"],"",[v,a]]],

[ % string concat
[[n,a],[[],[v,a],[v,a]]],
[[n,a],[[[v,e],"|",[v,d]],[v,b],[v,c]],":-",
[
	%[[n,equals4],[]],
	[[n,stringconcat],[[v,b],[v,e],[v,f]]],
	[[n,a],[[v,d],[v,f],[v,c]]]
]]
],

[[[[v,a],"ab"]]]).



test(210,[[n,grammar1],[[apple]]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],[]]] 
		  ]
		  ],
		  
		  [[n,noun],"->",[[apple]]]
],[[]]).



test(211,[[n,grammar1],[[apple,banana]]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],[]]] 
		  ]
		  ],
		  
		  [[n,noun],"->",[[apple,banana]]]
],[[]]).


test(212,[[n,grammar1],[[apple,banana,carrot]]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],[]]] 
		  ]
		  ],
		  
		  [[n,noun],"->",[[apple,banana],[carrot]]]
],[[]]).


test(213,[[n,reverse1],[[1,2,3],[],[v,a]]],
[
[[n,reverse1],[[],[v,a],[v,a]]],
[[n,reverse1],[[[v,a],"|",[v,d]],[v,b],[v,c]],":-",
[
	[[n,reverse1],[[v,d],[[v,a],"|",[v,b]],[v,c]]]
]]
],[[[[v,a],[3,2,1]]]]).


test(214,[[n,append1],[[1,2,3],[],[v,a]]],
[
[[n,append1],[[],[v,a],[v,a]]],
[[n,append1],[[[v,a],"|",[v,d]],[v,b],[[v,a],"|",[v,c]]],":-",
[
	[[n,append1],[[v,d],[v,b],[v,c]]]
]]
],[[[[v,a],[1,2,3]]]]).

% passes with occurs_check(on).
/*
test(215,[[n,function1],[[v,a]]],
[
        [[n,function1],[[[v,a]]],":-",
        [
        	[[n,true]]]]
]
,[]).
*/

test(215,[[n,findall],[[[v,a],[v,b]],[[n,append],[[v,a],[v,b],[1,2,3]]],[v,c]]],
[],[[[[v,c],[[[],[1,2,3]],[[1],[2,3]],[[1,2],[3]],[[1,2,3],[]]]]]]).

/*
% cut deletes cp data back to and including pred id call

test(216,[[n,count],[1,[v,n]]],
[
        [[n,count],[1,[v,a]],":-",[[[n,member],[[2,3],[v,a]]],[[n,cut]],[[n,fail]]]],
        [[n,count],[1,3],":-",[[[n,cut]]]]

        
] ,[[[[v,n], 2]]]).

*/

test(216,

%a(
% x[[n,apply_all_to_all],[["m","vine"],[v,inventory4]]],%
[[n,traverse],[2,8]], %2,8 or 2,6
%[[n,apply_all_to_all],[["k","c","m","vine"],[v,inventory4]]],
%[[n,traverse],[3,9,empty,empty,empty,empty]],
[
[[n,rainforest],[[[1,9,["*"]],[2,9,["*"]],[3,9,["*"]],[4,9,["*"]],[5,9,["*"]],[6,9,["*"]],[7,9,["*"]],[1,8,["*"]],[2,8,["k"]],[3,8,["c"]],[4,8,["vine"]],[5,8,[]],[6,8,[]],[7,8,["*"]],[1,7,["*"]],[2,7,["*"]],[3,7,["*"]],[4,7,["*"]],[5,7,["*"]],[6,7,["*"]],[7,7,["*"]]]]],
[[n,traverse],[[v,x],[v,y]],":-",
[
	[[n,"->"],
	[
		[[n,traverse],[[v,x],[v,y],[],[v,c],[],[v,a],"no",[v,b]]],

		[
		[[n,true]]
		],

		[
		[[n,false]]
		]
	]]%,
	%[[n,cut]]
]],
[[n,traverse],[[v,'_x'],[v,'_y'],[v,explored],[v,explored],[v,inventory],[v,inventory],[v,e],[v,e]],":-",
[
	[[n,"->"],
	[
		[[n,equals4],[[v,e],"e"]],

		[[n,true]],

		[[n,fail]]
	]]%,
	%[[n,cut]]
]],
[[n,traverse],[[v,x],[v,y],[v,explored1],[v,explored2],[v,inventory1],[v,inventory2],[v,e1],[v,e2]],":-",
[
	[[n,rainforest],[[v,map]]],
	[[n,member],[[[v,x],[v,y],[v,cell]],[v,map]]],
	[[n,"->"],
	[
		[[n,"->"],
		[
			[[n,equals4],[[v,cell],["*"]]],

			[
			[[n,true]]
			],

			[
			[[n,member],[[[v,x],[v,y]],[v,explored1]]]
			]
		]],

		[
		[
		[[n,equals4],[[v,explored1],[v,explored2]]],
		[[n,equals4],[[v,inventory1],[v,inventory2]]],
		[[n,=],[[v,e2],[v,e1]]]
		]
		],

		[
		[
		[[n,writeln],[[[v,x],[v,y]]]],
		[[n,"->"],
		[
			[[n,equals4],[[v,cell],[]]],

			[
			[[n,equals4],[[v,inventory4a],[v,inventory1]]]
			],

			[
			[
			[[n,equals4],[[v,cell],[[v,item]]]],
			[[n,append],[[v,inventory1],[[v,item]],[v,inventory3]]],
			[[n,apply_all_to_all],[[v,inventory3],[v,inventory4]]],
			[[n,equals4],[[v,inventory4a],[v,inventory4]]]
			]
			]
		]],
		[[n,writeln],[[v,inventory4a]]],
		[[n,"->"],
		[
			[[n,member],["e",[v,inventory4a]]],

			[
			[
			[[n,writeln],["Game Over"]],
			[[n,equals4],[[v,explored1],[v,explored2]]],
			[[n,equals4],[[v,inventory2],[v,inventory4a]]],
			[[n,=],[[v,e2],"e"]]%,
			%,[[n,cut]]
			%,[[n,trace]]
			]
			],

			[
			[
			[[n,append],[[v,explored1],[[[v,x],[v,y]]],[v,explored3]]],
			[[n,-],[[v,x],1,[v,xm1]]],
			[[n,-],[[v,y],1,[v,ym1]]],
			[[n,+],[[v,x],1,[v,xp1]]],
			[[n,+],[[v,y],1,[v,yp1]]],
			[[n,traverse],[[v,xm1],[v,y],[v,explored3],[v,explored4],[v,inventory4a],[v,inventory5],[v,e1],[v,e3]]],
			[[n,traverse],[[v,x],[v,ym1],[v,explored4],[v,explored5],[v,inventory5],[v,inventory6],[v,e3],[v,e4]]],
			[[n,traverse],[[v,xp1],[v,y],[v,explored5],[v,explored6],[v,inventory6],[v,inventory7],[v,e4],[v,e5]]],
			[[n,traverse],[[v,x],[v,yp1],[v,explored6],[v,explored2],[v,inventory7],[v,inventory2],[v,e5],[v,e2]]]
			]
			]
		]]
		]
		]
	]]
]],
[[n,apply_all_to_all],[[v,inventory1],[v,inventory2]],":-",
[
	[[n,findall],
	[
		[v,item3],

		[
		[[n,member],[[v,item1],[v,inventory1]]],
		[[n,member],[[v,item2],[v,inventory1]]],
		[[n,not],[[[[n,equals4],[[v,item1],[v,item2]]]]]],
		[[n,apply],[[v,item1],[v,item2],[v,item3]]],
		[[n,not],[[[[n,member],[[v,item3],[v,inventory1]]]]]]
		],

		[v,addeditems]
	]],
	[[n,"->"],
	[
		[[n,equals4],[[v,addeditems],[]]],

		[
		[[n,equals4],[[v,inventory1],[v,inventory2]]]
		],

		[
		[
		[[n,append],[[v,inventory1],[v,addeditems],[v,inventory3]]],
		[[n,apply_all_to_all],[[v,inventory3],[v,inventory2]]]
		]
		]
	]]
]],
[[n,apply],["k","c","m"]],
[[n,apply],["m","vine","e"]]
],
[[]]
%[[[[v,inventory4],["k","c","m","vine","e"]]]]
).

/*
test(217,[[n,a],[[v,a]]],
[[[n,a],[[v,a]],":-",[[[n,b],[[v,a]]]]],
[[n,b],[empty],":-",[[[n,true]]]]
%[[n,b],[2],":-",[[[n,true]]]]],
],
[[[[v,a],empty]]]).

test(218,[[n,member],[[1,2],[v,a]]],
[],
[[[[v,a],1]]]).

test(219,[[n,a],[[v,a]]],
[[[n,a],[[v,a]],":-",[[[n,b],[[v,a]]]]],
[[n,b],[empty],":-",[[[n,true]]]]
%[[n,b],[2],":-",[[[n,true]]]]],
],
[[[[v,a],empty]]]).

*/

test(217,

%a(
% x[[n,apply_all_to_all],[["m","vine"],[v,inventory4]]],%
[[n,traverse],[2,6]], %2,8 or 2,6
%[[n,apply_all_to_all],[["k","c","m","vine"],[v,inventory4]]],
%[[n,traverse],[3,9,empty,empty,empty,empty]],
[
/*[[n,rainforest],[[
[1,9,["*"]],[2,9,["*"]],[3,9,["*"]],[4,9,["*"]],[5,9,["*"]],[6,9,["*"]],[7,9,["*"]],
[1,8,["*"]],[2,8,["k"]],[3,8,["c"]],[4,8,["vine"]],[5,8,[]],[6,8,[]],[7,8,["*"]],
[1,7,["*"]],[2,7,["*"]],[3,7,["*"]],[4,7,["*"]],[5,7,["*"]],[6,7,["*"]],[7,7,["*"]]]]],
*/
%/*
[[n,rainforest],[[[1,9,[     ]],[2,9,[     ]],[3,9,["*"    ]],[4,9,["*"    ]],[5,9,["*"    ]],
 [1,8,[     ]],[2,8,["*"    ]],[3,8,[     ]],[4,8,[     ]],[5,8,["*"    ]],
 [1,7,[     ]],[2,7,["*"    ]],[3,7,[     ]],[4,7,["*"    ]],[5,7,[     ]],
 [1,6,["*"    ]],[2,6,[     ]],[3,6,["k"    ]],[4,6,["*"    ]],[5,6,["*"    ]],
 [1,5,["*"    ]],[2,5,[     ]],[3,5,[     ]],[4,5,["c"    ]],[5,5,["*"    ]],
 [1,4,[     ]],[2,4,["*"    ]],[3,4,[     ]],[4,4,[     ]],[5,4,["*"    ]],
 [1,3,["*"    ]],[2,3,[     ]],[3,3,[     ]],[4,3,["*"    ]],[5,3,[     ]],
 [1,2,["*"    ]],[2,2,[     ]],[3,2,["vine"    ]],[4,2,["*"    ]],[5,2,[     ]],
 [1,1,[     ]],[2,1,["*"    ]],[3,1,["*"    ]],[4,1,["*"    ]],[5,1,[     ]]]]],
%*/
[[n,traverse],[[v,x],[v,y]],":-",
[
	[[n,"->"],
	[
		[[n,traverse],[[v,x],[v,y],[],[v,c],[],[v,a],"no",[v,b]]],

		[
		[[n,true]]
		],

		[
		[[n,false]]
		]
	]]%,
	%[[n,cut]]
]],
[[n,traverse],[[v,'_x'],[v,'_y'],[v,explored],[v,explored],[v,inventory],[v,inventory],[v,e],[v,e]],":-",
[
	[[n,"->"],
	[
		[[n,equals4],[[v,e],"e"]],

		[[n,true]],

		[[n,fail]]
	]]%,
	%[[n,cut]]
]],
[[n,traverse],[[v,x],[v,y],[v,explored1],[v,explored2],[v,inventory1],[v,inventory2],[v,e1],[v,e2]],":-",
[
	[[n,rainforest],[[v,map]]],
	[[n,member],[[[v,x],[v,y],[v,cell]],[v,map]]],
	[[n,"->"],
	[
		[[n,"->"],
		[
			[[n,equals4],[[v,cell],["*"]]],

			[
			[[n,true]]
			],

			[
			[[n,member],[[[v,x],[v,y]],[v,explored1]]]
			]
		]],

		[
		[
		[[n,equals4],[[v,explored1],[v,explored2]]],
		[[n,equals4],[[v,inventory1],[v,inventory2]]],
		[[n,=],[[v,e2],[v,e1]]]
		]
		],

		[
		[
		[[n,writeln],[[[v,x],[v,y]]]],
		[[n,"->"],
		[
			[[n,equals4],[[v,cell],[]]],

			[
			[[n,equals4],[[v,inventory4a],[v,inventory1]]]
			],

			[
			[
			[[n,equals4],[[v,cell],[[v,item]]]],
			[[n,append],[[v,inventory1],[[v,item]],[v,inventory3]]],
			[[n,apply_all_to_all],[[v,inventory3],[v,inventory4]]],
			[[n,equals4],[[v,inventory4a],[v,inventory4]]]
			]
			]
		]],
		[[n,writeln],[[v,inventory4a]]],
		[[n,"->"],
		[
			[[n,member],["e",[v,inventory4a]]],

			[
			[
			[[n,writeln],["Game Over"]],
			[[n,equals4],[[v,explored1],[v,explored2]]],
			[[n,equals4],[[v,inventory2],[v,inventory4a]]],
			[[n,=],[[v,e2],"e"]]%,
			%,[[n,trace]]
			]
			],

			[
			[
			[[n,append],[[v,explored1],[[[v,x],[v,y]]],[v,explored3]]],
			[[n,-],[[v,x],1,[v,xm1]]],
			[[n,-],[[v,y],1,[v,ym1]]],
			[[n,+],[[v,x],1,[v,xp1]]],
			[[n,+],[[v,y],1,[v,yp1]]],
			[[n,traverse],[[v,xm1],[v,y],[v,explored3],[v,explored4],[v,inventory4a],[v,inventory5],[v,e1],[v,e3]]],
			[[n,traverse],[[v,x],[v,ym1],[v,explored4],[v,explored5],[v,inventory5],[v,inventory6],[v,e3],[v,e4]]],
			[[n,traverse],[[v,xp1],[v,y],[v,explored5],[v,explored6],[v,inventory6],[v,inventory7],[v,e4],[v,e5]]],
			[[n,traverse],[[v,x],[v,yp1],[v,explored6],[v,explored2],[v,inventory7],[v,inventory2],[v,e5],[v,e2]]]
			]
			]
		]]
		]
		]
	]]
]],
[[n,apply_all_to_all],[[v,inventory1],[v,inventory2]],":-",
[
	[[n,findall],
	[
		[v,item3],

		[
		[[n,member],[[v,item1],[v,inventory1]]],
		[[n,member],[[v,item2],[v,inventory1]]],
		[[n,not],[[[[n,equals4],[[v,item1],[v,item2]]]]]],
		[[n,apply],[[v,item1],[v,item2],[v,item3]]],
		[[n,not],[[[[n,member],[[v,item3],[v,inventory1]]]]]]
		],

		[v,addeditems]
	]],
	[[n,"->"],
	[
		[[n,equals4],[[v,addeditems],[]]],

		[
		[[n,equals4],[[v,inventory1],[v,inventory2]]]
		],

		[
		[
		[[n,append],[[v,inventory1],[v,addeditems],[v,inventory3]]],
		[[n,apply_all_to_all],[[v,inventory3],[v,inventory2]]]
		]
		]
	]]
]],
[[n,apply],["k","c","m"]],
[[n,apply],["m","vine","e"]]
],
[[]]
%[[[[v,inventory4],["k","c","m","vine","e"]]]]
).

test(218,[[n,findall1],[[[1,2],[3,4]],[v,b]]],%[[[1,11,111],[2,22,222],[3,33,333]],[v,b]]],

[
        [[n,findall1],[[v,a],[v,b]],":-",
        [       [[n,findall],[[v,b1],[[[n,member],[[v,a1],[v,a]]],
        
        [[n,findall],[[v,a3],[[[n,member],[[v,a2],[v,a1]]],
        [[n,a],[[v,a2],[v,a3]]]],
                [v,b1]]]],
                
                [v,b]]]	
        ]],
        
        [[n,a],[1,6]],
        [[n,a],[2,7]],
        [[n,a],[3,8]],
        [[n,a],[4,9]]
        
],[[[[v,b],[[6,7],[8,9]]
%[[1,11,111],[2,22,222],[3,33,333]]
]]]).


test(219,[[n,findall],[[[v,a],[v,b]],[[n,stringconcat],[[v,a],[v,b],"abc"]],[v,c]]],
[],[[[[v,c],[["","abc"],["a","bc"],["ab","c"],["abc",""]]]]]).


test(220,[[n,stringconcat],["a","b","ab"]],
[],
[[]]).

test(221,[[n,stringconcat],["a","b",[v,a]]],
[],
[[[[v,a],"ab"]]]).

test(222,[[n,stringconcat],["a",[v,a],"ab"]],
[],
[[[[v,a],"b"]]]).

test(223,[[n,stringconcat],[[v,a],"b","ab"]],
[],
[[[[v,a],"a"]]]).


test(224,[[n,append],[["a"],["b"],["a","b"]]],
[],
[[]]).

test(225,[[n,append],[["a"],["b"],[v,a]]],
[],
[[[[v,a],["a","b"]]]]).

test(226,[[n,append],[["a"],[v,a],["a","b"]]],
[],
[[[[v,a],["b"]]]]).

test(227,[[n,append],[[v,a],["b"],["a","b"]]],
[],
[[[[v,a],["a"]]]]).


test(228,[[n,findall],[[[v,a],[v,b]],[[n,append],[[v,a],[v,b],["a","b","c"]]],[v,c]]],
[],[[[[v,c],[[[],["a","b","c"]],[["a"],["b","c"]],[["a","b"],["c"]],[["a","b","c"],[]]]]]]).

test(229,[[n,findall],[[v,a],[[n,member],[[v,a],["a","b"]]],[v,c]]],
[],[[[[v,c],["a","b"]]]]).

test(230,[[n,append],[["a"],[v,a],[v,b]]],
[],
[[[[v,b],["a","|",[v,_]]]]]).

test(231,[[n,findall],[[[v,a],[v,c]],[[n,append],[[v,a],["a","b"],[v,c]]],[v,d]]],
[],
[[[[v,d],[[[],["a","b"]],[[[v,_]],[[v,_],"a","b"]],[[[v,_],[v,_]],[[v,_],[v,_],"a","b"]],[[[v,_],[v,_],[v,_]],[[v,_],[v,_],[v,_],"a","b"]],[[[v,_],[v,_],[v,_],[v,_]],[[v,_],[v,_],[v,_],[v,_],"a","b"]],[[[v,_],[v,_],[v,_],[v,_],[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],"a","b"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","b"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","b"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","b"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","b"]]]]]]).

test(232,[[n,member],["a",["a","b"]]],
[],
[[]]).

test(233,[[n,findall],[[v,a],[[n,member],["a",[v,a]]],[v,d]]],
[],
[[[[v,d],[["a","|",[v,_]],[[v,_],"a","|",[v,_]],[[v,_],[v,_],"a","|",[v,_]],[[v,_],[v,_],[v,_],"a","|",[v,_]],[[v,_],[v,_],[v,_],[v,_],"a","|",[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],"a","|",[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","|",[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","|",[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","|",[v,_]],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a","|",[v,_]]]]]]).

test(234,[[n,member],[[v,a],["a","b"]]],
[],
[[[[v,a],"a"]]]).

test(235,[[n,findall],[[[v,a],[v,b]],[[n,member],[[v,b],[v,a]]],[v,d]]],
[],
[[[[v,d],[[[v,_],[[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]]]]]]).

test(236,[[n,findall],[[[v,a],[v,b],[v,c]],[[n,append],[[v,a],["a"],[[v,b],"|",[v,c]]]],[v,d]]],
[],
[[[[v,d],[[[],"a",[]],[[[v,_]],[v,_],["a"]],[[[v,_],[v,_]],[v,_],[[v,_],"a"]],[[[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],"a"]],[[[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],"a"]],[[[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],"a"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],"a"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a"]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"a"]]]]]]).

test(237,[[n,findall],[[[v,a],[v,b],[v,c]],[[n,append],[[v,a],["b",[v,b]],[v,c]]],[v,d]]],
[],
[[[[v,d],[[[],[v,_],["b",[v,_]]],[[[v,_]],[v,_],[[v,_],"b",[v,_]]],[[[v,_],[v,_]],[v,_],[[v,_],[v,_],"b",[v,_]]],[[[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],"b",[v,_]]],[[[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],"b",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],"b",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"b",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"b",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"b",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"b",[v,_]]]]]]]).

test(238,[[n,findall],[[[v,a],[v,b],[v,c]],[[n,append],[[v,a],[v,b],[v,c]]],[v,d]]],
[],
[[[[v,d],[[[],[v,_],[v,_]],[[[v,_]],[v,_],[[v,_],"|",[v,_]]],[[[v,_],[v,_]],[v,_],[[v,_],[v,_],"|",[v,_]]],[[[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],"|",[v,_]]],[[[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]],[[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_]],[v,_],[[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],[v,_],"|",[v,_]]]]]]]).

test(239,[[n,append],[[v,a],["b",[v,b]],[[v,c]]]],
[],
[]).

test(240,[[n,append],[[v,a],["b"],[[v,c]]]],
[],
[[[[v,a],[]],[[v,c],"b"]]]).


test(241,%[[n,reverse1],[[v,d],[empty,empty,empty],[3,2,1]]],%
[[n,reverse1],[[v,a],[],[3,
2,1]]],
[
[[n,reverse1],[[],[v,a],[v,a]],":-",[[[n,cut]]]],
[[n,reverse1],[[[v,a],"|",[v,d]],[v,b],[v,c]],":-",
[
	[[n,reverse1],[[v,d],[[v,a],"|",[v,b]],[v,c]]],
	[[n,cut]]
]]
],[[[[v,a],[1,2,3
]]]]).

test(242,%[[n,reverse1],[[v,d],[empty,empty,empty],[3,2,1]]],%
[[n,append1],[[v,a],[],[1,2,3]]],
[
[[n,append1],[[],[v,a],[v,a]],":-",[[[n,cut]]]],
[[n,append1],[[[v,a],"|",[v,d]],[v,b],[[v,a],"|",[v,c]]],":-",
[
	[[n,append1],[[v,d],[v,b],[v,c]]],
	[[n,cut]]
]]
],[[[[v,a],[1,2,3
]]]]).

test(243,
[[n,back_propagate],[[v,a]]],
[
[[n,back_propagate],[[v,a]],":-",
[
	[[n,equals4],[[v,a],[v,b]]],
	[[n,equals4],[[v,b],1]]
]]
],[[[[v,a],1]]]).

% similar to test 56

%test(244,[[n,foldl1],[[n,add],[1,2,3],0,[v,d]]],
%test(244,[[n,var],[[v,a]]],
%test(244,[[n,string_chars1],[[v,a],[v,d]]],
%test(244,[[n,string_concat1],["a","",[v,d]]],
%test(244,[[n,append1],[[a],[],[v,d]]],
%test(244,[[n,string_chars1],["ab",[v,d]]],
test(244,[[n,foldl1],[[n,stringconcata1],["a","b"],"",[v,d]]],
%test(244,[[n,foldl1],[[n,add],[1,2,3],0,[v,d]]],
[
        [[n,foldl1],[[v,f],[],[v,l],[v,l]],":-",[[[n,cut]]]],
        [[n,foldl1],[[v,f],[[v,h],"|",[v,t]],[v,m1],[v,na]],":-",
        [       [[v,f],[[v,m1],[v,h],[v,m2]]],
                [[n,foldl1],[[v,f],[v,t],[v,m2],[v,na]]]
        ]
        ],

        [[n,add],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ],
        

[[n,stringconcata1],[[v,a],[v,b],[v,c]],":-",
[
	[[n,"->"],
	[
		[[n,"->"],
		[
			[
			[[n,var],[[v,a]]],
			[[n,not],[[[n,var],[[v,b]]]]],
			[[n,var],[[v,c]]]
			],

			[[n,true]],

			[
			[
			[[n,not],[[[n,var],[[v,a]]]]],
			[[n,var],[[v,b]]],
			[[n,var],[[v,c]]]
			]
			]
		]],

		[[n,fail]],

		[
		[[n,stringchars1],[[v,a],[v,a1]]],
		[[n,stringchars1],[[v,b],[v,b1]]],
		[[n,stringchars1],[[v,c],[v,c1]]],
		%[[n,writeln],[[v,c1]]],
		%[[n,trace2]],
		[[n,appenda1],[[v,a1],[v,b1],[v,c1]]],
		[[n,stringchars1],[[v,a],[v,a1]]],
		[[n,stringchars1],[[v,b],[v,b1]]],
		[[n,stringchars1],[[v,c],[v,c1]]]
		]
	]]
]],
[[n,stringchars1],[[v,a],[v,b]],":-",
[

	[[n,"->"],
	[[

	[[n,var],[[v,a]]],
	[[n,var],[[v,b]]]
	],
	[[n,true]],
	
	[[n,string_chars],[[v,a],[v,b]]]
	]]
]],
%[[n,stringchars1],[[v,a],[v,b]],":-",
%[
%	[[n,stringchars],[[v,a],[v,b]]]
%]],
[[n,appenda1],[[],[v,a],[v,a]]],
[[n,appenda1],[[[v,a],"|",[v,d]],[v,b],[[v,a],"|",[v,c]]],":-",
[
	[[n,appenda1],[[v,d],[v,b],[v,c]]]
]]

]

%,[[[[v,d], 6]]]).
%,[[]]).
%,[[]]).
%,[[[[v,d], "a"]]]).
%,[[[[v,d], [a]]]]).
%,[[[[v,d], [a]]]]).
,[[[[v,d], "ab"]]]).
%,[[[[v,d], 6]]]).

/*

*/

test(245,[[[n,equals4],[[v,a],"abc"]],[[n,string_chars],[[v,a],[[v,x],[v,y],[v,z]]]]],

[]

,[[[[v,a], "abc"],[[v,x], a],[[v,y], b],[[v,z], c]]]).

/*
test(246,[[n,a],[[v,b]]],
%test(244,[[n,foldl1],[[n,add],[1,2,3],0,[v,d]]],
[
        [[n,a],[[v,b]],":-",
        [       [[n,not],[[[[n,c],[v,c]]]]]
        ]]
]
,[[]]).

test(247,[[n,a],[[v,b]]],
%test(244,[[n,foldl1],[[n,add],[1,2,3],0,[v,d]]],
[
        [[n,a],[[v,b]],":-",
        [       [[n,"->"],[[[[n,c],[[v,c]]]],[[[n,c],[[v,c]]]],[[[n,c],[[v,c]]]]]]
        ]]
]
,[[]]).


test(248,[[n,a],[[v,b]]],
%test(244,[[n,foldl1],[[n,add],[1,2,3],0,[v,d]]],
[
        [[n,a],[[v,b]],":-",
        [       [[n,"->"],[[[[n,c],[[v,c]]]],[[[n,c],[[v,c]]]],[[n,"->"],[[[[n,c],[[v,c]]]],[[[n,c],[[v,c]]]],[[[n,c],[[v,c]]]]]]]]
        ]]
]
,[[]]).

test(249,[[n,conjunction],["true","false",[v,c]]],

[
        [[n,findall1],[[v,f],[v,l],[v,m1],[v,n]],":-",
        [       [[n,"->"],[[[v,f],[[v,h],[v,m2]]],
                [       [[n,wrap],[[v,m2],[v,m3]]],
                        [[n,append],[[v,m1],[v,m3],[v,m4]]]
                ],
                [
                        [[n,=],[[v,m1],[v,m4]]]
                ]]],
                [[n,findall1],[[v,f],[v,t],[v,m4],[v,n]]]

        ]
        ]]
        
,[[[[v,c], "false"]]]).

test(250,[[n,a]],

[[[n,a],":-",
	[
	[
	[[n,member3],[[[v,function],"|",[[v,typestatements2]]],[v,typestatements1]]],
	[[n,member3],[[[v,function],"|",[[v,modestatements2]]],[v,modestatements1]]],
	[[n,extract_modes1],[[v,typestatements2],[v,typestatements3],[v,vars1],[v,vars2],[v,modestatements2]]],
	[[n,"->"],
	[
		[[n,types],["on"]],

		[[n,debug_call],[[v,skip],[[v,function],[v,vars2]]]],

		[[n,true]]
	]],
	[[n,"->"],
	[
		[
		[[n,checktypes1],[[v,vars2],[v,typestatements3],[v,typestatements3],[v,typestatements1]]]
		],

		[
		[
		[[n,"->"],
		[
			[[n,types],["on"]],

			[[n,debug_exit],[[v,skip],[[v,function],[v,vars2]]]],

			[[n,true]]
		]],
		[[n,"->"],
		[
			[[n,types],["on"]],

			[
			[[n,debug_types_exit],[[[v,function],"/","~",[v,l],[v,input_type_check]]]]
			],

			[[n,true]]
		]]
		]
		],

		[
		[
		[[n,"->"],
		[
			[[n,types],["on"]],

			[[n,debug_fail],[[v,skip],[[v,function],[v,vars1]]]],

			[[n,true]]
		]],
		[[n,"->"],
		[
			[[n,types],["on"]],

			[
			[[n,debug_types_fail],[[[v,function],"/","~",[v,l],[v,input_type_check]]]]
			],

			[[n,true]]
		]]
		]
		]
	]]
	]
		]]],
[[]]).

test(251,[[n,a]],

[
[[n,checktypes1],[[v,vars1],[v,typestatements1],[v,typestatements2],[v,typestatements4]],":-",
[

	[
	[[n,not],[[[n,=],[[v,type],[v,dbw_list]]]]],
	[[n,not],[[[n,=],[[v,type],[v,dbw_brackets]]]]]
	]
	
	,[[n,not],[[[n,=],[[v,type],[v,dbw_list]]]]]
	
	]]],
[[]]).


test(252,[[n,a]],

[
[[n,checktypes1],[[v,vars1],[v,typestatements1],[v,typestatements2],[v,typestatements4]],":-",
[

	[[
	[[n,true]],
	[[n,true]]
	]]
	
	,[[n,true]]
	
	]]],
[[]]).

test(253,[[n,a]],

[
[[n,checktypes1],[[v,vars1],[v,typestatements1],[v,typestatements2],[v,typestatements4]],":-",
[

	[[n,"->"],
	[
		[[n,types],["on"]],

		[[n,checktypes0_inputs],[[v,function],[v,vars1],[v,typestatements1],[v,modestatements1]]],

		[[n,true]]
	]]]]],
	
[[]]).
*/

/*
test(246,%[[n,or12],[[v,a]]],
[[n,findall],[[v,b],[[n,findall],[[v,b],[[[n,or12],[[v,b]]]],[v,a]]],[v,a]]],
[
        [[n,or12],[1]],
        [[n,or12],[2]]        
        
],[[[[v,a],[1,2]]]]).
*/

/*
test(246,[[n,test]],

[
[[n,test],":-",
[
        [[n,member],[[1,2],[v,a]]],
        [[n,member],[[1,2],3]]
]]        
],[[[[v,a],[1,2]]]]).
*/

%test(247,[[[n,equals4],[[v,a],[]]]][[n,findall],[[v,d],[[n,append1],[[v,a],[v,b],[v,d]]],
%test(244,[[n,string_concat1],["a","",[v,d]]],
%test(244,[[n,append1],[[a],[],[v,d]]],
%test(244,[[n,string_chars1],["ab",[v,d]]],
%test(244,[[n,foldl1],[[n,string_concat1],["a","b"],"",[v,d]]],
%test(244,[[n,foldl1],[[n,add],[1,2,3],0,[v,d]]],
%[
/*
[[n,string_concat1],[[v,a],[v,b],[v,c]],":-",
[
		
		%[[n,string_chars1],[[v,c],[v,c1]]],
		%[[n,equals4],[[v,c],[v,c1]]],
		%[[n,writeln],[[v,c1]]],
		%[[n,trace2]],
		[[n,append1],[[a],[],[v,c1]]]]],


[[n,append1],[[],[v,a],[v,a]]],
[[n,append1],[[[v,a],"|",[v,d]],[v,b],[[v,a],"|",[v,c]]],":-",
[
	[[n,append1],[[v,d],[v,b],[v,c]]]
]]
		
	
],[[[[v,a],[1,2]]]]).

*/
/*
test(247,[[n,findall],[[[v,a],[v,b]],[[n,append200],[[v,a],[v,b],["a","b","c"]]],[v,c]]],
[
[[n,append200],[[],[v,a],[v,a]],":-",[[[n,cut]]]],
[[n,append200],[[[v,a],"|",[v,d]],[v,b],[[v,a],"|",[v,c]]],":-",
[
	[[n,append200],[[v,d],[v,b],[v,c]]]
	,[[n,cut]]
]]

],[[[[v,c],[[[],["a","b","c"]],[["a"],["b","c"]],[["a","b"],["c"]],[["a","b","c"],[]]]]]]).

test(248,[[n,test2]],

[
[[n,test2],":-",
[
	[[n,a],[1,[v,t]]],
	[[n,b],[[v,t]]]%,
	%[[n,cut]]
]],
[[n,test2],":-",
[
	[[n,=],[[v,t],[v,false]]],
	[[n,b],[[v,t]]]
]],
[[n,a],[1,true]],
[[n,b],[true],":-",
[
	[[n,writeln],[true]]
]],
[[n,b],[false],":-",
[
	[[n,writeln],[false]]
]]
],
[[]]).
*/

test(246,[[n,test2]],

[
[[n,test2],":-",
[
	[[n,a],[1,[v,t]]],
	[[n,b],[[v,t]]]
]],
[[n,test2],":-",
[
	[[n,=],[[v,t],[v,false]]],
	[[n,b],[[v,t]]]
]],
[[n,a],[1,true]],
[[n,b],[true],":-",
[
	[[n,writeln],[true]]
]],
[[n,b],[false],":-",
[
	[[n,writeln],[false]]
]]
],

[[]]).

% Tests 247 and 248: In Prolog, the clause in not and antecedent in ->/2 and ->/3 have cut after them if needed (there is no cp or not already a cut after the last cp).

% This is accomplished by editing the state machine (SM) for the algorithm in LPI and SSI, and converting the SM back to List Prolog in LPI.

/*
test(247,[[n,test]],

[
[[n,test],":-",
[
	[[n,not],[[[[n,member],[[1,2],[v,a]]]
	%,[[n,cut]]
	]]]
]]],

[]).

test(248,[[n,test],[[v,b]]],

[
[[n,test],[[v,b]],":-",
[
	[[n,findall],[[v,a],
	[[n,"->"],[[[[n,member],[[1,2],[v,a]]]
	%,[[n,cut]]
	],
	[[n,writeln],["1"]],
	[[n,writeln],["2"]]
	]]
	,[v,b]]]
]]],

[[[[v,b],[1]]]]).
*/

test(247,[[n,test2],[[v,a]]],

[
[[n,test2],[[v,a]],":-",
[
	[[n,shell_c],["a","#include<stdio.h>\n#include<stdlib.h>\n\nint main(void)\n{\n   char str1[20];\n\n       if (fgets(str1, sizeof(str1), stdin) == NULL) {\n        fprintf(stderr, \"Input error\\n\");\n        return 1;\n    }\n\n    printf(\"%s\\n\", str1);\n return 0;\n}",[v,a]]]
]]
],


[[[[v,a],"a"]]]).

%[0, [6, 7]], [6, []], [7, [6, 7]]

/*
test(248,[[n,w]],
[[[n,w],":-",[[[n,i]],[[n,e]]]],
[[n,e],":-",[[[n,i]],[[n,e]]]],
[[n,i]]
],
[[]]).


test(248,[[n,w]],
[[[n,w],":-",[[[n,y]],[[n,g]]]],
[[n,y],":-",[[[n,r]],[[n,b]]]],
[[n,g],":-",[[[n,p]]]],
[[n,r]],
[[n,b]],
[[n,p],":-",[[[n,w]]]]
],
[[]]).
%object(cube3,[["yellow",["red","blue"]],["red",[]],["blue",[]],["white",["yellow","green"]],["green",["purple"]],["purple",["white"]]],

test(249,[[n,w]],
[[[n,w],":-",[[[n,r]],[[n,b]]]],
[[n,r],":-",[[[n,w]]]],
[[n,b],":-",[[[n,w]]]]
],
[[]]).
*/

test(248,[[n,function],[[v,'_'],[1],[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,append],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,[[[[v,c], [1]]]]).

test(249,[[n,function],[1,2]],
[
        [[n,function],[[v,'_'],[v,'_']]]
]
,[[]]).

/*
test(250,[[n,function],[[v,a]]],
[
        [[n,function],[[v,a]],":-",
        [
                [[n,not],[[n,false]
                ]]
        ]
        ]

]
,[[]]).

*/


test(250,[[n,grammar1],[["a","a","a"]]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,noun],[[v,s],[]]] 
		  ]
		  ],
		  
		  [[n,noun],"->",[[]]],
		  [[n,noun],"->",[["a"],[[n,noun]]]]
],[[]]).



test(251,[[n,grammar1],[["b","c","b","c"]]],
[
		  [[n,grammar1],[[v,s]],":-",
		  [
		  			 [[n,2],[[v,s],[]]]
		  ]
		  ],
	/*
	[[n,1],"->",[[]]],
	[[n,1],"->",[["a"],[[n,2]],[[n,1]]
	]],
	*/
	[[n,2],"->",[[]]],
	[[n,2],"->",[["b"],[[n,3]],[[n,2]]]],
	[[n,3],"->",[[]]],
	[[n,3],"->",[["c"],[[n,3]]]]		  

],[[]]).


