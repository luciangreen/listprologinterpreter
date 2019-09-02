%% test(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

test(Debug,NTotal,Score) :- test(Debug,0,NTotal,0,Score),!.
test(_Debug,NTotal,NTotal,Score,Score) :- NTotal=22, !.
test(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test(NTotal3,Query,Functions,Result),
	(interpret(Debug,Query,Functions,Result)->(Score3 is Score1+1,writeln([test,NTotal3,passed]));(Score3=Score1,writeln([test,NTotal3,failed]))),
	writeln(""),
	test(Debug,NTotal3,NTotal2,Score3,Score2),!.

test1(Debug,N,Passed) :-
	test(N,Query,Functions,Result),
	((interpret(Debug,Query,Functions,Result1),%%writeln(Result1),
	Result=Result1)->(Passed=passed,writeln([test,N,passed]));(Passed=failed,writeln([test,N,failed]))),!.


%%writeln([eg1]),
test(1,[[n,function],[1,1,[v,c]]],
[
        [[n,function],[[v,a],[v,b],[v,c]],":-",
        [
                [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ]
]
,[[[[v,c], 2]]]).
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
        [[n,count],[1,2]],
        [[n,count],[[v,n],[v,p]],":-",
        [
                [[n,not],[[[n,=],[[v,n],1]]]],
                [[n,+],[[v,n],1,[v,m]]],
                [[n,count],[[v,m],[v,p]]]
        ]
        ]
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
        ]
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

		  [[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound],[[v,t],[v,u]],"->",
		  ["[",[[n,compound21],[[v,t],[v,v]]],"]",
		  [[n,compound213],[[v,v],[v,u]]]]],

		  [[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,a]],
		  [[n,code],[[n,wrap],["a",[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,a]],",",
		  [[n,compound21],[[],[v,compound1name]]],
		  [[n,code],[[n,wrap],["a",[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]]]],

		  [[n,compound212],[[v,a],[v,a],[v,t],[v,t]]],

		  [[n,a],["",""]],

		  [[n,a],"->",["a"]],
		  
		  [[n,a],[[v,a],[v,a]]]

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

test(15,[[n,grammar1],["[[aa,b],1]",[v,t]]],
%%test(15,[[n,compound213],["","",[["a"],1],[v,t]]],

[
		  [[n,grammar1],[[v,u],[v,t]],":-",
		  [
		  			 [[n,compound],[[v,u],"",[],[v,t]]]
		  			 %%[[n,number21],[[v,u],"","",[v,t]]]
		  			 %%[[n,compound213],["","",[["a"],1],[v,t]]]
		  ]
		  ],

		  [[n,compound213],["","",[v,t],[v,t]]],

		  [[n,compound213],[[v,u],[v,u],[v,t],[v,t]]], %% swapped these

		  [[n,compound],[[v,t],[v,u]],"->",
		  ["[",[[n,compound21],[[v,t],[v,v]]],"]",
		  [[n,compound213],[[v,v],[v,u]]]]],

		  [[n,compound212],["","",[v,t],[v,t]]],

		  [[n,compound212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]],
		  %%[[n,lookahead],["]"]],
		  [[n,code],[[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]]],
		  [[n,compound212],[[v,v],[v,u]]]]],

		  [[n,compound21],[[v,t],[v,u]],"->",
		  [[[n,item],[[v,i]]],",",
		  [[n,compound21],[[],[v,compound1name]]],
		  [[n,code],[[n,wrap],[[v,i],[v,itemname1]]],
		  [[n,append],[[v,t],[v,itemname1],[v,v]]],
		  [[n,append],[[v,v],[v,compound1name],[v,u]]]]]],

		  [[n,item],[[v,t]],"->",
		  [[[n,number21],["",[v,u]]],[[n,code],
		  [[n,stringtonumber],[[v,u],[v,t]]]]]],

		  [[n,item],[[v,t]],"->",[[[n,word21],["",[v,t]]]]],

		  [[n,item],[[v,t]],"->",[[[n,compound],[[],[v,t]]]]],

		  [[n,number212],["","",[v,t],[v,t]]],

		  [[n,number212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,number21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,commaorrightbracketnext]],
		  [[n,code],[[n,stringtonumber],[[v,a],[v,a1]]],
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

		  [[n,word212],["","",[v,t],[v,t]]],

		  [[n,word212],[[v,u],[v,u],[v,t],[v,t]]],

		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],[[n,commaorrightbracketnext]],
		  [[n,code],[[n,letters],[[v,a]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word212],[[v,v],[v,u]]]]],

		  [[n,word21],[[v,t],[v,u]],"->",
		  [[v,a],
		  [[n,code],[[n,letters],[[v,a]]],
		  [[n,stringconcat],[[v,t],[v,a],[v,v]]]],
		  [[n,word21],["",[v,wordstring]]],
		  [[n,code],
		  [[n,stringconcat],[[v,v],[v,wordstring],[v,u]]]]]],
		  
		  [[n,commaorrightbracketnext],"->",
		  [[[n,lookahead],[","]]]],

		  [[n,commaorrightbracketnext],"->",
		  [[[n,lookahead],["]"]]]],
		  
		  [[n,lookahead],[[v,a],[v,a],[v,b]],":-",
		  [[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]

%%],[[[v,t],[["a"],1]]]).
],[[[[v,t],[["aa","b"],1]]]]).


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

%% Blackl loves the grammar

test(17,[[n,grammar1],["aaa1 ,-'! a? b! b.",[v,t]]],
%%test(15,[[n,compound213],["","",[["a"],1],[v,t]]],

[
		  [[n,grammar1],[[v,u],[v,t]],":-",
		  [
		  			 [[n,compound21],[[v,u],"",[],[v,t]]]
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
		  [[[n,item],[[v,i]]]," ",
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
                [[n,member],[[v,m],[v,h]]],
                [[n,+],[[v,s1],1,[v,s3]]],
                [[n,positivityscore],[[v,t],[v,m],[v,s3],
                	[v,s2]]]
        ]],
        
        [[n,positivityscore],[[v,l],[v,m],[v,s1],[v,s2]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,not],[[[n,member],[[v,m],[v,h]]]]],
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
