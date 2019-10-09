%% test(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

test(Debug,NTotal,Score) :- test(Debug,0,NTotal,0,Score),!.
test(_Debug,NTotal,NTotal,Score,Score) :- NTotal=37, !.
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
                [[n,member],[[v,m],[v,h]]],
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
        [       [[n,member2],[[v,ab],[v,ab1]]],
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
                [[n,member],[[v,m],[v,h]]],
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

%% Starts at 3, decreases given a or b until 1

test(33,[[n,downpipe],[3,1,[[3,[4,2]],[2,[3,1]]]]],

[
        [[n,downpipe],[[v,a],[v,a],[v,b]]],
        [[n,downpipe],[[v,a],[v,b],[v,c]],":-",
        [       [[n,member2],[[v,c],[v,c1]]],
                [[n,equals1],[[v,c1],[[v,c11],[v,c12]]]],
                [[n,equals1],[[v,c12],[[v,c121],[v,c122]]]],
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
        [       [[n,not],[[[n,=],[[v,a],0]]]],
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

test(37,[[n,length],[[1],0,[v,l]]],
[
        [[n,length],[[],[v,l],[v,l]]],
        [[n,length],[[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,+],[[v,m1],1,[v,m2]]],
                [[n,length],[[v,t],[v,m2],[v,n]]]
        ]
        ]
],[[[[v,l], 1]]]).
