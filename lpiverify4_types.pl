%% test_types(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test_types(Debug,NTotal,Score) :- test_types(Debug,0,NTotal,0,Score),!.
test_types(_Debug,NTotal,NTotal,Score,Score) :- NTotal=22, !.
test_types(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test_types_cases(NTotal3,Query,Types,Modes,Functions,Result),
	(international_interpret([lang,"en"],Debug,Query,Types,Modes,Functions,Result)->(Score3 is Score1+1,writeln([test_types,NTotal3,passed]));(Score3=Score1,writeln([test_types,NTotal3,failed]))),
	writeln(""),
	test_types(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

test_types1(Debug,N,Passed) :-
	test_types_cases(N,Query,Types,Modes,Functions,Result),
	((international_interpret([lang,"en"],Debug,Query,Types,Modes,Functions,Result)%%writeln(Result1),
	%%Result=Result1
	)->(Passed=passed,writeln([test_types,N,passed]));(Passed=failed,writeln([test_types,N,failed]))),!.


%%writeln([eg1]),
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

test_types_cases(3,[[n,function],[[v,a]]],
[[[n,function],[[[t,brackets],[[t,number]]]]]],
[[[n,function],[output]]],
[
        [[n,function],[[1]]]
]
,[[[[v,a], [1]]]]).

test_types_cases(4,[[n,f],[[v,a],[v,b],[v,c],[v,d]]],
[[[n,f],[[t,number],[t,string],[t,number],[t,string]]]],
[[[n,f],[output,output,output,output]]],
[
        [[n,f],[1,"a",2,"b"]]
]
,[[[[v,a], 1],[[v,b], "a"],[[v,c], 2],[[v,d], "b"]]]).

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

%%test_types_cases(7,[[n,getitemn],[1,[1,2,3],[v,bb]]],
test_types_cases(7,[[n,map],[[[n,add],[[[n,add],[[[n,add],[1]]]]]],0,[v,d]]],
[
        [[n,map],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,number]]]]],
        [t,number],[t,number]]],
        
        [[n,map],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,any]]]]],
        [t,number],[t,number]]],
        
        [[n,add],[[t,number],[t,number],[t,number]]],
        
        [[n,getitemn],[[t,number],[[t,list],[[t,any]]],[t,any]]]

        %%[[n,getitemn],[[t,number],
        %%[[t,brackets],[[[t,list],[[t,any]]]]],[t,any]]]
],
[
        [[n,map],[input,input,output]],
                
        [[n,add],[input,input,output]],
        
        [[n,getitemn],[input,input,output]]
],
[
        [[n,map],[[v,f1],[v,l],[v,n]],":-",
        [        
                [[n,equals1],[[v,f1],[[v,f11],[v,f12]]]],
                [[n,=],[[v,f11],[n,add]]],
                [[n,getitemn],[1,[v,f12],[v,bb]]],
                [[n,number],[[v,bb]]],
                [[v,f11],[[v,l],[v,bb],[v,n]]]
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

,[[[[v,d], 3]]]).
%%,[[[[v,bb], 1]]]).


test_types_cases(8,[[n,f],[[v,d],[v,a],[v,c]]],
[[[n,f],[[t,number],[[t,list],[[t,number],[t,string]]],[t,number]]]],
[[[n,f],[output,output,output]]],
[
        [[n,f],[1,[1,"a",2,"b"],1]]
]
,[[[[v,d], 1],[[v,a], [1,"a",2,"b"]],[[v,c], 1]]]).

test_types_cases(9,[[n,f],[1,"a"]],
[
        [[n,f],[[t,a],[t,b]]],
        [[t,a],[[t,number]]],
        [[t,b],[[t,string]]]
],
[
        [[n,f],[input,input]]
],
[
        [[n,f],[1,"a"]]
]
,[[]]).

test_types_cases(10,[[n,f],["a"]],
[
        [[n,f],[[t,a]]],
        [[t,a],[[t,number]]],
        [[t,a],[[t,string]]]
],
[
        [[n,f],[input]]
],
[
        [[n,f],["a"]]
]
,[[]]).

test_types_cases(11,[[n,call1b],[[1,11,111],[v,b]]],
        [[[n,call1b],[[[t,brackets],[[t,number],[t,number],[t,number]]],[t,number]]]],
        [[[n,call1b],[input,output]]],

[
        [[n,call1b],[[v,a],[v,b]],":-",
        [       [[n,call],[[lang,same],same,[[n,member2a],[[v,a],[v,b]]],
        [[[n,member2a],[[[t,brackets],[[t,number],[t,number],[t,number]]],[t,number]]]],
        [[[n,member2a],[input,output]]],

[[[n,member2a],[[v,a],[v,b]],":-",
        [       [[n,member2],[[v,a],[v,b]]],[[n,cut]]]
        ]]]],
        [[n,cut]]]]       
        
],[[[[v,b],1]]]).


test_types_cases(12,[[n,call1b],[[1,11,111],[v,b]]],
        [[[n,call1b],[[[t,brackets],[[t,number],[t,number],[t,number]]],[t,number]]]],
        [[[n,call1b],[input,output]]],

[
        [[n,call1b],[[v,a],[v,b]],":-",
        [       [[n,call],[[lang,same],same,[[n,member2a],[[v,a],[v,b]]],
        %[[[n,member2a],[[[t,brackets],[[t,number],[t,number],[t,number]]],[t,number]]]],
        %[[[n,member2a],[input,output]]],

[[[n,member2a],[[v,a],[v,b]],":-",
        [       [[n,member2],[[v,a],[v,b]]],[[n,cut]]]
        ]]]],
        [[n,cut]]]]       
        
],[[[[v,b],1]]]).



test_types_cases(13,[[n,person],["not-care",[v,output]]],
        [[[n,person],[[t,string],[t,string]]]],
        [[[n,person],[input,output]]],

[
        [[n,person],["care","care"]],     
        [[n,person],[[v,a],"justice to care"]]    
        
],[[[[v,output],"justice to care"]]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Dereconstruction 3 of 4.txt",0,algorithms,"24.   *I prepared to be an expert on the brain.  I did this by writing about neuroscience.  First I wrote about food.  Second, I wrote about activity.  Third, I wrote about sleep.  In this way, I prepared to be an expert on the brain by writing about neuroscience."]

test_types_cases(14,[[n,neuroscience],["**","***",[v,output]]],
        [[[n,neuroscience],[[t,string],[t,string],[t,number]]]],
        [[[n,neuroscience],[input,input,output]]],

[
        [[n,neuroscience],[[v,a],[v,b],[v,c]],":-",
        [       [[n,string_length],[[v,a],[v,a1]]],
                [[n,string_length],[[v,b],[v,b1]]],
                [[n,+],[[v,a1],[v,b1],[v,c]]]
        ]]
        
],[[[[v,output],5]]]).

% ["Fundamentals of Meditation and Meditation Indicators","FUNDAMENTALS OF MEDITATION by Lucian Green Blue Nature 1 of 4.txt",0,algorithms,"9.     *I prepared to connect together main points from cliques.  I did this by writing on something interesting to do with the song.  First, I identified the topic.  Second, I constructed an aphohedron from all the song’s parts.  Third, I thought of interconnections between clique nodes from the randomly broken down aphohedron.  In this way, I prepared to connect together main points from cliques by writing on something interesting to do with the song."]

test_types_cases(15,[[n,connect_cliques],[[["a",1],[1,2],[2,"b"]],[["a",3],[3,4],[4,"b"]],[v,output]]],
        [[[n,connect_cliques],[[t,list2],[t,list2],[t,list2]]],
        [[t,list2],[[[t,list],[[t,set]]]]],
        [[t,set],[[[t,list],[[t,item]]]]],
        [[t,item],[[t,number]]],
        [[t,item],[[t,string]]]],
        [[[n,connect_cliques],[input,input,output]]],

[
        [[n,connect_cliques],[[v,a],[v,b],[v,c]],":-",
        [       [[n,append],[[v,a],[v,b],[v,c]]]
        ]]        
],[[[[v,output],[["a",1],[1,2],[2,"b"],["a",3],[3,4],[4,"b"]]]]]).

/**
test_types_cases(16,[[n,a],[[1,[1,[1]]]]],
        [[[n,a],[[t,b2]]],
        [[t,b2],[[t,number]]],
        [[t,b2],[[t,number],[t,b2]]]],
        [[[n,a],[input]]],

[
        [[n,a],[[v,a]],":-",
        [       [[n,true]]
        ]]        
],[[]]).
**/

% ["Fundamentals of Meditation and Meditation Indicators","FUNDAMENTALS OF MEDITATION by Lucian Green Children, H1, Earning Jobs and Protection in Jobs 3 of 4.txt",0,algorithms,"24.     *I prepared to watch the insect eat a fruit.  I did this by feeding it the raspberry.  First, I lifted the raspberry on a fork.  Second, I placed it in the airlock.  Third, I unlocked the airlock’s den side to feed the raspberry to the mosquitoes.  In this way, I prepared to watch the insect eat a fruit by feeding it the raspberry."]


test_types_cases(16,[[n,insect_food],["food",[v,stomach]]],
        [[[n,insect_food],[[t,string],[t,string]]]],
        [[[n,insect_food],[input,output]]],

[
        [[n,insect_food],[[v,mouth],[v,mouth]]]
],[[[[v,stomach],"food"]]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Drawing connections 1 of 4.txt",0,algorithms,"9.     I prepared to distance myself from *.  I did this by shelling the snow pea.  First, I read that Winston Churchill asked that if funding was diverted from arts then what would we be fighting for?  Second, I determined that arts was the conclusion from defence, not vice versa.  Third, I determined that arts is necessary rather than defence.  In this way, I prepared to distance myself from * by shelling the snow pea."]

test_types_cases(17,[[n,distance_myself],[2,[v,distance]]],
        [[[n,distance_myself],[[t,number],[t,number]]]],
        [[[n,distance_myself],[input,output]]],

        [[[n,distance_myself],[[v,a],[v,b]],":-",
        [       [[n,*],[[v,a],2,[v,b]]]
        ]]       
],[[[[v,distance],4]]]).

% ["Short Arguments","Nut_and_Bolt.txt",0,algorithms,"13. *I prepared to want the baby.  I did this by synthesising the chemistry of the reproductive system with the nut and bolt.  First, I found the baby.  Second, I found the parents.  Third, I bolted the baby to the parents."]

test_types_cases(18,[[n,want_baby],["yes","yes","yes",[v,result]]],
        [[[n,want_baby],[[t,string],[t,string],[t,string],[t,string]]]],
        [[[n,want_baby],[input,input,input,output]]],

        [[[n,want_baby],[[v,money],[v,as],[v,parents_in_academia],[v,result]],":-",
        [       [[n,=],[[v,money],"yes"]],
                [[n,=],[[v,as],"yes"]],
                [[n,=],[[v,parents_in_academia],"yes"]],
                [[n,=],["yes",[v,result]]]
                
        ]]       
],[[[[v,result],"yes"]]]).

% ["Short Arguments","Competition 3.txt",0,algorithms,"28. I explored losing as well.  I agreed with the competition.  I found the competitor.  * I saw he was weaker.  I agreed with (was stronger than) him."]

test_types_cases(19,[[n,saw_weaker],[1,0,[v,result]]],
        [[[n,saw_weaker],[[t,number],[t,number],[t,number]]]],
        [[[n,saw_weaker],[input,input,output]]],

        [[[n,saw_weaker],[[v,my_value],[v,his_value],[v,result]],":-",
        [       [[n,>],[[v,my_value],[v,his_value]]],
                [[n,=],[1,[v,result]]]

        ]]       
],[[[[v,result],1]]]).


% ["Time Travel","Interesting histories to visit 4.txt",0,algorithms,"*Interesting histories to visit 4"]

test_types_cases(20,[[n,visit_staged_history],["us",[v,staged_history]]],
        [[[n,visit_staged_history],[[t,string],[t,string]]]],
        [[[n,visit_staged_history],[input,output]]],

[
        [[n,visit_staged_history],[[v,now],[v,now]]]
],[[[[v,staged_history],"us"]]]).

% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green Time to Prepare 2 of 4.txt",0,algorithms,"11. *The teacher prepared to represent being interested in a lesson by “dotting it on”. He did this by climbing the rope ladder. First, he found the correct ladder. Second, he tested that the ladder was about to start. Third, he climbed the ladder with his arms and legs. In this way, the teacher prepared to represent being interested in a lesson by “dotting it on” by climbing the rope ladder."]

test_types_cases(21,[[n,memorise_point],["point",[v,memory_out]]],
        [[[n,memorise_point],[[t,string],[t,string]]]],
        [[[n,memorise_point],[input,output]]],

[
        [[n,memorise_point],[[v,memory_in],[v,memory_in]]]
],[[[[v,memory_out],"point"]]]).

% ["Fundamentals of Meditation and Meditation Indicators","FUNDAMENTALS OF MEDITATION by Lucian Green Hours Prayer 1 of 4.txt",0,algorithms,"8.    *I prepared to endorse Nietzsche’s brilliance.  I did this by writing Alexius Meinong’s probable comments on the Medicine blog.  First, I called it Anarchy 3.  Second, I liked brilliance.  Third, I liked Nietzsche’s brilliance.  In this way, I prepared to endorse Nietzsche’s brilliance by writing Alexius Meinong’s probable comments on the Medicine blog."]

test_types_cases(22,[[n,function],[[["a","b"],["b","c"]]]],
        [[[n,function],[[[t,list],[[t,list2]]]]],
        [[t,list2],[[t,string],[t,string]]],
        [[n,reverse],[[[t,list],[[t,list2]]],[[t,list],[[t,list2]]],[[t,list],[[t,list2]]]]],
        [[n,function2],[[[t,list],[[t,list2]]],[t,string],[t,string]]],
        [[n,length],[[[t,list],[[t,list2]]],[t,number],[t,number]]]],
        
        [[[n,function],[input]],
        [[n,reverse],[input,input,output]],
        [[n,function2],[input,input,input]],
        [[n,length],[input,input,output]]],

[[[n,function],[[v,a]],":-",
[[[n,length],[[v,a],0,[v,b]]],
[[n,=],[[v,b],1]]]],

[[n,function],[[v,a]],":-",
[[[n,head],[[v,a],[v,d]]],
[[n,equals1],[[v,d],[[v,e],[v,f]]]],
[[n,reverse],[[v,a],[],[v,a1]]],
[[n,head],[[v,a1],[v,d1]]],
[[n,equals1],[[v,d1],[[v,e1],[v,f1]]]],
[[n,function2],[[v,a],[v,f],[v,f1]]]]],

[[n,reverse],[[],[v,l],[v,l]]],

[[n,reverse],[[v,l],[v,m],[v,n]],":-",
[[[n,head],[[v,l],[v,h]]],
[[n,tail],[[v,l],[v,t]]],
[[n,wrap],[[v,h],[v,h1]]],
[[n,append],[[v,h1],[v,m],[v,o]]],
[[n,reverse],[[v,t],[v,o],[v,n]]]]],

[[n,function2],[[v,a],[v,b],[v,f]],":-",
[[[n,member2],[[v,a],[v,d]]],
[[n,equals1],[[v,d],[[v,b],[v,f]]]]]],

[[n,function2],[[v,a],[v,b],[v,c]],":-",
[[[n,member2],[[v,a],[v,d]]],
[[n,equals1],[[v,d],[[v,b],[v,f]]]],
[[n,function2],[[v,d],[v,f],[v,c]]]]],

[[n,length],[[],[v,l],[v,l]]],

[[n,length],[[v,l],[v,m1],[v,n]],":-",
[[[n,not],[[[n,=],[[v,l],[]]]]],
[[n,tail],[[v,l],[v,t]]],
[[n,+],[[v,m1],1,[v,m2]]],
[[n,length],[[v,t],[v,m2],[v,n]]]]]],
[[]]).

