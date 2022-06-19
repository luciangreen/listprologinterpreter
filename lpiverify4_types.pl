%% test_types(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test_types(Debug,NTotal,Score) :- test_types(Debug,0,NTotal,0,Score),!.
test_types(_Debug,NTotal,NTotal,Score,Score) :- NTotal=68, !.
test_types(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test_types_cases(NTotal3,Query,Types,Modes,Functions,Result),
	(international_interpret([lang,"en"],Debug,Query,Types,Modes,Functions,Result)->(Score3 is Score1+1,writeln0([test_types,NTotal3,passed]));(Score3=Score1,writeln0([test_types,NTotal3,failed]))),
	writeln0(""),
	test_types(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

test_types1(Debug,N,Passed) :-
	test_types_cases(N,Query,Types,Modes,Functions,Result),
	((international_interpret([lang,"en"],Debug,Query,Types,Modes,Functions,Result1),%writeln(Result1),
	Result=Result1
	)->(Passed=passed,writeln0([test_types,N,passed]));(Passed=failed,writeln0([test_types,N,failed]))),!.


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
[
[[n,function],[[[t,number]]]]
],
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

test_types_cases(7,[[n,map],[[[n,add],[[[n,add],[[[n,add],[1]]]]]],0,[v,d]]],
%test_types_cases(7,[[n,map],[1,0,[v,d]]],
[
[[n,map],[[t,map1],[t,number],[t,number]]],
[[t,map1],[[t,number]]],
[[t,map1],[[[t,predicatename],[[t,map1]]]]],
[[n,add],[[t,number],[t,number],[t,number]]],
[[n,getitemn],[[t,number],{[t,any]},[t,any]]]
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
%%,[[[[v,bb], 1]]]).


test_types_cases(8,[[n,f],[[v,d],[v,a],[v,c]]],
[
[[n,f],[[t,number],{[t,number],[t,string]},[t,number]]]
],
[[[n,f],[output,output,output]]],
[
        [[n,f],[1,[1,"a",2,"b"],1]]
]
,[[[[v,a],[1,"a",2,"b"]],[[v,c],1],[[v,d],1]]]).

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
        [
[[n,call1b],[[[t,number],[t,number],[t,number]],[t,number]]]
],
        [[[n,call1b],[input,output]]],

[
        [[n,call1b],[[v,a],[v,b]],":-",
        [       [[n,call],[[lang,same],same,[[n,member2a],[[v,a],[v,b]]],
        [[[n,member2a],[[[t,number],[t,number],[t,number]],[t,number]]]],
        [[[n,member2a],[input,output]]],

[[[n,member2a],[[v,a],[v,b]],":-",
        [       [[n,member2],[[v,a],[v,b]]],[[n,cut]]]
        ]]]],
        [[n,cut]]]]       
        
],[[[[v,b],1]]]).


test_types_cases(12,[[n,call1b],[[1,11,111],[v,b]]],
        [
[[n,call1b],[[[t,number],[t,number],[t,number]],[t,number]]]
],
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
        [
[[n,connect_cliques],[[t,list2],[t,list2],[t,list2]]],
[[t,list2],[{[t,set]}]],
[[t,set],[{[t,item]}]],
[[t,item],[[t,number]]],
[[t,item],[[t,string]]]
],
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

% trope chain

test_types_cases(22,[[n,function],[[["a","b"],["b","c"]]]],
        [
[[n,function],[{[t,list2]}]],
[[t,list2],[[t,string],[t,string]]],
[[n,reverse],[{[t,list2]},{[t,list2]},{[t,list2]}]],
[[n,function2],[{[t,list2]},[t,string],[t,string]]],
[[n,length],[{[t,list2]},[t,number],[t,number]]]
],
        
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
[[n,function2],[[v,a],[v,f],[v,f1]]]
%,[[n,cut]]
]],

[[n,reverse],[[],[v,l],[v,l]]],

[[n,reverse],[[v,l],[v,m],[v,n]],":-",
[[[n,head],[[v,l],[v,h]]],
[[n,tail],[[v,l],[v,t]]],
[[n,wrap],[[v,h],[v,h1]]],
[[n,append],[[v,h1],[v,m],[v,o]]],
[[n,reverse],[[v,t],[v,o],[v,n]]]]],

[[n,function2],[[v,a],[v,b],[v,f]],":-",
[[[n,member2],[[v,a],[v,d]]],
[[n,equals1],[[v,d],[[v,b],[v,f]]]]
%,[[n,cut]]
]],

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


% recursive types

test_types_cases(23,[[n,connect_cliques],[[["a",1],[1,2],[2,"b"]],[["a",3],[3,4],[4,"b"]],[v,output]]],
        [
[[n,connect_cliques],[[t,list2],[t,list2],[t,list2]]],
[[t,item],[[t,number]]],
[[t,item],[[t,string]]],
[[t,list2],[{[t,item]}]],
[[t,list2],[{[t,list2]}]]
],
        [[[n,connect_cliques],[input,input,output]]],

[
        [[n,connect_cliques],[[v,a],[v,b],[v,c]],":-",
        [       [[n,append],[[v,a],[v,b],[v,c]]]
        ]]        
],[[[[v,output],[["a",1],[1,2],[2,"b"],["a",3],[3,4],[4,"b"]]]]]).


% ["Mind Reading","Mr other times 7.txt",0,algorithms,"57. *I responsibly chose an ontological value (side of the car that the steering wheel was on in the particular car) by mind reading the other time."]

% Aus, UK - left hand traffic, US - right hand traffic

test_types_cases(24,[[n,hand_traffic],["Australia",[v,a1]]],
        [[[n,hand_traffic],[[t,string],[t,string]]]],
        [[[n,hand_traffic],[input,output]]],

[
        [[n,hand_traffic],["Australia","left"]],
        [[n,hand_traffic],["UK","left"]],
        [[n,hand_traffic],["US","right"]]
        
],[[[[v,a1],"left"]]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Finite Data will be a Solution in Conglish 2 of 4.txt",0,algorithms,"17.    *I prepared to judge the way the other person was speaking.  I did this by watching the diareasoner identify the speech rate in her partner.  First, I counted the number of words over the time.  Second, I counted the number of minutes.  Third, I calculated the speech rate to equal the number of words divided by the number of minutes.  In this way, I prepared to judge the way the other person was speaking by watching the diareasoner identify the speech rate in her partner."]

test_types_cases(25,[[n,way_of_speaking1],[["high-pitched","smiling"],[v,way]]],
        [
[[n,way_of_speaking1],[{[t,string]},{[t,string]}]],
[[n,way_of_speaking],[[t,string],[t,string]]]
],
        [[[n,way_of_speaking1],[input,output]],
        [[n,way_of_speaking],[input,output]]],

[
        [[n,way_of_speaking1],[[v,properties],[v,expression]],":-",
        [[[n,equals4],[[v,properties],[[v,item1a],[v,item1b]]]],
               [[n,findall],[[v,item2],
         [[[n,way_of_speaking],[[v,item1a],[v,item2]]]],
         %[[n,=],[[v,item1],[v,item1a]]]],
        [v,items2a]]],
         [[n,sort],[[v,items2a],[v,items2a1]]],
              [[n,findall],[[v,item2],
         [[[n,way_of_speaking],[[v,item1b],[v,item2]]]],
         %[[n,=],[[v,item1],[v,item1b]]]],
        [v,items2b]]],
        [[n,sort],[[v,items2b],[v,items2b1]]],
        [[n,intersection],[[v,items2a1],[v,items2b1],[v,expression]]]
        ]],        
        [[n,way_of_speaking],["high-pitched","happy"]],
        [[n,way_of_speaking],["high-pitched","unhappy"]],
        [[n,way_of_speaking],["low-pitched","angry"]],
        [[n,way_of_speaking],["smiling","happy"]],
        [[n,way_of_speaking],["frowning","sad"]]
],[[[[v,way],["happy"]]]]).

% ["Time Travel","Space Flight 2.txt",0,algorithms,"32. *The space ship algorithm automated meditation before space jumps and when it detected pedagogy help."]

% The time around the time to time travel to was mind read.

% choose >= 10 time units after the projectile has passed.

test_types_cases(26,[[n,choose_time],[[-15,-10,-5,0,5,10,15],[v,time]]],
        [
[[n,choose_time],[{[t,number]},[t,number]]]
],
        [[[n,choose_time],[input,output]]],

[
        [[n,choose_time],[[v,a],10],":-",
        [       [[n,member],[[v,a],10]]
        ]]        
],[[[[v,time],10]]]).


% ["Time Travel","Technologies in Times 1.txt",0,algorithms,"63. *The workings of DNA and RNA were examined in cloning for medicine."]

% The sequences were the same.

test_types_cases(27,[[n,same],[[1,2,3,4,5,6],[1,2,3,4,5,6]]],
        [
[[n,same],[{[t,number]},{[t,number]}]]
],
        [[[n,same],[input,input]]],

[
        [[n,same],[[v,sequence1],[v,sequence1]]]
],[[]]).

% ["Lecturer","Lecturer.txt",0,algorithms,"2. *I found what the person aimed for.  I wrote on hermeneutics.  I identified the discourse.  I grouped the topics into ideologies.  I grouped the ideas into ontologies."]

test_types_cases(28,[[n,aimed],[[["bulls-eye","red"],["outer-ring","blue"]],"bulls-eye",[v,object]]],
        [
[[n,aimed],[{{[t,string],[t,string]}},[t,string],[t,string]]]
],
        [[[n,aimed],[input,input,output]]],

[
        [[n,aimed],[[v,a],[v,b],[v,c]],":-",
        [       [[n,member2],[[v,a],[v,d]]],
                [[n,equals4],[[v,d],[[v,b],[v,c]]]]
        ]]        
],[[[[v,object],"red"]]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Analysing characteristics of arguments 1 of 4.txt",0,algorithms,"Do the premises work in all cases?"]

test_types_cases(29,[[n,verify_modus_ponens],[["a",["a","b"],"b"]]],
        [
[[n,verify_modus_ponens],[[[t,string],{[t,string],[t,string]},[t,string]]]]
],
        [[[n,verify_modus_ponens],[input]]],

[
        [[n,verify_modus_ponens],[[v,a]],":-",
        [       [[n,equals4],[[v,a],[[v,a1],[[v,a1],[v,b1]],[v,b1]]]]
        ]]        
],[[]]).


% ["Short Arguments","Rebreasoning.txt",0,algorithms,"8. I prepared to verify the connection.  I did this by moving on to the next point.  First, I breasoned out the first point.  Second, I breasoned out the connection to the second point.  Third, I moved on to the second point."]

% I prepared to work the connection out.

test_types_cases(30,[[n,work_modus_ponens_out],[["a","b"],[v,mp]]],
        [
[[n,work_modus_ponens_out],[{[t,string]},{[t,string]}]]
],
        [[[n,work_modus_ponens_out],[input,output]]],

[
        [[n,work_modus_ponens_out],[[v,a],[v,c]],":-",
        [       [[n,equals4],[[v,a],[[v,a1],[v,b1]]]],
                [[n,equals4],[[v,c],[[v,a1],"->",[v,b1]]]]
        ]]        
],[[[[v,mp],["a","->","b"]]]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Exploring opposites in Hamlet 2 of 4.txt",0,algorithms,"11.    *I prepared to experience the art forms of God (the master).  I did this by trusting God (the master).  First, I trusted the art of the master.  Second, I trusted the music of the master.  Third, I trusted the architecture of the master.  In this way, I prepared to experience the art forms of God (the master) by trusting God (the master)."]

test_types_cases(31,[[n,art],[["I","ate","apple"],[v,art_form]]],
        
[[[n,art],[{[t,string]},[[t,string],{[t,string]}]]]]
,
        [[[n,art],[input,output]]],

[
        [[n,art],[[v,a],[v,c]],":-",
        [       [[n,equals4],[[v,a],[[v,a1],[v,b1],[v,c1]]]],
                [[n,equals4],[[v,c],[[v,b1],[[v,a1],[v,c1]]]]]
        ]]        
],[[[[v,art_form],["ate",["I","apple"]]]]]).

% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green Time to Do 3 of 4.txt",0,algorithms,"28. The fun park visitor prepared to ride the helter skelter. He did this by licking the chocolate from his finger. First, he started from the base of his finger. Second, he spiraled his tongue upwards, licking all the chocolate from his finger on the way. Third, he stopped when he reached the top. In this way, the fun park visitor prepared to ride the helter skelter by licking the chocolate from his finger."]

% Triangle train line

test_types_cases(32,[[n,triangle_train1],["Canterbury","Bambury"]],
        [
[[n,triangle_train1],[[t,string],[t,string]]],
[[n,triangle_train],[[t,string],[t,string]]],
[[n,link],[[t,string],[t,string]]]
],
        [[[n,triangle_train1],[input,input]],
        [[n,triangle_train],[input,output]],
        [[n,link],[input,output]]],

[
        [[n,triangle_train1],[[v,a],[v,b]],":-",
        [       [[n,triangle_train],[[v,a],[v,b]]]]],
        [[n,triangle_train],[[v,a],[v,b]],":-",
        [       [[n,link],[[v,a],[v,b]]]]],
        [[n,triangle_train],[[v,a],[v,b]],":-",
        [       [[n,link],[[v,a],[v,c]]],
                [[n,triangle_train],[[v,c],[v,b]]]
        ]],
        %[[n,link],["Canterbury","Bambury"]],
        [[n,link],["Canterbury","Avignon"]],
        [[n,link],["Bambury","Canterbury"]],
        %[[n,link],["Bambury","Avignon"]],
        [[n,link],["Avignon","Bambury"]]
        %[[n,link],["Avignon","Canterbury"]]
        
],[[]]).


test_types_cases(33,[[n,wear],[["hat","head"],[v,c]]],
        [
[[n,wear],[[[t,string],[t,string]],[[t,string],[t,string]]]]
],
        [[[n,wear],[input,output]]],

[
        [[n,wear],[[v,c],[v,c]],":-",
        [        
                [[n,equals4],[[v,c],["hat","head"]]]
        ]]
],[[[[v,c],["hat","head"]]]]).

% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green Breathsonings 4 of 4.txt",0,algorithms,"41.   I loved planet meditation (books).  I did this by holding the reflection (philosophy) retreat.  First, I covered texts.  Second, I covered retreat details.  Third, I gave presents out.  In this way, I prepared to love planet meditation (books) by holding the reflection (philosophy) retreat."]

%% travelling 10 space units and 15 time units in the maximum jump of 1 space unit and 1 time unit takes 10 space jumps and 15 time jumps

test_types_cases(34,[[n,space_time_jump],[[10,15],[v,c]]],
        [
[[n,space_time_jump],[[[t,number],[t,number]],[[t,number],[t,number]]]]
],
        [[[n,space_time_jump],[input,output]]],

[
        [[n,space_time_jump],[[v,c],[v,c]]]
],[[[[v,c],[10,15]]]]).


% ["Short Arguments","Medicine - Quantum Box of Circulatory System 1.txt",0,algorithms,"5. *I used cardiovascular activity to maintain circulatory system flow."]

test_types_cases(35,[[n,circulation1],["heart1","cells"]],
        [[[n,circulation1],[[t,string],[t,string]]],
        [[n,circulation],[[t,string],[t,string]]],
        [[n,link],[[t,string],[t,string]]]],
        [[[n,circulation1],[input,input]],
        [[n,circulation],[input,output]],
        [[n,link],[input,output]]],

[
        [[n,circulation1],[[v,a],[v,b]],":-",
        [       [[n,circulation],[[v,a],[v,b]]]]],
        [[n,circulation],[[v,a],[v,b]],":-",
        [       [[n,link],[[v,a],[v,b]]]]],
        [[n,circulation],[[v,a],[v,b]],":-",
        [       [[n,link],[[v,a],[v,c]]],
                [[n,circulation],[[v,c],[v,b]]]
        ]],
        %[[n,link],["Canterbury","Bambury"]],
        [[n,link],["heart1","lungs"]],
        [[n,link],["lungs","heart2"]],
        %[[n,link],["Bambury","Avignon"]],
        [[n,link],["heart2","cells"]],
        [[n,link],["cells","heart1"]]
        
],[[]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Philosophical Computational English 4 of 4.txt",0,algorithms,"32.   *I prepared to buy products that I added value to.  I did this by breasoning out 5 As per day for sales.  First, I trialled the product.  Second, I found a new use for the product.  Third I used the product for thus new use.  In this way, I prepared to buy products that I added value to by breasoning out 5 As per day for sales."]

test_types_cases(36,[[n,stock_because_buy],["word processor",[v,c]]],
        [[[n,stock_because_buy],[[t,string],[t,string]]]],
        [[[n,stock_because_buy],[input,output]]],

[
        [[n,stock_because_buy],[[v,c],[v,c]]]
],[[[[v,c],"word processor"]]]).

% ["Lecturer","Lecturer - Recordings Pedagogy.txt",0,algorithms,"6. *The necessary amount of work didn't become a headache.  I prevented the muscle ache from recordings.  I used the quantum box to prevent muscle aches.  This included headaches.  I prevented body aches becoming headaches."]

% The number of As of work equals the number of As in medicine

test_types_cases(37,[[n,medicine_as],[3,[v,medicine_as]]],
        [[[n,medicine_as],[[t,number],[t,number]]]],
        [[[n,medicine_as],[input,output]]],

[
        [[n,medicine_as],[[v,as],[v,as]]]
],[[[[v,medicine_as],3]]]).

% ["Medicine","MEDICINE by Lucian Green Heart 2 of 4.txt",0,algorithms,"17.    *I prepared to go running. I did this by flexing the ball of my foot. First, I stood up. Second, I leant against a wall. Third, I performed a calf stretch. In this way, I prepared to go running by flexing the ball of my foot."]

test_types_cases(38,[[n,run_checklist],["true","true","true",[v,run]]],
        [[[n,run_checklist],[[t,string],[t,string],[t,string],[t,string]]]],
        [[[n,run_checklist],[input,input,input,output]]],

[
        [[n,run_checklist],[[v,stretches],[v,water],[v,gear],[v,run]],":-",
        [       [[n,=],[[v,stretches],"true"]],
                [[n,=],[[v,water],"true"]],
                [[n,=],[[v,gear],"true"]],
                [[n,=],[[v,run],"true"]]
        ]]
],[[[[v,run],"true"]]]).


% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green Time to Prepare 3 of 4.txt",0,algorithms,"27. *The bottler prepared to put a cork in the bottle. He did this by closing the refrigerator door. First, he pushed the door with his hand. Second, he lifted the latch. Third, he closed the door. In this way, the bottler prepared to put a cork in the bottle by closing the refrigerator door."]

% fill or empty a bottle

test_types_cases(39,[[n,fill_or_empty_bottle],["nothing",[v,a2]]],
        [[[n,fill_or_empty_bottle],[[t,string],[t,string]]]],
        [[[n,fill_or_empty_bottle],[input,output]]],

[
        [[n,fill_or_empty_bottle],["nothing","liquid"]],
        [[n,fill_or_empty_bottle],["liquid","nothing"]]
],[[[[v,a2],"liquid"]]]).

% ["Lecturer","Lecturer Culturology.txt",0,algorithms,"3. *Reverse CAW was guessing the input and output.  Culturology is good.  I applied back-translation to an algorithm.  I found that reversing the algorithm resulted in the same result as the original.  Reverse interpret was CAW."]

test_types_cases(40,[[n,guess_io],["+",[v,a2],[v,a3]]],
        [
[[n,guess_io],[[t,string],{[t,item]},{[t,item]}]],
[[t,item],[[t,number]]],
[[t,item],[[t,string]]]
],
        [[[n,guess_io],[input,output,output]]],

[
        [[n,guess_io],["+",[1,1],[2]]],
        [[n,guess_io],["-",[1,1],[0]]]
],[[[[v,a2],[1,1]],[[v,a3],[2]]]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Order in Conglish 2 of 4.txt",0,algorithms,"15.   *I prepared to order the Conglish subjects.  I did this by observing the marriage.  First, I observed the partner place the ring on his or her partner’s finger.  Second, I observed the couple say their vows.  Third, I observed the couple sign the wedding register.  In this way, I prepared to order the Conglish subjects by observing the marriage."]

% Order strings by length

test_types_cases(41,[[n,order_strings],[["***","*","**"],[v,ordered_strings]]],
        [
[[n,order_strings],[{[t,string]},{[t,string]}]]
],
        [[[n,order_strings],[input,output]]],

[
        [[n,order_strings],[[v,strings],[v,ordered_strings]],":-",
        [       [[n,sort],[[v,strings],[v,ordered_strings]]]]]
],[[[[v,ordered_strings],["*", "**", "***"]]]]).

%  ["Medicine","MEDICINE by Lucian Green 250 Breasonings 1 of 4.txt",0,algorithms,"5. I prepared to listen to the classical music, which had an expanse of 50 As.  I did this by listening to classical music.  First, I found the record.  Second, I played it on the gramophone.  Third, I listened to the classical music.  In this way, I prepared to listen to the classical music, which had an expanse of 50 As by listening to classical music."]

test_types_cases(42,[[n,as_expanse],[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]]],
[
[[n,as_expanse],[{[t,number]}]]
],
        [[[n,as_expanse],[input]]],
[
        [[n,as_expanse],[[v,numbers]],":-",
        [       [[n,findall],[[v,num],[[[n,member2],[[v,numbers],[v,num]]],
        	[[n,number],[[v,num]]]],[v,num2]]]]]
],[[]]).

test_types_cases(43,[[n,is_classical],[[2,3,5,6,7,10,11]]],
[
[[n,is_classical],[[t,numbers]]],
[[n,is_set1],[[t,numbers],[t,numbers]]],
[[t,numbers],[{[t,number]}]]
],
        [[[n,is_classical],[input]],
        [[n,is_set1],[input,input]]],
[
        [[n,is_classical],[[v,numbers]],":-",
        [       %[[n,trace2]],
        	[[n,is_set1],[[v,numbers],[2,3,5,6,7,10,11]]]]],
        	
        [[n,is_set1],[[v,set1],[v,set2]],":-",
        [       [[n,sort],[[v,set1],[v,set3]]],
        						[[n,sort],[[v,set2],[v,set3]]]
        ]]

],[[]]).

test_types_cases(44,[[n,find_record],[[[1,"a"],[2,"b"]],1,[v,r]]],
[
[[n,find_record],[{[[t,number],[t,string]]},[t,number],[t,string]]]
],
        [[[n,find_record],[input,input,output]]],
[
        [[n,find_record],[[v,pairs],[v,num],[v,rec]],":-",
        [       %[[n,trace2]],
        	[[n,member],[[v,pairs],[[v,num],[v,rec]]]]]]
]

,[[[[v,r],"a"]]]).


test_types_cases(45,[[n,play_gramophone],[[1,2,3,4,5],2,[v,p]]],
[
[[n,play_gramophone],[{[t,number]},[t,number],{[t,number]}]]
],
        [[[n,play_gramophone],[input,input,output]]],
[
        [[n,play_gramophone],[[v,tracks],[v,first_track],[v,rest]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[[v,a],"|",[v,b]],[v,tracks]]],
        	[[n,"->"],[[[n,equals4],[[v,a],[v,first_track]]],
        	[[n,equals4],[[v,rest],[v,tracks]]],
        	[[n,play_gramophone],[[v,b],[v,first_track],[v,rest]]]
        	]]
]]]

,[[[[v,p],[2,3,4,5]]]]).

/*
 Sales
1. Inner child

1. The product was spoon-fed to the customer.
*/

test_types_cases(46,[[n,spoon_feed],[1]],
[[[n,spoon_feed],[[t,number]]]],
        [[[n,spoon_feed],[input]]],
[

        [[n,spoon_feed],[5]],
        [[n,spoon_feed],[[v,n1]],":-",
        [       %[[n,trace2]],
        	[[n,+],[[v,n1],1,[v,n2]]],
        	[[n,spoon_feed],[[v,n2]]]]]
]
,[[]]).

% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Perspectives 1 of 4.txt",0,algorithms,"1. The first technique can be used to give a perspective on a text. For example, given the reason 'X is younger than Y' the perspective gives the conclusion 'X was likely to have been looked after by Y'."]

test_types_cases(47,[[n,greater_than],[2,1]],
[[[n,greater_than],[[t,number],[t,number]]]],
        [[[n,greater_than],[input,input]]],
[

        [[n,greater_than],[[v,n1],[v,n2]],":-",
        [       %[[n,trace2]],
        	[[n,>],[[v,n1],[v,n2]]]]]
]
,[[]]).

% ["Short Arguments","Rebreathsoning.txt",0,algorithms,"2. *I prepared to side with the pole.  I did this by observing meantness (sic).  First, I found the statement to be unwavering through time.  Second, I found it to be unwavering in relation to other statements.  Third, I found it to be unwavering in relation with other people."]

test_types_cases(48,[[n,pole],[["a",[1,2,3]],["b",[4,5,6]],3,[v,a_or_b]]],
[
[[n,pole],[[t,pole],[t,pole],[t,number],[t,string]]],
[[t,pole],[[t,string],{[t,number]}]]
],
        [[[n,pole],[input,input,input,output]]],
[

        [[n,pole],[[v,pole1],[v,pole2],[v,person],[v,pole_name]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,pole1],[[v,pole_name],[v,list1]]]],
        	[[n,member2],[[v,list1],[v,person]]]]],

        [[n,pole],[[v,pole1],[v,pole2],[v,person],[v,pole_name]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,pole2],[[v,pole_name],[v,list1]]]],
        	[[n,member2],[[v,list1],[v,person]]]]]

]
,[[[[v,a_or_b],"a"]]]).

% I did this by observing meantness (sic).

test_types_cases(49,[[n,meantness],[[["a","b"],["b","c"]],["a","b","c"]]],
[
[[n,meantness],[[t,lists_string],[t,list_string]]],
[[t,lists_string],[{[t,list_string]}]],
[[t,list_string],[{[t,string]}]]
],
        [[[n,meantness],[input,input]]],
[

        [[n,meantness],[[v,lists],[]]],
        [[n,meantness],[[v,lists],[v,list]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,member2],[[v,lists],[v,list1]]],
        	[[n,member2],[[v,list1],[v,head]]],
        	%[[n,delete],[[v,lists],[v,list1],[v,lists2]]],
        	[[n,meantness],[[v,lists],[v,tail]]
        	]]]
]
,[[]]).

% First, I found the statement to be unwavering through time.

test_types_cases(50,[[n,unwavering],[[1,1,1,1]]],
[
[[n,unwavering],[[t,list_number]]],
[[t,list_number],[{[t,number]}]],
[[n,1],[[t,number]]]
],

[[[n,unwavering],[input]],
[[n,1],[input]]],
[

        [[n,unwavering],[[]]],
        [[n,unwavering],[[v,list]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,1],[[v,head]]],
        	[[n,unwavering],[[v,tail]]]]],

        [[n,1],[1]]
]
,[[]]).

% Second, I found it to be unwavering in relation to other statements.

test_types_cases(51,[[n,unwavering_list],[[[1,1,1,1],[1,1,1,1],[1,1,1,1]]]],
[
[[n,unwavering_list],[{[t,list_number]}]],
[[n,unwavering],[[t,list_number]]],
[[t,list_number],[{[t,number]}]],
[[n,1],[[t,number]]]
],

[[[n,unwavering_list],[input]],
[[n,unwavering],[input]],
[[n,1],[input]]],
[

        [[n,unwavering_list],[[]]],
        [[n,unwavering_list],[[v,list]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,unwavering],[[v,head]]],
        	[[n,unwavering_list],[[v,tail]]]]],

        [[n,unwavering],[[]]],
        [[n,unwavering],[[v,list]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,1],[[v,head]]],
        	[[n,unwavering],[[v,tail]]]]],

        [[n,1],[1]]
]
,[[]]).


% Third, I found it to be unwavering in relation with other people.

test_types_cases(52,[[n,unwavering_people],%[[[[1],[1]],[[1],[1]]]]],
[[[[[1,1,1,1],[1,1,1,1],[1,1,1,1]],[[1,1,1,1],[1,1,1,1],[1,1,1,1]]],[[[1,1,1,1],[1,1,1,1],[1,1,1,1]],[[1,1,1,1],[1,1,1,1],[1,1,1,1]]]]]],
[
[[n,unwavering_people],[[t,unwavering_people1]]],
[[t,unwavering_people1],[[t,number]]],
[[t,unwavering_people1],[{[t,unwavering_people1]}]],
[[n,unwavering],[{[t,number]}]],
[[n,1],[[t,number]]]
],

[[[n,unwavering_people],[input]],
[[n,unwavering],[input]],
[[n,1],[input]]],
[

        [[n,unwavering_people],[[]]],
        
        [[n,unwavering_people],[[v,list]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,unwavering],[[v,head]]],
        	[[n,unwavering_people],[[v,tail]]]]],

        [[n,unwavering_people],[[v,list]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,not],[[[n,unwavering],[[v,head]]]]],
        	[[n,unwavering_people],[[v,head]]],
        	[[n,unwavering_people],[[v,tail]]]]],

        [[n,unwavering],[[]]],
        
        [[n,unwavering],[[v,list]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,1],[[v,head]]],
        	[[n,unwavering],[[v,tail]]]
        	%[[n,cut]]
        	]],

        [[n,1],[1]]
]
,[[]]).

% ["Fundamentals of Meditation and Meditation Indicators","FUNDAMENTALS OF MEDITATION by Lucian Green Heads of State 4 of 4.txt",0,algorithms,"39. *I prepared to enjoy subsidised accreditation.  I did this by agreeing with the government.  First, I read the government policy.  Second, I verified that it was a good idea.  Third, I agreed with it.  In this way, I prepared to enjoy subsidised accreditation by agreeing with the government."]

test_types_cases(53,[[n,subsidised_accreditation],[1,30]],
[[[n,subsidised_accreditation],[[t,number],[t,number]]]],
        [[[n,subsidised_accreditation],[input,input]]],
[

        [[n,subsidised_accreditation],[1,1]],
        [[n,subsidised_accreditation],[1,30]],
        [[n,subsidised_accreditation],[1,100]],
        [[n,subsidised_accreditation],[1,400]]
]
,[[]]).

% I did this by agreeing with the government.


test_types_cases(54,[[n,agree_with_government],[1,["a"],[v,end]]],
[
[[n,agree_with_government],[[t,number],{[t,string]},[[t,number],[t,string]]]]
],
        [[[n,agree_with_government],[input,input,output]]],
[

        [[n,agree_with_government],[[v,policy],[v,part],[v,end]],":-",
        [
                [[n,wrap],[[v,policy],[v,policy1]]],
                [[n,append],[[v,policy1],[v,part],[v,end]]]
        ]
        ]
]
,[[[[v,end],[1,"a"]]]]).

% First, I read the government policy.

test_types_cases(55,[[n,read_policy],[[n,+],1,1,[v,a]]],
[[[n,read_policy],[[t,predicatename],[t,number],[t,number],[t,number]]]],
        [[[n,read_policy],[input,input,input,output]]],
[

        [[n,read_policy],[[v,pred_name],[v,var1],[v,var2],[v,var3]],":-",
        [
                %[[n,trace2]],
                [[v,pred_name],[[v,var1],[v,var2],[v,var3]]]
        ]]
]
,[[[[v,a],2]]]).

% ["Short Arguments","Two_Types.txt",0,algorithms,"1. I prepared to sell the artificial breasts.  I did this by stating that the breast cooled and stored the milk.  First, I read the temperature of the breast.  Second, I read how much milk it stored.  Third, I simulated these."]

test_types_cases(56,[[n,amount_earned],[2,2,[v,a]]],
[[[n,amount_earned],[[t,number],[t,number],[t,number]]]],
        [[[n,amount_earned],[input,input,output]]],
[

        [[n,amount_earned],[[v,n1],[v,n2],[v,n3]],":-",
        [       %[[n,trace2]],
        	[[n,*],[[v,n1],[v,n2],[v,n3]]]]]
]
,[[[[v,a],4]]]).

% I did this by stating that the breast *cooled and stored the milk.

test_types_cases(57,[[n,cool],[[-4.1,-4,-4.2],-5,-4]],
[
[[n,cool],[{[t,number]},[t,number],[t,number]]]
],
        [[[n,cool],[input,input,input]]],
[

        [[n,cool],[[],[v,n2],[v,n3]]],
        [[n,cool],[[v,list],[v,n2],[v,n3]],":-",
        [       %[[n,trace2]],
        	[[n,equals4],[[v,list],[[v,head],"|",[v,tail]]]],
        	[[n,>=],[[v,head],[v,n2]]],
        	[[n,>=],[[v,n3],[v,head]]],
        	[[n,cool],[[v,tail],[v,n2],[v,n3]]]]]
]
,[[]]).

% I did this by stating that the breast cooled and *stored the milk.

test_types_cases(58,[[n,store],[["milk"],"milk"]],
[
[[n,store],[{[t,string]},[t,string]]]
],
        [[[n,store],[input,input]]],
[

        [[n,store],[[[v,a]],[v,a]]]
]
,[[]]).


% ["Fundamentals of Meditation and Meditation Indicators","FUNDAMENTALS OF MEDITATION by Lucian Green Appearances 4 of 4.txt",0,algorithms,"31.    I prepared to enter the room in the heartland.  I did this by writing the Room Essay Press Release.  First, I wrote that 250 breasonings expanded to 50 As.  Second, I wrote that a breasoned out pop song expanded to 50 As.  Third, I wrote the classical music composition contained 5 pop songs.  In this way, I prepared to enter the room in the heartland by writing the Room Essay Press Release."]

% * I prepared to enter the room in the heartland.

% Follows a path with directions along the paths

test_types_cases(59,[[n,tour_heartland],[[1,2]]],

        [
[[n,tour_heartland],[{[t,number]}]],
[[n,tour_heartland1],[{{[t,number]}},{[t,number]}]],
[[n,heartland],[{{[t,number]}}]]
],
        
        [[[n,tour_heartland],[input]],
        [[n,tour_heartland1],[input,input]],
        [[n,heartland],[output]]],
[

        [[n,tour_heartland],[[v,path]],":-",
        [
         [[n,heartland],[[v,heartland]]],
         [[n,tour_heartland1],[[v,heartland],[v,path]]]
        ]],

        [[n,tour_heartland1],[[v,heartland],[]]],
        [[n,tour_heartland1],[[v,heartland],[[v,single_step]]]],
        [[n,tour_heartland1],[[v,heartland],[v,path]],":-",
        [
         [[n,equals4],[[v,path],[[v,curr_step],[v,next_step],"|",[v,rest]]]],
         [[n,member2],[[v,heartland],[v,step]]],
         [[n,equals4],[[v,step],[[v,curr_step],"|",[v,links_to]]]],	
         [[n,member2],[[v,links_to],[v,next_step]]],
         [[n,equals4],[[v,next],[[v,next_step],"|",[v,rest]]]],
         [[n,tour_heartland1],[[v,heartland],[v,next]]]
        ]],

        [[n,heartland],[[[1, % node number
        2,3 % links to
        ],[2,3],[3]]]]
        
]
,[[]]).

% * I did this by writing the Room Essay Press Release.

test_types_cases(60,[[n,find_in_room],["newspaper",[v,x],[v,y]]],

        [
[[n,find_in_room],[[t,string],[t,number],[t,number]]],
[[n,room],[{{[t,number],[t,number],[t,string]}}]]
],
        
        [[[n,find_in_room],[input,output,output]],
        [[n,room],[output]]],

[
        [[n,find_in_room],[[v,string],[v,x],[v,y]],":-",
        [
         [[n,room],[[v,room]]],
         [[n,member2],[[v,room],[[v,x],[v,y],[v,string]]]]
        ]],

        [[n,room],[
        [
         [1,3,""],[2,3,"newspaper"],[3,3,""],
         [1,2,""],[2,2,""],[3,2,""],
         [1,1,"door"],[2,1,""],[3,1,""]
        ]
        ]]
     
]
,[[[[v,x],2],[[v,y],3]]]).

        
% * First, I wrote that 250 breasonings expanded to 50 As.

test_types_cases(61,[[n,return],[250,4000,[v,return]]],

        [[[n,return],[[t,number],[t,number],[t,number]]]],

        [[[n,return],[input,input,output]]],

[
        [[n,return],[[v,take],[v,give],[v,return]],":-",
        [
         [[n,/],[[v,give],[v,take],[v,return]]]
        ]]
]
,[[[[v,return],16]]]).

% ["Mind Reading","Mr Cryptography 3.txt",0,algorithms,"51. The robot was classed disabled (rather, superabled) in human terms, so was modified to be human-like when interpreting messages following decryption."]

test_types_cases(62,[[n,text2b_as_per_business_hour],[[v,brs]]],

        [[[n,text2b_as_per_business_hour],[[t,number]]]],

        [[[n,text2b_as_per_business_hour],[output]]],

[
        [[n,text2b_as_per_business_hour],[[v,brs]],":-",
        [
         [[n,*],[80,% breasonings per A
         100, % As per week allowed in Text to Breasonings
         [v,a1]]], % br per week
         
         [[n,/],[[v,a1],% br per week
         7, % days
         [v,a2]]], % br per day
         
         [[n,/],[[v,a2],% br per day
         8, % business hours per day
         [v,a3]]], % breasonings per hour
         
         [[n,ceiling],[[v,a3],[v,brs]]]
        ]]
]
,[[[[v,brs],143]]]).

% ["Short Arguments","God Algorithm.txt",0,algorithms,"5. I prepared to ask why why was.  I did this by stating that I knew the result of the God algorithm was why.  First, I noticed the occurrence of the event.  Second, I read the result of the God algorithm.  Third, I worked out that the result explained the occurrence."]
        
% * I did this by stating that I knew the result of the God algorithm was why.

test_types_cases(63,[[n,each_topic],[[v,return]]],

        [[[n,each_topic],[[t,number]]]],

        [[[n,each_topic],[output]]],

[
        [[n,each_topic],[[v,return]],":-",
        [
         [[n,*],[4,4000,[v,return]]]
        ]]
]
,[[[[v,return],16000]]]).

% ["Medicine","MEDICINE by Lucian Green Head of State Head Ache Prevention 4 of 4.txt",0,algorithms,"32. I prepared to observe the people like my lecturer friend and meditation student was a doctor and friend.  I did this by observing the hansard.  First, I found the hansard.  Second, I observed him listen to the politician.  Third, I observed him take notes.  In this way, I prepared to observe the people like my lecturer friend and meditation student was a doctor and friend by observing the hansard."]

% * I prepared to observe the people like my lecturer friend and meditation student was a doctor and friend.

test_types_cases(64,[[n,characters],["Tom",[v,characters]]],

        [
[[n,characters],[[t,string],{[t,string]}]],
[[n,c],[[t,string],[t,string]]]
],

        [[[n,characters],[input,output]],
        [[n,c],[input,output]]],

[
        [[n,characters],[[v,person],[v,characters]],":-",
        [
         [[n,findall],[[v,c],[[[n,c],[[v,person],[v,c]]]],[v,characters]]]
        ]],
        
        [[n,c],["Tom","Chef"]],
        [[n,c],["Tom","Baker"]],
        [[n,c],["Tom","Writer"]],
        [[n,c],["John","Writer"]]
        
]
,[[[[v,characters],["Chef","Baker","Writer"]]]]).


test_types_cases(65,
[[n,characters],[["a",[["a"],"a"]]]],
%[[n,characters],[[["a"],"a"]]],
        [
[[n,characters],[[[t,string],[[[t,string]],[t,string]]]]]
],
        %[[[n,characters],[[[t,brackets],[[[t,brackets],[[t,string]]],[t,string]]]]]],

        [[[n,characters],[input]]],

[
        [[n,characters],[[v,person]]]
 
]
,[[]]).

test_types_cases(66,
[[n,characters],[["a",[["a"],"a"]]]],
%[[n,characters],[[["a"],"a"]]],
        [
[[n,characters],[{[t,string],{{[t,string]},[t,string]}}]]
],
        %[[[n,characters],[[[t,brackets],[[[t,brackets],[[t,string]]],[t,string]]]]]],

        [[[n,characters],[input]]],

[
        [[n,characters],[[v,person]]]
 
]
,[[]]).


test_types_cases(67,
[[n,characters],[[["a",["a"]],"a"]]],
%[[n,characters],[[["a",["a"]]]]],
        [
[[n,characters],[[[[t,string],[[t,string]]],[t,string]]]]
],
                %[[[n,characters],[[[t,brackets],[[[t,brackets],[[t,string],[[t,brackets],[[t,string]]]]]]]]]],

        [[[n,characters],[input]]],

[
        [[n,characters],[[v,person]]]
 
]
,[[]]).


test_types_cases(68,
[[n,characters],[[["a",["a"]],"a"]]],
%[[n,characters],[[["a",["a"]]]]],
        [
[[n,characters],[{{[t,string],{[t,string]}},[t,string]}]]
],
                %[[[n,characters],[[[t,brackets],[[[t,brackets],[[t,string],[[t,brackets],[[t,string]]]]]]]]]],

        [[[n,characters],[input]]],

[
        [[n,characters],[[v,person]]]
 
]
,[[]]).

test_types_cases(69,
[[n,characters],[[a]]],
%[[n,characters],[[["a"],"a"]]],
        [
[[n,characters],[{[t,atom]}]]
],
        %[[[n,characters],[[[t,brackets],[[[t,brackets],[[t,string]]],[t,string]]]]]],

        [[[n,characters],[input]]],

[
        [[n,characters],[[v,person]]]
 
]
,[[]]).
