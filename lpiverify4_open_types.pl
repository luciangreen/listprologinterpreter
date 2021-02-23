%% test_open_types(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test_open_types(Debug,NTotal,Score) :- test_open_types(Debug,0,NTotal,0,Score),!.
test_open_types(_Debug,NTotal,NTotal,Score,Score) :- NTotal=7, !.
test_open_types(Debug,NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	test_open_types_cases(NTotal3,Query,Types,Modes,Functions),
	((international_interpret([lang,"en"],Debug,Query,Types,Modes,Functions,Result),not(Result=[]))->(Score3 is Score1+1,writeln([test_open_types,NTotal3,result,Result]),writeln([test_open_types,NTotal3,passed]));(Score3=Score1,writeln([test_open_types,NTotal3,failed]))),
	writeln(""),
	test_open_types(Debug,NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

test_open_types1(Debug,N,Passed) :-
	test_open_types_cases(N,Query,Types,Modes,Functions),
	(((international_interpret([lang,"en"],Debug,Query,Types,Modes,Functions,Result),not(Result=[]))%%writeln(Result1),
	%%Result=Result1
	)->(Passed=passed,writeln([test_open_types,N,result,Result]),writeln([test_open_types,N,passed]));(Passed=failed,writeln([test_open_types,N,failed]))),!.


test_open_types_cases(1,[[n,true_vs_good],[[v,t],[v,g]]],

[[[n,true_vs_good],
[
[[t,brackets],
	[
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]]
	]
]
,
[[t,brackets],
	[
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]],
	[[t,brackets],[[t,number],[t,number]]]
	]
]]
],
[[n,random1],[[t,number],[t,number],[t,number]]]]

,[[[n,true_vs_good],[output,output]],
[[n,random1],[input,input,output]]],
[
        [[n,true_vs_good],[[v,t],[v,g]],":-",
        [
                [[n,random1],[0.1,4.6,[v,y1]]],
                [[n,random1],[[v,y1],4.7,[v,y2]]],
                [[n,random1],[[v,y2],4.8,[v,y3]]],
                [[n,random1],[[v,y3],4.9,[v,y4]]],
                
                [[n,equals2],[[v,c11],[0,0]]],
                [[n,wrap],[[v,c11],[v,c12]]],

                [[n,equals2],[[v,c21],[1,[v,y1]]]],
                [[n,wrap],[[v,c21],[v,c22]]],
                [[n,append],[[v,c12],[v,c22],[v,c23]]],
                
                [[n,equals2],[[v,c31],[2,[v,y2]]]],
                [[n,wrap],[[v,c31],[v,c32]]],
                [[n,append],[[v,c23],[v,c32],[v,c33]]],
                
                [[n,equals2],[[v,c41],[3,[v,y3]]]],
                [[n,wrap],[[v,c41],[v,c42]]],
                [[n,append],[[v,c33],[v,c42],[v,c43]]],
                
                [[n,equals2],[[v,c51],[4,[v,y4]]]],
                [[n,wrap],[[v,c51],[v,c52]]],
                [[n,append],[[v,c43],[v,c52],[v,c53]]],
                
                [[n,equals2],[[v,c61],[5,5]]],
                [[n,wrap],[[v,c61],[v,c62]]],
                [[n,append],[[v,c53],[v,c62],[v,g]]],
                
                [[n,equals3],[[v,t],[[0,0],[1,1],
                [2,2],[3,3],[4,4],[5,5]]]
        ]]],
        
        [[n,random1],[[v,a1],[v,a2],[v,n5]],":-",
        [
                [[n,-],[[v,a2],[v,a1],[v,a3]]],
                [[n,random],[[v,n1]]],
                [[n,*],[[v,a3],[v,n1],[v,n2]]],
                [[n,+],[[v,n2],[v,a1],[v,n21]]],
                [[n,*],[10,[v,n21],[v,n3]]],
                [[n,round],[[v,n3],[v,n4]]],
                [[n,/],[[v,n4],10,[v,n5]]]
        ]]
]).

test_open_types_cases(2,[[n,true_vs_good],[[[n,a],[1]],1,[v,g2]]],

        [[[n,true_vs_good],[[[t,brackets],[[t,predicatename],
        [[t,brackets],[[t,number]]]]],
        [t,number],[t,number]]]],
        
[[[n,true_vs_good],[input,input,output]]],



[
        [[n,true_vs_good],[[v,f1],[v,l],[v,n]],":-",
        [        
                [[n,equals3],[[v,n],1]
                
        ]]
]]).

test_open_types_cases(3,[[n,function],[[v,a]]],
[[[n,function],[[[t,brackets],[[t,number]]]]]],
[[[n,function],[output]]],
[
        [[n,function],[[1]]]
]
).


% ["Computational English","COMPUTATIONAL ENGLISH by Lucian Green Drawing connections 3 of 4.txt",0,algorithms,"23.   *I prepared to cultivate people.  I did this by writing 16 250 breasoning areas of study influenced by Plato’s forms about Popology.  First, I equated Plato’s forms with Lucianic Popology, by equating people with objects.  Second, I equated the names of Plato’s forms with an agreed with argument, by writing simulations of people are in people’s minds.  Third, I equated the functions of Plato’s forms with a positive argument, by writing people are stronger than objects.  In this way, I prepared to cultivate people by writing 16 250 breasoning areas of study influenced by Plato’s forms about Popology."]

% read do you create the person, do you switch them on to existing for the rest of their life?


test_open_types_cases(4,[[n,cultivate_person],[[v,a],[v,b]]],
        [[[n,cultivate_person],[[t,string],[t,string]]]],
        [[[n,cultivate_person],[output,output]]],

[
        [[n,cultivate_person],[[v,a],[v,b]],":-",
        [        
        %% do you create the person

                [[n,writeln],["Do you create the person?"]],
                [[n,read_string],[[v,a]]],
                
        %% do you switch them on to existing for the rest of their life?
                [[n,writeln],["Do you switch them on to existing for the rest of their life?"]],

                [[n,read_string],[[v,b]]]

                
        ]]
]).

% ["Fundamentals of Meditation and Meditation Indicators","FUNDAMENTALS OF MEDITATION by Lucian Green Blue Nature 4 of 4.txt",0,algorithms,"31.    I prepared to give the song I am not a Peach and medicine degree away.  I did this by giving the woman the anti-depression song and anti-depression degree.  First, I gave the woman the anti-depression song.  Second, I gave the woman the anti-depression degree.  Third, I observed her as happy.  In this way, I prepared to give the song I am not a Peach and medicine degree away by giving the woman the anti-depression song and anti-depression degree."]

% Prevents rumination

test_open_types_cases(5,[[n,prevent_rumination],[[]]],
        [[[n,prevent_rumination],[[[t,list],[[t,string]]]]]],
        [[[n,prevent_rumination],[input]]],

[
        [[n,prevent_rumination],[[v,list]],":-",
        [        

                [[n,writeln],["What would you like to think about?"]],
                [[n,read_string],[[v,item]]],

                [[n,"->"],[[[n,member],[[v,list],[v,item]]],
                
                [[[n,writeln],["Please do not think of"]],
                [[n,writeln],[[v,item]]],
                [[n,writeln],["again."]],
                [[n,=],[[v,list],[v,list2]]]],
                
                [[[n,wrap],[[v,item],[v,item2]]],
                [[n,append],[[v,list],[v,item2],[v,list2]]]]]],

                [[n,writeln],["Have you finished thinking (y/n)?"]],
                [[n,read_string],[[v,y_n]]],

                [[n,"->"],[[[n,=],[[v,y_n],"y"]],

                [[n,true]],
                
                [[n,prevent_rumination],[[v,list2]]]]]
        ]]
]).

% ["Fundamentals of Meditation and Meditation Indicators","FUNDAMENTALS OF MEDITATION by Lucian Green Yellow God 2 of 4.txt",0,algorithms,"12. I prepared to put through an A with a “negatable pressure cup appearance”, in other words negatably but in a way that is protected by meditation, placing a medical question on oneself (thinking of a dental drill, the medical question and a conclusion) for a child to be conceived, a job to be earned or an H1 to be supported. I did this by holding the dog model, like the pressure cup. First, I picked up the dog model. Second, I held it. Third, I placed it on the ground. In this way, I prepared to put through an A with a “negatable pressure cup appearance” by holding the dog model, like the pressure cup."]

% Text to breasoning checklist

test_open_types_cases(6,[[n,t2b_checklist],[[v,a],[v,b],[v,c]]],
        [[[n,t2b_checklist],[[t,string],[t,string],[t,string]]]],
        [[[n,t2b_checklist],[output,output,output]]],

[
        [[n,t2b_checklist],[[v,a],[v,b],[v,c]],":-",
        [        
                [[n,writeln],["Do you study education?"]],
                [[n,read_string],[[v,a]]],

                [[n,writeln],["Do you study medicine?"]],
                [[n,read_string],[[v,b]]],

                [[n,writeln],["Do you study meditation?"]],
                [[n,read_string],[[v,c]]]
        ]]
]).

% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green Part of Room 1 of 4.txt",0,algorithms,"6. The disabilities teacher student prepared to assess a “done-up” assignment (with a short story containing 64 departmental perspectives about it) and a “seen-as” version of “A” quality written by the student. He did this by placing the bird model in the hole. First, he lifted the bird model up. Second, he walked to the hole. Third, he placed it in the hole. In this way, the disabilities teacher student prepared to assess a “done-up” assignment and a “seen-as” version of “A” quality written by the student by placing the bird model in the hole."]

test_open_types_cases(7,[[n,episode_character],[[v,a]]],
        [[[n,episode_character],[[t,loop0]]],
        [[t,loop0],[[[t,brackets],[[t,loop1],[t,loop1],[t,loop1],[t,loop1]]]]],
        [[t,loop1],[[[t,brackets],[[t,loop2],[t,loop2],[t,loop2],[t,loop2]]]]],
        [[t,loop2],[[[t,brackets],[[t,items],[t,items],[t,items],[t,items]]]]],
        
        [[t,items],[[t,number],[t,number],[t,number],[t,string]]]],
        [[[n,episode_character],[output]]],

[
        [[n,episode_character],[[v,ds3]],":-",
        [        
                [[n,findall],[[v,ds2],
         [
         [[n,member2],[[1,2,3,4],[v,l1]]],
         				[[n,findall],[[v,ds1],
         [
         [[n,member2],[[1,2,3,4],[v,l2]]],
         					[[n,findall],[[[v,l1],[v,l2],[v,l3],[v,d]],
         [
         [[n,member2],[[1,2,3,4],[v,l3]]],
         [[n,equals4],[[v,line],["Level",[v,l1],[v,l2],[v,l3],
         "Please write a detail."]]],
         [[n,writeln],[[v,line]]],
         [[n,read_string],[[v,d]]]],

        [v,ds1]]]],
        [v,ds2]]]]
        ,
        [v,ds3]]]
    
        ]]
]).
