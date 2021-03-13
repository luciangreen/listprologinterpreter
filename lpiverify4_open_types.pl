%% test_open_types(Debug[on/off],Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

test_open_types(Debug,NTotal,Score) :- test_open_types(Debug,0,NTotal,0,Score),!.
test_open_types(_Debug,NTotal,NTotal,Score,Score) :- NTotal=23, !.
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

% I want to swim if I am hot

test_open_types_cases(8,[[n,swim],[[v,c]]],
        [[[n,swim],[[t,string]]]],
        [[[n,swim],[output]]],

[
        [[n,swim],[[v,c]],":-",
        [        
                [[n,writeln],["Are you hot (y/n)?"]],
                [[n,read_string],[[v,a]]],

                [[n,"->"],[[[n,=],[[v,a],"y"]],
                [[[n,writeln],["Would you like to go for a swim?"]],
                [[n,read_string],[[v,b]]],
                [[n,"->"],[[[n,=],[[v,b],"y"]],
                [[n,=],[[v,c],"swim"]],
                [[n,=],[[v,c],"no swim"]]]]],
                
                [[n,=],[[v,c],"no swim"]]]]                
        ]]
]).


test_open_types_cases(9,[[n,recognise_above_waist],[[v,c]]],
        [[[n,recognise_above_waist],[[t,string]]],
        [[n,person],[[t,string],[t,string],[t,string],[t,string]]]],
        [[[n,recognise_above_waist],[output]],
        [[n,person],[input,input,input,output]]],

[
        [[n,recognise_above_waist],[[v,d1]],":-",
        [        
                [[n,writeln],["Does the person have a male chest (y/n)?"]],
                [[n,read_string],[[v,a]]],
                [[n,"->"],[[[n,=],[[v,a],"y"]],
                [[n,=],[[v,a1],"male chest"]],
                [[n,=],[[v,a1],"female chest"]]]],                

                [[n,writeln],["Does the person have a male face (y/n)?"]],
                [[n,read_string],[[v,b]]],
                [[n,"->"],[[[n,=],[[v,b],"y"]],
                [[n,=],[[v,b1],"male face"]],
                [[n,=],[[v,b1],"female face"]]]],                

                [[n,writeln],["What colour hair does the person have (brown/blonde/auburn)?"]],
                [[n,read_string],[[v,c]]],
                [[n,"->"],[[[n,=],[[v,c],"brown"]],
                [[n,=],[[v,c1],"brown hair"]],
                [[n,"->"],[[[n,=],[[v,c],"blonde"]],
                [[n,=],[[v,c1],"blonde hair"]],
                [[n,=],[[v,c1],"auburn hair"]]]]]],
                
                [[n,"->"],[[[n,person],[[v,a1],[v,b1],[v,c1],[v,d1]]],
                [[n,true]],
                [[n,=],[[v,d1],"Unknown"]]]]
        ]],
        
        [[n,person],["male chest","male face","brown hair","Harry"]],
        [[n,person],["female chest","female face","blonde hair","Susan"]],
        [[n,person],["female chest","female face","auburn hair","April"]]
]).


test_open_types_cases(10,[[n,want_me],[[v,c]]],
        [[[n,want_me],[[t,string]]]],
        [[[n,want_me],[output]]],

[
        [[n,want_me],[[v,a1]],":-",
        [        
                [[n,writeln],["Do you want me for 1-food, 2-activity, 3-toy or 4-not want me?"]],
                [[n,read_string],[[v,a]]],
                [[n,"->"],[[[n,=],[[v,a],"4"]],
                [[n,=],[[v,a1],"no"]],
                [[n,=],[[v,a1],"yes"]]]]         
        ]]

]).

% ["Fundamentals of Pedagogy and Pedagogy Indicators","PEDAGOGY INDICATORS by Lucian Green Aigs for Pedagogy Helper 2 of 3.txt",0,algorithms,"24.   *The pedagogy helper was in natural law.  The pedagogy aigs helper was legally right.  The self examined the water line at different times.  The self was above it at those times.  The self offered the Aigs service."]

% Check whether the algorithm/argument is classified under natural law.

test_open_types_cases(11,[[n,natural_law],[[v,a1]]],
        [[[n,natural_law],[[t,string]]]],
        [[[n,natural_law],[output]]],

[
        [[n,natural_law],[[v,a1]],":-",
        [        
                [[n,writeln],["Is the idea from pedagogy, meditation or medicine (y/n)?"]],
                [[n,read_string],[[v,a]]],
                [[n,"->"],[[[n,=],[[v,a],"y"]],
                [[n,=],[[v,a1],"true"]],
                [[[n,writeln],["Does the idea not contain genetic modification of an organism, not include nuclear energy, and is compatible with natural-law (y/n)?"]],
                [[n,read_string],[[v,b]]],
                [[n,"->"],[[[n,=],[[v,b],"y"]],
                [[n,=],[[v,a1],"true"]],
                [[n,=],[[v,a1],"false"]]]]]]]                
        ]]
]).

% ["Medicine","MEDICINE by Lucian Green Doctor Sutra 1 of 4.txt",0,algorithms,"3.    *The Vag prepared to keep on his own line. He did this by checking his indicator of his health. First, he stepped onto the line. Second, he walked along it. Third, he left the line. In this way, the Vag prepared to keep on his own line by checking his indicator of his health."]

% Use headache medicine if you have a stress, not pathological headache.

test_open_types_cases(12,[[n,headache_medicine],[[v,a1]]],
        [[[n,headache_medicine],[[t,string]]]],
        [[[n,headache_medicine],[output]]],

[
        [[n,headache_medicine],[[v,a1]],":-",
        [        
                [[n,writeln],["Do you have a stress, not pathological headache (y/n)?"]],
                [[n,read_string],[[v,a]]],
                [[n,"->"],[[[n,=],[[v,a],"n"]],
                [[n,=],[[v,a1],"true"]],
                [[n,=],[[v,a1],"false"]]]]                
        ]]
]).

% ["Short Arguments","Competition.txt",0,algorithms,"7. *I liked breasonings and equality - and economic freedom.  I performed better by using the daily regimen to go to church (play the note).  Confidence blocks and blocks from lack of practice were cleared.  I maintained a high level of performance.  I functioned (played the note) positively.  "]

% Would you like the same as someone else?

test_open_types_cases(13,[[n,same],[[v,a1]]],
        [[[n,same],[[t,string]]]],
        [[[n,same],[output]]],

[
        [[n,same],[[v,a1]],":-",
        [        
                [[n,writeln],["Would you like the same as someone else?  What is it?"]],
                [[n,read_string],[[v,a1]]]
        ]]
]).

% ["Mind Reading","Mr other times 8.txt",0,algorithms,"[""Green, L 2021, <i>Mr other times 8</i>, Lucian Academy Press, Melbourne."",""Green, L 2021"",1,""*Mr other times 8"]

% What would you like to remember?

test_open_types_cases(14,[[n,remember],[[v,a1]]],
        [[[n,remember],[[t,string]]]],
        [[[n,remember],[output]]],

[
        [[n,remember],[[v,a1]],":-",
        [        
                [[n,writeln],["What would you like to remember?"]],
                [[n,read_string],[[v,a1]]],
                [[n,writeln],["Press <return> to display the memory?"]],
                [[n,read_string],[[v,a2]]],
                [[n,writeln],[[v,a1]]]
        ]]
]).

% ["Medicine","MEDICINE by Lucian Green Panic attack prevented by deep breathing and sutra 1 of 4.txt",0,algorithms,"1a. *I prepared to identify and prevent class distinctions. I did this by writing the Box song argument. First, I wrote about the box. Second, I wrote about the specific. Third, I wrote about the general. In this way, I prepared to identify and prevent class distinctions by writing the Box song argument."]

% Is the simulated intelligence a life form?

test_open_types_cases(15,[[n,life],[[v,a1]]],
        [[[n,life],[[t,string]]]],
        [[[n,life],[output]]],

[
        [[n,life],[[v,a3]],":-",
        [        
                [[n,writeln],["Does the entity feel (y/n)?"]],
                [[n,read_string],[[v,a1]]],
                [[n,writeln],["Does the entity have human thoughts (y/n)?"]],
                [[n,read_string],[[v,a2]]],
                [[n,"->"],[[[[n,=],[[v,a1],"y"]],[[n,=],[[v,a2],"y"]]],
                [[n,=],[[v,a3],"true"]],
                [[n,=],[[v,a3],"false"]]]]

        ]]
]).

% ["Medicine","MEDICINE by Lucian Green Quantum Box and Prayer 3 of 4.txt",0,algorithms,"27.   *I prepared to make sure that my day in the rooms was fine.  I did this by enjoying dialogue with the quantum box/prayer character.  First, I mentioned the first visible level of matter in the object to the character.  Second, I listened to the character negate that the level was problematic (say that it was fine).  Third, I repeated this for all the visible levels of matter in the object.  In this way, I prepared to make sure that my day in the rooms was fine by enjoying dialogue with the quantum box/prayer character."]

test_open_types_cases(16,[[n,fifty_algorithms],[[v,a1]]],
        [[[n,fifty_algorithms],[[t,string]]]],
        [[[n,fifty_algorithms],[output]]],

[
        [[n,fifty_algorithms],[[v,a3]],":-",
        [        
                [[n,writeln],["What will you use the fifty algorithms for (e.g. politician, professor, actor or musician)?"]],
                [[n,read_string],[[v,a3]]]

        ]]
]).

% ["Short Arguments","Professor Algorithm - Student.txt",0,algorithms,"6. *I prepared to smile.  I did this by symbolising the verb.  First, I enjoyed the song.  Second, I rummaged in the Christmas sack.  Third, I pulled the theatrical mask out of the sack."]

test_open_types_cases(17,[[n,prepare_to_smile],[[v,a1]]],
        [[[n,prepare_to_smile],[[t,string]]]],
        [[[n,prepare_to_smile],[output]]],

[
        [[n,prepare_to_smile],[[v,a3]],":-",
        [        
                [[n,writeln],["What do you find touching, lovely or inspiring about the person?"]],
                [[n,read_string],[[v,a3]]]

        ]]
]).

% ["Time Travel","Meditate to Time Travel 4.txt",0,algorithms,"49. *I meditated to avoid insider trading by time travelling."]

test_open_types_cases(18,[[n,detect_insider_trading],[[v,a1]]],
        [[[n,detect_insider_trading],[[t,string]]]],
        [[[n,detect_insider_trading],[output]]],

[
        [[n,detect_insider_trading],[[v,a3]],":-",
        [        
                [[n,writeln],["After learning insider information, did you trade?"]],
                [[n,read_string],[[v,a3]]]

        ]]
]).

% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green X Dimension 2 of 4.txt",0,algorithms,"17.                 *I prepared to confirm the ability to breason in meditation.  I did this by unblocking not wanting to write breasonings in meditation.  First, I studied Nietzsche in Arts.  Second, I studied Creative Writing.  Third, I studied Education.  In this way, I prepared to confirm the ability to breason in meditation by unblocking not wanting to write breasonings in meditation."]

test_open_types_cases(19,[[n,breason],[[v,a1]]],
        [[[n,breason],[[t,string]]]],
        [[[n,breason],[output]]],

[
        [[n,breason],[[v,a3]],":-",
        [        
                [[n,writeln],["If I told you a single way of satisfying the spiritual requirements to have a child, earn a high distinction and prevent quantum ailments such as headaches (where it would not be possible to do these things in other ways) would you be interested?"]],
                [[n,read_string],[[v,a3]]]

        ]]
]).

% ["Short Arguments","Simulated Intelligence 2.txt",0,algorithms,"14. *The man added to the simulation.  God (the man) took care of people in subsets of the simulation.  God found the subset of the simulation.  It was the place.  The man recorded them."]

test_open_types_cases(20,[[n,add_to_simulation],[[v,a1]]],
        [[[n,add_to_simulation],[[t,string]]]],
        [[[n,add_to_simulation],[output]]],

[
        [[n,add_to_simulation],[[v,a3]],":-",
        [        
                [[n,writeln],["Do you add text to breasonings to the simulation?"]],
                [[n,read_string],[[v,a3]]]

        ]]
]).

% ["Fundamentals of Pedagogy and Pedagogy Indicators","FUNDAMENTALS OF PEDAGOGY by Lucian Green X Dimension 3 of 4.txt",0,algorithms,"29.                 *I prepared to help Earth avoid catastrophe.  I did this by stating that I am peaceful.  First, I made vegan food available.  Second, I guided the number of children per family.  Third, I recommended green transport.  In this way, I prepared to help Earth avoid catastrophe by stating that I am peaceful."]

% How will you make sure that the food tastes delicious?

/**

?- test_open_types1(off,21,R).
How will you make sure that the food tastes delicious?
|: I will use softer, fresh ingredients and use the best type of cold pressed extra virgin olive oil when cooking.
[test_open_types,21,result,[[[[v,a1],I will use softer, fresh ingredients and use the best type of cold pressed extra virgin olive oil when cooking.]]]]
[test_open_types,21,passed]
R = passed.

**/

test_open_types_cases(21,[[n,delicious],[[v,a1]]],
        [[[n,delicious],[[t,string]]]],
        [[[n,delicious],[output]]],

[
        [[n,delicious],[[v,a3]],":-",
        [        
                [[n,writeln],["How will you make sure that the food tastes delicious?"]],
                [[n,read_string],[[v,a3]]]

        ]]
]).

% ["Creating and Helping Pedagogues","CREATE AND HELP PEDAGOGUES by Lucian Green Areas of Study to Create a Pedagogue 1 of 1.txt",0,algorithms,"1. *A peer should create a Pedagogue by writing 30 areas of study with 5 As, per student before they have the professor algorithm breasoned out for him or her. Have spiritual questions and answers set up to expand these breasonings, e.g. use the ways of thinking like breasonings, etc."]

% Department algorithm filer

/**
?- test_open_types1(off,22,R).
What department is the algorithm from?
|: Popology
What subject is the algorithm from?
|: Interpreter
What is a short description of the algorithm (2-3 words)?
|: Verification of input, and output using a verify script.
**/

test_open_types_cases(22,[[n,department_filer],[[v,a1],[v,a2],[v,a3]]],
        [[[n,department_filer],[[t,string],[t,string],[t,string]]]],
        [[[n,department_filer],[output,output,output]]],

[
        [[n,department_filer],[[v,a1],[v,a2],[v,a3]],":-",
        [        
                [[n,writeln],["What department is the algorithm from?"]],
                [[n,read_string],[[v,a1]]],
                [[n,writeln],["What subject is the algorithm from?"]],
                [[n,read_string],[[v,a2]]],
                [[n,writeln],["What is a short description of the algorithm (2-3 words)?"]],
                [[n,read_string],[[v,a3]]]
        ]]
]).

% ["Short Arguments","Green_Sutra.txt",0,algorithms,"8. *I prepared to notice the Lucian Effect.  I did this by teaching others.  First, I taught the person.  Second, they taught someone else.  Third, I noticed the positive effects."]

test_open_types_cases(23,[[n,lucian_effect],[[v,a1]]],
        [[[n,lucian_effect],[[t,string]]]],
        [[[n,lucian_effect],[output]]],

[
        [[n,lucian_effect],[[v,a1]],":-",
        [        
                [[n,writeln],["Would you like to silently repeat the Lucian mantra for twenty minutes twice per day, letting your thoughts become lighter and forgetting your stress?"]],
                [[n,read_string],[[v,a1]]]
        ]]
]).

