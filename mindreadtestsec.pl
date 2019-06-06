%% mind read test

%% Make files different for different tests

/**
1. Breason out 250 br A - as receiver/idea	
2. Time this for A for the topic breasoned out and not breasoned out at random intervals for an interval of time (not to do with the random intervals), person to me
3. Does it take considerably longer for detected As? - calculate median of mostly hopefully false trials and differences from it x labelled graph first - do 10 trials over 10 minutes with 3 hopefully successful attempts that are labelled

[dateandtime,date(2019,4,27,10,28,25.09528613090515,0,UTC,false)]
[[*5,lorelle],[5,dam],[5,adrian],[4,water],[4,seed],[2,redblackduck]]
lorelle but after start

[dateandtime,date(2019,4,27,10,28,50.22005605697632,0,UTC,false)]
[[8,seed],[6,redblackduck],[*4,dam],[3,water],[3,adrian],[1,lorelle]]
dam

[dateandtime,date(2019,4,27,10,29,13.600827693939209,0,UTC,false)]
[[*7,redblackduck],[7,lorelle],[6,dam],[5,seed],[3,water],[3,adrian]]
red black duck after start

[dateandtime,date(2019,4,27,10,29,37.128156661987305,0,UTC,false)]
[[6,seed],[6,dam],[5,water],[*5,redblackduck],[4,adrian],[3,lorelle]]
red black duck

[dateandtime,date(2019,4,27,10,30,1.5190646648406982,0,UTC,false)]
[[9,adrian],[*8,redblackduck],[7,dam],[6,seed],[5,water],[4,lorelle]]
red black duck

Is it detecting what I am really thinking (or someone else) are the thoughts intertwined - all x

I seem to have really thought about 1. the main character 2. the reason 3. the next character 4. what was later 5. who would put them away

Is the seen as version able to be used?  Is it a stream of consciousness detector?

***250s for dotting yourself on speaking and dotting yourself receiving, and yourself speaking and yourself receiving (each day?)


[dateandtime,date(2019,4,27,11,5,29.561346292495728,0,UTC,false)]
[[7,seed],[7,adrian],[*6,water],[6,lorelle],[6,dam],[4,redblackduck]]
water
[dateandtime,date(2019,4,27,11,5,53.5652551651001,0,UTC,false)]
[[7,seed],[*6,water],[6,dam],[5,lorelle],[3,redblackduck],[2,adrian]]
water

[dateandtime,date(2019,4,27,11,6,17.240721464157104,0,UTC,false)]
[[*6,dam],[5,redblackduck],[5,adrian],[4,water],[4,seed],[3,lorelle]]
dam

[dateandtime,date(2019,4,27,11,6,41.15016746520996,0,UTC,false)]
[[7,redblackduck],[7,lorelle],[*7,dam],[6,adrian],[3,water],[3,seed]]
dam

*** think to binary opposition x objects to either side - just do 250s for l,r, then turn off with a 250

*** put back positive protective thoughts on top, just do 250, then turn off with 250

working ok

**/

%%use_module(library(pio)).

:- use_module(library(date)).
:- include('texttobr2qb').
:- include('mindreadtestshared').

sectest(Person):-
	daysbspeoplearmy, %% dot me on
	daysbspeoplearmy, %% dot them on
	find_time(H,M,S),
	daysbspeoplearmy, %% dot question on
	threats(0,Threats),
	daysbspeoplearmy, %% dot answer on
	writeln([Person,H,M,S,Threats,threats]).
	
threats(Threats1,Threats2):-
	%% "Given that they are not likely to have meant it and that there is nothing wrong, is there anything else that is wrong?"
	trialy2_6("Yes",R1),
	trialy2_6("No",R2),
		R=[R1,R2/**,R3,R4,R5,R6,R7,R8,R9,R10**,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20,R21,R22,R23,R24,R25,R26,R27**/
		],
	sort(R,RA),
	reverse(RA,RB),
	RB=[[_,Answer]|_Rest],
	
	(Answer="No"->Threats2=Threats1;(Threats3 is Threats1+1,threats(Threats3,Threats2))).

trialy1(R1) :-
	trial0(A1), %% Control
	trial0(A2), %% Test 1
	(A1>A2->R1=true;R1=fail).

trial0(Av) :- N is 10, trial1(N,0,S),
	Av is S/N.

trial1(0,A,A) :- !.
trial1(N,A,B) :- mindreadtest(S), A1 is A+S,
	N1 is N-1,trial1(N1,A1,B).
	
mindreadtest(Sec) :-
	%% 250 br for characters to be br out with 10 br each from person to me - do when initial 250 br test done and doing 10 br test
	%%comment(fiftyastest),
	%%random(X),X1 is 10*X, X2 is floor(X1), (X2=<2 -> (
	%%texttobr,writeln(['true test']), %%); %% use breasonings breasoned out by computer for not by me, for job medicine for "me", at last time point
	%%true), %% leave last time point blank
	(texttobr2(2)),%% make an A to detect reaction to gracious giving or blame of in following
	get_time(TimeStamp1),
	%%phrase_from_file(string(_String), 'file.txt'),
	(daysbspeoplearmy(2)), %% test breasonings breasoned out by computer for not by me, for job medicine for "me", at last time point
	%% is gracious giving or blame
	get_time(TimeStamp2),
	%%comment(turnoffas),
   Sec is TimeStamp2 - TimeStamp1.

/**string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

comment(fiftyastest).
comment(turnoffas).
**/