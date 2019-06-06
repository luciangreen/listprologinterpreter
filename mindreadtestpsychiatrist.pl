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

sectest(Person1):-
	
Person1=[Item1a,Item2a,_,_,_,_,_,_,Month2,Day2,_],
	Person2=[Item1a,Item2a],
get_time(TS),stamp_date_time(TS,date(_Year,Month1,Day1,_Hour1,_Minute1,_Seconda,_A,_TZ,_False),local),

	((Month2 is mod(Month1,2)) ->
	((Day1 is Day2)->do_c(Month1,Day1,Person2);true);true).

do_c(Month1,Day1,Person):-
	daysbspeoplearmy, %% dot me on
	daysbspeoplearmy, %% dot them on
	find_time(Hour,Minutes,Seconds),
	%% "Do you see (hallucinatory) appearances?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",R1),
	trialy2_6("No",R2),
	R=[R1,R2/**,R3,R4,R5,R6,R7,R8,R9,R10**,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20,R21,R22,R23,R24,R25,R26,R27**/
		],
	sort(R,RA),
	reverse(RA,RB),
	RB=[[_,RC]|_Rest1],
	daysbspeoplearmy, %% dot answer on

	%% "Do you feel depressed?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",S1),
	trialy2_6("No",S2),
	S=[S1,S2/**,S3,S4,S5,S6,S7,S8,S9,S10**,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23,S24,S25,S26,S27**/
		],
	sort(S,SA),
	reverse(SA,SB),
	SB=[[_,SC]|_Rest2],
	daysbspeoplearmy, %% dot answer on

	%% "Do you have headaches?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",T1),
	trialy2_6("No",T2),
	T=[T1,T2/**,T3,T4,T5,T6,T7,T8,T9,T10**,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22,T23,T24,T25,T26,T27**/
		],
	sort(T,TA),
	reverse(TA,TB),
	TB=[[_,TC]|_Rest3],
	daysbspeoplearmy, %% dot answer on

	%% "Do you have a job?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",U1),
	trialy2_6("No",U2),
	U=[U1,U2/**,U3,U4,U5,U6,U7,U8,U9,U10**,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21,U22,U23,U24,U25,U26,U27**/
		],
	sort(U,UA),
	reverse(UA,UB),
	UB=[[_,UC]|_Rest4],
	daysbspeoplearmy, %% dot answer on

	%% "Do you have a business?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",V1),
	trialy2_6("No",V2),
	V=[V1,V2/**,V3,V4,V5,V6,V7,V8,V9,V10**,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23,V24,V25,V26,V27**/
		],
	sort(V,VA),
	reverse(VA,VB),
	VB=[[_,VC]|_Rest5],
	daysbspeoplearmy, %% dot answer on

	%% "Are you doing training?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",W1),
	trialy2_6("No",W2),
	W=[W1,W2/**,W3,W4,W5,W6,W7,W8,W9,W10**,W11,W12,W13,W14,W15,W16,W17,W18,W19,W20,W21,W22,W23,W24,W25,W26,W27**/
		],
	sort(W,WA),
	reverse(WA,WB),
	WB=[[_,WC]|_Rest6],
	daysbspeoplearmy, %% dot answer on

	%% "Do you have a partner?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",X1),
	trialy2_6("No",X2),
	X=[X1,X2/**,X3,X4,X5,X6,X7,X8,X9,X10**,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27**/
		],
	sort(X,XA),
	reverse(XA,XB),
	XB=[[_,XC]|_Rest7],
	daysbspeoplearmy, %% dot answer on

	%% "Do you have sex?"
	daysbspeoplearmy, %% dot question on
	trialy2_6("Yes",Y1),
	trialy2_6("No",Y2),
	Y=[Y1,Y2/**,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10**,Y11,Y12,Y13,Y14,Y15,Y16,Y17,Y18,Y19,Y20,Y21,Y22,Y23,Y24,Y25,Y26,Y27**/
		],
	sort(Y,YA),
	reverse(YA,YB),
	YB=[[_,YC]|_Rest8],
	daysbspeoplearmy, %% dot answer on

	writeln([Person,[appointment,Month1,Day1,Hour,Minutes,Seconds],["Do you see (hallucinatory) appearances?",RC],["Do you feel depressed?",SC],["Do you have headaches?",TC],["Do you have a job?",UC],["Do you have a business?",VC],["Are you doing training?",WC],["Do you have a partner?",XC],["Do you have sex?",YC]]).

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