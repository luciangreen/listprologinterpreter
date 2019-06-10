%% mind read test

%% Make files different for different tests

%% *** Important: initialise program before running for the first time:
%% N is 1*2*3*5,texttobr2(N). %% 100 As for 1 (turned on)*2 (to and from computer)*3 (rb, itself (already done), graciously give or blame, radio button for graciously give or blame)*5 (5 objects)
%% N is 1*2*3*5,texttobr2(N). %% 100 As for 1 (turned off)*2 (to and from computer)*3 (rb, itself (already done), graciously give or blame, radio button for graciously give or blame)*5 (5 objects)
%% also breason out and dot on objects before line above and breason out and dot on when recognising and saying object (with all objects having different breasonings)

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
	find_time(Hour,Minutes,Seconds),
	%% "Do you see (hallucinatory) appearances?"
	trialy2_6("Yes",R1),
	trialy2_6("No",R2),
	R=[R1,R2/**,R3,R4,R5,R6,R7,R8,R9,R10**,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20,R21,R22,R23,R24,R25,R26,R27**/
		],
	sort(R,RA),
	reverse(RA,RB),
	RB=[[_,RC]|_Rest1],

	%% "Do you feel depressed?"
	trialy2_6("Yes",S1),
	trialy2_6("No",S2),
	S=[S1,S2/**,S3,S4,S5,S6,S7,S8,S9,S10**,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23,S24,S25,S26,S27**/
		],
	sort(S,SA),
	reverse(SA,SB),
	SB=[[_,SC]|_Rest2],

	%% "Do you have headaches?"
	trialy2_6("Yes",T1),
	trialy2_6("No",T2),
	T=[T1,T2/**,T3,T4,T5,T6,T7,T8,T9,T10**,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22,T23,T24,T25,T26,T27**/
		],
	sort(T,TA),
	reverse(TA,TB),
	TB=[[_,TC]|_Rest3],

	%% "Do you have a job?"
	trialy2_6("Yes",U1),
	trialy2_6("No",U2),
	U=[U1,U2/**,U3,U4,U5,U6,U7,U8,U9,U10**,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21,U22,U23,U24,U25,U26,U27**/
		],
	sort(U,UA),
	reverse(UA,UB),
	UB=[[_,UC]|_Rest4],

	%% "Do you have a business?"
	trialy2_6("Yes",V1),
	trialy2_6("No",V2),
	V=[V1,V2/**,V3,V4,V5,V6,V7,V8,V9,V10**,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23,V24,V25,V26,V27**/
		],
	sort(V,VA),
	reverse(VA,VB),
	VB=[[_,VC]|_Rest5],

	%% "Are you doing training?"
	trialy2_6("Yes",W1),
	trialy2_6("No",W2),
	W=[W1,W2/**,W3,W4,W5,W6,W7,W8,W9,W10**,W11,W12,W13,W14,W15,W16,W17,W18,W19,W20,W21,W22,W23,W24,W25,W26,W27**/
		],
	sort(W,WA),
	reverse(WA,WB),
	WB=[[_,WC]|_Rest6],

	%% "Do you have a partner?"
	trialy2_6("Yes",X1),
	trialy2_6("No",X2),
	X=[X1,X2/**,X3,X4,X5,X6,X7,X8,X9,X10**,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27**/
		],
	sort(X,XA),
	reverse(XA,XB),
	XB=[[_,XC]|_Rest7],

	%% "Do you have sex?"
	trialy2_6("Yes",Y1),
	trialy2_6("No",Y2),
	Y=[Y1,Y2/**,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10**,Y11,Y12,Y13,Y14,Y15,Y16,Y17,Y18,Y19,Y20,Y21,Y22,Y23,Y24,Y25,Y26,Y27**/
		],
	sort(Y,YA),
	reverse(YA,YB),
	YB=[[_,YC]|_Rest8],

	writeln([Person,[appointment,Month1,Day1,Hour,Minutes,Seconds],["Do you see (hallucinatory) appearances?",RC],["Do you feel depressed?",SC],["Do you have headaches?",TC],["Do you have a job?",UC],["Do you have a business?",VC],["Are you doing training?",WC],["Do you have a partner?",XC],["Do you have sex?",YC]]).
