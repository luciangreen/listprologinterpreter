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

sectest(Person):-
	find_time(H,M,S),
	threats(0,Threats),
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
