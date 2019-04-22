%% Meditation.pl

%% lucian, green, friendliness, medicine, sutras, courses, qi gong and yoga mantras and sutras - arem 10 br links
%% Stops linking in meditators 100 years after date of learning LM

:- include('listprolog').
:- include('texttobr').
:- include('la_strings').
:- include('mergetexttobrdict').
:- include('edit.pl').

meditation :-
	File="I love you, I love you, I love you.  Arem.",
	Utterances=[lucianicmeditationapps,dailyregimen,nobodyproblemsapp,lucian, green],
	meditators(Meditators),
	length(Utterances,UL),
	length(Meditators,ML),
	Length2 is UL+ML,	texttobr2(Length2,u,File,u),!.
initiate_utterances(_File,[]) :- !.
initiate_utterances(File,[_Utterances|Utterances]) :-
		texttobr2(File),texttobr(File),
		initiate_utterances(File,Utterances).
protect(_File,_Year,_Month,_Day,[],Old1,Old1) :- !.
protect(File,Year1,_Month1,_Day1,[Meditator|Meditators],Old1,Old2) :-
	Meditator=[_FirstName,_LastName,_DayDOB,_MonthDOB,_YearDOB,_DayLearned,_MonthLearned,YearLearned],
	Year2 is YearLearned+101,
	(Year2=Year1->append(Old1,[Meditator],Old3);Old3=Old1),
	texttobr2(File),texttobr(File), %% Sender
	texttobr2(File),texttobr(File), %% Recipient
	
	protect(File,Year1,_Month,_Day,Meditators,Old3,Old2).
currentDate(Today) :-
    get_time(Stamp),
    stamp_date_time(Stamp,DateTime,local),
    date_time_value(date,DateTime,Today).
meditators([
[meditator1firstname,surname,dobd,dobm,doby,started_day,started_month,started_year],
[meditator2firstname,surname,dobd,dobm,doby,started_day,started_month,started_year]
]).
