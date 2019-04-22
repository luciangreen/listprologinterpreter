%% Medicine.pl

medicine :-
	File="I love you, I love you, I love you.  Love.",
	doctors(Doctors),
	length(Doctors,DL),
	texttobr2(DL,u,File,u),!.
doctors([
%% Don't include meditators here
[doctor1firstname,surname,dobd,dobm,doby,started_day,started_month,started_year],
[doctor2firstname,surname,dobd,dobm,doby,started_day,started_month,started_year]
%% number of meditators + number of doctors= e.g. 21
]).
