:- include(library(edit)).
:- multifile
	edit:edit_command/2.
:- multifile prolog_edit:load/0.
%%:- use_module(library(filesex)).

run1 :-
	Brdict1='brdict1.txt',
	Brdict2='brdict2.txt',
	Brdict1vps='brdict1vps.txt',
	Brdict2vps='brdict2vps.txt',
	MacPath='~/yourfolder/',
	MacPath1='/Users/yourname/yourfolder/',
	VPSPath='root@xxx.xxx.xxx.xxx:/var/www/yourdomain.com/',
	Scp='scp -p',
	atom_concat(MacPath1,Brdict1,   Brdict1Filename),
	atom_concat(MacPath1,Brdict1vps,Brdict1vpsFilename),

	
	shell1(Scp,VPSPath,Brdict1,MacPath,Brdict1vps,Command1),
	shell1(Command1),

	((time_file(Brdict1Filename,Brdict1FilenameTime),
	time_file(Brdict1vpsFilename,Brdict1FilenameTime))->true;

	(prep1(Brdict1,Brdict1Term),
	prep1(Brdict1vps,Brdict1vpsTerm),

	(Brdict1Term=Brdict1vpsTerm->true;
	
	(append(Brdict1Term,Brdict1vpsTerm,Brdict1Term2),

	update1(Brdict1,Brdict1Term2),

	shell1(Scp,MacPath,Brdict1,VPSPath,Brdict1,Command3),
	shell1(Command3))))),

	shell1('rm',MacPath,Brdict1vps,Command5),
	shell1(Command5),


	atom_concat(MacPath1,Brdict2,   Brdict2Filename),
	atom_concat(MacPath1,Brdict2vps,Brdict2vpsFilename),

	shell1(Scp,VPSPath,Brdict2,MacPath,Brdict2vps,Command2),
	shell1(Command2),
	
	((time_file(Brdict2Filename,Brdict2FilenameTime),
	time_file(Brdict2vpsFilename,Brdict2FilenameTime))->true;

	(prep2(Brdict2,Brdict2Term),
	prep2(Brdict2vps,Brdict2vpsTerm),
	
	(Brdict2Term=Brdict2vpsTerm->true;

	(append(Brdict2Term,Brdict2vpsTerm,Brdict2Term2),
	
	update1(Brdict2,Brdict2Term2),
	
	shell1(Scp,MacPath,Brdict2,VPSPath,Brdict2,Command4),
	shell1(Command4))))),
	shell1('rm',MacPath,Brdict2vps,Command6),
	shell1(Command6),
!.
	
prep1(Brdict1,Brdict1Term1) :-
	phrase_from_file(string(BrDict0), Brdict1),
	splitfurther(BrDict0,BrDict01),
	sort(BrDict01,Brdict1Term1),
	!.
	
prep2(Brdict2,Brdict2Term1) :-
	phrase_from_file(string(BrDict0t), Brdict2),
	splitfurthert(BrDict0t,BrDict01t),
	sort(BrDict01t,Brdict2Term1),
	!.

update1(File,BrDict1) :-
	sort(BrDict1,BrDict2),
	open(File,write,Stream),
	write(Stream,BrDict2),
	close(Stream).
	
shell1(Command1,Path,Object,Command4) :-
	atom_concat(Command1,' ',Command2),
	atom_concat(Command2,Path,Command3),
	atom_concat(Command3,Object,Command4).

shell1n(Command1,Command2,Command3) :-
	atom_concat(Command1,'\n',Command1A),
	atom_concat(Command1A,Command2,Command3).

shell1(Command1,Path1,Object1,Path2,Object2,Command5) :-
	shell1(Command1,Path1,Object1,Command4),
	shell1(Command4,Path2,Object2,Command5).
	
shell1(Command) :-
				(bash_command(Command,_)->
					true;
					(writeln(["Failed shell1 command: ",Command]),abort)
				).

bash_command(Command, Output) :-
        setup_call_cleanup(process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out)).
