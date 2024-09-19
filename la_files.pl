delete_invisibles_etc(F,G) :-
	findall(J,(member(H,F),atom_string(H,J),not(J="."),not(J=".."),not(string_concat(".",_,J))),G),!.


open_file_s(Path,File_term) :-
		phrase_from_file_s(string(File), Path),
		string_codes(File_string,File),
		term_to_atom(File_term,File_string),!.

open_file_sh(F1,File_term) :-
 (absolute_url(F1)->
 F1=Path;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,Path))),
		split_string(Path,"/","/",Path1),
		append(_P1,[P2],Path1),
		string_concat(P2,"tmp54837",P3),
		
		Scp="scp -pr ",
		concat_list([Scp,Path," ",P3],Command),
		shell1_s(Command),
	

		open_file_s(P3,File_term),
		
		Rm="rm -rf",
		concat_list([Rm," ",P3],Command1),
		shell1_s(Command1),
		!.


open_string_file_s(Path,File_string) :-
		phrase_from_file_s(string(File), Path),
		string_codes(File_string,File),!.
		%term_to_atom(File_term,File_string),!.


save_file_s(Path,Content_term_or_string) :-
	((compound(Content_term_or_string)->true;Content_term_or_string=[])->term_to_atom(Content_term_or_string,String);((string(Content_term_or_string)->true;(atom(Content_term_or_string)->true;
	number(Content_term_or_string))
	)->Content_term_or_string=String;
	(concat_list(["Error: save_file_s content not in compound, atom or string format."],Notification),
	writeln0(Notification),abort))),
	(open_s(Path,write,Stream),
	write(Stream,String),
	close(Stream)),
	%sleep(2),
	!.

% working_directory_sh(_,"root@x.x.x.x:~/").
% save_file_sh("a1.txt", [a]).

save_file_sh(F1,File_term) :-
 (absolute_url(F1)->
 F1=Path;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,Path))),
		split_string(Path,"/","/",Path1),
		append(_P1,[P2],Path1),
		string_concat(P2,"tmp54837",P3),

		save_file_s(P3,File_term),
		
		Mv="rsync -avz --remove-source-files ",
		concat_list([Mv,P3," ",Path],Command),
		shell1_s(Command),
	
		!.

directory_files_s(F1,B) :-
	atom_string(F1,F2),
	directory_files(F2,B),!.


	% ssh root@46.250.240.201
	%  open_file_sh("root@x.x.x.x:~/Dropbox/GitHub/",)
	
% directory_files_sh("",A).
	
directory_files_sh(F1,B) :-	
 (absolute_url(F1)->
 F1=F2;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,F2))),
	split_string(F2,":",":",F),
	append([G],[H],F),
	string_concat(K,K1,H),
	string_length(K,2),
	(K1=""->K11="./";K11=K1),
	foldr(string_concat,["main_tmp :- catch((directory_files('",K11,"',A),term_to_atom(A,A1),write(A1)),Err,handle_error(Err)),halt.\nmain_tmp :- halt(1).\nhandle_error(_Err):-\n  halt(1)."],S1),
	foldr(string_concat,[G,":~/tmp54837.pl"],P1),
	save_file_sh(P1,S1),
	foldr(string_concat,["ssh ",G," swipl --goal=main_tmp --stand_alone=true -o tmp54837 -c tmp54837.pl"],S2),
	(catch(shell1_s(S2),_,fail)->
	(foldr(string_concat,["ssh ",G," ./tmp54837"],S),
	(catch(shell1_s(S,Out),_,fail)->
	(
	term_to_atom(B%Out1
	,Out),
	foldr(string_concat,["ssh ",G," rm tmp54837.pl\nssh ",G," rm tmp54837"],S3),
	shell1_s(S3)
	);(writeln("directory_files_sh aborted."),abort))
	);(writeln("directory_files_sh aborted."),abort)),!.
	
exists_file_s(F1) :-
 atom_string(F2,F1),
 exists_file(F2),!.

exists_file_sh(F1) :-
 (absolute_url(F1)->
 F1=F2;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,F2))),
	split_string(F2,":",":",F),
	append([G],[H],F),
	string_concat(K,K1,H),
	string_length(K,2),
	foldr(string_concat,["main_tmp :- catch(exists_file('",K1,"'),Err,handle_error(Err)),halt.\nmain_tmp :- halt(1).\nhandle_error(_Err):-\n  halt(1)."],S1),
	foldr(string_concat,[G,":~/tmp54837.pl"],P1),
	save_file_sh(P1,S1),
	foldr(string_concat,["ssh ",G," swipl --goal=main_tmp --stand_alone=true -o tmp54837 -c tmp54837.pl"],S2),
	(catch(shell1_s(S2),_,fail)->
	(foldr(string_concat,["ssh ",G," ./tmp54837"],S),
	(catch(shell1_s(S,_Out),_,fail)->
	(
	true,
	foldr(string_concat,["ssh ",G," rm tmp54837.pl\nssh ",G," rm tmp54837"],S3),
	shell1_s(S3)
	);fail))
	;(writeln("exists_file aborted."),abort)),!.

exists_directory_s(F1) :-
 atom_string(F2,F1),
 exists_directory(F2),!.

exists_directory_sh(F1) :-
 (absolute_url(F1)->
 F1=F2;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,F2))),
	split_string(F2,":",":",F),
	append([G],[H],F),
	string_concat(K,K1,H),
	string_length(K,2),
	foldr(string_concat,["main_tmp :- catch(exists_directory('",K1,"'),Err,handle_error(Err)),halt.\nmain_tmp :- halt(1).\nhandle_error(_Err):-\n  halt(1)."],S1),
	foldr(string_concat,[G,":~/tmp54837.pl"],P1),
	save_file_sh(P1,S1),
	foldr(string_concat,["ssh ",G," swipl --goal=main_tmp --stand_alone=true -o tmp54837 -c tmp54837.pl"],S2),
	(catch(shell1_s(S2),_,fail)->
	(foldr(string_concat,["ssh ",G," ./tmp54837"],S),
	(catch(shell1_s(S,_Out),_,fail)->
	(
	true,
	foldr(string_concat,["ssh ",G," rm tmp54837.pl\nssh ",G," rm tmp54837"],S3),
	shell1_s(S3)
	);fail))
	;(writeln("exists_directory_sh aborted."),abort)),!.

delete_file_sh(F1) :-
 (absolute_url(F1)->
 F1=F2;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,F2))),
	split_string(F2,":",":",F),
	append([G],[H],F),
	string_concat(K,K1,H),
	string_length(K,2),
	foldr(string_concat,["main_tmp :- catch(delete_file('",K1,"'),Err,handle_error(Err)),halt.\nmain_tmp :- halt(1).\nhandle_error(_Err):-\n  halt(1)."],S1),
	foldr(string_concat,[G,":~/tmp54837.pl"],P1),
	save_file_sh(P1,S1),
	foldr(string_concat,["ssh ",G," swipl --goal=main_tmp --stand_alone=true -o tmp54837 -c tmp54837.pl"],S2),
	(catch(shell1_s(S2),_,fail)->
	(foldr(string_concat,["ssh ",G," ./tmp54837"],S),
	(catch(shell1_s(S,_Out),_,fail)->
	(
	true,
	foldr(string_concat,["ssh ",G," rm tmp54837.pl\nssh ",G," rm tmp54837"],S3),
	shell1_s(S3)
	);fail))
	;(writeln("delete_file_sh aborted."),abort)),!.


delete_directory_sh(F1) :-
 (absolute_url(F1)->
 F1=F2;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,F2))),
	split_string(F2,":",":",F),
	append([G],[H],F),
	string_concat(K,K1,H),
	string_length(K,2),
	foldr(string_concat,["main_tmp :- catch(delete_directory('",K1,"'),Err,handle_error(Err)),halt.\nmain_tmp :- halt(1).\nhandle_error(_Err):-\n  halt(1)."],S1),
	foldr(string_concat,[G,":~/tmp54837.pl"],P1),
	save_file_sh(P1,S1),
	foldr(string_concat,["ssh ",G," swipl --goal=main_tmp --stand_alone=true -o tmp54837 -c tmp54837.pl"],S2),
	(catch(shell1_s(S2),_,fail)->
	(foldr(string_concat,["ssh ",G," ./tmp54837"],S),
	(catch(shell1_s(S,_Out),_,fail)->
	(
	true,
	foldr(string_concat,["ssh ",G," rm tmp54837.pl\nssh ",G," rm tmp54837"],S3),
	shell1_s(S3)
	);fail))
	;(writeln("delete_directory_sh aborted."),abort)),!.

make_directory_s(F1) :-
 atom_string(F2,F1),
 make_directory(F2),!.

% make_directory_sh("root@x.x.x.x:~/a").

make_directory_sh(F1) :-
 (absolute_url(F1)->
 F1=F2;
 (working_directory_sh(F11,F11),
 string_concat_url(F11,F1,F2))),
	split_string(F2,":",":",F),
	append([G],[H],F),
	string_concat(K,K1,H),
	string_length(K,2),
	foldr(string_concat,["main_tmp :- catch(make_directory('",K1,"'),Err,handle_error(Err)),halt.\nmain_tmp :- halt(1).\nhandle_error(_Err):-\n  halt(1)."],S1),
	foldr(string_concat,[G,":~/tmp54837.pl"],P1),
	save_file_sh(P1,S1),
	foldr(string_concat,["ssh ",G," swipl --goal=main_tmp --stand_alone=true -o tmp54837 -c tmp54837.pl"],S2),
	(catch(shell1_s(S2),_,fail)->
	(foldr(string_concat,["ssh ",G," ./tmp54837\nssh ",G," rm tmp54837.pl\nssh ",G," rm tmp54837"
	],S),
	(catch(shell1_s(S,_Out),_,fail)->
	(
	true
	);(writeln("make_directory_sh aborted."),abort))
	);(writeln("make_directory_sh aborted."),abort)),!.

string_concat_url("",F1,F1) :-!.
string_concat_url(F11,F1,F2) :-
 (string_concat(_F12,"/",F11)->
 string_concat(F11,F1,F2);
 (foldr(string_concat,[F11,"/",F1],F2))),!.
 
absolute_url(A) :-
 ((split_string(A,":",":",F),
 append([_G],[_H],F))->true;
 (split_string(A,"/","/",Path1),
 length(Path1,L),L>=2,
 append([P1],_P2,Path1),
 not(string_concat("/",_,P1)))),!.
 
working_directory1(A1,B1) :-
 (string(A1)->atom_string(A,A1);A=A1),
 (string(B1)->atom_string(B,B1);B=B1),
 term_to_atom(working_directory(A,B),Atom),
 	catch(working_directory(A,B), _, (foldr(string_concat,["Error on ",Atom%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text41),writeln1(Text41)%fail%abort
 	)),!.

% working_directory_sh(_,"root@x.x.x.x:~/").

working_directory_sh(A,B) :-
 ((var(A),not(var(B)))->
 (dynamic(working_directory_sh1/1),
 
 	split_string(B,":",":",F),
	append([G],[H],F),
	string_concat(K,K1,H),
	string_length(K,2),
	(K1=""->K11="./";K11=K1),
	foldr(string_concat,[G,":",K,K11],B1),

 retractall(working_directory_sh1(_)),
 assertz(working_directory_sh1(B1))
 );
 ((var(A),var(B))->
 (dynamic(working_directory_sh1/1),
 working_directory_sh1(A),A=B))),!.
 
 
 
container(A) :-
A,!.
 /*%trace,
 get_time(TS),stamp_date_time(TS,date(Year,Month,Day,Hour1,Minute1,Seconda,_A,_TZ,_False),local),
 atomic_list_concat(Seconda1,'.',Seconda),
 atomic_list_concat(Seconda1,'',Seconda2),
 
	foldr(string_concat,[Year ,"_", Month ,"_", Day ,"_", Hour1 ,"_", Minute1 ,"_", Seconda2],FN),


%trace,
 working_directory(WD,WD),
 cd,
 working_directory(WDCD,WDCD),
 atom_concat(WDCD,WDCD3,WD),
 once(sub_string_strings(WDCD3,WDCD4,'/',WDCD5)),once(sub_string_strings(WDCD5,WDCD6,'/',_)),
 foldr(atom_concat,[WDCD,WDCD4,'/',WDCD6,'/'],WDCD1),
 working_directory(_,WDCD1),
 make_directory(FN),
 foldr(atom_concat,[WDCD1,FN,'/'],WDCD2),
 working_directory(_,WDCD2),

 %home_dir1(HD),
 %catch
 ((functor(A,Name,Arity),
 functor(A2,Name,Arity),
 source_file(A2,Path),
 term_to_atom(A,AA),
 
reverse_string(FN,RFN),
%trace,
foldr(string_concat,["#!/usr/bin/swipl -g main",FN," -q\n\n",":-include('",Path,"').\n","handle_error(_Err):-\n  halt(1).\n","main",FN," :-\n    catch((working_directory(_,'",WD,"'),A=",AA,",A,term_to_atom(A,LP1),writeln(\"",RFN,"\"),write(LP1)),Err, handle_error(Err)), nl,\n    halt.\n","main",FN," :- halt(1).\n"],String),

%working_directory1(A1,A1),

foldr(string_concat,["tmp",FN,".pl"],GP),
save_file_s(GP,String),
foldr(string_concat,["chmod +x ",GP,"\n","swipl -g main",FN," -q ./",GP],S3),%,


(catch(bash_command(S3,LP), _, (foldr(string_concat,["Container error."],Text4),writeln(Text4),
	fail
 	)
 	)
 	->
 	(
%trace,
delete_tmp(FN),
%trace,
string_strings(LP,LP1),
string_strings(RFN,Sub_list),
once((sub_list(LP1,_Before_list,Sub_list,LP2))),
foldr(atom_concat,LP2,LP3),
	term_to_atom(A,LP3)
	);(delete_tmp(FN),working_directory1(_,WD),fail)),
		
	working_directory1(_,WD))
	),
	!.


delete_tmp(FN):-
foldr(string_concat,["rm -f tmp",FN,".pl"
 ],Command315),
 	catch(bash_command(Command315,_), _, (foldr(string_concat,["Error deleting tmp",FN,".pl."
	],Text42),writeln(Text42),abort
 	)),
 	working_directory(_,'../'),
	delete_directory(FN),
	!.
	
*/