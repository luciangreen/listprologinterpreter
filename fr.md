# List Prolog Interpreter\\n\\n# Prerequisites\\n\\n* Please download and install SWI-Prolog for your machine at https://www.swi-prolog.org/build/.\\n\\n* You may need to install gawk using Homebrew.\\n\\n* Install <a href=\\""https://github.com/soimort/translate-shell\\"">Translation Shell</a> on Mac, etc.\\nChange line in culturaltranslationtool/ctt.pl\\nconcat_list([\\""../../../trans \\"",FromLang,\\"":\\"",ToLang,\\"" '\\"",Input1,\\""'\\""],F), to correct location of <a href=\\""https://github.com/soimort/translate-shell\\"">trans</a>.\\n\\n# 1. Install manually\\n\\nDownload <a href=\\""http://github.com/luciangreen/listprologinterpreter/\\"">this repository</a>, the <a href=\\""https://github.com/luciangreen/Languages\\"">Languages repository</a> and <a href=\\""https://github.com/luciangreen/culturaltranslationtool\\"">Cultural Translation Tool</a> into the same folder.\\n\\n# 2. Or Installation from List Prolog Package Manager (LPPM)\\n\\n* Download the <a href=\\""https://github.com/luciangreen/List-Prolog-Package-Manager\\"">LPPM Repository</a>:\\n\\ngit clone https://github.com/luciangreen/List-Prolog-Package-Manager.git\\ncd List-Prolog-Package-Manager\\nswipl\\n['lppm'].\\nlppm_install(\\""luciangreen\\"",\\""listprologinterpreter\\"")\\nhalt\\n\\n\\n# Running\\n\\n* Download the repository to your machine.\\nIn the SWI-Prolog environment, enter:\\ncd listprologinterpreter\\nswipl\\n['listprolog'].\\n\\n* Running the tests\\nTo run all tests, enter:\\n"["test(off,NTotal,Score).","\\n\\n* To run a specific test:\\n","test1(off,TestNumber,Passed).","\\nwhere TestNumber is the test number from <a href=\\""https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl\\"">lpiverify4.pl</a>.\\n\\n* The query ","test1(off,7,Passed)."," tests the reverse predicate:\\ntest(7,","[[n,reverse],[[1,2,3],[],[v,l]]]",",\\n","[[[n,reverse],[[],[v,l],[v,l]]],[[n,reverse],[[v,l],[v,m],[v,n]],\\"":-\\"",[[[n,head],[[v,l],[v,h]]],[[n,tail],[[v,l],[v,t]]],[[n,wrap],[[v,h],[v,h1]]],[[n,append],[[v,h1],[v,m],[v,o]]],[[n,reverse],[[v,t],[v,o],[v,n]]]]]]",",\\n","[[[v,l], [3, 2, 1]]]",").\\n\\n\\n# Documentation\\n\\n* The interpreter is called in the form:\\n","international_interpret([lang,en],Debug,Query,TypeStatements,ModeStatements,Functions,Result).","\\n\\nWhere:\\nDebug - on or off for trace,\\nQuery - the query e.g. [[n,reverse],[[1,2,3],[],[v,l]]]\\nTypeStatements - e.g. [[n,reverse],[[[t,number],[t,number],[t,number]],[],[[t,number],[t,number],[t,number]]]]\\nModeStatements - e.g. [[n,reverse],[[input,input,input],[],[output,output,output]]]\\nFunctions - the algorithm e.g. see reverse above\\nResult - the result, e.g. [[[v,l], [3, 2, 1]]] ([] indicates failed and [[]] indicates the empty list).\\n\\n* Statements may be negated in the form:\\n\\n","[[n,not],[Statement]]","\\n\\n* Statements may be connected by the disjunctive (or):\\n\\n","[[n,or],[Statements1,Statements2]]","\\n\\n* If-then statements may either be in the form:\\n\\n","[[n,\\""->\\""],[Statements1,Statements2]]","\\n\\nThis means \\""If Statements1 then Statements2\\"".\\n\\n* Or, if-then statements may be in the form:\\n\\n","[[n,\\""->\\""],[Statements1,Statements2,Statements2a]]","\\n\\nThis means \\""If Statements1 then Statements2, else Statements2a\\"".\\n\\n* ","[[n,+],[1,2,[v,b]]]]"," ","[v,b]=3","\\n* ","[[n,-],[1,2,[v,b]]]]"," ","[v,b]=-1","\\n* ","[[n,*],[1,2,[v,b]]]]"," ","[v,b]=2","\\n* ","[[n,/],[1,2,[v,b]]]]"," ","[v,b]=0.5","\\n\\n* ","[[n,cut]]"," - behaves like swipl's ! (doesn't allow backtracking forward or back past it)\\n\\n* ","[[n,true]]"," - behaves like true\\n\\n* ","[[n,fail]]"," - fails the current predicate\\n\\n* ","[[n,atom],[Variable]]",", e.g. ","[[n,atom],[[v,a]]]"," - returns true if ","[v,a]=","e.g. ","'a'",", an atom\\n\\n* ","[[n,string],[Variable]]",", e.g. ","[[n,string],[[v,a]]]"," - returns true if ","[v,a]=","e.g. ","\\""a\\""",", a string\\n\\n* ","[[n,number],[Variable]]",", e.g. ","[[n,number],[14]]"," - returns true where ","14"," is a number\\n\\n* ","[[n,letters],[Variable]]",", e.g. ","[[n,letters],[\\""abc\\""]]"," - returns true where ","\\""abc\\"""," is letters\\n\\n* ","[[n,IsOperator],[Variable1,Variable2]]",", where ","IsOperator=is"," or ","IsOperator=\\""=\\""",", e.g. ","[[n,=],[[v,a],1]]"," - returns true if ","[v,a]=1","\\n\\n* ","[[n,ComparisonOperator],[Variable1,Variable2]]",", where ","ComparisonOperator=\\"">\\"",\\"">=\\"",\\""<\\"", \\""=<\\"", \\""=\\"" or \\""=\\=\\"""," e.g. ","[[n,=\\=],[1,2]]"," - returns ","not(1=2)=true",".\\n\\n* ","[[n,equals1],[Variable1,[Variable2,Variable3]]]"," e.g. ","[[n,equals1],[[\\""a\\"",\\""b\\""],[[v,a],[v,b]]]]"," returns ","[v,a]=\\""a\\"""," and ","[v,b]=\\""b\\""","\\n\\n* ","[[n,equals2],[Variable1,[Variable2,Variable3]]]"," e.g. ","[[n,equals2],[[v,a],[\\""a\\"",\\""b\\""]]]"," returns ","[v,a]=[\\""a\\"",\\""b\\""]","\\n\\n* ","[[n,equals3],[Variable1,Variable2]]"," e.g. ","[[n,equals3],[[v,a],[1,2,3]]]"," returns ","[v,a]=[1,2,3]","\\n\\n* ","[[n,wrap],[Variable1,Variable2]]"," e.g. ","[[n,wrap],[\\""a\\"",[v,b]]]"," returns ","[v,b]=[\\""a\\""]","\\n\\n* ","[[n,unwrap],[Variable1,Variable2]]"," e.g. ","[[n,wrap],[[\\""a\\""],[v,b]]]"," returns ","[v,b]=\\""a\\""","\\n\\n* ","[[n,head],[Variable1,Variable2]]"," e.g. ","[[n,head],[[\\""a\\"",\\""b\\"",\\""c\\""],[v,b]]]"," returns ","[v,b]=\\""a\\""","\\n\\n* ","[[n,tail],[Variable1,Variable2]]"," e.g. ","[[n,tail],[[\\""a\\"",\\""b\\"",\\""c\\""],[v,b]]]"," returns ","[v,b]=[\\""b\\"",\\""c\\""]","\\n\\n* ","[[n,member],[Variable1,Variable2]]"," e.g. ","[[n,member],[[\\""a\\"",\\""b\\"",\\""c\\""],[v,b]]]"," returns ","[v,b]=\\""a\\""",", ","[v,b]=\\""b\\"""," or ","[v,b]=\\""c\\""",", or e.g. ","[[n,member],[[\\""a\\"",\\""b\\"",\\""c\\""],\\""c\\""]]"," returns true.\\n\\n* ","[[n,member2],[Variable1,Variable2]]"," e.g. ","[[n,member2],[[\\""a\\"",\\""b\\"",\\""c\\""],[v,b]]]"," returns ","[v,b]=\\""a\\""",", ","[v,b]=\\""b\\"""," or ","[v,b]=\\""c\\""","\\n\\n* ","[[n,delete],[Variable1,Variable2,Variable3]]"," e.g. ","[[n,delete],[[\\""a\\"",\\""b\\"",\\""b\\"",\\""c\\""],\\""b\\"",[v,c]]]"," returns ","[v,c]=[\\""a\\"",\\""c\\""]","\\n\\n* ","[[n,append],[Variable1,Variable2,Variable3]]"," e.g. ","[[n,append],[[\\""a\\"",\\""b\\""],[\\""c\\""],[v,d]]]"," returns ","[v,d]=[\\""a\\"",\\""b\\"",\\""c\\""]",".  Note: Variable2 must be in list form, not e.g. ","\\""c\\""",", or the command will fail.  Wrap may wrap in ","[]",".\\n\\n* ","[[n,stringconcat],[Variable1,Variable2,Variable3]]"," e.g. ","[[n,stringconcat],[\\""hello \\"",\\""john\\"",[v,c]]]"," returns ","[v,c]=\\""hello john\\""",".\\n\\n* ","[[n,stringtonumber],[Variable1,Variable2]]"," e.g. ","[[n,stringtonumber],[\\""3\\"",[v,b]]]"," returns ","[v,b]=3","\\n\\n* ","[[n,random],[Variable1]]"," e.g. ","[[n,random],[[v,r]]]"," returns e.g. ","[v,r]=0.19232608946956326","\\n\\n* ","[[n,length],[Variable1,Variable2]]"," e.g. ","[[n,length],[[1,2,3],[v,l]]]"," returns ","[v,l]=3","\\n\\n* ","[[n,ceiling],[Variable1,Variable2]]"," e.g. ","[[n,ceiling],[0.19232608946956326,[v,c]]]"," returns ","[v,c]=1","\\n\\n* ","[[n,date],[Year,Month,Day,Hour,Minute,Seconds]]"," e.g. ","[[n,date],[[v,year],[v,month],[v,day],[v,hour],[v,minute],[v,second]]]"," returns e.g. ","[v,year]=2019",", ","[v,month]=11",", ","[v,day]=6",", ","[v,hour]=12",", ","[v,minute]=15",", ","[v,second]=20.23353409767151",".\\n\\n* ","[[n,sqrt],[Variable1,Variable2]]"," e.g. ","[[n,ceiling],[4,[v,c]]]"," returns ","[v,c]=2","\\n\\n* ","[[n,round],[Variable1,Variable2]]"," e.g. ","[[n,round],[1.5,[v,c]]]"," returns ","[v,c]=2","\\n\\n* ","[[n,equals4],[Variable1,Variable2]]"," e.g. ","[[n,equals4],[[[v,c],\\""|\\"",[v,d]],[1,2,3]]"," returns ","[v,c]=1"," and ","[v,d]=[2,3]",".  You may use either order (i.e. a=1 or 1=a).  Multiple items are allowed in the head of the list, there may be lists within lists, and lists with pipes must have the same number of items in the head in each list, or no pipe in the other list.\\n\\n* ","[[n,findall],[Variable1,Variable2,Variable3]]"," e.g. ","[[n,equals3],[[v,a],[1,2,3]]],[[n,findall],[[v,a1],[[n,member2],[[v,a],[v,a1]]],[v,b]]]"," returns ","[v,b]=[1,2,3]","\\n\\n* ","[[n,string_from_file],[Variable1,Variable2]]"," e.g. ","[[n,string_from_file],[[v,a],\\""file.txt\\""]]"," returns ","[v,a]=\\""Hello World\\""","\\n\\n* ","[[n,maplist],[Variable1,Variable2,Variable3,Variable4]]"," e.g. ","[[n,maplist],[[n,+],[1,2,3],0,[v,b]]]"," returns ","[v,b]=6","\\n\\n\\n# Grammars\\n\\n* List Prolog supports grammars, for example:\\n\\n\\ntest(8,","[[n,grammar1],[\\""apple\\""]]",",\\n","[[[n,grammar1],[[v,s]],\\"":-\\"",[[[n,noun],[[v,s],\\""\\""]]]],[[n,noun],\\""->\\"",[\\""apple\\""]]]",",\\n","[[]]",").\\n\\n* In ","[[n,noun],[[v,s],\\""\\""]]",", the argument ","[v,s]"," is the entry string and ","\\""\\"""," is the exit string.\\n\\n* In the above example, the word ","\\""apple\\"""," is parsed.  Grammars use ","\\""->\\""",", not ","\\"":-\\""",".\\n\\n* Grammars may be recursive (see test 9 in <a href=\\""https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl\\"">lpiverify4.pl</a>), i.e. they may repeat until triggering the base case.\\n\\n* Grammars may have extra arguments, placed after the other arguments.  The entry and exit string arguments are only used outside the grammar, and can be accessed, e.g.:\\n","[[n,lookahead],[[v,a],[v,a],[v,b]],\\"":-\\"",[[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]","	  \\nNote: ","\\"":-\\""",", not ","\\""->\\""","  \\n		\\n		\\n* Base cases for recursive grammars require e.g.\\n","[[n,compound212],[\\""\\"",\\""\\"",[v,t],[v,t]]]","\\n\\nand a \\""bottom case\\"" in case it is not at the end of the string, e.g.:\\n","[[n,compound212],[[v,u],[v,u],[v,t],[v,t]]]","\\n\\ngiven the clause:\\n","[[n,compound21],[[v,t],[v,u]],\\""->\\"",[[[n,a]],[[n,code],[[n,wrap],[\\""a\\"",[v,itemname1]]],[[n,append],[[v,t],[v,itemname1],[v,v]]]],[[n,compound212],[[v,v],[v,u]]]]],","\\n\\nIn it, ","[[n,a]]"," calls a grammar predicate called ","\\""a\\""",".  ","[[n,code],...]"," is similar to ","{}"," in SWI-Prolog (it allows commands to be called within a grammar).  The code wraps a string and appends it to a list, before exiting code and calling the grammar predicate ","compound212",".  ","v"," and ","u"," are not entry and exit strings, they are extra arguments, handled with ","t"," in the base cases above.\\n\\n* Sometimes there is another recursive clause, which calls itself:\\n","[[n,compound21],[[v,t],[v,u]],\\""->\\"",[[[n,a]],\\"",\\"",[[n,compound21],[[],[v,compound1name]]],[[n,code],[[n,wrap],[\\""a\\"",[v,itemname1]]],[[n,append],[[v,t],[v,itemname1],[v,v]]],[[n,append],[[v,v],[v,compound1name],[v,u]]]]]],","\\n		  \\n","[[n,a]]",", a call, could be substituted with ","[v,b]",", however ","[[n,a]]"," would call a grammar predicate and ","[v,b]"," would return a character.\\n\\n* When we need to find out what the next character is but parse the character somewhere else, we use lookahead.\\n\\nE.g.:\\n","[[n,word21],[[v,t],[v,u]],\\""->\\"",[[v,a],[[n,commaorrightbracketnext]],[[n,code],[[n,letters],[[v,a]]],[[n,stringconcat],[[v,t],[v,a],[v,v]]]],[[n,word212],[[v,v],[v,u]]]]],","\\n\\nWith ","commaorrightbracketnext"," (which looks ahead for a comma or ","\\""]\\""","), it doesn't return true in ","\\""a\\"""," of ","\\""ab,c\\""","\\nand goes to ","\\""b\\"""," instead as wanted.\\n\\n* Note, we can call ","lookahead"," as a grammar predicate:\\n","[[n,lookahead],[\\""ate\\""]]","\\n\\neven though the predicate itself is not a grammar predicate:\\n","[[n,lookahead],[[v,a],[v,a],[v,b]],\\"":-\\"",[[[n,stringconcat],[[v,b],[v,d],[v,a]]]]]","\\n\\n* Predicates or predicates to modify to provide the function of string to list (test 15), split on character(s) (test 17), ","intersection"," and ","minus"," are in <a href=\\""https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl\\"">lpiverify4.pl</a>.\\n\\n# Functional List Prolog (FLP)\\n\\n* List Prolog has an optional functional mode.  In FLP, function calls may be passed as variables and functions may have strong types.\\n\\n* In the following, ","[[n,function],[[[n,function2],[2]],1,1,[v,c]]]"," function2 is passed as a variable.  ","[v,f11]"," is replaced with the function name.\\n\\n%% c=f((g(2)), 1, 1)\\ntest(53,","[[n,function],[[[n,function2],[2]],1,1,[v,c]]]",",\\n[\\n","[[n,function],[[v,f1],[v,a],[v,b],[v,c]],\\"":-\\"",[[[n,equals1],[[v,f1],[[v,f11],[v,f12]]]],[[n,getitemn],[1,[v,f12],[v,bb]]],[[v,f11],[[v,bb],[v,d],[v,f]]],[[n,+],[[v,a],[v,b],[v,e]]],[[n,+],[[v,e],[v,f],[v,g]]],[[n,+],[[v,g],[v,d],[v,c]]]]]",",\\n","[[n,function2],[[v,bb],[v,a],[v,f]],\\"":-\\"",[[[n,is],[[v,a],[v,bb]]],[[n,is],[[v,f],1]]]]",",\\n","[[n,getitemn],[1,[v,b],[v,c]],\\"":-\\"",[[[n,head],[[v,b],[v,c]]]]]",",\\n","[[n,getitemn],[[v,a],[v,b],[v,c]],\\"":-\\"",[[[n,not],[[[n,=],[[v,a],0]]]],[[n,tail],[[v,b],[v,t]]],[[n,-],[[v,a],1,[v,d]]],[[n,getitemn],[[v,d],[v,t],[v,c]]]]]","\\n]\\n,","[[[[v,c], 5]]]",").\\n