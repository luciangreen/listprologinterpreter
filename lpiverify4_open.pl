%% Test cases, Debug=trace=on or off, N=output=result
testopen(Debug,NTotal) :- testopen(Debug,0,NTotal),!.
testopen(_Debug,NTotal,NTotal) :- NTotal=4, !.
testopen(Debug,NTotal1,NTotal2) :-
	NTotal3 is NTotal1+1,
	testopen_cases(NTotal3,Query,Functions),
	((international_interpret([lang,"en"],Debug,Query,Functions,Result),not(Result=[]))->(writeln0([test,NTotal3,result,Result]),writeln0([test,NTotal3,passed]));(writeln0([test,NTotal3,failed]))),
	writeln0(""),
	testopen(Debug,NTotal3,NTotal2),!.

%% Test individual cases, Debug=trace=on or off, N=case number
testopen1(Debug,N) :-
	testopen_cases(N,Query,Functions),
((international_interpret([lang,"en"],Debug,Query,Functions,Result),not(Result=[]))->(writeln0([test,N,result,Result]),writeln0([test,N,passed]));(writeln0([test,N,failed]))),!.

testopen_cases(1,[[n,datetime],[[v,year],[v,month],[v,day],[v,hour],[v,minute],[v,second]]],

[
[[n,datetime],[[v,y],[v,m],[v,d],[v,h],[v,mi],[v,s]],":-",
	[[[n,date],[[v,y],[v,m],[v,d],[v,h],[v,mi],[v,s]]]]]]       
).

testopen_cases(2,[[n,algwriter],[[v,na]]],
[
        [[n,algwriter],[[v,na]],":-",
        [       [[n,makerandomlist],[3,[],[v,r1]]],
                [[n,makerandomlist],[3,[],[v,r2]]],
                [[n,wrap],[[v,r1],[v,nb1]]],
                [[n,wrap],[[v,r2],[v,nb2]]],
                [[n,append],[[v,nb1],[v,nb2],[v,nb3]]],
                [[n,randomfns],[8,[v,nb3],[v,na]]]
        ]],

        [[n,makerandomlist],[0,[v,a],[v,a]]],
        [[n,makerandomlist],[[v,a],[v,c1],[v,c]],":-",
        [       [[n,not],[[[n,=],[[v,a],0]]]],
                [[n,random],[[v,r]]],
                [[n,*],[[v,r],5,[v,r1]]],
                [[n,ceiling],[[v,r1],[v,n1]]],
                [[n,wrap],[[v,n1],[v,n2]]],
                [[n,append],[[v,c1],[v,n2],[v,nb3]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,makerandomlist],[[v,d],[v,nb3],[v,c]]]
        ]],
        
        [[n,randomfns],[0,
        						[v,a],[v,a]]],
        [[n,randomfns],[[v,a],[v,b],
        						[v,c]],":-",
        [       [[n,not],[[[n,=],[[v,a],0]]]],
                [[n,randomlist],[[v,b],[v,na1]]],
                [[n,randomlist],[[v,b],[v,na2]]],
                [[n,randomfn],[[v,na1],[v,na2],[v,nb]]],
                [[n,wrap],[[v,nb],[v,nb2]]],
                [[n,append],[[v,b],[v,nb2],[v,nb3]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,randomfns],[[v,d],
                			[v,nb3],[v,c]]]
        ]],

        [[n,randomlist],[[v,b],[v,na]],":-",
        [       [[n,random],[[v,r]]],
                [[n,length],[[v,b],[v,bl]]],
                [[n,*],[[v,r],[v,bl],[v,n]]],
                [[n,ceiling],[[v,n],[v,n1]]],
                [[n,getitemn],[[v,n1],[v,b],[v,na]]]
        ]],

        [[n,getitemn],[0,[v,a],[]]],
        [[n,getitemn],[1,[v,b],[v,c]],":-",
        [       [[n,head],[[v,b],[v,c]]]
        ]],
        [[n,getitemn],[[v,a],[v,b],[v,c]],":-",
        [       [[n,not],[[[n,=],[[v,a],1]]]],
                [[n,tail],[[v,b],[v,t]]],
                [[n,-],[[v,a],1,[v,d]]],
                [[n,getitemn],[[v,d],[v,t],[v,c]]]
        ]],
        
        [[n,randomfn],[[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,random],[[v,r]]],
                [[n,*],[[v,r],9,[v,n]]],
                [[n,ceiling],[[v,n],[v,n1]]],
                [[n,fna],[[v,n1],[v,a1],[v,a2],[v,b]]]
        ]],

        [[n,fna],[1,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,reverse],[[v,a1],[],[v,b]]]
        ]],
        
        [[n,fna],[2,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,sort0],[[v,a1],[v,b]]]
        ]],
                
        [[n,fna],[3,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,findall1],[[n,dividebyfour],[v,a1],[],[v,b]]]
        ]],
        
        [[n,fna],[4,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,append1],[[v,a1],[v,a2],[v,b]]]
        ]],
        
        [[n,fna],[5,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,findall1],[[n,plusone],[v,a1],[],[v,b]]]
        ]],
        
        [[n,fna],[6,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,findall1],[[n,plustwo],[v,a1],[],[v,b]]]
        ]],
        
        [[n,fna],[7,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,findall1],[[n,multiplybytwo],[v,a1],[],[v,b]]]
        ]],
        
        [[n,fna],[8,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,minus1],[[v,a1],[v,a2],[v,b]]]
        ]],
        
        [[n,fna],[9,[v,a1],[v,a2],[v,b]],":-",
        [
                [[n,intersection1],[[v,a1],[v,a2],[],[v,b]]]
        ]],
        
        [[n,reverse],[[],[v,l],[v,l]]],
        [[n,reverse],[[v,l],[v,m],[v,n]],":-",
        [       [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,wrap],[[v,h],[v,h1]]],
                [[n,append],[[v,h1],[v,m],[v,o]]],
                [[n,reverse],[[v,t],[v,o],[v,n]]]
        ]
        ],

        [[n,sort0],[[v,l],[v,n]],":-",
        [       [[n,sort1],[[v,l],[],[v,n]]]
        ]
        ],
        [[n,sort1],[[],[v,l],[v,l]]],
        [[n,sort1],[[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,maximum],[[v,t],[v,h],[v,m2],[],[v,r]]],
                [[n,wrap],[[v,m2],[v,m3]]],
                [[n,append],[[v,m1],[v,m3],[v,m4]]],
                [[n,sort1],[[v,r],[v,m4],[v,n]]]
        ]
        ],
        [[n,maximum],[[],[v,l],[v,l],[v,r],[v,r]]],
        [[n,maximum],[[v,l],[v,m1],[v,n],[v,r1],[v,r2]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,"->"],[[[n,>=],[[v,m1],[v,h]]],
                        [[[n,=],[[v,m2],[v,m1]]],
                        [[n,wrap],[[v,h],[v,h2]]],
                        [[n,append],[[v,r1],[v,h2],[v,r3]]]],
                        [[[[n,=],[[v,m2],[v,h]]]],
                        [[n,wrap],[[v,m1],[v,m12]]],
                        [[n,append],[[v,r1],[v,m12],[v,r3]]]]]],
                [[n,maximum],[[v,t],[v,m2],[v,n],[v,r3],[v,r2]]]
        ]
        ],

        [[n,map],[[v,f],[],[v,l],[v,l]]],
        [[n,map],[[v,f],[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[v,f],[[v,m1],[v,h],[v,m2]]],
                [[n,map],[[v,f],[v,t],[v,m2],[v,n]]]
        ]
        ],

        [[n,add],[[v,a],[v,b],[v,c]],":-",
        [       [[n,+],[[v,a],[v,b],[v,c]]]
        ]
        ],

        [[n,findall1],[[v,f],[],[v,l],[v,l]]],
        [[n,findall1],[[v,f],[v,l],[v,m1],[v,n]],":-",
        [       [[n,not],[[[n,=],[[v,l],[]]]]],
                [[n,head],[[v,l],[v,h]]],
                [[n,tail],[[v,l],[v,t]]],
                [[n,"->"],[[[v,f],[[v,h],[v,m2]]],
                [       [[n,wrap],[[v,m2],[v,m3]]],
                        [[n,append],[[v,m1],[v,m3],[v,m4]]]
                ],
                [
                        [[n,=],[[v,m1],[v,m4]]]
                ]]],
                [[n,findall1],[[v,f],[v,t],[v,m4],[v,n]]]

        ]
        ],

        [[n,plusone],[[v,a],[v,c]],":-",
        [       [[n,+],[[v,a],1,[v,c]]]
        ]
        ],

        [[n,plustwo],[[v,a],[v,c]],":-",
        [       [[n,+],[[v,a],2,[v,c]]]
        ]
        ],

        [[n,multiplybytwo],[[v,a],[v,c]],":-",
        [       [[n,*],[[v,a],2,[v,c]]]
        ]
        ],

        [[n,dividebyfour],[[v,a],[v,c]],":-",
        [       [[n,/],[[v,a],4,[v,c]]]
        ]
        ],

[[n,intersection1],[[], [v,a], [v,l], [v,l]]],
[[n,intersection1],[[v,l1], [v,l2], [v,l3a], [v,l3]],":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,intersection2],[[v,i1],[v,l2],[],[v,l5]]],
	[[n,append],[[v,l3a],[v,l5],[v,l6]]],
	[[n,intersection1],[[v,l4],[v,l2],[v,l6],[v,l3]]]]],
[[n,intersection2],[[v,a], [], [v,l], [v,l]]],
[[n,intersection2],[[v,i1], [v,l1], [v,l2], [v,l3]], ":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,wrap],[[v,i1],[v,i11]]],
	[[n,append],[[v,l2],[v,i11],[v,l5]]],
	[[n,intersection2],[[v,i1], [v,l4], [v,l5], [v,l3]]]]],
[[n,intersection2],[[v,i1], [v,l1], [v,l2], [v,l3]], ":-",
	[[[n,head],[[v,l1],[v,i2]]],
	[[n,tail],[[v,l1],[v,l4]]],
	[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],
	[[n,intersection2],[[v,i1], [v,l4], [v,l2], [v,l3]]]]],

        [[n,append1],[[v,b],[v,c],[v,a]],":-",
        [
                [[n,append],[[v,b],[v,c],[v,a]]]
        ]
        ],

[[n,minus1],[[v,l], [], [v,l]]],
[[n,minus1],[[v,l1], [v,l2], [v,l3]],":-",
	[[[n,head],[[v,l2],[v,i1]]],
	[[n,tail],[[v,l2],[v,l5]]],
	[[n,delete2],[[v,l1],[v,i1],[],[v,l6]]],
	[[n,minus1],[[v,l6], [v,l5], [v,l3]]]]],
[[n,delete2],[[], [v,a], [v,l], [v,l]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",
	[[[n,head],[[v,l1],[v,i1]]],
	[[n,tail],[[v,l1],[v,l5]]],
	[[n,delete2],[[v,l5],[v,i1],[v,l2],[v,l3]]]]],
[[n,delete2],[[v,l1],[v,i1],[v,l2],[v,l3]],":-",
	[[[n,head],[[v,l1],[v,i2]]],
	[[n,tail],[[v,l1],[v,l5]]],
	[[n,not],[[[n,=],[[v,i1],[v,i2]]]]],
	[[n,wrap],[[v,i2],[v,i21]]],
	[[n,append],[[v,l2],[v,i21],[v,l6]]],
	[[n,delete2],[[v,l5],[v,i1],[v,l6],[v,l3]]]]]
]).


testopen_cases(3,[[n,episode_character],[[v,a]]],

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
         %[[n,=],[[v,item1],[v,item1b]]]],
        [v,ds2]]]]
        ,
         %[[n,=],[[v,item1],[v,item1b]]]],
        [v,ds3]]]
    
        ]]
]).%,[[[[v,a],"success"]]]).



testopen_cases(4,[[n,test]],

[[[n,test],":-",[[[n,findall],[[[v,x1],[v,y1],[v,s]],[[[n,member2],[[1,2],[v,y1]]],[[n,member2],[[a,b],[v,x1]]],[[n,writeln],["Enter cell"]],[[n,writeln],[[v,y1]]],[[n,writeln],[[v,x1]]],[[n,read_string],[[v,s]]]],[v,z]]],[[n,findall],[["<tr>",[v,z1],"</tr>"],[[[n,member2],[[1,2],[v,y2]]],[[n,findall],[["<td>",[v,s2],"</td>"],[[[n,member2],[[a,b],[v,x2]]],[[n,member2],[[v,z],[[v,x2],[v,y2],[v,s2]]]]],[v,z1]]]],[v,z2]]],[[n,equals4],[[v,z3],["<table>",[v,z2],"</table>"]]],[[n,flatten1],[[v,z3],[v,z4]]],[[n,concat_list],[[v,z4],[v,z5]]],[[n,writeln],[[v,z5]]]]],[[n,flatten1],[[v,a],[v,b]],":-",[[[n,flatten2],[[v,a],[],[v,b]]]]],[[n,flatten2],[[],[v,b],[v,b]]],[[n,flatten2],[[v,a],[v,b],[v,c]],":-",[[[n,"->"],[[[n,not],[[[n,"->"],[[[n,equals4],[[v,a],[[v,a1],"|",[v,a2]]]],[[n,true]],[[n,equals4],[[v,a],[]]]]]]],[[n,append],[[v,b],[[v,a]],[v,c]]],[[[n,equals4],[[v,a],[[v,d],"|",[v,e]]]],[[n,flatten2],[[v,d],[v,b],[v,f]]],[[n,flatten2],[[v,e],[v,f],[v,c]]]]]]]],[[n,concat_list],[[v,a1],[v,b]],":-",[[[n,equals4],[[v,a1],[[v,a],"|",[v,list]]]],[[n,concat_list],[[v,a],[v,list],[v,b]]],[[n,cut]]]],[[n,concat_list],[[v,a],[],[v,a]],":-",[[[n,cut]]]],[[n,concat_list],[[v,a],[v,list],[v,b]],":-",[[[n,equals4],[[v,list],[[v,item],"|",[v,items]]]],[[n,stringconcat],[[v,a],[v,item],[v,c]]],[[n,concat_list],[[v,c],[v,items],[v,b]]]]]]).
