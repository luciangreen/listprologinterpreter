%% test(Debug[on/off],Total,Score).

test(Debug,N29,S30) :- N22 is 0,S23 is 0,
N23 is N22+1,
%%writeln([eg1]),
((interpret(Debug,[function,[1,1,c]],
[
        [function,[a,b,c],(:-),
        [
                [c,is,a+b]
        ]
        ]
]
,[[c, 2]]) ,S24 is S23 +1)->true;S24 is S23),
N24 is N23+1,
%%writeln([eg2]),
((interpret(Debug,[function,[1,1,c]],
[
        [function,[a,b,c],(:-),
        [
                [d,is,a+b],
                [c,is,d+1]
        ]
        ]
]
,[[c, 3]]), S25 is S24 +1)->true;S25 is S24),
N25 is N24+1,
%%writeln([eg3]),
((interpret(Debug,[function,[1,1,c]],
[
        [function,[a,b,c],(:-),
        [
                [function2,[d,f]],
                [e,is,a+b],
                [g,is,e+f],
                [c,is,g+d]
        ]
        ],
        [function2,[a,f],(:-),
        [
                [a,is,2],
                [f,is,1]
        ]
        ]
]
,[[c, 5]]), S26 is S25 +1)->true;S26 is S25),
N26 is N25+1,
%%writeln([eg4]),
((interpret(Debug,[append1,[a]],
[
        [append1,[a],(:-),
        [
                [b,[b]],
                [c,[c]],
                [append,b,c,a]
        ]
        ],
        [b,["b"]],
        [c,["c"]]
]
,[[a, ["b", "c"]]]), S27 is S26 +1)->true;S27 is S26),
N27 is N26+1,
%%writeln([eg5]),
((
interpret(Debug,[count,[1,n]],
[
        [count,[1,2]],
        [count,[n,p],(:-),
        [
                [m,is,n+1],
                [count,[m,p]]
        ]
        ]
] ,[[n, 2]]),S28 is S27 +1)->true;S28 is S27),
N28 is N27+1,
%%writeln([eg6]),
((
interpret(Debug,[count,[0,n]],
[
        [count,[1,2]],
        [count,[n,p],(:-),
        [
                [m,is,n+1],
                [count,[m,p]]
        ]
        ]
] ,[[n, 2]]),S29 is S28 +1)->true;S29 is S28),
N29 is N28+1,
%%writeln([eg7]),
((
interpret(on,[reverse,[[1,2,3],[],l]],
[
        [reverse,[[],l,l]],
        [reverse,[l,m,n],(:-),
        [       [head,l,h],
                [tail,l,t],
                [append,h,m,o],
                [reverse,[t,o,n]]
        ]
        ]
],[[l, [3, 2, 1]]]),S30 is S29 +1)->true;S30 is S29).
