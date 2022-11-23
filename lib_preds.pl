lib_preds1(
[
[[
[[n,string_concat],[[t,string],[t,string],[t,string]]
]
],
[
[[n,string_concat],[input,input,intut]],
[[n,string_concat],[input,input,output]],
[[n,string_concat],[input,output,input]],
[[n,string_concat],[output,input,input]],
[[n,string_concat],[output,output,input]]
],
[
[[n,string_concat],[[v,a],[v,b],[v,c]],":-",
[
	[[n,"->"],
	[
		[[n,"->"],
		[
			[
			[[n,var],[[v,a]]],
			[[n,not],[[[n,var],[[v,b]]]]],
			[[n,var],[[v,c]]]
			],

			[[n,true]],

			[
			[
			[[n,not],[[[n,var],[[v,a]]]]],
			[[n,var],[[v,b]]],
			[[n,var],[[v,c]]]
			]
			]
		]],

		[[n,fail]],

		[
		[[n,string_chars1_full],[[v,a],[v,a1]]],
		[[n,string_chars1_full],[[v,b],[v,b1]]],
		[[n,string_chars1_full],[[v,c],[v,c1]]],
		%[[n,writeln],[[v,c1]]],
		%[[n,trace2]],
		[[n,append],[[v,a1],[v,b1],[v,c1]]],
		[[n,string_chars1_full],[[v,a],[v,a1]]],
		[[n,string_chars1_full],[[v,b],[v,b1]]],
		[[n,string_chars1_full],[[v,c],[v,c1]]]
		]
	]]
]]
]],

[[
[[n,string_chars1_full],[[t,string],{[t,atom]}]]
],
[
[[n,string_chars1_full],[input,input]],
[[n,string_chars1_full],[output,input]],
[[n,string_chars1_full],[input,output]],
[[n,string_chars1_full],[output,output]]
],
[
[[n,string_chars1_full],[[v,a],[v,b]],":-",
[

	[[n,"->"],
	[[

	[[n,var],[[v,a]]],
	[[n,var],[[v,b]]]
	],
	[[n,true]],
	
	[[n,string_chars],[[v,a],[v,b]]]
	]]
]]
]
],

[[
[[n,stringconcat],[[t,string],[t,string],[t,string]]
]
],
[
[[n,stringconcat],[input,input,intut]],
[[n,stringconcat],[input,input,output]],
[[n,stringconcat],[input,output,input]],
[[n,stringconcat],[output,input,input]],
[[n,stringconcat],[output,output,input]]
],
[
[[n,stringconcat],[[v,a],[v,b],[v,c]],":-",
[
	[[n,string_concat],[[v,a],[v,b],[v,c]]]
]]
]],

[[
[[n,subtract],[{[t,any]},{[t,any]},{[t,any]}]]
],
[
[[n,subtract],[input,input,output]],
[[n,subtract],[input,input,input]]
],
[
[[n,subtract],[[v,c],[],[v,c]],":-",
[
	[[n,true%cut
	]]
]],
[[n,subtract],[[v,a],[[v,b1],"|",[v,b2]],[v,e]],":-",
[
	[[n,delete],[[v,a],[v,b1],[v,d]]],
	[[n,subtract],[[v,d],[v,b2],[v,e]]]
]]
]],

[[
[[n,delete],[{[t,any]},[t,any],{[t,any]}]]
],
[
[[n,delete],[input,input,output]],
[[n,delete],[input,input,input]]
],
[
[[n,delete],[[v,a],[v,b],[v,c]],":-",
[
	[[n,delete],[[v,a],[v,b],[],[v,c]]]
		,[[n,%true%
	cut
	]]

]],

[[n,delete],[[],[v,'_'],[v,c],[v,c]],":-",
[
	[[n,true%
	%cut
	]]
]],
[[n,delete],[[[v,a1],"|",[v,a2]],[v,a1],[v,c],[v,d]],":-",
[
	[[n,delete],[[v,a2],[v,a1],[v,c],[v,d]]],
	[[n,true%
	%cut
	]]
]],
[[n,delete],[[[v,a1],"|",[v,a2]],[v,b],[v,c],[v,d]],":-",
[
	[[n,delete],[[v,a2],[v,b],[[v,a1],"|",[v,c]],[v,d]]],
	[[n,true%
	%cut
	]]
]]
]],
/*
[[n,intersection],[[v,a],[v,b],[v,c]],":-",
[
	[[n,findall],
	[
		[v,d],

		[
		[[n,member],[[v,d],[v,a]]],
		[[n,member],[[v,d],[v,b]]]
		],

		[v,c]
	]]
]],
*/
[[
[[n,intersection],[{[t,any]},{[t,any]},{[t,any]}]]
],
[
[[n,intersection],[input,input,output]],
[[n,intersection],[input,input,input]]
],
[
[[n,intersection],[[v,a],[v,b],[v,c]],":-",
[
	[[n,intersection],[[v,a],[v,b],[],[v,c]]],
	[[n,cut]]
]],
[[n,intersection],[[],[v,'_'],[v,a],[v,a]],":-",
[
	[[n,cut]]
]],
[[n,intersection],[[[v,a1],"|",[v,a2]],[v,b],[v,c],[v,d]],":-",
[
	[[n,member1_full],[[v,a1],[v,b]]],
	[[n,intersection],[[v,a2],[v,b],[[v,a1],"|",[v,c]],[v,d]]]
]],
[[n,intersection],[[[v,'_'],"|",[v,a2]],[v,b],[v,c],[v,d]],":-",
[
	[[n,intersection],[[v,a2],[v,b],[v,c],[v,d]]]
]]
]],

[[
[[n,member1_full],[[t,any],{[t,any]}]]
],
[
[[n,member1_full],[input,input]],
[[n,member1_full],[output,input]],
[[n,member1_full],[input,output]]
],
[
[[n,member1_full],[[v,a],[[v,a],"|",[v,'_']]],":-",
[
	[[n,true%cut
	]]
]],
[[n,member1_full],[[v,a],[[v,b],"|",[v,b2]]],":-",
[
	[[n,not],[[[n,equals4],[[v,a],[v,b]]]]],
	[[n,member1_full],[[v,a],[v,b2]]] 
]]
]]
,

[[
[[n,member],[{[t,any]},[t,any]]]
],
[
[[n,member],[input,input]],
[[n,member],[output,input]],
[[n,member],[input,output]]
],
[
[[n,member],[[v,a],[v,b]],":-",
[
	[[n,member1_full],[[v,b],[v,a]]] % swapped
]]
]],

[[
[[n,member2],[{[t,any]},[t,any]]]
],
[
[[n,member2],[input,input]],
[[n,member2],[output,input]],
[[n,member2],[input,output]]
],
[
[[n,member2],[[v,a],[v,b]],":-",
[
	[[n,member1_full],[[v,b],[v,a]]] % swapped
]]
]],

[[
[[n,member3],[{[t,any]},[t,any]]]
],
[
[[n,member3],[input,input]],
[[n,member3],[output,input]],
[[n,member3],[input,output]]
],
[
[[n,member3],[[v,a],[v,b]],":-",
[
	[[n,member1_full],[[v,b],[v,a]]] % swapped
]]
]],

[[
[[n,append],[{[t,any]},{[t,any]},{[t,any]}]]
],
[
[[n,append],[input,input,input]],
[[n,append],[input,input,output]],
[[n,append],[output,input,input]],
[[n,append],[input,output,input]],
[[n,append],[output,output,input]]
],
[
[[n,append],[[],[v,a],[v,a]],":-",[[[n,true]]]],
[[n,append],[[[v,h],"|",[v,d]],[v,b],[[v,h],"|",[v,c]]],":-",
[
	[[n,append],[[v,d],[v,b],[v,c]]]
]]
]]
]
).

lib_preds([Types,Modes,Preds]) :-

 lib_preds1(Lib_preds1),
 findall(Types1,member([Types1,_,_],Lib_preds1),Types2), foldr(append,Types2,Types),
 findall(Modes1,member([_,Modes1,_],Lib_preds1),Modes2), foldr(append,Modes2,Modes),
 findall(Preds1,member([_,_,Preds1],Lib_preds1),Preds2), foldr(append,Preds2,Preds).