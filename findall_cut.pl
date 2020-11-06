%% ?- findall1([[1,2,3],[4,5,6]],B).
%% B = [[1, 2, 3]].

        findall1(A,B) :-
        	findall(B1,(member(A1,A), !,
        
        findall(A2,member(A2,A1),
                B1)),
                B).