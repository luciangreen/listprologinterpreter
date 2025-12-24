:- op(400, xfy, user:(..=)).

/*
=..(f(1,2),A).
A = [f, 1, 2].

B =..([f, 1, 2]).
B = f(1,2).

% see starlog_to_prolog repository
starlog_to_prolog:[f,1,2]..=B.
B = f(1,2).

starlog_to_prolog: ..=([f,1,2],B).
B = f(1, 2).
*/

..=(A,B) :- B=..A,!.
