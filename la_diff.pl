/*
?- ['pldiff.pl'].

% From files:
?- diff_files('old.txt', 'new.txt', Edits),
   render_unified(Edits, Lines),
   maplist(writeln, Lines).

% From strings:
?- S1 = "a\nb\nc\nf\n",
   S2 = "a\nb\nj\nd\nf\n",
   diff_strings(S1, S2, Edits),
   render_unified(Edits, Lines),
   maplist(writeln, Lines).
*/

/*
:- module(pldiff, [
    diff_files/3,        % +FileA, +FileB, -Edits
    diff_strings/3,      % +StringA, +StringB, -Edits
    diff_lines/3,        % +LinesA, +LinesB, -Edits
    render_unified/2     % +Edits, -LinesOut
]).
*/

diff_lists(A,B,C) :-
	atomic_list_concat(A,'\n',A1),
	atomic_list_concat(B,'\n',B1),
	diff_strings(A1,B1,C),!.

/*  Edits is a list of terms:
      keep(Line)  – identical in A and B
      del(Line)   – present in A, removed in B
      add(Line)   – present in B, inserted vs A
*/

:- use_module(library(readutil)).

%% Public convenience predicates

diff_files(FileA, FileB, Edits) :-
    read_file_to_string(FileA, SA, []),
    read_file_to_string(FileB, SB, []),
    split_string(SA, "\n", "\r", LA0),
    split_string(SB, "\n", "\r", LB0),
    % Remove a possible trailing empty line created by final newline
    strip_trailing_empty(LA0, LA),
    strip_trailing_empty(LB0, LB),
    diff_lines(LA, LB, Edits),!.

diff_strings(SA, SB, Edits) :-
    split_string(SA, "\n", "\r", LA0),
    split_string(SB, "\n", "\r", LB0),
    strip_trailing_empty(LA0, LA),
    strip_trailing_empty(LB0, LB),
    diff_lines(LA, LB, Edits1),
    reverse(Edits1,Edits),!.

%% Core: diff on list of lines

diff_lines(A, B, Edits) :-
    lcs_rows(A, B, Rows),           % dynamic-programming table (all rows)
    backtrack_edits(A, B, Rows, Rev),
    reverse(Rev, Edits).

%% Render to unified-style lines with +/-/space prefixes (no hunk headers)

render_unified(Edits, LinesOut) :-
    maplist(edit_to_line, Edits, LinesOut).

edit_to_line(S, L) :- string_concat(" ", S, L).
edit_to_line([d,S],  L) :- string_concat("-", S, L).
edit_to_line([i,S],  L) :- string_concat("+", S, L).

%% Utility: remove a single trailing empty element (from a final newline)

strip_trailing_empty(L0, L) :-
    (   append(Init, [""], L0)
    ->  L = Init
    ;   L = L0).

/* =======================
   LCS dynamic programming
   ======================= */

% lcs_rows(+A,+B,-Rows)
% Rows is a list of length N+1, each an array (list) of length M+1.
% Rows[I][J] = LCS length of A[1..I], B[1..J].

lcs_rows(A, B, Rows) :-
    length(B, M),
    zeros(M, ZeroRow),
    lcs_build_rows(A, B, ZeroRow, [ZeroRow], RowsRev),
    reverse(RowsRev, Rows).

% Build rows top-to-bottom accumulating in reverse (for tail recursion friendliness)
lcs_build_rows([], _B, _PrevRow, Acc, Acc).
lcs_build_rows([Ai|As], B, PrevRow, Acc, RowsOut) :-
    lcs_build_row_for_ai(Ai, B, PrevRow, CurRow),
    lcs_build_rows(As, B, CurRow, [CurRow|Acc], RowsOut).

% Build one DP row given Ai and previous row
lcs_build_row_for_ai(Ai, B, PrevRow, CurRow) :-
    % CurRow starts with a leading 0 for column 0
    lcs_build_row_cols(Ai, B, PrevRow, [0], CurRow).

lcs_build_row_cols(_Ai, [], _PrevRow, Acc, Row) :- reverse(Acc, Row).
lcs_build_row_cols(Ai, [Bj|Bs], PrevRow, Acc, Row) :-
    % j index advances with traversal over Bs; we need:
    %   up   = PrevRow[j]     (same column)
    %   left = CurRow[j-1]    (last computed in Acc)
    %   diag = PrevRow[j-1]
    % Get positions by list indexing helpers:
    length(Acc, J),            % Acc currently holds j cells (0..j-1), so J is j
    nth0(J, PrevRow, Up),      % PrevRow[j]
    Jm1 is J - 1,
    nth0(Jm1, PrevRow, Diag, _),    % PrevRow[j-1]; safe because Acc has at least 1 (col 0)
    Acc = [Left|_],                 % last computed value is left (for col j-1)
    (   Ai == Bj
    ->  Val is Diag + 1
    ;   Val is max(Up, Left)
    ),
    lcs_build_row_cols(Ai, Bs, PrevRow, [Val|Acc], Row).

% Construct a row of M+1 zeros
zeros(M, Row) :-
    M1 is M + 1,
    length(Row, M1),
    maplist(=(0), Row).

/* ==================
   Backtracking Edits
   ================== */

backtrack_edits(A, B, Rows, Edits) :-
    length(A, N),
    length(B, M),
    bt(N, M, A, B, Rows, [], Edits).

% Accessor for DP cell Rows[I][J] (1-based I/J over items; row/col 0 exist)
cell(Rows, I, J, Val) :-
%trace,
%writeln(cell(Rows, I, J, Val)),
	%Rows1 is Rows,
	%trace,
	I1 is I, J1 is J,
    nth0(I1, Rows, Row),   % row index is 0..N
    nth0(J1, Row, Val).    % col index is 0..M

bt(0, 0, _A, _B, _Rows, Acc, Acc) :- !.
bt(I, 0, A, _B, _Rows, Acc, Edits) :- I > 0, !,
    nth1(I, A, Ai),
    I1 is I - 1,
    bt(I1, 0, A, [], [], [[d,Ai]|Acc], Edits).
bt(0, J, _A, B, _Rows, Acc, Edits) :- J > 0, !,
	J0 is J,
    nth1(J0, B, Bj),
    J1 is J - 1,
    bt(0, J1, [], B, [], [[i,Bj]|Acc], Edits).
bt(I, J, A, B, Rows, Acc, Edits) :-
    nth1(I, A, Ai),
    nth1(J, B, Bj),
    (   Ai == Bj
    ->  I1 is I - 1, J1 is J - 1,
        bt(I1, J1, A, B, Rows, [%keep
        Ai|Acc], Edits)
    ;   cell(Rows, I-1, J, Up),      % Rows[I-1][J]
        cell(Rows, I,   J-1, Left),  % Rows[I][J-1]
        (   J > 0, (I =:= 0 ; Left >= Up)
        ->  % prefer left when equal to bias toward insertions like UNIX diff
            nth1(J, B, Bj2),
            J1 is J - 1,
            bt(I, J1, A, B, Rows, [[i,%bj_to_str
            Bj2]|Acc], Edits)
        ;   % delete from A (move up)
            nth1(I, A, Ai2),
            I1 is I - 1,
            bt(I1, J, A, B, Rows, [[d,%ai_to_str
            Ai2]|Acc], Edits)
        )
    ).

% Ensure terms remain strings (no functor leakage if reused)
bj_to_str(S) :- S = S.
ai_to_str(S) :- S = S.
