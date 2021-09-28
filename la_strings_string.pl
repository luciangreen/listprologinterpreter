string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

word1([])-->[].
word1([A|As]) --> [A],word1(As),{%%atom_codes(A,AC),
char_type(A,alpha)},!.
