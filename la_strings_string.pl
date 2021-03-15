string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

