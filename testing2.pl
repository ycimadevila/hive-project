:-module('testing2', [if_name_queen/1, a/1]).
if_name_queen(Name) :-
    (Name =:= "WQ1",
    true);
    (Name =:= "BQ1",
    true);
    true    
.
a(A):-
    A == "AA"
.
