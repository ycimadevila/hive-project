objeto([1, 2, 3]).

inlist(Elem):-
    bagof(X, objeto(X), P),
    nth0(0, P, P1),
    member2(Elem, P1)
.
member2(X, [Y|Ys]) :-
    X = Y ; member2(X, Ys).

a():-
    (false,
    false);
    true
.
b() :-
    true,
    (false;
    (true,
    true,
    false)) %true
.

p():-
    true,
    write(holi),
    false.