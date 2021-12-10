:-module('test',[west/4,south/4,east/4,north/4,surroundings/3]).
:- meta_predicate closure0(2,?,?).
:- meta_predicate closure(2,?,?).

:- meta_predicate closure0(2,?,?,+). % internal

closure0(R_2, X0,X) :-
   closure0(R_2, X0,X, [X0]).

closure(R_2, X0,X) :-
   call(R_2, X0,X1),
   closure0(R_2, X1,X, [X1,X0]).
?

closure0(_R_2, X,X, _).
closure0(R_2, X0,X, Xs) :-
   call(R_2, X0,X1),
   non_member(X1, Xs),
   closure0(R_2, X1,X, [X1|Xs]).

non_member(_E, []).
non_member(E, [X|Xs]) :-
   dif(E,X),
   non_member(E, Xs).



west(X,Y,X1,Y1):- X1 is X-1, Y1 is Y.%izquierdad
south_west(X,Y,X1,Y1):- X1 is X-1 + Y mod 2, Y1 is Y+1.%izquierda abajo
%south(X,Y,X1,Y1):- X1 is X, Y1 is Y-1.
south_east(X,Y,X1,Y1):- X1 is X + Y mod 2, Y1 is Y+1.%derecha abajo 
east(X,Y,X1,Y1):- X1 is X+1, Y1 is Y.%derecha
north_east(X,Y,X1,Y1):- X1 is X + Y mod 2, Y1 is Y-1.%derecha arriba 
%north(X,Y,X1,Y1):- X1 is X, Y1 is Y+1.
north_west(X,Y,X1,Y1):- X1 is X-1 + Y mod 2, Y1 is Y-1.%izquierda arriba


surroundings(X,Y, Z):- west(X,Y,W1,W2), south_east(X,Y,SE1,SE2),
south_west(X,Y,SW1,SW2), east(X,Y,E1,E2), north_east(X,Y,NE1,NE2),
north_west(X,Y,NW1,NW2), Z = [W1,W2,SW1,SW2,SE1,SE2,E1,E2,NE1,NE2,NW1,NW2].

check_conected(L) :- forall((select(S,L,R),member(T,R)), closure(connected(L),S,T)).

isNeighbour(X,Y,X1,Y1) :- west(X,Y,X1,Y1), south_west(X,Y,X1,Y1),south(X,Y,X1,Y1),
south_east(X,Y,X1,Y1), east(X,Y,X1,Y1), north_east(X,Y,X1,Y1),north(X,Y,X1,Y1),north_west(X,Y,X1,Y1).

% DFS con ojo cerrao
connected(L, [Cx, Cy], [Hx, Hy]) :- member([Hx, Hy], L),isNeighbour(Hx, Hy, Cx, Cy).