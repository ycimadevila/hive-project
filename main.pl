module('main',[piece/3, position/4, add/1, add/3, move/3]).

meta_predicate piece(?,?,?). %% Name, X, Y
meta_predicate position(?,?,?,?). %% X, Y, Name1, Name2

turn(1).

add_piece(Name, X, Y) :-
    assert(piece(Name, X, Y)).

remove_piece(Name, X, Y):-
    retract(piece(Name, X, Y)).

add_position(X, Y, Name1, Name2) :-
    assert(position(X, Y, Name1, Name2)).

remove_position(X, Y, Name1, Name2) :-
    retract(position(0X, Y, Name1, Name2)). 

%% print the piece
print_piece([]) :- nl.
print_piece([Name|N]):- write(" <"), write(Name), write("> "), print_piece(N).

%% prints a list of elements
print_row([]) :- nl.
print_row([Tile | Tiles]) :- write(Tile), write(" "), print_row(Tiles).

%%% random objects
piece(a1, 1, 1).
piece(a2, 1, 1).
piece(a3, 1, 1).
piece(a4, 1, 1).

position(1, 2, e1, e3).
position(1, 3, e1, e2).
position(1, 4, e1, e4).
position(1, 2, e2, e1).

%% test print
wr([]):- nl.
wr([X:Y| List]) :- write(X), write("-->"), write(Y), nl, wr(List).

get_movement([]) :- nl.
get_movement([Str, S], I) :- 
    I1 is I+1, 
    get_movement(S, I1).

move(Name1, Mov, Name2) :-
    false.


%%% METODOS DE INSERCION %%%
add(Name) :- 
    %% analizar si es el turno 1
    bagof(X, turn(X), Y), Y =:= [1],
    %% agregar objeto
    assert(piece(Name, X, Y)).

add(Name1, Name2, Mov) :- 
    %% analizar si es posicion valida
    %% analizar si son del mismo color N1 y N2
    %% analizar si los alrededores de N2 son validos
    %% agregar objeto
    false.

move(Name1, Name2, Mov) :-
    %% ver si la colmena no se rompe
    %% ver si la posicion no esta ocupada
    %% ver si el recorrido pertenece a N1
    %% eliminar objeto anterior
    %% agregar objeto nuevo
    false.

%% metodos auxiliares %%
hive_dfs(Name1, Name2) :-
    %% analiza por DFS si la colmena esta desconectada
    false.

%% Metodos de Movimiento
move_right(X, Y, X1, X2) :-
    X1 is X + 1,
    Y1 is Y.

move_up_right(X, Y, X1, X2) :-
    X1 is X + 1,
    Y1 is Y - 1.

move_down_right(X, Y, X1, X2) :-
    X1 is X + 1,
    Y1 is Y + 1.

move_left(X, Y, X1, X2) :-
    X1 is X - 1,
    Y1 is Y.

move_up_left(X, Y, X1, X2) :-
    X1 is X - 1,
    Y1 is Y - 1.

move_down_left(X, Y, X1, X2) :-
    X1 is X - 1,
    Y1 is Y + 1.