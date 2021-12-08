module('main',[piece/3, position/4, add/1, add/3, move/3]).

meta_predicate piece(?,?,?). %% Name, X, Y
meta_predicate position(?,?,?,?). %% X, Y, Name1, Name2

%% turnos pares para las negras e impares para las blancas
turn(1).
%% 1 si la reina ya esta en el tablero, 0 e.o.c.
is_queen(0).

%% METODOS INTERNOS %%
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
    %% ver si la reina ya esta en el tableto
    %% ver si la colmena no se rompe
    %% ver si la posicion no esta ocupada
    %% ver si el recorrido pertenece a N1
    %% eliminar objeto anterior
    %% agregar objeto nuevo
    false.

%% METODOS AUXILIARES %%
hive_dfs(Name1, Name2) :-
    %% analiza por DFS si la colmena esta desconectada
    false.

is_queen_on_table() :-
    bagof(X, is_queen(X), P), P =:= [1].

surroundings(X, Y, Z) :- 
    move_right(X, Y, Xr, Yr),
    move_up_right(X, T, Xru, Yru),
    move_down_right(X, T, Xdu, Ydu),
    move_left(X, Y, Xl, Yl),
    move_up_left(X, Y, Xlu, Ylu),
    move_down_left(X, Y, Xld, Yld),
    Z = [Xr, Yr, Xru, Yru, Xdu, Ydu, Xl, Yl, Xlu, Ylu, Xld, Yld]
.

%% METODOS DE MOVIMIENTO %%
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

%% POSIBLES POSICIONES %%

%% X: valor de x
%% Y: valor de y
%% Pos: lista de posibles posiciones
queen_bee(X, Y, Pos) :-
    %% la reina solo se mueve a una posicion vacia con un solo paso 
    %% obtener posibles posiciones
    %% comprobar validez (si no desconectan el grafo)
    %% retornarlas
    false.
    
beetle(X, Y, Pos) :-
    %% el escarabajo se mueve un solo paso, (no interesa si la posicion esta vacia o no) 
    %% obtener posibles posiciones
    %% comprobar validez (si no desconectan el grafo)
    %% retornarlas
    false.

ant(X, Y, Pos) :-
    %% puede rodear la colmena, no puede ir a espacios vacios cualesquiera
    %% obtener posibles posiciones
    %% comprobar validez (si no desconectan el grafo)
    %% retornarlas
    false.

spider(X, Y, Pos) :-
    %% se puede mover 3 posisiones, siempre tocando piezas en su trayectoria, manteniendo el contacto con la colmena
    %% obtener posibles posiciones
    %% comprobar validez (si no desconectan el grafo)
    %% retornarlas
    false.

grasshopper(X, Y, Pos):-
    %% caminar en linea recta pasando por encima de las fichas existentes hasta encontrar un hueco
    %% obtener posibles posiciones
    %% comprobar validez (si no desconectan el grafo)
    %% retornarlas
    false.

lady_bug(X, Y, Pos):
    %% se puede mover 3 posisiones, 2 por encima de la colmena, 1 para salir de ella
    %% obtener posibles posiciones
    %% comprobar validez (si no desconectan el grafo)
    %% retornarlas
    false.