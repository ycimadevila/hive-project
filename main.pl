:-module('main',[piece/3, position/4, add/1, add/3, move/3]).
:-use_module(inputs)

meta_predicate piece(?,?,?). %% Name, X, Y
meta_predicate position(?,?,?,?). %% X, Y, Name1, Name2

%% turnos pares para las negras e impares para las blancas
turn(1).
%% 1 si la reina ya esta en el tablero, 0 e.o.c.
wqueen(0).
bqueen(0).

%% piezas que ya se encuentran en el tablero
pieces_on_table([]).

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
piece(a2, 2, 1).
piece(a3, 3, 1).
piece(a4, 4, 1).

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
    assert(piece(Name, 1, 1)),
    assert(position(1, 1, [Name])),
    if_name_queen(Name),
    %% agrega una nueva pieza
    bagof(X, pieces_on_table(X), Pieces),
    append(Pieces, [Name], Pieces_),
    retract(pieces_on_table(Pieces)),
    append(pieces_on_table(Pieces_))
.

add(Name1, Name2, Mov) :-  % analisis por si el turno es el 2do
    bagof(X, turn(X), Y), Y =:= [2],
    %% obtener posicion de Name1
    bagof(X, piece(Name1, X, _), [X1]),
    bagof(Y, piece(Name1, _, Y), [Y1]),
    %% obtener posicion a la que va
    move(X1, Y1, Mov, X1_, X2_),
    %% analizar si es posicion valida (pos vacia)
    bagof(Temp, piece(Temp, X1_, Y1_), Temp1), 
    Temp1 =:= [],
    %% agregar objeto
    assert(piece(Name2, X1_, Y1_)),
    assert(position, X1_, Y1_, [Name2]),
    if_name_queen(Name2).
    %% agrega una nueva pieza
    bagof(X, pieces_on_table(X), Pieces),
    append(Pieces, [Name], Pieces_),
    retract(pieces_on_table(Pieces)),
    append(pieces_on_table(Pieces_))
.

%% Name1 pieza existente
%% Name2 pieza a poner
%% Mov pos a colocar
add(Name1, Name2, Mov) :-  
    %% obtener posicion de Name1
    describe_piece(Name1, Col1, _, _),
    describe_piece(Name2, Col2, _, ),
    Col1 =:= Col2,
    bagof(X, piece(Name1, X, _), [X1]),
    bagof(Y, piece(Name1, _, Y), [Y1]),

    %% analizar si es posicion valida (pos vacia)
    bagof(Temp, piece(Temp, X1, Y1), Temp1), 
    Temp1 =:= [],

    %% analizar si los alrededores de N2 son validos, si son del mismo color
    describe_piece(Name1, Col, _, _),
    bagof(Temp, turn(Temp1), [Turn]),
    Col =:= Turn mod 2,
    check_valid_surrounding_positions(X1, Y1, Col),

    %% analizar si uno de sus parametros X, Y son menores que 1 (expancionar el tablero)
    expand_table(X, Y),
    
    %% agregar objeto
    assert(piece(Name2, X1, Y1)),
    assert(position, X1, Y1, [Name]),

    %% actualiza el turno
    retract(turn(Turn)),
    assert(turn(Turn + 1)),
    if_name_queen(Name2),

    %% agrega una nueva pieza
    bagof(X, pieces_on_table(X), Pieces),
    append(Pieces, [Name], Pieces_),
    retract(pieces_on_table(Pieces)),
    append(pieces_on_table(Pieces_))
.

%% name1 pieza que se mueve
%% name2 pieza guia
%% Mov, posicion del movimiento
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

is_queen_on_table(0) :-
    bagof(X, bqueen(X), P), P =:= [1].

is_queen_on_table(1) :-
    bagof(X, wqueen(X), P), P =:= [1].

add_queen_to_table(0) :-
    bagof(X, bqueen(X), P), P =:= [1]; % Hace cortocircuito si la reina ya esta en el tablero, no la agrega de nuevo
    retract(bqueen(0)),
    assert(bqueen(1)),
.

add_queen_to_table(1) :-
    bagof(X, wqueen(X), P), P =:= [1]; % Hace cortocircuito si la reina ya esta en el tablero, no la agrega de nuevo
    retract(wqueen(0)),
    assert(wqueen(1)),
.

surroundings(X, Y, Z) :- 
    move_right(X, Y, Xr, Yr),
    move_up_right(X, T, Xru, Yru),
    move_down_right(X, T, Xrd, Yrd),
    move_left(X, Y, Xl, Yl),
    move_up_left(X, Y, Xlu, Ylu),
    move_down_left(X, Y, Xld, Yld),
    Z = [Xr, Yr, Xru, Yru, Xrd, Yrd, Xl, Yl, Xlu, Ylu, Xld, Yld]
.

check_valid_surrounding_positions(X, Y, Col):-.
    surroundings(X, Y, [Xr, Yr, Xru, Yru, Xrd, Yrd, Xl, Yl, Xlu, Ylu, Xld, Yld]),
    is_poss_piece(Xr, Yr, Col),
    is_poss_piece(Xru, Yru, Col),
    is_poss_piece(Xrd, Yrd, Col),
    is_poss_piece(Xl, Yl, Col),
    is_poss_piece(Xlu, Ylu, Col),
    is_poss_piece(Xld, Yld, Col),
.

is_poss_piece(X, Y, Col) :-
    bagof(T, piece(T, _, _), Temp_),
    (Temp_ =:= [];
    (nth0(0, Temp_, Temp),
    describe_piece(Temp, Col1, _, _),
    Col =:= Col1))
.

if_name_queen(Name) :-
    (Name =:= 'WQ1',
    add_queen_to_table(1));
    (Name =:= 'BQ1',
    add_queen_to_table(0));
    true    
.

expand_table(X, Y) :-
    expand_x(X);
    expand_y(Y);
    true
.

expand_x(X) :-
    X < 1,
    bagof(X, piece(X, _, _), P)
.

%% METODOS DE MOVIMIENTO %%
move(X, Y, 1, X1, Y1) :- move_right(X, Y, X1, X2). % derecha
move(X, Y, 2, X1, Y1) :- move_down_right(X, Y, X1, X2). % derecha abajo
move(X, Y, 3, X1, Y1) :- move_down_left(X, Y, X1, X2). % izquierda abajo
move(X, Y, 4, X1, Y1) :- move_left(X, Y, X1, X2). % izquierda
move(X, Y, 5, X1, Y1) :- move_up_left(X, Y, X1, X2). % izquierda arriba
move(X, Y, 6, X1, Y1) :- move_up_right(X, Y, X1, X2). % derecha arriba



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

rec([], N, N1).
rec([X|Rx], N, N1) :-
    N1 =:= N;
    write(X), nl,
    N2 is N1 + 1,
    rec(Rx, N, N2)
.


%% POSIBLES POSICIONES %%

%% X: valor de x
%% Y: valor de y
%% Pos: lista de posibles posiciones
queen_bee(X, Y, Pos) :-
    %% la reina solo se mueve a una posicion vacia con un solo paso 
    %% obtener posibles posiciones
    %% comprobar validez
    %% retornarlas
    false.
    
beetle(X, Y, Pos) :-
    %% el escarabajo se mueve un solo paso, (no interesa si la posicion esta vacia o no) 
    %% obtener posibles posiciones
    %% comprobar validez 
    %% retornarlas
    false.

ant(X, Y, Pos) :-
    %% puede rodear la colmena, no puede ir a espacios vacios cualesquiera
    %% obtener posibles posiciones
    %% comprobar validez
    %% retornarlas
    false.

spider(X, Y, Pos) :-
    %% se puede mover 3 posisiones, siempre tocando piezas en su trayectoria, manteniendo el contacto con la colmena
    %% obtener posibles posiciones
    %% comprobar validez 
    %% retornarlas
    false.

grasshopper(X, Y, Pos):-
    %% caminar en linea recta pasando por encima de las fichas existentes hasta encontrar un hueco
    %% obtener posibles posiciones
    %% comprobar validez 
    %% retornarlas
    false.

lady_bug(X, Y, Pos) :-
    %% se puede mover 3 posisiones, 2 por encima de la colmena, 1 para salir de ella
    %% obtener posibles posiciones
    %% comprobar validez 
    %% retornarlas
    false.