%:- module('main',[addfirst/1, add/3, move/3, if_name_queen/1]).

:- dynamic add_queen_to_table/1.
:- dynamic piece/3.
:- dynamic position/3. % X,Y,lista
:- dynamic turn/1.
:- dynamic wqueen/1.
:- dynamic bqueen/1.
:- dynamic pieces_on_table/1.
:- dynamic queen_surrounding/1.
:- dynamic beetle_surrounding/1.

:- use_module(utils).

describe_piece(S,C,T,N):- 
    string_chars(S, Ch), 
    nth0(0, Ch, C1),
    color_code(C1,C),
    nth0(1, Ch, T),
    nth0(2, Ch, N).
color_code('B', 0).
color_code('W',1).

% meta_predicate piece(?,?,?). %% Name, X, Y
% meta_predicate position(?,?,?,?). %% X, Y, Name1, Name2


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
addfirst(Name) :- 
    %% turnos pares para las negras e impares para las blancas
    assert(turn(1)),

    %% 1 si la reina ya esta en el tablero, 0 e.o.c.
    assert(wqueen(0)),
    assert(bqueen(0)),
    
    %% piezas que ya se encuentran en el tablero
    assert(pieces_on_table([])),

    %% analizar si es el turno 1
    findall(X, turn(X), Y), Y =:= [1],

    %% agregar objeto
    assert(piece(Name, 0, 0)),
    assert(position(0, 0, [Name])),
    if_name_queen(Name),

    %% agrega una nueva pieza
    findall(X, pieces_on_table(X), [Pieces]),
    append(Pieces, [Name], Pieces_),
    retract(pieces_on_table(Pieces)),
    assert(pieces_on_table(Pieces_)),
    retract(turn(1)),
    assert(turn(2))
.
% analisis por si el turno es el 2do
add(Name1, Name2, Mov) :-  
    findall(X, turn(X), Y), Y =:= [2],
    %% obtener posicion de Name1
    findall(X, piece(Name1, X, _), X1_),
    findall(X, piece(Name1, _, X), Y1_),
    
    nth0(0, X1_, X1),
    nth0(0, Y1_, Y1),

    %% obtener posicion a la que va
    move(X1, Y1, Mov, X_, Y_),

    %% analizar si es posicion valida (pos vacia)
    findall(Temp, piece(Temp, X_, Y_), Temp1), 
    Temp1 == [],

    %% agregar objeto
    assert(piece(Name2, X_, Y_)),
    assert(position(X_, Y_, [Name2])),
    
    if_name_queen(Name2),

    %% agrega una nueva pieza
    findall(X, pieces_on_table(X), [Pieces]),
    append(Pieces, [Name2], Pieces_),
    retract(pieces_on_table(Pieces)),
    assert(pieces_on_table(Pieces_)),
    retract(turn(2)),
    assert(turn(3))
.


%% Name1 pieza existente
%% Name2 pieza a poner
%% Mov pos a colocar
add(Name1, Name2, Mov) :-  
    %% obtener posicion de Name1
    describe_piece(Name1, Col1, _, _),
    describe_piece(Name2, Col2, _, _),
    Col1 =:= Col2,
    findall(X, piece(Name1, X, _), X1_),
    findall(X, piece(Name1, _, X), Y1_),
    
    nth0(0, X1_, X1),
    nth0(0, Y1_, Y1),
    
    %% obtener posicion a la que va
    move(X1, Y1, Mov, X_, Y_),

    %% analizar si es posicion valida (pos vacia)
    findall(Temp, piece(Temp, X_, Y_), Temp1), 
    Temp1 =:= [],

    %% analizar si los alrededores de N2 son validos, si son del mismo color
    describe_piece(Name1, Col, _, _),
    findall(Temp, turn(Temp1), [Turn]),
    Col =:= Turn mod 2,
    check_valid_surrounding_positions(X1, Y1, Col),
    
    %% agregar objeto
    write(agrego),
    assert(piece(Name2, X1, Y1)),
    write(agrego),
    assert(position( X1, Y1, [Name])),

    %% actualiza el turno
    retract(turn(Turn)),
    assert(turn(Turn + 1)),
    if_name_queen(Name2),

    %% agrega una nueva pieza
    findall(X, pieces_on_table(X), Pieces),
    append(Pieces, [Name], Pieces_),
    retract(pieces_on_table(Pieces)),
    append(pieces_on_table(Pieces_)),
    findall(X, turn(X), [X_]),
    retract(turn(X_)),
    Xt = X_ + 1,
    assert(Xt)
.

%% name1 pieza guia
%% name2 pieza que se mueve
%% Mov, posicion del movimiento
move(Name1, Name2, Mov, X, Y) :-
    X = 1,
    Y = 0,
    %% ver si la reina ya esta en el tableto
    describe_piece(Name2, Col, _, _),
    is_queen_on_table(Col),

    %% ver si la colmena no se rompe
    true,

    %% obtener posicion
    true,

    %% ver si la posicion no esta ocupada y si lo esta ver si la ficha no es un escarabajo
    %% ver si el recorrido pertenece a N1
    %% eliminar objeto anterior
    %% agregar objeto nuevo
    false
.

%% METODOS AUXILIARES %%
hive_dfs(Name1, Name2) :-
    %% analiza por DFS si la colmena esta desconectada
    false.

is_queen_on_table(0) :-
    findall(X, bqueen(X), P), P =:= [1].

is_queen_on_table(1) :-
    findall(X, wqueen(X), P), P =:= [1].

add_queen_to_table(Val) :-
    Val =:= 0,
    findall(X, bqueen(X), P), P =:= [1]; % Hace cortocircuito si la reina ya esta en el tablero, no la agrega de nuevo
    retract(bqueen(0)),
    assert(bqueen(1))
.

add_queen_to_table(Val) :-
    Val =:= 1,
    findall(X, wqueen(X), P), P =:= [1]; % Hace cortocircuito si la reina ya esta en el tablero, no la agrega de nuevo
    retract(wqueen(0)),
    assert(wqueen(1))
.


check_valid_surrounding_positions(X, Y, Col):-
    surroundings(X, Y, [Xr, Yr, Xru, Yru, Xrd, Yrd, Xl, Yl, Xlu, Ylu, Xld, Yld]),
    is_poss_piece(Xr, Yr, Col),
    is_poss_piece(Xru, Yru, Col),
    is_poss_piece(Xrd, Yrd, Col),
    is_poss_piece(Xl, Yl, Col),
    is_poss_piece(Xlu, Ylu, Col),
    is_poss_piece(Xld, Yld, Col)
.

is_poss_piece(X, Y, Col) :-
    findall(T, piece(T, _, _), Temp_),
    (Temp_ =:= [];
    (nth0(0, Temp_, Temp),
    describe_piece(Temp, Col1, _, _),
    Col =:= Col1))
.

is_poss_empty(X, Y, Col) :-
    findall(T, piece(T, _, _), Temp_),
    (Temp_ =:= [])
.

if_name_queen(Name) :-
    (Name == "WQ1",
    add_queen_to_table(1));
    (Name == "BQ1",
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
    findall(X, pieces_on_table(X), List),
    iter_list(1, 0, List)
.
expand_y(Y) :-
    Y < 1,
    findall(Y, pieces_on_table(Y), List),
    iter_list(0, 1, List)
.

iter_list(_, _, []).
iter_list(X, Y, [Item|List]):-
    findall([X1,Y1], piece(Item, X1, Y1), [Name_Temp]),
    nth0(0, Name_Temp, Xt),
    nth0(1, Name_Temp, Yt),
    X_ is Xt + X,
    Y_ is Yt + Y,
    assert(piece(Item, X_, Y_)),
    retract(piece(Item, X_, Y_)),
    iter_list(X, Y, List)
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

selector(Val, X, Y, Pos):-
    (Val == 'Q',
    queen(X, Y, Pos));
    (Val == 'B',
    beetle(X, Y, Pos));
    (Val == 'A',
    ant(X, Y, Pos));
    (Val == 'G',
    grasshopper(X, Y, Pos));
    (Val == 'S',
    spider(X, Y, Pos));
    (Val == 'L',
    lady_bug(X, Y, Pos))
.

%% X: valor de x
%% Y: valor de y
%% Pos: lista de posibles posiciones
queen(X, Y, Pos) :-
    %% Reina
    %% la reina solo se mueve a una posicion vacia con un solo paso 
    %% obtener posibles posiciones
    %% comprobar validez(sin la regla de una colmena)
    %% retornarlas  
    surroundings(X,Y,Pos1),
    iter_queen_surrounding(Pos1,Pos2),
    queen_surrounding(Pos),
    retract(queen_surrounding(Pos)).

iter_queen_surrounding([], Ans):-
    assertz(queen_surrounding(Ans)).
iter_queen_surrounding([X,Y|Tail], Ans):-
    (not(position(X,Y,_)),    
    Pos = [[X,Y]],
    append(Ans,Pos,Ans1),
    iter_queen_surrounding(Tail, Ans1));
    iter_queen_surrounding(Tail, Ans1).
        

beetle(X, Y, Pos) :-
    %% Escarabajo
    %% el escarabajo se mueve un solo paso, (no interesa si la posicion esta vacia o no) 
    %% obtener posibles posiciones
    %% comprobar validez(sin regla de una colmenaa)
    %% retornarlas
    surroundings(X,Y,Pos1),  
    iter_beetle_surrounding(Pos1,Pos2),
    beetle_surrounding(Pos),
    retract(beetle_surrounding(Pos)).

iter_beetle_surrounding([], Ans):-
    assertz(beetle_surrounding(Ans)).
iter_beetle_surrounding([X,Y|Tail], Ans):- 
    Pos = [[X,Y]],
    append(Ans,Pos,Ans1),
    writeln(Ans1),
    iter_beetle_surrounding(Tail, Ans1).

ant(X, Y, Pos) :-
    %% Hormiga
    %% puede rodear la colmena, no puede ir a espacios vacios cualesquiera
    %% obtener posibles posiciones
    %% comprobar validez
    %% retornarlas
    false.

spider(X, Y, Pos) :-
    %% Araña
    %% se puede mover 3 posisiones, siempre tocando piezas en su trayectoria, manteniendo el contacto con la colmena
    %% obtener posibles posiciones
    %% comprobar validez 
    %% retornarlas
    false.

grasshopper(X, Y, Pos):-
    %% Saltamontes
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