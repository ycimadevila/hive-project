:-module('inputs', [player_turn/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%
%   DEVOLVER RESULTADOS  %
%%%%%%%%%%%%%%%%%%%%%%%%%%
player_turn(R):- get_play(R1),length(R1,S), process_play(R1, S, R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   OBTENER LA ENTRADA Y PROCESARLA  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%obtener la jugada del player
get_play(L):- read(X), tokenize(X,L).

%separa el movimiento del jugador para que quede de la forma:
%[Pieza1, Punto de contacto, Pieza2]
tokenize(S,L):- split_string(S, " ", " ", L).

process_play(L,1, R):-
    nth0(0, L, P), 
    describe_piece(P, C,T,N), 
    R = [[C, T, N]].
process_play(L, 3, R):- 
    get_pieces(L,P1,P2),
    describe_piece(P1, C1,T1,N1),
    describe_piece(P2, C2,T2,N2),
    describe_move(L,M),
    R = [[C1,T1,N1],M,[C2,T2,N2]].


%saca de la lista las dos piezas involucradas en el movimiento
get_pieces(L, P1, P2):- nth0(0, L, P1), nth0(2, L, P2).

describe_move(L, M):- nth0(1, L, M1), move_code(M1,M).


%dada una pieza(S) obtiene el color(C), tipo de pieza(P) y el id(N) 
describe_piece(S,C,T,N):- 
    string_chars(S, Ch), 
    nth0(0, Ch, C1),
    color_code(C1,C),
    nth0(1, Ch, T),
    nth0(2, Ch, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   CONVERTIR LOS DATOS A INT   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%convertir el string de movimiento en un código de movimiento
move_code("|*", 1).% derecha
move_code("/*", 2).%derecha abajo
move_code("*\\", 3).%izquierda abajo
move_code("*|", 4).%izquierda
move_code("*/", 5).%izquierda arriba
move_code("\\*", 6).%derecha arriba

%codigo de colores de las fichas
color_code('B', 0).
color_code('W',1).


