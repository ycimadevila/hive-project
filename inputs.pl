:-module('inputs', [get_move/1, describe_piece/4, get_pieces/2, move_code/2]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   OBTENER LA ENTRADA Y PROCESARLA  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%obtener la jugada del player
get_play(L):- read(X), tokenize(X,L).

%separa el movimiento del jugador para que quede de la forma:
%[Pieza1, Punto de contacto, Pieza2]
tokenize(S,L):- split_string(S, " ", " ", L).

%saca de la lista las dos piezas involucradas en el movimiento
get_pieces(L, P1, P2):- nth0(0, L, P1), nth0(2, L, P2).

%dada una pieza(S) obtiene el color(C), tipo de pieza(P) y el id(N) 
describe_piece(S,C,T,N):- 
    string_chars(S, Ch), 
    nth0(0, Ch, C1),%negro = 0, blanco = 1
    nth0(1, Ch, T),
    nth0(2, Ch, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   CONVERTIR LOS DATOS A INT   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%convertir el string de movimiento en un código de movimiento
move_code("|*", 1).% derecha
move_code("/*", 2).%derecha abajo
move_code("*\", 3).%izquierda abajo
move_code("*|", 4).%izquierda
move_code("*/", 5).%izquierda arriba
move_code("\*", 6).%derecha arriba







