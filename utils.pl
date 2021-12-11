:-module('utils', [surroundings/3, move/5]).
:-use_module(library(ugraphs)).

:- dynamic vertex/3.
:- dynamic edge/2.

surroundings(X, Y, Z) :- 
    move_right(X, Y, Xr, Yr),
    move_up_right(X, Y, Xru, Yru),
    move_down_right(X, Y, Xdu, Ydu),
    move_left(X, Y, Xl, Yl),
    move_up_left(X, Y, Xlu, Ylu),
    move_down_left(X, Y, Xld, Yld),
    Z = [Xr, Yr, Xru, Yru, Xdu, Ydu, Xl, Yl, Xlu, Ylu, Xld, Yld]
.

%% METODOS DE MOVIMIENTO %%
move(X, Y, 1, X1, Y1) :- move_right(X, Y, X1, X2). % derecha
move(X, Y, 2, X1, Y1) :- move_down_right(X, Y, X1, X2). % derecha abajo
move(X, Y, 3, X1, Y1) :- move_down_left(X, Y, X1, X2). % izquierda abajo
move(X, Y, 4, X1, Y1) :- move_left(X, Y, X1, X2). % izquierda
move(X, Y, 5, X1, Y1) :- move_up_left(X, Y, X1, X2). % izquierda arriba
move(X, Y, 6, X1, Y1) :- move_up_right(X, Y, X1, X2). % derecha arriba



move_right(X, Y, X1, Y1) :-
    X1 is X+1, 
    Y1 is Y.

move_up_right(X, Y, X1, Y1) :-
    X1 is X + Y mod 2, 
    Y1 is Y-1.

move_down_right(X, Y, X1, Y1) :-
    X1 is X + Y mod 2, 
    Y1 is Y+1.

move_left(X, Y, X1, Y1) :-
    X1 is X-1,
    Y1 is Y.

move_up_left(X, Y, X1, Y1) :-
    T is Y mod 2,
    Y1 is Y - 1,
    X1 is X-1 + T. 

move_down_left(X, Y, X1, Y1) :-
    X1 is X-1 + Y mod 2, 
    Y1 is Y+1.

%% CONSTRUIR UN GRAFO %%

graph(L, G):- 
    vertices_edges_to_ugraph([], [], G),
    build_vertex(L,0,G),

    write("G--->"), writeln(G),

    build_edges(L,G),
    
    writeln(sali)
.

build_vertex([],_,_).
build_vertex([Pos|Tail],Id,G):- 
    Id1 is Id +1,   
    nth0(0, Pos, X),
    nth0(1,Pos,Y),    
    assertz(vertex(X,Y,Id1)),
    add_vertices(G, [Id1], G1),
    build_vertex(Tail,Id1,G1).
    
build_edges([],_).
build_edges([Pos|Tail],G):-
    nth0(0, Pos, X),
    nth0(1,Pos,Y),
    surroundings(X,Y,S),

    write("S-->"), writeln(S), 
    write("Tail-->"), writeln(Tail),

    register_all_surroundings(S,X,Y,G),

    write(registre),

    build_edges(Tail).

register_all_surroundings([],_,_,_).
register_all_surroundings([X1,Y1|Tail],X,Y,G):-
    vertex(X1, Y1, V1),
    vertex(X, Y, V2),
    write("G-->"), writeln(G),
    add_edges(G, [V1-V2,V2-V1], G1),  
    write("G1-->"), writeln(G1),
    register_all_surroundings(Tail,X,Y,G1).
    



