% ------------------------------------------------------------
% |TDA: Pixrgb                                               |
% ------------------------------------------------------------

% Descripción: Predicado que permite obtener la estrucura de un pixrgb. Este TDA se utiliza para representar una imagen en formato
% RGB. Representado a traves de una lista que alamacena diferentes elementos de diferentes caracteristicas.
% X -> Coordenada X del pixel dentro de un plano cartesiano. Entero.
% Y -> Coordenada Y del pixel dentro de un plano cartesiano. Entero.
% R -> Valor del color rojo del pixel. Entero.
% G -> Valor del color verde del pixel. Entero.
% B -> Valor del color azul del pixel. Entero.
% Depth -> Profundidad de la imagen. Entero.
% Se debe tener en cuenta que los colores RGB se representan en un rango de 0 a 255.

/* Predicados:
    - c(X)                                                              Aridad: 1.
    - pixrgb(X, Y, R, G, B, Depth, Pix)                                 Aridad: 7.
    - isPixRGB(Pix)                                                     Aridad: 1.
    - byteToHex(Byte, Hex)                                              Aridad: 2.
    - rgbToHex([H|T], [H1|T1])                                          Aridad: 2.
*/

/* Metas primarias:
    - pixrgb(X, Y, R, G, B, Depth, Pix)                  
*/

/* Metas secundarias:
    - c(X)                                                   
    - isPixRGB(Pix)
    - byteToHex.
    - rgbToHex.                                                  
*/

% Clausulas:

% Reglas: 

% Predicados:

% Dominio: Entero X Entero X Entero X Entero X Pixrgb.
pixrgb(X,Y,R,G,B,Depth,Pix):-
    integer(X),
    integer(Y),
    c(R),
    c(G),
    c(B),
    integer(Depth),
    Pix = [X,Y,R,G,B,Depth], !.

% Descripción: Predicado que permite confirmar si un elemento X se encuentra dentro del rango 0 < X < 255.
% Dominio: Entero.
c(X):- integer(X), X >= 0, X =< 255.

% Descripción: Predicado que permite confirmar si un elemento es un pixrgb.
% Dominio: Pix.
isPixRGB(Pix) :-
    length(Pix, Largo),
    Largo =:= 6.

% Se creara los casos bases para un predicado que permita tranformar un decimal a Hex.
byteToHex(0, 0) :- !.
byteToHex(1, 1) :- !.
byteToHex(2, 2) :- !.
byteToHex(3, 3) :- !.
byteToHex(4, 4) :- !.
byteToHex(5, 5) :- !.
byteToHex(6, 6) :- !.
byteToHex(7, 7) :- !.
byteToHex(8, 8) :- !.
byteToHex(9, 9) :- !.
byteToHex(10, 'A') :- !.
byteToHex(11, 'B') :- !.
byteToHex(12, 'C') :- !.
byteToHex(13, 'D') :- !.
byteToHex(14, 'E') :- !.
byteToHex(15, 'F') :- !.

% Descrición: Predicado que permite transformar un decimal a Hex.
% Dominio: Entero X Entero
byteToHex(Decimal, Hex) :-
    Decimal > 15,
    Div is Decimal / 16,
    Decimal1 is floor(Div),
    Decimal2 is (Div - Decimal1) * 16,
    round(Decimal2, Decimal3),
    byteToHex(Decimal1, Hex1),
    byteToHex(Decimal3, Hex2),
    atom_concat(Hex1, Hex2, Hex).

% Descripción: Predicado que envuelve la recursión. Transforma una lista de bytes RGB a Hex.
% Dominio: Lista X Lista
rgbToHex([], []) :- !.
rgbToHex([H|T], [H1|T1]) :-
    getbyIndex(0, H, X),
    getbyIndex(1, H, Y),
    getbyIndex(2, H, R),
    getbyIndex(3, H, G),
    getbyIndex(4, H, B),
    getbyIndex(5, H, Depth),
    byteToHex(R, R1),
    byteToHex(G, G1),
    byteToHex(B, B1),
    atom_concat(R1, G1, RG),
    atom_concat(RG, B1, Hex),
    pixhex(X, Y, Hex, Depth, H1),
    rgbToHex(T, T1).