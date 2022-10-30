%% Obtener un elemento de una lista por su índice
getbyIndex(0, [X], X) :- !.
getbyIndex(0, [H|_], H) :- !.
getbyIndex(I, [_|T], E) :- NuevoIndex is I-1, getbyIndex(NuevoIndex, T, E), !.



%TD1: Constructor1
imagen(Altura,Anchura,Pix,Imagen):-
    integer(Altura),
    integer(Anchura),
    Imagen = [Altura,Anchura,Pix].

%Pix necesarios:
%Se utiliza una variable anonima para llamar cuantas veces se quiera.

%pix1: pixbit
pixbit(X,Y,Bit,Depth,Pix):-
    integer(X),
    integer(Y),
    integer(Bit),
    (Bit =:= 1; Bit =:= 0),
    integer(Depth),
    Pix = [X,Y,Bit,Depth], !.

%pix2: pixrgb

c(X):- integer(X), X >= 0, X =< 255.

pixrgb(X,Y,R,G,B,Depth,Pix):-
    integer(X),
    integer(Y),
    c(R),
    c(G),
    c(B),
    integer(Depth),
    Pix = [X,Y,R,G,B,Depth], !.

%pix3: pixhex

pixhex(X,Y,Hex,Depth,Pix):-
    integer(X),
    integer(Y),
    atom(Hex),
    integer(Depth),
    Pix = [X,Y,Hex,Depth], !.

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - is bitmap:
%Las funciones se llaman predicados!!!
%Aqui se comprueba que los añadidos sean pixbits

isPixbit(Pix) :-
    length(Pix, Largo),
    Largo =:= 4,
    getbyIndex(2, Pix, X),
    integer(X).

isHex(Pix) :-
    length(Pix, Largo),
    Largo =:= 4,
    getbyIndex(2, Pix, Hex),
    atom(Hex).

isPixRGB(Pix) :-
    length(Pix, Largo),
    Largo =:= 6.



% Recursión
isBitmapAux([]) :- !.
isBitmapAux([H|T]) :-
    isPixbit(H),
    isBitmapAux(T).

% Predicado final que solamente envuelve la recursión
isBitmap(Imagen) :-
    getbyIndex(2, Imagen, Bits),
    isBitmapAux(Bits).




% Ejemplos de uso
ejemploImage(X) :-
    pixbit(0, 0, 1, 10, PA),
    pixbit(0, 1, 0, 20, PB),
    pixbit(1, 0, 0, 30, PC),
    pixbit(1, 1, 1, 4, PD),
    imagen(2, 2, [PA, PB, PC, PD], X).

ejemploisBitMap() :-
    ejemploImage(X),
    isBitmap(X).

ejemploIsPixBit() :-
    pixbit(0, 0, 1, 10, PA),
    isPixbit(PA).


























