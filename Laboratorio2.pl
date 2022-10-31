
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

isPixbit(Pix) :-
    length(Pix, Largo),
    Largo =:= 4,
    getbyIndex(2, Pix, X),
    integer(X).


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

isPixRGB(Pix) :-
    length(Pix, Largo),
    Largo =:= 6.


%pix3: pixhex

pixhex(X,Y,Hex,Depth,Pix):-
    integer(X),
    integer(Y),
    atom(Hex),
    integer(Depth),
    Pix = [X,Y,Hex,Depth], !.

isHex(Pix) :-
    length(Pix, Largo),
    Largo =:= 4,
    getbyIndex(2, Pix, Hex),
    atom(Hex).

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - is bitmap / imageIsBitmap: boolean
%Las funciones se llaman predicados!!!
%Aqui se comprueba que los añadidos sean pixbits

%% Obtener un elemento de una lista por su índice
getbyIndex(0, [X], X) :- !.
getbyIndex(0, [H|_], H) :- !.
getbyIndex(I, [_|T], E) :- 
    NuevoIndex is I-1, 
    getbyIndex(NuevoIndex, T, E), !.


% Recursión
isBitmapAux([]) :- !.
isBitmapAux([H|T]) :-
    isPixbit(H),
    isBitmapAux(T).

% Predicado final que solamente envuelve la recursión
isBitmap(Imagen) :-
    getbyIndex(2, Imagen, Bits),
    isBitmapAux(Bits).

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - is hexmap / imageIsHexmap: boolean

% Recursión
isHexmapAux([]) :- !.
isHexmapAux([H|T]) :-
    isHex(H),
    isHexmapAux(T).

isHexmap(Imagen) :-
    getbyIndex(2, Imagen, Hexs),
    isHexmapAux(Hexs).

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - is rgbmap / imageIsPixmap: boolean

% Recursión
isRGBmapAux([]) :- !.
isRGBmapAux([H|T]) :-
    isPixRGB(H),
    isRGBmapAux(T).

isRGBmap(Imagen) :-
    getbyIndex(2, Imagen, RGBs),
    isRGBmapAux(RGBs).

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - flipH / imageFlipH:

% Descripción: Predicado que invierte el orden de una lista.
% Dominio: Lista X Lista X Lista

% (Anchura - 1) = z
% z - x = Nueva cordenada

restH(Z) :-
    getbyIndex(1, Imagen, Anchura),
    Z is Anchura - 1.

newcoordH(NewX):-
    getbyIndex(0, Pix, X),
    restH(Z),
    NewX is Z - X.

replace(_, _, [], []) :- !.
replace(O, R, [O|T], [R|NT]) :- !, replace(O, R, T, NT).
replace(O, R, [H|T], [H|NT]) :- H /= O, replace(O, R, T, NT).

flipH(Imagen, ImagenH):-
    newcoordH(NewX),
    replace(X, NewX, Pix, NewPix),
    ImagenH = [Altura,Anchura,NewPix].

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - flipV / imageFlipV:

% (Altura - 1) = z
% z - y = Nueva cordenada

restV(Z) :-
    getbyIndex(0, Imagen, Altura),
    Z is Altura - 1.

newcoordV(NewY):-
    getbyIndex(1, Pix, Y),
    restV(Z),
    NewY is Z - Y.

flipV(Imagen, ImagenV):-
    newcoordV(NewY),
    replace(Y, NewY, Pix, NewPix),
    ImagenV = [Altura,Anchura,NewPix].

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - Crop / imageCrop:

% Descripción: Predicado que recorta una imagen en base a las coordenadas de un pixel.
% Dominio: Imagen X Imagen X Entero X Entero X Entero X Entero
% Imagen: Imagen a recortar. Imagen: Imagen recortada. Entero: Coordenada X del pixel. Entero: Coordenada Y del pixel. Entero: Ancho del recorte. Entero: Alto del recorte.

% Recursión
cropAux([], _, _, _, _, []) :- !.

cropAux([H|T], X, Y, W, H, [H|NT]) :-
    getbyIndex(0, H, X),
    getbyIndex(1, H, Y),
    cropAux(T, X, Y, W, H, NT).

cropAux([H|T], X, Y, W, H, NT) :-
    getbyIndex(0, H, X),
    getbyIndex(1, H, Y),
    cropAux(T, X, Y, W, H, NT).

imageCrop(Imagen, ImagenC, X, Y, W, H) :-
    getbyIndex(2, Imagen, Pixs),
    cropAux(Pixs, X, Y, W, H, PixsC),
    ImagenC = [H,W,PixsC].

%------------------------------------------------------------------------------------------------------------------------------

%TDA image - Rotate / imageRotate:

% Descripción: Predicado que rota una imagen en base a un ángulo.
% Dominio: Imagen X Imagen X Entero
% Imagen: Imagen a rotar. Imagen: Imagen rotada. Entero: Ángulo de rotación.

% Recursión
rotateAux([], _, []) :- !.
rotateAux([H|T], Angulo, [H|NT]) :-
    getbyIndex(0, H, X),
    getbyIndex(1, H, Y),
    rotateAux(T, Angulo, NT).

rotateAux([H|T], Angulo, NT) :-
    getbyIndex(0, H, X),
    getbyIndex(1, H, Y),
    rotateAux(T, Angulo, NT).

imageRotate(Imagen, ImagenR, Angulo) :-
    getbyIndex(2, Imagen, Pixs),
    rotateAux(Pixs, Angulo, PixsR),
    ImagenR = [Altura,Anchura,PixsR].


%------------------------------------------------------------------------------------------------------------------------------

%TDA image - imgRGBtoHex / imageRGBtoHex:

% Descripción: Predicado que convierte una imagen RGB a Hex.
% Dominio: Imagen X Imagen
% Imagen: Imagen RGB. Imagen: Imagen Hex.

% Recursión
RGBtoHexAux([], []) :- !.
RGBtoHexAux([H|T], [H|NT]) :-
    isPixRGB(H),
    RGBtoHexAux(T, NT).

RGBtoHexAux([H|T], NT) :-
    isPixRGB(H),
    RGBtoHexAux(T, NT).

imageRGBtoHex(Imagen, ImagenH) :-
    getbyIndex(2, Imagen, Pixs),
    RGBtoHexAux(Pixs, PixsH),
    ImagenH = [Altura,Anchura,PixsH].

%------------------------------------------------------------------------------------------------------------------------------

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


























