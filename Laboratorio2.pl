%2 - TD1: Constructor1
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

%3 - TDA image - is bitmap / imageIsBitmap: boolean
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

%4 - TDA image - is rgbmap / imageIsPixmap: boolean

% Recursión
isRGBmapAux([]) :- !.
isRGBmapAux([H|T]) :-
    isPixRGB(H),
    isRGBmapAux(T).

isRGBmap(Imagen) :-
    getbyIndex(2, Imagen, RGBs),
    isRGBmapAux(RGBs).

%------------------------------------------------------------------------------------------------------------------------------

%5 - TDA image - is hexmap / imageIsHexmap: boolean

% Recursión
isHexmapAux([]) :- !.
isHexmapAux([H|T]) :-
    isHex(H),
    isHexmapAux(T).

isHexmap(Imagen) :-
    getbyIndex(2, Imagen, Hexs),
    isHexmapAux(Hexs).

%------------------------------------------------------------------------------------------------------------------------------

%6 - TDA image - flipH / imageFlipH:

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

%7 - TDA image - flipV / imageFlipV:

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

%8 - TDA image - Crop / imageCrop:

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

imageCrop(Imagen, X, Y, W, H, ImagenC) :-
    getbyIndex(2, Imagen, Pixs), 
    cropAux(Pixs, X, Y, W, H, PixsC),
    ImagenC = [H,W,PixsC].

%------------------------------------------------------------------------------------------------------------------------------

%9 - TDA image - Rotate / imageRotate90:

% Descripción: Predicado que rota una imagen en base a un ángulo.
% Dominio: Imagen X Imagen
% Imagen: Imagen a rotar. Imagen: Imagen rotada. Entero: Ángulo de rotación.

% Rotación en 90° es (x,y) = (-y,x).

rotate90(Imagen, ImagenR) :-
    getbyIndex(2, Imagen, Pixs),
    getbyIndex(0, Pixs, OldX),
    getbyIndex(1, Pixs, OldY),
    NewX is -OldY,
    NewY is OldX,
    replace(OldX, NewX, Pixs, NewPixs),
    replace(OldY, NewY, NewPixs, PixsR),
    ImagenR = [Altura,Anchura,PixsR].

% Se añaade la rotación en 180° y 270° para agregar predicados de mayor complejidad. Estos no son necesarios para el funcionamiento 
% del programa, pero se agregan para tener una mayor variedad de predicados.
% Rotación en 180° es (x,y) = (-x,-y).

rotate180(Imagen, ImagenR) :-
    getbyIndex(2, Imagen, Pixs),
    getbyIndex(0, Pixs, OldX),
    getbyIndex(1, Pixs, OldY),
    NewX is -OldX,
    NewY is -OldY,
    replace(OldX, NewX, Pixs, NewPixs),
    replace(OldY, NewY, NewPixs, PixsR),
    ImagenR = [Altura,Anchura,PixsR].

% Rotación en 270° es (x,y) = (y,-x).

rotate270(Imagen, ImagenR) :-
    getbyIndex(2, Imagen, Pixs),
    getbyIndex(0, Pixs, OldX),
    getbyIndex(1, Pixs, OldY),
    NewX is OldY,
    NewY is -OldX,
    replace(OldX, NewX, Pixs, NewPixs),
    replace(OldY, NewY, NewPixs, PixsR),
    ImagenR = [Altura,Anchura,PixsR].

%------------------------------------------------------------------------------------------------------------------------------

%10 - TDA image - imgRGBtoHex / imageRGBtoHex:

% Descripción: Predicado que convierte una imagen RGB a Hex.
% Dominio: Imagen X Imagen
% Imagen: Imagen RGB. Imagen: Imagen Hex.

% Debemos comprender que el formato RGB es un formato de 3 bytes, mientras que el formato Hex es un formato de 6 bytes.
% Eje: RGB = [255,255,255] -> Hex = [FFFFFF]
% para esto, se debe dividir el valor de cada byte en 16, del cual se obtendrá el primer dígito del byte Hex. Si la división no es
% exacta, el decimal sobrante se multiplicara por 16.
% Eje: 255 / 16 = 15.9375
% 15 = F, 
% 0.9375 * 16 = 15 
% 15 = F
% Hex = [FF]

% Se creara los casos bases para un predicado que permita tranformar un decimal a Hex.

% Recursión
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
byteToHex(10, A) :- !.
byteToHex(11, B) :- !.
byteToHex(12, C) :- !.
byteToHex(13, D) :- !.
byteToHex(14, E) :- !.
byteToHex(15, F) :- !.

% Recursión
byteToHex(Decimal, Hex) :-
    Decimal > 15,
    Div is Decimal / 16,
    Decimal1 is floor(Div),
    Decimal2 is (Div - Decimal1) * 16,
    byteToHex(Decimal1, Hex1),
    byteToHex(Decimal2, Hex2),
    atom_concat(Hex1, Hex2, Hex).

% Recursión
rgbToHex([], []) :- !.

rgbToHex([H|T], [H1|T1]) :-
    getbyIndex(0, H, R),
    getbyIndex(1, H, G),
    getbyIndex(2, H, B),
    byteToHex(R, R1),
    byteToHex(G, G1),
    byteToHex(B, B1),
    atom_concat(R1, G1, RG),
    atom_concat(RG, B1, H1),
    rgbToHex(T, T1).

imageRGBtoHex(Imagen, ImagenHex) :-
    getbyIndex(2, Imagen, Pixs),
    rgbToHex(Pixs, PixsH),
    ImagenHex = [Altura,Anchura,PixsH].

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


























