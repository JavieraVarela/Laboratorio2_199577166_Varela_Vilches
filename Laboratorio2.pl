% Predicados no relacionados a TDAs (Auxiliares)
% -------------------------------------------------------------
% Lista, Indice, Elemento deseado, lista final
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% Descripción: Predicado que agrega un elemento al final de una lista.
% Dominio: Lista X any X Lista
append_final([], X, [X]).
append_final([H|T], X, [H|L]) :- append_final(T, X, L).





%2 - TD1: Constructor1
imagen(Anchura,Altura,Pix,Imagen):-
    integer(Altura),
    integer(Anchura),
    Imagen = [Anchura,Altura,Pix].

getAnchura(Imagen, Anchura) :-
    getbyIndex(0, Imagen, Anchura).

getAltura(Imagen, Altura) :-
    getbyIndex(1, Imagen, Altura).

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
restH(Imagen, Z) :-
    getAnchura(Imagen, Anchura),
    Z is Anchura - 1.

newcoordH(RestH, Pix, NewX):-
    getbyIndex(0, Pix, X),
    NewX is RestH - X.

modificarCoordenadaAux(RestH, Pix, NewPix) :-
    newcoordH(RestH, Pix, NewX),
    replace(Pix, 0, NewX, NewPix), !.

modificarCoordenadaAux2(_ ,[], []) :- !.
modificarCoordenadaAux2(RestH, [H|T], [NewH|NewT]) :-
    modificarCoordenadaAux(RestH, H, NewH),
    modificarCoordenadaAux2(RestH, T, NewT).

flipH(Imagen, NewImagen) :-
    restH(Imagen, RestH),
    getbyIndex(2, Imagen, Pixs),
    modificarCoordenadaAux2(RestH, Pixs, NewPixs),
    replace(Imagen, 2, NewPixs, NewImagen).

%------------------------------------------------------------------------------------------------------------------------------

%7 - TDA image - flipV / imageFlipV:

% (Altura - 1) = z
% z - y = Nueva cordenada

restV(Imagen, Z) :-
    getAltura(Imagen, Altura),
    Z is Altura - 1.

newcoordV(RestV, NewY):-
    getbyIndex(1, Pix, Y),
    NewY is RestV - Y.

modificarCoordenadaAux(RestV, Pix, NewPix) :-
    newcoordV(RestV, NewY),
    replace(Pix, 1, NewY, NewPix), !.

modificarCoordenadaAux2(_ ,[], []) :- !.
modificarCoordenadaAux2(RestV, [H|T], [NewH|NewT]) :-
    modificarCoordenadaAux(RestV, H, NewH),
    modificarCoordenadaAux2(RestV, T, NewT).

flipV(Imagen, NewImagen) :-
    restV(Imagen, RestV),
    getbyIndex(2, Imagen, Pixs),
    modificarCoordenadaAux2(RestV, Pixs, NewPixs),
    replace(Imagen, 2, NewPixs, NewImagen).


%------------------------------------------------------------------------------------------------------------------------------

%8 - TDA image - Crop / imageCrop:

% Descripción: Predicado que recorta una imagen en base a las coordenadas de un pixel.
% Dominio: Imagen X Imagen X Entero X Entero X Entero X Entero
% Imagen: Imagen a recortar. Imagen: Imagen recortada. Entero: Coordenada X del pixel. Entero: Coordenada Y del pixel. Entero: Ancho del recorte. Entero: Alto del recorte.



%------------------------------------------------------------------------------------------------------------------------------

%9 - TDA image - imgRGBtoHex / imageRGBtoHex:

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

byteToHex(Decimal, Hex) :-
    Decimal > 15,
    Div is Decimal / 16,
    Decimal1 is floor(Div),
    Decimal2 is (Div - Decimal1) * 16,
    round(Decimal2, Decimal3),
    byteToHex(Decimal1, Hex1),
    byteToHex(Decimal3, Hex2),
    atom_concat(Hex1, Hex2, Hex).

% Recursión
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

imageRGBtoHex(Imagen, ImagenHex) :-
    getbyIndex(2, Imagen, Pixs),
    rgbToHex(Pixs, PixsH),
    getAnchura(Imagen, Anchura),
    getAltura(Imagen, Altura),
    ImagenHex = [Anchura,Altura,PixsH].

%------------------------------------------------------------------------------------------------------------------------------

%10 - TDA image - to histogram / imageToHistogram:

% Descripción: Predicado que convierte una imagen a un histograma.
% Dominio: Imagen X Histograma
% Imagen: Imagen a convertir. Histograma: Histograma de la imagen.

% Recomendacciones:
% - Reconocer cuantos colores hay.
% - Crear un predicado que permita contar cuantas veces aparece un color en la imagen.
% 





%------------------------------------------------------------------------------------------------------------------------------

%11 - TDA image - Rotate / imageRotate90:

% Descripción: Predicado que rota una imagen en base a un ángulo.
% Dominio: Imagen X Imagen
% Imagen: Imagen a rotar. Imagen: Imagen rotada. Entero: Ángulo de rotación.

% Rotación en 90° es (x,y) = (-y,x).
rotate90Aux(Pix, NewPix) :-
    getbyIndex(0, Pix, X),
    getbyIndex(1, Pix, Y),
    NewX is -Y,
    NewY is X,
    replace(Pix, 0, NewX, NewPix1),
    replace(NewPix1, 1, NewY, NewPix), !.

rotate90Aux2([], []) :- !.
rotate90Aux2([H|T], [H1|T1]) :-
    rotate90Aux(H, NewPix),
    H1 = NewPix,
    rotate90Aux2(T, T1).

rotate90(Imagen, ImagenR) :-
    getbyIndex(2, Imagen, Pixs),
    rotate90Aux2(Pixs, PixsR),
    getAltura(Imagen, Altura),
    getAnchura(Imagen, Anchura),
    ImagenR = [Anchura,Altura,PixsR].
%------------------------------------------------------------------------------------------------------------------------------

% Ejemplos de uso
ejemploImage(X) :-
    pixbit(0, 0, 1, 10, PA),
    pixbit(0, 1, 0, 20, PB),
    pixbit(1, 0, 0, 30, PC),
    pixbit(1, 1, 1, 4, PD),
    imagen(2, 2, [PA, PB, PC, PD], X).

ejemploImage2(X) :-
    pixhex( 0, 0, '#FF0000', 20, PA), 
    pixhex( 0, 1, '#FF0000', 20, PB), 
    pixhex( 0, 2, '#FF0000', 20, PC), 
    pixhex( 1, 0, '#0000FF', 30, PD), 
    pixhex( 1, 1, '#0000FF', 4, PE), 
    pixhex( 1, 2, '#0000FF', 4, PF), 
    pixhex( 2, 0, '#0000FF', 4, PG), 
    pixhex( 2, 1, '#0000FF', 4, PH), 
    pixhex( 2, 2, '#0000FF', 4, PI),
    imagen(3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], X).

ejemploImage3(X) :-
    pixrgb(0, 0, 200, 200, 200, 10, PA),
    pixrgb(0, 1, 200, 200, 200, 20, PB),
    pixrgb(1, 0, 190, 190, 190, 30, PC),
    pixrgb(1, 1, 190, 190, 190, 4, PD),
    imagen(2, 2, [PA, PB, PC, PD], X).

ejemploisBitMap() :-
    ejemploImage(X),
    isBitmap(X).

ejemploIsPixBit() :-
    pixbit(0, 0, 1, 10, PA),
    isPixbit(PA).