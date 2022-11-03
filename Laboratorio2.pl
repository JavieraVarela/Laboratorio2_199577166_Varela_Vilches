% -------------------------------------------------------------
% |Predicados no relacionados a TDAs (Auxiliares)             |
% -------------------------------------------------------------

% Clausulas:

% Reglas:

% Predicados:

% Descripción: Predicado que permite remplazar un elemento de una lista por otro.
% Dominio: Lista X Elemento X Elemento X Lista.
% Ej: Lista, Indice, Elemento deseado, lista final
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% Descripción: Predicado que agrega un elemento al final de una lista.
% Dominio: Lista X any X Lista
append_final([], X, [X]).
append_final([H|T], X, [H|L]) :- append_final(T, X, L).

% Descripción: Predicado que verifica si un elemento está en una lista.
% Dominio: Lista X Lista
mymember(L, [L|_]).
mymember(L, [_|L1]) :- mymember(L, L1).

% Descripción: Obtener un elemento de una lista por su índice
% Dominio: Lista X Indice X Elemento
getbyIndex(0, [X], X) :- !.
getbyIndex(0, [H|_], H) :- !.
getbyIndex(I, [_|T], E) :- 
    NuevoIndex is I-1, 
    getbyIndex(NuevoIndex, T, E), !.


% Descripción: Predicado que elimina el elemento posicionado al final de una lista.
% Dominio: Lista X Lista
deleteLastElement([_], []) :- !.
deleteLastElement([Head, Next|Tail], [Head|NTail]):-
  deleteLastElement([Next|Tail], NTail).

removerOcurrencias(_, [], []) :- !.
removerOcurrencias(X, [X|Xs], Y) :- !, removerOcurrencias(X, Xs, Y).
removerOcurrencias(X, [T|Xs], Y) :- !, removerOcurrencias(X, Xs, Z), append([T], Z, Y).

removerElemento(_, [], []) :- !.
removerElemento(R, [R|T], T) :- !.
removerElemento(R, [H|T], [H|T2]) :- H \= R, removerElemento(R, T, T2).

removerDuplicados([], []) :- !.
removerDuplicados([H|T], X) :-
    mymember(H, T), !,
    removerDuplicados(T, X).
removerDuplicados([H|T], [H|T2]) :-
    removerDuplicados(T, T2).


%------------------------------------------------------------------------------------------------------------------------------

% ------------------------------------------------------------
% |TDA: Imagen                                               |
% ------------------------------------------------------------

/* Descripción: El TDA Imagen se representa a traves de una lista que almacena elementos del tipo entero, correspondiendo a la anchura
y altura de la imagen, además de una lista de listas que almacena los pixeles dentro. */

/* Dominio: 
    - Anchura: Entero 
    - Altura: Entero
    - Pix: Lista de listas de pixeles
    - Imagen: Estructura */

/* Predicados:  
    - imagen(Anchura, Altura, Pix, Imagen)                              Aridad: 4.
    - getAnchura(Imagen, Anchura)                                       Aridad: 2.
    - getAltura(Imagen, Altura)                                         Aridad: 2.
    - getPix(Imagen, Pix)                                               Aridad: 2. 
    - pixbit(X,Y,Bit,Depth,Pix)                                         Aridad: 5.  
    - isPixBit(Pix)                                                     Aridad: 1.
    - c(X)                                                              Aridad: 1.
    - pixrgb(X, Y, R, G, B, Depth, Pix)                                 Aridad: 7.
    - isPixRGB(Pix)                                                     Aridad: 1.
    - pixHex(X, Y, Hex, Depth, Pix)                                     Aridad: 5.
    - isPixHex(Pix)                                                     Aridad: 1.
    - isBitmapAux([Lista])                                              Aridad: 1.
    - isBitmap(Imagen)                                                  Aridad: 1.
    - isRGBmapAux([Lista])                                              Aridad: 1.
    - isRGBmap(Imagen)                                                  Aridad: 1.
    - isHexmapAux([Lista])                                              Aridad: 1.
    - isHexmap(Imagen)                                                  Aridad: 1.
    - restH(Imagen, Z)                                                  Aridad: 2.
    - restV(Imagen, Z)                                                  Aridad: 2.
    - newcoordH(RestH, Pix, NewX)                                       Aridad: 3.
    - newcoordV(RestV, Pix, NewY)                                       Aridad: 3.
    - modificarCoordenadaAuxH(RestH, Pix, NewPix)                       Aridad: 3.
    - modificarCoordenadaAux2H(RestH, [H|T], [NH|NT])                   Aridad: 3.
    - flipH(Imagen, NewImagen)                                          Aridad: 2.
    - modificarCoordenadaAuxV(RestV, Pix, NewPix)                       Aridad: 3.
    - modificarCoordenadaAux2V(RestV, [H|T], [NH|NT])                   Aridad: 3.
    - flipV(Imagen, NewImagen)                                          Aridad: 2.
    - CropAux1(Pix, X1, Y1, X2, Y2, NewPix)                             Aridad: 6.
    - CropAux2(Pix, X1, Y1, X2, Y2, NewPix)                             Aridad: 6.
    - Crop(Imagen, X1, Y1, X2, Y2, NewImagen)                           Aridad: 6.
    - byteToHex(Byte, Hex)                                              Aridad: 2.
    - rgbToHex([H|T], [H1|T1])                                          Aridad: 2.
    - imageRGBtoHex(Image, ImageHex)                                    Aridad: 2.
    - countColor(Pix, Color, Count)                                     Aridad: 3.
    - histogramaux([H|T], [H1|T1])                                      Aridad: 2.
    - imageToHistogram(Image, Histogram)                                Aridad: 2. 
    - rotate90Aux(Pix, NewPix)                                          Aridad: 2.
    - rotate90Aux2([H|T], [H1|T1])                                      Aridad: 2.
    - rotate90(Imagen, NewImagen)                                       Aridad: 2.
    - lessFrequent([H|T], Element)                                      Aridad: 2.
    - lessFrequentAux([], Element, Element)                             Aridad: 3.
    - compressAux([H|T], [H1|T1])                                       Aridad: 2.
    - compress(Imagen, CompressImagen)                                  Aridad: 2.
    - modificarPix(Pix, [H|T], [H1|T1])                                 Aridad: 3.
    - modificarPix2(Pix, [H|T], [H1|T1])                                Aridad: 3.
    - changePixel(Imagen, Pix, ImagenR)                                 Aridad: 3.
    - modificarRGB(Pix, NewR, NewG, NewB)                               Aridad: 4.
    - modificarRGBaux(Pix, NewPix)                                      Aridad: 2.
    - modificarRGBaux2([H|T], [NewH|NewT])                              Aridad: 2.
    - invertColorRGB(Imagen, NewImagen)                                 Aridad: 2.
    */

/* Metas primarias:
    - imagen.
    - isBitmap.
    - isRGBmap.
    - isHexmap.
    - flipH.
    - flipV.
    - Crop.
    - imageRGBtoHex.
    - imageToHistogram.
    - rotate90.
    - compress.
    - changePixel.
    - invertColorRGB. 
    */

/* Metas segundarias:
    - getAnchura.
    - getAltura.
    - getPix.
    - pixbit.
    - isPixBit.
    - c.
    - pixrgb.
    - isPixRGB.
    - pixHex.
    - isPixHex.
    - isBitmapAux.
    - isRGBmapAux.
    - isHexmapAux.
    - restH.
    - restV.
    - newcoordH.
    - newcoordV.
    - modificarCoordenadaAuxH.
    - modificarCoordenadaAux2H.
    - modificarCoordenadaAuxV.
    - modificarCoordenadaAux2V.
    - CropAux1.
    - CropAux2.
    - byteToHex.
    - rgbToHex.
    - countColor.
    - histogramaux.
    - rotate90Aux.
    - rotate90Aux2.
    - lessFrequent.
    - lessFrequentAux.
    - compressAux.
    - modificarPix.
    - modificarPix2.
    - modificarRGB.
    - modificarRGBaux.
    - modificarRGBaux2.
    */

% Clausulas:

% Reglas:

% Predicados: 

imagen(Anchura,Altura,Pix,Imagen):-
    integer(Altura),
    integer(Anchura),
    Imagen = [Anchura,Altura,Pix].

% Descripción: Predicado que obtiene la anchura de una imagen.
% Dominio: Imagen X Anchura.
getAnchura(Imagen, Anchura) :-
    getbyIndex(0, Imagen, Anchura).

% Descripción: Predicado que obtiene la altura de una imagen.
% Dominio: Imagen X Altura.
getAltura(Imagen, Altura) :-
    getbyIndex(1, Imagen, Altura).

% Descripción: Predicado que obtiene los pixeles de una imagen.
% Dominio: Imagen X Pix.
getPix(Imagen, Pix) :-
    getbyIndex(2, Imagen, Pix).


%Pix necesarios:

% pix1: pixbit

% Descripción: Predicado que permite obtener la estrucura de un pixbit.
% Dominio: Entero X Entero X Entero X Entero X Pixbit.
pixbit(X,Y,Bit,Depth,Pix):-
    integer(X),
    integer(Y),
    integer(Bit),
    (Bit =:= 1; Bit =:= 0),
    integer(Depth),
    Pix = [X,Y,Bit,Depth], !.

% Descripción: Predicado que permite confirmar si un elemento es un pixbit.
% Dominio: Pixbit.
isPixbit(Pix) :-
    length(Pix, Largo),
    Largo =:= 4,
    getbyIndex(2, Pix, X),
    integer(X).


%pix2: pixrgb

% Descripción: Predicado que permite confirmar si un elemento X se encuentra dentro del rango 0 < X < 255.
% Dominio: Entero.
c(X):- integer(X), X >= 0, X =< 255.

% Descripción: Predicado que permite obtener la estrucura de un pixrgb.
% Dominio: Entero X Entero X Entero X Entero X Pixrgb.
pixrgb(X,Y,R,G,B,Depth,Pix):-
    integer(X),
    integer(Y),
    c(R),
    c(G),
    c(B),
    integer(Depth),
    Pix = [X,Y,R,G,B,Depth], !.

% Descripción: Predicado que permite confirmar si un elemento es un pixrgb.
% Dominio: Pix.
isPixRGB(Pix) :-
    length(Pix, Largo),
    Largo =:= 6.


%pix3: pixhex

% Descripción: Predicado que permite obtener la estrucura de un pixhex.
% Dominio: Entero X Entero X Atom X Entero X Pixhex.
pixhex(X,Y,Hex,Depth,Pix):-
    integer(X),
    integer(Y),
    atom(Hex),
    integer(Depth),
    Pix = [X,Y,Hex,Depth], !.

% Descripción: Predicado que permite confirmar si un elemento es un pixhex.
% Dominio: Pix.
isHex(Pix) :-
    length(Pix, Largo),
    Largo =:= 4,
    getbyIndex(2, Pix, Hex),
    atom(Hex).

%------------------------------------------------------------------------------------------------------------------------------

% TDA image - is bitmap / imageIsBitmap.

% Descripción: Predicado que permite confirmar si una imagen esta compuesta por pixbits.
% Dominio: Imagen.
% Recorrido: Boolean.

% Descripción: Predicado que permite realizar la recursión con todos los elementos de una lista. Permite confirmar si el elemento
% analizado es un pixbit.
isBitmapAux([]) :- !.
isBitmapAux([H|T]) :-
    isPixbit(H),
    isBitmapAux(T).

% Predicado final que solamente envuelve la recursión. Permite confirmar si una imagen es un bitmap.
isBitmap(Imagen) :-
    getbyIndex(2, Imagen, Bits),
    isBitmapAux(Bits).

%------------------------------------------------------------------------------------------------------------------------------

%4 - TDA image - is rgbmap / imageIsPixmap.

% Descripción: Predicado que permite confirmar si una imagen esta compuesta por pixrgb.
% Dominio: Imagen.
% Recorrido: Boolean.

% Descripción: Predicado que permite realizar la recursión con todos los elementos de una lista. Permite confirmar si el elemento
% analizado es un pixrgb.
isRGBmapAux([]) :- !.
isRGBmapAux([H|T]) :-
    isPixRGB(H),
    isRGBmapAux(T).

% Predicado final que solamente envuelve la recursión. Permite confirmar si una imagen es un rgbmap.
isRGBmap(Imagen) :-
    getbyIndex(2, Imagen, RGBs),
    isRGBmapAux(RGBs).

%------------------------------------------------------------------------------------------------------------------------------

%5 - TDA image - is hexmap / imageIsHexmap

% Descripción: Predicado que permite confirmar si una imagen esta compuesta por pixhex.
% Dominio: Imagen.
% Recorrido: Boolean.

% Descripción: Predicado que permite realizar la recursión con todos los elementos de una lista. Permite confirmar si el elemento
% analizado es un pixhex.
isHexmapAux([]) :- !.
isHexmapAux([H|T]) :-
    isHex(H),
    isHexmapAux(T).

% Predicado final que solamente envuelve la recursión. Permite confirmar si una imagen es un hexmap.
isHexmap(Imagen) :-
    getbyIndex(2, Imagen, Hexs),
    isHexmapAux(Hexs).

%------------------------------------------------------------------------------------------------------------------------------
%6 - TDA image - flipH / imageFlipH:

% Descripción: Predicado que permite voltear una imagen horizontalmente.
% Para obtener las nuevas coordenadas de las imagenes se realiza la siguiente operación matematica:
%   (Anchura - 1) = z
%   z - x = Nueva cordenada
% Al obtener la nueva coordenada, esta debe remplazarse dentro de la lista de pixeles en la estructura de la imagen.
% Dominio: Lista X Lista

% Descripción: Nos permite obtener un valor Z el cual sera utilizado para restar a la coordenada X actual de un pixel.
% Dominio: Entero X Entero
restH(Imagen, Z) :-
    getAnchura(Imagen, Anchura),
    Z is Anchura - 1.

% Descripción: Predicado que permite obtener la nueva coordenada X de un pixel.
% Dominio: Entero X Entero X Entero
newcoordH(RestH, Pix, NewX):-
    getbyIndex(0, Pix, X),
    NewX is RestH - X.

% Descripción: Permite modificar la coordenada X de un pixel.
% Dominio: Entero X Lista X Lista
modificarCoordenadaAuxH(RestH, Pix, NewPix) :-
    newcoordH(RestH, Pix, NewX),
    replace(Pix, 0, NewX, NewPix), !.

% Descripción: Predicado que permite realizar la recursión con todos los elementos de una lista.
% Dominio: Entero X Lista X Lista
modificarCoordenadaAux2H(_ ,[], []) :- !.
modificarCoordenadaAux2H(RestH, [H|T], [NewH|NewT]) :-
    modificarCoordenadaAuxH(RestH, H, NewH),
    modificarCoordenadaAux2H(RestH, T, NewT).

% Predicado final que solamente envuelve la recursión. Permite modificar las coordenadas X de todos los pixeles de una imagen.
flipH(Imagen, NewImagen) :-
    restH(Imagen, RestH),
    getbyIndex(2, Imagen, Pixs),
    modificarCoordenadaAux2H(RestH, Pixs, NewPixs),
    replace(Imagen, 2, NewPixs, NewImagen).

%------------------------------------------------------------------------------------------------------------------------------

%7 - TDA image - flipV / imageFlipV:

% Descripción: Predicado que permite voltear una imagen verticalmente.
% Para obtener las nuevas coordenadas de las imagenes se realiza la siguiente operación matematica:
%   (Altura - 1) = z
%   z - y = Nueva cordenada
% Al obtener la nueva coordenada, esta debe remplazarse dentro de la lista de pixeles en la estructura de la imagen.
% Dominio: Lista X Lista

% Descripción: Nos permite obtener un valor Z el cual sera utilizado para restar a la coordenada Y actual de un pixel.
% Dominio: Entero X Entero
restV(Imagen, Z) :-
    getAltura(Imagen, Altura),
    Z is Altura - 1.

% Descripción: Predicado que permite obtener la nueva coordenada Y de un pixel.
% Dominio: Entero X Entero X Entero
newcoordV(RestV, Pix, NewY):-
    getbyIndex(1, Pix, Y),
    NewY is RestV - Y.

% Descripción: Permite modificar la coordenada Y de un pixel.
% Dominio: Entero X Lista X Lista
modificarCoordenadaAuxV(RestV, Pix, NewPix) :-
    newcoordV(RestV, Pix, NewY),
    replace(Pix, 1, NewY, NewPix), !.

% Descripción: Predicado que permite realizar la recursión con todos los elementos de una lista.
% Dominio: Entero X Lista X Lista
modificarCoordenadaAux2V(_ ,[], []) :- !.
modificarCoordenadaAux2V(RestV, [H|T], [NewH|NewT]) :-
    modificarCoordenadaAuxV(RestV, H, NewH),
    modificarCoordenadaAux2V(RestV, T, NewT).

% Predicado final que solamente envuelve la recursión. Permite modificar las coordenadas Y de todos los pixeles de una imagen.
flipV(Imagen, NewImagen) :-
    restV(Imagen, RestV),
    getbyIndex(2, Imagen, Pixs),
    modificarCoordenadaAux2V(RestV, Pixs, NewPixs),
    replace(Imagen, 2, NewPixs, NewImagen).


%------------------------------------------------------------------------------------------------------------------------------

%8 - TDA image - Crop / imageCrop:

% Descripción: Predicado que recorta una imagen en base a las coordenadas de un pixel.
% Dominio: Imagen X Entero X Entero X Entero X Entero X Imagen
% Imagen: Imagen a recortar. Entero: Coordenada X1 del pixel. Entero: Coordenada Y1 del pixel. Entero: Coordenada X2 del pixel. Entero: Coordenada Y2 del pixel.
% Imagen: Imagen recortada.

% Descripción: Predicado que permite obtener los pixeles de una imagen que están dentro de un recorte.
% Dominio: Lista X Entero X Entero X Entero X Entero X Lista
cropAux1(Pix, X1, Y1, X2, Y2, Newpix):-
    getbyIndex(0, Pix, X),
    getbyIndex(1, Pix, Y),
    (X >= X1, X =< X2, Y >= Y1, Y =< Y2 -> Newpix = Pix ; Newpix = []).

% Descripción: Predicado que permite realizar la recursión con todos los elementos de una lista.
% Dominio: Lista X Entero X Entero X Entero X Entero X Lista
cropAux2([], _, _, _, _, []) :- !.
cropAux2([H|T], X1, Y1, X2, Y2, [NewH|NewT]) :-
    cropAux1(H, X1, Y1, X2, Y2, NewH),
    cropAux2(T, X1, Y1, X2, Y2, NewT).

% Predicado final que envuelve la recursión. Otorga una nueva imagenes con la nueva anchura, altura y lista de pixeles.
crop(Imagen, X1, Y1, X2, Y2, NewImagen) :-
    getbyIndex(2, Imagen, Pixs),
    cropAux2(Pixs, X1, Y1, X2, Y2, NewPixs),
    removerOcurrencias([], NewPixs, NewPixs2),
    NewAltura is Y2 - Y1,
    NewAnchura is X2 - X1,
    NewImagen = [NewAltura, NewAnchura, NewPixs2].

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

% Descripción: Predicado que envuelve la recursión. Transforma una imagen RGB a Hex.
% Dominio: Imagen X Imagen
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
% - Reconocer cuantos colores hay en cada Imagen.
% - Crear un predicado que permita contar cuantas veces aparece un color en la imagen.
% - Los colores se encuentran presenten en los pix del TDA imagen. Por lo tanto se debe crear un predicado que permita obtener
% los colores perteneciente a cada pix.
% - En pixbit el color se obtiene cuando el bit es 1.
% - En pixhex el color se obtiene a partir del atom Hex.
% - En pixrgb el color se obtiene a partir de los valores R, G y B. Como se ha creado un predicado que permie transformar un pixrgb
% a un pixhex, se puede utilizar este predicado para obtener el color de manera más sencilla.

% Descripción: Predicado que permite obtener cuantas veces un color se repite en una lista de pix.
% Dominio: Lista X Color X Entero
countColor([], _, 0) :- !.
countColor([H|T], Color, Count):-
    getbyIndex(2, H, Color),
    countColor(T, Color, Count1),
    Count is Count1 + 1, !.

countColor([_|T], Color, Count):-
    countColor(T, Color, Count).

getColors([], []) :- !.
getColors([H|T], [H1|T1]) :-
    getbyIndex(2, H, Color),
    H1 = Color,
    getColors(T, T1).

% Descripción: Predicado que permite realizar la recursión con todos los elementos de la lista de pix.
% Dominio: Lista X Lista.
histogramaux(_,[],[]) :- !.
histogramaux(Pixs, [H|T], [H1|T1]) :-
    countColor(Pixs, H, Count),
    H1 = [H, Count],
    histogramaux(Pixs, T, T1).

% Predicado final que envuelve la recursión. Se obitiene el mayor numero entregando el histograma.
imageToHistogram(Imagen, Histograma):-
    (isRGBmap(Imagen) -> imageRGBtoHex(Imagen, ImagenHex) ; ImagenHex = Imagen),
    getbyIndex(2, ImagenHex, Pixs),
    getColors(Pixs, Colors1),
    removerDuplicados(Colors1, Colors2),
    histogramaux(Pixs, Colors2, Histograma).

%------------------------------------------------------------------------------------------------------------------------------

%11 - TDA image - Rotate / imageRotate90:

% Descripción: Predicado que rota una imagen en un angulo de 90 grados.
% Para esto se utiliza la rotación de coordenadas dentro de un plano cartesiano. Se utiliza la formula:
%   (x,y) = (-y,x).
% Dominio: Imagen X Imagen
% Imagen: Imagen a rotar. Imagen: Imagen rotada.


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

% 12 - TDA image - Compress / imageCompress:

% Descripción: Predicado que comprime una imagen.
% Para comprimir una imagen se busca los pixeles que más se repiten y se guardan en una nueva lista (Esto para recuperarlos en 
% un futuro). Las imagenes comprimidas solo se pueden trabajar con los demás predicados si es descomprimida antes.
% Dominio: Imagen X Imagen
% Imagen: Imagen a comprimir. Imagen: Imagen comprimida.

% Descripción: Predicado que permite obtener el elemento que menos se repite en una lista.
% Dominio: Lista X Elemento
% Lista: Lista de la cual se quiere obtener el elemento que menos se repite. Elemento: Elemento que menos se repite en la lista.

lessFrequent([H|T], Element) :-
    lessFrequentAux(T, H, Element).

lessFrequentAux([], Element, Element) :- !.
lessFrequentAux([H|T], Element, Element1) :-
    getbyIndex(1, H, Count),
    getbyIndex(1, Element, Count1),
    Count < Count1,
    lessFrequentAux(T, H, Element1), !.

lessFrequentAux([_|T], Element, Element1) :-
    lessFrequentAux(T, Element, Element1).

% Descripción: Predicado que permite almacenar el elemento que menos se repite en una lista.
% Dominio: Elemento X Lista X Lista

lessFrequentAux2(Element, [], [Element]) :- !.

% Descripción: Predicado que envuelve la recursión anterior para poder comprimir las imagenes a traves de los pix.
% Dominio: Imagen X compressimagen

compressAux([], []) :- !.
compressAux([H|T], [H1|T1]) :-
    lessFrequent(H, Element),
    lessFrequentAux2(Element, H, H2),
    H1 = H2,
    compressAux(T, T1).

compress(Imagen, CompressImagen) :-
    getbyIndex(2, Imagen, Pixs),
    compressAux(Pixs, PixsR),
    CompressImagen = [Imagen, PixsR].

%------------------------------------------------------------------------------------------------------------------------------

% 13 - TDA image - changePixel / imageChangePixel:

% Descripción: Predicado que permite cambiar un pixel de una imagen.
% Dominio: Imagen X Pix X Imagen.
% Imagen: Imagen a la cual se le quiere cambiar el pixel. Pix: Pixel que se quiere cambiar. Imagen: Imagen con el pixel cambiado.

% Nota: El pixel remplazado debe tener en consideración el tipo de imagen. Si es una imagen del tipo bit, debe verificar a traves 
% del predicado isPixbit que el pixel sea de tipo bit. Si es una imagen del tipo RGB, debe verificar a traves del predicado 
% isPixRGB que el pixel sea de tipo RGB. Si es una imagen del tipo Hex, debe verificar a traves del predicado isHex que el 
% pixel sea de tipo Hex.

modificarPix(_, [], []) :- !.
modificarPix(Pix, [H|T], [H1|T1]) :-
    getbyIndex(0, H, X),
    getbyIndex(1, H, Y),
    getbyIndex(0, Pix, X1),
    getbyIndex(1, Pix, Y1),
    X == X1,
    Y == Y1,
    H1 = Pix,
    modificarPix(Pix, T, T1), !.

modificarPix2(Pix, [H|T], [H1|T1]) :-
    H1 = H,
    modificarPix(Pix, T, T1).

changePixel(Imagen, Pix, ImagenR) :-
    getbyIndex(2, Imagen, Pixs),
    modificarPix2(Pix, Pixs, PixsR),
    ImagenR = [Imagen, PixsR].

%------------------------------------------------------------------------------------------------------------------------------

%14 - TDA image - invertColorRGB / imageinvertColorRGB:

% Descripción: Predicado que permite obtener el color simetricamente opuesto en cada canal dentro de un pixel.
% Dominio: PixRGB.
% Recorrido: PixRGB.

% Nota: Una referencia para el color simetricamente opuesto:
% 0 -> 255
% 1 -> 254
% 2 -> 253

% Una solución matematica para este caso es tomar el valor maximo correspondiente a 255 y restarle el valor contenido en las variables
% R, G y B.
% Ejemplo:  pixrgb(1, 1, 190 (R), 0 (G), 234 (B), 255, PD)
% 255 - 190 = 65.
% 255 - 0 = 255.
% 255 - 255 = 0.
% De esta forma se planea obtener el valor opuesto del RGB.

/*
pixrgb(X,Y,R,G,B,Depth,Pix):-
    integer(X),
    integer(Y),
    c(R),
    c(G),
    c(B),
    integer(Depth),
    Pix = [X,Y,R,G,B,Depth], !.
*/

% Descripción: Predicado que permite encontrar el valor simetricamente opuesto de cada variable RGB dentro de un Pix.
% Dominio: Pix X Entero X Entero X Entero.

modificarRGB(Pix, NewR, NewG, NewB):-
    getbyIndex(2, Pix, R),
    getbyIndex(3, Pix, G),
    getbyIndex(4, Pix, B),
    NewR is 255 - R,
    NewG is 255 - G,
    NewB is 255 - B.

% Descripción: Predicado que permite remplazar el valor obtenido en cada RGB por su opuesto simetrico.
% Dominio: Pix X pix.
modificarRGBaux(Pix, NewPix):-
    modificarRGB(Pix, NewR, NewG, NewB),
    replace(Pix, 2, NewR, NewPix),
    replace(Pix, 3, NewG, NewPix),
    replace(Pix, 4, NeWB, NewPix), !.

% Descripción: Predicado que permite la recursión en cada uno de los pixeles de la lista.
% Dominio: Lista X Lista.
modificarRGBaux2([], []):- !.
modificarRGBaux2([H|T], [NewH|NewT]):-
    modificarRGBaux(H, NewH),
    modificarRGBaux(T, NewT).

% Predicado final que permite crear una imagen con los nuevos Pix modificados.
invertColorRGB(Imagen, NewImagen):-
    modificarRGB(Imagen, NewR, NewG, NewB),
    getbyIndex(2,Imagen, Pixs).
    modificarRGBaux2(Pixs, NewPixs).
    replace(Imagen, 2, Newpixs, NewImagen).

