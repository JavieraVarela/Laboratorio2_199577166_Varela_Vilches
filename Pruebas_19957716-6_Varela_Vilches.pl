:- include('main_19957716-6_Varela_Vilches.pl').

% ----------------------------------------------
% Ejemplos para el predicado constructor de Image
% ----------------------------------------------

% Resultado esperado: La creación de una imagen con bits.
%X = [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0|...], [1, 1|...]]].
ejemploImage(X) :-
    pixbit(0, 0, 1, 10, PA),
    pixbit(0, 1, 0, 20, PB),
    pixbit(1, 0, 0, 30, PC),
    pixbit(1, 1, 1, 4, PD),
    imagen(2, 2, [PA, PB, PC, PD], X).

% Resultado esperado: La creación de una imagen con hexadecimales.
% X = [3, 3, [[0, 0, '#FF0000', 20], [0, 1, '#FF0000', 20], [0, 2, '#FF0000'|...], [1, 0|...], [1|...], [...|...]|...]].
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

% Resultado esperado: La creación de una imagen con RGB.
% X = [2, 2, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]].
ejemploImage3(X) :-
    pixrgb(0, 0, 200, 200, 200, 10, PA),
    pixrgb(0, 1, 200, 200, 200, 20, PB),
    pixrgb(1, 0, 190, 190, 190, 30, PC),
    pixrgb(1, 1, 190, 190, 190, 4, PD),
    imagen(2, 2, [PA, PB, PC, PD], X).

% Más ejemplos de pixeles para otros predicados.

% Resultado esperado: La creación de un pixel con RGB.
% X = [2, 2, [[0, 0, 0, 20, 123|...], [0, 1, 230, 20|...], [1, 0, 50|...]]].
ejemploImage4(X) :-
    pixrgb(0, 0, 0, 20, 123, 10, PA),
    pixrgb(0, 1, 230, 20, 13, 20, PB),
    pixrgb(1, 0, 50, 90, 143, 30, PC),
    imagen(2, 2, [PA, PB, PC], X).

% Resultado esperado: La creación de un pixel con RGB.
% X = [2, 2, [[0, 0, 4, 64, 121|...], [0, 1, 20, 23|...], [1, 0, 70|...], [1, 1|...]]].
ejemploImage5(X) :-
    pixrgb(0, 0, 4, 64, 121, 10, PA),
    pixrgb(0, 1, 20, 23, 64, 20, PB),
    pixrgb(1, 0, 70, 34, 32, 30, PC),
    pixrgb(1, 1, 10, 190, 83, 30, PD),
    imagen(2, 2, [PA, PB, PC, PD], X).

% ----------------------------------------------
% Ejemplos para el predicado isbitmap
% ----------------------------------------------

% Resultado esperado: True.
ejemploisBitMap() :-
    ejemploImage(X),
    isBitmap(X).

% Resultado esperado: False.
ejemploisBitMap2() :-
    ejemploImage2(X),
    isBitmap(X).

% Resultado esperado: False.
ejemploisBitMap3() :-
    ejemploImage3(X),
    isBitmap(X).

% ----------------------------------------------
% Ejemplos para el predicado isRGBmap
% ----------------------------------------------

% Resultado esperado: False.
ejemploisRGBmap() :-
    ejemploImage(X),
    isRGBmap(X).

% Resultado esperado: False.
ejemploisRGBmap2() :-
    ejemploImage2(X),
    isRGBmap(X).

% Resultado esperado: True.
ejemploisRGBmap3() :-
    ejemploImage3(X),
    isRGBmap(X).

% ----------------------------------------------
% Ejemplos para el predicado isHexmap
% ----------------------------------------------

% Resultado esperado: False.
ejemploisHexmap() :-
    ejemploImage(X),
    isHexmap(X).

% Resultado esperado: True.
ejemploisHexmap2() :-
    ejemploImage2(X),
    isHexmap(X).

% Resultado esperado: False.
ejemploisHexmap3() :-
    ejemploImage3(X),
    isHexmap(X).

% ----------------------------------------------
% Ejemplos para el predicado flipH
% ----------------------------------------------

% Resultado esperado: La imagen con los pixeles invertidos horizontalmente.
%X = [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0|...], [1, 1|...]]],
%Y = [2, 2, [[1, 0, 1, 10], [1, 1, 0, 20], [0, 0, 0|...], [0, 1|...]]].
ejemploflipH(Y) :-
    ejemploImage(X),
    flipH(X, Y).

% Resultado esperado: La imagen con los pixeles invertidos horizontalmente.
%X = [3, 3, [[0, 0, '#FF0000', 20], [0, 1, '#FF0000', 20], [0, 2, '#FF0000'|...], [1, 0|...], [1|...], [...|...]|...]],
%Y = [3, 3, [[2, 0, '#FF0000', 20], [2, 1, '#FF0000', 20], [2, 2, '#FF0000'|...], [1, 0|...], [1|...], [...|...]|...]].
ejemploflipH2(Y) :-
    ejemploImage2(X),
    flipH(X, Y).

% Resultado esperado: La imagen con los pixeles invertidos horizontalmente.
%X = [2, 2, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]],
%Y = [2, 2, [[1, 0, 200, 200, 200|...], [1, 1, 200, 200|...], [0, 0, 190|...], [0, 1|...]]].
ejemploflipH3(Y) :-
    ejemploImage3(X),
    flipH(X, Y).

% ----------------------------------------------
% Ejemplos para el predicado flipV
% ----------------------------------------------

% Resultado esperado: La imagen con los pixeles invertidos verticalmente.
%X = [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0|...], [1, 1|...]]],
%Y = [2, 2, [[0, 1, 1, 10], [0, 0, 0, 20], [1, 1, 0|...], [1, 0|...]]].
ejemploflipV(Y) :-
    ejemploImage(X),
    flipV(X, Y).

% Resultado esperado: La imagen con los pixeles invertidos verticalmente.
%X = [3, 3, [[0, 0, '#FF0000', 20], [0, 1, '#FF0000', 20], [0, 2, '#FF0000'|...], [1, 0|...], [1|...], [...|...]|...]],
%Y = [3, 3, [[0, 2, '#FF0000', 20], [0, 1, '#FF0000', 20], [0, 0, '#FF0000'|...], [1, 2|...], [1|...], [...|...]|...]].
ejemploflipV2(Y) :-
    ejemploImage2(X),
    flipV(X, Y).

% Resultado esperado: La imagen con los pixeles invertidos verticalmente.
%X = [2, 2, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]],
%Y = [2, 2, [[0, 1, 200, 200, 200|...], [0, 0, 200, 200|...], [1, 1, 190|...], [1, 0|...]]].
ejemploflipV3(Y) :-
    ejemploImage3(X),
    flipV(X, Y).

% ----------------------------------------------
% Ejemplos para el predicado Crop
% ----------------------------------------------
% Resultado esperado: La imagen conserva los pixeles dentro del rango dado.
%X = [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0|...], [1, 1|...]]],
%Y = [1, 1, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0|...], [1, 1|...]]].
ejemploCrop(Y) :-
    ejemploImage(X),
    crop(X, 0, 0, 1, 1, Y).

% Resultado esperado: La imagen conserva los pixeles dentro del rango dado.
%X = [3, 3, [[0, 0, '#FF0000', 20], [0, 1, '#FF0000', 20], [0, 2, '#FF0000'|...], [1, 0|...], [1|...], [...|...]|...]],
%Y = [1, 1, [[0, 0, '#FF0000', 20], [0, 1, '#FF0000', 20], [1, 0, '#0000FF'|...], [1, 1|...]]].
ejemploCrop2(Y) :-
    ejemploImage2(X),
    crop(X, 0, 0, 1, 1, Y).

% Resultado esperado: La imagen conserva los pixeles dentro del rango dado.
%X = [2, 2, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]],
%Y = [1, 1, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]].
ejemploCrop3(Y) :-
    ejemploImage3(X),
    crop(X, 0, 0, 1, 1, Y).

% ----------------------------------------------
% Ejemplos para el predicado imageRGBtoHex
% ----------------------------------------------

% Resultado esperado: La imagen con los pixeles en formato RGB convertidos a Hexadecimal.
%X = [2, 2, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]],
%Y = [2, 2, [[0, 0, 'C8C8C8', 10], [0, 1, 'C8C8C8', 20], [1, 0, 'BEBEBE'|...], [1, 1|...]]].
ejemploimageRGBtoHex(Y) :-
    ejemploImage3(X),
    imageRGBtoHex(X, Y).

% Resultado esperado: La imagen con los pixeles en formato RGB convertidos a Hexadecimal.
%X = [2, 2, [[0, 0, 0, 20, 123|...], [0, 1, 230, 20|...], [1, 0, 50|...]]],
%Y = [2, 2, [[0, 0, '0147B', 10], [0, 1, 'E614D', 20], [1, 0, '325A8F'|...]]].
ejemploimageRGBtoHex2(Y) :-
    ejemploImage4(X),
    imageRGBtoHex(X, Y).

% Resultado esperado: La imagen con los pixeles en formato RGB convertidos a Hexadecimal.
%X = [2, 2, [[0, 0, 4, 64, 121|...], [0, 1, 20, 23|...], [1, 0, 70|...], [1, 1|...]]],
%Y = [2, 2, [[0, 0, '44079', 10], [0, 1, '141740', 20], [1, 0, '462220'|...], [1, 1|...]]].
ejemploimageRGBtoHex3(Y) :-
    ejemploImage5(X),
    imageRGBtoHex(X, Y).

% ----------------------------------------------
% Ejemplos para el predicado imageToHistogram
% ----------------------------------------------

% Resultado esperado: El histograma de la imagen.
%X = [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0|...], [1, 1|...]]],
%Y = [[0, 2], [1, 2]].
ejemploimageToHistogram(Y) :-
    ejemploImage(X),
    imageToHistogram(X, Y).

% Resultado esperado: El histograma de la imagen.
%X = [3, 3, [[0, 0, '#FF0000', 20], [0, 1, '#FF0000', 20], [0, 2, '#FF0000'|...], [1, 0|...], [1|...], [...|...]|...]],
%Y = [['#FF0000', 3], ['#0000FF', 6]].
ejemploimageToHistogram2(Y) :-
    ejemploImage2(X),
    imageToHistogram(X, Y).

% Resultado esperado: El histograma de la imagen.
%X = [2, 2, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]],
%Y = [['C8C8C8', 2], ['BEBEBE', 2]].
ejemploimageToHistogram3(Y) :-
    ejemploImage3(X),
    imageToHistogram(X, Y).

% ----------------------------------------------
% Ejemplos para el predicado rotate90
% ----------------------------------------------

% Resultado esperado: La imagen rotada 90 grados.
%   (x,y) = (-y,x).
%X = [2, 2, [[0, 0, 1, 10], [0, 1, 0, 20], [1, 0, 0|...], [1, 1|...]]],
%Y = [2, 2, [[0, 0, 1, 10], [-1, 0, 0, 20], [0, 1, 0|...], [-1, 1|...]]].
ejemplorotate90(Y) :-
    ejemploImage(X),
    rotate90(X, Y).

% Resultado esperado: La imagen rotada 90 grados.
%   (x,y) = (-y,x).
%X = [3, 3, [[0, 0, '#FF0000', 20], [0, 1, '#FF0000', 20], [0, 2, '#FF0000'|...], [1, 0|...], [1|...], [...|...]|...]],
%Y = [3, 3, [[0, 0, '#FF0000', 20], [-1, 0, '#FF0000', 20], [-2, 0, '#FF0000'|...], [0, 1|...], [-1|...], [...|...]|...]].
ejemplorotate902(Y) :-
    ejemploImage2(X),
    rotate90(X, Y).

% Resultado esperado: La imagen rotada 90 grados.
%   (x,y) = (-y,x).
%X = [2, 2, [[0, 0, 200, 200, 200|...], [0, 1, 200, 200|...], [1, 0, 190|...], [1, 1|...]]],
%Y = [2, 2, [[0, 0, 200, 200, 200|...], [-1, 0, 200, 200|...], [0, 1, 190|...], [-1, 1|...]]].
ejemplorotate903(Y) :-
    ejemploImage3(X),
    rotate90(X, Y).

% ----------------------------------------------
% Ejemplos para el predicado compress
% ----------------------------------------------

% Resultado esperado: La imagen comprimida. Debe entregar la lista con los pixeles que menos se repiten.
% Ninguna expectativa de su funcionamineto. Predicado parcialmente terminado.
ejemplocompress(Y) :-
    ejemploImage(X),
    compress(X, Y).

ejemplocompress2(Y) :-
    ejemploImage2(X),
    compress(X, Y).

ejemplocompress3(Y) :-
    ejemploImage3(X),
    compress(X, Y).

% ----------------------------------------------
% Ejemplos para el predicado changePixel
% ----------------------------------------------

% Resultado: Pixel seleccionado cambiado.
% Ninguna expectativa de su funcionamiento. Predicado parcialemente terminado.
ejemplochangePixel() :-
    ejemploImage(X),
    changePixel(X, 0, 0, 0, 0, 0, Y),
    write(Y).

ejemplochangePixel2() :-
    ejemploImage2(X),
    changePixel(X, 0, 0, 0, 0, 0, Y),
    write(Y).

ejemplochangePixel3() :-
    ejemploImage3(X),
    changePixel(X, 0, 0, 0, 0, 0, Y),
    write(Y).

% ----------------------------------------------
% Ejemplos para el predicado invertColorRGB
% ----------------------------------------------

% Resultado esperado: La imagen con los pixeles RGB invertidos.
% Ejemplo:  pixrgb(1, 1, 190 (R), 0 (G), 234 (B), 255, PD)
% 255 - 190 = 65.
% 255 - 0 = 255.
% 255 - 255 = 0.
% Ninguna expectativa de su funcionamiento. Predicado parcialmente terminado. 
ejemploinvertColorRGB() :-
    ejemploImage3(X),
    invertColorRGB(X, Y),
    write(Y).

ejemploinvertColorRGB2() :-
    ejemploImage4(X),
    invertColorRGB(X, Y),
    write(Y).

ejemploinvertColorRGB3() :-
    ejemploImage5(X),
    invertColorRGB(X, Y),
    write(Y).

% ----------------------------------------------
% Ejemplos para otros predicados no obligatorios
% ----------------------------------------------

%IsPixBit
%-------------

% Resultado esperado: La creación de un PixBit
% PA = [0, 0, 1, 10].
ejemploIsPixBit() :-
    pixbit(0, 0, 1, 10, PA),
    isPixbit(PA).

% Resultado esperado: La creación de un PixBit
% False. Debido a que no cumple con los requerimientos.
ejemploIsPixBit2() :-
    pixbit( 0, 0, '#FF0000', 20, PA),
    isPixbit(PA).

% Resultado esperado: La creación de un PixBit
% False. Debido a que no cumple con los requerimientos.
ejemploIsPixBit3() :-
    pixbit(0, 0, 200, 200, 200, 10, PA),
    isPixbit(PA).

%IsPixRGB
%-------------

% Resultado esperado: La creación de un PixRGB.
% False. Debido a que no cumple con los requerimientos.
ejemploIsPixRGB() :-
    pixrgb(0, 0, 1, 10, PA),
    isPixRGB(PA).

% Resultado esperado: La creación de un PixRGB.
% False. Debido a que no cumple con los requerimientos.
ejemploIsPixRGB2() :-
    pixrgb( 0, 0, '#FF0000', 20, PA),
    isPixRGB(PA).

% Resultado esperado: La creación de un PixRGB.
% PA = [0, 0, 200, 200, 200, 10].
ejemploIsPixRGB3() :-
    pixrgb(0, 0, 200, 200, 200, 10, PA),
    isPixRGB(PA).

%IsPixHex
%-------------

% Resultado esperado: La creación de un PixHex.
% False. Debido a que no cumple con los requerimientos.
ejemploIsPixHex() :-
    pixhex(0, 0, 1, 10, PA),
    isHex(PA).

% Resultado esperado: La creación de un PixHex.
% PA = [0, 0, '#FF0000', 20].
ejemploIsPixHex2() :-
    pixhex( 0, 0, '#FF0000', 20, PA),
    isHex(PA).

% Resultado esperado: La creación de un PixHex.
% False. Debido a que no cumple con los requerimientos.
ejemploIsPixHex3() :-
    pixhex(0, 0, 200, 200, 200, 10, PA),
    isHex(PA).
