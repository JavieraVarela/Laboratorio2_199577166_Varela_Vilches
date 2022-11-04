% ------------------------------------------------------------
% |TDA: PixHex                                               |
% ------------------------------------------------------------

% Descripción: Predicado que permite obtener la estrucura de un pixhex. Este TDA se utiliza para representar un pixel en formato 
% hexadecimal. Es representado a traves de una lista que almacena elementos de diferentes caracteristicas. 
% X -> Coordenada X del pixel en el plano cartesiano. Entero.
% Y -> Coordenada Y del pixel en el plano cartesiano. Entero.
% Hex -> Contiene el color en valor hexadecimal. String.
% Depth -> Profundidad del pixel. Entero.

/* Predicados:
    - pixHex(X, Y, Hex, Depth, Pix)                                     Aridad: 5.
    - isPixHex(Pix)                                                     Aridad: 1.
*/

/* Metas primarias:
    - pixHex(X, Y, Hex, Depth, Pix)                            
*/

/* Metas secundarias:
    - isPixHex(Pix)                                                    
*/

% Clausulas: 

% Reglas:

% Predicados:

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
    