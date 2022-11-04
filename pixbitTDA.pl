% ------------------------------------------------------------
% |TDA: pixbit                                               |
% ------------------------------------------------------------

% Descripción: Predicado constructor que permite obtener la estructura de un pixbit. Este TDA se representa a traves de una lista
% que almacena elementos de diferente caracteristicas. 
% x -> Coordenada X en un plano carteciano. Entero.
% y -> Coordenada Y en un plano carteciano. Entero.
% Bit -> Valor del bit. 0 o 1. Entero.
% Depth -> Profundidad del bit. Entero.

/* Predicados:
    - pixbit(X,Y,Bit,Depth,Pix)                                         Aridad: 5.
    - isPixbit(Pix)                                                     Aridad: 1.
*/

/* Metas primaria:
    - pixbit(X,Y,Bit,Depth,Pix)                                                                                     
*/

/* Metas secundarias:
    - isPixbit(Pix)                                                                                                 
*/

% Clausulas:

% Reglas:

% Predicados:

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