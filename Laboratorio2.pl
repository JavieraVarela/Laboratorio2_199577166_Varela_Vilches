%TD1: Constructor1
imagen(Altura,Anchura,Pix):- integer(Altura), integer(Anchura), Pix = [].

%Pix necesarios:

%pix1: pixbit
pixbit(X,Y,Bit,Depth):- integer(X), integer(Y), Bit = 1; Bit = 0, integer(Depth).

%pix2: pixrgb

c(X):- integer(X), X >= 0, X =< 255.
pixrgb(X,Y,R,G,B,Depth);- integer(X), integer(Y), c(R), c(G), c(B), integer(Depth).

%pix3: pixhex

pixhex(X,Y,Hex,Depth):- integer(X), integer(Y), string(Hex), integer(Depth).

%------------------------------------------------------------------------------------------------------------------------------

