:- dynamic jugador1/1.
:- dynamic jugador1/2.
:- dynamic jugador2/1.
:- dynamic jugador2/2.
:- dynamic turno/1.
:- dynamic ficha/3.

jugador1(persona).
jugador1(peon,'<').
jugador1(rey, '<<').

jugador2(peon, '>').
jugador2(rey, '>>').

turno(jugador1).

%ficha(1, 2, '<').
%ficha(1, 4, '<').
%ficha(1, 6, '<').
%ficha(1, 8, '<').
%ficha(2, 1, '<').
%ficha(2, 3, '<').
ficha(2, 5, '<').
%ficha(2, 7, '<').
%ficha(6, 3, '<').
%ficha(3, 4, '<').
%ficha(3, 6, '<').
%ficha(3, 8, '<').

ficha(3, 6, '>').
ficha(5, 6, '>').
%ficha(6, 5, '>').
%ficha(6, 7, '>').
%ficha(7, 2, '>').
%ficha(7, 4, '>').
%ficha(7, 6, '>').
%ficha(7, 8, '>').
%ficha(8, 1, '>').
%ficha(8, 3, '>').
%ficha(8, 5, '>').
%ficha(8, 7, '>').

jugar :- hacerPregunta, imprimirTablero.
% jugar :- hacerPregunta, jugada.

hacerMovimiento(jugador1) :- jugador1(Y), elegirTipo(Y).
hacerMovimiento(jugador2) :- jugador2(Y), elegirTipo(Y).

elegirTipo(maquina) :- moverMaquina.
elegirTipo(persona) :- moverPersona.

moverMaquina :- write('juega la maquina '), turno(X), write(X),  write('\n'), imprimirTablero.
moverPersona :- write('juega la persona '), turno(X), write(X),  write('\n'), imprimirTablero.

actualizarTurno(jugador1) :- retract(turno(jugador1)), assert(turno(jugador2)).
actualizarTurno(jugador2) :- retract(turno(jugador2)), assert(turno(jugador1)).

jugada :- turno(X), hacerMovimiento(X).

jugada(X1,Y1,X2,Y2) :- hacerJugada(X1,Y1,X2,Y2).
jugada(_,_,_,_)     :- write('Movimiento Invalido'), jugada.

hacerJugada(X1,Y1,X2,Y2) :- posicionValida(X1,Y1), posicionValida(X2,Y2), turno(Z), fichaValida(X1,Y1,Z), elegirFicha(X1,Y1,X2,Y2,Z), imprimirTablero.

comerDeNuevo(X1,Y1,X2,Y2) :- posicionValida(X1,Y1), posicionValida(X2,Y2), turno(Z), fichaValida(X1,Y1,Z), elegirFicha(X1,Y1,X2,Y2,Z).

comerDeNuevo(_,_,_,_).

elegirFicha(X1,Y1,X2,Y2,Z) :- ficha(X1,Y1,Z), jugador1(W, Z), moverFichaJugador1(X1,Y1,X2,Y2,W).

moverFichaJugador1(X1,Y1,X2,Y2,peon)    :-  (Y2 =:= Y1 - 1 ; Y2 =:= Y1 + 1), X2 =:= X1 + 1, not(ficha(X2,Y2,Z)), jugador1(peon, Z), retract(ficha(X1,Y1,Z)), turno(W), establecerFicha(W,X2,Y2).

moverFichaJugador1(X1,Y1,X2,Y2,peon)    :-  Y2 =:= Y1 + 1 , X2 =:= X1 + 1, ficha(X2,Y2,Z), not(jugador1(_,Z)), jugador2(_,Z), Y3 is Y2 + 1, X3 is X2 + 1,
                                            comerFicha(X1,Y1,X2,Y2,X3,Y3), repetirJugadaJugador1(X3,Y3).

moverFichaJugador1(X1,Y1,X2,Y2,peon)    :-  Y2 =:= Y1 - 1 , X2 =:= X1 + 1, ficha(X2,Y2,Z), not(jugador1(_,Z)), jugador2(_,Z), Y3 is Y2 - 1, X3 is X2 + 1,
                                            comerFicha(X1,Y1,X2,Y2,X3,Y3), repetirJugadaJugador1(X3,Y3).

repetirJugadaJugador1(X,Y)  :- Y2 is Y - 1, X2 is X + 1, comerDeNuevo(X,Y,X2,Y2).

repetirJugadaJugador1(X,Y)  :- Y2 is Y + 1, X2 is X + 1, comerDeNuevo(X,Y,X2,Y2).

comerFicha(X1,Y1,X2,Y2,X3,Y3) :- posicionValida(X3,Y3), casillaVacia(X3,Y3), ficha(X1,Y1,W), ficha(X2,Y2,Z),
                                 retract(ficha(X1,Y1,W)), retract(ficha(X2,Y2,Z)), turno(J), establecerFicha(J,X3,Y3).

casillaVacia(X,Y) :- not(ficha(X,Y,_)).

establecerFicha(jugador1,X,Y)            :-  X =:= 8, jugador1(rey, Z) , assert(ficha(X,Y,Z)).
establecerFicha(jugador1,X,Y)            :-  jugador1(peon, Z) , assert(ficha(X,Y,Z)).

% moverFichaJugador1(X1,Y1,X2,Y2,rey)     :-  

posicionValida(X,Y) :- (X =< 8), (X >= 1), (Y =< 8), (Y >= 1).

fichaValida(X,Y,jugador1) :- ficha(X,Y,Z), jugador1(_,Z).
fichaValida(X,Y,jugador2) :- ficha(X,Y,Z), jugador2(_,Z).

hacerPregunta :- write('\nDesea jugar contra la maquina? (S,N)\n'), read(X), actualizarJugadores(X).

actualizarJugadores('s')    :- assert(jugador2(maquina)).
actualizarJugadores('n')    :- assert(jugador2(persona)).
actualizarJugadores(_)      :- jugar.

imprimirTablero    :-   write('\n'),
                        write('     1    2    3    4    5    6    7    8  \n'),
                        imprimirFilas(1).

imprimirFilas(9).
imprimirFilas(X) :- write(X), write('  |  '), imprimirFila(X), write('\n'), X1 is X + 1, imprimirFilas(X1).

imprimirFila(X) :- imprimirLista(X,1).

imprimirLista(_,9).
imprimirLista(F, C) :- ficha(F,C,Z), write(Z), write(' |  '), C1 is C + 1, imprimirLista(F,C1).
imprimirLista(F, C) :- write('  |  ') , C1 is C + 1, imprimirLista(F,C1).