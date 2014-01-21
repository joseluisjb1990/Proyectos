:- dynamic jugador1/1.
:- dynamic jugador1/2.
:- dynamic jugador2/1.
:- dynamic jugador2/2.
:- dynamic turno/1.
:- dynamic ficha/3.
:- dynamic ganador/1.

jugador1(persona).
jugador1(peon,'<').
jugador1(rey, '<<').

jugador2(peon, '>').
jugador2(rey, '>>').

peon('>').
peon('<').

rey('>>').
rey('<<').

turno(jugador1).

%%%%%%%%%%%%%%%%%%%%%%                  TABLERO             %%%%%%%%%%%%%%%%%%%%

ficha(1, 2, '<').
ficha(1, 4, '<').
ficha(1, 6, '<').
ficha(1, 8, '<').

ficha(2, 1, '<').
ficha(2, 3, '<').
ficha(2, 5, '<').
ficha(2, 7, '<').

ficha(3, 2, '<').
ficha(3, 4, '<').
ficha(3, 6, '<').
ficha(3, 8, '<').


ficha(6, 1, '>').
ficha(6, 3, '>').
ficha(6, 5, '>').
ficha(6, 7, '>').

ficha(7, 2, '>').
ficha(7, 4, '>').
ficha(7, 6, '>').
ficha(7, 8, '>').

ficha(8, 1, '>').
ficha(8, 3, '>').
ficha(8, 5, '>').
ficha(8, 7, '>').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jugar :- hacerPregunta, imprimirTablero, write('\n Turno del jugador 1 \n'), !.

actualizarTurno(jugador1)   :-  retract(turno(jugador1)), assert(turno(jugador2)), 
                                not(juegoTerminado), write('\n Turno del jugador 2 \n'), jugarMaquina.

actualizarTurno(jugador2)   :-  retract(turno(jugador2)), assert(turno(jugador1)), 
                                not(juegoTerminado), write('\n Turno del jugador 1 \n').

actualizarTurno(_)          :-  turno(T), jugadorContrario(T,C), hayGanador(C), !. 

jugada(X1,Y1,X2,Y2) :-  X1 =\= X2 , Y1 =\= Y2 , posicionValida(X1,Y1), posicionValida(X2,Y2), 
                        ficha(X1,Y1,Z), turno(W), apply(W, [F,Z]), moverFicha(X1,Y1,X2,Y2,F), 
                        imprimirTablero, actualizarTurno(W), !.

jugada(_,_,_,_)     :-  ganador(G), hayGanador(G), !.
jugada(_,_,_,_)     :-  write('\n Movimiento Invalido \n'), imprimirTablero, !.

repetirJugada(X,Y)  :-  Y2 is Y + 1, X2 is X + 1, comerDeNuevo(X,Y,X2,Y2).
repetirJugada(X,Y)  :-  Y2 is Y - 1, X2 is X + 1, comerDeNuevo(X,Y,X2,Y2).
repetirJugada(X,Y)  :-  Y2 is Y + 1, X2 is X - 1, comerDeNuevo(X,Y,X2,Y2).
repetirJugada(X,Y)  :-  Y2 is Y - 1, X2 is X - 1, comerDeNuevo(X,Y,X2,Y2).
repetirJugada(_,_)  :-  !.

comerDeNuevo(X1,Y1,X2,Y2)   :-   posicionValida(X2,Y2), not(casillaVacia(X2,Y2)), 
                                 ficha(X1,Y1,Z), turno(W), apply(W, [F,Z]), moverFicha(X1,Y1,X2,Y2,F).

moverFicha(X1,Y1,X2,Y2,peon)    :-  casillaVacia(X2,Y2), movimientoValido(X1,Y1,X2,Y2,peon,_), 
                                    moverVacio(X1,Y1,X2,Y2).

moverFicha(X1,Y1,X2,Y2,peon)    :-  turno(W), verificarCasilla(X2,Y2,W), movimientoValido(X1,Y1,X2,Y2,peon,Z), 
                                    comerFicha(X1,Y1,X2,Y2,Z).

moverFicha(X1,Y1,X2,Y2,rey) :-  casillaVacia(X2,Y2), movimientoValido(X1,Y1,X2,Y2,rey,_), 
                                moverVacio(X1,Y1,X2,Y2).

moverFicha(X1,Y1,X2,Y2,rey) :-  turno(W), verificarCasilla(X2,Y2,W), movimientoValido(X1,Y1,X2,Y2,rey,Z), 
                                comerFicha(X1,Y1,X2,Y2,Z).

moverVacio(X1,Y1,X2,Y2) :- buscarFicha(X1,Y1,X2,Y2,F), retract(ficha(X1,Y1,_)), assert(ficha(X2,Y2,F)).

movimientoValido(X1,Y1,X2,Y2,peon,derecha)          :-  turno(jugador1),  X2 =:= X1 + 1, Y2 =:= Y1 + 1.
movimientoValido(X1,Y1,X2,Y2,peon,izquierda)        :-  turno(jugador1),  X2 =:= X1 + 1, Y2 =:= Y1 - 1.
movimientoValido(X1,Y1,X2,Y2,peon,antiderecha)      :-  turno(jugador2),  X2 =:= X1 - 1, Y2 =:= Y1 + 1.
movimientoValido(X1,Y1,X2,Y2,peon,antizquierda)     :-  turno(jugador2),  X2 =:= X1 - 1, Y2 =:= Y1 - 1.
movimientoValido(X1,Y1,X2,Y2,rey,derecha)           :-  diagonalDerecha(X1,Y1,X2,Y2).
movimientoValido(X1,Y1,X2,Y2,rey,izquierda)         :-  diagonalIzquierda(X1,Y1,X2,Y2).
movimientoValido(X1,Y1,X2,Y2,rey,antiderecha)       :-  antidiagonalDerecha(X1,Y1,X2,Y2).
movimientoValido(X1,Y1,X2,Y2,rey,antizquierda)      :-  antidiagonalIzquierda(X1,Y1,X2,Y2).

comerFicha(X1,Y1,X2,Y2,derecha)         :-  X3 is X2 + 1, Y3 is Y2 + 1, posicionValida(X3,Y3), 
                                            casillaVacia(X3,Y3), buscarFicha(X1,Y1,X3,Y3,Z), 
                                            retract(ficha(X1,Y1,_)), retract(ficha(X2,Y2,_)), 
                                            assert(ficha(X3,Y3,Z)), repetirJugada(X3,Y3) , !.

comerFicha(X1,Y1,X2,Y2,izquierda)       :-  X3 is X2 + 1, Y3 is Y2 - 1, posicionValida(X3,Y3), 
                                            casillaVacia(X3,Y3), buscarFicha(X1,Y1,X3,Y3,Z), 
                                            retract(ficha(X1,Y1,_)), retract(ficha(X2,Y2,_)), 
                                            assert(ficha(X3,Y3,Z)), repetirJugada(X3,Y3), !.

comerFicha(X1,Y1,X2,Y2,antiderecha)     :-  X3 is X2 - 1, Y3 is Y2 + 1, posicionValida(X3,Y3), 
                                            casillaVacia(X3,Y3), buscarFicha(X1,Y1,X3,Y3,Z), 
                                            retract(ficha(X1,Y1,_)), retract(ficha(X2,Y2,_)), 
                                            assert(ficha(X3,Y3,Z)), repetirJugada(X3,Y3), !.

comerFicha(X1,Y1,X2,Y2,antizquierda)    :-  X3 is X2 - 1, Y3 is Y2 - 1, posicionValida(X3,Y3), 
                                            casillaVacia(X3,Y3), buscarFicha(X1,Y1,X3,Y3,Z), 
                                            retract(ficha(X1,Y1,_)), retract(ficha(X2,Y2,_)), 
                                            assert(ficha(X3,Y3,Z)), repetirJugada(X3,Y3), !.

buscarFicha(X1,Y1,_,_,Z)    :- ficha(X1,Y1,Z), rey(Z).
buscarFicha(_,_,X2,_,Z)     :- turno(T), verificarRey(X2,T), apply(T,[rey,Z]).
buscarFicha(_,_,_,_,Z)      :- turno(T), apply(T,[peon, Z]).

verificarRey(X,jugador1)    :-  X =:= 8.
verificarRey(X,jugador2)    :-  X =:= 1.

establecerFicha(X,Y,_,_,_)              :-  retract(ficha(X,Y,_)), fail.
establecerFicha(X1,_,X2,Y2,jugador1)    :-  X1 =:= 8, jugador1(rey, Z) , assert(ficha(X2,Y2,Z)).
establecerFicha(_,_,X2,Y2,jugador1)     :-  jugador1(peon, Z) , assert(ficha(X2,Y2,Z)).
establecerFicha(X1,_,X2,Y2,jugador2)    :-  X1 =:= 1, jugador2(rey, Z) , assert(ficha(X2,Y2,Z)).
establecerFicha(_,_,X2,Y2,jugador2)     :-  jugador2(peon, Z) , assert(ficha(X2,Y2,Z)).

verificarCasilla(X,Y,W) :-  apply(W, [peon, Z1]), not(ficha(X,Y,Z1)), apply(W, [rey, Z2]), not(ficha(X,Y,Z2)).

moverRey(X1,Y1,X2,Y2,_) :-  casillaVacia(X2,Y2) , ficha(X1,Y1,Z) , retract(ficha(X1,Y1,Z)), assert(ficha(X2,Y2,Z)).

diagonalDerecha(X1,Y1,X2,Y2)          :-  X2 > X1, Y2 > Y1, X3 is X2 - X1, Y3 is Y2 - Y1, 
                                          Y3 =:= X3, X4 is X1 + 1, Y4 is Y1 + 1, caminoLibre(X4,Y4,X2,Y2,derecha).

diagonalIzquierda(X1,Y1,X2,Y2)        :-  X2 > X1, Y1 > Y2, X3 is X2 - X1, Y3 is Y1 - Y2, Y3 =:= X3, X4 is X1 + 1, 
                                          Y4 is Y1 - 1, caminoLibre(X4,Y4,X2,Y2,izquierda).

antidiagonalDerecha(X1,Y1,X2,Y2)      :-  X1 > X2, Y2 > Y1, X3 is X1 - X2, Y3 is Y2 - Y1, Y3 =:= X3, X4 is X1 - 1, 
                                          Y4 is Y1 + 1, caminoLibre(X4,Y4,X2,Y2,antiderecha).

antidiagonalIzquierda(X1,Y1,X2,Y2)    :-  X1 > X2, Y1 > Y2, X3 is X1 - X2, Y3 is Y1 - Y2, Y3 =:= X3, X4 is X1 - 1, 
                                          Y4 is Y1 - 1, caminoLibre(X4,Y4,X2,Y2,antizquierda).

caminoLibre(X,Y,X,Y,_)                  :- !.
caminoLibre(X1,Y1,X2,Y2,derecha)        :- casillaVacia(X1,Y1), X3 is X1 + 1, Y3 is Y1 + 1, caminoLibre(X3,Y3,X2,Y2,derecha).
caminoLibre(X1,Y1,X2,Y2,izquierda)      :- casillaVacia(X1,Y1), X3 is X1 + 1, Y3 is Y1 - 1, caminoLibre(X3,Y3,X2,Y2, izquierda).
caminoLibre(X1,Y1,X2,Y2,antiderecha)    :- casillaVacia(X1,Y1), X3 is X1 - 1, Y3 is Y1 + 1, caminoLibre(X3,Y3,X2,Y2,antiderecha).
caminoLibre(X1,Y1,X2,Y2,antizquierda)   :- casillaVacia(X1,Y1), X3 is X1 - 1, Y3 is Y1 - 1, caminoLibre(X3,Y3,X2,Y2,antizquierda).

casillaVacia(X,Y) :- not(ficha(X,Y,_)).

posicionValida(X,Y) :- (X =< 8), (X >= 1), (Y =< 8), (Y >= 1).

hacerPregunta :- write('\nDesea jugar contra la maquina? (S,N)\n'), read(X), actualizarJugadores(X).

actualizarJugadores('s')    :-  assert(jugador2(maquina)).
actualizarJugadores('n')    :-  assert(jugador2(persona)).
actualizarJugadores(_)      :-  hacerPregunta.

juegoTerminado  :-  movimientosPosibles(L), length(L,N), N =:=0.

hayGanador(jugador1)    :- write('\n Ha ganado el jugador 1 \n'), !.
hayGanador(jugador2)    :- write('\n Ha ganado el jugador 2 \n'), !.

jugadorContrario(jugador1,jugador2) :- !.
jugadorContrario(jugador2,jugador1) :- !.

casillaPermitida(_,ficha(X,Y),vacia)    :-  posicionValida(X,Y), casillaVacia(X,Y).
casillaPermitida(An,ficha(X,Y),comer)   :-  posicionValida(X,Y), verificarCasilla(X,Y,jugador2), 
                                            verificarSiguiente(An,ficha(X,Y)).

verificarSiguiente(ficha(X1,Y1) , ficha(X2,Y2)) :- X2 > X1, Y2 > Y1, X3 is X2 + 1 , Y3 is Y2 + 1, posicionValida(X3,Y3), casillaVacia(X3,Y3).
verificarSiguiente(ficha(X1,Y1) , ficha(X2,Y2)) :- X2 < X1, Y2 > Y1, X3 is X2 - 1 , Y3 is Y2 + 1, posicionValida(X3,Y3), casillaVacia(X3,Y3).
verificarSiguiente(ficha(X1,Y1) , ficha(X2,Y2)) :- X2 > X1, Y2 < Y1, X3 is X2 + 1 , Y3 is Y2 - 1, posicionValida(X3,Y3), casillaVacia(X3,Y3).
verificarSiguiente(ficha(X1,Y1) , ficha(X2,Y2)) :- X2 < X1, Y2 < Y1, X3 is X2 - 1 , Y3 is Y2 - 1, posicionValida(X3,Y3), casillaVacia(X3,Y3).

agregarFicha(An,Ac,Z,R)     :- casillaPermitida(An,Ac,P), append(Z,[par(An,Ac,P)],R).
agregarFicha(_,_,Acc,Acc)   :- !.

movimientoPosible(ficha(X,Y,'>'),Z) :-  X1 is X - 1, Y1 is Y - 1,
                                        X2 is X - 1, Y2 is Y + 1,
                                        Z = [par(ficha(X,Y),ficha(X1,Y1)), par(ficha(X,Y),ficha(X2,Y2))].
                                         
movimientoPosible(ficha(X,Y,'>>'),Z)    :-  agregarDerecha(X,Y,X,Y,[],R1), 
                                            agregarIzquierda(X,Y,X,Y,R1,R2),
                                            agregarAntiderecha(X,Y,X,Y,R2,R3),
                                            agregarAntizquierda(X,Y,X,Y,R3,Z).

agregarDerecha(X0,Y0,X,Y,E,S)       :-  X1 is X + 1, Y1 is Y + 1, continuar(X0,Y0,X1,Y1,E,Aux,agregarDerecha), 
                                        agregarLista(X0,Y0,X1,Y1,Aux,S), !.
agregarIzquierda(X0,Y0,X,Y,E,S)     :-  X1 is X + 1, Y1 is Y - 1, continuar(X0,Y0,X1,Y1,E,Aux, agregarIzquierda), 
                                        agregarLista(X0,Y0,X1,Y1,Aux,S), !. 
agregarAntiderecha(X0,Y0,X,Y,E,S)   :-  X1 is X - 1, Y1 is Y + 1, continuar(X0,Y0,X1,Y1,E,Aux, agregarAntiderecha), 
                                        agregarLista(X0,Y0,X1,Y1,Aux,S), !. 
agregarAntizquierda(X0,Y0,X,Y,E,S)  :-  X1 is X - 1, Y1 is Y - 1, continuar(X0,Y0,X1,Y1,E,Aux, agregarAntizquierda), 
                                        agregarLista(X0,Y0,X1,Y1,Aux,S), !. 

agregarLista(X0,Y0,X1,Y1,L,S)   :-  posicionValida(X1,Y1), append(L,[par(ficha(X0,Y0),ficha(X1,Y1))],S).
agregarLista(_,_,_,_,L,L)       :-  !.

continuar(X0,Y0,X,Y,E,S,F)  :- posicionValida(X,Y), casillaVacia(X,Y), apply(F, [X0,Y0,X,Y,E,S]).
continuar(_,_,_,_,E,E,_)    :- !.

verificarMovimientos([],Acc,Acc)                :-  !.
verificarMovimientos([par(An,Ac) | Tail],Acc,R) :-  agregarFicha(An,Ac, Acc, RAcc), verificarMovimientos(Tail, RAcc, R).

listaMovimientos([],R,R)            :-  !.
listaMovimientos([X | Tail],Z,S)    :-  apply(movimientoPosible,[X,L]), verificarMovimientos(L,[],R), 
                                        append(Z,R,Aux), listaMovimientos(Tail, Aux,S), !.

seleccionarRandom(L,S)    :-  random(R), length(L,N), F is 1/N, seleccionarElemento(L,R,F,S).

seleccionarElemento([X | _ ], R, F,X)   :-  R < F.
seleccionarElemento([_ | Tail], R, F,S) :-  R >= F, N is F + F, seleccionarElemento(Tail, R, N, S).

aplicarJugada(par(ficha(X1,Y1),ficha(X2,Y2))) :- jugada(X1,Y1,X2,Y2).

moverMaquina :- movimientosPosibles(R), separarPorComer(R,[],[],V,C), seleccionarJugada(C,V,P) , aplicarJugada(P), !.

seleccionarJugada([],V,S) :- seleccionarRandom(V,S), !.
seleccionarJugada(C,_,S)  :- seleccionarRandom(C,S), !.

separarPorComer([], AccV,AccC,AccV,AccC)                                        :-  !.
separarPorComer([par(ficha(X,Y), ficha(X1,Y1), comer) | Tail],AccV,AccC,SV,SC)  :-  append(AccC,[par(ficha(X,Y), ficha(X1,Y1))],Aux), 
                                                                                    separarPorComer(Tail,AccV,Aux,SV,SC), !.

separarPorComer([par(ficha(X,Y), ficha(X1,Y1), vacia) | Tail],AccV,AccC,SV,SC)  :-  append(AccV,[par(ficha(X,Y), ficha(X1,Y1))],Aux), 
                                                                                    separarPorComer(Tail,Aux,AccC,SV,SC), !.

jugarMaquina    :-  jugador2(maquina), moverMaquina, !.
jugarMaquina    :-  !.

movimientosPosibles(R)  :-  turno(T), apply(T,[peon,Peon]), findall(ficha(X,Y,Peon),ficha(X,Y,Peon),L1), 
                            apply(T,[rey,Rey]), findall(ficha(X,Y,Rey),ficha(X,Y,Rey),L2), append(L1, L2, L), 
                            listaMovimientos(L,[],R),!.

%%%%%%%%%%%%%%%%%%%%%%%         IMPRESION DEL TABLERO   %%%%%%%%%%%%%%%%%%%%%%%%

imprimirTablero    :-   write('\n'),
                        write('     1    2    3    4    5    6    7    8  \n'),
                        imprimirFilas(1),
                        !.

imprimirFilas(9).
imprimirFilas(X) :- write(X), write('  |  '), imprimirFila(X), write('\n'), X1 is X + 1, imprimirFilas(X1).

imprimirFila(X) :- imprimirLista(X,1).

imprimirLista(_,9).
imprimirLista(F, C) :- ficha(F,C,Z), peon(Z), write(Z), write(' |  '), C1 is C + 1, imprimirLista(F,C1).
imprimirLista(F, C) :- ficha(F,C,Z), rey(Z), write(Z), write('|  '), C1 is C + 1, imprimirLista(F,C1).

imprimirLista(F, C) :- write('  |  ') , C1 is C + 1, imprimirLista(F,C1).

%%%%%%%%%%%%%%%%%%%%%%          PREDICADOS PARA DECIDIR LA JUGADA   %%%%%%%%%%%%
