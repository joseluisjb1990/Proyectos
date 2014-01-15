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

% jugar :- hacerPregunta, imprimirTablero, hacerMovimiento(jugador1(X)).
jugar :- hacerPregunta, jugada.

hacerMovimiento(jugador1) :- jugador1(Y), elegirTipo(Y).
hacerMovimiento(jugador2) :- jugador2(Y), elegirTipo(Y).

elegirTipo(maquina) :- moverMaquina.
elegirTipo(persona) :- moverPersona.

moverMaquina :- write('juega la maquina '), turno(X), write(X),  write('\n').
moverPersona :- write('juega la persona '), turno(X), write(X),  write('\n').

actualizarTurno(jugador1) :- retract(turno(jugador1)), assert(turno(jugador2)).
actualizarTurno(jugador2) :- retract(turno(jugador2)), assert(turno(jugador1)).

jugada :- turno(X), hacerMovimiento(X).

jugada(X1,Y1,X2,Y2) :- fichaValida(X1,Y1), posicionValida(X2,Y2),turno(Z), elegirFicha(X1,Y1,X2,Y2,Z).

elegirFicha(X1,Y1,X2,Y2,jugador1) :- ficha(X1,Y1,Z), jugador1(W, Z), moverFichaJugador1(X1,Y1,X2,Y2,W).

moverFichaJugador1(X1,Y1,X2,Y2,peon)    :- 
moverFichaJugador1(X1,Y1,X2,Y2,rey)     :-  

posicionValida(X,Y) :- X <= 8, X >= 1, Y <= 8, Y >= 1.

fichaValida(X,Y) :- posicionValida(X,Y), turno(Z), verificarFicha(X,Y,Z).
fichaValida(_,_) :- write('No puedes mover la ficha seleccionada'), jugada.

verificarFicha(X,Y,jugador1) :- ficha(X,Y,Z), jugador1(_,Z).
verificarFicha(X,Y,jugador2) :- ficha(X,Y,Z), jugador2(_,Z).

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

