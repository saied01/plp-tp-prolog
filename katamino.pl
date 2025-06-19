:- use_module(piezas).


% sublista(+Descartar, +Tomar, +L, -R): es verdadero cuando R es la sublista de descartar Descartar elementos de L y luego tomar Tomar elementos.
sublista(Descartar,Tomar,L,R) :- 
    length(Pref, Descartar), 
    length(R, Tomar), 
    append(Pref,Suf,L), 
    append(R,_,Suf).

% El predicado sublista es reversible para 'R' (último argumento) pero NO lo es para 'Descartar' (primer argumento).
% Si hacemos un llamado por ejemplo: sublista(N,3,[a,b,c,d,e,f],[c,d,e]). El predicado devuelve N=2, que es correcto
% ya que se necesitan descartar dos elementos para que sea verdadero que [c,d,e] es la sublista correcta.
% Pero luego si le pedimos más valores se cuelga. Esto ocurre ya que length no es reversible en el primer argumento.
% Entonces lo que ocurre es que length sigue probando nuevos valores de N para el largo de la lista no instanciada,
% pero como no hay ningún valor válido, sigue aumentando N infinitamente y se cuelga.
    

% fila_vacia(+K, -Fila): es verdadero cuando Fila es una lista de K variables (no instanciadas).
fila_vacia(0, []).
fila_vacia(K, [_|R]) :-
    K > 0,
    K1 is K - 1,
    fila_vacia(K1, R).

% tablero(+K, -T): es verdadero cuando T es una matriz de 5 filas y Cols columnas, todas con variables (no instanciadas).
tablero(K, T) :- 
    K > 0,
    fila_vacia(K, F1),
    fila_vacia(K, F2),
    fila_vacia(K, F3),
    fila_vacia(K, F4),
    fila_vacia(K, F5),
    T = [F1, F2, F3, F4, F5].


% tamanio(+M, -F, -C) es verdadero cuando M tenga F filas y C columnas. (definido con "ni" porque sino tiene problemas).
tamanio(M, F, C) :- length(M, F), contarColumnas(M, C).

% contarColumnas(+M,-C) es verdadero cuando todas las filas de M tienen largo C. El caso base asume que nunca se llama al
% predicado con una lista vacia de antemano.
contarColumnas([], _). 
contarColumnas([Mh|Mt],C) :- length(Mh, C), contarColumnas(Mt, C).



% coordenadas(+T, -IJ) es verdadero para todo par (I,J) que sea una coordenada de elementos del tablero.
coordenadas(T, (I,J)) :-
    tamanio(T, F, C),
    between(1, F, I),
    between(1, C, J).


% kPiezas(+K, -PS) es verdadero cuando PS es una lista de longitud K de identificadores de piezas.
kPiezas(K, PS) :- nombrePiezas(L), elegirK(K, L, PS).

% elegirK(+K, +KL, -PS) elige las piezas desde la lista "nombePiezas". Tiene 3 casos: 
% Base: Para K=0 la única lista que unifica es la vacía.
% Elige el K actual: Es verdadero cuando la cabeza de la lista de nombres coincide con la cabeza de la lista de kPiezas. K disminuye en 1.
% No elige el K actual: Es verdadero cuando las cabezas de las listas no coinciden por lo que PS se mantiene y K no disminuye.
elegirK(0, _, []).
elegirK(K, [H | KT], [H | PSt]) :- 
    K > 0, 
    K1 is K - 1, 
    elegirK(K1, KT, PSt).
elegirK(K, [_ | KT], PS) :- 
    K > 0, 
    elegirK(K, KT, PS).


% seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST) es verdadero cuando ST es una sección de tamaño ALTO*ANCHO del tablero T a partir de la coordenada IJ.
seccionTablero(T, ALTO, ANCHO, (I, J), ST) :- Iindex is I-1, Jindex is J-1, 
                                              sublista(Iindex, ALTO, T, RecorteALTO), 
                                              recorteDeListas(Jindex, ANCHO, RecorteALTO, ST).

recorteDeListas(DESCARTAR, TOMAR, [OriginalHead|[]], [RecortadaHead|[]]) :- 
    sublista(DESCARTAR, TOMAR, OriginalHead, RecortadaHead).

recorteDeListas(DESCARTAR, TOMAR, [OriginalHead|OriginalTail], [RecortadaHead|RecortadaTail]) :- 
    sublista(DESCARTAR, TOMAR, OriginalHead, RecortadaHead), 
    recorteDeListas(DESCARTAR, TOMAR, OriginalTail, RecortadaTail).


% ubicarPieza(+Tablero, +Identificador) es verdadero cuando la pieza Identificador puede ubicarse en alguna posición del Tablero T.
ubicarPieza(T, LETRA) :- pieza(LETRA, PIEZA), 
                         tamanio(PIEZA, AltoPieza, AnchoPieza), 
                         tamanio(T, AltoT, AnchoT),
                         between(1, AnchoT, J), 
                         between(1, AltoT, I),
                         seccionTablero(T, AltoPieza, AnchoPieza, (I,J), PIEZA).



% ubicarPiezas(+Tablero, +Poda, +Identificadores) lista todas las posibles opciones de ubicar todas las piezas mencionadas en Identificadores.
% Es verdadero cuando se logra ubicar todas las piezas en Identificadores, sobre el Tablero T, utilizando la poda Poda. 
% Si la poda es sinPoda, no se realiza ningún chequeo entre colocaciones. Si se especifica otra estrategia,
% se aplica despues de ubicar cada pieza para descartar tableros inválidos de forma anticipada.
ubicarPiezas(_, _, []).
ubicarPiezas(T, sinPoda, [Ihead | Itail]) :-
    ubicarPieza(T, Ihead),
    ubicarPiezas(T, sinPoda, Itail).
ubicarPiezas(T, P, [Ihead | Itail]) :-
    P \= sinPoda,
    ubicarPieza(T, Ihead),
    poda(P, T),
    ubicarPiezas(T, P, Itail).


% llenarTablero(+Poda, +Columnas, -Tablero) enumera todas las formas distintas de llenar un tablero con la cantidad de columnas (y piezas) indicada.
llenarTablero(P, Cols, T) :- 
    tablero(Cols, T), 
    kPiezas(Cols, Piezas), 
    ubicarPiezas(T, P, Piezas).


% cantSoluciones(+Poda, +Columnas, -N) es verdadero cuando N es igual a la cantidad de soluciones posibles
% para un tablero con 'Columnas' columnas.
cantSoluciones(Poda, Columnas, N) :-
    findall(T, llenarTablero(Poda, Columnas, T), TS),
    length(TS, N).

% Para K = 3:
% % 21,958,525 inferences, 1.222 CPU in 1.222 seconds (100% CPU, 17963441 Lips)
% N = 28.
% Para K = 4:
% % 817,021,495 inferences, 45.270 CPU in 45.271 seconds (100% CPU, 18047606 Lips)
% N = 200.
% No probamos con K = 5 ya que sin poda tardaría mucho tiempo.


% poda(+Poda, +Tablero) es verdadero si el tablero satisface la poda.
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).
todosGruposLibresModulo5(T) :- 
    posicionesLibres(T, CoordLibres),
    agrupar(CoordLibres, Grupos),
    maplist(length, Grupos, Largos),
    maplist(mod5, Largos).


posicionesLibres(T, CoordLibres) :- 
    findall((I, J), (coordenadas(T, (I, J)), esLibre(T, (I, J))),CoordLibres).

esLibre(T, (I,J)) :- 
    nth1(I, T, Fila),
    nth1(J, Fila, Elem),
    var(Elem).

mod5(N) :- N mod 5 =:= 0.
