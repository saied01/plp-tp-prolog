:- use_module(piezas).


% sublista(+Descartar, +Tomar, +L, -R)?: es verdadero cuando R es la sublista de descartar n elementos de L y luego tomar m elementos.
sublista(Descartar,Tomar,L,R) :- 
    length(Pref, Descartar), 
    length(R, Tomar), 
    append(Pref,Suf,L), 
    append(R,_,Suf).

% El predicado sublista es reversible para 'R' (ultimo argumento) pero NO lo es para 'Descartar' (primer argumento).
% Si hacemos un llamado por ejemplo: sublista(N,3,[a,b,c,d,e,f],[c,d,e]). El predicado devuelve N=2, que es correcto
% ya que se necesitan descartar dos elementos para que sea verdadero que [c,d,e] es la sublista correcta.
% Pero luego si le pedimos mas valores se cuelga, esto ocurre ya que length no es reversible en el primer argumento.
% Entonces lo que ocurre es que ength sigue probando nuevos valores de N para el largo de la lista no instanciada,
% pero como no hay ningun valor valido, sigue aumentando N infinitamente y se cuelga.
    

% fila_vacia(+K, -Fila): generar una sola fila de K variables no instanciadas distintas.
fila_vacia(0, []).
fila_vacia(K, [_|R]) :-
    K > 0,
    K1 is K - 1,
    fila_vacia(K1, R).

% tablero(+K, -T): generar la matriz repitiendo las K columnas 5 veces, vacias (variables no instanciadas).
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



% coordenadas(+T, -IJ) debe ser verdadero para todo par IJ que sea una coordenada de elementos del tablero.
coordenadas(T, IJ) :-
    IJ = (I,J),
    tamanio(T, F, C),
    between(1, F, I),
    between(1, C, J).


% kPiezas(+K, -PS) debe ser verdadero cuando PS es una lista de longitud K de identificadores de piezas.
kPiezas(K, PS) :- nombrePiezas(L), elegirK(K, L, PS).

% elegirK(+K, +KL, -PS) elige las piezas desde la lista "nombePiezas". Tiene 3 casos: 
% Base: si K = 0, la lista que corresponde es la vacia.
% Elige el K actual: La cabeza de la lista de nombres coincide con la cabeza de la lista de kPiezas, y K disminuye en 1.
% No elige el K actual: Las cabezas de las listas no coinciden por lo que PS se mantiene y K no disminuye.
elegirK(0, _, []).
elegirK(K, [H | KT], [H | PSt]) :- 
    K > 0, 
    K1 is K - 1, 
    elegirK(K1, KT, PSt).
elegirK(K, [_ | KT], PS) :- 
    K > 0, 
    elegirK(K, KT, PS).


% seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)
seccionTablero(T, ALTO, ANCHO, (I, J), ST) :- Iindex is I-1, Jindex is J-1, 
                                              sublista(Iindex, ALTO, T, RecorteALTO), 
                                              recorteDeListas(Jindex, ANCHO, RecorteALTO, ST).


recorteDeListas(DESCARTAR, TOMAR, [OriginalHead|[]], [RecortadaHead|[]]) :- 
    sublista(DESCARTAR, TOMAR, OriginalHead, RecortadaHead).

recorteDeListas(DESCARTAR, TOMAR, [OriginalHead|OriginalTail], [RecortadaHead|RecortadaTail]) :- 
    sublista(DESCARTAR, TOMAR, OriginalHead, RecortadaHead), 
    recorteDeListas(DESCARTAR, TOMAR, OriginalTail, RecortadaTail).


ubicarPieza(T, LETRA) :- pieza(LETRA, PIEZA), 
                         tamanio(PIEZA, AltoPieza, AnchoPieza), tamanio(T, AltoT, AnchoT),
                         between(1, AnchoT, J), between(1, AltoT, I),
                         seccionTablero(T, AltoPieza, AnchoPieza, (I,J), PIEZA).



% ubicarPiezas(+Tablero, +Poda, +Identificadores) lista todas las posibles opciones de ubicar todas las piezas mencionadas en Identificadores.
ubicarPiezas(_, _, []).
ubicarPiezas(T, P, [Ihead | Itail]) :- ubicarPieza(T, Ihead), ubicarPiezas(T, P, Itail).

% poda(+Poda, +Tablero) es verdadero si el tablero satisface la poda.
poda(sinPoda, _).



% llenarTablero(+Poda, +Columnas, -Tablero) enumera todas las formas distintas de llenar un tablero con la cantidad de columnas (y piezas) indicada.
llenarTablero(P, Cols, T) :- 
    tablero(Cols, T), 
    kPiezas(Cols, Piezas), 
    ubicarPiezas(T, P, Piezas).


cantSoluciones(Poda, Columnas, N) :-
    findall(T, llenarTablero(Poda, Columnas, T), TS),
    length(TS, N).

% Para K = 3:
% % 21,958,525 inferences, 1.222 CPU in 1.222 seconds (100% CPU, 17963441 Lips)
% N = 28.
% Para K = 4:
% % 817,021,495 inferences, 45.270 CPU in 45.271 seconds (100% CPU, 18047606 Lips)
% N = 200.
% No probamos con K = 5 ya que sin poda tardaria mucho tiempo.


poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).
todosGruposLibresModulo5(T) :- 
    findall((I,J), 
    (coordenadas(T, (I,J)), 
    libre(T, (I,J))), Lista),
    agrupar(CoordLibres, Grupos),
    maplist(length, Grupos, Largos),
    maplist(modulo5, Largos).