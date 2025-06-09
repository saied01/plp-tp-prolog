:- use_module(piezas).


% sublista(+Descartar, +Tomar, +L, -R)?: es verdadero cuando R es la sublista de descartar n elementos de L y luego tomar m elementos.
sublista(Descartar,Tomar,L,R) :- 
    length(Pref, Descartar), 
    length(R, Tomar), 
    append(Pref,Suf,L), 
    append(R,_,Suf).



% fila_vacia(+K, -Fila): generar una sola fila de K variables no instanciadas distintas.
fila_vacia(0, []).
fila_vacia(K, [_|R]) :-
    K > 0,
    K1 is K - 1,
    fila_vacia(K1, R).

% tablero(+K, -T): generar matriz de 5 filas y K columnas, vacias (variables no instanciadas).
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