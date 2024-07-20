%%%% -*- Mode: Prolog -*-
% testo_progetto.pl

% MINIMUM SPANNING TREES

% Componenti del gruppo

% Studente Alessandro Biagiotti
% Numero di matricola: 869014

% Studente Youssef Benchaib
% Numero di matricola: 844515

% Studente Giacomo Elemi
% Numero di matricola: 806904

% Sezione di definizione di predicati dinamici

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic arc/3.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.


% SEZIONE DI CODIFICA DELL'INTERFACCIA

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

delete_graph(G) :-
    vertex_key(G, _, _),
    retractall(vertex_key(G, _, _)),
    vertex_previous(G, _, _),
    retractall(vertex_previous(G, _, _)),
    graph(G),
    retract(graph(G)),
    retractall(vertex(G, _)), retractall(arc(G, _, _, _)), !.

delete_graph(G) :-
    graph(G),
    retract(graph(G)),
    retractall(vertex(G, _)), retractall(arc(G, _, _, _)), !.

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- new_graph(G),
		    assert(vertex(G, V)), !.

graph_vertices(G, Vs) :-
    graph(G),
    findall(vertex(G, V), vertex(G, V), Vs).

list_vertices(G) :- listing(vertex(G, _)).

new_arc(_, X, X, _) :- !.
new_arc(G, X, Y, W) :- arc(G, X, Y, W1),
		       retract(arc(G, X, Y, W1)),
		       retract(arc(G, Y, X, W1)),
		       assert(arc(G, X, Y, W)),
		       assert(arc(G, Y, X, W)), !.
new_arc(G, X, Y, W) :- new_vertex(G, X),
		       new_vertex(G, Y), assert(arc(G, X, Y, W)), !,
		       assert(arc(G, Y, X, W)), !.

graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, X, Y, W), arc(G, X, Y, W), Es).

vertex_neighbors(G, V, List) :-
    graph(G),
    vertex(G, V),
    findall(arc(G, V, Y, W), arc(G, V, Y, W), List).


adjs(G, V, Vs) :-
    graph(G),
    vertex(G, V),
    findall(vertex(G, Y), arc(G, V, Y, _), List),
    list_check(List, V, Vs).

% Controllo che i vertici che mi ha passato la findall siano
% effettivamente collegati da archi doppi e non singoli
list_check([], _, []) :- !.
list_check([vertex(G, X)| T], V, [vertex(G, X) | T1]) :-
    arc(G, X, V, _),
    arc(G, V, X, _),
    list_check(T, V, T1), !.
list_check([_ | T], V, T1) :-
    list_check(T, V, T1), !.


list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

list_graph(G) :-
    graph(G),
    listing(vertex(G, _)),
    listing(arc(G, _, _, _)).

read_graph(G, FileName) :-
    graph(G),
    delete_graph(G),
    new_graph(G),
    csv_read_file(FileName, Es,
                  [functor(arc), arity(3), separator(0'\t)]),
    new_arcList(Es, G), !.

read_graph(G, FileName) :-
    new_graph(G),
    csv_read_file(FileName, Es,
                  [functor(arc), arity(3), separator(0'\t)]),
    new_arcList(Es, G), !.

new_arcList([], _).
new_arcList([arc(X, Y, W) | T], G) :-
    new_arc(G, X, Y, W), new_arcList(T, G).

write_graph(G, FileName) :- write_graph(G, FileName, graph).

write_graph(G, FileName, graph) :-
    graph(G),
    graph_arcs(G, Es),
    arc_conv(Es, List),
    csv_write_file(FileName, List, [separator(0'\t)]), !.
write_graph(G, FileName, edges) :-
    arc_conv(G, Es),
    csv_write_file(FileName, Es, [separator(0'\t)]).

arc_conv([], []).
arc_conv([arc(_, X, Y, W) | T], [arc(X, Y, W) | T1]) :-
    arc_conv(T, T1).





% GESTIONE DELLO HEAP

new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.

delete_heap(H) :-
    heap(H, _),
    retract(heap(H, _)), !,
    retractall(heap_entry(H, _, _, _)).

delete_heap(_) :- !.

heap_has_size(H, S) :- heap(H, S).

heap_empty(H) :- heap(H, 0).

heap_not_empty(H) :-
    heap_has_size(H, S),
    S > 0.

swap(H, SP, FP) :-
    retract(heap_entry(H, SP, KS, VS)),
    retract(heap_entry(H, FP, KF, VF)),
    assert(heap_entry(H, FP, KS, VS)),
    assert(heap_entry(H, SP, KF, VF)).

heap_update(H, A) :-
    heap_has_size(H, S),
    NS is S + A,
    retract(heap(H, S)),
    assert(heap(H, NS)).

list_heap(H) :-
    heap(H, _),
    listing(heap(H, _)),
    listing(heap_entry(H, _, _, _)).

heap_head(H, K, V) :-
    heap(H, _),
    heap_entry(H, 0, K, V).

% Caso base: heap vuota
heap_insert(H, K, V) :-
    heap_empty(H),
    assert(heap_entry(H, 0, K, V)),
    heap_update(H, 1), !.

% la heap non è vuota, e il padre è minore o uguale al figlio
heap_insert(H, K, V) :-
    heap_has_size(H, S),
    FP is floor((S - 1) / 2),
    heap_entry(H, FP, FK, _),
    FK =< K,
    assert(heap_entry(H, S, K, V)),
    heap_update(H, 1), !.

% la heap non è vuota e il padre è maggiore del figlio
heap_insert(H, K, V) :-
    heap_has_size(H, S),
    FP is floor((S - 1) / 2),
    heap_entry(H, FP, FK, _),
    FK > K,
    assert(heap_entry(H, S, K, V)),
    heap_update(H, 1),
    heap_switch(H, S), !.





% Caso base: arriviamo alla radice
heap_switch(_, 0) :- !.

% Il padre è minore uguale del figlio
heap_switch(H, P) :-
    heap_entry(H, P, K, _),
    FP is floor((P - 1) / 2),
    heap_entry(H, FP, FK, _),
    FK =< K, !.

% Il padre è maggiore del figlio
heap_switch(H, P) :-
    heap_entry(H, P, K, _),
    FP is floor((P - 1) / 2),
    heap_entry(H, FP, FK, _),
    FK > K,
    swap(H, P, FP),
    heap_switch(H, FP), !.



% Caso base: heap vuota
heap_extract(H, _, _) :-
    heap_empty(H), !.

% Caso base: estrazione della radice con albero di dimensione 1
heap_extract(H, K, V) :-
    heap(H, 1),
    retract(heap_entry(H, _, K, V)),
    heap_update(H, -1), !.

% Caso in cui l'albero contiene più di un elemento
heap_extract(H, K, V) :-
    retract(heap_entry(H, 0, K, V)),
    heap_has_size(H, S),
    LP is S - 1,
    retract(heap_entry(H, LP, LK, LV)),
    assert(heap_entry(H, 0, LK, LV)),
    heap_update(H, -1),
    heapify(H, 0), !.


% Caso foglia
heapify(H, P) :-
    heap_has_size(H, S),
    LSP is 2 * P + 1,
    RSP is 2 * P + 2,
    LSP >= S,
    RSP >= S, !.

% Gestione del padre con un figlio

% Padre minore uguale dell'unico figlio
heapify(H, P) :-
    heap_has_size(H, S),
    SP is 2 * P + 1,
    SP + 1 >= S,
    heap_entry(H, SP, SK, _),
    heap_entry(H, P, K, _),
    K =< SK, !.

% Padre maggiore dell'unico figlio
heapify(H, P) :-
    heap_has_size(H, S),
    SP is 2 * P + 1,
    SP + 1 >= S,
    heap_entry(H, SP, SK, _),
    heap_entry(H, P, K, _),
    SK < K,
    swap(H, SP, P),
    heapify(H, SP), !.

% Gestione del padre con due figli

% Padre minore uguale dei due figli
heapify(H, P) :-
    heap_has_size(H, S),
    LSP is 2 * P + 1,
    RSP is LSP + 1,
    LSP < S,
    RSP < S,
    heap_entry(H, P, K, _),
    heap_entry(H, LSP, KL, _),
    heap_entry(H, RSP, KR, _),
    K =< KL,
    K =< KR, !.

% Padre maggiore dei figli, inoltre figlio sinistro minore di figlio
% destro
heapify(H, P) :-
    heap_has_size(H, S),
    LSP is 2 * P + 1,
    RSP is LSP + 1,
    LSP < S,
    RSP < S,
    heap_entry(H, P, K, _),
    heap_entry(H, LSP, KL, _),
    heap_entry(H, RSP, KR, _),
    K > KL,
    K > KR,
    KL =< KR,
    swap(H, LSP, P),
    heapify(H, LSP), !.

% Padre maggiore dei figli, inoltre il figlio destro minore del figlio
% sinistro
heapify(H, P) :-
    heap_has_size(H, S),
    LSP is 2 * P + 1,
    RSP is LSP + 1,
    LSP < S,
    RSP < S,
    heap_entry(H, P, K, _),
    heap_entry(H, LSP, KL, _),
    heap_entry(H, RSP, KR, _),
    K > KL,
    K > KR,
    KR =< KL,
    swap(H, RSP, P),
    heapify(H, RSP), !.

% Padre minore uguale di figlio sinistro e maggiore del destro
heapify(H, P) :-
    heap_has_size(H, S),
    LSP is 2 * P + 1,
    RSP is LSP + 1,
    LSP < S,
    RSP < S,
    heap_entry(H, P, K, _),
    heap_entry(H, LSP, KL, _),
    heap_entry(H, RSP, KR, _),
    K =< KL,
    K > KR,
    KR =< KL,
    swap(H, RSP, P),
    heapify(H, RSP), !.

% Padre minore uguale di figlio destro e maggiore di sinistro
heapify(H, P) :-
    heap_has_size(H, S),
    LSP is 2 * P + 1,
    RSP is LSP + 1,
    LSP < S,
    RSP < S,
    heap_entry(H, P, K, _),
    heap_entry(H, LSP, KL, _),
    heap_entry(H, RSP, KR, _),
    K > KL,
    K =< KR,
    KR > KL,
    swap(H, LSP, P),
    heapify(H, LSP), !.





% ALGORITMO DI PRIM

mst_prim(G, Source) :-
    clean_previous_mst(G),
    graph(G),
    vertex(G, Source),
    delete_heap(h),
    new_heap(h),
    mst_prim_initialization(G, h),
    retract(vertex_key(G, Source, inf)),
    assert(vertex_key(G, Source, 0)),
    assert(vertex_previous(G, Source, null)),
    mst_neighbors(G, Source, As),
    heap_insertion_list(As, h),
    heap_extract(h, W, arc(G, _, Y, W)),
    mst_algorithm(G, Y, h), !.

mst_algorithm(G, Source, H) :-
    heap_head(H, W1, arc(G, _, _, W1)),
    W1 \= inf,
    mst_neighbors(G, Source, As),
    heap_insertion_list(As, H),
    heap_not_empty(H),
    heap_head(H, W, arc(G, X, Y, W)),
    heap_extract(H, W, arc(G, X, Y, W)),
    mst_algorithm(G, Y, H), !.

mst_algorithm(G, _, H) :-
    heap_not_empty(H),
    heap_head(H, inf, arc(G, X, Y, inf)),
    heap_extract(H, W, arc(G, X, Y, W)),
    mst_algorithm(G, Y, H), !.

mst_algorithm(_, _, H) :-
    heap_has_size(H, 0), !.



% Predicati di appoggio

clean_previous_mst(G) :-
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)).

mst_neighbors(G, V, Arcs) :-
    findall(arc(G, V, Y, W), arc(G, V, Y, W), Arcs).

mst_prim_initialization(G, h) :-
    findall(arc(G, vertice, X, inf), vertex(G, X), List),
    vertex_key_initialization(G, List),
    heap_insertion_list(List, h).

vertex_key_initialization(_, []) :- !.

vertex_key_initialization(G, [arc(G, vertice, X, inf) | T]) :-
    assert(vertex_key(G, X, inf)),
    vertex_key_initialization(G, T), !.


heap_insertion_list([], _) :- !.

% Inserisco l'arco senza fare controlli perchè pesa infinito
heap_insertion_list([arc(G, X, Y, inf) | T], H) :-
    heap_insert(H, inf, arc(G, X, Y, inf)),
    heap_insertion_list(T, H), !.

% Caso in cui l'arco che voglio inserire sia meno pesante di quello che c'è già
heap_insertion_list([arc(G, Source, Y, W) | T], H) :-
    heap_entry(H, P, W1, arc(G, X, Y, W1)),
    W1 > W,
    retract(heap_entry(H, P, W1, arc(G, X, Y, W1))),
    assert(heap_entry(H, P, W, arc(G, Source, Y, W))),
    retract(vertex_key(G, Y, W1)),
    assert(vertex_key(G, Y, W)),
    change_previous(G, Y, X, Source),
    heap_switch(H, P),
    heap_insertion_list(T, H), !.

% Altrimenti vado avanti a visitare i vicini
heap_insertion_list([_ | T], H) :-
    heap_insertion_list(T, H), !.

change_previous(G, Dest, Father, New_Father) :-
    retract(vertex_previous(G, Dest, Father)),
    assert(vertex_previous(G, Dest, New_Father)), !.

change_previous(G, Dest, _, New_Father) :-
    assert(vertex_previous(G, Dest, New_Father)), !.






% RESTITUIZIONE DELL'ALBERO MST IN PREORDINE

mst_get(G, Source, Mst) :-
    graph(G),
    vertex(G, Source),
    findall(arc(G, Source, S), vertex_previous(G, S, Source), As3),
    regularize(As3, As),
    list_sort(As, As_sorted),
    mst_calculation(As_sorted, Mst), !.


mst_calculation([], []) :- !.

mst_calculation([arc(G, F, S, W) | T], Mst) :-
    findall(arc(G, S, N), vertex_previous(G, N, S), As3),
    regularize(As3, As),
    list_sort(As, As_sorted),
    arc(G, F, S, W),
    mst_calculation(As_sorted, Preorder1),
    mst_calculation(T, Preorder2),
    append(Preorder1, Preorder2, Mst_t),
    append([arc(G, F, S, W)], Mst_t, Mst), !.



%Predicati di appoggio

list_sort(List, Sorted) :-
    sort(3, @=<, List, Dest),
    sort(4, @=<, Dest, Sorted).

regularize([], []) :- !.

regularize([arc(G, X, Y) | T], [arc(G, X, Y, W) | T1]) :-
    arc(G, X, Y, W),
    regularize(T, T1), !.




%%%%




% end of file -*- testo_progetto.pl
