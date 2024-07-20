--------------------------------------------------------------------------------
PROGETTO  'Graph Algorithms: Minimum Spanning Trees'
DOCENTI Marco Antoniotti, Gabriella Pasi e Rafael Penaloza
CONSEGNA Venerdi' 16 gennaio 2021, ore 23:55 GMT+1 Time

Alessandro Biagiotti 869014
Youssef Benchaib 844515
Giacomo Elemi 806904
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
OBIETTIVO

Implementare l'algoritmo di Prim per la soluzione del problema MST per grafi
non-diretti e connessi con pesi non negativi.
Per l'implementazione dell'algoritmo di Prim abbiamo utilizzato una coda a 
priorita' (priority queue), in altre parole un MINHEAP.

Conoscenze necessarie per la comprensione del progetto:

-Conoscenze base di programmazione in linguaggio Prolog
-Conoscenze grafi, heap, heapsort
-Struttura e funzionamento delle code a priorita'
-Algoritmo di Prim

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
INFO AGGIUNTIVE

Linguaggio: SWI-Prolog 8.2.2

--------------------------------------------------------------------------------
NOTE AL LETTORE

Il readme è stato composto su Emacs rispettando la regola che impone
di scrivere il testo entro le 80 colonne.
Nel caso in cui all'interno del readme dovessero comparire caratteri
illeggibili è dovuto al fatto che è stato fatto un porting da blocco
note e nel testo originale erano presenti delle lettere accentate,
dovrebbero essere state rimosse tutte in questa versione.
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEI DATI

--------------------------------------------------------------------------------
NOTE AL LETTORE

- Nell'ultima versione del file la prima regola consente di eliminare tutti i 
  dati presenti nella base di conoscenza con il predicato 'consult'

- All'interno dell'algoritmo abbiamo utilizzato una rappresentazione
  "ad arco doppio" quindi all'interno della base di conoscenza avremo
  l'arco di andata e l'arco di ritorno
--------------------------------------------------------------------------------

------------------------------Predicati obbligatori-----------------------------
new_graph(G)
Predicato che aggiunge un nuovo grafo di nome G alla base di conoscenza

delete_graph(G)
Predicato che rimuove il grafo di nome G e tutti i vertici e gli archi
a lui associati dalla base di conoscenza, se su di esso e' stato eseguito
l'algoritmo di Prim verranno cancellati anche i relativi vertex_key e
vertex_previous

new_vertex(G, V)
Predicato che aggiunge un vertice V al grafo G, il predicato che rappresenta il
vertice deve essere vertex(G, V)

graph_vertices(G, Vs)
Predicato che ritorna true se Vs e' una lista contenente tutti i vertici di G

list_vertices(G)
Predicato che stampa a console la lista dei vertici di G

new_arc(G, U, V, Weight)
Predicato che aggiunge un arco appartenente al grafo G alla base di
conoscenza con le seguenti discriminanti:
- Se il grafo non esiste lo crea
- Se uno dei due vertici non esiste lo crea
- Se l'arco esiste già lo sostituisce all'interno della base di conoscenza

graph_arcs(G, Es)
Predicato utilizzato per creare una lista Es contenente tutti 
gli archi del grafo G

vertex_neighbors(G, V, Ns)
Predicato che ritorna true se V e' un vertice del grafo G e Ns e' 
una lista contenente gli archi, arc(G, V, N, W), che portano ai vertici N
immediatamente raggiungibili da V

adjs(G, V, Vs)
Predicato che ritorna true se V e' un vertice del grafo G e' Vs e' una lista
che contiene i vertici, vertex(G, V), ad esso adiacenti

list_arcs(G)
Predicato che stampa a console la lista degli archi del grafo G

list_graph(G)
Predicato che stampa a console la lista dei vertici e degli archi del grafo G

read_graph(G, FileName)
Predicato che riceve un grafo dal file csv 'FileName' e lo inserisce nella
base di conoscenza.
Se il grafo esiste gia' lo sovrascrive

write_graph(G, FileName)
Predicato che riceve il nome di un file csv 'FileName' e ci scrive dentro
il grafo G.

write_graph(G, FileName, Type)
Predicato che riceve il nome di un file csv 'FileName' e ci scrive dentro
il grafo G secondo il valore dell'argomento Type. Type puo' essere
graph o edges:
- Se Type e' graph allora G sarà il nome di un grafo nella base di
conoscenza e il suo contenuto verra' scritto all'interno del file
- Se Type e' edges allora G e' una lista di archi e il suo contenuto
verra' scritto all'interno del file.

------------------------------Predicati d'appoggio-----------------------------
arcConv([], [])
Predicato che si occupa di fare una compressione di una lista di elementi arc/4 
in una lista di elementi di arc/3 in modo da prepararla per essere inserita nel
file .csv

new_arcList([arc(X, Y, W) | T], G) :-
Predicato che presa in ingresso una lista contenente tutti i termini arc/3
provenienti dal file .csv passa gli argomeni di arc/3 a new_arc/4 che li
inserisce nella base di conoscenza aggiungendo l'informazione del grafo


--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
GESTIONE DELLO HEAP

------------------------------Predicati obbligatori-----------------------------
new_heap(H)
Predicato che inserisce un nuovo heap nella base di conoscenza, la
prima posizione all'interno dello heap e' 0

delete_heap(H)
Predicato che elimina uno heap dalla base di conoscenza e tutte le
heap_entry ad essa legate

heap_has_size(H, S)
Predicato che restituisce true quando S e' la dimensione attuale dello heap

heap_empty(H)
Predicato che restituisce true se l'heap H e' vuoto

heap_not_empty(H)
Predicato che restituisce true se l'heap H non e' vuoto

heap_head(H, K, V)
Predicato che restituisce true quando l'elemento dello heap H con chiave 
minima K e' V.

heap_insert(H, K, V)
Predicato che restituisce true quando l'elemento V viene correttamente 
inserito nello heap H con chiave K

heap_extract(H, K, V)
Predicato che restituisce true quando l'elemento V viene correttamente 
rimosso dall'heap H con chiave K

list_heap(H)
Predicato che stampa a console lo stato interno dello heap

-------------------------------Predicati d'appoggio-----------------------------
swap(H, SP, FP)
Predicato che scambia due elementi SP-FP nello heap H

heap_update(H, A)
Predicato che incrementa/decrementa di A la dimensione dello heap H

heap_switch(H, P)
Predicato che si occupa di far scalare il figlio all'interno dello heap finche'
il padre non e' minore o non diventa la radice.

heapify(H, P)
Predicato che riordina lo heap H rispettando 'heap property'

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
ALGORTIMO DI PRIM

--------------------------------------------------------------------------------
NOTA AL LETTORE

- Abbiamo deciso di implementare l'algoritmo di Prim impiegando gli
  archi anzichè i soli vertici come valori della heap_entry per un
  semplice fatto di dispersivita'.
  Penso che in questo modo tutte le informazioni siano sempre dove
  devono essere e sono veloci da raggiungere in ogni momento
  dell'esecuzione dell'algoritmo.

- Abbiamo deciso di eseguire la sort tramite la funzione consigliata da
  Professor M. Antoniotti (la sort/4) che sfrutta il predicato di
  riordino @=<

--------------------------------------------------------------------------------

-------------------------------Predicati obbligatori----------------------------
vertex_key(G, V, K)
Predicato che restituisce true quando V e' un vertice di G e, durante e dopo
l'esecuzione dell'algoritmo di Prim, contiene il peso minimo di un arco che
connette V nell'albero minimo; se questo arco non esiste (ed all'inizio
dell'esecuzione) allora K e' inf

vertex_previous(G, V, U)
Predicato che restituisce true quando V ed U sono vertici di G e il vertice U
e' il vertice 'genitore' di V nel minimum spanning tree

mst_prim(G, Source)
Questo predicato ha successo con un effetto collaterale. Dopo la sua esecuzione,
la base-dati Prolog ha al suo interno i predicati vertex_key(G, V, K)
e vertex_previous(G, V, U) per ogni V appartenente a G

mst_get(G, Source, PreorderTree)
Questo predicato e' vero quando PreorderTree e' una lista degli archi
dell' Mst ordinata secondo un attraversamento preorder dello stesso
fatta rispetto al peso dell'arco, nel caso di vertici di peso uguale
si è opera un ordinamento lessicografico degli stessi (vedi nota sopra). 

-------------------------------Predicati d'appoggio-----------------------------
mst_algorithm(G, Source, H)
Predicato che si occupa della parte computazionale dell'algoritmo di prim 
visitando il grafo in profondita', la computazione ha fine quando la heap che 
contiene gli archi rimane vuota

clean_previous_mst(G)
Predicato che cancella tutti i dati di una precedente esecuzione dell'algoritmo 
di prim sul grafo G dalla base di conoscenza

mst_neighbours(G, V, Arcs)
Predicato che crea una lista di archi percorribili dal nodo V ai vicini di V del
grafo G

mst_prim_initialization(G, H)
Predicato che costruisce una lista di archi fittizi del tipo
arc(G, vertice, X, inf)
utili solamente all'inizializzazione dell'algoritmo e chiama
heap_insertion_list e vertex_key_initialization

vertex_key_initialization(G, [arc(G, vertice, X, inf) | T])
Il predicato prende in ingresso la lista di archi generati dalla
mst_prim_initialization e va a creare una entry vertex_key per
ciascuno di essi mettendo la loro distanza ad infinito. I vertex
previous non vengono inizializzati prima dell'inizio della
computazione

heap_insertion_list([arc(G, U, V, Weight) | T], H)
Predicato che inserisce una lista di arc/4 nello heap, secondo le seguenti
regole:
- L'arco viene inserito se e solo se il nodo di destinazione non e' ancora 
  stato visitato
- Se all'interno dello heap vi e' un arco con la medesima destinazione ma peso 
  maggiore, verra' sostituito dall'arco in ingresso e verranno
  aggiornate le relative vertex_key e vertex_previous
- Nel caso il nodo sia stato gia' visitato o vi e' un arco con peso inferiore 
  diretto allo stesso nodo all'interno dello heap, verra' scartato

change_previous(G, Dest, Father, New_Father)
Il predicato si occupa di cancellare la vecchia entry vertex_previous
sostituendola con quella nuova che è stata trovata durante
l'aggiornamento della heap tramite la visita dei vicini.

mst_calculation([arc(G, U, V, Weight) | T], Mst)
Predicato che si occupa di visitare l'albero in preorder inserendolo in una
lista che verra' restituita come Mst

list_sort(List, Sorted)
Predicato che ordina List rispetto al peso dell'arco. In caso di archi con pari
peso ordina rispetto all'ordinamento 'lessicografico' del vertice destinazione.

regularize([arc(G, U, V)], [arc(G, U, V, Weight)])
Predicato che trasfroma arc/3 in arc/4 aggiugendone il peso associato
