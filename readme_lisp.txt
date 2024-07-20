--------------------------------------------------------------------------------
PROGETTO  'Graph Algorithms: Minimum Spanning Trees'
DOCENTI Marco Antoniotti, Gabriella Pasi e Rafael Penaloza
CONSEGNA Venerdì 16 gennaio 2021, ore 23:59 GMT+1 Time

Alessandro Biagiotti 869014
Youssef Benchaib 844515
Giacomo Elemi 806904
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
OBIETTIVO

Implementare l’algoritmo di Prim per la soluzione del problema MST per grafi 
non-diretti e connessi con pesi non negativi.
Per l’implementazione dell’algoritmo di Prim abbiamo utilizzato una coda a 
priorità (priority queue), in altre parole un MINHEAP.

Conoscenze necessarie per la comprensione del progetto:

- Conoscenze base di programmazione in linguaggio Common Lisp
- Conoscenze grafi, heap, heapsort
- Struttura e funzionamento delle code a priorità
- Algoritmo di Prim

--------------------------------------------------------------------------------
INFO AGGIUNTIVE

Linguaggio: Common Lisp
--------------------------------------------------------------------------------
NOTE AL LETTORE

- Come per il Readme di Prolog, anche questo testo rispetta la
  convenzione delle 80 colonne e, allo stesso modo, e' stato
  originariamente importato da un file di testo formattato tramite
  blocco note, questo significa che qualunque carattere illeggibile
  dovesse apparire all'interno del testo e' una lettera accentata non
  riconosciuta; questa versione del file dovrebbe essere priva di
  refusi di questo tipo.

- Ogniqualvolta vi dovesse essere un riferimento al linguaggio Lisp
  all'interno del file si trattera' di un'abbreviazione del nome
  Common Lisp e non di un riferimento erroneo all'intera famiglia di
  linguaggi
  
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
INTERFACCIA LISP PER LA MANIPOLAZIONE DEI DATI

----------------------------Funzioni obbligatorie-------------------------------
new-graph (graph-id -> graph-id)
Funzione che genera un nuovo grafo e lo inserisce nella hash-table *graphs*

is-graph (graph-id -> boolean)
Funzione che ritorna il graph-id se il grafo esiste, altrimenti ritorna NIL

delete-graph (graph-id -> NIL)
Funzione che rimuove il grafo graph-id dal sistema (con vertici archi etc)

new-vertex (graph-id vertex-id -> vertex-rep)
Funzione che aggiunge un nuovo vertice vertex-id al grafo graph-id

graph-vertices (graph-id -> vertex-rep-list)
Funzione che ritorna una lista di vertici del grafo

new-arc (graph-id dep-id arr-id &optional (weight 1) -> arc-rep)
Funzione che aggiunge un arco del grafo graph-id nella hash-table *arcs*
Se il grafo o uno dei due vertici non esistono viene restituito un
errore.
Si e' deciso, per comodita', di utilizzare una rappresentazione
che inserisca nella hashtable l'arco di andata e l'arco di ritorno,
inoltre qualora fosse gia' presente all'interno della hashtable un
arco che ha la stessa destinazione questo verra' sovrascritto al nuovo
valore.
Per concludere, in questa funzione si fa uso per la prima volta della
hashtable *neighbors* che contiene tutti gli archi vicini di ogni
vertice, quando creiamo un nuovo grafo, questo verrà inserito
all'interno della tavola (la sua utilita' viene spiegata nella sezione
delle strutture dati aggiuntive).

graph-arcs (graph-id -> arc-rep-list)
Funzione che ritorna una lista di tutti gli archi presenti nel grafo graph-id

graph-vertex-neighbors (graph-id vertex-id -> arc-rep-list)
Funzione che ritorna una lista arc-rep-list contenente gli archi che portano ai 
vertici N immediatamente raggiungibili dal vertice vertex-id

graph-vertex-adjacent (graph-id vertex-id -> vertex-rep-list)
Funzione che ritorna una lista vertex-rep-list contenente i vertici adiacenti al
vertice vertex-id

graph-print (graph-id)
Funzione che stampa a console la lista dei vertici e degli archi del grafo 
graph-id

------------------------------Strutture d'appoggio------------------------------
hashtable *neighbours*
Fa corrispondere ad ogni vertice una lista dei suoi archi vicini.
Ho deciso di crearla, andando ad occupare più spazio, perchè migliora
drasticamente le performance del nostro algoritmo.
Siamo passati da 90 secondi di tempo di esecuzione sul file
"primkiller_50k.LISP" gentilmente concessoci dal nostro collega Luca
di Pierro a meno di un secondo sullo stesso input cambiando solamente
il modo in cui si accede ai vicini di ciascun nodo.

hashtable *positions*
Fa corrispondere ad ogni oggetto dello heap la sua posizione
all'interno dell'array che che memorizza lo heap.
Implementata in modo da avere un tempo di accesso ai dati all'interno
dello heap costante e non dover implementare una ricerca lineare che
sarebbe lunga e costosa

hashtable *visited*
Fa corrispondere ad ogni vertice del grafo un booleano che indica
se un nodo e' o non e' gia stato visitato in precedenza

-------------------------------Funzioni d'appoggio------------------------------
is-vertex (graph-id vertex-id -> vertex-rep o NIL)
Funzione che ritorna il vertex-id se il vertice esiste, altrimenti ritorna NIL

find-arc (graph-id dep-id arr-id neighbors -> arc-rep o NIL)
Funzione che cerca all'interno di una lista l'arco che vada da dep-id
ad arr-id, l'abbiamo implementata perchè quando andiamo a modificare
un arco che già esiste nella hashtable vogliamo che questa modifica
si ripercuota anche sulla hashtable *neighbors*.

is-arc (graph-id dep-id arr-id -> arc-rep o NIL)
Funzione che ritorna l'arco con vertice di partenza dep-id e vertice di arrivo 
arr-id se esiste, altrimenti ritorna NIL

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
GESTIONE DELLO HEAP

------------------------------Funzioni obbligatorie-----------------------------
new-heap (heap-id &optional (capacity42) -> heap-rep)
Funzione che inserisce un nuovo heap nella hash-table *heaps*.
La numerazione delle posizioni interne allo heap parte da 0

heap-size (heap-rep -> heap-size)
Funzione che restituisce la dimensione attuale dello heap

heap-id (heap-rep -> heap-id)
Funzione che restituisce l'id dello heap

heap-actual-heap (heap-rep -> actual-heap)
Funzione che restituisce l'array che contiene i dati dello heap

heap-delete (heap-id -> T)
Funzione che elimina uno heap identificato da heap-id dalla hashtable

heap-empty (heap-id -> boolean)
Funzione che restituisce true se l'heap heap-id e' vuoto

heap-not-empty (heap-id -> boolean)
Funzione che restituisce true se l'heap heap-id non e' vuoto

heap-head (heap-id -> (K V))
Funzione ritorna una lista di due elementi dove K è la chiave minima e V il
valore associato

heap-insert (heap-id key value -> boolean)
Funzione che restituisce true quando l’elemento V viene correttamente 
inserito nello heap heap-id con chiave K e la sua posizione viene salvata
nell'hashtable *positions*.
Chiama una funzione heap_switch per fare in modo che il valore
inserito galleggi verso l'alto andando a piazzarsi nella posizione corretta.
Se la dimensione dello heap raggiunge la dimensione dell'array aumenta la
dimesione dell'array di 10

heap-extract (heap-id -> (K V))
Funzione che restituisce una lista con K, V e con K minima, la coppia viene
rimossa dallo heap heap-id e dall'hashtable *positions*
Se la dimensione dello heap e' diversa da quella dell'array elimina le celle
dell'array in eccesso

heap-print (heap-id -> boolean)
Funzione che stampa a console lo stato interno dello heap heap/id

-------------------------------Funzioni d'appoggio------------------------------
is-heap (heap-id -> boolean)
Funzione che restituisce true se l'heap esiste, altrimenti restituisce false

heap-update (heap-id amount -> heap-rep)
Funzione che cambia la dimensione dello heap di amount

get-father (heap-id pos -> (K V))
Funzione che restituisce la coppia (K V) che occupa la posizione del padre

get-element (heap-id pos -> (K V))
Funzione che restituisce la coppia (K V) che occupa la posizione indicata

set-element (heap-id key value pos -> pos)
Funzione che imposta l'oggetto in posizione pos alla coppia (key value)
Viene aggiornato anche il valore nella hashtable *positions*

swap-pos (heap-id key value pos -> pos)
Funzione che si occupa dello spostamento dell'elemento della coppia (key value)
nella posizione del padre e dello spostamento inverso, cambiando di conseguenza
la hashtable *positions*

swap (heap-id key value pos1 pos2 -> pos)
Funzione che scambia le due coppie (key value) delle posizioni pos1 e pos2,
cambiando di conseguenza le posizioni all'interno della hashtable *positions*

delete-node (heap-id pos -> T)
Funzione che cancella il nodo in posizione pos dallo heap heap-id

get-position (heap-id key -> pos o NIL)
Funzione che restituisce la posizione di un elemento all'interno
dell'array che contiene i dati dello heap.
Sebbene il nome possa essere fuorviante, abbiamo utilizzato il valore
V di ogni coppia (K V) come chaive all'interno della hashtable
*positions*, qualcosa di più riguardo a questa implementazione nella
prossima sezione

fp (pos -> pos) 
Funzione che restituisce la posizione del padre

left-son (pos -> pos)
Funzione che restituisce la posizione del figlio sinistro

right-son (heap-id -> pos)
Funzione che restituisce la posizione del figlio destro

heap-switch (heap-id key value pos -> T)
Funzione che si occupa di far risalire verso l'alto la coppia (key value) fino
a che l'elemento non diventa maggiore del padre o non diventa
l'elemento in posizione 0 all'interno dello heap

heapify (heap-id key value pos -> T)
Funzione che si occupa di far rispettare allo heap heap-id la heap property

positions-print (heap-id -> NIL)
Funzione che stampa le posizioni delle chiavi presenti all'interno dello heap
heap-id, inoltre le celle vuote non vengono considerate

--------------------------------------------------------------------------------
ALGORTIMO DI PRIM

--------------------------------------------------------------------------------
NOTE AL LETTORE

- Come detto prima abbiamo impiegato il valore V della coppia (K V)
  per ogni elemento presente all'interno dello heap per indicizzare i
  valori nella hashtable *positions*, questa scelta ci ha costretti a
  virare dall'originale implementazione in prolog che consisteva
  nell'inserire gli archi all'interno della heap direttamente, mentre
  in prolog potrebbe non dare un vantaggio rilevante, in lisp il fatto
  di avere direttamente a portata di mano il genitore di ogni nodo
  all'interno dello heap puo' essere utile ed evitarci di andare a
  visitare i vicini troppe volte.
  Il motivo per cui non mi sono sentito di creare un'implementazione
  del genere e' per il fatto che all'interno dello heap le entry
  sarebbero state del tipo

  (list 'arc 'graph 'dep 'arr 'weight)

  e, a meno che non si riduce il numero di elementi inseriti nella
  hashtable positions ai soli 'graph e 'arr accedere alla hashtable
  *positions* diviene poi impossibile, questo perchè: come faccio a
  trovare la posizione di un arco come quello indicato sopra se non so
  quale sia il nodo di partenza originale e il peso che avevo prima?

- Per la mst-get, su indicazione del professor M. Antoniotti, ci siamo
  comportati come in Prolog, impiegando la funzione sort che ci
  consente di implementare una nostra versione della funzione di
  ordinamento, nel caso di confronti tra numeri abbiamo impiegato il
  confronto aritmetico puro, nel resto dei casi abbiamo impiegato la funzione
  string< per decretare l'ordine 
--------------------------------------------------------------------------------


------------------------------Funzioni obbligatorie-----------------------------
mst-vertex-key (graph-id vertex-id -> K)
Funzione che dato un vertex-id di un grafo graph-id ritorna il peso minimo di un
arco che connette vertex-id nell’albero minimo; se questo arco non esiste allora
k è MOST-POSITIVE-DOUBLE-FLOAT

mst-previous (graph-id V -> U)
Ritorna il vertice U che e' il vertice genitore di V nel mst

mst-prim (graph-id source -> NIL)
Funzione che termina con un effetto collaterale. Dopo la sua esecuzione, la 
hash-table *vertex-keys* contiene al suo interno le associazioni (graph-id V) ⇒
K per ogni V appartenente a graph-id; la hash-table *previous* contiene le 
associazioni (graph-id V) ⇒ U calcolate durante l’esecuzione dell’algoritmo di 
Prim.
La funzione inizializza la dimensione dell'array dello heap, che conterra' i
vertici del grafo, al numero totale di vertici presenti nel grafo in ingresso

mst-get (graph source -> preorder-mst)
Funzione che ritorna preorder-mst che è una lista degli archi del MST ordinata 
secondo un attraversamento preorder dello stesso, fatta rispetto al peso
dell’arco.
Nel caso di archi con peso uguale si e' deciso di stamparli secondo l'ordine
lessicografico

-------------------------------Funzioni d'appoggio------------------------------
mst-algorithm (graph-id source heap -> NIL)
Funzione che si occupa della gran parte della computazione dell'algoritmo di
prim, durante la sua esecuzione modifica i vertex key e mst previous dei vertici
che compaiono nello heap, e' in grado di gestire componenti non connesse
riconoscendole per la loro distanza infinita(most positive double float) dalla
componente in analisi.
La computazione ha fine quando la heap diventa vuota

clean-previous-mst (graph-id -> T)
Funzione che elimina tutte le entry nelle hashtable *vertex-keys* e *previous*
del grafo graph-id

mst-prim-initialization (graph source heap -> NIL)
Funzione che inizializza le strutture dati ausiliarie neccessarie al corretto
funzionamento dell'algoritmo di prim:
- inizializza i vertex-key a infinito
- i nodi a non visitato
- inizializza a infinito la distanza dei nodi all'interno dello heap

set-not-visited (graph-id vertex-id -> NIL)
Imposta a NIL il valore del vertice vertex-id all'interno dell'hashtable
*visited*

set-visited (graph-id vertex-id -> T)
Imposta a T il valore del vertice vertex-id all'interno dell'hashtable *visited*

heap-insertion-list (heap-id arc-list -> T)
Funzione che inserisce gli archi all'interno di arclist nello heap heap-id
secondo il seguente criterio:
- L'arco viene inserito all'interno dello heap se e solo se la destinazione non
  e' ancora stata visitata
- se il peso dell'arco e' minore del peso memorizzato all'interno
  dello heap il valore viene sovrascritto e vengono cambiati i
  vertex-key e vertex-previous corrispondenti
- altrimenti si prosegue con l'inserimento.
L'esecuzione termina quando la lista e' vuota

set-key (heap-id weight pos -> (K V))
Funzione che cambia la chiave di una coppia (K V) all'interno dello
heap in posizione pos

already-visited (graph vertex -> boolean)
Funzione che restituisce true se il nodo e' gia' stato visitato, false altrimenti

set-vertex-key (graph-id heap-entry -> K)
Funzione che setta vertex-key di un nodo a un certo valore

set-mst-previous (graph-id heap-entry -> U)
Funzione che chiama find-father sulla lista dei vicini del nodo in
modo da cercare il padre del vertice contenuto in heap-entry così da settarne
il valore del padre a U

find-father (graph heap-entry arc-list -> arr-id)
Funzione che restituisce l'id del vertice di arrivo di un arco che
parte dal nodo che stiamo considerando e arriva in arr-id.
Nota: Restituiamo arr-id perche' i nodi presenti in arc-list sono i
vicini del nodo che stiamo ispezionando ma il grafo che a noi interessa corre
in direzione opposta

get-graph (arc -> graph-id)
Funzione che restituisce il grafo a cui appartiene un arco arc

get-departure(arc ->  dep-id)
Funzione che restituisce il vertice di partenza di un arco arc

get-arrival(arc ->  arr-id)
Funzione che restituisce il vertice di arrivo di un arco arc

get-weight (arc ->  weight)
Funzione che restituisce il peso dell'arco arc

mst-calculation (graph-id arc-list -> arc-list)
Funzione che fa la maggior parte del lavoro di computazione per la
mst-get, si occupa della costruzione dell'albero in preorder tree appoggiandosi
alla funzione list-sort

list-sort (arc-list -> arc-list)
Funzione che mette in ordine una lista secondo l'ordinamento dei pesi.
Se due oggetti hanno lo stesso peso, utilizza l'ordinamento
lessicografico per le stringhe e l'ordinamento aritmetico per i numeri

regularize (arc-list -> arc-list)
Funzione che prende in ingresso una lista di archi arc/3, simile a
quanto accadeva in prolog, e restituisce la lista ricostituita in cui
ogni arco ha il rispettivo peso

find-mst-previous (graph source -> arc-list)
Funzione che restituisce una lista contenente i previous di un nodo
inserendoli all'interno di una lista, serve per costruire gli arc/3 di
cui ho parlato nella precedente funzione
