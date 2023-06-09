struct NodoL;
struct LinkedListSt;
typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL
struct IteratorSt;
typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL

// Crea una lista vacía.
// EFICIENCIA: O(1)
LinkedList nil();

// Indica si la lista está vacía.
// EFICIENCIA: O(1)
bool isEmpty(LinkedList xs);

// Devuelve el primer elemento.
// PRECOND: La lista xs no está vacía.
// EFICIENCIA: O(1)
int head(LinkedList xs);

// Agrega un elemento al principio de la lista.
// EFICIENCIA: O(1)
void Cons(int x, LinkedList xs);

// Quita el primer elemento.
// PRECOND: La lista xs no está vacía.
// EFICIENCIA: O(1)
void Tail(LinkedList xs);

// Devuelve la cantidad de elementos.
// EFICIENCIA: O(1)
int length(LinkedList xs);

// Agrega un elemento al final de la lista.
// EFICIENCIA: O(1)
void Snoc(int x, LinkedList xs);

// Apunta el recorrido al primer elemento.
// EFICIENCIA: O(1)
ListIterator getIterator(LinkedList xs);

// Devuelve el elemento actual en el recorrido.
// PRECOND: No está al final del recorrido
// EFICIENCIA: O(1)
int current(ListIterator ixs);

// Reemplaza el elemento actual por otro elemento.
// PRECOND: No está al final del recorrido
// EFICIENCIA: O(1)
void SetCurrent(int x, ListIterator ixs);

// Pasa al siguiente elemento.
// PRECOND: No está al final del recorrido
// EFICIENCIA: O(1)
void Next(ListIterator ixs);

// Indica si el recorrido ha terminado.
// EFICIENCIA: O(1)
bool atEnd(ListIterator ixs);

// Libera la memoria ocupada por el iterador.
// EFICIENCIA: O(1)
void DisposeIterator(ListIterator ixs);

// Libera la memoria ocupada por la lista
// EFICIENCIA: O(n), n el tamaño de la lista.
void DestroyL(LinkedList xs);

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// El header de la segunda lista se destruye.
// EFICIENCIA: O(1)
void Append(LinkedList xs, LinkedList ys);