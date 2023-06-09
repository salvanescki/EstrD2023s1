#include "LinkedListV2.h"

struct NodoS;
struct SetSt;
typedef SetSt* Set;     // INV.REP.: el puntero NO es NULL

// Crea un conjunto vacío.
// EFICIENCIA: O(1)
Set emptyS();

// Indica si el conjunto está vacío.
// EFICIENCIA: O(1)
bool isEmptyS(Set s);

// Indica si el elemento pertenece al conjunto.
// EFICIENCIA: O(n), n elementos del Set.
bool belongsS(int x, Set s);

// Agrega un elemento al conjunto.
// EFICIENCIA: O(n), n elementos del Set.
void AddS(int x, Set s);

// Quita un elemento dado.
// EFICIENCIA: O(n), n elementos del Set.
void RemoveS(int x, Set s);

// Devuelve la cantidad de elementos.
// EFICIENCIA: O(1)
int sizeS(Set s);

// Devuelve una lista con los lementos del conjunto.
// EFICIENCIA: O(n), n elementos del Set.
LinkedList setToList(Set s);

// Libera la memoria ocupada por el conjunto.
// EFICIENCIA: O(n), n elementos del Set.
void DestroyS(Set s);