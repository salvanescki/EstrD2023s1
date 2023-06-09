struct NodeT;

typedef NodeT* Tree;

// Devuelve un árbol vacío, el cual se representa con NULL
// EFICIENCIA: O(1)
Tree emptyT();

// Crea un nodo y lo devuelve.
// EFICIENCIA: O(1)
Tree nodeT(int elem, Tree left, Tree right);

// Describe si el árbol está vacío.
// EFICIENCIA: O(1)
bool isEmptyT(Tree t);

// Devuelve la raíz de un árbol.
// EFICIENCIA: O(1)
int rootT(Tree t);

// Devuelve el subárbol hijo izquierdo de un árbol
// EFICIENCIA: O(1)
Tree left(Tree t);

// Devuelve el subárbol hijo derecho de un árbol
// EFICIENCIA: O(1)
Tree right(Tree t);
