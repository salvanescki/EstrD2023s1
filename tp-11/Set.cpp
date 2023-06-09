#include <iostream>
#include "Set.h"
#include "LinkedListV2.h"

using namespace std;

struct NodoS {
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt {
    /* INV. REP:
      - El set no tiene elementos repetidos*/
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

// AUX

// Agrega un elemento x en el Set s. Como los Sets no tienen orden, se agrega al principio.
// EFICIENCIA: O(1) 
void agregar(int x, SetSt* s){
    NodoS* n = new NodoS;
    n->elem = x;
    n->siguiente = s->primero;
    s->primero = n;
    s->cantidad++;
}

// Busca un elemento en el Set. Si lo encuentra devuelve un puntero a su nodo, sino devuelve null.
// PRECOND: El Set no está vacío.
// EFICIENCIA: O(n), n elementos del Set.
NodoS* findS(int x, Set s){
    NodoS* actual = s->primero;
    while(actual != NULL && actual->elem != x){
        actual = actual->siguiente;
    }
    return actual;
}

// Busca el anterior nodo a un elemento en el Set. Si lo encuentra devuelve un puntero a su nodo,
// sino devuelve null.
// PRECOND: El Set no está vacío.
// EFICIENCIA: O(n), n elementos del Set.
NodoS* findPrevious(int x, Set s){
    NodoS* actual = s->primero;
    while(actual != NULL && actual->siguiente->elem != x){
        actual = actual->siguiente;
    }
    return actual;
}

//

Set emptyS(){
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

bool isEmptyS(Set s){
    return s->cantidad == 0;
}

bool belongsS(int x, Set s){
    if (isEmptyS(s) || findS(x,s) == NULL){
        return false;
    }
    return findS(x,s)->elem == x;
}

void AddS(int x, Set s){
    if(!belongsS(x, s)) agregar(x, s);
}

void RemoveS(int x, Set s){
    if (s->primero->elem == x){
        delete s->primero;
        s->primero = NULL;
    } else {
        NodoS* anterior = findPrevious(x, s);
        NodoS* actual = anterior->siguiente;
        anterior->siguiente = anterior->siguiente->siguiente;
        delete actual;
    }
    s->cantidad--;
}

int sizeS(Set s){
    return s->cantidad;
}

LinkedList setToList(Set s){
    LinkedList xs = nil();
    NodoS* actual = s->primero;
    while(actual != NULL){
        Cons(actual->elem, xs);
        actual = actual->siguiente;
    }
    return xs;
}

void DestroyS(Set s){
    NodoS* temp = s->primero;
    while(s->primero != NULL){
        s->primero = s->primero->siguiente;
        delete temp;
        temp = s->primero;
    }
    delete s;
}