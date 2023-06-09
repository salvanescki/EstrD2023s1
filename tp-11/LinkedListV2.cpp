#include <iostream>
#include "LinkedListV2.h"

using namespace std;

struct NodoL {
    int elem;           // valor del nodo
    NodoL* siguiente;   // puntero al siguiente nodo
};

struct LinkedListSt {
    /* INV.REP.:
        - cantidad indica la cantidad de nodos que se pueden recorrer
          desde primero por siguiente hasta alcanzar a NULL
        - primero == NULL sii ultimo == NULL
        - Si ultimo != NULL, ultimo->siguiente == NULL
    */
    int cantidad;       // cantidad de elementos
    NodoL* primero;     // puntero al primer nodo
    NodoL* ultimo;      // puntero al último nodo
};

struct IteratorSt {
    NodoL* current;
};

LinkedList nil(){
    LinkedListSt* xs = new LinkedListSt;
    xs->cantidad = 0;
    xs->primero = NULL;
    xs->ultimo = NULL;
    return xs;
}

bool isEmpty(LinkedList xs){
    return xs->cantidad == 0;
}

int head(LinkedList xs){
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = xs->primero;
    xs->primero = nodo;
    if(xs->ultimo == NULL) xs->ultimo = nodo;
    xs->cantidad++;
}

void Tail(LinkedList xs){
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    if(xs->primero == NULL) xs->ultimo = NULL;
    xs->cantidad--;
    delete temp;
}

int length(LinkedList xs){
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = NULL;
    if(xs->primero == NULL){
        xs->primero = nodo;
    } else {
        xs->ultimo->siguiente = nodo;
    }
    xs->ultimo = nodo;
    xs->cantidad++;
}

ListIterator getIterator(LinkedList xs){
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}

int current(ListIterator ixs){
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x;
}

void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){
    delete ixs;
}

void DestroyL(LinkedList xs){
    NodoL* temp = xs->primero;
    while(xs->primero != NULL){
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;
}

void Append(LinkedList xs, LinkedList ys){
    xs->ultimo->siguiente = ys->primero;
    xs->ultimo = ys->ultimo;
    xs->cantidad += ys->cantidad;
    delete ys;
}