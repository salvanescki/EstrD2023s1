#include <iostream>
#include "Queue.h"

struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};
struct QueueSt {
    /* INV. REP:
        - primero == NULL sii ultimo == NULL
        - Si ultimo != NULL, ultimo->siguiente == NULL
    */
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};


Queue emptyQ(){
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

int firstQ(Queue q){
    return q->primero->elem;
}

void Enqueue(int x, Queue q){
    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = NULL;
    if (q->ultimo == NULL){
        q->primero = n;
    } else {
        q->ultimo->siguiente = n;
    }
    q->ultimo = n;
    q->cantidad++;
}

void Dequeue(Queue q){
    NodoQ* temp = q->primero;
    q->primero = q->primero->siguiente;
    if(q->primero == NULL) q->ultimo = NULL;
    q->cantidad--;
    delete temp;
}

int lengthQ(Queue q){
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2){
    q1->ultimo->siguiente = q2->primero;
    q1->ultimo = q2->ultimo;
    q1->cantidad += q2->cantidad;
    delete q2;
}

void DestroyQ(Queue q){
    // No hice llamados sucesivos a Dequeue porque por cada uno crea un nuevo temp.
    NodoQ* temp = q->primero;
    while(q->primero != NULL){
        q->primero = q->primero->siguiente;
        delete temp;
        temp = q->primero;
    }
    delete q;
}