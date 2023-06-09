#include <iostream>
#include "LinkedListV2.h"
#include "Set.h"
#include "Queue.h"
#include "Tree.h"
#include "ArrayList.h"

using namespace std;

//AUX
string status(bool test){
    return test? "OK" : "ERROR";
}

void ShowList(LinkedList xs) {
  ListIterator ixs = getIterator(xs);
  cout << "[";
  if (!atEnd(ixs)) {
    cout << " " << current(ixs);
    Next(ixs);
  }
  while (!atEnd(ixs)) {  
    cout << ", " << current(ixs);
    Next(ixs);
  }
  cout << " ]" << endl;
}

int unoSi(bool p){
    return p? 1: 0;
}

/*---------------------------------------- LINKED LIST ------------------------------------------------*/

// Devuelve la suma de todos los elementos.
// EFICIENCIA: O(n), n el tamaño de la lista. Ya que, recorre linealmente la lista mediante el iterador
int sumatoria(LinkedList xs){
    int sum = 0;
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        sum += current(ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return sum;
}

// Incrementa en uno todos los elementos.
// EFICIENCIA: O(n), n el tamaño de la lista. Ya que, recorre linealmente la lista mediante el iterador
void Sucesores(LinkedList xs){
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        SetCurrent(current(ixs) + 1, ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
}

// Indica si el elemento pertenece a la lista.
// EFICIENCIA: En el peor caso recorre toda la lista, O(n)
// Todas las operaciones de iteradores son O(1).
bool pertenece(int x, LinkedList xs){
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        if(current(ixs) == x){
            DisposeIterator(ixs);
            return true;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return false;
}

// Indica la cantidad de elementos iguales a x.
// EFICIENCIA: O(n), n el tamaño de la lista.
// Todas las operaciones de iteradores son O(1).
int apariciones(int x, LinkedList xs){
    ListIterator ixs = getIterator(xs);
    int cont = 0;
    while(!atEnd(ixs)){
        if(current(ixs) == x){
            cont++;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return cont;
}

// Devuelve el elemento más chico de la lista.
// EFICIENCIA: O(n), n el tamaño de la lista.
// Todas las operaciones de iteradores son O(1).
int minimo(LinkedList xs){
    ListIterator ixs = getIterator(xs);
    int min = INT32_MAX;
    while(!atEnd(ixs)){
        if(current(ixs) < min){
            min = current(ixs);
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return min;
}

// Dada una lista genera otra con los mismos elementos, en el mismo orden.
// EFICIENCIA: Se recorre cada nodo de xs (n nodos), para cada uno de ellos, se llama a Snoc, la cual tiene costo O(n).
// Por lo tanto, el costo total es de O(n^2). Si Snoc fuera de costo O(1), esta función tendría costo O(n).
LinkedList copy(LinkedList xs){
    LinkedList ys = nil();
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)){
        Snoc(current(ixs), ys);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return ys;
}

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye.
// EFICIENCIA: O(m*n), siendo m la cantidad de nodos de ys y, por cada uno de ellos, se llama a Snoc
// la cual, ahora mismo tiene un costo de O(n). Podría mejorar a costo O(m) si Snoc fuera de costo O(1).
void UserAppend(LinkedList xs, LinkedList ys){
    ListIterator iys = getIterator(ys);
    while(!atEnd(iys)){
        Snoc(current(iys), xs);
        Next(iys);
    }
    DisposeIterator(iys);
    DestroyL(ys);
}

/*------------------------------------ ÁRBOLES RECURSIVO -----------------------------------------*/

// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int rSumarT(Tree t){
    if(isEmptyT(t)) return 0;
    return rootT(t) + rSumarT(left(t)) + rSumarT(right(t));
}

// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
// en inglés).
int rSizeT(Tree t){
    if(isEmptyT(t)) return 0;
    return 1 + rSizeT(left(t)) + rSizeT(right(t));
}

// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// árbol.
bool rPerteneceT(int e, Tree t){
    if (isEmptyT(t)) return false;
    return rootT(t) == e || rPerteneceT(e, left(t)) || rPerteneceT(e, right(t));
}

// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
int rAparicionesT(int e, Tree t){
    if (isEmptyT(t)) return 0;
    return unoSi(rootT(t) == e) + rAparicionesT(e, left(t)) + rAparicionesT(e, right(t));
}

// Dado un árbol devuelve su altura.
int rHeightT(Tree t){
    if (isEmptyT(t)) return 0;
    return 1 + max(rHeightT(left(t)),rHeightT(right(t)));
}

void recToList(Tree t, ArrayList xs){
    if (!isEmptyT(t)){
        recToList(left(t), xs);
        add(rootT(t), xs);
        recToList(right(t), xs);
    }
}

// Dado un árbol devuelve una lista con todos sus elementos.
ArrayList rToList(Tree t){
    ArrayList xs = newArrayList();
    recToList(t, xs);
    return xs;
}

bool isLeaf(Tree t){
    return left(t) == NULL && right(t) == NULL;
}

void recLeaves(Tree t, ArrayList xs){
    if(!isEmptyT(t)){
        recLeaves(left(t), xs);
        if(isLeaf(t)){
            add(rootT(t), xs);
        }
        recLeaves(right(t), xs);
    }
}

// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList rLeaves(Tree t){
    ArrayList xs = newArrayList();
    recLeaves(t, xs);
    return xs;
}

void recLevelN(int n, Tree t, ArrayList xs){
    if (n > 0){
        recLevelN(n-1, left(t), xs);
        recLevelN(n-1, right(t), xs);
    } else {
        add(rootT(t), xs);
    }
}

// Dados un número n y un árbol devuelve una lista con los nodos de nivel n
ArrayList rLevelN(int n, Tree t){
    ArrayList xs = newArrayList();
    recLevelN(n, t, xs);
    return xs;
}

/*------------------------------------ ÁRBOLES ITERATIVO -----------------------------------------*/

// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int iSumarT(Tree t){


}

// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
// en inglés).
int iSizeT(Tree t){
    
}

// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// árbol.
bool iPerteneceT(int e, Tree t){
    
}

// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
int iAparicionesT(int e, Tree t){
    
}

// Dado un árbol devuelve una lista con todos sus elementos.
ArrayList iToList(Tree t){
    
}

/*-------------------------------------------- MAIN ----------------------------------------------------*/

int main(){
    LinkedList xs = nil();
    for(int i = 100; i > 0; i--){
        Cons(i, xs);
    }

    bool listTest1 = sumatoria(xs) == 5050;
    Sucesores(xs);
    bool listTest2 = head(xs) == 2 && pertenece(101, xs);
    Cons(101, xs);
    bool listTest3 = apariciones(101,xs) == 2;
    bool listTest4 = minimo(xs) == 2;

    cout << "Pruebas de LinkedList: " << endl;
    cout << "listTest1 = " << status(listTest1) << " \nlistTest2 = " << status(listTest2) << " \nlistTest3 = " << status(listTest3) << " \nlistTest4 = " << status(listTest4) << endl;   
    
    LinkedList ys = nil();
    for(int i = 1000; i >= 900; i--){
        Cons(i, ys);
    }

    LinkedList zs = nil();
    for(int i = 1100; i > 1000; i--){
        Cons(i, zs);
    }
    
    cout << "Prueba de append usuario" << endl;
    UserAppend(ys,zs);
    ShowList(ys);

    LinkedList as = nil();
    for(int i = 1000; i >= 900; i--){
        Cons(i, as);
    }

    LinkedList bs = nil();
    for(int i = 1100; i > 1000; i--){
        Cons(i, bs);
    }

    cout << "Prueba de append interfaz" << endl;
    Append(as,bs);
    ShowList(as);

    DestroyL(xs);
    DestroyL(ys);
    DestroyL(zs);
    DestroyL(as);
    DestroyL(bs);

    Set emptySet = emptyS();
    
    bool setTest1 = isEmptyS(emptySet);

    Set set1 = emptyS();
    for(int i = 0; i < 10; i++){
        AddS(1, set1);
    }

    bool setTest2 = !isEmptyS(set1);
    bool setTest3 = belongsS(1, set1);
    RemoveS(1, set1);
    bool setTest4 = isEmptyS(set1);
    bool setTest5 = sizeS(set1) == 0;
    AddS(1, set1);
    bool setTest6 = sizeS(set1) == 1;
    AddS(1, set1);
    bool setTest7 = sizeS(set1) == 1;
    AddS(2, set1);
    bool setTest8 = sizeS(set1) == 2;

    cout << "Pruebas de Set: " << endl;
    cout << "setTest1 = " << status(setTest1) << " \nsetTest2 = " << status(setTest2) << " \nsetTest3 = " << status(setTest3) 
    << " \nsetTest4 = " << status(setTest4) << " \nsetTest5 = " << status(setTest5) << " \nsetTest6 = " << status(setTest6) 
    << " \nsetTest7 = " << status(setTest7) << " \nsetTest8 = " << status(setTest8) << endl;

    AddS(3, set1);
    cout << "Set {1, 2, 3}: " << endl;
    ShowList(setToList(set1));

    DestroyS(emptySet);
    DestroyS(set1);

    Queue q1 = emptyQ();
    
    bool qTest1 = isEmptyQ(q1);

    for(int i = 0; i < 10; i++){
        Enqueue(i, q1);
    }

    bool qTest2 = !isEmptyQ(q1) && lengthQ(q1) == 10;
    bool qTest3 = firstQ(q1) == 0;
    Dequeue(q1);
    bool qTest4 = firstQ(q1) == 1 && lengthQ(q1) == 9;

    Queue q2 = emptyQ();

    for(int i = 10; i < 20; i++){
        Enqueue(i, q2);
    }

    MergeQ(q1, q2);
    bool qTest5 = lengthQ(q1) == 19;

    for(int i = 0; i < 9; i++){
        Dequeue(q1);
    }

    bool qTest6 = firstQ(q1) == 10;

    cout << "Pruebas de Queue: " << endl;
    cout << "qTest1 = " << status(qTest1) << " \nqTest2 = " << status(qTest2) << " \nqTest3 = " << status(qTest3) 
    << " \nqTest4 = " << status(qTest4) << " \nqTest5 = " << status(qTest5) << " \nqTest6 = " << status(qTest6) << endl;

    DestroyQ(q1);

}