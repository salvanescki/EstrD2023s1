#include <iostream>
#include "LinkedListV2.h"
#include "Set.h"

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

}