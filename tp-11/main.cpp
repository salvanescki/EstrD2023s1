#include<iostream>
#include "LinkedList.h"

using namespace std;

//AUX
string status(bool test){
    return test? "OK" : "ERROR";
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
void Append(LinkedList xs, LinkedList ys){
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

    bool test1 = sumatoria(xs) == 5050;
    Sucesores(xs);
    bool test2 = head(xs) == 2 && pertenece(101, xs);
    Cons(101, xs);
    bool test3 = apariciones(101,xs) == 2;
    bool test4 = minimo(xs) == 2;

    cout << "Pruebas de LinkedList: " << endl;
    cout << "test1 = " << status(test1) << " \ntest2 = " << status(test2) << " \ntest3 = " << status(test3) << " \ntest4 = " << status(test4) << endl;   

}