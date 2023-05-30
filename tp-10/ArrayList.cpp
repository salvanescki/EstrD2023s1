#include <iostream>
#include "ArrayList.h"

using namespace std;

struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};

ArrayList newArrayList(){
    return newArrayListWith(16);
}

ArrayList newArrayListWith(int capacidad){
    ArrayListSt* xs = new ArrayListSt;
    xs->cantidad = 0;
    xs->elementos = new int[capacidad];
    xs->capacidad = capacidad;
    return xs;
}

int lengthAL(ArrayList xs){
    return xs->cantidad;
}

int get(int i, ArrayList xs){
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs){
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs){
    int* temp = new int[capacidad];
    int length = min(xs->capacidad, capacidad);
    for(int i = 0; i < length; i++){
        temp[i] = xs->elementos[i];
    }
    delete xs->elementos;
    xs->capacidad = capacidad;
    xs->elementos = temp;
}

void add(int x, ArrayList xs){
    if(xs->cantidad == xs->capacidad){
        resize(xs->capacidad * 2, xs);
    }
    xs->elementos[xs->cantidad++] = x;
    xs->cantidad++;
}

void remove(ArrayList xs){
    xs->cantidad--;
}