#include <iostream>
#include "Par.h"

using namespace std;

Par consPar(int x, int y){
    Par par;
    par.x = x;
    par.y = y;
    return par;
}

int fst(Par p){
    return p.x;
}

int snd(Par p){
    return p.y;
}

int maxDelPar(Par p){
    int max = p.x;
    if (p.y > p.x) {
        max = p.y;
    }
    return max;
}

Par swap(Par p){
    int aux = p.x;
    p.x = p.y;
    p.y = aux;
    return p;
}

Par divisionYResto(int n, int m){
    Par p;
    p.x = n / m;
    p.y = n % m;
    return p;
}