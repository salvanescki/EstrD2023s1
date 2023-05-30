#include <iostream>
#include "Fraccion.h"

using namespace std;

Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

int numerador(Fraccion f){
    return f.numerador;
}

int denominador(Fraccion f){
    return f.denominador;
}


float division(Fraccion f){
    return f.numerador / f.denominador;
}

Fraccion multF(Fraccion f1, Fraccion f2){
    return consFraccion(f1.numerador * f2.numerador, f1.denominador * f2.denominador);
}

int mcd(int a, int b){
    if (a % b == 0){
        return b;
    } else {
        return mcd(b, a % b);
    }
}

Fraccion simplificada(Fraccion p){
    return consFraccion(
        p.numerador/mcd(p.numerador, p.denominador),
        p.denominador/mcd(p.numerador, p.denominador)
    );
}

Fraccion sumF(Fraccion f1, Fraccion f2){
    return consFraccion(
        f2.denominador * f1.numerador + f1.denominador * f2.numerador,
        f1.denominador * f2.denominador
    );
}
