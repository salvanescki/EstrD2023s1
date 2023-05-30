#include <iostream>
#include "Pokemon.h"

using namespace std;

struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}

int energia(Pokemon p){
    return p->vida;
}

void perderEnergia(int energia, Pokemon p){
    p->vida -= energia;
}

bool superaA(Pokemon p1, Pokemon p2){
    return (p1->tipo == "agua" && p2->tipo == "fuego") 
           || (p1->tipo == "fuego" && p2->tipo == "planta")
           || (p1->tipo == "planta" && p2->tipo == "agua");
}