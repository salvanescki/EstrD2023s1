#include <iostream>
#include "Entrenador.h"
#include "Pokemon.h"

using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->pokemon = pokemon;
    e->cantPokemon = cantidad;
    return e;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int cont = 0;
    for(int i = 0; i<e->cantPokemon;i++){
        if (tipoDePokemon(e->pokemon[i]) == tipo){
            cont++;
        }
    }
    return cont;
}

Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemon[i-1];
}

bool pokemonLeGanaATodos(Pokemon p, Entrenador e){
    int i = 0;
    while (i < e->cantPokemon && superaA(p,e->pokemon[i])){
        i++;
    }
    return i == e->cantPokemon;
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    for(int i = 0; i<e1->cantPokemon;i++){
        if(pokemonLeGanaATodos(e1->pokemon[i],e2)){
            return true;
        }
    }
    return false;
}