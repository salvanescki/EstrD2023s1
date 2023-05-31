#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"
#include "Persona.h"
#include "ArrayList.h"

using namespace std;

// Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs){
    int sum = 0;
    for(int i = 0; i<lengthAL(xs);i++){
        sum += get(i,xs);
    }
    return sum;
}
// Incrementa en uno todos los elementos.
void sucesores(ArrayList xs){
    for(int i = 0; i<lengthAL(xs);i++){
        set(i, get(i,xs)+1,xs);
    }
}
// Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs){
    for(int i = 0; i<lengthAL(xs);i++){
        if(get(i,xs) == x) return true;
    }
    return false;
}
// Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs){
    int cont = 0;
    for(int i = 0; i<lengthAL(xs);i++){
        if(get(i,xs) == x) cont++;
    }
    return cont;
}
// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList zs = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    for(int i = 0; i<lengthAL(xs);i++){
        add(get(i,xs),zs);
    }
    for(int i = 0; i<lengthAL(ys);i++){
        add(get(i,ys),zs);
    }
    return zs;
}
// Devuelve el elemento más chico de la lista.
int minimo(ArrayList xs){
    int minimo = get(0,xs);
    for(int i = 0; i<lengthAL(xs); i++){
        if(get(i,xs) < minimo) minimo = get(i, xs);
    }
    return minimo;
}

void llenarArrayDesdeElUno(ArrayList xs){
    for (int i = 1; i<=lengthAL(xs); i++){
        add(i,xs);
    }
}

void showArray(ArrayList xs){
    cout << "[ ";
    for (int i = 0; i<lengthAL(xs); i++){
        cout << get(i,xs) << ", ";
    }
    cout << " ]"<< endl;
}

int main(){
    Persona nicolas = consPersona("Nicolas", 25);
    bool persTest1 = nombre(nicolas) == "Nicolas" && edad(nicolas) == 25;
    crecer(nicolas);
    bool persTest2 = edad(nicolas) == 26;
    cambioDeNombre("Leonel",nicolas);
    bool persTest3 = nombre(nicolas) == "Leonel";
    Persona gaston = consPersona("Gaston", 22);
    bool persTest4 = esMayorQueLaOtra(nicolas, gaston);
    bool persTest5 = laQueEsMayor(nicolas, gaston) == nicolas;
    cout << "Pruebas de Persona.cpp: " << endl;
    cout << "test1 = " << persTest1 << " \ntest2 = " << persTest2 << " \ntest3 = " << persTest3 << " \ntest4 = " << persTest4 << " \ntest5 = " << persTest5 << endl;

    Pokemon charmander = consPokemon("fuego");
    Pokemon squirtle = consPokemon("agua");
    Pokemon bulbasaur = consPokemon("planta");
    bool pokeTest1 = tipoDePokemon(charmander) == "fuego" && energia(charmander) == 100;
    perderEnergia(66, charmander);
    bool pokeTest2 = energia(charmander) == 34;
    bool pokeTest3 = superaA(charmander,bulbasaur) && !superaA(charmander,squirtle) && superaA(bulbasaur, squirtle);
    cout << "Pruebas de Pokemon.cpp: " << endl;
    cout << "test1 = " << pokeTest1 << " \ntest2 = " << pokeTest2 << " \ntest3 = " << pokeTest3 << endl;

    Pokemon ashPokemon[3];
    ashPokemon[0] = squirtle;
    ashPokemon[1] = charmander;
    ashPokemon[2] = bulbasaur;
    Entrenador ash = consEntrenador("Ash", 3, ashPokemon);

    Pokemon starmie = consPokemon("agua");
    Pokemon mistyPokemon[3];
    mistyPokemon[0] = starmie;
    mistyPokemon[1] = starmie;
    mistyPokemon[2] = starmie;
    Entrenador misty = consEntrenador("Misty", 3, mistyPokemon);

    bool entreTest1 = nombreDeEntrenador(ash) == "Ash" && cantidadDePokemon(ash) == 3;
    bool entreTest2 = cantidadDePokemonDe("fuego", ash) == 1
                      && cantidadDePokemonDe("agua", ash) == 1
                      && cantidadDePokemonDe("planta", ash) == 1;
    bool entreTest3 = pokemonNro(1, ash) == squirtle
                      && pokemonNro(2, ash) == charmander
                      && pokemonNro(3, ash) == bulbasaur;
    bool entreTest4 = leGanaATodos(ash, misty);
    cout << "Pruebas de Entrenador.cpp: " << endl;
    cout << "test1 = " << entreTest1 << " \ntest2 = " << entreTest2 << " \ntest3 = " << entreTest3 << " \ntest4 = " << entreTest4 << endl;   

    ArrayList arr1 = newArrayListWith(100);
    llenarArrayDesdeElUno(arr1);
    showArray(arr1);
    ArrayList arr2 = newArrayListWith(10);
    llenarArrayDesdeElUno(arr2);

    bool alTest1 = sumatoria(arr1) == 5050;
    sucesores(arr2);
    bool alTest2 = get(0,arr2) == 2;
    bool alTest3 = pertenece(11, arr2) && !pertenece(1, arr2);
    add(2,arr2);
    bool alTest4 = apariciones(2, arr2) == 2;
    ArrayList arr3 = append(arr1,arr2);
    bool alTest5 = get(100, arr3) == 2;
    bool alTest6 = minimo(arr1) == 1 && minimo(arr2) == 2 && minimo(arr3) == 1;

    cout << "Pruebas de ArrayList.cpp: " << endl;
    cout << "test1 = " << sumatoria(arr1)+1 << " \ntest2 = " << alTest2 << " \ntest3 = " << alTest3 << " \ntest4 = " << alTest4 << " \ntest5 = " << alTest5 << " \ntest6 = " << alTest6 << endl;   
}