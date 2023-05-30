#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"
#include "Persona.h"

using namespace std;

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

}