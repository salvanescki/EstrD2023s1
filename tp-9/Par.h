#include <iostream>
using namespace std;

struct ParSt{
    int x;
    int y;
};

typedef ParSt Par;

Par consPar(int x, int y);
// Propósito: construye un par

int fst(Par p);
// Propósito: devuelve la primera componente

int snd(Par p);
// Propósito: devuelve la segunda componente

int maxDelPar(Par p);
// Propósito: devuelve la mayor componente

Par swap(Par p);
// Propósito: devuelve un par con las componentes intercambiadas

Par divisionYResto(int n, int m);
// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números