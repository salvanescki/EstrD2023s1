#include <iostream>
#include "Par.h"
#include "Fraccion.h"

using namespace std;

void showFraccion(Fraccion f){
    cout << numerador(f) << "/" << denominador(f) << endl;
}

int main(){
    cout << "Pruebas de Par.cpp" << endl;

    Par p = consPar(3, 4);
    bool test1 = fst(p) == 3 && snd(p) == 4;
    bool test2 = maxDelPar(p) == 4;
    p = swap(p);
    bool test3 = fst(p) == 4 && snd(p) == 3;
    Par q = divisionYResto(6, 4);
    bool test4 = fst(q) == 1 && snd(q) == 2;
    cout << "test1 = " << test1 << " \ntest2 = " << test2  << " \ntest3 = " << test3 << " \ntest4 = " << test4 << endl;

    cout << "Pruebas de Fraccion.cpp" << endl;
    Fraccion f1 = consFraccion(3,4);
    Fraccion f2 = consFraccion(8,2);
    Fraccion f3 = consFraccion(5,10);
    bool test21 = numerador(f1) == 3 && denominador(f1) == 4;
    bool test22 = division(f2) == 4 && division(f1) == 0;
    bool test23 = numerador(multF(f1,f2)) == 3*8 && denominador(multF(f1,f2)) == 4*2;
    bool test24 = numerador(simplificada(f2)) == 4 && denominador(simplificada(f2)) == 1
                && numerador(simplificada(f3)) == 1 && denominador(simplificada(f3)) == 2;
    bool test25 = numerador(sumF(f1,f3)) == 50 && denominador(sumF(f1,f3)) == 40;
    
    cout << "test1 = " << test21 << " \ntest2 = " << test22 << " \ntest3 = " << test23 << " \ntest4 = " << test24 << " \ntest5 = " << test25 << endl;


}