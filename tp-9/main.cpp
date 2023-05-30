#include <iostream>
#include "Par.h"

using namespace std;

int main(){
    Par p = consPar(3, 4);
    bool test1 = fst(p) == 3 && snd(p) == 4;
    bool test2 = maxDelPar(p) == 4;
    p = swap(p);
    bool test3 = fst(p) == 4 && snd(p) == 3;
    Par q = divisionYResto(6, 4);
    bool test4 = fst(q) == 1 && snd(q) == 2;
    cout << "test1 = " << test1 << " \ntest2 = " << test2  << " \ntest3 = " << test3 << " \ntest4 = " << test4 << endl;
}