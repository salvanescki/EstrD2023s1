#include <iostream>

using namespace std;

// En todas las de abajo se considera la eficiencia en memoria O(1) y se habla, por lo tanto de la eficiencia en tiempo

void printN(int n, string s){
/*  Propósito: imprime n veces un string s.
    Eficiencia: O(n) siendo n el número de veces a imprimir pasada por parámetro */
    for (int i = 0; i < n; i++){
        cout << s << endl;
    }
}
void cuentaRegresiva(int n){
/*  Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    Eficiencia: O(n) siendo n el número de veces a imprimir pasada por parámetro */
    for (int i = n; i >= 0; i--){
        cout << i << endl;
    }
}
void desdeCeroHastaN(int n){
/*  Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    Eficiencia: O(n) siendo n el número de veces a imprimir pasada por parámetro */
    for (int i = 0; i <= n; i++){
        cout << i << endl;
    }
}
int mult(int n, int m){
/*  Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    Eficiencia: O(m) siendo m el segundo argumento */
    int resultado = 0;
    for (int i = 0; i < m; i++){
        resultado += n;
    }
    return resultado;
}
void primerosN(int n, string s){
/*  Propósito: imprime los primeros n char del string s, separados por un salto de línea.
    Precondición: el string tiene al menos n char.
    Eficiencia: O(n) siendo n la cant de char a imprimir del string */
    for (int i = 0; i < n; i++){
        cout << s[i] << endl;
    }
}
bool pertenece(char c, string s){
/*  Propósito: indica si un char c aparece en el string s.
    Eficiencia: O(n) siendo n la longitud del string s */

    for (int i = 0; i < s.length(); i++){
        if (s[i] == c) return true;
    };
    return false;
}
int apariciones(char c, string s){
/*  Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    Eficiencia: O(n) siendo n la longitud del string s */
    int acum = 0;
    for (int i = 0; i < s.length(); i++){
            if (s[i] == c) acum++;
    };
    return acum;
}

int main(){
    printN(3, "Hola mundo");
    cuentaRegresiva(10);
    desdeCeroHastaN(10);
    bool test4 = mult(2,3) == 2*3;
    cout << "Test 4: " << test4 << endl;
    primerosN(10, "Paralelepipedo");
    bool test6 = pertenece('a', "hablar");
    cout << "Test 6: " << test6 << endl;
    bool test7 = apariciones('a', "banana") == 3;
    cout << "Test 7: " << test7 << endl;
}