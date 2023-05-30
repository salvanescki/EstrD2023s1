#include <iostream>

using namespace std;

void printN(int n, string s){
/*  Propósito: imprime n veces un string s.
    Eficiencia: 
     - Tiempo: O(S) siendo S la longitud de s 
     - Memoria: O(S) siendo S la longitud de s */
    if(n != 0){
        cout << s << endl;
        printN(n-1, s);
    }
}

void cuentaRegresiva(int n){
/*  Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    Eficiencia: 
     - Tiempo: O(n) para el argumento n
     - Memoria: O(n) para el argumento n */
    if (n >= 0){
        cout << n << endl;
        cuentaRegresiva(n-1);
    }
}

void desdeCeroHastaNconN(int n, int i){
/*  Eficiencia: 
     - Tiempo: O(n-1) siendo n el argumento
     - Memoria: O(n-1) siendo n el argumento */
    if (i <= n){
        cout << i << endl;
        desdeCeroHastaNconN(n, i++);
    }
}

void desdeCeroHastaN(int n){
/*  Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    Eficiencia: 
     - Tiempo: O(n) siendo n el argumento 
     - Memoria: O(n) siendo n el argumento */
    cout << 0 << endl;
    desdeCeroHastaNconN(n, 1);
}

int mult(int n, int m){
/*  Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    Eficiencia: 
     - Tiempo: O(m) siendo m el segundo argumento
     - Memoria: O(m) siendo m el segundo argumento */
    if (m == 0){
        return 0;
    } else {
        return n + mult(n,m-1);
    }
}

void primerosNdeN(int n, int i, string s){
/*  Precondición: el string tiene al menos n char.
    Eficiencia: 
     - Tiempo: O(S-1) siendo S la longitud de s
     - Memoria: O(S-1) siendo S la longitud de s */
    if (i >= 0){
        cout << s[n-i] << endl;
        primerosNdeN(n, n-1, s);
    }
}

void primerosN(int n, string s){
/*  Propósito: imprime los primeros n char del string s, separados por un salto de línea.
    Precondición: el string tiene al menos n char.
    Eficiencia: 
     - Tiempo: O(S) siendo S la longitud de s 
     - Memoria: O(S) siendo S la longitud de s */
    cout << s[0] << endl;
    primerosNdeN(n, n-1, s);
}

bool perteneceRE(char c, string s, int i){
/*  Eficiencia: 
     - Tiempo: O(S-1) siendo S la longitud de s
     - Memoria: O(S-1) siendo S la longitud de s */
    if (i == s.length()){
        return false;
    }
    return s[i] == c || perteneceRE(c,s,i++);
}

bool pertenece(char c, string s){
/*  Propósito: indica si un char c aparece en el string s.
    Eficiencia: 
     - Tiempo: O(S) siendo S la longitud de s
     - Memoria: O(S) siendo S la longitud de s */

    return s[0] == c || perteneceRE(c,s,1);
}

int unoSi(bool booleano){
    if (booleano) return 1;
    else return 0;
}

int aparicionesRE(char c, string s, int i){
/*  Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    Eficiencia: 
     - Tiempo: O(n) siendo n la longitud de s 
     - Memoria: O(n) siendo n la longitud de s */
    if (i == s.length()){
        return 0;
    }
    return unoSi(s[i] == c) + aparicionesRE(c,s,i++);
}

int apariciones(char c, string s){
/*  Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    Eficiencia: 
     - Tiempo: O(n) siendo n la longitud de s 
    int acum = 0;
    for (int i = 0; i < s.length(); i++){
            if (s[i] == c) acum++;
    };
    return acum;
     - Memoria: O(n) siendo n la longitud de s */
    return unoSi(s[0] == c) + aparicionesRE(c,s,1);
}

int main(){
    printN(3, "Hola mundo");
    cuentaRegresiva(10);
    //desdeCeroHastaN(10); ROTO
    bool test4 = mult(2,3) == 2*3;
    cout << "Test 4: " << test4 << endl;
    //primerosN(10, "Paralelepipedo"); ROTO
    bool test6 = pertenece('a', "hablar");
    cout << "Test 6: " << test6 << endl;
    /*bool test7 = apariciones('a', "banana") == 3;
    cout << "Test 7: " << test7 << endl; ROTO*/
}