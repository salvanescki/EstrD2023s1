struct FraccionSt {
    int numerador;
    int denominador;
};

typedef FraccionSt Fraccion;

Fraccion consFraccion(int numerador, int denominador);
// Propósito: construye una fraccion
// Precondición: el denominador no es cero

int numerador(Fraccion f);
// Propósito: devuelve el numerador

int denominador(Fraccion f);
// Propósito: devuelve el denominador

float division(Fraccion f);
// Propósito: devuelve el resultado de hacer la división

Fraccion multF(Fraccion f1, Fraccion f2);
// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)

Fraccion simplificada(Fraccion p);
// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro

Fraccion sumF(Fraccion f1, Fraccion f2);
// Propósito: devuelve la primera componente (¿A qué se refiere?, devuelvo la suma)
