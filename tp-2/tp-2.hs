-- Funciones de la práctica 1 que utilizo

sucesor :: Int -> Int
sucesor n = n + 1

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

data Persona = P String Int
        --      Nombre Edad
        deriving Show

edad :: Persona -> Int
edad (P n e) = e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                        then p1
                        else p2

-- Funciones de la práctica 1 adaptadas a la práctica actual

data TipoDePokemon = Agua | Fuego | Planta
        deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int
--                          TipoDePokemon PorcentajeEnergia
        deriving Show

data Entrenador = ConsEntrenador String [Pokemon]
--                              Nombre ListaPokemons
        deriving Show

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t p) = t

esMismoTipoQue :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipoQue Fuego Fuego = True
esMismoTipoQue Agua Agua = True
esMismoTipoQue Planta Planta = True
esMismoTipoQue _ _ = False

esTipoSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperiorA Agua Fuego = True
esTipoSuperiorA Fuego Planta = True
esTipoSuperiorA Planta Agua = True
esTipoSuperiorA _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esTipoSuperiorA (tipoDe p1)(tipoDe p2)

------------------------------------Funciones auxiliares--------------------------------------

agregarSi :: a -> Bool -> [a] -> [a]
agregarSi e True l = e : l
agregarSi _ _ l = l

------------------------------------Recursión sobre Listas------------------------------------
-- 1

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

-- 2

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 3

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = sucesor n : sucesores ns

-- 4

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (p:ps) = p && conjuncion ps

-- 5

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (p:ps) = p || disyuncion ps

-- 6

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (l:ls) = l ++ aplanar ls

-- 7

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- 8

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSino(e == x) + apariciones e xs

-- 9

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA k (n:ns) = agregarSi n (n < k) (losMenoresA k ns)

-- 10

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (l:ls) = agregarSi l (longitud l > n) (lasDeLongitudMayorA n ls)

-- 11

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 12

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

-- 13

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

-- 14

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (x:xs) (y:ys) = if x > y
                            then x : zipMaximos xs ys
                            else y : zipMaximos xs ys

-- 15

elMinimo :: Ord a => [a] -> a
-- PRECOND: La lista no está vacía
elMinimo [x] = x
elMinimo (x:xs) = if x < elMinimo xs
                    then x
                    else elMinimo xs

------------------------------------Recursión sobre Números------------------------------------

-- 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 2

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n - 1)

-- 3

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n - 1) e

-- 4

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs

-- 5

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 ps = ps
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n - 1) xs

------------------------------------Registros------------------------------------

-- 1

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA 0 ps = ps
mayoresA _ [] = []
mayoresA n (p:ps) = agregarSi p (edad p > n) (mayoresA n ps)

sumatoriaEdades :: [Persona] -> Int
sumatoriaEdades [] = 0
sumatoriaEdades (p:ps) = edad p + sumatoriaEdades ps

promedioEdad :: [Persona] -> Int
-- PRECOND: La lista posee al menos una persona
promedioEdad ps = div (sumatoriaEdades ps) (longitud ps)

elMasViejo :: [Persona] -> Persona
-- PRECOND: La lista posee al menos una persona
elMasViejo [p] = p
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)

-- 2

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pks) = longitud pks

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador n []) = 0
cantPokemonDe tipo (ConsEntrenador n (p:pks)) = unoSiCeroSino (esMismoTipoQue tipo (tipoDe p)) + cantPokemonDe tipo (ConsEntrenador n pks)

--

tipoDePokemonLeGanaATodosLosDe :: TipoDePokemon -> Entrenador -> Bool
tipoDePokemonLeGanaATodosLosDe t (ConsEntrenador n []) = True
tipoDePokemonLeGanaATodosLosDe t (ConsEntrenador n (p:pks)) = esTipoSuperiorA t (tipoDe p) && tipoDePokemonLeGanaATodosLosDe t (ConsEntrenador n pks)

cantidadDePokemonDeTipo :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDeTipo t (ConsEntrenador _ []) = 0
cantidadDePokemonDeTipo t (ConsEntrenador n (p:pks)) = unoSiCeroSino(esMismoTipoQue (tipoDe p) t) + cantidadDePokemonDeTipo t (ConsEntrenador n pks)

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = if tipoDePokemonLeGanaATodosLosDe t e2
                                                then cantidadDePokemonDeTipo t e1
                                                else 0

--

tieneAlMenosUnPokemonDelTipo :: TipoDePokemon -> Entrenador -> Bool
tieneAlMenosUnPokemonDelTipo tipo (ConsEntrenador n []) = False
tieneAlMenosUnPokemonDelTipo tipo (ConsEntrenador n (p:pks)) = esMismoTipoQue tipo (tipoDe p) || tieneAlMenosUnPokemonDelTipo tipo (ConsEntrenador n pks)

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = tieneAlMenosUnPokemonDelTipo Fuego e && tieneAlMenosUnPokemonDelTipo Agua e && tieneAlMenosUnPokemonDelTipo Planta e

-- 3

data Seniority = Junior | SemiSenior | Senior
        deriving Show
data Proyecto = ConsProyecto String
        deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
        deriving Show
data Empresa = ConsEmpresa [Rol]
        deriving Show

--

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer s p) = p
proyectoDeRol (Management s p) = p

nombreDeProyecto :: Proyecto -> String
nombreDeProyecto (ConsProyecto s) = s

proyectoPerteneceA :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceA _ [] = False
proyectoPerteneceA (ConsProyecto s) (pj:pjs) = s == nombreDeProyecto pj || proyectoPerteneceA (ConsProyecto s) pjs

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa (r:rls)) = agregarSi (proyectoDeRol r) (not (proyectoPerteneceA (proyectoDeRol r) (proyectos (ConsEmpresa rls)))) (proyectos (ConsEmpresa rls))

--

seniorityEsIgual :: Seniority -> Seniority -> Bool
seniorityEsIgual Junior Junior = True
seniorityEsIgual SemiSenior SemiSenior = True
seniorityEsIgual Senior Senior = True
seniorityEsIgual _ _ = False

esDevSenior :: Rol -> Bool
esDevSenior (Developer s p) = seniorityEsIgual s Senior
esDevSenior _ = False

estaEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
estaEnAlgunProyecto dev pjs = proyectoPerteneceA (proyectoDeRol dev) pjs

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior _ [] = 0
losDevSenior (ConsEmpresa []) _ = 0
losDevSenior (ConsEmpresa (r:rls)) pjs = unoSiCeroSino(esDevSenior r && estaEnAlgunProyecto r pjs) + losDevSenior (ConsEmpresa rls) pjs

--

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn _ (ConsEmpresa []) = 0
cantQueTrabajanEn pjs (ConsEmpresa (r:rls)) = unoSiCeroSino(estaEnAlgunProyecto r pjs) + cantQueTrabajanEn pjs (ConsEmpresa rls)

--

nombresPorProyecto :: [Proyecto] -> [String]
nombresPorProyecto [] = []
nombresPorProyecto (pj:pjs) = nombreDeProyecto pj : nombresPorProyecto pjs

proyectosPorCadaRol :: Empresa -> [Proyecto]
proyectosPorCadaRol (ConsEmpresa []) = []
proyectosPorCadaRol (ConsEmpresa (r:rls)) = proyectoDeRol r : proyectosPorCadaRol (ConsEmpresa rls)

aparicionesDeProyectoCadaProyecto :: [Proyecto] -> [Proyecto] -> [(Proyecto, Int)]
aparicionesDeProyectoCadaProyecto [] _= []
aparicionesDeProyectoCadaProyecto _ [] = []
aparicionesDeProyectoCadaProyecto (pj:pjs) lp = (pj, apariciones (nombreDeProyecto pj) (nombresPorProyecto lp) ) : aparicionesDeProyectoCadaProyecto pjs lp

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = aparicionesDeProyectoCadaProyecto (proyectos e) (proyectosPorCadaRol e)