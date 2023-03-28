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
agregarSi e True xs = e : xs
agregarSi _ _ xs = xs

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
aplanar (xs:xss) = xs ++ aplanar xss

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
lasDeLongitudMayorA n (xs:xss) = agregarSi xs (longitud xs > n) (lasDeLongitudMayorA n xss)

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
elMinimo (x:xs) = min x (elMinimo xs)

------------------------------------Recursión sobre Números------------------------------------

-- 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 2

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n < 1 
                        then [] 
                        else n : cuentaRegresiva (n - 1)

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

--

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (ConsEntrenador n pks) = pks

cantPokemonDeTipoEnLista :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeTipoEnLista t [] = 0
cantPokemonDeTipoEnLista t (p:pks) = unoSiCeroSino (esMismoTipoQue t (tipoDe p)) + cantPokemonDeTipoEnLista t pks

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t e = cantPokemonDeTipoEnLista t (pokemonsDe e)

--

tipoDePokemonLeGanaATodosLosDeLista :: TipoDePokemon -> [Pokemon] -> Bool
tipoDePokemonLeGanaATodosLosDeLista t [] = True
tipoDePokemonLeGanaATodosLosDeLista t (p:pks) = esTipoSuperiorA t (tipoDe p) && tipoDePokemonLeGanaATodosLosDeLista t pks

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = if tipoDePokemonLeGanaATodosLosDeLista t (pokemonsDe e2)
                                                then cantPokemonDe t e1
                                                else 0

--

tieneAlMenosUnPokemonDelTipo :: TipoDePokemon -> Entrenador -> Bool
tieneAlMenosUnPokemonDelTipo t e = cantPokemonDe t e > 0

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

rolesEnEmpresa :: Empresa -> [Rol]
rolesEnEmpresa (ConsEmpresa rls) = rls

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer s p) = p
proyectoDeRol (Management s p) = p

nombreDeProyecto :: Proyecto -> String
nombreDeProyecto (ConsProyecto s) = s

proyectoEsIgualA :: Proyecto -> Proyecto -> Bool
proyectoEsIgualA p1 p2 = nombreDeProyecto p1 == nombreDeProyecto p2

proyectoPerteneceALista :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceALista _ [] = False
proyectoPerteneceALista p (pj:pjs) = proyectoEsIgualA p pj || proyectoPerteneceALista p pjs

proyectosEnListaDeRoles :: [Rol] -> [Proyecto]
proyectosEnListaDeRoles [] = []
proyectosEnListaDeRoles (r:rls) = proyectoDeRol r : proyectosEnListaDeRoles rls

listaProyectosSinRepetir :: [Proyecto] -> [Proyecto]
listaProyectosSinRepetir [] = []
listaProyectosSinRepetir (pj:pjs) = if proyectoPerteneceALista pj pjs
                                        then listaProyectosSinRepetir pjs
                                        else pj : listaProyectosSinRepetir pjs

proyectos :: Empresa -> [Proyecto]
proyectos e = listaProyectosSinRepetir(proyectosEnListaDeRoles (rolesEnEmpresa e))
--

seniorityEsIgualA :: Seniority -> Seniority -> Bool
seniorityEsIgualA Junior Junior = True
seniorityEsIgualA SemiSenior SemiSenior = True
seniorityEsIgualA Senior Senior = True
seniorityEsIgualA _ _ = False

esDevSenior :: Rol -> Bool
esDevSenior (Developer s p) = seniorityEsIgualA s Senior
esDevSenior _ = False

estaEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
estaEnAlgunProyecto dev pjs = proyectoPerteneceALista (proyectoDeRol dev) pjs

esDevSeniorEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
esDevSeniorEnAlgunProyecto r pjs = esDevSenior r && estaEnAlgunProyecto r pjs

cantDevSeniorTrabajandoEnProyectosEnListaRoles :: [Rol] -> [Proyecto] -> Int
cantDevSeniorTrabajandoEnProyectosEnListaRoles [] _ = 0
cantDevSeniorTrabajandoEnProyectosEnListaRoles _ [] = 0
cantDevSeniorTrabajandoEnProyectosEnListaRoles (r:rls) pjs = unoSiCeroSino(esDevSeniorEnAlgunProyecto r pjs) + cantDevSeniorTrabajandoEnProyectosEnListaRoles rls pjs

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior e pjs = cantDevSeniorTrabajandoEnProyectosEnListaRoles (rolesEnEmpresa e) pjs

--

cantEmpleadosTrabajandoEnProyectosEnListaRoles :: [Rol] -> [Proyecto] -> Int
cantEmpleadosTrabajandoEnProyectosEnListaRoles [] _ = 0
cantEmpleadosTrabajandoEnProyectosEnListaRoles _ [] = 0
cantEmpleadosTrabajandoEnProyectosEnListaRoles (r:rls) pjs = unoSiCeroSino(estaEnAlgunProyecto r pjs) + cantEmpleadosTrabajandoEnProyectosEnListaRoles rls pjs

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn pjs e = cantEmpleadosTrabajandoEnProyectosEnListaRoles (rolesEnEmpresa e) pjs

--

parRepetido :: (Proyecto, Int) -> (Proyecto, Int) -> Bool
parRepetido (p1,n1) (p2,n2) = proyectoEsIgualA p1 p2

parPerteneceALista :: (Proyecto, Int) -> [(Proyecto, Int)] -> Bool
parPerteneceALista _ [] = False
parPerteneceALista pi (pn:pns) = parRepetido pi pn || parPerteneceALista pi pns

listaParesProyectoCantidadSinRepetir :: [(Proyecto, Int)] -> [(Proyecto, Int)]
listaParesProyectoCantidadSinRepetir [] = []
listaParesProyectoCantidadSinRepetir (pn : pns) = if parPerteneceALista pn pns
                                                        then listaParesProyectoCantidadSinRepetir pns
                                                        else pn : listaParesProyectoCantidadSinRepetir pns

cantEmpleadosAsignadosPorProyecto :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
cantEmpleadosAsignadosPorProyecto [] _ = []
cantEmpleadosAsignadosPorProyecto _ [] = []
cantEmpleadosAsignadosPorProyecto (pj:pjs) rls = (pj, cantEmpleadosTrabajandoEnProyectosEnListaRoles rls [pj]) : cantEmpleadosAsignadosPorProyecto pjs rls

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = listaParesProyectoCantidadSinRepetir (cantEmpleadosAsignadosPorProyecto (proyectosEnListaDeRoles (rolesEnEmpresa e)) (rolesEnEmpresa e))