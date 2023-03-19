------------------------------------Números Enteros------------------------------------

-- 1

sucesor :: Int -> Int
sucesor n = n + 1

sumar :: Int -> Int -> Int
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int)
-- PRECOND: m no debe ser 0
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if (n > m)
                    then n
                    else m

{-

2. Ejemplos que denoten 10 utilizando las cuatro funciones anteriores:

a. maxDelPar(divisionYResto (sucesor 9) (sumar 4 (-3)))
b. sumar (maxDelPar(divisionYResto 9 5)) (sucesor 5)
c. sucesor (sumar (maxDelPar(divisionYResto 10 3)) 6)
d. maxDelPar(divisionYResto (sumar 70 30) (sucesor 9))

-}

-- 1.

------------------------------------Tipos enumerativos------------------------------------

-- 1

data Dir = Norte | Este | Sur | Oeste
        deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir -> Dir
-- PRECOND: La dirección no debe ser Oeste
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste

{-
siguiente es una función Parcial, ya que evalua los casos de Norte, Sur y Este, pero no Oeste.
-}

-- 2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
        deriving Show

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

numeroDeDia :: DiaDeSemana -> Int
numeroDeDia Lunes = 1
numeroDeDia Martes = 2
numeroDeDia Miercoles = 3
numeroDeDia Jueves = 4
numeroDeDia Viernes = 5
numeroDeDia Sabado = 6
numeroDeDia Domingo = 7

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroDeDia d1 > numeroDeDia d2

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio primerDia = False
estaEnElMedio ultimoDia = False
estaEnElMedio _ = True

-- 3

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False

------------------------------------Registros------------------------------------

-- 1

data Persona = P String Int
        --      Nombre Edad
        deriving Show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (sucesor e)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P n e) = P nuevoNombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if(esMayorQueLaOtra p1 p2)
                        then p1
                        else p2

-- 2

data TipoDePokemon = Agua | Fuego | Planta
        deriving Show

data Pokemon = Pk TipoDePokemon Int
--                TipoDePokemon PorcentajeEnergia
        deriving Show

data Entrenador = E String Pokemon Pokemon
--                  Nombre Pokemon Pokemon
        deriving Show

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (Pk t p) = t

esTipoSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperiorA Agua Fuego = True
esTipoSuperiorA Fuego Planta = True
esTipoSuperiorA Planta Agua = True
esTipoSuperiorA _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esTipoSuperiorA (tipoDe p1)(tipoDe p2)

esMismoTipoQue :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipoQue Fuego Fuego = True
esMismoTipoQue Agua Agua = True
esMismoTipoQue Planta Planta = True
esMismoTipoQue _ _ = False

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (E n p1 p2) = unoSiCeroSino(esMismoTipoQue (tipoDe p1) tipo) + unoSiCeroSino(esMismoTipoQue (tipoDe p2) tipo)

listaPokemonDeEntrenador :: Entrenador -> [Pokemon]
listaPokemonDeEntrenador (E n p1 p2) = [p1, p2]

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1,e2) = listaPokemonDeEntrenador e1 ++ listaPokemonDeEntrenador e2

------------------------------------Funciones Polimórficas------------------------------------

-- 1

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
-- Es para distinguir que no corresponden, necesariamente, al mismo dato

{-
2. Estas funciones son polimórficas porque no restringen el tipo del dato que se les pasa como argumento, puedo pasar tanto un String, como un Int, como una Persona, etc.
-}

------------------------------------Pattern Matching sobre listas------------------------------------

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (p:_) = p

sinElPrimero :: [a] -> [a]
sinElPrimero(p:r) = r

splitHead :: [a] -> (a,[a])
splitHead l = (elPrimero l, sinElPrimero l)