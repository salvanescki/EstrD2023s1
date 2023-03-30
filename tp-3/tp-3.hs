-- Funciones de anteriores prácticas

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSino(e == x) + apariciones e xs

------------------------------------Tipos Recursivos Simples------------------------------------

-- Celdas con Bolitas --

data Color = Azul | Rojo
        deriving Show
data Celda = Bolita Color Celda | CeldaVacia
        deriving Show

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False

--

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c1 (Bolita c2 cl) = unoSiCeroSino(esMismoColor c1 c2) + nroBolitas c1 cl

--

poner :: Color -> Celda -> Celda
poner c cl = Bolita c cl

--

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c1 (Bolita c2 cl) = if esMismoColor c1 c2 then cl else Bolita c2 (sacar c1 cl)

--

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cl = cl
ponerN n c cl = poner c (ponerN (n - 1) c cl)

--

-- Camino hacia el Tesoro --

data Objeto = Cacharro | Tesoro
        deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
        deriving Show

camino0 = Fin
camino1 = Nada (Nada (Nada (Nada (Nada (Nada Fin)))))
camino2 = Nada (Nada (Cofre [Cacharro, Cacharro, Cacharro] (Nada (Nada (Nada (Nada Fin))))))
camino3 = Nada (Nada (Cofre [Cacharro, Cacharro, Cacharro, Tesoro, Cacharro] (Nada (Nada (Nada (Nada Fin))))))
camino4 = Nada (Nada (Cofre [Cacharro, Cacharro, Cacharro, Tesoro, Cacharro, Tesoro, Tesoro] (Nada (Nada (Nada (Nada Fin))))))
camino5 = Nada (Nada (Cofre [Cacharro, Cacharro, Cacharro, Tesoro, Cacharro, Tesoro, Tesoro] (Nada (Nada (Nada (Nada(Cofre [Tesoro] Fin)))))))
camino6 = Cofre [Tesoro] Fin

--

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (obj:objs) = esTesoro obj || hayTesoroEnLista objs

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre objs cm) = hayTesoroEnLista objs || hayTesoro cm
hayTesoro (Nada cm) = hayTesoro cm

--

pasosHastaTesoro :: Camino -> Int
-- PRECOND: Tiene que haber al menos un Tesoro
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre objs cm) = if hayTesoroEnLista objs then 0 else pasosHastaTesoro cm
pasosHastaTesoro (Nada cm) = 1 + pasosHastaTesoro cm

--

siguienteCamino :: Camino -> Camino
-- PRECOND: El camino no debe ser Fin
siguienteCamino (Cofre _ cm) = cm
siguienteCamino (Nada cm) = cm

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn 0 (Nada _) = False
hayTesoroEn 0 (Cofre objs cm) = hayTesoroEnLista objs
hayTesoroEn n cm = hayTesoroEn (n - 1) (siguienteCamino cm)

--

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre objs cm) = unoSiCeroSino(hayTesoroEnLista objs) + cantTesorosEnCamino cm
cantTesorosEnCamino (Nada cm) = cantTesorosEnCamino cm

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n cm = cantTesorosEnCamino cm >= n

{-
cantTesorosEntre :: Int -> Int -> Camino -> Int
-}

--