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

------------------------------------Tipos arbóreos------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

{-
(NodeT 10 (NodeT 2 EmptyT EmptyT)(NodeT 3 EmptyT EmptyT))

(NodeT 10 (NodeT 2 (NodeT 4 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT))(NodeT 3 (NodeT 9 EmptyT EmptyT) (NodeT 12 EmptyT EmptyT))
-}

-- Árboles binarios --

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x lt rt) = x + sumarT lt + sumarT rt

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x lt rt) = 1 + sizeT lt + sizeT rt

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x lt rt) = NodeT (2 * x) (mapDobleT lt) (mapDobleT rt)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT x1 (NodeT x2 lt rt) = x1 == x2 || perteneceT x1 lt || perteneceT x1 rt

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT x1 (NodeT x2 lt rt) = unoSiCeroSino(x1 == x2) + aparicionesT x1 lt + aparicionesT x1 rt

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x lt rt) = x : (leaves lt ++ leaves rt)

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ EmptyT EmptyT) = 1
heightT (NodeT _ lt rt) = 1 + max (heightT lt) (heightT rt)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x lt rt) = (NodeT x (mirrorT rt) (mirrorT lt))

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x lt rt) = toList lt ++ [x] ++ toList rt

levelN :: Int -> Tree a -> [a]
levelN 0 (NodeT x _ _) = [x]
levelN n (NodeT x lt rt) = levelN (n-1) lt ++ levelN (n-1) rt

--

concatenarHojasPorNivel :: [[a]] -> [[a]] -> [[a]]
concatenarHojasPorNivel xss [] = xss
concatenarHojasPorNivel [] yss = yss
concatenarHojasPorNivel (xs:xss) (ys:yss) = (xs ++ ys) : concatenarHojasPorNivel xss yss


listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x lt rt) = [x] : concatenarHojasPorNivel (listPerLevel lt) (listPerLevel rt)

--