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
-- En caso que la cantidad de tesoros sea por tesoro individual y no por cofre:
cantTesorosEnLista :: [Objeto] -> Int
cantTesorosEnLista [] = 0
cantTesorosEnLista (obj:objs) = unoSiCeroSino(esTesoro obj) + cantTesorosEnLista objs

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre objs cm) = cantTesorosEnLista objs + cantTesorosEnCamino cm
cantTesorosEnCamino (Nada cm) = cantTesorosEnCamino cm

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n cm = cantTesorosEnCamino cm >= n

{-En caso que la cantidad de tesoros en camino sea la cantidad de cofres con al menos un tesoro

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre objs cm) = unoSiCeroSino(hayTesoroEnLista objs) + cantTesorosEnCamino cm
cantTesorosEnCamino (Nada cm) = cantTesorosEnCamino cm

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n cm = cantTesorosEnCamino cm >= n
-}

--

cantTesorosDesde :: Int -> Camino -> Int
-- PRECOND: i>=0
cantTesorosDesde 0 cm = cantTesorosEnCamino cm
cantTesorosDesde n Fin = 0
cantTesorosDesde n (Cofre objs cm) = cantTesorosDesde (n-1) cm
cantTesorosDesde n (Nada cm) = cantTesorosDesde (n-1) cm

subCaminoHasta :: Int -> Camino -> Camino
-- PRECOND: i>=0
subCaminoHasta 0 _ = Fin
subCaminoHasta _ Fin = Fin
subCaminoHasta n (Nada cm) = Nada (subCaminoHasta (n-1) cm)
subCaminoHasta n (Cofre objs cm) = Cofre objs (subCaminoHasta (n-1) cm)

cantTesorosEntre :: Int -> Int -> Camino -> Int
-- PRECOND: i<=j
cantTesorosEntre i j cm = cantTesorosDesde i (subCaminoHasta (j+1) cm)

------------------------------------Tipos arbóreos------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

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
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x lt rt) = leaves lt ++ leaves rt

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

elArbolMasAltoEntre :: Tree a -> Tree a -> Tree a
elArbolMasAltoEntre EmptyT t = t
elArbolMasAltoEntre t EmptyT = t
elArbolMasAltoEntre t1 t2 = if heightT t1 > heightT t2 then t1 else t2

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x EmptyT EmptyT) = [x]
ramaMasLarga (NodeT x lt rt) = x : ramaMasLarga(elArbolMasAltoEntre lt rt)

--

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x lt rt) = [x] :
                                   consACada x (todosLosCaminos lt)
                                ++ consACada x (todosLosCaminos rt)

todosLosCaminosMaximales :: Tree a -> [[a]]
todosLosCaminosMaximales EmptyT = []
todosLosCaminosMaximales (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminosMaximales (NodeT x lt rt) = consACada x (todosLosCaminosMaximales lt ++ todosLosCaminosMaximales rt)

-- Expresiones Aritméticas --

data ExpA = Valor Int
        | Sum ExpA ExpA
        | Prod ExpA ExpA
        | Neg ExpA
        deriving Show

--

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum exp1 exp2) = eval exp1 + eval exp2
eval (Prod exp1 exp2) = eval exp1 * eval exp2
eval (Neg exp) = (-1) * (eval exp)

--

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) exp = exp
simplificarSuma exp (Valor 0) = exp
simplificarSuma exp1 exp2 = Sum exp1 exp2

simplificarProducto :: ExpA -> ExpA -> ExpA
simplificarProducto (Valor 0) exp = Valor 0
simplificarProducto (Valor 1) exp = exp
simplificarProducto exp (Valor 0) = Valor 0
simplificarProducto exp (Valor 1) = exp
simplificarProducto exp1 exp2 = Prod exp1 exp2

simplificarNegativo :: ExpA -> ExpA
simplificarNegativo (Neg exp) = exp
simplificarNegativo exp = Neg exp

simplificar :: ExpA -> ExpA
simplificar (Sum exp1 exp2) = simplificarSuma (simplificar exp1) (simplificar exp2)
simplificar (Prod exp1 exp2) = simplificarProducto (simplificar exp1) (simplificar exp2)
simplificar (Neg exp) = simplificarNegativo(simplificar exp)
simplificar exp = exp