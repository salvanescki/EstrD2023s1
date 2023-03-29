-- Funciones de anteriores prácticas

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSino(e == x) + apariciones e xs

--

celdaPrueba = Bolita Rojo ( Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))))

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

