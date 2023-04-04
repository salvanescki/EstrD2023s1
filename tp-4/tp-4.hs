------------------------------------Pizzas------------------------------------

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

ejPizza = Capa Queso(Capa Salsa (Capa Jamon (Capa (Aceitunas 8) Prepizza)))

--

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (ig:igs) = Capa ig (armarPizza igs)

--

mismoIngrediente :: Ingrediente -> Ingrediente -> Bool
mismoIngrediente Salsa Salsa = True
mismoIngrediente Queso Queso = True
mismoIngrediente Jamon Jamon = True
mismoIngrediente (Aceitunas _) (Aceitunas _) = True
mismoIngrediente _ _ = False

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ig p) = if mismoIngrediente ig Jamon
                            then sacarJamon p
                            else Capa ig (sacarJamon p)

--

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso ig = mismoIngrediente ig Salsa || mismoIngrediente ig Queso

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ig p) = esSalsaOQueso ig && tieneSoloSalsaYQueso p

--

siEsAceitunaDuplicar :: Ingrediente -> Ingrediente
siEsAceitunaDuplicar (Aceitunas cant) = Aceitunas (cant*2)
siEsAceitunaDuplicar ig = ig

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ig p) = (Capa (siEsAceitunaDuplicar ig) (duplicarAceitunas p))

-- cantidadDeCapas Pizza -> Int

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps
