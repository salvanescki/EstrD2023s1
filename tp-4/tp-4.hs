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

------------------------------------Mapa de Tesoros------------------------------------

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa

-- De la práctica anterior

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (obj:objs) = esTesoro obj || hayTesoroEnLista objs

--

ejMapa = Bifurcacion (Cofre [Chatarra])
                    (Bifurcacion (Cofre [Chatarra,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )
                    (Bifurcacion (Cofre [Chatarra,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )

ejMapa2 = Bifurcacion (Cofre [Chatarra])
                    (Bifurcacion (Cofre [Chatarra,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )
                    (Bifurcacion (Cofre [Chatarra,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Tesoro, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )

--

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre objs) = hayTesoroEnLista objs

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

--

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c) = hayTesoroEnCofre c
hayTesoroEn [] (Bifurcacion c _ _) = hayTesoroEnCofre c
hayTesoroEn ds (Fin _) = error "No existe esa direccion en el mapa"
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d
                                            then hayTesoroEn ds m1
                                            else hayTesoroEn ds m2

--

singularSi :: a -> Bool -> [a]
singularSi a True = [a]
singularSi _ _ = []

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss

caminoAlTesoro :: Mapa -> [Dir]
--PRECOND: Existe un único tesoro
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = singularSi [] (hayTesoroEnCofre c)
                                       ++ (Izq : caminoAlTesoro m1)
                                       ++ (Der : caminoAlTesoro m2)