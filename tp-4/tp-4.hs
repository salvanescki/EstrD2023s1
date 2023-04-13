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
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

-- De la práctica anterior

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (obj:objs) = esTesoro obj || hayTesoroEnLista objs

agruparPorNivel :: [[a]] -> [[a]] -> [[a]]
agruparPorNivel xss [] = xss
agruparPorNivel [] yss = yss
agruparPorNivel (xs:xss) (ys:yss) = (xs ++ ys) : agruparPorNivel xss yss

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss

-- heightT adaptado a Mapa
longCamino :: Mapa -> Int
longCamino (Fin _) = 0
longCamino (Bifurcacion _ (Fin _) (Fin _)) = 1
longCamino (Bifurcacion _ c1 c2) = 1 + max (longCamino c1) (longCamino c2)

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

ejMapa3 = Bifurcacion (Cofre [Tesoro])                                                                                
                    (Bifurcacion (Cofre [Tesoro,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Bifurcacion (Cofre [Chatarra, Chatarra, Chatarra, Chatarra])
                                (Fin (Cofre []))
                                (Fin (Cofre [])))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )
                    (Bifurcacion (Cofre [Tesoro,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Tesoro, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Tesoro, Chatarra]))
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

{-Tengo que arreglarlo, solo devuelve los casos donde el tesoro se encuentra en una Bifurcación, pero no los casos en donde está en un Fin

loboYSubordinadosDeLobo :: Lobo -> [Nombre]
loboYSubordinadosDeLobo (Cria n) = [n]
loboYSubordinadosDeLobo (Explorador n _ l1 l2) = n: nombreDe l1 : nombreDe l2 : loboYSubordinadosDeLobo l1 ++ loboYSubordinadosDeLobo l2
loboYSubordinadosDeLobo (Cazador n _ l1 l2 l3) = n: nombreDe l1 : nombreDe l2 : nombreDe l3 : loboYSubordinadosDeLobo l1 ++ loboYSubordinadosDeLobo l2 ++ loboYSubordinadosDeLobo l3

siEsSubordinadoAgregarSuperior :: Nombre -> [Nombre] -> Nombre -> [Nombre] -> [Nombre]
siEsSubordinadoAgregarSuperior n1 subs n2 sups = if pertenece n1 subs then n2 : sups else sups

superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL n1 (Cria _) = []
superioresDelCazadorL n1 (Explorador n2 _ l1 l2) = siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l1) n2 (superioresDelCazadorL n1 l1)
                                                    ++ siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l2) n2 (superioresDelCazadorL n1 l2)
superioresDelCazadorL n1 (Cazador n2 _ l1 l2 l3) = siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l1) n2 (superioresDelCazadorL n1 l1)
                                                    ++ siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l2) n2 (superioresDelCazadorL n1 l2)
                                                    ++ siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l3) n2 (superioresDelCazadorL n1 l3)
superioresDelCazador :: Nombre -> Manada -> [Nombre]
--PRECOND: hay un cazador con dicho nombre y es único.
superioresDelCazador n (M l) = superioresDelCazadorL n l
-}

caminoAlTesoro :: Mapa -> [Dir]
--PRECOND: Existe un único tesoro
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c
                                        then [] ++ (Izq : caminoAlTesoro m1) ++ (Der : caminoAlTesoro m2)
                                        else caminoAlTesoro m1 ++ caminoAlTesoro m2

--

esMasAltoQue :: Mapa -> Mapa -> Bool
esMasAltoQue t1 t2 = longCamino t1 > longCamino t2

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if esMasAltoQue m1 m2
                                                then (Izq : caminoDeLaRamaMasLarga m1)
                                                else (Der : caminoDeLaRamaMasLarga m2)

--

tesorosDeListaDeObjetos :: [Objeto] -> [Objeto]
tesorosDeListaDeObjetos [] = []
tesorosDeListaDeObjetos (obj:objs) = if esTesoro obj 
                                        then obj : tesorosDeListaDeObjetos objs
                                        else tesorosDeListaDeObjetos objs

tesorosDeCofre :: Cofre -> [Objeto]
tesorosDeCofre (Cofre objs) = tesorosDeListaDeObjetos objs

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [tesorosDeCofre c]
tesorosPorNivel (Bifurcacion c m1 m2) = tesorosDeCofre c : agruparPorNivel (tesorosPorNivel m1) (tesorosPorNivel m2)

--

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = [[]]
todosLosCaminos (Bifurcacion c m1 m2) = consACada Izq (todosLosCaminos m1)
                                     ++ consACada Der (todosLosCaminos m2)

------------------------------------Nave Espacial------------------------------------

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
        deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
        deriving Show
data Sector = S SectorId [Componente] [Tripulante]
        deriving Show

type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

data Nave = N (Tree Sector)
        deriving Show

ejNave = N (NodeT (S "j6" [LanzaTorpedos, (Motor 1), (Almacen [Comida, Oxigeno, Torpedo, Combustible])] ["Jesus", "Juan", "Jose", "Jeremias", "Jacobo", "Julián"])
                (NodeT (S "a2" [LanzaTorpedos, (Motor 2), (Almacen [Comida, Oxigeno, Torpedo, Combustible])] ["Ariel", "Amadeo"])
                    (EmptyT)
                    (EmptyT)
                )
                (NodeT (S "b2" [LanzaTorpedos, (Motor 3), (Almacen [Comida, Oxigeno, Torpedo, Combustible])] ["Benito", "Bartolomeo"])
                    (EmptyT)
                    (EmptyT)
                )
            )
-- De prácticas anteriores

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

--

idSector :: Sector -> SectorId
idSector (S id _ _) = id

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 ++ sectoresT t2

sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

--

propulsionC :: Componente -> Int
propulsionC (Motor p) = p
propulsionC _ = 0

propulsionCs :: [Componente] -> Int
propulsionCs [] = 0
propulsionCs (c:cs) = propulsionC c + propulsionCs cs

propulsionS :: Sector -> Int
propulsionS (S _ cs _) = propulsionCs cs

propulsionT :: Tree Sector -> Int
propulsionT EmptyT = 0
propulsionT (NodeT s t1 t2) = propulsionS s + propulsionT t1 + propulsionT t2

poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = propulsionT t

--

barrilesC :: Componente -> [Barril]
barrilesC (Almacen barriles) = barriles
barrilesC _ = []

barrilesCs :: [Componente] -> [Barril]
barrilesCs [] = []
barrilesCs (c:cs) = barrilesC c ++ barrilesCs cs

barrilesS :: Sector -> [Barril]
barrilesS (S _ cs _) = barrilesCs cs

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesS s ++ barrilesT t1 ++ barrilesT t2

barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

--

esMismoSectorId :: SectorId -> SectorId -> Bool
esMismoSectorId id1 id2 = id1 == id2

agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes cs1 (S id cs2 ts) = S id (cs1 ++ cs2) ts

agregarASectorS :: [Componente] -> SectorId -> Sector -> Sector
agregarASectorS cs1 id1 s = if esMismoSectorId id1 (idSector s)
                                            then agregarComponentes cs1 s
                                            else s

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT cs id EmptyT = EmptyT
agregarASectorT cs id (NodeT s t1 t2) = NodeT (agregarASectorS cs id s) (agregarASectorT cs id t1) (agregarASectorT cs id t2)

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- En caso de no existir el sector en la nave, no añade la lista
agregarASector [] _ n = n
agregarASector cs id (N t) = N (agregarASectorT cs id t)

--

asignarTripulanteAS :: Tripulante -> Sector -> Sector
asignarTripulanteAS tp (S id cps tps) = S id cps (tp:tps)

asignarTripulanteAT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAT _ _ EmptyT = EmptyT
asignarTripulanteAT tp [] (NodeT s t1 t2) = NodeT s (asignarTripulanteAT tp [] t1) (asignarTripulanteAT tp [] t2)
asignarTripulanteAT tp (st:sts) (NodeT s t1 t2) = if esMismoSectorId st (idSector s) 
                                                    then NodeT (asignarTripulanteAS tp s) (asignarTripulanteAT tp sts t1) (asignarTripulanteAT tp sts t2)
                                                    else NodeT s (asignarTripulanteAT tp sts t1) (asignarTripulanteAT tp sts t2)

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PRECOND: Todos los id de la lista pertenecen a la Nave
asignarTripulanteA tp [] (N t) = N t
asignarTripulanteA tp sts (N t) = N (asignarTripulanteAT tp sts t)

--

esTripulanteAsignadoASector :: Tripulante -> Sector -> Bool
esTripulanteAsignadoASector tp (S id cps tps) = pertenece tp tps

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT tp EmptyT = []
sectoresAsignadosT tp (NodeT s t1 t2) = if esTripulanteAsignadoASector tp s 
                                            then idSector s : sectoresAsignadosT tp t1 ++ sectoresAsignadosT tp t2
                                            else sectoresAsignadosT tp t1 ++ sectoresAsignadosT tp t2

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tp (N t) = sectoresAsignadosT tp t

--

sinRepetir :: Eq a => [a] -> [a]
sinRepetir [] = []
sinRepetir (x:xs) = if pertenece x xs
                        then xs
                        else x:xs

tripulantesS :: Sector -> [Tripulante]
tripulantesS (S id cps tps) = tps

tripulantesT :: Tree Sector -> [Tripulante]
tripulantesT EmptyT = []
tripulantesT (NodeT s t1 t2) = tripulantesS s ++ tripulantesT t1 ++ tripulantesT t2

tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = sinRepetir(tripulantesT t)

------------------------------------Manada de Lobos------------------------------------

type Presa = String
type Territorio = String
type Nombre = String
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cria Nombre
            deriving Show
data Manada = M Lobo
            deriving Show

--

ejManada = M (Cazador "Roberto" ["Presa1", "Presa2", "Presa3", "Presa4", "Presa5", "Presa6"] 
                (Explorador "Diego" ["Quilmes", "Berazategui"] 
                    (Cria "Cria2")
                    (Cria "Cria3")
                )
                (Explorador "Dora" ["Bernal", "Berazategui"] 
                    (Cria "Cria4")
                    (Cria "Cria5")
                )
                (Cria "Cria1")
            )

ejManada2 = M (Cazador "Beta" ["Presa1", "Presa2"]
                (Cazador "Gamma" ["Presa1"] 
                    (Cria "Cria2")
                    (Cria "Cria3")
                    (Cria "Cria4")
                )
                (Cazador "Omega" [] 
                    (Cria "Cria5")
                    (Cria "Cria6")
                    (Cazador "Alfa" ["Presa1", "Presa2", "Presa3"]
                        (Cria "Cria7")
                        (Cria "Cria8")
                        (Cria "Cria9")
                    )
                )
                (Cria "Cria1")
            )

--De las prácticas anteriores
unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0
--

esCria :: Lobo -> Bool
esCria (Cria _) = True
esCria _ = False

unoSiEsCriaSinoCantDeCrias :: Lobo -> Int
unoSiEsCriaSinoCantDeCrias (Cria _) = 1
unoSiEsCriaSinoCantDeCrias l = cantidadDeCriasL l

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Explorador _ _ l1 l2) = unoSiEsCriaSinoCantDeCrias l1 + unoSiEsCriaSinoCantDeCrias l2
cantidadDeCriasL (Cazador _ _ l1 l2 l3) = unoSiEsCriaSinoCantDeCrias l1 + unoSiEsCriaSinoCantDeCrias l2 + unoSiEsCriaSinoCantDeCrias l3

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M l) = unoSiEsCriaSinoCantDeCrias l

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cria _) = 0
cantidadDeAlimentoL (Explorador _ _ l1 l2) = cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cazador _ ps l1 l2 l3) = length ps + cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2 + cantidadDeAlimentoL l3

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M l) = cantidadDeAlimentoL l

buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

--

elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (n1, c1) (n2, c2) = if (c1>=c2) then (n1, c1)
                                                else (n2, c2)

elegir :: [ (Nombre, Int) ] -> (Nombre, Int)
-- PRECOND: la lista no es vacía
elegir (nc : [])  = nc
elegir (nc : ncs) = elegirEntre nc (elegir ncs)

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador n presas l1 l2 l3) = elegir [ (n, length presas)
                                                , elAlfaL l1
                                                , elAlfaL l2
                                                , elAlfaL l3
                                                ]
elAlfaL (Explorador n _ l1 l2)      = elegir [ elAlfaL l1
                                                , elAlfaL l2
                                                , (n, 0)
                                                ]
elAlfaL (Cria n)                    = (n, 0)

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaL l

--

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria _) = []
losQueExploraronL t (Cazador n ps l1 l2 l3) = losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador n ts l1 l2) = if pertenece t ts 
                                                then n : losQueExploraronL t l1 ++ losQueExploraronL t l2
                                                else losQueExploraronL t l1 ++ losQueExploraronL t l2

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

--

estaEnLosTerritorios :: Territorio -> [Territorio] -> Bool
estaEnLosTerritorios _ [] = False
estaEnLosTerritorios t1 (t2:t2s) = if t1 == t2
                                    then True
                                    else False || estaEnLosTerritorios t1 t2s

sinTerritoriosRepetidos :: [Territorio] -> [Territorio]
sinTerritoriosRepetidos [] = []
sinTerritoriosRepetidos (t:ts) = if estaEnLosTerritorios t ts
                                    then sinTerritoriosRepetidos ts
                                    else t : sinTerritoriosRepetidos ts

territoriosL :: Lobo -> [Territorio]
territoriosL (Cria _) = []
territoriosL (Explorador _ ts l1 l2) = ts ++ territoriosL l1 ++ territoriosL l2
territoriosL (Cazador _ _ l1 l2 l3) = territoriosL l1 ++ territoriosL l2 ++ territoriosL l3

territorios :: Manada -> [Territorio]
territorios (M l) = sinTerritoriosRepetidos(territoriosL l)

exploradoresPorTerritorioDeLista :: [Territorio] -> Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorioDeLista [] _ = []
exploradoresPorTerritorioDeLista (t:ts) m = (t, losQueExploraron t m) : exploradoresPorTerritorioDeLista ts m

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio m = exploradoresPorTerritorioDeLista (territorios m) m

--

nombreDe :: Lobo -> Nombre
nombreDe (Cria n) = n
nombreDe (Cazador n _ _ _ _) = n
nombreDe (Explorador n _ _ _) = n

esMismoNombre :: Nombre -> Nombre -> Bool
esMismoNombre n1 n2 = n1 == n2

listaSiTienePorSubordinado :: Nombre -> [Nombre] -> [Nombre]
listaSiTienePorSubordinado n ns = if pertenece n ns then ns else []

loboYSubordinadosDeLobo :: Lobo -> [Nombre]
loboYSubordinadosDeLobo (Cria n) = [n]
loboYSubordinadosDeLobo (Explorador n _ l1 l2) = n: nombreDe l1 : nombreDe l2 : loboYSubordinadosDeLobo l1 ++ loboYSubordinadosDeLobo l2
loboYSubordinadosDeLobo (Cazador n _ l1 l2 l3) = n: nombreDe l1 : nombreDe l2 : nombreDe l3 : loboYSubordinadosDeLobo l1 ++ loboYSubordinadosDeLobo l2 ++ loboYSubordinadosDeLobo l3

siEsSubordinadoAgregarSuperior :: Nombre -> [Nombre] -> Nombre -> [Nombre] -> [Nombre]
siEsSubordinadoAgregarSuperior n1 subs n2 sups = if pertenece n1 subs then n2 : sups else sups

superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL n1 (Cria _) = []
superioresDelCazadorL n1 (Explorador n2 _ l1 l2) = siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l1) n2 (superioresDelCazadorL n1 l1)
                                                    ++ siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l2) n2 (superioresDelCazadorL n1 l2)
superioresDelCazadorL n1 (Cazador n2 _ l1 l2 l3) = siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l1) n2 (superioresDelCazadorL n1 l1)
                                                    ++ siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l2) n2 (superioresDelCazadorL n1 l2)
                                                    ++ siEsSubordinadoAgregarSuperior n1 (loboYSubordinadosDeLobo l3) n2 (superioresDelCazadorL n1 l3)

superioresDelCazador :: Nombre -> Manada -> [Nombre]
--PRECOND: hay un cazador con dicho nombre y es único.
superioresDelCazador n (M l) = superioresDelCazadorL n l