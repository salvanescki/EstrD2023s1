{-
Sector, siendo C la cantidad de contenedores y T la cantidad de tripulantes:
crearS :: SectorId -> Sector O(1)
sectorId :: Sector -> SectorId O(1)
componentesS :: Sector -> [Componente] O(1)
tripulantesS :: Sector -> Set Nombre O(1)
agregarC :: Componente -> Sector -> Sector O(1)
agregarT :: Nombre -> Sector -> Sector O(log T)

Tripulante, siendo S la cantidad de sectores:
crearT :: Nombre -> Rango -> Tripulante O(1)
asignarS :: SectorId -> Tripulante -> Tripulante
O(log S)
sectoresT :: Tripulante -> Set SectorId O(1)
nombre :: Tripulante -> String O(1)
rango :: Tripulante -> Rango

Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a O(1)
addS :: a -> Set a -> Set a O(log N)
belongsS :: a -> Set a -> Bool O(log N)
unionS :: Set a -> Set a -> Set a O(N log N)
setToList :: Set a -> [a] O(N)
sizeS :: Set a -> Int O(1)

Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v O(1)
assocM :: k -> v -> Map k v -> Map k v O(log K) 
lookupM :: k -> Map k v -> Maybe v O(log K)
deleteM :: k -> Map k v -> Map k v O(log K)
domM :: Map k v -> [k] O(K)

MaxHeap, siendo M la cantidad de elementos en la heap:
emptyH :: MaxHeap a O(1)
isEmptyH :: MaxHeap a -> Bool O(1)
insertH :: a -> MaxHeap a -> MaxHeap a O(log M)
maxH :: MaxHeap a -> a O(1)
deleteMaxH :: MaxHeap a -> MaxHeap a O(log M)
-}

type SectorId = String
type Nombre = String
type Rango = String

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

------------------------------------------ Invariantes ------------------------------------------

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
{-
    INV. REP: en N ms mt t
    * En ms, el SectorId define univocamente a su Sector y, además, se corresponde con el id del Sector.
    * Cada Nombre en mt define unívocamente a su Tripulante asignado y, además, se corresponde con el nombre de ese Tripulante.
    * No hay elementos repetidos en t.
    * Sea t' un Tripulante y s un Sector. El nombre de t' está en los tripulantes de s, si y solo si, el SectorId de s está en los sectores de t'.
      Es decir, si un tripulante está en un sector, ese sector está entre los sectores del tripulante.
    * Todos los Tripulantes de t están en mt, y viceversa.
-}

------------------------------------------ Implementación ------------------------------------------

-- Siendo T la cant de Tripulantes y S la cant de Sectores

construir :: [SectorId] -> Nave
{-
    PROP: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
    EFICIENCIA: O(S)
    No me quedó O(S), ahora mismo está implementada con O(S log S)
-}
construir [] = N emptyM emptyM emptyH
construir (id:ids) = N (assocM id (crearS id) (construir ids)) emptyM emptyH

--

ingresarT :: Nombre -> Rango -> Nave -> Nave
{-
    PROP: Incorpora un tripulante a la nave, sin asignarle un sector.
    EFICIENCIA: O(log T)
-}
ingresarT n r (N ms mt t) = 
    let  nuevoTripulante = (crearT n r)
     in  N ms (assocM n nuevoTripulante mt) (insertH nuevoTripulante t)
{-
    En este caso, por el INV. REP. sabemos que t y mt tienen la misma cantidad de elementos (T). Por lo que:
    * assocM tiene un costo de O(log T)
    * insertH tiene un costo de O(log T)
    Ambas se aplican solo una vez, por lo que el costo total de ingresarT es O(log T)
-}

--

sectoresAsignados :: Nombre -> Nave -> Set SectorId
{-
    PROP: Devuelve los sectores asignados a un tripulante.
    PRECOND: Existe un tripulante con dicho nombre.
    EFICIENCIA: O(log M)
-}
sectoresAsignados n (N _ mt _) = sectoresT (fromJust (lookupM n mt))
{-
    Como mencioné anteriormente, la cant de elementos en el MaxHeap es la misma que en el Map (T).
    fromJust O(1), lookupM O(log T), sectoresT O(1)
    sectoresAsignados costo total O(log T)
-}

--

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
{- 
    PROP: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
    PRECOND: Existe un sector con dicho id.
    EFICIENCIA: O(log S) 
-}
datosDeSector id (N ms _ _) = let sector = (fromJust(lookupM id ms))
                               in (tripulantesS sector, componentesS sector)
{-
    tripulantesS O(1), componentesS O(1)
    y lookupM se hace 2 veces, O(log S) ya que se hace sobre ms que tiene S Sectores.
    Costo total de datosDeSector es O(log S)
-}

--

tripulantesT :: MaxHeap Tripulante -> [Tripulante]
{- 
    PROP: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-}
tripulantesT t = if isEmptyH t 
                    then []
                    else maxH t : tripulantesT (deleteMaxH t)


tripulantesN :: Nave -> [Tripulante]
{- 
    PROP: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
    EFICIENCIA: O(log T)
    No me quedó O(log T) sino O(T log T)
-}
tripulantesN (N _ _ t) = tripulantesT t

{-
    Para cada tripulante del MaxHeap, tomo el máximo y devuelvo el MaxHeap sin este.
    Obtener el máximo es O(1), pero devolver el MaxHeap sin el máximo es O(log T).
    Esto lo estoy haciendo T veces, ya que antes de T llamados el Heap no queda vacío.
    Por lo que, el costo total de tripulantesN es O(T log T)
-}

--

agregarComponentesASector :: [Componente] -> Sector -> Sector
-- Eficiencia: O(C)
agregarComponentesASector [] s = s
agregarComponentesASector (c:cs) s = agregarC c (agregarComponentesASector cs s)

agregarASectorEnMs :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
-- Eficiencia: O(C + log S)
agregarASectorEnMs cs id ms = let sector = (fromJust(lookupM id ms))
                               in assocM id (agregarComponentesASector cs sector) ms

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
{- 
    PROP: Asigna una lista de componentes a un sector de la nave.
    EFICIENCIA: O(C + log S), siendo C la cantidad de componentes dados.
-}
agregarASector cs id (N ms mt t) = N (agregarASectorEnMs cs id ms) mt t

{-
    En agregarASectorEnMs se llama a assocM una vez, con costo O(log S) siendo S los
    elementos del Map de Sectores ms. Y agregarComponentesASector realiza agregarC O(1),
    para cada componente de la lista dada (C componentes), por lo que es O(C).
    Por lo tanto, agregarASector tiene un costo de O(C + log S)
-}

--

borrarH :: Nombre -> MaxHeap Tripulante -> MaxHeap Tripulante
{- 
    Eficiencia:
    Hace llamados recusivos sobre T elementos,
    * maxH, nombre e == son O(1)
    * deleteMaxH tiene costo O(log T) y se llama en todos los casos
    * insertH es O(log T) y se llama en todos los casos salvo 1.
    Por lo tanto, hace T veces O(log T). Su costo total es O(T log T)
-}
borrarH n t = 
    let m = maxH t
     in if n == nombre m
            then deleteMaxH t
            else insertH m (borrarH n (deleteMaxH t))

reemplazarEnHeap :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
-- Eficiencia: Hace O(log T + T log T), por lo que es O(T log T)
reemplazarEnHeap t' t = insertH t' (borrarH (nombre t') t)

asignarASector :: Nombre -> SectorId -> Nave -> Nave
{- 
    PROP: Asigna un sector a un tripulante.
    NOTA: No importa si el tripulante ya tiene asignado dicho sector.
    PRECOND: El tripulante y el sector existen.
    EFICIENCIA: O(log S + log T + T log T) 
-}
asignarASector n id (N ms mt t) = 
    let nTripulante = asignarS id (fromJust(lookupM n mt)) -- O(1 + log T)
        nSector = agregarT n (fromJust(lookupM id ms))     -- O(1 + log S)
     in
        (N 
            (assocM id nSector ms)                         -- O(log S + 1 + log S) = O(log S)
            (assocM n nTripulante mt)                      -- O(log T + 1 + log T) = O(log T)
            (reemplazarEnHeap nTripulante t)               -- O(T log T)
        )

-- El costo total es O(log S + log T + T log T)

------------------------------------------ Usuario ------------------------------------------

sectoresTripulantes :: [Tripulante] -> Nave -> Set SectorId
{-
    Siendo S el peor caso, donde el tripulante tiene todos los sectores asignados
    unionS es O(S log S) y es llamado T veces
    Costo total: O(T (S log S))
-}
sectoresTripulantes [] _ = emptyS
sectoresTripulantes (t:ts) n = unionS (sectoresT t) (sectoresTripulantes ts n)

sectores :: Nave -> Set SectorId
{-
    PROP: Devuelve todos los sectores no vacíos (con tripulantes asignados)
    EFICIENCIA: 
    * tripulantesN es O(T log T)
    * sectoresTripulantes es O(T (S log S))
    El costo total de sectores es O(T(S log S + log T))
-}
sectores n = sectoresTripulantes (tripulantesN n) n

--

esTripulanteSinSectores :: Tripulante -> Nave -> Bool
-- Eficiencia: O(1)
esTripulanteSinSectores t = sizeS (sectoresT t) == 0

tripulantesSinSectores :: [Tripulante] -> [Tripulante]
-- Eficiencia: O(T)
tripulantesSinSectores [] = []
tripulantesSinSectores (t:ts) = if esTripulanteSinSectores t
                                    then t : tripulantesSinSectores ts
                                    else tripulantesSinSectores ts

sinSectoresAsignados :: Nave -> [Tripulante]
{-
    PROP: Devuelve los tripulantes que no poseen sectores asignados
    EFICIENCIA: O(T + T log T) = O(T log T)
-}
sinSectoresAsignados n = tripulantesSinSectores (tripulantesN n)

--

barrilesDe :: Almacen -> [Barril]
-- O(1)
barrilesDe (Almacen bs) = bs

esAlmacen :: Componente -> Bool
-- O(1)
esAlmacen (Almacen _) = True
esAlmacen _ = False

barrilesEnComponentes :: [Componente] -> [Barril]
{-
    Eficiencia: Siendo c' la cant de componentes pasada por parámetro. O(c')
    ya que aplica operaciones constantes por cada llamado recursivo, en c' elementos.
-}
barrilesEnComponentes [] = []
barrilesEnComponentes (c:cs) = if esAlmacen c
                                then barrilesDe c : barrilesEnComponentes cs
                                else barrilesEnComponentes cs

barrilesEnSector :: SectorId -> Nave -> [Barril]
{-
    Eficiencia:
    * lookupM en ms cuesta O(log S)
    * fromJust es O(1)
    * componentesS es O(1)
    * barrilesEnComponentes es O(c') siendo c' los componentes del sector
    En total O(log S + c')
-}
barrilesEnSector id (N ms _ _) =
    let sector = (fromJust(lookupM id ms))
     in barrilesEnComponentes (componentesS sector)

barrilesS :: [SectorId] -> Nave -> [Barril]
{-
    Por cada sector de la nave (S sectores), barrilesS aplica:
    * barrilesEnSector, tiene costo O(log S + c'), con c' los componentes del Sector
    Sea C la cantidad total de componentes de la nave
    Costo total: O(S log S + C)
-}
barrilesS [] _ = []
barrilesS (id:ids) n = barrilesEnSector id n : barrilesS ids n

barriles :: Nave -> [Barril]
{-
    PROP: Devuelve todos los barriles de los sectores asignados de la nave.
    EFICIENCIA:
    * barrilesS tiene un costo de O(S log S + C)
    * sectores tiene un costo de O(T (S log S + log T))
    * setToList tiene un costo de O(S), siendo S los Sectores de la Nave
    Costo Total: O(S log S + C + T S log S + T log T) + S) 
               = O(T(S(log S + 1) + log T) + C)
-}
barriles n = barrilesS (setToList (sectores n)) n