module Empresa 
    (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, agregarASector, borrarEmpleado)
where

import Empleado -- (Empleado, consEmpleado, cuil, incorporarSector, sectores)
import SetV1
import MapV3

type SectorId = Int
type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)
{-
    INV. REP: en ConsE ms mc
        * ms y mc son BST.
        * ms contiene un Set con Empleados que también es un BST.
-}

-- Aux

instance Eq Empleado where
  e1 == e2 = cuil e1 == cuil e2

fromJust :: Maybe a -> a
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

-- S = cant de Sectores de la Empresa
-- E = cant de Empleados de la Empresa

{-
    Costo O(1) ya que solo crea una Empresa vacía.
-}
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

{-
    Costo O(log E) ya que aplica 1 vez fromJust, de costo O(1) y lookupM, de costo logarítmico, sobre el Map de Empleados
    el cual tiene todos los empleados de la Empresa (E Empleados). Por lo tanto, O(log E).
-}
buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- PRECOND: El CUIL debe pertenece a algún Empleado de la Empresa
buscarPorCUIL c (ConsE _ mc) = fromJust(lookupM c mc)

{-
    Costo O(log S + E) ya que, si bien las aplica 1 sola vez:
    * lookupM se aplica sobre el Map de Sectores, por lo que tiene costo O(log S)
    * fromJust tiene costo constante, O(1)
    * setToList tiene costo lineal y se aplica sobre lo que devuelve lookupM, es decir una lista de Empleados en el Sector.
      El peor caso es que dicha lista contuviera a todos los Empleados de la Empresa. Por lo que su costo es O(E)
    En total, empleadosDelSector tiene un costo de O(log S + E) 
-}
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector s (ConsE ms _) = setToList(fromJust(lookupM s ms))

{-
    Costo O(E) ya que utiliza keys, de costo lineal, sobre el Map que tiene todos los CUIL como claves (E CUILs, 1 por Empleado de la Empresa).
-}
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ mc) = keys mc

{-
    Costo O(S) ya que utiliza keys, de costo lineal, sobre el Map que tiene todos los SectorIDs como claves (S Sectores).
-}
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE ms _) = keys ms

{-
    Costo O(log S) ya que aplica assocM, de costo O(log K) con K la cantidad de claves del Map, sobre el Map que contiene como claves a los SectorIDs (S Sectores).
-}
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector s (ConsE ms mc) = ConsE (assocM s emptyS ms) mc

{-
    Al menos en mi implementación utilizo muchas funciones así que voy a hacer un análisis lo más detallado posible:
    * agregarEmpleadoASectores utiliza, por cada elemento de la lista de SectorIDs pasada por el Usuario (N Sectores) (N <= S):
        * lookupM en el Map de SectorIDs (S Sectores), por lo que su costo es O(log S)
        * fromJust, O(1)
        * addS sobre el Set que devuelve lookupM, si consideramos el peor caso (que todos los empleados están trabajando en ese Sector) O(log E) en promedio
        * assocM, tiene costo O(log S) con (S Sectores)
    * agregarEmpleado, además de utilizar agregarEmpleadoASectores O(N (log S + log E)), utiliza un assocM en el Map de CUILs (E Empleados).
    * incorporarSector tiene costo O(log S) e incorporarSectores O(S log S)
      Por lo tanto, el costo final es O(N(log S + log E) + log E) aprox. O(S log S + N log E)
-}
incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores [] e = e
incorporarSectores (s:ss) e = incorporarSector s (incorporarSectores ss e)

agregarEmpleadoASectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoASectores e []     ms  = ms
agregarEmpleadoASectores e (s:ss) ms = assocM s (addS e (fromJust(lookupM s ms))) (agregarEmpleadoASectores e ss ms)

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado ss c (ConsE ms mc) = let empleado = (incorporarSectores ss (consEmpleado c))
                                      in ConsE (agregarEmpleadoASectores empleado ss ms) (assocM c empleado mc)

{-
    El costo de agregarASector, como uso agregarEmpleadoASectores, es el mismo O(N log S + N log E).
    Pero como N en este caso es 1 (ya que solo agrego a 1 sector), el costo pasa a ser O(log S + log E).
-}

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- PRECOND: El CUIL debe pertenece a un Empleado en la Empresa
agregarASector s c (ConsE ms mc) = let empleado = (fromJust (lookupM c mc))
                                    in ConsE (agregarEmpleadoASectores empleado [s] ms) mc

{-
    borrarEmpleadoDeSectores es muy similar a agregarEmpleadoASectores, ya que es una lógica muy parecida:
    * Por cada Sector en el que haya trabajado el Empleado (N Sectores) (N <= S):
        * assocM sobre el Map de Sectores O(log S)
        * lookupM sobre el Map de Sectores O(log S)
        * fromJust, O(1)
        * removeS, O(log E) en el peor caso posible.
    borrarEmpleadoDeCUIL tiene costo O(log E)
    borrarEmpleado utiliza, además de borrarEmpleadoDeCUIL y borrarEmpleadoDeSectores, lookupM en el Map de CUILs, costo O(log E).
    Por lo tanto, borrarEmpleado tiene un costo de O(N log S + N log E)
-}
borrarEmpleadoDeSectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
borrarEmpleadoDeSectores [] _ ms = ms
borrarEmpleadoDeSectores (s:ss) e ms = assocM s (removeS e (fromJust(lookupM s ms))) (borrarEmpleadoDeSectores ss e ms)

borrarEmpleadoDeCUIL :: CUIL -> Map CUIL Empleado -> Map CUIL Empleado
borrarEmpleadoDeCUIL c mc = deleteM c mc

borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE ms mc) = let empleado = fromJust (lookupM c mc)
                                  in ConsE (borrarEmpleadoDeSectores (sectores empleado) empleado ms) (borrarEmpleadoDeCUIL c mc)