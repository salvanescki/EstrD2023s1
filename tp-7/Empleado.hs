module Empleado
    (Empleado, consEmpleado, cuil, incorporarSector, sectores)
where

import SetV1

type SectorId = Int
type CUIL = Int

data Empleado = ConsE CUIL (Set SectorId)
{-
    INV. REP: en ConsE c ss
    * ss es un BST
-}

consEmpleado :: CUIL -> Empleado                        -- O(1)
consEmpleado c = ConsE c emptyS

cuil :: Empleado -> CUIL                                -- O(1)
cuil (ConsE c _) = c

incorporarSector :: SectorId -> Empleado -> Empleado    -- O(log S) en promedio porque el invariante me asegura que addS tiene dicho costo al ser BST
incorporarSector s (ConsE c ss) = ConsE c (addS s ss)

sectores :: Empleado -> [SectorId]                      -- O(S)
sectores (ConsE _ ss) = setToList ss