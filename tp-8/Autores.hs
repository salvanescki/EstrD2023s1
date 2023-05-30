----------------------------------------------------- Ejercicio a) -----------------------------------------------------
{-
 Implementar las siguientes funciones como usuario del TAD Organizador, establecer su eficiencia y justificarla:
-}

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
{-
    PROP: Dadas dos Personas y un Organizador, denota el conjunto de aquellos programas en las que las Personas programaron juntas
    PRECOND: Las Personas deben existir en el Organizador
    EFICIENCIA:
    * programasDe es O(log P) en peor caso, con P la cant total de Personas del Organizador. Se llama 2 veces
    * intersection depende de la cantidad de elementos de los Sets. En este caso, programasDe devuelve los programas en los que trabajó
      un determinado programador. En el peor caso, dicho programador trabajó en todos los programas del Organizador (C checksums).
      En dicho caso, su costo es O(C log C)
    Por lo que, el costo total de programasEnComun con esta implementación es O(log P + log P + C log C)
-}
programasEnComun p1 p2 org = intersection (programasDe org p1) (programasDe org p2)

-- AUX
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--

esUnGranHacker :: Organizador -> Persona -> Bool
{-
    PROP: Denota verdadero si la Persona indicada aparece como autor de todos los programas del Organizador.
    PRECOND: La Persona debe existir en el Organizador
    EFICIENCIA:
    * nroProgramasDePersona tiene costo O(log P), siendo P la cant de Personas del Organizador
    * todosLosProgramas tiene costo O(C), siendo C la cant de códigos del Organizador
    * longitud tiene costo O(C), ya que recorre una lista con C elementos
    * == es O(1)
    Por lo tanto, el costo de esUnGranHacker es O(log P + 1 + C + C) = O(log P + C)
-}
esUnGranHacker org p = (nroProgramasDePersona org p) == longitud (todosLosProgramas org)

----------------------------------------------------- Ejercicio b) -----------------------------------------------------
{-
 Implementar el TAD Organizador suponiendo el siguiente tipo de representación:

    data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

 a) Escribir los invariantes de representación para poder crear elementos válidos del TAD
 b) Implementar las funciones de la interfaz, respetando las restricciones de eficiencia pedidas. Justifique en cada caso por
    qué se obtiene la eficiencia buscada
-}

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
{- 
    INV. REP: en MkO mc mp
    * Todas las Personas asignadas a un Checksum en mc, pertenecen a mp, y viceversa.
    * Todos los Checksum de mc, están asignados a alguna Persona en mp, y viceversa.
-}

nuevo :: Organizador
{-
    Propósito: Un organizador vacío.
    Eficiencia: O(1)
-}
nuevo = MkO emptyM emptyM

--

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
{- 
    Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
    de dicho programa.
    Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
    no está vacío.
    Eficiencia:
    * agregarProgramaACodigos es O(log C)
    * agregarProgramaAPersonas es O(P(log P + log c' + 1))
    Costo total de agregarPrograma es O(log C + P(log P + log c' + 1))
-}
agregarPrograma (MkO mc mp) c sp = MkO (agregarProgramaACodigos c sp mc) (agregarProgramaAPersonas sp c mp)

agregarProgramaACodigos :: Checksum -> Set Persona -> Map Checksum (Set Persona) -> Map Checksum (Set Persona)
{-
    Precondición: el identificador del programa que se agrega no fue usado previamente en el Map, y el Set de personas
    no está vacío.
    Eficiencia: O(log C), ya que assocM tiene dicho costo con los elementos del Map (Y mc tiene C elementos).
-}
agregarProgramaACodigos c sp mc = assocM c sp mc

agregarProgramaAPersonas :: Set Persona -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
{-
    Precondición: el identificador del programa que se agrega no fue usado previamente en el Map, y el Set de personas
    no está vacío.
    Eficiencia: 
    * agregarProgramaACadaPersona cuesta O(P (log P + log c'))
    * set2List cuesta O(P) en el peor caso (todas las Personas).
    Costo total de agregarProgramaAPersonas es O(P(log P + log c' + 1))
-}
agregarProgramaAPersonas sp c mp = agregarProgramaACadaPersona (set2List sp) c mp

agregarProgramaACadaPersona :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
{-
    Precondición: el identificador del programa que se agrega no fue usado previamente en el Map, y la lista de personas
    no está vacía.
    Eficiencia: Para cada persona p de la lista de personas (peor caso P personas)
    * fromJust tiene costo O(1)
    * lookupM en mp (P elementos), tiene costo O(log P)
    * Sea c' la cant de programas en los que participa el p actual, addS tiene costo O(log c')
    * assocM trabaja sobre mp, por lo que su costo es O(log P)
    El costo total de agregarProgramaACadaPersona es O(P (log P + log c'))
-}
agregarProgramaACadaPersona [] _ mp = mp
agregarProgramaACadaPersona (p:ps) c mp = 
            let codigos = fromJust(lookupM p mp)
             in assocM p (addS c codigos) (agregarProgramaACadaPersona ps c mp)

--
todosLosProgramas :: Organizador -> [Checksum]
{- 
    Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
    Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
    Ya que, domM tiene un costo O(C) siendo C las claves del Map mc.
-}
todosLosProgramas (MkO mc mp) = domM mc

autoresDe :: Organizador -> Checksum -> Set Persona
{- 
    Propósito: denota el conjunto de autores que aparecen en un programa determinado.
    Precondición: el Checksum debe corresponder a un programa del organizador.
    Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
    fromJust tiene costo O(1), mientras que lookupM tiene costo O(log C), con C las claves del Map mc.
-}
autoresDe (MkO mc _) c = fromJust (lookupM c mc)

programasDe :: Organizador -> Persona -> Set Checksum
{- 
    Propósito: denota el conjunto de programas en los que participó una determinada persona.
    Precondición: la persona debe existir en el organizador.
    Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
    fromJust tiene costo O(1), mientras que lookupM tiene costo O(log P), con P las claves del Map mp.
-}
programasDe (MkO _ mp) p = fromJust (lookupM p mp)

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
{- 
    Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
    Precondición: las personas deben ser distintas.
    Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
    programas del organizador, y C la cantidad total de programas.
    Justificación:
    * sizeS tiene costo O(1)
    * programasDe es O(log P) en peor caso, con P la cant total de Personas del Organizador. Se llama 2 veces
    * intersection depende de la cantidad de elementos de los Sets. En este caso, programasDe devuelve los programas en los que trabajó
      un determinado programador. En el peor caso, dicho programador trabajó en todos los programas del Organizador (C checksums).
      En dicho caso, su costo es O(C log C)
    Por lo que, el costo total de programasEnComun con esta implementación es O(log P + log P + C log C) = O(log P + C log C)
-}
programaronJuntas org p1 p2 = sizeS (intersection (programasDe org p1) (programasDe org p2)) > 0

nroProgramasDePersona :: Organizador -> Persona -> Int
{- 
    Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
    Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador. 
-}
nroProgramasDePersona org p = sizeS (programasDe org p)

----------------------------------------------------- Ejercicio c) -----------------------------------------------------
{-
 Implementar una variante del TAD Organizador suponiendo que en la interfaz del TAD Organizador se agrega una nueva
 operación:

    elMayorPrograma :: Organizador -> Maybe Checksum
        Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
        Nothing si no puede devolver un programa.
        Eficiencia: O(1) en peor caso.

 Esto puede requerir modificar el tipo de representación, agregar invariantes, y modificar operaciones existentes. Reescribir
 sólo las operaciones que tienen cambios sustanciales y no en las que, por ejemplo, sólo se modifica un pattern matching
-}

{- 
    La consigna me asegura que Checksum tiene comparadores de Ord y Eq. Por lo que supongo que el orden está dado por el size
    que tiene el Set de Personas asignadas a él. En el caso de que no sea así o bien tendría que cambia el TAD Checksum, o bien
    crear un nuevo TAD Programa que contenga su Checksum, el Set Persona y el sizeS del mismo. En dicho caso, reemplazaría el mc
    por un MaxHeap de Programas y adaptaría todas las funciones de acceso de mc actuales a acceso en el TAD Programa.
-}

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) (MaxHeap Checksum)
{- 
    INV. REP: en MkO mc mp h
    * No hay repetidos en h
    * Todas las Personas asignadas a un Checksum en mc, pertenecen a mp, y viceversa.
    * Todos los Checksum de mc, están asignados a alguna Persona en mp, y viceversa.
    * Todos los Checksum de mc están en h, y viceversa.
-}

elMayorPrograma :: Organizador -> Maybe Checksum
elMayorPrograma (MkO _ _ h) = maxH h

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
{- 
    Nuevo costo total de agregarPrograma es O(log C + P(log P + log c' + 1) + log C)
-}
agregarPrograma (MkO mc mp h) c sp = MkO (agregarProgramaACodigos c sp mc) (agregarProgramaAPersonas sp c mp) (agregarProgramaAHeap c h)

agregarProgramaAHeap :: Checksum -> MaxHeap Checksum -> MaxHeap Checksum
-- O(log C)
agregarProgramaAHeap c h = insertH c h