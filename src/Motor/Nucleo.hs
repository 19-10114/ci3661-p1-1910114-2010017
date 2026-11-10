{-|
Module      : Motor.Nucleo
Description : Lógica funcional pura de ejecución de aventuras.
Maintainer  : Stefano Casale (19-10114)

Este módulo implementa la lógica puramente funcional del motor de aventuras:
- Creación del estado inicial ('estadoInicial').
- Ejecución de comandos del jugador ('ejecutar').
- Desplazamiento entre salas con verificación de salidas bloqueadas.
- Desbloqueo permanente de salidas tras el primer cruce con llave (Decisión D8).
- Manipulación de objetos: tomar, soltar, examinar e inventario.
- Variante 3: Combinar objetos conmutativamente (a + b == b + a).
- Verificación pura de la condición de victoria ('haGanado').
-}
module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  , haGanado
  , describirSala
  , mostrarDireccion
  ) where

import Data.List (delete, find)
import qualified Data.Map.Strict as Map
import Motor.Tipos

-- | Genera el estado inicial del juego a partir de un mundo validado.
estadoInicial :: Mundo -> Estado
estadoInicial m = Estado
  { estadoMundo      = m
  , estadoSalaActual = mundoInicio m
  , estadoInventario = []
  }

-- | Evalúa si el estado actual cumple la condición de victoria del mundo.
haGanado :: Estado -> Bool
haGanado est =
  case mundoMeta (estadoMundo est) of
    LlegarA salaMeta ->
      estadoSalaActual est == salaMeta
    LlevarA objMeta salaMeta ->
      estadoSalaActual est == salaMeta && objMeta `elem` estadoInventario est

-- | Ejecuta un comando puramente sobre el estado, devolviendo los mensajes
-- dirigidos al jugador y el nuevo estado del juego.
ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar cmd est =
  case cmd of
    Ir dir ->
      ejecutarIr dir est

    Mirar ->
      let msgs = case Map.lookup (estadoSalaActual est) (mundoSalas (estadoMundo est)) of
            Just sala -> describirSala (Just sala)
            Nothing   -> ["Error: la sala actual no se encuentra en el mapa del mundo."]
      in (msgs, est)

    Tomar obj ->
      ejecutarTomar obj est

    Soltar obj ->
      ejecutarSoltar obj est

    Examinar obj ->
      ejecutarExaminar obj est

    Inventario ->
      ejecutarInventario est

    Combinar obj1 obj2 ->
      ejecutarCombinar obj1 obj2 est

    Ayuda ->
      ( [ "Comandos disponibles:"
        , "  ir <dirección>               - Moverse (norte, sur, este, oeste, arriba, abajo)"
        , "  <dirección>                  - Moverse directamente (ej: norte, sur)"
        , "  mirar                        - Describir la sala actual"
        , "  tomar / coger <objeto>       - Recoger un objeto de la sala"
        , "  soltar / dejar <objeto>      - Dejar un objeto del inventario en la sala"
        , "  examinar <objeto>            - Ver la descripción de un objeto"
        , "  inventario / inv             - Listar objetos en el inventario"
        , "  combinar <obj1> con <obj2>   - Sintetizar un nuevo objeto (Variante 3)"
        , "  guardar <archivo>            - Guardar partida en un archivo"
        , "  cargar <archivo>             - Recuperar partida guardada"
        , "  ayuda                        - Ver esta lista de comandos"
        , "  salir                        - Finalizar el juego"
        ]
      , est
      )

    Salir ->
      (["¡Gracias por jugar! Hasta pronto."], est)

    Guardar _ ->
      (["Comando guardar en implementación (Paso 5)."], est)

    Cargar _ ->
      (["Comando cargar en implementación (Paso 5)."], est)

-- | Lógica pura de movimiento con desbloqueo permanente (Decisión D8).
ejecutarIr :: Direccion -> Estado -> ([String], Estado)
ejecutarIr dir est =
  case Map.lookup (estadoSalaActual est) (mundoSalas (estadoMundo est)) of
    Nothing ->
      (["Error: la sala actual no existe en el mundo."], est)

    Just salaActual ->
      case Map.lookup dir (salaSalidas salaActual) of
        Nothing ->
          (["No hay salida hacia el " ++ mostrarDireccion dir ++ "."], est)

        Just (Salida destino Nothing) ->
          let nuevoEstado = est { estadoSalaActual = destino }
              victoriaMsg = if haGanado nuevoEstado then ["*** ¡Has ganado! ***"] else []
              salaDestino = Map.lookup destino (mundoSalas (estadoMundo nuevoEstado))
              msgs = describirSala salaDestino ++ victoriaMsg
          in (msgs, nuevoEstado)

        Just (Salida destino (Just req)) ->
          if req `elem` estadoInventario est
            then
              let salidaDesbloqueada = Salida destino Nothing
                  salidasActualizadas = Map.insert dir salidaDesbloqueada (salaSalidas salaActual)
                  salaActualizada = salaActual { salaSalidas = salidasActualizadas }
                  mundoActualizado = (estadoMundo est)
                    { mundoSalas = Map.insert (salaId salaActual) salaActualizada (mundoSalas (estadoMundo est)) }
                  nuevoEstado = est
                    { estadoMundo      = mundoActualizado
                    , estadoSalaActual = destino
                    }
                  victoriaMsg = if haGanado nuevoEstado then ["*** ¡Has ganado! ***"] else []
                  salaDestino = Map.lookup destino (mundoSalas mundoActualizado)
                  msgs = [ "Usas " ++ unIdObjeto req ++ " para abrir la salida hacia el "
                           ++ mostrarDireccion dir ++ ". La puerta ha quedado desbloqueada."
                         , ""
                         ] ++ describirSala salaDestino ++ victoriaMsg
              in (msgs, nuevoEstado)
            else
              ( ["La salida hacia el " ++ mostrarDireccion dir
                 ++ " está bloqueada. Requiere: " ++ unIdObjeto req ++ "."]
              , est
              )

-- | Toma un objeto de la sala actual y lo transfiere al inventario.
ejecutarTomar :: IdObjeto -> Estado -> ([String], Estado)
ejecutarTomar obj est =
  case Map.lookup (estadoSalaActual est) (mundoSalas (estadoMundo est)) of
    Nothing -> (["Error: la sala actual no existe en el mundo."], est)
    Just salaActual ->
      if obj `elem` salaObjetos salaActual
        then
          let nuevaSala = salaActual { salaObjetos = delete obj (salaObjetos salaActual) }
              nuevoMundo = (estadoMundo est)
                { mundoSalas = Map.insert (salaId salaActual) nuevaSala (mundoSalas (estadoMundo est)) }
              nuevoEstado = est
                { estadoMundo      = nuevoMundo
                , estadoInventario = obj : estadoInventario est
                }
              victoriaMsg = if haGanado nuevoEstado then ["*** ¡Has ganado! ***"] else []
              msgs = ["Has tomado: " ++ unIdObjeto obj ++ "."] ++ victoriaMsg
          in (msgs, nuevoEstado)
        else
          (["No ves " ++ unIdObjeto obj ++ " aquí."], est)

-- | Suelta un objeto del inventario y lo deja en la sala actual.
ejecutarSoltar :: IdObjeto -> Estado -> ([String], Estado)
ejecutarSoltar obj est =
  if obj `elem` estadoInventario est
    then case Map.lookup (estadoSalaActual est) (mundoSalas (estadoMundo est)) of
      Nothing -> (["Error: la sala actual no existe en el mundo."], est)
      Just salaActual ->
        let nuevaSala = salaActual { salaObjetos = obj : salaObjetos salaActual }
            nuevoMundo = (estadoMundo est)
              { mundoSalas = Map.insert (salaId salaActual) nuevaSala (mundoSalas (estadoMundo est)) }
            nuevoEstado = est
              { estadoMundo      = nuevoMundo
              , estadoInventario = delete obj (estadoInventario est)
              }
        in (["Has soltado: " ++ unIdObjeto obj ++ "."], nuevoEstado)
    else
      (["No llevas " ++ unIdObjeto obj ++ " en tu inventario."], est)

-- | Muestra la descripción de un objeto si está en el inventario o en la sala actual.
ejecutarExaminar :: IdObjeto -> Estado -> ([String], Estado)
ejecutarExaminar obj est =
  let enInventario = obj `elem` estadoInventario est
      enSala = case Map.lookup (estadoSalaActual est) (mundoSalas (estadoMundo est)) of
        Just sala -> obj `elem` salaObjetos sala
        Nothing   -> False
  in if enInventario || enSala
       then case Map.lookup obj (mundoObjetos (estadoMundo est)) of
         Just info -> ([objetoDescripcion info], est)
         Nothing   -> (["No hay nada especial sobre " ++ unIdObjeto obj ++ "."], est)
       else
         (["No tienes ni ves " ++ unIdObjeto obj ++ " aquí."], est)

-- | Lista los objetos que el jugador porta actualmente.
ejecutarInventario :: Estado -> ([String], Estado)
ejecutarInventario est =
  let inv = estadoInventario est
      msg = if null inv
        then "Tu inventario está vacío."
        else "Llevas contigo: " ++ unwords (map unIdObjeto inv)
  in ([msg], est)

-- | Variante 3 (Combinar): Sintetiza dos objetos del inventario en un nuevo objeto.
-- Es simétrica/conmutativa: combinar a con b == combinar b con a (Decisión D5).
ejecutarCombinar :: IdObjeto -> IdObjeto -> Estado -> ([String], Estado)
ejecutarCombinar obj1 obj2 est =
  let inv = estadoInventario est
      tieneIngredientes =
        if obj1 == obj2
          then length (filter (== obj1) inv) >= 2
          else (obj1 `elem` inv) && (obj2 `elem` inv)
  in if not tieneIngredientes
       then (["No tienes ambos objetos en tu inventario para combinarlos."], est)
       else
         let recetas = mundoRecetas (estadoMundo est)
             coincide (Receta r1 r2 _) =
               (r1 == obj1 && r2 == obj2) || (r1 == obj2 && r2 == obj1)
         in case find coincide recetas of
           Nothing ->
             (["No parece que estos dos objetos se puedan combinar."], est)

           Just (Receta _ _ res) ->
             let invSinIngrediente1 = delete obj1 inv
                 invSinIngredientes = delete obj2 invSinIngrediente1
                 nuevoInv = res : invSinIngredientes
                 nuevoEstado = est { estadoInventario = nuevoInv }
                 victoriaMsg = if haGanado nuevoEstado then ["*** ¡Has ganado! ***"] else []
                 msgs = [ "Has combinado " ++ unIdObjeto obj1 ++ " con " ++ unIdObjeto obj2
                          ++ " para obtener: " ++ unIdObjeto res ++ "."
                        ] ++ victoriaMsg
             in (msgs, nuevoEstado)

-- | Formatea la descripción visual de una sala, sus objetos y sus salidas.
describirSala :: Maybe Sala -> [String]
describirSala Nothing = ["Estás en un lugar indefinido."]
describirSala (Just sala) =
  [ salaDescripcion sala
  , ""
  , if null (salaObjetos sala)
      then "No hay objetos aquí."
      else "Objetos: " ++ unwords (map unIdObjeto (salaObjetos sala))
  , if Map.null (salaSalidas sala)
      then "No hay salidas visibles."
      else "Salidas: " ++ unwords (map (mostrarDireccion . fst) (Map.toList (salaSalidas sala)))
  ]

-- | Traduce una 'Direccion' a su representación textual en español.
mostrarDireccion :: Direccion -> String
mostrarDireccion Norte  = "norte"
mostrarDireccion Sur    = "sur"
mostrarDireccion Este   = "este"
mostrarDireccion Oeste  = "oeste"
mostrarDireccion Arriba = "arriba"
mostrarDireccion Abajo  = "abajo"
