{-|
Module      : Motor.Nucleo
Description : Lógica funcional pura de ejecución de aventuras.
Maintainer  : Stefano Casale (19-10114)

Este módulo implementa la lógica puramente funcional del motor de aventuras:
- Creación del estado inicial ('estadoInicial').
- Ejecución de comandos del jugador ('ejecutar').
- Desplazamiento entre salas con verificación de salidas bloqueadas.
- Desbloqueo permanente de salidas tras el primer cruce con llave (Decisión D8).
- Verificación pura de la condición de victoria ('haGanado').
-}
module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  , haGanado
  , describirSala
  , mostrarDireccion
  ) where

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

    -- Los comandos de inventario y objetos se completan en el Paso 4
    Tomar _ ->
      (["Comando tomar en implementación (Paso 4)."], est)

    Soltar _ ->
      (["Comando soltar en implementación (Paso 4)."], est)

    Examinar _ ->
      (["Comando examinar en implementación (Paso 4)."], est)

    Inventario ->
      ( if null (estadoInventario est)
          then ["Tu inventario está vacío."]
          else ["Llevas contigo: " ++ unwords (map unIdObjeto (estadoInventario est))]
      , est
      )

    Guardar _ ->
      (["Comando guardar en implementación (Paso 5)."], est)

    Cargar _ ->
      (["Comando cargar en implementación (Paso 5)."], est)

    Combinar _ _ ->
      (["Comando combinar en implementación (Paso 4)."], est)

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
          -- Salida sin bloqueo: movimiento directo
          let nuevoEstado = est { estadoSalaActual = destino }
              victoriaMsg = if haGanado nuevoEstado then ["*** ¡Has ganado! ***"] else []
              salaDestino = Map.lookup destino (mundoSalas (estadoMundo nuevoEstado))
              msgs = describirSala salaDestino ++ victoriaMsg
          in (msgs, nuevoEstado)

        Just (Salida destino (Just req)) ->
          -- Salida bloqueada: requiere tener el objeto en el inventario
          if req `elem` estadoInventario est
            then
              -- Tiene el objeto: cruza y la salida queda permanentemente desbloqueada
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
              -- No tiene el objeto: no se mueve
              ( ["La salida hacia el " ++ mostrarDireccion dir
                 ++ " está bloqueada. Requiere: " ++ unIdObjeto req ++ "."]
              , est
              )

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
