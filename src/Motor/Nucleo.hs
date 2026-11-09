{-|
Module      : Motor.Nucleo
Description : Lógica funcional pura de ejecución de aventuras.
Maintainer  : Stefano Casale (19-10114)
-}
module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  ) where

import Motor.Tipos

-- | Genera el estado inicial a partir de un mundo validado.
estadoInicial :: Mundo -> Estado
estadoInicial m = Estado
  { estadoMundo      = m
  , estadoSalaActual = mundoInicio m
  , estadoInventario = []
  }

-- | Ejecuta un comando sobre un estado puramente, devolviendo los mensajes y el nuevo estado.
ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar _ est = ([], est)
