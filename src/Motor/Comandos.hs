{-|
Module      : Motor.Comandos
Description : Intérprete y analizador de comandos del jugador.
Maintainer  : Stefano Casale (19-10114)
-}
module Motor.Comandos
  ( leerComando
  ) where

import Motor.Tipos

-- | Parsea una línea de texto introducida por el usuario y produce un 'Comando'.
leerComando :: String -> Maybe Comando
leerComando _ = Nothing
