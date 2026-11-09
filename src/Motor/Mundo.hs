{-|
Module      : Motor.Mundo
Description : Parser de archivos de mundo y validación exhaustiva V1–V10.
Maintainer  : Nazareth Colmenarez (20-10017)

Este módulo corresponde al módulo de trabajo asignado a Nazareth Colmenarez
según Plan_Trabajo_Nazareth.pdf.
-}
module Motor.Mundo
  ( leerMundo
  ) where

import Motor.Tipos

-- | Función obligatoria de la especificación:
-- Recibe el texto del archivo; reconoce la sintaxis y valida semánticamente.
-- Es pura.
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo _ = Left [ErrorSintaxis 0 "Módulo asignado a Nazareth Colmenarez"]
