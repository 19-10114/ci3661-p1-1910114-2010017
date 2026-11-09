{-|
Module      : Motor.Comandos
Description : Intérprete y analizador de comandos del jugador.
Maintainer  : Stefano Casale (19-10114)

Este módulo implementa la función pura 'leerComando' que traduce texto
ingresado por el usuario a valores del tipo 'Comando'.
Soporta direcciones directas ('norte', 'sur', etc.), abreviaturas,
sinónimos del enunciado ('coger', 'inv'), la Variante 3 ('combinar'),
insensibilidad a mayúsculas y filtrado de artículos cosméticos.
-}
module Motor.Comandos
  ( leerComando
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intercalate)
import Motor.Tipos

-- | Parsea una línea de texto introducida por el usuario y produce un 'Comando'.
-- Es pura, insensible a mayúsculas/minúsculas y tolera espacios redundantes.
-- Si la entrada no corresponde a ningún comando válido, devuelve 'Nothing'.
leerComando :: String -> Maybe Comando
leerComando linea =
  case words (map toLower (limpiarEspacios linea)) of
    [] -> Nothing

    -- Direcciones directas (indispensables para guiones oficiales como entrega.txt)
    [dir] | Just d <- parsearDireccion dir -> Just (Ir d)

    -- Movimiento canónico con "ir"
    ["ir", dir] | Just d <- parsearDireccion dir -> Just (Ir d)

    -- Mirar entorno general
    ["mirar"] -> Just Mirar

    -- Mirar objeto específico (sinónimo común de examinar)
    ("mirar" : resto) | Just obj <- normalizarIdObjeto resto -> Just (Examinar obj)

    -- Tomar / Coger
    (verbo : resto) | verbo `elem` ["tomar", "coger", "agarrar"]
                    , Just obj <- normalizarIdObjeto resto -> Just (Tomar obj)

    -- Soltar / Dejar
    (verbo : resto) | verbo `elem` ["soltar", "dejar"]
                    , Just obj <- normalizarIdObjeto resto -> Just (Soltar obj)

    -- Examinar
    (verbo : resto) | verbo `elem` ["examinar", "x", "inspeccionar"]
                    , Just obj <- normalizarIdObjeto resto -> Just (Examinar obj)

    -- Inventario
    [inv] | inv `elem` ["inventario", "inv", "i"] -> Just Inventario

    -- Guardar partida
    ("guardar" : resto) | not (null resto) -> Just (Guardar (unwords resto))

    -- Cargar partida
    ("cargar" : resto) | not (null resto) -> Just (Cargar (unwords resto))

    -- Variante 3: Combinar objetos
    ("combinar" : resto) -> parsearCombinar resto

    -- Ayuda
    [h] | h `elem` ["ayuda", "help", "?"] -> Just Ayuda

    -- Salir
    [s] | s `elem` ["salir", "quit", "exit", "q"] -> Just Salir

    _ -> Nothing

-- | Reconoce una dirección cardinal o vertical (nombre completo o abreviatura).
parsearDireccion :: String -> Maybe Direccion
parsearDireccion s = case s of
  "norte"  -> Just Norte
  "sur"    -> Just Sur
  "este"   -> Just Este
  "oeste"  -> Just Oeste
  "arriba" -> Just Arriba
  "abajo"  -> Just Abajo
  "n"      -> Just Norte
  "s"      -> Just Sur
  "e"      -> Just Este
  "o"      -> Just Oeste
  _        -> Nothing

-- | Parsea la sintaxis de combinación: "combinar <obj1> con <obj2>" o con "y".
parsearCombinar :: [String] -> Maybe Comando
parsearCombinar args =
  case break (`elem` ["con", "y"]) args of
    (antes, _:despues) | not (null antes) && not (null despues) -> do
      obj1 <- normalizarIdObjeto antes
      obj2 <- normalizarIdObjeto despues
      return (Combinar obj1 obj2)
    _ -> Nothing

-- | Normaliza tokens de objeto eliminando artículos determinados y uniendo palabras con '_'.
normalizarIdObjeto :: [String] -> Maybe IdObjeto
normalizarIdObjeto ws =
  let sinArticulos = case ws of
        (art:resto) | art `elem` ["el", "la", "los", "las", "un", "una"] && not (null resto) -> resto
        _ -> ws
  in if null sinArticulos
     then Nothing
     else Just (IdObjeto (intercalate "_" sinArticulos))

-- | Elimina espacios en blanco al inicio y al final de una cadena.
limpiarEspacios :: String -> String
limpiarEspacios = dropWhile isSpace . dropWhileEnd isSpace
