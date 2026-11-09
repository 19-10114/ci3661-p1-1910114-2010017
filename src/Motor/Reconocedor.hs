{-|
Module      : Motor.Reconocedor
Description : Combinadores monádicos de reconocimiento sobre ReadS.
Maintainer  : Nazareth Colmenarez (20-10017)

Este módulo corresponde al módulo de trabajo asignado a Nazareth Colmenarez
según Plan_Trabajo_Nazareth.pdf.
-}
module Motor.Reconocedor where

import Data.Char (isSpace)
import Data.List (stripPrefix)

newtype Parser a = Parser { runParser :: ReadS a }

satisfy :: (Char -> Bool) -> Parser Char
satisfy condicion = Parser reconocer
  where
    reconocer (c : resto)
      | condicion c = [(c, resto)]
    reconocer _ = []

char :: Char -> Parser Char
char esperado = satisfy (== esperado)

string :: String -> Parser String
string prefijo = Parser $ \entrada ->
  case stripPrefix prefijo entrada of
    Just resto -> [(prefijo, resto)]
    Nothing -> []

espacios :: Parser String
espacios = Parser $ \entrada ->
  let (blancos, resto) = span isSpace entrada
  in [(blancos, resto)]