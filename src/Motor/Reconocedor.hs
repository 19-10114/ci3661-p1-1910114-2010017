{-|
Module      : Motor.Reconocedor
Description : Combinadores monádicos de reconocimiento sobre ReadS.
Maintainer  : Nazareth Colmenarez (20-10017)

Este módulo corresponde al módulo de trabajo asignado a Nazareth Colmenarez
según Plan_Trabajo_Nazareth.pdf.
-}
module Motor.Reconocedor where

import Control.Applicative (Alternative (..))
import Data.Char (isAsciiLower, isDigit, isSpace)
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe)

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

instance Functor Parser where
  fmap transformar (Parser reconocer) = Parser $ \entrada ->
    [(transformar valor, resto) | (valor, resto) <- reconocer entrada]

instance Applicative Parser where
  pure valor = Parser $ \entrada -> [(valor, entrada)]

  Parser reconocerFuncion <*> Parser reconocerValor = Parser $ \entrada ->
    [ (funcion valor, restoFinal)
    | (funcion, resto) <- reconocerFuncion entrada
    , (valor, restoFinal) <- reconocerValor resto
    ]

instance Monad Parser where
  Parser reconocer >>= siguiente = Parser $ \entrada ->
    [ resultado
    | (valor, resto) <- reconocer entrada
    , resultado <- runParser (siguiente valor) resto
    ]

instance Alternative Parser where
  empty = Parser $ const []

  Parser primero <|> Parser segundo = Parser $ \entrada ->
    take 1 (primero entrada ++ segundo entrada)

identificador :: Parser String
identificador = some (satisfy caracterDeIdentificador)
  where
    caracterDeIdentificador c =
      isAsciiLower c || isDigit c || c == '_' || c `elem` "áéíóúñü"

reconocerCompleto :: Parser a -> String -> Maybe a
reconocerCompleto parser entrada =
  listToMaybe [valor | (valor, "") <- runParser parser entrada]

separadorBloque :: Parser String
separadorBloque = espacios *> string "---" <* espacios