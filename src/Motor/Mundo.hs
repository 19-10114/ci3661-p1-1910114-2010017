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

import Control.Applicative (optional, some, (<|>))
import Data.Char (isAsciiUpper, isSpace)
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import qualified Motor.Reconocedor as R
import Motor.Tipos

type Linea = (Int, String)
type V = Validacion ErrorMundo

data Campo = Campo Int String String

data Cabecera = Cabecera String IdSala Meta [Receta]

-- Las salidas quedan en una lista para poder detectar direcciones repetidas.
data SalaCruda = SalaCruda IdSala String [(Direccion, Salida)] [IdObjeto]

data BloqueCrudo
  = BloqueObjeto Objeto
  | BloqueSala SalaCruda

data MundoCrudo = MundoCrudo Cabecera [BloqueCrudo]

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo texto =
  validacionAEither (construirMundo <$> parsearMundoCrudo (zip [1 ..] (lines texto)))

parsearMundoCrudo :: [Linea] -> V MundoCrudo
parsearMundoCrudo lineas =
  case separarBloques lineas of
    [] -> Fallo [ErrorSintaxis 1 "Falta la cabecera del mundo"]
    cabecera : bloques ->
      MundoCrudo <$> parsearCabecera cabecera <*> traverse parsearBloque bloques

separarBloques :: [Linea] -> [[Linea]]
separarBloques = filter (not . null) . foldr agregar [[]]
  where
    agregar linea@(_, texto) (actual : siguientes)
      | Just _ <- R.reconocerCompleto R.separadorBloque texto =
          [] : actual : siguientes
      | ignorar texto = actual : siguientes
      | otherwise = (linea : actual) : siguientes
    agregar _ [] = []

    ignorar texto =
      case dropWhile isSpace texto of
        [] -> True
        '#' : _ -> True
        _ -> False

parsearCabecera :: [Linea] -> V Cabecera
parsearCabecera = conCampos ["MUNDO", "INICIO", "META", "RECETA"] construir
  where
    construir linea campos =
      Cabecera
        <$> obligatorio linea "MUNDO" textoLibre campos
        <*> obligatorio linea "INICIO" (reconocer idSalaP) campos
        <*> obligatorio linea "META" (reconocer metaP) campos
        <*> repetible "RECETA" (reconocer recetaP) campos

parsearBloque :: [Linea] -> V BloqueCrudo
parsearBloque bloque@((linea, texto) : resto) =
  case parsearCampo (linea, texto) of
    Right (Campo _ "OBJETO" _) ->
      conCampos ["OBJETO", "DESC"] parsearObjeto bloque
    Right (Campo _ "SALA" _) ->
      conCampos ["SALA", "DESC", "SALIDA", "OBJETO"] parsearSala bloque
    Right (Campo _ clave _) ->
      Fallo (ErrorSintaxis linea ("El bloque debe comenzar con SALA u OBJETO, no con " ++ clave)
        : erroresDeFormato resto)
    Left err -> Fallo (err : erroresDeFormato resto)
  where
    erroresDeFormato = fst . partitionEithers . map parsearCampo
parsearBloque [] = Fallo [ErrorSintaxis 1 "Bloque vacío"]

parsearObjeto :: Int -> [Campo] -> V BloqueCrudo
parsearObjeto linea campos =
  BloqueObjeto <$> (Objeto
    <$> obligatorio linea "OBJETO" (reconocer idObjetoP) campos
    <*> obligatorio linea "DESC" textoLibre campos)

parsearSala :: Int -> [Campo] -> V BloqueCrudo
parsearSala linea campos =
  BloqueSala <$> (SalaCruda
    <$> obligatorio linea "SALA" (reconocer idSalaP) campos
    <*> obligatorio linea "DESC" textoLibre campos
    <*> repetible "SALIDA" (reconocer salidaP) campos
    <*> repetible "OBJETO" (reconocer idObjetoP) campos)

conCampos :: [String] -> (Int -> [Campo] -> V a) -> [Linea] -> V a
conCampos permitidas construir bloque =
  case bloque of
    [] -> Fallo [ErrorSintaxis 1 "Bloque vacío"]
    (linea, _) : _ ->
      let (erroresFormato, campos) = partitionEithers (map parsearCampo bloque)
          desconocidas =
            [ ErrorSintaxis n ("Clave desconocida " ++ clave)
            | Campo n clave _ <- campos
            , clave `notElem` permitidas
            ]
      in comprobar (erroresFormato ++ desconocidas) *> construir linea campos

parsearCampo :: Linea -> Either ErrorMundo Campo
parsearCampo (linea, texto) =
  case R.reconocerCompleto campoP texto of
    Just (clave, valor) -> Right (Campo linea clave valor)
    Nothing -> Left (ErrorSintaxis linea "Se esperaba una línea con formato CLAVE: valor")

campoP :: R.Parser (String, String)
campoP = do
  _ <- R.espacios
  clave <- some (R.satisfy isAsciiUpper)
  _ <- R.espacios
  _ <- R.char ':'
  _ <- R.espacios
  valor <- R.Parser (\resto -> [(dropWhileEnd isSpace resto, "")])
  pure (clave, valor)

obligatorio :: Int -> String -> (String -> Maybe a) -> [Campo] -> V a
obligatorio linea clave convertir campos =
  case camposConClave clave campos of
    [] -> Fallo [ErrorSintaxis linea ("Falta el campo obligatorio " ++ clave)]
    primero : repetidos ->
      comprobar [ErrorSintaxis n ("Campo repetido " ++ clave) | Campo n _ _ <- repetidos]
        *> validarCampo convertir primero
        <* traverse (validarCampo convertir) repetidos

repetible :: String -> (String -> Maybe a) -> [Campo] -> V [a]
repetible clave convertir =
  traverse (validarCampo convertir) . camposConClave clave

camposConClave :: String -> [Campo] -> [Campo]
camposConClave clave campos =
  [campo | campo@(Campo _ nombre _) <- campos, nombre == clave]

validarCampo :: (String -> Maybe a) -> Campo -> V a
validarCampo convertir (Campo linea clave texto) =
  case convertir texto of
    Just valor -> Exito valor
    Nothing -> Fallo [ErrorSintaxis linea ("Valor inválido para " ++ clave ++ ": " ++ texto)]

comprobar :: [ErrorMundo] -> V ()
comprobar [] = Exito ()
comprobar errores = Fallo errores

textoLibre :: String -> Maybe String
textoLibre = Just

reconocer :: R.Parser a -> String -> Maybe a
reconocer = R.reconocerCompleto

idSalaP :: R.Parser IdSala
idSalaP = IdSala <$> R.identificador

idObjetoP :: R.Parser IdObjeto
idObjetoP = IdObjeto <$> R.identificador

espacioObligatorio :: R.Parser String
espacioObligatorio = some (R.satisfy isSpace)

palabra :: String -> R.Parser String
palabra texto = R.string texto <* espacioObligatorio

simbolo :: String -> R.Parser String
simbolo texto = R.espacios *> R.string texto <* R.espacios

metaP :: R.Parser Meta
metaP =
  (LlegarA <$> (palabra "llegar" *> palabra "a" *> idSalaP))
  <|> (LlevarA <$> (palabra "llevar" *> idObjetoP <* espacioObligatorio)
               <*> (palabra "a" *> idSalaP))

salidaP :: R.Parser (Direccion, Salida)
salidaP = do
  direccion <- direccionP
  _ <- simbolo "->"
  destino <- idSalaP
  requisito <- optional (espacioObligatorio *> palabra "requiere" *> idObjetoP)
  pure (direccion, Salida destino requisito)

direccionP :: R.Parser Direccion
direccionP =
      Norte  <$ R.string "norte"
  <|> Sur    <$ R.string "sur"
  <|> Este   <$ R.string "este"
  <|> Oeste  <$ R.string "oeste"
  <|> Arriba <$ R.string "arriba"
  <|> Abajo  <$ R.string "abajo"

recetaP :: R.Parser Receta
recetaP = Receta
  <$> idObjetoP
  <*> (simbolo "+" *> idObjetoP)
  <*> (simbolo "->" *> idObjetoP)

construirMundo :: MundoCrudo -> Mundo
construirMundo (MundoCrudo (Cabecera titulo inicio meta recetas) bloques) =
  Mundo titulo inicio meta objetos salas recetas
  where
    objetos = Map.fromList
      [(objetoId objeto, objeto) | BloqueObjeto objeto <- bloques]
    salas = Map.fromList
      [ (ident, Sala ident descripcion (Map.fromList salidas) presentes)
      | BloqueSala (SalaCruda ident descripcion salidas presentes) <- bloques
      ]