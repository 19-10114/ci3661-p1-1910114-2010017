{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Motor.Persistencia
Description : Entrada y salida pura de persistencia de partidas y lectura de mundos.
Maintainer  : Stefano Casale (19-10114)

Este módulo aísla todas las operaciones con el sistema de archivos del juego:
- Guardado del estado en archivos serializados con 'Show'.
- Carga segura de partidas con 'readMaybe' y verificación de compatibilidad de mundo (Decisión D7).
- Lectura de archivos de descripción de mundo garantizando codificación UTF-8.
-}
module Motor.Persistencia
  ( guardarPartida
  , cargarPartida
  , leerArchivoMundo
  , leerTextoUtf8
  , escribirTextoUtf8
  ) where

import Control.Exception (IOException, catch)
import Motor.Mundo (leerMundo)
import Motor.Tipos
import System.Directory (doesFileExist)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStr, hSetEncoding, utf8, withFile)
import Text.Read (readMaybe)

-- | Escribe texto garantizando codificación UTF-8 en cualquier sistema operativo.
escribirTextoUtf8 :: FilePath -> String -> IO ()
escribirTextoUtf8 ruta contenido =
  withFile ruta WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h contenido

-- | Lee texto completo garantizando codificación UTF-8 sin problemas de bloqueo.
leerTextoUtf8 :: FilePath -> IO String
leerTextoUtf8 ruta =
  withFile ruta ReadMode $ \h -> do
    hSetEncoding h utf8
    texto <- hGetContents h
    length texto `seq` return texto

-- | Guarda el estado actual del juego en un archivo de partida.
-- Emplea 'PartidaGuardada' para asociar el título del mundo al estado (Decisión D7).
guardarPartida :: FilePath -> Estado -> IO ()
guardarPartida ruta est = do
  let partida = PartidaGuardada
        { partidaTituloMundo = mundoTitulo (estadoMundo est)
        , partidaEstado      = est
        }
  escribirTextoUtf8 ruta (show partida)

-- | Carga una partida guardada desde disco de forma totalmente segura.
-- No lanza excepciones bajo ninguna circunstancia:
-- 1. Si el archivo no existe -> Left informativo.
-- 2. Si el archivo está corrupto o mal formado -> Left informativo.
-- 3. Si el archivo corresponde a otro mundo -> Left indicando la discrepancia.
-- 4. Si todo es correcto -> Right con el nuevo Estado recuperado.
cargarPartida :: FilePath -> Estado -> IO (Either String Estado)
cargarPartida ruta estActual = do
  existe <- doesFileExist ruta
  if not existe
    then return (Left ("El archivo de guardado «" ++ ruta ++ "» no existe."))
    else do
      resultadoLectura <- (Right <$> leerTextoUtf8 ruta) `catch` \(e :: IOException) ->
        return (Left ("Error al leer el archivo «" ++ ruta ++ "»: " ++ show e))
      case resultadoLectura of
        Left err -> return (Left err)
        Right contenido ->
          case readMaybe contenido of
            Nothing ->
              return (Left ("El archivo «" ++ ruta ++ "» está corrupto o no tiene un formato válido de partida guardada."))
            Just (PartidaGuardada tituloGuardado estadoCargado) ->
              let tituloActual = mundoTitulo (estadoMundo estActual)
              in if tituloGuardado /= tituloActual
                then return (Left ("La partida guardada pertenece al mundo «" ++ tituloGuardado
                                  ++ "», pero la partida actual es del mundo «" ++ tituloActual ++ "»."))
                else return (Right estadoCargado)

-- | Lee y valida un archivo de mundo desde disco, aislando la entrada/salida.
leerArchivoMundo :: FilePath -> IO (Either [ErrorMundo] Mundo)
leerArchivoMundo ruta = do
  existe <- doesFileExist ruta
  if not existe
    then return (Left [ErrorSintaxis 0 ("No se pudo abrir el archivo «" ++ ruta ++ "»: no existe.")])
    else do
      resultadoLectura <- (Right <$> leerTextoUtf8 ruta) `catch` \(e :: IOException) ->
        return (Left [ErrorSintaxis 0 ("Error al leer «" ++ ruta ++ "»: " ++ show e)])
      case resultadoLectura of
        Left errs -> return (Left errs)
        Right contenido -> return (leerMundo contenido)
