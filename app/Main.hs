{-|
Module      : Main
Description : Punto de entrada del ejecutable aventura: CLI, validación y bucle interactivo.
Maintainer  : Stefano Casale (19-10114)

Este módulo implementa la interfaz de línea de comandos (CLI) del motor de aventuras:
- Procesamiento de argumentos: 'aventura MUNDO' y 'aventura -validar MUNDO'.
- Modo de validación pura con salida informativa y códigos de retorno oficiales.
- Modo interactivo con soporte de entrada por tubería/redirección (< guion.txt).
- Manejo seguro de fin de entrada (isEOF) y serialización/persistencia de partidas.
- Emisión garantizada del mensaje de victoria «*** ¡Has ganado! ***» con código de salida 0.
-}
module Main (main, bucleJuego) where

import qualified Data.Map.Strict as Map
import Motor.Comandos (leerComando)
import Motor.Nucleo (describirSala, ejecutar, estadoInicial, haGanado)
import Motor.Persistencia (cargarPartida, guardarPartida, leerArchivoMundo)
import Motor.Tipos
  ( Comando (Cargar, Guardar, Salir)
  , Estado (estadoMundo, estadoSalaActual)
  , Mundo (mundoSalas, mundoTitulo)
  , mostrarErrorMundo
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO
  ( BufferMode (NoBuffering)
  , hFlush
  , hPutStrLn
  , hSetBuffering
  , hSetEncoding
  , isEOF
  , stderr
  , stdin
  , stdout
  , utf8
  )

-- | Punto de entrada principal del ejecutable.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-validar", ruta] -> modoValidar ruta
    [ruta, "-validar"] -> modoValidar ruta
    [ruta]             -> modoJuego ruta
    _                  -> do
      hPutStrLn stderr "Uso: aventura [-validar] ARCHIVO_MUNDO"
      exitWith (ExitFailure 1)

-- | Ejecuta la verificación estática del archivo de mundo sin iniciar la partida.
-- Si es válido, imprime «Mundo válido.» y termina con código 0.
-- Si contiene errores, imprime cada uno con el prefijo «Error: » y termina con código 1.
modoValidar :: FilePath -> IO ()
modoValidar ruta = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  resultado <- leerArchivoMundo ruta
  case resultado of
    Left errores -> do
      mapM_ (putStrLn . mostrarErrorMundo) errores
      exitWith (ExitFailure 1)
    Right _ -> do
      putStrLn "Mundo válido."
      exitSuccess

-- | Inicia una sesión interactiva de juego en el mundo especificado.
modoJuego :: FilePath -> IO ()
modoJuego ruta = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout NoBuffering

  resultado <- leerArchivoMundo ruta
  case resultado of
    Left errores -> do
      mapM_ (putStrLn . mostrarErrorMundo) errores
      exitWith (ExitFailure 1)

    Right mundo -> do
      -- 1. Mostrar título del mundo
      putStrLn (mundoTitulo mundo)
      putStrLn ""

      -- 2. Mostrar descripción de la sala inicial
      let estIni = estadoInicial mundo
          salaIni = Map.lookup (estadoSalaActual estIni) (mundoSalas mundo)
      mapM_ putStrLn (describirSala salaIni)

      -- 3. Si la sala inicial ya cumple la condición de victoria (caso extremo), terminar.
      if haGanado estIni
        then do
          putStrLn "*** ¡Has ganado! ***"
          exitSuccess
        else bucleJuego estIni

-- | Bucle principal del juego: muestra el indicador (> ), lee un comando,
-- lo interpreta, ejecuta la transición pura y maneja la terminación limpia.
bucleJuego :: Estado -> IO ()
bucleJuego est = do
  finEntrada <- isEOF
  if finEntrada
    then exitSuccess
    else do
      putStr "> "
      hFlush stdout
      linea <- getLine
      if null (words linea)
        then bucleJuego est
        else case leerComando linea of
          Nothing -> do
            putStrLn "No entiendo qué quieres decir. Escribe 'ayuda' para ver los comandos."
            bucleJuego est

          Just Salir -> do
            putStrLn "¡Gracias por jugar! Hasta pronto."
            exitSuccess

          Just (Guardar ruta) -> do
            guardarPartida ruta est
            putStrLn ("Partida guardada en «" ++ ruta ++ "».")
            bucleJuego est

          Just (Cargar ruta) -> do
            resCarga <- cargarPartida ruta est
            case resCarga of
              Left err -> do
                putStrLn err
                bucleJuego est
              Right nuevoEst -> do
                putStrLn ("Partida cargada exitosamente desde «" ++ ruta ++ "».")
                putStrLn ""
                let salaActual = Map.lookup (estadoSalaActual nuevoEst) (mundoSalas (estadoMundo nuevoEst))
                mapM_ putStrLn (describirSala salaActual)
                if haGanado nuevoEst
                  then do
                    putStrLn "*** ¡Has ganado! ***"
                    exitSuccess
                  else bucleJuego nuevoEst

          Just cmd -> do
            let (msgs, nuevoEst) = ejecutar cmd est
            mapM_ putStrLn msgs
            if haGanado nuevoEst
              then do
                let msgVictoria = "*** ¡Has ganado! ***"
                if msgVictoria `elem` msgs
                  then return ()
                  else putStrLn msgVictoria
                exitSuccess
              else bucleJuego nuevoEst
