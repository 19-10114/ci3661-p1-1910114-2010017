module Main where

import Motor.Comandos
import Motor.Tipos
import Test.HUnit

-- | Casos de prueba para 'leerComando'
testsComandos :: Test
testsComandos = TestList
  [ -- Movimiento canónico con "ir"
    TestCase (assertEqual "ir norte" (Just (Ir Norte)) (leerComando "ir norte"))
  , TestCase (assertEqual "ir sur" (Just (Ir Sur)) (leerComando "ir sur"))
  , TestCase (assertEqual "ir este" (Just (Ir Este)) (leerComando "ir este"))
  , TestCase (assertEqual "ir oeste" (Just (Ir Oeste)) (leerComando "ir oeste"))
  , TestCase (assertEqual "ir arriba" (Just (Ir Arriba)) (leerComando "ir arriba"))
  , TestCase (assertEqual "ir abajo" (Just (Ir Abajo)) (leerComando "ir abajo"))

    -- Movimiento directo (sinónimo sin "ir", crítico para guiones/entrega.txt)
  , TestCase (assertEqual "norte directo" (Just (Ir Norte)) (leerComando "norte"))
  , TestCase (assertEqual "oeste directo" (Just (Ir Oeste)) (leerComando "oeste"))
  , TestCase (assertEqual "arriba directo" (Just (Ir Arriba)) (leerComando "arriba"))
  , TestCase (assertEqual "abreviatura n" (Just (Ir Norte)) (leerComando "n"))
  , TestCase (assertEqual "abreviatura o" (Just (Ir Oeste)) (leerComando "o"))

    -- Mirar y examinar
  , TestCase (assertEqual "mirar entorno" (Just Mirar) (leerComando "mirar"))
  , TestCase (assertEqual "mirar con espacios y mayúsculas" (Just Mirar) (leerComando "  MIRAR  "))
  , TestCase (assertEqual "examinar objeto" (Just (Examinar (IdObjeto "carnet"))) (leerComando "examinar carnet"))
  , TestCase (assertEqual "abreviatura x" (Just (Examinar (IdObjeto "llave"))) (leerComando "x llave"))
  , TestCase (assertEqual "mirar objeto específico" (Just (Examinar (IdObjeto "moneda"))) (leerComando "mirar moneda"))

    -- Tomar y soltar con sinónimos y artículos
  , TestCase (assertEqual "tomar carnet" (Just (Tomar (IdObjeto "carnet"))) (leerComando "tomar carnet"))
  , TestCase (assertEqual "coger sinónimo" (Just (Tomar (IdObjeto "carnet"))) (leerComando "coger carnet"))
  , TestCase (assertEqual "tomar con artículo" (Just (Tomar (IdObjeto "carnet"))) (leerComando "tomar el carnet"))
  , TestCase (assertEqual "tomar con espacios unidos" (Just (Tomar (IdObjeto "memoria_usb"))) (leerComando "tomar la memoria usb"))
  , TestCase (assertEqual "soltar objeto" (Just (Soltar (IdObjeto "carnet"))) (leerComando "soltar carnet"))
  , TestCase (assertEqual "dejar sinónimo" (Just (Soltar (IdObjeto "moneda"))) (leerComando "dejar la moneda"))

    -- Inventario
  , TestCase (assertEqual "inventario completo" (Just Inventario) (leerComando "inventario"))
  , TestCase (assertEqual "inv sinónimo" (Just Inventario) (leerComando "inv"))
  , TestCase (assertEqual "i abreviatura" (Just Inventario) (leerComando "i"))

    -- Persistencia
  , TestCase (assertEqual "guardar archivo" (Just (Guardar "partida.sav")) (leerComando "guardar partida.sav"))
  , TestCase (assertEqual "cargar archivo" (Just (Cargar "partida.sav")) (leerComando "cargar partida.sav"))

    -- Variante 3: Combinar
  , TestCase (assertEqual "combinar estándar"
              (Just (Combinar (IdObjeto "llave") (IdObjeto "moneda")))
              (leerComando "combinar llave con moneda"))
  , TestCase (assertEqual "combinar con artículos y espacios"
              (Just (Combinar (IdObjeto "memoria_usb") (IdObjeto "carnet")))
              (leerComando "combinar la memoria usb con el carnet"))

    -- Ayuda y salir
  , TestCase (assertEqual "ayuda" (Just Ayuda) (leerComando "ayuda"))
  , TestCase (assertEqual "salir" (Just Salir) (leerComando "salir"))
  , TestCase (assertEqual "q abreviatura salir" (Just Salir) (leerComando "q"))

    -- Comandos inválidos
  , TestCase (assertEqual "cadena vacía" Nothing (leerComando ""))
  , TestCase (assertEqual "solo espacios" Nothing (leerComando "     "))
  , TestCase (assertEqual "dirección inexistente" Nothing (leerComando "noreste"))
  , TestCase (assertEqual "ir sin dirección" Nothing (leerComando "ir"))
  , TestCase (assertEqual "tomar sin argumento" Nothing (leerComando "tomar"))
  , TestCase (assertEqual "soltar sin argumento" Nothing (leerComando "soltar"))
  , TestCase (assertEqual "guardar sin archivo" Nothing (leerComando "guardar"))
  , TestCase (assertEqual "cargar sin archivo" Nothing (leerComando "cargar"))
  , TestCase (assertEqual "combinar incompleto" Nothing (leerComando "combinar carnet"))
  , TestCase (assertEqual "palabra desconocida" Nothing (leerComando "saltar"))
  ]

main :: IO ()
main = do
  resultado <- runTestTT testsComandos
  if errors resultado + failures resultado == 0
    then return ()
    else error "Pruebas unitarias fallaron"
