module Main where

import qualified Data.Map.Strict as Map
import Motor.Comandos
import Motor.Nucleo
import Motor.Tipos
import Test.HUnit

-- ============================================================================
-- MUNDO DE PRUEBA EN MEMORIA PARA TESTS UNITARIOS DEL NÚCLEO
-- ============================================================================

mockMundo :: Mundo
mockMundo = Mundo
  { mundoTitulo   = "Mundo de Prueba"
  , mundoInicio   = IdSala "vestibulo"
  , mundoMeta     = LlegarA (IdSala "tesoro")
  , mundoObjetos  = Map.fromList [(IdObjeto "llave_dorada", Objeto (IdObjeto "llave_dorada") "Una llave reluciente.")]
  , mundoSalas    = Map.fromList
      [ ( IdSala "vestibulo"
        , Sala
            { salaId          = IdSala "vestibulo"
            , salaDescripcion = "Un vestíbulo amplio con suelo de mármol."
            , salaSalidas     = Map.fromList
                [ (Norte, Salida (IdSala "pasillo") Nothing)
                , (Este,  Salida (IdSala "tesoro") (Just (IdObjeto "llave_dorada")))
                ]
            , salaObjetos     = [IdObjeto "llave_dorada"]
            }
        )
      , ( IdSala "pasillo"
        , Sala
            { salaId          = IdSala "pasillo"
            , salaDescripcion = "Un pasillo estrecho y oscuro."
            , salaSalidas     = Map.fromList [(Sur, Salida (IdSala "vestibulo") Nothing)]
            , salaObjetos     = []
            }
        )
      , ( IdSala "tesoro"
        , Sala
            { salaId          = IdSala "tesoro"
            , salaDescripcion = "La cámara del tesoro secreta."
            , salaSalidas     = Map.fromList [(Oeste, Salida (IdSala "vestibulo") Nothing)]
            , salaObjetos     = []
            }
        )
      ]
  , mundoRecetas  = []
  }

-- ============================================================================
-- TESTS DE MOTOR.NUCLEO (MOVIMIENTO, BLOQUEOS, PERMANENCIA Y VICTORIA)
-- ============================================================================

testsNucleo :: Test
testsNucleo = TestList
  [ -- 1. Estado inicial
    TestCase $ do
      let est = estadoInicial mockMundo
      assertEqual "Inicio en sala vestibulo" (IdSala "vestibulo") (estadoSalaActual est)
      assertEqual "Inventario inicial vacío" [] (estadoInventario est)
      assertEqual "Aún no ha ganado" False (haGanado est)

    -- 2. Movimiento libre sin obstáculos
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (_, est1) = ejecutar (Ir Norte) est0
      assertEqual "Moverse al norte llega al pasillo" (IdSala "pasillo") (estadoSalaActual est1)

    -- 3. Movimiento hacia dirección sin salida
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, est1) = ejecutar (Ir Oeste) est0
      assertEqual "No se mueve si no hay salida" (IdSala "vestibulo") (estadoSalaActual est1)
      assertEqual "Mensaje indica ausencia de salida"
                  ["No hay salida hacia el oeste."] msgs

    -- 4. Movimiento hacia salida bloqueada SIN el objeto requerido
  , TestCase $ do
      let est0 = estadoInicial mockMundo -- inventario vacío
          (msgs, est1) = ejecutar (Ir Este) est0
      assertEqual "No cruza la puerta cerrada sin llave" (IdSala "vestibulo") (estadoSalaActual est1)
      assertEqual "Mensaje advierte sobre bloqueo y requerimiento"
                  ["La salida hacia el este está bloqueada. Requiere: llave_dorada."] msgs

    -- 5. Movimiento hacia salida bloqueada CON el objeto requerido
  , TestCase $ do
      let est0 = (estadoInicial mockMundo) { estadoInventario = [IdObjeto "llave_dorada"] }
          (_, est1) = ejecutar (Ir Este) est0
      assertEqual "Cruza con la llave a la cámara del tesoro" (IdSala "tesoro") (estadoSalaActual est1)
      assertEqual "Alcanzó la meta de LlegarA y gana" True (haGanado est1)

    -- 6. Desbloqueo permanente (Decisión D8)
  , TestCase $ do
      -- Paso a: cruza hacia el este con la llave
      let estConLlave = (estadoInicial mockMundo) { estadoInventario = [IdObjeto "llave_dorada"] }
          (_, estEnTesoro) = ejecutar (Ir Este) estConLlave
      -- Paso b: regresa al vestíbulo (salida oeste libre)
      let (_, estDeVuelta) = ejecutar (Ir Oeste) estEnTesoro
      -- Paso c: simulamos que pierde la llave (inventario vacío)
      let estSinLlave = estDeVuelta { estadoInventario = [] }
      -- Paso d: intenta cruzar nuevamente hacia el este SIN la llave
      let (_, estReingreso) = ejecutar (Ir Este) estSinLlave
      assertEqual "La salida este quedó permanentemente desbloqueada tras el primer cruce"
                  (IdSala "tesoro") (estadoSalaActual estReingreso)

    -- 7. Mirar describe la sala
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, _) = ejecutar Mirar est0
      assertEqual "Mirar produce al menos 4 líneas descriptivas" True (length msgs >= 4)

    -- 8. Condición de victoria con LlevarA
  , TestCase $ do
      let mundoMetaLlevar = mockMundo { mundoMeta = LlevarA (IdObjeto "llave_dorada") (IdSala "pasillo") }
          estSinObj = (estadoInicial mundoMetaLlevar) { estadoSalaActual = IdSala "pasillo", estadoInventario = [] }
          estConObj = (estadoInicial mundoMetaLlevar) { estadoSalaActual = IdSala "pasillo", estadoInventario = [IdObjeto "llave_dorada"] }
      assertEqual "No gana en pasillo sin el objeto requerido" False (haGanado estSinObj)
      assertEqual "Gana en pasillo portando el objeto requerido" True (haGanado estConObj)
  ]

-- ============================================================================
-- TESTS DE MOTOR.COMANDOS (ANÁLISIS DE ENTRADA, SINÓNIMOS, VARIANTES)
-- ============================================================================

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

-- ============================================================================
-- EJECUTOR DE SUITE COMPLETA
-- ============================================================================

main :: IO ()
main = do
  putStrLn "--- Ejecutando Pruebas de Motor.Comandos ---"
  resComandos <- runTestTT testsComandos
  putStrLn "\n--- Ejecutando Pruebas de Motor.Nucleo (Movimiento y Bloqueos) ---"
  resNucleo <- runTestTT testsNucleo
  let totalFallos = errors resComandos + failures resComandos + errors resNucleo + failures resNucleo
  if totalFallos == 0
    then putStrLn "\n[OK] Todas las pruebas unitarias pasaron exitosamente."
    else error "\n[FALLO] Al menos una prueba unitaria falló."
