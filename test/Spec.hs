module Main where

import qualified Data.Map.Strict as Map
import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (ejecutar, estadoInicial, haGanado)
import Motor.Reconocedor (identificador, reconocerCompleto, runParser, separadorBloque)
import Motor.Tipos
import Test.HUnit

testsComandos :: Test
testsComandos = TestList
  [ TestLabel entrada $ TestCase $
      assertEqual entrada esperado (leerComando entrada)
  | (entrada, esperado) <- casos
  ]
  where
    casos =
      [ ("ir norte", Just (Ir Norte))
      , ("ir sur", Just (Ir Sur))
      , ("ir este", Just (Ir Este))
      , ("ir oeste", Just (Ir Oeste))
      , ("ir arriba", Just (Ir Arriba))
      , ("ir abajo", Just (Ir Abajo))
      , ("norte", Just (Ir Norte))
      , ("oeste", Just (Ir Oeste))
      , ("arriba", Just (Ir Arriba))
      , ("n", Just (Ir Norte))
      , ("o", Just (Ir Oeste))
      , ("mirar", Just Mirar)
      , ("  MIRAR  ", Just Mirar)
      , ("examinar carnet", Just (Examinar (IdObjeto "carnet")))
      , ("x llave", Just (Examinar (IdObjeto "llave")))
      , ("mirar moneda", Just (Examinar (IdObjeto "moneda")))
      , ("tomar carnet", Just (Tomar (IdObjeto "carnet")))
      , ("coger carnet", Just (Tomar (IdObjeto "carnet")))
      , ("tomar el carnet", Just (Tomar (IdObjeto "carnet")))
      , ("tomar la memoria usb", Just (Tomar (IdObjeto "memoria_usb")))
      , ("soltar carnet", Just (Soltar (IdObjeto "carnet")))
      , ("dejar la moneda", Just (Soltar (IdObjeto "moneda")))
      , ("inventario", Just Inventario)
      , ("inv", Just Inventario)
      , ("i", Just Inventario)
      , ("guardar partida.sav", Just (Guardar "partida.sav"))
      , ("cargar partida.sav", Just (Cargar "partida.sav"))
      , ("combinar llave con moneda", Just (Combinar (IdObjeto "llave") (IdObjeto "moneda")))
      , ("combinar la memoria usb con el carnet", Just (Combinar (IdObjeto "memoria_usb") (IdObjeto "carnet")))
      , ("ayuda", Just Ayuda)
      , ("salir", Just Salir)
      , ("q", Just Salir)
      , ("", Nothing)
      , ("     ", Nothing)
      , ("noreste", Nothing)
      , ("ir", Nothing)
      , ("tomar", Nothing)
      , ("soltar", Nothing)
      , ("guardar", Nothing)
      , ("cargar", Nothing)
      , ("combinar carnet", Nothing)
      , ("saltar", Nothing)
      ]

testsReconocedor :: Test
testsReconocedor = TestList
  [ TestCase $
      assertEqual "el identificador deja el resto sin consumir"
        [("sala_2", " -> patio")]
        (runParser identificador "sala_2 -> patio")
  , TestCase $
      assertEqual "identificador con letras españolas"
        (Just "cañón_2")
        (reconocerCompleto identificador "cañón_2")
  , TestCase $
      assertEqual "identificador con guion inválido"
        Nothing
        (reconocerCompleto identificador "sala-2")
  , TestCase $
      assertEqual "separador con espacios"
        (Just "---")
        (reconocerCompleto separadorBloque "  ---  ")
  , TestCase $
      assertEqual "separador con texto sobrante"
        Nothing
        (reconocerCompleto separadorBloque "--- extra")
  ]

testsMundos :: Test
testsMundos = TestList
  [ TestCase $
      assertEqual "mundo válido con un objeto de meta obtenido por receta"
        (Right "Taller")
        (fmap mundoTitulo (leerMundo mundoConReceta))
  ,   TestLabel "mundo de ejemplo del profesor" $ TestCase $ do
      texto <- readFile "mundos/entrega.txt"
      assertEqual "se acepta el mundo de ejemplo"
        (Right "Entrega a última hora")
        (fmap mundoTitulo (leerMundo texto))
  , mundoInvalido "mundos/invalidos/sintaxis.txt" 2
  , mundoInvalido "mundos/invalidos/referencias.txt" 5
  , mundoInvalido "mundos/invalidos/receta.txt" 4
  ]

mundoInvalido :: FilePath -> Int -> Test
mundoInvalido archivo esperados = TestLabel archivo $ TestCase $ do
  texto <- readFile archivo
  case leerMundo texto of
    Left errores -> assertEqual "cantidad de errores" esperados (length errores)
    Right _ -> assertFailure "Se aceptó un mundo que debía ser inválido"

mundoConReceta :: String
mundoConReceta = unlines
  [ "MUNDO: Taller"
  , "INICIO: taller"
  , "META: llevar gancho a taller"
  , "RECETA: palo + cuerda -> gancho"
  , "---"
  , "OBJETO: palo"
  , "DESC: Un palo."
  , "---"
  , "OBJETO: cuerda"
  , "DESC: Una cuerda."
  , "---"
  , "OBJETO: gancho"
  , "DESC: Un gancho."
  , "---"
  , "SALA: taller"
  , "DESC: Un taller."
  , "OBJETO: palo"
  , "OBJETO: cuerda"
  ]

mockMundo :: Mundo
mockMundo = Mundo
  { mundoTitulo = "Mundo de Prueba"
  , mundoInicio = IdSala "vestibulo"
  , mundoMeta = LlegarA (IdSala "tesoro")
  , mundoObjetos = Map.fromList
      [(IdObjeto "llave_dorada", Objeto (IdObjeto "llave_dorada") "Una llave reluciente.")]
  , mundoSalas = Map.fromList
      [ ( IdSala "vestibulo"
        , Sala
            { salaId = IdSala "vestibulo"
            , salaDescripcion = "Un vestíbulo amplio con suelo de mármol."
            , salaSalidas = Map.fromList
                [ (Norte, Salida (IdSala "pasillo") Nothing)
                , (Este, Salida (IdSala "tesoro") (Just (IdObjeto "llave_dorada")))
                ]
            , salaObjetos = [IdObjeto "llave_dorada"]
            }
        )
      , ( IdSala "pasillo"
        , Sala
            { salaId = IdSala "pasillo"
            , salaDescripcion = "Un pasillo estrecho y oscuro."
            , salaSalidas = Map.fromList [(Sur, Salida (IdSala "vestibulo") Nothing)]
            , salaObjetos = []
            }
        )
      , ( IdSala "tesoro"
        , Sala
            { salaId = IdSala "tesoro"
            , salaDescripcion = "La cámara del tesoro secreta."
            , salaSalidas = Map.fromList [(Oeste, Salida (IdSala "vestibulo") Nothing)]
            , salaObjetos = []
            }
        )
      ]
  , mundoRecetas = []
  }

testsNucleo :: Test
testsNucleo = TestList
  [ TestCase $ do
      let est = estadoInicial mockMundo
      assertEqual "Inicio en sala vestibulo" (IdSala "vestibulo") (estadoSalaActual est)
      assertEqual "Inventario inicial vacío" [] (estadoInventario est)
      assertEqual "Aún no ha ganado" False (haGanado est)

  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (_, est1) = ejecutar (Ir Norte) est0
      assertEqual "Moverse al norte llega al pasillo" (IdSala "pasillo") (estadoSalaActual est1)

  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, est1) = ejecutar (Ir Oeste) est0
      assertEqual "No se mueve si no hay salida" (IdSala "vestibulo") (estadoSalaActual est1)
      assertEqual "Mensaje indica ausencia de salida"
        ["No hay salida hacia el oeste."] msgs

  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, est1) = ejecutar (Ir Este) est0
      assertEqual "No cruza la puerta cerrada sin llave" (IdSala "vestibulo") (estadoSalaActual est1)
      assertEqual "Mensaje advierte sobre bloqueo y requerimiento"
        ["La salida hacia el este está bloqueada. Requiere: llave_dorada."] msgs

  , TestCase $ do
      let est0 = (estadoInicial mockMundo) { estadoInventario = [IdObjeto "llave_dorada"] }
          (_, est1) = ejecutar (Ir Este) est0
      assertEqual "Cruza con la llave a la cámara del tesoro" (IdSala "tesoro") (estadoSalaActual est1)
      assertEqual "Alcanzó la meta de LlegarA y gana" True (haGanado est1)

  , TestCase $ do
      let estConLlave = (estadoInicial mockMundo) { estadoInventario = [IdObjeto "llave_dorada"] }
          (_, estEnTesoro) = ejecutar (Ir Este) estConLlave
          (_, estDeVuelta) = ejecutar (Ir Oeste) estEnTesoro
          estSinLlave = estDeVuelta { estadoInventario = [] }
          (_, estReingreso) = ejecutar (Ir Este) estSinLlave
      assertEqual "La salida este quedó permanentemente desbloqueada tras el primer cruce"
        (IdSala "tesoro") (estadoSalaActual estReingreso)

  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, _) = ejecutar Mirar est0
      assertEqual "Mirar produce al menos 4 líneas descriptivas" True (length msgs >= 4)

  , TestCase $ do
      let mundoMetaLlevar = mockMundo { mundoMeta = LlevarA (IdObjeto "llave_dorada") (IdSala "pasillo") }
          estSinObj = (estadoInicial mundoMetaLlevar) { estadoSalaActual = IdSala "pasillo", estadoInventario = [] }
          estConObj = (estadoInicial mundoMetaLlevar) { estadoSalaActual = IdSala "pasillo", estadoInventario = [IdObjeto "llave_dorada"] }
      assertEqual "No gana en pasillo sin el objeto requerido" False (haGanado estSinObj)
      assertEqual "Gana en pasillo portando el objeto requerido" True (haGanado estConObj)
  ]

main :: IO ()
main = do
  resultado <- runTestTT (TestList
    [ testsComandos
    , testsReconocedor
    , testsMundos
    , testsNucleo
    ])
  if errors resultado + failures resultado == 0
    then return ()
    else error "Pruebas unitarias fallaron"