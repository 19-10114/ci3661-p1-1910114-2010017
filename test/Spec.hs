module Main where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (ejecutar, estadoInicial, haGanado)
import Motor.Persistencia (cargarPartida, guardarPartida, leerArchivoMundo)
import Motor.Reconocedor (identificador, reconocerCompleto, runParser, separadorBloque)
import Motor.Tipos
import System.Directory (doesFileExist, removeFile)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, utf8, withFile)
import Test.HUnit

leerArchivoUtf8 :: FilePath -> IO String
leerArchivoUtf8 ruta = withFile ruta ReadMode $ \h -> do
  hSetEncoding h utf8
  contenido <- hGetContents h
  length contenido `seq` return contenido

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
      assertEqual "separador con dos guiones"
        (Just "--")
        (reconocerCompleto separadorBloque "  --  ")
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
      texto <- leerArchivoUtf8 "mundos/entrega.txt"
      assertEqual "se acepta el mundo de ejemplo"
        (Right "Entrega a última hora")
        (fmap mundoTitulo (leerMundo texto))
  , mundoInvalido "mundos/invalidos/sintaxis.txt" 2
  , mundoInvalido "mundos/invalidos/referencias.txt" 5
  , mundoInvalido "mundos/invalidos/receta.txt" 4
  , mundoInvalido "../material/mundos/invalido_semantica.txt" 9
  ]

mundoInvalido :: FilePath -> Int -> Test
mundoInvalido archivo esperados = TestLabel archivo $ TestCase $ do
  texto <- leerArchivoUtf8 archivo
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
      [ (IdObjeto "llave_dorada", Objeto (IdObjeto "llave_dorada") "Una llave reluciente.")
      , (IdObjeto "polvo_rojo",   Objeto (IdObjeto "polvo_rojo")   "Un extraño polvo rojizo.")
      , (IdObjeto "polvo_azul",   Objeto (IdObjeto "polvo_azul")   "Un frasco con polvo azulado.")
      , (IdObjeto "pildora_magica", Objeto (IdObjeto "pildora_magica") "Una píldora brillante alquímica.")
      ]
  , mundoSalas = Map.fromList
      [ ( IdSala "vestibulo"
        , Sala
            { salaId = IdSala "vestibulo"
            , salaDescripcion = "Un vestíbulo amplio con suelo de mármol."
            , salaSalidas = Map.fromList
                [ (Norte, Salida (IdSala "pasillo") Nothing)
                , (Este, Salida (IdSala "tesoro") (Just (IdObjeto "llave_dorada")))
                ]
            , salaObjetos = [IdObjeto "llave_dorada", IdObjeto "polvo_rojo", IdObjeto "polvo_azul"]
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
  , mundoRecetas =
      [ Receta (IdObjeto "polvo_rojo") (IdObjeto "polvo_azul") (IdObjeto "pildora_magica")
      ]
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

    -- 9. Tomar objeto
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, est1) = ejecutar (Tomar (IdObjeto "llave_dorada")) est0
      assertEqual "Inventario contiene el objeto tomado" [IdObjeto "llave_dorada"] (estadoInventario est1)
      let salaVestibulo = Map.lookup (IdSala "vestibulo") (mundoSalas (estadoMundo est1))
      assertEqual "Objeto removido de la sala"
        (Just [IdObjeto "polvo_rojo", IdObjeto "polvo_azul"])
        (fmap salaObjetos salaVestibulo)
      assertEqual "Mensaje de confirmación" ["Has tomado: llave_dorada."] msgs

    -- 10. Tomar objeto inexistente
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, est1) = ejecutar (Tomar (IdObjeto "objeto_fantasma")) est0
      assertEqual "Inventario permanece vacío" [] (estadoInventario est1)
      assertEqual "Mensaje de error" ["No ves objeto_fantasma aquí."] msgs

    -- 11. Soltar objeto
  , TestCase $ do
      let est0 = (estadoInicial mockMundo) { estadoInventario = [IdObjeto "llave_dorada"] }
          (msgs, est1) = ejecutar (Soltar (IdObjeto "llave_dorada")) est0
      assertEqual "Inventario queda vacío" [] (estadoInventario est1)
      let salaVestibulo = Map.lookup (IdSala "vestibulo") (mundoSalas (estadoMundo est1))
      assertEqual "Objeto añadido a la sala"
        (Just [IdObjeto "llave_dorada", IdObjeto "llave_dorada", IdObjeto "polvo_rojo", IdObjeto "polvo_azul"])
        (fmap salaObjetos salaVestibulo)
      assertEqual "Mensaje de soltar" ["Has soltado: llave_dorada."] msgs

    -- 12. Soltar objeto que no se posee
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgs, est1) = ejecutar (Soltar (IdObjeto "llave_dorada")) est0
      assertEqual "Mensaje de no llevado" ["No llevas llave_dorada en tu inventario."] msgs
      assertEqual "Estado sin alteraciones" est0 est1

    -- 13. Examinar en sala y en inventario
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgsEnSala, _) = ejecutar (Examinar (IdObjeto "polvo_rojo")) est0
      assertEqual "Examinar en la sala muestra descripción" ["Un extraño polvo rojizo."] msgsEnSala
      let estConObj = est0 { estadoInventario = [IdObjeto "pildora_magica"] }
          (msgsEnInv, _) = ejecutar (Examinar (IdObjeto "pildora_magica")) estConObj
      assertEqual "Examinar en inventario muestra descripción" ["Una píldora brillante alquímica."] msgsEnInv
      let (msgsNoVisto, _) = ejecutar (Examinar (IdObjeto "objeto_invisible")) est0
      assertEqual "Examinar objeto inexistente" ["No tienes ni ves objeto_invisible aquí."] msgsNoVisto

    -- 14. Inventario vacío vs con objetos
  , TestCase $ do
      let est0 = estadoInicial mockMundo
          (msgsVacio, _) = ejecutar Inventario est0
      assertEqual "Inventario vacío" ["Tu inventario está vacío."] msgsVacio
      let estLleno = est0 { estadoInventario = [IdObjeto "polvo_rojo", IdObjeto "polvo_azul"] }
          (msgsLleno, _) = ejecutar Inventario estLleno
      assertEqual "Inventario con objetos" ["Llevas contigo: polvo_rojo polvo_azul"] msgsLleno

    -- 15. Combinar (Variante 3: simétrica y conmutativa)
  , TestCase $ do
      let est0 = (estadoInicial mockMundo)
                   { estadoInventario = [IdObjeto "polvo_rojo", IdObjeto "polvo_azul"] }
          (msgs, est1) = ejecutar (Combinar (IdObjeto "polvo_rojo") (IdObjeto "polvo_azul")) est0
      assertEqual "Inventario reemplaza ingredientes por producto"
        [IdObjeto "pildora_magica"] (estadoInventario est1)
      assertEqual "Mensaje de combinación exitosa"
        ["Has combinado polvo_rojo con polvo_azul para obtener: pildora_magica."] msgs

      -- Prueba de conmutatividad: combinar b con a
      let (msgsConmutativo, estConmutativo) =
            ejecutar (Combinar (IdObjeto "polvo_azul") (IdObjeto "polvo_rojo")) est0
      assertEqual "Combinación conmutativa da el mismo producto"
        [IdObjeto "pildora_magica"] (estadoInventario estConmutativo)
      assertEqual "Mensaje conmutativo"
        ["Has combinado polvo_azul con polvo_rojo para obtener: pildora_magica."] msgsConmutativo

    -- 16. Combinar con ingredientes faltantes
  , TestCase $ do
      let est0 = (estadoInicial mockMundo) { estadoInventario = [IdObjeto "polvo_rojo"] }
          (msgs, est1) = ejecutar (Combinar (IdObjeto "polvo_rojo") (IdObjeto "polvo_azul")) est0
      assertEqual "Falta un ingrediente"
        ["No tienes ambos objetos en tu inventario para combinarlos."] msgs
      assertEqual "Inventario sin cambios" [IdObjeto "polvo_rojo"] (estadoInventario est1)

    -- 17. Combinar receta inexistente
  , TestCase $ do
      let est0 = (estadoInicial mockMundo)
                   { estadoInventario = [IdObjeto "llave_dorada", IdObjeto "polvo_rojo"] }
          (msgs, est1) = ejecutar (Combinar (IdObjeto "llave_dorada") (IdObjeto "polvo_rojo")) est0
      assertEqual "Receta inexistente"
        ["No parece que estos dos objetos se puedan combinar."] msgs
      assertEqual "Inventario sin cambios"
        [IdObjeto "llave_dorada", IdObjeto "polvo_rojo"] (estadoInventario est1)

    -- 18. Victoria mediante combinación en la sala de meta
  , TestCase $ do
      let mundoMetaAlquimia = mockMundo
            { mundoMeta = LlevarA (IdObjeto "pildora_magica") (IdSala "vestibulo") }
          est0 = (estadoInicial mundoMetaAlquimia)
                   { estadoInventario = [IdObjeto "polvo_rojo", IdObjeto "polvo_azul"] }
          (msgs, est1) = ejecutar (Combinar (IdObjeto "polvo_rojo") (IdObjeto "polvo_azul")) est0
      assertEqual "Gana al fabricar el objeto de la meta en la sala de meta" True (haGanado est1)
      assertEqual "Mensaje incluye felicitación oficial de victoria"
        [ "Has combinado polvo_rojo con polvo_azul para obtener: pildora_magica."
        , "*** ¡Has ganado! ***"
        ] msgs
  ]

-- | Ejecuta secuencialmente una lista de comandos sin IO, acumulando los mensajes.
ejecutarSecuencia :: [Comando] -> Estado -> ([String], Estado)
ejecutarSecuencia [] est = ([], est)
ejecutarSecuencia (c:cs) est =
  let (msgs1, est1) = ejecutar c est
      (msgs2, est2) = ejecutarSecuencia cs est1
  in (msgs1 ++ msgs2, est2)

testsSecuencias :: Test
testsSecuencias = TestList
  [ TestLabel "Secuencia pura: chocar con salida bloqueada, tomar llave y desbloquear permanentemente" $ TestCase $ do
      let est0 = estadoInicial mockMundo
      -- 1. Intenta cruzar hacia el este (bloqueado por llave_dorada)
      let (msgs1, est1) = ejecutarSecuencia [Ir Este] est0
      assertEqual "No se mueve de vestíbulo" (IdSala "vestibulo") (estadoSalaActual est1)
      assertEqual "Mensaje indica bloqueo por llave_dorada"
        ["La salida hacia el este está bloqueada. Requiere: llave_dorada."] msgs1

      -- 2. Toma la llave y cruza hacia el este
      let (msgs2, est2) = ejecutarSecuencia [Tomar (IdObjeto "llave_dorada"), Ir Este] est1
      assertEqual "Cruza a la cámara del tesoro" (IdSala "tesoro") (estadoSalaActual est2)
      assertEqual "Alcanzó meta LlegarA" True (haGanado est2)
      assertEqual "Mensajes incluyen uso de llave y victoria"
        True (any (== "*** ¡Has ganado! ***") msgs2)

      -- 3. Regresa al vestíbulo, suelta la llave y vuelve a entrar sin ella
      let (msgs3, est3) = ejecutarSecuencia
            [ Ir Oeste
            , Soltar (IdObjeto "llave_dorada")
            , Ir Este
            ] est2
      assertEqual "Entra de nuevo al tesoro sin tener la llave" (IdSala "tesoro") (estadoSalaActual est3)
      assertEqual "Inventario no tiene la llave" False (IdObjeto "llave_dorada" `elem` estadoInventario est3)
      assertEqual "Vuelve a ganar al reingresar" True (haGanado est3)
      assertEqual "No vuelve a pedir la llave" False (any ("bloqueada" `isInfixOf`) msgs3)

  , TestLabel "Secuencia pura: combinación simétrica y obtención inductiva de producto para ganar" $ TestCase $ do
      let mundoMetaQuimica = mockMundo
            { mundoMeta = LlevarA (IdObjeto "pildora_magica") (IdSala "pasillo") }
          est0 = estadoInicial mundoMetaQuimica
          secuencia =
            [ Tomar (IdObjeto "polvo_azul")
            , Tomar (IdObjeto "polvo_rojo")
            , Inventario
            , Combinar (IdObjeto "polvo_azul") (IdObjeto "polvo_rojo")
            , Examinar (IdObjeto "pildora_magica")
            , Ir Norte
            ]
          (msgs, estFinal) = ejecutarSecuencia secuencia est0

      assertEqual "Llega a la sala pasillo" (IdSala "pasillo") (estadoSalaActual estFinal)
      assertEqual "Inventario solo contiene el producto sintetizado"
        [IdObjeto "pildora_magica"] (estadoInventario estFinal)
      assertEqual "Condición de victoria cumplida" True (haGanado estFinal)
      assertEqual "Mensaje oficial de victoria emitido"
        True (any (== "*** ¡Has ganado! ***") msgs)

  , TestLabel "Secuencia pura: cadena de recetas en varios pasos (A + B -> C, C + D -> E)" $ TestCase $ do
      let mundoAlquimiaCadena = mockMundo
            { mundoObjetos = Map.fromList
                [ (IdObjeto "hierro",   Objeto (IdObjeto "hierro") "Un lingote de hierro.")
                , (IdObjeto "carbon",   Objeto (IdObjeto "carbon") "Un trozo de carbón mineral.")
                , (IdObjeto "acero",    Objeto (IdObjeto "acero") "Una barra de acero templado.")
                , (IdObjeto "mango",    Objeto (IdObjeto "mango") "Un mango de madera tallada.")
                , (IdObjeto "espada",   Objeto (IdObjeto "espada") "Una espada reluciente.")
                ]
            , mundoRecetas =
                [ Receta (IdObjeto "hierro") (IdObjeto "carbon") (IdObjeto "acero")
                , Receta (IdObjeto "acero")  (IdObjeto "mango")  (IdObjeto "espada")
                ]
            , mundoSalas = Map.fromList
                [ ( IdSala "vestibulo"
                  , Sala
                      { salaId = IdSala "vestibulo"
                      , salaDescripcion = "Forja antigua."
                      , salaSalidas = Map.empty
                      , salaObjetos = [IdObjeto "hierro", IdObjeto "carbon", IdObjeto "mango"]
                      }
                  )
                ]
            , mundoMeta = LlevarA (IdObjeto "espada") (IdSala "vestibulo")
            }
          est0 = estadoInicial mundoAlquimiaCadena
          secuencia =
            [ Tomar (IdObjeto "hierro")
            , Tomar (IdObjeto "carbon")
            , Tomar (IdObjeto "mango")
            , Combinar (IdObjeto "hierro") (IdObjeto "carbon")
            , Combinar (IdObjeto "mango") (IdObjeto "acero") -- Conmutativo: mango + acero
            ]
          (msgs, estFinal) = ejecutarSecuencia secuencia est0

      assertEqual "Inventario final contiene únicamente la espada resultante"
        [IdObjeto "espada"] (estadoInventario estFinal)
      assertEqual "Victoria al completar la síntesis de la meta en la sala objetivo"
        True (haGanado estFinal)
      assertEqual "Mensaje de victoria presente"
        True (any (== "*** ¡Has ganado! ***") msgs)

  , TestLabel "Secuencia pura: comandos inválidos y de error no corrompen el estado del juego" $ TestCase $ do
      let est0 = estadoInicial mockMundo
          secuencia =
            [ Ir Abajo                                               -- No hay salida
            , Tomar (IdObjeto "fantasma")                            -- Objeto no existe
            , Soltar (IdObjeto "fantasma")                           -- No lo lleva
            , Combinar (IdObjeto "fantasma") (IdObjeto "polvo_rojo") -- No lo tiene
            , Mirar                                                  -- Informativo puro
            , Ayuda                                                  -- Informativo puro
            , Inventario                                             -- Vacío
            ]
          (_, estFinal) = ejecutarSecuencia secuencia est0

      assertEqual "Sala actual intacta tras comandos fallidos"
        (estadoSalaActual est0) (estadoSalaActual estFinal)
      assertEqual "Inventario intacto y vacío"
        [] (estadoInventario estFinal)
      assertEqual "Salas y objetos intactos en el mundo"
        (mundoSalas (estadoMundo est0)) (mundoSalas (estadoMundo estFinal))

  , TestLabel "Variante 3: intentar combinar con solo 1 copia cuando la receta pide el mismo objeto" $ TestCase $ do
      let mundoDoble = mockMundo
            { mundoObjetos = Map.fromList
                [ (IdObjeto "cristal", Objeto (IdObjeto "cristal") "Un cristal.")
                , (IdObjeto "diamante", Objeto (IdObjeto "diamante") "Un diamante.")
                ]
            , mundoRecetas =
                [ Receta (IdObjeto "cristal") (IdObjeto "cristal") (IdObjeto "diamante")
                ]
            }
          estConUnCristal = (estadoInicial mundoDoble)
            { estadoInventario = [IdObjeto "cristal"] }
          (msgs1, est1) = ejecutar (Combinar (IdObjeto "cristal") (IdObjeto "cristal")) estConUnCristal

      assertEqual "Rechaza combinación si no tiene 2 unidades del mismo ingrediente"
        ["No tienes ambos objetos en tu inventario para combinarlos."] msgs1
      assertEqual "Inventario preserva la única copia"
        [IdObjeto "cristal"] (estadoInventario est1)

      -- Caso con 2 copias:
      let estConDosCristales = estConUnCristal
            { estadoInventario = [IdObjeto "cristal", IdObjeto "cristal"] }
          (msgs2, est2) = ejecutar (Combinar (IdObjeto "cristal") (IdObjeto "cristal")) estConDosCristales

      assertEqual "Acepta combinación al tener 2 unidades del mismo ingrediente"
        [IdObjeto "diamante"] (estadoInventario est2)
      assertEqual "Mensaje confirma obtención de diamante"
        ["Has combinado cristal con cristal para obtener: diamante."] msgs2

  , TestLabel "Variante 3: combinación conserva copias sobrantes en el inventario" $ TestCase $ do
      let estConSobrantes = (estadoInicial mockMundo)
            { estadoInventario =
                [ IdObjeto "polvo_rojo"
                , IdObjeto "polvo_azul"
                , IdObjeto "polvo_rojo"
                , IdObjeto "llave_dorada"
                ]
            }
          (msgs, estFinal) =
            ejecutar (Combinar (IdObjeto "polvo_rojo") (IdObjeto "polvo_azul")) estConSobrantes

      assertEqual "Consume solo 1 de cada ingrediente y conserva el resto"
        [IdObjeto "pildora_magica", IdObjeto "polvo_rojo", IdObjeto "llave_dorada"]
        (estadoInventario estFinal)
      assertEqual "Mensaje correcto"
        ["Has combinado polvo_rojo con polvo_azul para obtener: pildora_magica."] msgs
  ]

testsPersistencia :: Test
testsPersistencia = TestList
  [ TestLabel "guardar y cargar partida exitosa" $ TestCase $ do
      let rutaTemp = "test_guardado_ok.sav"
          est0 = (estadoInicial mockMundo) { estadoInventario = [IdObjeto "llave_dorada"] }
      guardarPartida rutaTemp est0
      resultadoCarga <- cargarPartida rutaTemp est0
      existe <- doesFileExist rutaTemp
      if existe then removeFile rutaTemp else return ()
      case resultadoCarga of
        Left err -> assertFailure ("Falló la carga válida: " ++ err)
        Right estCargado -> do
          assertEqual "Sala actual recuperada" (estadoSalaActual est0) (estadoSalaActual estCargado)
          assertEqual "Inventario recuperado" (estadoInventario est0) (estadoInventario estCargado)
          assertEqual "Título del mundo coincide"
            (mundoTitulo (estadoMundo est0))
            (mundoTitulo (estadoMundo estCargado))

  , TestLabel "cargar partida de archivo inexistente retorna Left" $ TestCase $ do
      let rutaInexistente = "archivo_que_no_existe_12345.sav"
          est0 = estadoInicial mockMundo
      resultadoCarga <- cargarPartida rutaInexistente est0
      case resultadoCarga of
        Left msg -> assertEqual "Mensaje indica inexistencia"
          ("El archivo de guardado «" ++ rutaInexistente ++ "» no existe.") msg
        Right _ -> assertFailure "Se cargó un archivo inexistente"

  , TestLabel "cargar partida corrupta retorna Left" $ TestCase $ do
      let rutaCorrupta = "test_guardado_corrupto.sav"
          est0 = estadoInicial mockMundo
      writeFile rutaCorrupta "ESTO NO ES UN ESTADO SERIALIZADO VALIDO :("
      resultadoCarga <- cargarPartida rutaCorrupta est0
      existe <- doesFileExist rutaCorrupta
      if existe then removeFile rutaCorrupta else return ()
      case resultadoCarga of
        Left msg -> assertEqual "Mensaje indica archivo corrupto"
          ("El archivo «" ++ rutaCorrupta ++ "» está corrupto o no tiene un formato válido de partida guardada.") msg
        Right _ -> assertFailure "Se cargó una partida con contenido corrupto"

  , TestLabel "cargar partida de mundo distinto retorna Left (Decisión D7)" $ TestCase $ do
      let rutaDistinto = "test_guardado_distinto.sav"
          mundoA = mockMundo { mundoTitulo = "Mundo Fantasía" }
          mundoB = mockMundo { mundoTitulo = "Mundo Ciencia Ficción" }
          estA = estadoInicial mundoA
          estB = estadoInicial mundoB
      guardarPartida rutaDistinto estA
      resultadoCarga <- cargarPartida rutaDistinto estB
      existe <- doesFileExist rutaDistinto
      if existe then removeFile rutaDistinto else return ()
      case resultadoCarga of
        Left msg -> assertEqual "Mensaje advierte incompatibilidad de títulos de mundo"
          ("La partida guardada pertenece al mundo «Mundo Fantasía», pero la partida actual es del mundo «Mundo Ciencia Ficción».") msg
        Right _ -> assertFailure "Se permitió cargar una partida perteneciente a otro mundo"

  , TestLabel "leerArchivoMundo carga archivo existente" $ TestCase $ do
      res <- leerArchivoMundo "mundos/entrega.txt"
      case res of
        Left errs -> assertFailure ("Error al leer mundo de entrega: " ++ show errs)
        Right m -> assertEqual "Título del mundo leído" "Entrega a última hora" (mundoTitulo m)

  , TestLabel "leerArchivoMundo reporta error con archivo inexistente" $ TestCase $ do
      res <- leerArchivoMundo "mundos/no_existe_987.txt"
      case res of
        Left errs -> assertEqual "Reporta error de archivo inexistente" 1 (length errs)
        Right _ -> assertFailure "Se leyó un archivo que no existe"
  ]

main :: IO ()
main = do
  resultado <- runTestTT (TestList
    [ testsComandos
    , testsReconocedor
    , testsMundos
    , testsNucleo
    , testsSecuencias
    , testsPersistencia
    ])
  if errors resultado + failures resultado == 0
    then return ()
    else error "Pruebas unitarias fallaron"
