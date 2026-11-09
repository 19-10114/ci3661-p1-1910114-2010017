{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Motor.Tipos
Description : Modelado algebraico de tipos del dominio del motor de aventuras.
Maintainer  : Stefano Casale (19-10114), Nazareth Colmenarez (20-10017)

Este módulo define los tipos de datos fundamentales que modelan el mundo,
sus componentes (salas, objetos, salidas, recetas), el estado del juego,
los comandos del jugador y los tipos de error sintáctico y semántico.
-}
module Motor.Tipos
  ( -- * Identificadores fuertemente tipados
    IdSala(..)
  , IdObjeto(..)

    -- * Componentes espaciales y direcciones
  , Direccion(..)
  , Salida(..)
  , Sala(..)

    -- * Objetos y Alquimia (Variante 3: Combinar)
  , Objeto(..)
  , Receta(..)

    -- * Meta y Definición del Mundo
  , Meta(..)
  , Mundo(..)

    -- * Estado y Persistencia
  , Estado(..)
  , PartidaGuardada(..)

    -- * Comandos del Jugador
  , Comando(..)

    -- * Errores de Validación (V1 a V10)
  , ErrorMundo(..)
  , mostrarErrorMundo

    -- * Acumulador Aplicativo de Validación
  , Validacion(..)
  , validacionAEither
  , eitherAValidacion
  ) where

import Data.Map.Strict (Map)

-- | Identificador de una sala en el mundo.
newtype IdSala = IdSala { unIdSala :: String }
  deriving (Eq, Ord, Show, Read)

-- | Identificador de un objeto en el mundo.
newtype IdObjeto = IdObjeto { unIdObjeto :: String }
  deriving (Eq, Ord, Show, Read)

-- | Direcciones espaciales permitidas para el desplazamiento.
data Direccion
  = Norte
  | Sur
  | Este
  | Oeste
  | Arriba
  | Abajo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Representación de una salida desde una sala.
data Salida = Salida
  { salidaDestino   :: !IdSala          -- ^ Sala a la que conduce la salida.
  , salidaRequisito :: !(Maybe IdObjeto) -- ^ Objeto necesario para cruzarla (si está bloqueada).
  } deriving (Eq, Show, Read)

-- | Representación de un objeto interactivo del mundo.
data Objeto = Objeto
  { objetoId          :: !IdObjeto -- ^ Identificador único del objeto.
  , objetoDescripcion :: !String   -- ^ Descripción visual al examinar.
  } deriving (Eq, Show, Read)

-- | Representación de una sala en el mapa de juego.
data Sala = Sala
  { salaId          :: !IdSala                 -- ^ Identificador único de la sala.
  , salaDescripcion :: !String                 -- ^ Descripción mostrada al entrar o mirar.
  , salaSalidas     :: !(Map Direccion Salida) -- ^ Salidas disponibles por dirección.
  , salaObjetos     :: ![IdObjeto]             -- ^ Objetos presentes actualmente en la sala.
  } deriving (Eq, Show, Read)

-- | Condición de victoria del mundo.
data Meta
  = LlegarA !IdSala            -- ^ El jugador gana al pisar esta sala.
  | LlevarA !IdObjeto !IdSala  -- ^ El jugador gana al estar en la sala portando el objeto.
  deriving (Eq, Show, Read)

-- | Receta de síntesis de objetos (Variante 3: Combinar).
data Receta = Receta
  { recetaIngrediente1 :: !IdObjeto -- ^ Primer ingrediente.
  , recetaIngrediente2 :: !IdObjeto -- ^ Segundo ingrediente.
  , recetaResultado    :: !IdObjeto -- ^ Objeto sintetizado al combinar.
  } deriving (Eq, Show, Read)

-- | Representación global e inmutable de la estructura del mundo.
data Mundo = Mundo
  { mundoTitulo   :: !String                 -- ^ Título del mundo (cabecera MUNDO).
  , mundoInicio   :: !IdSala                 -- ^ Sala inicial donde comienza el jugador.
  , mundoMeta     :: !Meta                   -- ^ Condición de victoria.
  , mundoObjetos  :: !(Map IdObjeto Objeto)  -- ^ Catálogo de todos los objetos existentes.
  , mundoSalas    :: !(Map IdSala Sala)      -- ^ Mapa de todas las salas indexadas por IdSala.
  , mundoRecetas  :: ![Receta]               -- ^ Recetas válidas para la Variante 3.
  } deriving (Eq, Show, Read)

-- | Estado activo y mutable de una partida en curso.
-- Implementa el patrón de Mundo Vivo inmutable con persistencia estructural.
data Estado = Estado
  { estadoMundo      :: !Mundo     -- ^ Copia viva del mundo con sus objetos y salidas actualizadas.
  , estadoSalaActual :: !IdSala    -- ^ Ubicación actual del jugador.
  , estadoInventario :: ![IdObjeto]-- ^ Objetos que el jugador lleva consigo.
  } deriving (Eq, Show, Read)

-- | Estructura para serialización de partidas guardadas con comprobación de título.
data PartidaGuardada = PartidaGuardada
  { partidaTituloMundo :: !String  -- ^ Título del mundo para validar congruencia al cargar.
  , partidaEstado      :: !Estado  -- ^ Instantánea completa del estado.
  } deriving (Eq, Show, Read)

-- | Comandos interpretables por el motor.
data Comando
  = Ir !Direccion
  | Mirar
  | Tomar !IdObjeto
  | Soltar !IdObjeto
  | Examinar !IdObjeto
  | Inventario
  | Guardar !FilePath
  | Cargar !FilePath
  | Combinar !IdObjeto !IdObjeto
  | Ayuda
  | Salir
  deriving (Eq, Show, Read)

-- | Modelado algebraico de todos los posibles errores sintácticos (V1) y semánticos (V2 a V10).
data ErrorMundo
  -- | V1: Error de sintaxis con número de línea del archivo original.
  = ErrorSintaxis !Int !String
  -- | V2: Dos salas o dos objetos comparten el mismo identificador.
  | ErrorIdDuplicado !String !String
  -- | V3: Una salida apunta hacia una sala inexistente.
  | ErrorSalidaInexistente !IdSala !Direccion !IdSala
  -- | V4: Una salida requiere un objeto que no existe.
  | ErrorRequisitoInexistente !IdSala !Direccion !IdObjeto
  -- | V5: Una sala define dos salidas en la misma dirección.
  | ErrorSalidasDuplicadas !IdSala !Direccion
  -- | V6: Una sala contiene un objeto no definido.
  | ErrorObjetoNoDefinido !IdSala !IdObjeto
  -- | V7: Un objeto está colocado en más de una sala (o repetido en la misma).
  | ErrorObjetoColocadoMultiple !IdObjeto ![IdSala]
  -- | V8: INICIO o META hacen referencia a una sala o un objeto inexistentes.
  | ErrorInicioInexistente !IdSala
  | ErrorMetaInexistente !String
  -- | V9: Una sala no es alcanzable desde INICIO siguiendo salidas.
  | ErrorSalaInalcanzable !IdSala
  -- | V10: Un objeto requerido no está colocado ni es conseguible por receta.
  | ErrorObjetoNoConseguible !IdObjeto
  -- | Variante 3: Una receta utiliza un objeto inexistente en el catálogo.
  | ErrorRecetaObjetoInexistente !IdObjeto
  deriving (Eq, Show)

-- | Formatea un 'ErrorMundo' conforme a la especificación oficial:
-- Cada mensaje en su propia línea, comenzando exactamente con "Error: ".
mostrarErrorMundo :: ErrorMundo -> String
mostrarErrorMundo err = "Error: " ++ detalle err
  where
    detalle (ErrorSintaxis linea msg) =
      "línea " ++ show linea ++ ": " ++ msg
    detalle (ErrorIdDuplicado tipo idNom) =
      "identificador de " ++ tipo ++ " duplicado: «" ++ idNom ++ "»"
    detalle (ErrorSalidaInexistente (IdSala origen) dir (IdSala dest)) =
      "la sala «" ++ origen ++ "» tiene salida " ++ show dir ++ " hacia sala inexistente «" ++ dest ++ "»"
    detalle (ErrorRequisitoInexistente (IdSala origen) dir (IdObjeto obj)) =
      "la salida " ++ show dir ++ " de «" ++ origen ++ "» requiere un objeto inexistente «" ++ obj ++ "»"
    detalle (ErrorSalidasDuplicadas (IdSala origen) dir) =
      "la sala «" ++ origen ++ "» tiene más de una salida en dirección " ++ show dir
    detalle (ErrorObjetoNoDefinido (IdSala sala) (IdObjeto obj)) =
      "la sala «" ++ sala ++ "» contiene el objeto no definido «" ++ obj ++ "»"
    detalle (ErrorObjetoColocadoMultiple (IdObjeto obj) salas) =
      "el objeto «" ++ obj ++ "» está colocado en múltiples salas: " ++ unwords (map unIdSala salas)
    detalle (ErrorInicioInexistente (IdSala inicio)) =
      "la sala de INICIO «" ++ inicio ++ "» no existe"
    detalle (ErrorMetaInexistente desc) =
      "la META hace referencia a un elemento inexistente: " ++ desc
    detalle (ErrorSalaInalcanzable (IdSala sala)) =
      "la sala «" ++ sala ++ "» no es alcanzable desde la sala de inicio"
    detalle (ErrorObjetoNoConseguible (IdObjeto obj)) =
      "el objeto «" ++ obj ++ "» es necesario para ganar o cruzar pero no está en ninguna sala accesible"
    detalle (ErrorRecetaObjetoInexistente (IdObjeto obj)) =
      "la receta utiliza el objeto no definido «" ++ obj ++ "»"

-- | Functor aplicativo acumulador de errores (Decisión D4).
-- A diferencia de 'Either', que cortocircuita ante el primer fallo en su mónada,
-- 'Validacion' combina los errores mediante el monoide de listas ('++').
data Validacion e a
  = Fallo ![e]
  | Exito !a
  deriving (Eq, Show)

instance Functor (Validacion e) where
  fmap _ (Fallo es) = Fallo es
  fmap f (Exito x)  = Exito (f x)

instance Applicative (Validacion e) where
  pure = Exito
  Fallo e1 <*> Fallo e2 = Fallo (e1 ++ e2)
  Fallo e1 <*> _        = Fallo e1
  _        <*> Fallo e2 = Fallo e2
  Exito f  <*> Exito x  = Exito (f x)

-- | Proyección de 'Validacion' a la firma estándar 'Either [e] a'.
validacionAEither :: Validacion e a -> Either [e] a
validacionAEither (Fallo es) = Left es
validacionAEither (Exito x)  = Right x

-- | Inyección de 'Either [e] a' a 'Validacion e a'.
eitherAValidacion :: Either [e] a -> Validacion e a
eitherAValidacion (Left es) = Fallo es
eitherAValidacion (Right x) = Exito x
