## D1. Representación del mundo e identificadores

Stefano Casale.

**Contexto.** El motor maneja identificadores para salas y objetos, además de colecciones para modelar el mapa, las conexiones y los inventarios. Se requería evitar confusiones entre nombres de entidades en el código y asegurar búsquedas eficientes en tiempo de ejecución.

**Opciones.**
1. Usar `String` puro o sinónimos de tipo (`type IdSala = String`) junto con listas asociativas `[(String, a)]`.
2. Definir tipos nuevos envoltorios (`newtype IdSala = IdSala String`, `newtype IdObjeto = IdObjeto String`) y almacenar las salas y objetos en mapas asociativos balanceados (`Data.Map.Strict`).

**Decisión.** Adoptamos `newtype` para `IdSala` e `IdObjeto`, y `Data.Map.Strict` para las colecciones `mundoSalas`, `mundoObjetos` y `salaSalidas`. El sistema de tipos de Haskell previene que un identificador de objeto se use donde se espera una sala (o viceversa) en tiempo de compilación. Las búsquedas y actualizaciones se realizan en tiempo $O(\log n)$ en lugar del $O(n)$ de las listas.

**Consecuencias.** El código gana seguridad estática y legibilidad. La unicidad de identificadores queda respaldada por la estructura de `Map`. Como contraparte, se requiere desempaquetar o envolver los identificadores al interactuar con el reconocedor léxico o formatear texto para el usuario.

## D2. Qué guarda el Estado y modificaciones puras

Stefano Casale.

**Contexto.** Durante la partida, el jugador se traslada, toma o suelta objetos, combina recetas y desbloquea salidas. El motor debía registrar estas mutaciones de forma estrictamente funcional y pura.

**Opciones.**
1. Almacenar en el estado únicamente un registro de modificaciones ("delta"): una lista de objetos tomados, posiciones actuales de ítems y un conjunto de puertas desbloqueadas sobre un `Mundo` estático global.
2. Incorporar el `Mundo` completo dentro del registro `Estado` (`estadoMundo :: Mundo`), modificando directamente las salas, inventarios y salidas en cada turno.

**Decisión.** Mantener el `Mundo` completo dentro de `Estado` como una estructura inmutable que evoluciona funcionalmente turno a turno.

**Consecuencias.** Se simplifica la función `ejecutar`, pues no requiere reconstruir dinámicamente la vista del mundo en cada consulta. Gracias al intercambio estructural (*structural sharing*) de los árboles de `Data.Map`, actualizar una sala o transferir un objeto solo reasigna las ramas del árbol involucradas ($O(\log n)$), sin duplicar el resto de la memoria del mapa. Guardar o restaurar la partida consiste en serializar directamente el `Estado`.

## D3. Reconocedor y números de línea

Nazareth Colmenares.

**Contexto.** El motor tiene que leer el archivo por partes y poder indicar
la línea original cuando encuentra un error de sintaxis.

**Opciones.** Podíamos usar `ReadS` directamente en cada función o envolverlo
en un tipo `Parser` para poder combinar reconocedores con operadores comunes.

**Decisión.** Usamos `Parser` sobre `ReadS`. En `Motor.Reconocedor`, `<|>` toma
el primer resultado disponible y prueba la segunda opción desde la entrada
original si falla la primera. `reconocerCompleto` comprueba que no quede texto
sin consumir. En `Motor.Mundo` numeramos las líneas antes de quitar comentarios
y líneas vacías.

**Consecuencias.** Podemos reutilizar reconocedores como `identificador` en
varios campos y conservar las líneas originales en los errores. Como `<|>`
prioriza la primera opción que produce un resultado, el orden de las
alternativas importa cuando dos opciones pueden reconocer el mismo prefijo.

## D4. Acumulación de errores

Nazareth Colmenares.

**Contexto.** Un mundo puede tener varios errores independientes. Queremos
reportarlos juntos para que se puedan corregir en una sola revisión.

**Opciones.** Podíamos encadenar cada regla con `Either` y `do`, o combinar
los resultados de las reglas con `Validacion` y su instancia `Applicative`.

**Decisión.** `leerMundo` separa la sintaxis de la semántica con `Either`.
Dentro de cada etapa, usamos `Validacion` para concatenar los errores.
Si V1 falla, reportamos los errores sintácticos y detenemos el proceso.
Si V1 pasa, ejecutamos juntas las reglas V2–V10 y las de recetas.

**Consecuencias.** Podemos reportar, por ejemplo, una salida duplicada y
un objeto sin definir en la misma ejecución. Conservamos las salidas en
listas hasta completar las validaciones, porque convertirlas antes a un
`Map` podría ocultar una dirección repetida.

## D5. Objetos obtenidos mediante recetas

Nazareth Colmenares.

**Contexto.** Un objeto requerido por una salida o por la meta puede
conseguirse fabricándolo. A veces hace falta aplicar varias recetas.

**Opciones.** Podíamos verificar solo los objetos colocados, aplicar las
recetas una sola vez o repetirlas hasta que el conjunto de objetos
conseguibles deje de crecer.

**Decisión.** En `Motor.Mundo`, `cerrarRecetas` empieza con los objetos
colocados y agrega el resultado de cada receta cuyos ingredientes ya
estén en el conjunto. Repite el proceso hasta que no aparezcan resultados
nuevos. Las recetas con referencias inexistentes se reportan por separado.

**Consecuencias.** V10 puede reconocer cadenas de recetas. La comprobación
es aproximada porque no simula el consumo de ingredientes ni el orden de
las acciones durante una partida.

## D6. Formas de comandos aceptadas en el motor

Stefano Casale.

**Contexto.** La especificación básica exige comandos como `ir <dirección>`, `tomar <objeto>`, `mirar`, etc. Sin embargo, los guiones de prueba automatizados suministrados (como `guiones/entrega.txt`) envían direcciones cardinales desnudas (`norte`, `oeste`, `arriba`), y los usuarios tienden a emplear mayúsculas, artículos determinados o abreviaturas comunes.

**Opciones.**
1. Aceptar únicamente la sintaxis literal estricta de la tabla del enunciado (`ir norte`, `tomar carnet`).
2. Implementar un analizador flexible y tolerante que normalice mayúsculas, admita direcciones directas, descarte artículos en español y soporte sinónimos habituales de aventuras conversacionales.

**Decisión.** `Motor.Comandos.leerComando` convierte la entrada a minúsculas, permite direcciones directas sin el prefijo `ir` (`norte`, `oeste`, `arriba`, `n`, `s`), filtra artículos opcionales (`el`, `la`, `los`, `las`) en comandos con objeto (`tomar la llave_dorada`), y admite sinónimos frecuentes (`coger` por `tomar`, `dejar` por `soltar`, `inv`/`i` por `inventario`, `x` por `examinar` y `q` por `salir`).

**Consecuencias.** El motor supera sin fricción los guiones oficiales y ofrece una interacción cómoda sin comprometer la pureza del analizador. Se añade una capa ligera de filtrado previo de palabras antes de la correspondencia por patrones.

## D7. Formato y compatibilidad de las partidas guardadas

Nazareth Colmenares.

**Contexto.** Una partida guardada debe permitir que el jugador retome el
juego desde el estado que tenía al guardarla. Al cargarla, el juego también
debe informar si el archivo falta, si su contenido es inválido o si
corresponde a otro mundo.

**Opciones.** Podíamos diseñar un formato de texto propio y escribir su
lector, o guardar el estado completo con `Show` y recuperarlo con `readMaybe`.

**Decisión.** En `Motor.Persistencia`, `guardarPartida` crea un valor
`PartidaGuardada` con el título del mundo y el estado completo. Lo convierte
a texto con `show` y lo escribe en UTF-8. Al cargar, `cargarPartida` usa
`readMaybe` y compara el título guardado con el del mundo actual. Devuelve
el estado recuperado si los títulos coinciden, o un mensaje de error si
falla la lectura, el formato es inválido o los títulos difieren.

**Consecuencias.** Reutilizamos las representaciones de Haskell para
recuperar el estado sin mantener un lector propio. Si cambia la
representación de los datos, algunas partidas antiguas podrían dejar de
cargar. La comparación por título también permite que dos mundos distintos
con el mismo título se consideren compatibles.

## D8. Comportamiento de salidas bloqueadas

Stefano Casale.

**Contexto.** Una sala puede declarar una salida con `requiere <objeto>`. El enunciado indica que el jugador solo puede cruzarla si porta dicho objeto, pero deja abierta la semántica posterior: si la cerradura vuelve a trabarse en cruces sucesivos o si queda permanentemente abierta.

**Opciones.**
1. Requerir el objeto en cada tránsito (cerradura con pestillo automático). Si el jugador deja la llave en la sala contigua, no podría volver.
2. Desbloquear la puerta de manera permanente tras el primer tránsito exitoso portando la llave.

**Decisión.** Desbloqueo permanente. En `Motor.Nucleo.ejecutarIr`, cuando el jugador cruza una salida bloqueada portando el requisito en su inventario, la salida se transforma a `Salida destino Nothing` dentro de la sala en `estadoMundo`.

**Consecuencias.** El jugador puede explorar libremente la nueva zona e incluso soltar la llave sin riesgo de quedar atrapado o requerir retrocesos artificiales. Modela fielmente la experiencia clásica de los juegos de mazmorras y aventuras conversacionales.
