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