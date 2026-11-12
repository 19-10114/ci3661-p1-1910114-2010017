# Uso de inteligencia artificial

## Stefano Casale

Usé el asistente de inteligencia artificial Antigravity para la planificación
inicial del proyecto en pareja, el modelado funcional de los tipos inmutables
del dominio en `Motor.Tipos`, la estructuración del intérprete de comandos en
`Motor.Comandos`, el bucle principal de ejecución CLI en `Main.hs` y el diseño
exhaustivo de casos de prueba con HUnit en `test/Spec.hs`.

### Ejemplo de una corrección

Durante la implementación del módulo de persistencia (`Motor.Persistencia`) y
las pruebas de lectura de mundos, el asistente sugirió utilizar la función
estándar `readFile` de `Prelude`. En sistemas operativos Windows, `readFile`
opera utilizando la página de códigos local del sistema (CP1252), lo cual
provocaba que caracteres acentuados en UTF-8 (como la vocal «ú» en el título
«Entrega a última hora» de `mundos/entrega.txt`) se interpretaran de forma
corrupta (`\9500\9553ltima hora`), haciendo fallar la validación sintáctica
y las pruebas automatizadas de títulos.

Rechacé esa sugerencia e implementé la lectura y escritura seguras mediante
`withFile` fijando explícitamente `hSetEncoding h utf8` tanto en
`Motor.Persistencia` como en `test/Spec.hs`. De esta forma se aseguró la total
portabilidad multiplataforma del motor tanto en Linux como en Windows.

## Nazareth Colmenares

Usé ChatGPT para obtener propuestas de código para el reconocedor y la
validación de mundos. También lo usé como guía para organizar los cambios en los
que me tocaria trabajar. 

### Ejemplo de una corrección

En el paso de sintaxis, ChatGPT me dio una versión de `separarBloques` y,
por separado, una línea para reemplazarla. Al pegar el cambio quedaron
dos definiciones de la misma función y GHCi reportó `Multiple declarations`.
Eliminé la definición anterior y conservé la que usa `filter (not . null)`.
Después recargué el módulo y comprobé que el mundo inválido reportaba los
cuatro errores esperados y que el mundo válido se aceptaba.
