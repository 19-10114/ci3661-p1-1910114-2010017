# Uso de inteligencia artificial

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