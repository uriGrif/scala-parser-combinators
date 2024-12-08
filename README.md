# Parser Combinators

Trabajo practico de la materia TADP de UTN FRBA, con el objetivo de aprender nociones sobre el paradigma de programacion hibrido Objetos-Funcional, utilizando el lenguaje Scala.

## Objetivo

Definir un lenguaje de propósito específico para describir imágenes.
Luego, utilizando parser combinators, escribir un parser que permita tomar una imagen descripta en nuestro lenguaje y transformarla en un modelo intermedio que podamos manipular.
Finalmente, utiliza una API provista por la catedra que permite generar la imagen.


### Ejemplo
```python
grupo(
	escala[1.2, 1.2](
		grupo(
			color[0, 0, 80](rectangulo[0 @ 0, 600 @ 700]),
			color[255, 255, 120](circulo[80 @ 80, 50]),
			color[0, 0, 80](circulo[95 @ 80, 40])
		)
	),
	color[50, 50, 50](triangulo[80 @ 270, 520 @ 270, 300 @ 690]),
	color[80, 80, 80](triangulo[80 @ 270, 170 @ 270, 300 @ 690]),
	color[100, 100, 100](triangulo[200 @ 200, 400 @ 200, 300 @ 150]),
	color[100, 100, 100](triangulo[200 @ 200, 400 @ 200, 300 @ 400]),
	color[150, 150, 150](triangulo[400 @ 200, 300 @ 400, 420 @ 320]),
	color[150, 150, 150](triangulo[300 @ 400, 200 @ 200, 180 @ 320]),
	color[100, 100, 100](triangulo[150 @ 280, 200 @ 200, 180 @ 320]),
	color[100, 100, 100](triangulo[150 @ 280, 200 @ 200, 150 @ 120]),
	color[100, 100, 100](triangulo[400 @ 200, 450 @ 300, 420 @ 320]),
	color[100, 100, 100](triangulo[400 @ 200, 450 @ 300, 450 @ 120]),
	grupo(
		escala[0.4, 1](
			color[0, 0,0](
				grupo(
					circulo[970 @ 270, 25],
					circulo[530 @ 270, 25]
				)
			)
		)
	)
)
```

## Enunciado

[Parser Combinators](https://docs.google.com/document/d/11X2vk3FbRaArhWiy_FqtcEnJQHBGHOx1l_tkqeMxSAU/edit?tab=t.0)
