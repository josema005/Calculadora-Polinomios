# Caluculadora-Polinomios

Creado con Scheme, usando DrRacket. 
Scheme es un lenguaje de programación funcional.

El proyecto tiene como objetivo estudiar algunas de las técnicas fundamentales de programación funcional, en especial técnicas de uso de parámetros y recursividad.

Se utilizará la siguiente representacion de polinomios:
La lista: 
(0 2 3 1) 
es una representación del polinomio: 
2𝑥 + 3𝑥^2 + 𝑥^3
La lista:
(1 2 3 -4 0 5)
es una represantación del polinomio
1+ 2x + 3^2 -4x^3 + 5x^5


*Para enviar varios polinomios como parametro se usa la siguiente estructura : '((p1) (p2)......(pn))
La calculadora cumple las siguientes funciones:
-Despliegue de polinomios : Recibe una lista '(1 2 3 -4 0 5) y retorna el polinomio de la lista (1+ 2x + 3^2 -4x^3 + 5x^5).  display-p
-Suma de polinomios: Recibe n polinomios y retorna su suma.                                                                  suma-p
-Resta de polinomios: Recibe n polinomios y retorna su resta.                                                                resta-p
-Multiplicacion de polinomios: Recibe n polinomios y retorna su multiplicación.                                              multi-p
-Division de polinomios 1: Recibe 2 polinomios, retorna el cociente.
-Division de polinomios 2: Recibe 2 polinomios, retorna el residuo.
-Division de polinomios 3: Recibe 2 polinomios, retorna el cociente y el residuo.                                            divi-total
-Derivacion de polinomios: Deriva n polinomios.                                                                         deriva-polinomios
-Evaluacion de polinomios: Recibe un polinomios para evaluar en un valor x. Retorna el resultado de la evaluacion.           evalua
-Factorizacion de polinomios (Grado 2): Recibe el polinomio y lo factoriza por formula general.                              grado2         https://es.wikipedia.org/wiki/Ecuación_de_segundo_grado
-Factorizacion de polinomios (Grado 3 o superior): Recibe el polinomio y lo factoriza por division sintetica.         grado3    gradoN

Para factorizar con division sintetica los polinomios deben ser regulares.
