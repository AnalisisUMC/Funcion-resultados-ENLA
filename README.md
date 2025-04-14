# Función resultados ENLA
Conjunto de funciones que permite obtener los resultados de aprendizaje de la ENLA 2022 en adelante considerando el diseño muestral y calculando los errores estándar de manera automática. Además, contiene funciones para generar gráficos de medida promedio y niveles de logro para un año o comparando dos años de manera automática.

# Librerías requeridas

- tidyverse
- ggplot2
- Hmisc # Permite calcular cuantiles ponderados considerando los pesos de muestreo.
- ggtext # Extiende las capacidades de personalización de texto en gráficos creados con ggplot2.
- survey # Paquete para estimar resultados con muestras complejas.
- ggh4x # Permite crear y editar textos anidados en los ejes.
- legendry # Permite perzonalizar textos y etiquetas en leyendas y ejes.
- stringi # Permite manipular caracteres especiales, como son las tildes, en cadenas de texto.

# Cargar las funciones

devtools::source_url("https://raw.githubusercontent.com/AnalisisUMC/Funcion-resultados-ENLA/refs/heads/main/funciones.R")

# ADVERTENCIA

Esta función es para uso interno de la Oficina de Medición de la Calidad de Los Aprendizajes (UMC) del Ministerio de Educación y está ajustada a la estructura de las bases de datos de rendimiento de los años 2022 en adelante.

La función "tablares( )" puede presentar errores si las bases de datos se abren con la librería "rio", debido a que esta librería asigna una clase a las variables que no es compatible con el paquete survey. Se recomienda utilizar las librerías "fst", "foreign" u "openxlsx" según sea el caso.

# DESCRIPCIÓN

## Función "tablares"
La función "tablares( )" permite obtener resultados y sus errores estandar a nivel nacional y por estratos de las evaluaciones nacionales de logro de aprendizajes (ENLA) de manera automática.

Se consideran tres argumentos: tablares(base_de_datos, area_evaluada, tipo_estrato)

Ejemplo:

- tablares(bd2p_2022, "Matemática", "Nacional")  Calcula los resultados nacionales en medida promedio y niveles de logro de 2.° grado de primaria para el área evaluada de Matemática 

- tablares(bd4p_2022, "Matemática", "Estratos")  Calcula los resultados por estratos de sexo, área, gestión, gestión y área, características y DRE en medida promedio y niveles de logro de 4.° grado de primaria para el área evaluada de Matemática 

- tablares(bd2s_2023, "Lectura", "CUALQUIER_OTRO_ESTRATO")  Calcula los resultados por el estrato que se defina en medida promedio y niveles de logro de 2.° grado de secundaria para el área evaluada de Lectura 
