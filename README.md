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

- tablares(bd6p_2022, "Matemática", "Nacional")  Calcula los resultados nacionales en medida promedio y niveles de logro de 6.° grado de primaria para el área evaluada de Matemática 

- tablares(bd4p_2023, "Matemática", "Estratos")  Calcula los resultados por estratos de sexo, área, gestión, gestión y área, características y DRE en medida promedio y niveles de logro de 4.° grado de primaria para el área evaluada de Matemática

- tablares(bd4p_2024,"Lectura","Regiones)  Calcula los resultados por estratos de sexo, área, gestión, gestión y área, características en medida promedio y niveles de logro en 4.° grado de primaria para cada región en el área evaluada de Lectura.

- tablares(bd2s_2023, "Lectura", "CUALQUIER_OTRO_ESTRATO")  Calcula los resultados por el estrato que se defina en medida promedio y niveles de logro de 2.° grado de secundaria para el área evaluada de Lectura 

## Función "graf_nac"
La función "graf_nac( )" genera un gráfico con los resultados de logros de aprendizaje para los resultados de la opción "Nacional" de la función tablares( ).

Se considerar un argumento: graf_nac(base_de_datos).

Ejemplo:

- graf_nac(lec4p_24_nac)

A continuación se muestra un ejemplo del gráfico que se genera con la función:

![image](https://github.com/user-attachments/assets/e94cd860-786d-4cce-9b85-3feb975076ab)

Las características para guardar este tipo de gráficos como ".png" usando la función ggsave son las siguientes:

ggsave(g1, 
filename = "02 Gráficos/g1_nac.png", 
w = 6.5, 
h = 4.5, 
dpi = 600)

## Función "graf_estrat"

La función "graf_estrat( )" genera un gráfico con los resultados de logros de aprendizaje para los resultados de la opción "Estratos" comparando con la opción "Nacional" generados por la función tablares( ).

Se consideran tres argumentos: graf_estr1(bd_nacional, bd_estratos, nom_tipo), donde:

- bd_nacional: es la base de datos de resultados a nivel nacional.
- bd_estratos: es la base de datos de resultados para los estratos calculados de manera automática.
- nom_tipo: define el tipo de gráfico que se va a generar.

A continuación se muestran los gráficos para cada una de las opciones de nom_tipo.

### nom_tipo = "Tipo 1"

![a1](https://github.com/user-attachments/assets/4ef441a3-74f8-4176-b315-dc55bd56c9cb)


