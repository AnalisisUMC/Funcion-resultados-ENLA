# Función-resultados-ENLA
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
