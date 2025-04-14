
# Conteo de missing en las variables #
#------------------------------------#

funmiss <- function(x) {
  
  sum(is.na(x))/length(x)*100
  
  }

# Patrón de missing #
#-------------------#

g_patron_missing <- function(data, preg){

  data[preg] %>%
    mutate(id = row_number()) %>%
    gather(-id, key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    ggplot(aes(rev(key), id, fill = isna)) +
    geom_raster(alpha = 0.8) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(size = 4.2, color = "black", hjust = 0),
          axis.text.x = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 8.5),
          legend.text = element_text(size = 8, color = "#5A5D63"),
          plot.margin = unit(c(0, 0, 0.0, 0), "cm")) +
    scale_fill_manual(name = "", values = c('gray', 'black'), labels = c("Presente", "Missing")) +
    labs(x = 'Pregunta\n', 
         y = "Observaciones", 
         title = " ",
         subtitle,
         captiomn) +
    scale_x_discrete(labels = rev(preg)) +
    coord_flip()

}

# Media pesada # 
#--------------#

weighted_mean <- function(x, w, ..., na.rm = FALSE){
  
  if(na.rm){
    
    df_omit <- na.omit(data.frame(x, w))
    
    return(weighted.mean(df_omit$x, df_omit$w, ...))
    
  } 
  
  weighted.mean(x, w, ...)
}

# Desviación estándar pesada #
#----------------------------#

weighted_sd <- function(x, weights) {
  # Eliminar valores faltantes en x y en los pesos correspondientes
  complete_cases <- complete.cases(x, weights)
  x <- x[complete_cases]
  weights <- weights[complete_cases]
  
  # Calcular la desviación estándar ponderada
  weighted_var <- sum(weights * (x - weighted.mean(x, w = weights))^2) / sum(weights)
  sqrt(weighted_var)
}

# Cálculo de la moda con pesos #
#------------------------------#

lamoda_pesos <- function(x, weights = NULL) {
  if (is.null(weights)) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
  } else {
    # Verificar que la longitud de 'x' y 'weights' sea la misma
    if (length(x) != length(weights)) {
      stop("La longitud de 'x' y 'weights' debe ser la misma.")
    }
    
    # Crear una tabla de frecuencia ponderada
    df <- data.frame(x, weights)
    df <- df[order(df$x), ]
    df <- df[!duplicated(df$x), ]
    df$weighted_freq <- ave(df$weights, df$x, FUN = sum)
    
    # Encontrar la moda basada en las frecuencias ponderadas
    moda <- df[df$weighted_freq == max(df$weighted_freq), "x"]
    moda
  }
}

# Redondear #
#-----------#

redondear<-function(x,d=0){
  (floor(x*10**d)+as.numeric((x*10**d-floor(x*10**d))>=0.5))/10**d
}

# Cambiar punto por coma decimal #
#--------------------------------#

puntocoma2<-function(x,dec=0){
  if(is.numeric(x)){
    if(length(dim(x))==2){
      gsub("\\.",",",apply(redondear(x,dec), 2, sprintf,fmt=paste0("%.",dec,"f")))
    }else{gsub("\\.",",",sprintf(paste0("%.",dec,"f"), redondear(x,dec)))}
  }else{
    if(length(dim(x))==2){
      redondear(apply(gsub(",","\\.",x),2,as.numeric),dec)
    }else{redondear(as.numeric(gsub(",","\\.",x)),dec)}
  }
  
} 

# Ver en Excel #
#--------------#

show_in_excel <- function(.data){
  tmp <- paste0(tempfile(),".xlsx")
  write.xlsx(.data,tmp)
  browseURL(url=tmp)
}

# Contar NA en las filas #
#------------------------#

rowSumsNA <- function(x) rowSums(is.na(x)) 

# Acortar oraciones en variables tipo factor #
#--------------------------------------------#

str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

# Colores UMC #
#-------------#

color_pre_in <- '#A5A5A5' # Color "Previo al inicio"
color_inicio <- '#A74D4B' # Color "En inicio"
color_proces <- '#F79646' # Color "En proceso"
color_satisf <- '#9BBB59' # Color "Satisfactorio"

# Duplicar texto del "eje Y" hacia la derecha del gráfico #
#---------------------------------------------------------#

guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

# Crear percentiles pesados #
#---------------------------#

percentil_umc <- function(var, weights, percentil){
  
  if (percentil == "tercil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.33), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.33), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.67), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.67), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "cuartil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.25), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.25), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.75), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.75), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q4",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "quintil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q4",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q5",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "decil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.10), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.10), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.30), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.30), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q4",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q5",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q6",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.70), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q7",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.70), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q8",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.90), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q9",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.90), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q10",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "nse"){
    
    case_when({{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.35), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE muy bajo",
              {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.35), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE bajo",
              {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.85), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE medio",
              {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.85), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE alto",
    )
    
  }
  
}

# Función que calcula los resultados de rendimiento junto con sus errores estandar #
#----------------------------------------------------------------------------------#

tablares <- function(bd_datos,areacurr,alcance){

  conteo <- nchar(bd_datos %>% 
                    select(matches("^M500_.*_CT$")) %>% 
                    names()) %>% 
    as.numeric()
  
  if (conteo > 18) {
    
    #==========#
    # Censales #
    #==========#
    
    if (alcance == "Nacional") {
      
      if (areacurr == "Lectura") {
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática") {
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      } 
      
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      
      a1 <- bd_datos %>% 
        filter(!is.na(!!sym(nom2_areaeval))) %>% 
        group_by(!!sym(nom2_areaeval)) %>% 
        summarise(tot = sum(!!sym(nom_peso), na.rm = TRUE)) %>% 
        mutate(ptot = redondear(tot/sum(tot)*100,8)) %>% 
        select(-tot) %>% 
        setNames(c("tipo_esp","valor")) %>% 
        mutate(tipo_esp = case_when(
          grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
          grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
          grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
          grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
          grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
          grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
          TRUE ~ "Limpiar variable"
        ),
        area_eval = nom_area,
        estrato1_1 = alcance,
        estrato1_2 = alcance,
        estrato2 = alcance,
        error = NA) %>% 
        mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                TRUE ~ "Nivel de logro")) %>%
        mutate(año = str_sub(nom1_areaeval, -7, -4),
               grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                      nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                      nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                      nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                      TRUE ~ "Limpiar variable")) %>% 
        select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
      
      a2 <- bd_datos %>% 
        filter(!is.na(!!sym(nom2_areaeval))) %>% 
        mutate(nacional = alcance) %>% 
        group_by(nacional) %>% 
        summarise(valor = redondear(weighted_mean(!!sym(nom1_areaeval),!!sym(nom_peso),na.rm = TRUE),8)) %>% 
        setNames(c("tipo_esp","valor")) %>% 
        mutate(tipo_esp = case_when(
          grepl("^Nacional", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
          grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
          grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
          grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
          grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
          grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
          TRUE ~ "Limpiar variable"
        ),
        area_eval = nom_area,
        estrato1_1 = alcance,
        estrato1_2 = alcance,
        estrato2 = alcance,
        error = NA) %>% 
        mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                TRUE ~ "Nivel de logro")) %>%
        mutate(año = str_sub(nom1_areaeval, -7, -4),
               grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                      nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                      nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                      nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                      TRUE ~ "Limpiar variable")) %>% 
        select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
      
      return(rbind(a1,a2))
      
    } else if (alcance %in% "Estratos") {
      
      if (areacurr == "Lectura") {
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática") {
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      } 
      
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      
      nom_estratos <- c("sexo","gestion2","gestion3","area","caracteristica2","nom_dre")
      
      estratos <- list()
      
      for(i in 1:length(nom_estratos)){
        
        b1 <- bd_datos %>% 
          filter(!is.na(!!sym(nom_peso))) %>% 
          mutate_at(vars(sexo),
                    funs(as.character)) %>% 
          mutate(sexo = case_when(sexo %in% "1" ~ "Hombre",
                                  sexo %in% "2" ~ "Mujer",
                                  TRUE ~ sexo)) %>% 
          mutate(gestion2 = case_when(gestion2 %in% c("Estatal","Público") ~ "Pública",
                                      gestion2 %in% c("No estatal","Privado") ~ "Privada",
                                      TRUE ~ gestion2)) %>%
          mutate(gestion3 = case_when(area %in% "Urbana" & gestion2 %in% "Pública" ~ "Pública urbana",
                                      gestion2 %in% "Privada" ~ "Privada",
                                      TRUE ~ NA_character_))
        
        a1 <- b1 %>% 
          filter(!is.na(!!sym(nom2_areaeval))) %>% 
          filter(!is.na(!!sym(nom_estratos[[i]]))) %>% 
          group_by(!!sym(nom_estratos[[i]]),!!sym(nom2_areaeval)) %>% 
          summarise(tot = sum(!!sym(nom_peso), na.rm = TRUE)) %>% 
          mutate(ptot = redondear(tot/sum(tot)*100,8)) %>% 
          select(-tot) %>% 
          setNames(c("estrato2","tipo_esp","valor")) %>% 
          mutate(tipo_esp = case_when(
            grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
            grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
            grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
            grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
            grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
            grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
            TRUE ~ "Limpiar variable"
          ),
          area_eval = nom_area,
          estrato1_1 = nom_estratos[[i]],
          estrato1_2 = nom_estratos[[i]],
          error = NA) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>%
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
        
        a2 <- b1 %>% 
          filter(!is.na(!!sym(nom2_areaeval))) %>% 
          filter(!is.na(!!sym(nom_estratos[[i]]))) %>%
          group_by(!!sym(nom_estratos[[i]])) %>% 
          summarise(valor = redondear(weighted_mean(!!sym(nom1_areaeval), !!sym(nom_peso), na.rm = T),8)) %>% 
          setNames(c("estrato2","valor")) %>% 
          mutate(tipo_esp = "Medida promedio",
                 area_eval = nom_area,
                 estrato1_1 = nom_estratos[[i]],
                 estrato1_2 = nom_estratos[[i]],
                 error = NA) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>%
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
        
        estratos[[i]] <- rbind(a1,a2)
        
      }
      
      return(bind_rows(estratos) %>% 
               filter(
                 !(grado_eval %in% "2.° grado de secundaria" & estrato1_1 %in% "caracteristica2")
               ))
      
    } else if (alcance == "Regiones"){
      
      if (areacurr == "Lectura"){
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática"){
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      } 
      
      # Variables de identificación #
      #-----------------------------#
      
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      
      nom_estratos <- c("sexo","gestion2","gestion3","area","caracteristica2","nom_ugel")
      
      # Base con identificador de regiones y ugeles uniformizado #
      #----------------------------------------------------------#
      
      bd_region <- bd_datos %>% 
        mutate_at(vars(nom_dre,nom_ugel,area),
                  ~str_trim(., side = "right"))
      
      # Vector con nombre de regiones #
      #-------------------------------#
      
      etiq_reg <- unique(bd_region$nom_dre)
      
      # Listas para guardar resultados #
      #--------------------------------#
      
      resul_reg <- list()
      
      estratos <- list()
      
      # Loop para el cálculo de resultados por estratos para cada región #
      #------------------------------------------------------------------#
      
      for (r in 1:length(etiq_reg)) {  # Iterar sobre cada región
        
        for(i in 1:length(nom_estratos)){ # Iterar sobre cada estrato
          
          b1 <- bd_region %>% 
            filter(nom_dre %in% etiq_reg[[r]]) %>% 
            filter(!is.na(!!sym(nom_peso))) %>% 
            mutate_at(vars(sexo),
                      funs(as.character)) %>% 
            mutate(sexo = case_when(sexo %in% "1" ~ "Hombre",
                                    sexo %in% "2" ~ "Mujer",
                                    TRUE ~ sexo)) %>% 
            mutate(gestion2 = case_when(gestion2 %in% c("Estatal","Público") ~ "Pública",
                                        gestion2 %in% c("No estatal","Privado") ~ "Privada",
                                        TRUE ~ gestion2)) %>%
            mutate(gestion3 = case_when(area %in% "Urbana" & gestion2 %in% "Pública" ~ "Pública urbana",
                                        gestion2 %in% "Privada" ~ "Privada",
                                        TRUE ~ NA_character_))
          
          a1 <- b1 %>% 
            filter(!is.na(!!sym(nom2_areaeval))) %>% 
            filter(!is.na(!!sym(nom_estratos[[i]]))) %>% 
            group_by(!!sym(nom_estratos[[i]]),!!sym(nom2_areaeval)) %>% 
            summarise(tot = sum(!!sym(nom_peso), na.rm = TRUE)) %>% 
            mutate(ptot = redondear(tot/sum(tot)*100,8)) %>% 
            select(-tot) %>% 
            setNames(c("estrato2","tipo_esp","valor")) %>% 
            mutate(tipo_esp = case_when(
              grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
              grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
              grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
              grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
              grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
              grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
              TRUE ~ "Limpiar variable"
            ),
            area_eval = nom_area,
            estrato1_1 = etiq_reg[[r]],
            estrato1_2 = nom_estratos[[i]],
            error = NA) %>% 
            mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                    TRUE ~ "Nivel de logro")) %>%
            mutate(año = str_sub(nom1_areaeval, -7, -4),
                   grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                          nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                          nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                          nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                          TRUE ~ "Limpiar variable")) %>% 
            select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error) %>% 
            mutate(estrato1_2 = case_when(estrato1_2 %in% "sexo" ~ "Sexo",
                                          estrato1_2 %in% "gestion2" ~ "Gestión",
                                          estrato1_2 %in% "gestion3" ~ "Gestión y área",
                                          estrato1_2 %in% "area" ~ "Área",
                                          estrato1_2 %in% "caracteristica2" ~ "Característica",
                                          estrato1_2 %in% "nom_ugel" ~ "UGEL",
                                          TRUE ~ NA_character_))
          
          a2 <- b1 %>% 
            filter(!is.na(!!sym(nom2_areaeval))) %>% 
            filter(!is.na(!!sym(nom_estratos[[i]]))) %>%
            group_by(!!sym(nom_estratos[[i]])) %>% 
            summarise(valor = redondear(weighted_mean(!!sym(nom1_areaeval), !!sym(nom_peso), na.rm = T),8)) %>% 
            setNames(c("estrato2","valor")) %>% 
            mutate(tipo_esp = "Medida promedio",
                   area_eval = nom_area,
                   estrato1_1 = etiq_reg[[r]],
                   estrato1_2 = nom_estratos[[i]],
                   error = NA) %>% 
            mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                    TRUE ~ "Nivel de logro")) %>%
            mutate(año = str_sub(nom1_areaeval, -7, -4),
                   grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                          nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                          nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                          nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                          TRUE ~ "Limpiar variable")) %>%
            select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error) %>% 
            mutate(estrato1_2 = case_when(estrato1_2 %in% "sexo" ~ "Sexo",
                                          estrato1_2 %in% "gestion2" ~ "Gestión",
                                          estrato1_2 %in% "gestion3" ~ "Gestión y área",
                                          estrato1_2 %in% "area" ~ "Área",
                                          estrato1_2 %in% "caracteristica2" ~ "Característica",
                                          estrato1_2 %in% "nom_ugel" ~ "UGEL",
                                          TRUE ~ NA_character_))
          
          estratos[[i]] <- rbind(a1,a2) 

        }
        
        resul_reg[[r]] <- bind_rows(estratos)
        
      }
      
      return(bind_rows(resul_reg) %>% 
        filter(
          !(estrato1_1 %in% "Lima Metropolitana" & estrato2 %in% "Rural")
        ))
      
    } else {
      
      if (areacurr == "Lectura") {
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática") {
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      } 
      
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      
      nom_estratos <- c(alcance)
      
      estratos <- list()
      
      for(i in 1:length(nom_estratos)){
        
        b1 <- bd_datos %>% 
          filter(!is.na(!!sym(nom_peso))) %>% 
          mutate_at(vars(sexo),
                    funs(as.character)) %>% 
          mutate(sexo = case_when(sexo %in% "1" ~ "Hombre",
                                  sexo %in% "2" ~ "Mujer",
                                  TRUE ~ sexo))
        
        a1 <- b1 %>% 
          filter(!is.na(!!sym(nom2_areaeval))) %>% 
          filter(!is.na(!!sym(nom_estratos[[i]]))) %>% 
          group_by(!!sym(nom_estratos[[i]]),!!sym(nom2_areaeval)) %>% 
          summarise(tot = sum(!!sym(nom_peso), na.rm = TRUE)) %>% 
          mutate(ptot = redondear(tot/sum(tot)*100,8)) %>% 
          select(-tot) %>% 
          setNames(c("estrato2","tipo_esp","valor")) %>% 
          mutate(tipo_esp = case_when(
            grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
            grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
            grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
            grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
            grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
            grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
            TRUE ~ "Limpiar variable"
          ),
          area_eval = nom_area,
          estrato1_1 = nom_estratos[[i]],
          estrato1_2 = nom_estratos[[i]],
          error = NA) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>%
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
        
        a2 <- b1 %>% 
          filter(!is.na(!!sym(nom2_areaeval))) %>% 
          mutate(nacional = "Nacional") %>% 
          filter(!is.na(!!sym(nom_estratos[[i]]))) %>% 
          group_by(!!sym(nom_estratos[[i]])) %>% 
          summarise(valor = redondear(weighted_mean(!!sym(nom1_areaeval),!!sym(nom_peso)),8)) %>% 
          setNames(c("estrato2","valor")) %>% 
          mutate(tipo_esp = "Medida promedio",
                 area_eval = nom_area,
                 estrato1_1 = nom_estratos[[i]],
                 estrato1_2 = nom_estratos[[i]],
                 error = NA) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>%
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
        
        estratos[[i]] <- rbind(a1,a2)
        
      }
      
      return(bind_rows(estratos) %>% 
               filter(
                 !(grado_eval %in% "2.° grado de secundaria" & estrato1_1 %in% "caracteristica2")
               ))
      
    }
    
  } else { 
    
    #============#
    # Muestrales #
    #============#
    
    # Filtros por área evaluada #
    #---------------------------#
    
    if (alcance == "Nacional") {
      
      if (areacurr == "Lectura") {
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom_pikIE <- "pikIE_L"
        nom_prob_sec <- "prob_sec_L" 
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática") {
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom_pikIE <- "pikIE_M"
        nom_prob_sec <- "prob_sec_M"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      } 
      
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      nom_año <- str_sub(nom1_areaeval, -7, -4)
      
      bd1 <- bd_datos %>% 
        mutate(cod_mod8 = paste0(cod_mod7,anexo)) %>%
        filter(!is.na(!!sym(nom_peso)))
      
      nom_strata <- bd1 %>% 
        select(starts_with("Estrato")) %>% 
        names()
      
      if (!nom_año %in% "2022"){
        
        # Diseño muestral #
        #-----------------#
        options(survey.lonely.psu = "certainty")
        
        dmuestral <- svydesign(id=~cod_mod8,
                               strata=as.formula(paste0("~", nom_strata)),
                               nest=TRUE,
                               weights=as.formula(paste0("~", nom_peso)),
                               data=bd1,
                               pps = "brewer")
        
      } else if (nom_año %in% "2022"){
        
        # Diseño muestral #
        #-----------------#
        options(survey.lonely.psu = "certainty")
        
        dmuestral <- svydesign(id=~cod_mod8+ID_SECCION,
                               strata=as.formula(paste0("~", nom_strata)),
                               nest=TRUE,
                               fpc=as.formula(paste0("~", nom_pikIE,"+",nom_prob_sec)),
                               data=bd1,
                               pps='brewer')
      }
      
      # Cálculo de resultados #
      #-----------------------#
      
      nom_vector <- c(nom1_areaeval,nom2_areaeval)
      
      nacional <- list()
      
      for (i in 1:length(nom_vector)) {
        
        nacional[[i]] <- lapply(nom_vector[[i]], function(var) {
          
          formula <- as.formula(paste0("~", var))  # Crear la fórmula
          
          resultado <- svymean(formula,
                               design = dmuestral, 
                               na.rm = TRUE) %>% 
            as.data.frame() %>% 
            rownames_to_column(var = "tipo_esp") %>%
            mutate(tipo_esp = case_when(
              grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
              grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
              grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
              grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
              grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
              grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
              TRUE ~ "Limpiar variable"
            ),
            area_eval = nom_area,
            estrato1_1 = alcance,
            estrato1_2 = alcance,
            estrato2 = alcance) %>% 
            mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                    TRUE ~ "Nivel de logro")) %>%
            mutate(año = str_sub(nom1_areaeval, -7, -4),
                   grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                          nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                          nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                          nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                          TRUE ~ "Limpiar variable")) %>% 
            setNames(c("tipo_esp", "valor", "error","area_eval","estrato1_1","estrato1_2","estrato2","tipo","año","grado_eval"))  # Renombrar columnas
          
          # Aquí aplicamos el redondeo basado en el índice fuera del `lapply`
          
          if (i == 1) {
            
            # Si es medida promedio, redondeamos "valor" a 0 decimales
            
            resultado <- resultado %>% 
              mutate(valor = redondear(valor, 8), 
                     error = redondear(error, 8))
            
          } else if (i == 2) {
            # Si es nivel de logro, redondeamos "valor" multiplicado por 100 a 1 decimal
            resultado <- resultado %>% 
              mutate(valor = redondear(valor*100, 8), 
                     error = redondear(error*100, 8))
          }
          
        })
        
      }  
      
      bd_int <- bind_rows(nacional) %>% 
        select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
      
      return(bd_int)
      
    } else if (alcance == "Estratos") {
      
      if (areacurr == "Lectura") {
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom_pikIE <- "pikIE_L"
        nom_prob_sec <- "prob_sec_L" 
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática") {
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom_pikIE <- "pikIE_M"
        nom_prob_sec <- "prob_sec_M" 
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      }
      
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      nom_año <- str_sub(nom1_areaeval, -7, -4)
      
      bd1 <- bd_datos %>% 
        mutate(cod_mod8 = paste0(cod_mod7,anexo)) %>%
        filter(!is.na(!!sym(nom_peso))) %>% 
        mutate_at(vars(sexo),
                  funs(as.character)) %>% 
        mutate(sexo = case_when(sexo %in% "1" ~ "Hombre",
                                sexo %in% "2" ~ "Mujer",
                                sexo %in% c(" ","N") ~ NA_character_,
                                TRUE ~ sexo)) %>% 
        filter(!is.na(sexo)) %>% 
        mutate(gestion2 = case_when(gestion2 %in% c("Estatal","Público") ~ "Pública",
                                    gestion2 %in% c("No estatal","Privado") ~ "Privada",
                                    TRUE ~ gestion2)) %>% 
        mutate(gestion3 = case_when(area %in% "Urbana" & gestion2 %in% "Pública" ~ "Pública urbana",
                                    gestion2 %in% "Privada" ~ "Privada",
                                    TRUE ~ NA_character_))
      
      nom_strata <- bd1 %>% 
        select(starts_with("Estrato")) %>% 
        names()
      
      # Diseño muestral #
      #-----------------#
      
      options(survey.lonely.psu = "certainty")
      
      if (!nom_año %in% "2022"){
        
        dmuestral <- svydesign(id=~cod_mod8,
                               strata=as.formula(paste0("~", nom_strata)),
                               nest=TRUE,
                               weights=as.formula(paste0("~", nom_peso)),
                               data=bd1,
                               pps = "brewer")
        
      } else if (nom_año %in% "2022") {
        
        dmuestral <- svydesign(id=~cod_mod8+ID_SECCION,
                               strata=as.formula(paste0("~", nom_strata)),
                               nest=TRUE,
                               fpc=as.formula(paste0("~", nom_pikIE,"+",nom_prob_sec)),
                               data=bd1,
                               pps='brewer')
      }
      
      # Cálculo de resultados #
      #-----------------------#
      
      nom_estratos <- c("sexo","gestion2","gestion3","area","caracteristica2","nom_dre")
      
      estratos <- list()
      
      for(i in 1:length(nom_estratos)){
        
        a1 <- svyby(
          as.formula(paste0("~",nom1_areaeval)),
          as.formula(paste0("~",nom_estratos[[i]])),
          design = dmuestral,
          svymean,
          na.rm = TRUE
        ) %>% 
          setNames(c("estrato2","valor","error")) %>% 
          mutate(valor = redondear(valor,8),
                 error = redondear(error,8)) %>% 
          mutate(estrato1_1 = nom_estratos[[i]],
                 estrato1_2 = nom_estratos[[i]],
                 tipo_esp = "Medida promedio",
                 area_eval = nom_area) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>%
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error) %>% 
          filter(!(grado_eval %in% "2.° grado de primaria" & año %in% c("2018","2023") & estrato1_1 %in% "nom_dre")) %>%
          filter(!(grado_eval %in% "6.° grado de primaria" & año %in% c("2022") & estrato1_1 %in% "nom_dre"))       
        
        a2 <- svyby(
          as.formula(paste0("~",nom2_areaeval)),
          as.formula(paste0("~",nom_estratos[[i]])),
          design = dmuestral,
          svymean,
          na.rm = TRUE
        ) %>% 
          pivot_longer(
            cols = -!!sym(nom_estratos[[i]]),                       # Todas las columnas excepto "sexo"
            names_to = c("categoria", "tipo_esp"),                  # Crear dos columnas: "categoria" y "niveles"
            names_pattern = "(se)?\\.?(.+)",                        # Captura el prefijo "se" opcionalmente
            values_drop_na = TRUE                                   # Opcional: elimina valores NA si existen
          ) %>% 
          mutate(categoria = case_when(categoria %in% "se" ~ "error",
                                       TRUE ~ "valor")) %>%        # Asigna "valor" cuando "categoria" no tiene contenido
          pivot_wider(
            names_from = categoria,
            values_from = value
          ) %>% 
          mutate_at(vars(valor,error),
                    ~redondear(.*100,8)) %>% 
          mutate(tipo_esp = case_when(
            grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
            grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
            grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
            grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
            grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
            grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
            TRUE ~ "Limpiar variable"
          )) %>% 
          setNames(c("estrato2","tipo_esp","valor","error")) %>% 
          mutate(estrato1_1 = nom_estratos[[i]],
                 estrato1_2 = nom_estratos[[i]],
                 area_eval = nom_area) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>% 
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error) %>% 
          filter(!(grado_eval %in% "2.° grado de primaria" & año %in% c("2018","2023") & estrato1_1 %in% "nom_dre")) %>%
          filter(!(grado_eval %in% "6.° grado de primaria" & año %in% c("2022") & estrato1_1 %in% "nom_dre")) 
        
        
        estratos[[i]] <- rbind(a1,a2)
        
      }
      
      return(bind_rows(estratos) %>% 
               filter(
                 !(grado_eval %in% "2.° grado de secundaria" & estrato1_1 %in% "caracteristica2")
               ))
      
      
    } else if (alcance == "Regiones"){
      
      if (areacurr == "Lectura"){
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom_pikIE <- "pikIE_L"
        nom_prob_sec <- "prob_sec_L" 
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática"){
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom_pikIE <- "pikIE_M"
        nom_prob_sec <- "prob_sec_L" 
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      } 
      
      # Variables de identificación #
      #-----------------------------#
      
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      
      nom_año <- str_sub(nom1_areaeval, -7, -4)
      
      nom_estratos <- c("sexo","gestion2","gestion3","area", "caracteristica2")
      
      # Base con identificador de regiones y ugeles uniformizado #
      #----------------------------------------------------------#
      
      bd1 <- bd_datos %>% 
        mutate(cod_mod8 = paste0(cod_mod7,anexo)) %>%
        mutate_at(vars(nom_dre,nom_ugel,area),
                  ~str_trim(., side = "right")) %>% 
        mutate_at(vars(sexo),
                  funs(as.character)) %>% 
        mutate(sexo = case_when(sexo %in% "1" ~ "Hombre",
                                sexo %in% "2" ~ "Mujer",
                                TRUE ~ sexo)) %>% 
        mutate(gestion2 = case_when(gestion2 %in% c("Estatal","Público") ~ "Pública",
                                    gestion2 %in% c("No estatal","Privado") ~ "Privada",
                                    TRUE ~ gestion2)) %>%
        mutate(gestion3 = case_when(area %in% "Urbana" & gestion2 %in% "Pública" ~ "Pública urbana",
                                    gestion2 %in% "Privada" ~ "Privada",
                                    TRUE ~ NA_character_))
      
      # Vector con nombre de regiones #
      #-------------------------------#
      
      etiq_reg <- unique(bd1$nom_dre)
      
      # Identificador de representatividad de la muestra #
      #--------------------------------------------------#
      
      nom_strata <- bd1 %>% 
        select(starts_with("Estrato")) %>% 
        names()
      
      # Listas para guardar resultados #
      #--------------------------------#
      
      resul_reg <- list()
      
      estratos <- list()
      
      # Loop para el cálculo de resultados por estratos para cada región #
      #------------------------------------------------------------------#
      
      for (r in 1:length(etiq_reg)) {  # Iterar sobre cada región
        
        for(i in 1:length(nom_estratos)){ # Iterar sobre cada estrato
          
          bd_region <- bd1 %>% 
            filter(nom_dre %in% etiq_reg[[r]]) %>% 
            filter(!is.na(!!sym(nom_peso)))
          
          # Diseño muestral #
          #-----------------#
          
          options(survey.lonely.psu = "certainty")
          
          if (!nom_año %in% "2022"){
            
            dmuestral <- svydesign(id=~cod_mod8,
                                   strata=as.formula(paste0("~",nom_strata)),
                                   nest=TRUE,
                                   weights=as.formula(paste0("~",nom_peso)),
                                   data=bd_region,
                                   pps = "brewer")
            
          } else if (nom_año %in% "2022") {
            
            dmuestral <- svydesign(id=~cod_mod8+ID_SECCION,
                                   strata=as.formula(paste0("~", nom_strata)),
                                   nest=TRUE,
                                   fpc=as.formula(paste0("~", nom_pikIE,"+",nom_prob_sec)),
                                   data=bd_region,
                                   pps='brewer')
          }
          
          # Cálculo medida promedio #
          #-------------------------#
          
          a1 <- svyby(
            as.formula(paste0("~",nom1_areaeval)),
            as.formula(paste0("~",nom_estratos[[i]])),
            design = dmuestral,
            svymean,
            na.rm = TRUE
          ) %>% 
            setNames(c("estrato2","valor","error")) %>% 
            mutate(valor = redondear(valor,8),
                   error = redondear(error,8)) %>% 
            mutate(estrato1_1 = etiq_reg[[r]],
                   estrato1_2 = nom_estratos[[i]],
                   tipo_esp = "Medida promedio",
                   area_eval = nom_area) %>% 
            mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                    TRUE ~ "Nivel de logro")) %>%
            mutate(año = str_sub(nom1_areaeval, -7, -4),
                   grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                          nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                          nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                          nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                          TRUE ~ "Limpiar variable")) %>% 
            select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error) %>% 
            mutate(estrato1_2 = case_when(estrato1_2 %in% "sexo" ~ "Sexo",
                                          estrato1_2 %in% "gestion2" ~ "Gestión",
                                          estrato1_2 %in% "gestion3" ~ "Gestión y área",
                                          estrato1_2 %in% "area" ~ "Área",
                                          estrato1_2 %in% "caracteristica2" ~ "Característica",
                                          TRUE ~ NA_character_))
          
          # Cálculo niveles de logro #
          #--------------------------#
          
          a2 <- svyby(
            as.formula(paste0("~",nom2_areaeval)),
            as.formula(paste0("~",nom_estratos[[i]])),
            design = dmuestral,
            svymean,
            na.rm = TRUE
          ) %>% 
            pivot_longer(
              cols = -!!sym(nom_estratos[[i]]),                       # Todas las columnas excepto "sexo"
              names_to = c("categoria", "tipo_esp"),                  # Crear dos columnas: "categoria" y "niveles"
              names_pattern = "(se)?\\.?(.+)",                        # Captura el prefijo "se" opcionalmente
              values_drop_na = TRUE                                   # Opcional: elimina valores NA si existen
            ) %>% 
            mutate(categoria = case_when(categoria %in% "se" ~ "error",
                                         TRUE ~ "valor")) %>%        # Asigna "valor" cuando "categoria" no tiene contenido
            pivot_wider(
              names_from = categoria,
              values_from = value
            ) %>% 
            mutate_at(vars(valor,error),
                      ~redondear(.*100,8)) %>% 
            mutate(tipo_esp = case_when(
              grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
              grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
              grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
              grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
              grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
              grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
              TRUE ~ "Limpiar variable"
            )) %>% 
            setNames(c("estrato2","tipo_esp","valor","error")) %>% 
            mutate(estrato1_1 = etiq_reg[[r]],
                   estrato1_2 = nom_estratos[[i]],
                   area_eval = nom_area) %>% 
            mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                    TRUE ~ "Nivel de logro")) %>% 
            mutate(año = str_sub(nom1_areaeval, -7, -4),
                   grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                          nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                          nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                          nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                          TRUE ~ "Limpiar variable")) %>% 
            select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error) %>% 
            mutate(estrato1_2 = case_when(estrato1_2 %in% "sexo" ~ "Sexo",
                                          estrato1_2 %in% "gestion2" ~ "Gestión",
                                          estrato1_2 %in% "gestion3" ~ "Gestión y área",
                                          estrato1_2 %in% "area" ~ "Área",
                                          estrato1_2 %in% "caracteristica2" ~ "Característica",
                                          TRUE ~ NA_character_))
    
          estratos[[i]] <- rbind(a1,a2)
          
        }
        
        resul_reg[[r]] <- bind_rows(estratos)
        
      }
      
      return(bind_rows(resul_reg) %>% 
               filter(
                 !(estrato1_1 %in% "Lima Metropolitana" & estrato2 %in% "Rural")
               ) %>% 
               filter(
                 !(grado_eval %in% "2.° grado de secundaria" & estrato1_1 %in% "caracteristica2")
               ) %>% 
               filter(
                 !(grado_eval %in% "2.° grado de primaria" & año %in% c("2018","2023"))
               ) %>% 
               filter(
                 !(grado_eval %in% "6.° grado de primaria" & año %in% c("2022"))
               )
             )
      
    } else {
      
      if (areacurr == "Lectura") {
        
        nom_peso <- "peso_CT"
        nom_area <- "Lectura"
        nom_pikIE <- "pikIE_L"
        nom_prob_sec <- "prob_sec_L" 
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CT$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CT$")) %>% 
          names()
        
      } else if (areacurr == "Matemática") {
        
        nom_peso <- "peso_MA"
        nom_area <- "Matemática"
        nom_pikIE <- "pikIE_M"
        nom_prob_sec <- "prob_sec_M" 
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_MA$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_MA$")) %>% 
          names()
        
      } else if (areacurr == "Ciencias sociales") {
        
        nom_peso <- "peso_CS"
        nom_area <- "Ciencias sociales"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CS$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CS$")) %>% 
          names()
        
      } else if (areacurr == "Ciencia y tecnología") {
        
        nom_peso <- "peso_CN"
        nom_area <- "Ciencia y tecnología"
        nom1_areaeval <- bd_datos %>% 
          select(matches("^M500_.*_CN$")) %>% 
          names()
        nom2_areaeval <- bd_datos %>% 
          select(matches("^grupo_.*_CN$")) %>% 
          names()
        
      }
      
      nom_estratos <- c(alcance)
      nom_gradoeval <- str_sub(nom1_areaeval, -10, -9)
      nom_año <- str_sub(nom1_areaeval, -7, -4)
      
      bd1 <- bd_datos %>% 
        mutate(cod_mod8 = paste0(cod_mod7,anexo)) %>%
        filter(!is.na(!!sym(nom_peso))) %>% 
        mutate_at(vars(sexo),
                  funs(as.character)) %>% 
        mutate(sexo = case_when(sexo %in% "1" ~ "Hombre",
                                sexo %in% "2" ~ "Mujer",
                                TRUE ~ sexo)) %>%
        filter(!is.na(nom_estratos)) %>% 
        mutate(gestion2 = case_when(gestion2 %in% c("Estatal","Público") ~ "Pública",
                                    gestion2 %in% c("No estatal","Privado") ~ "Privada",
                                    TRUE ~ gestion2)) %>% 
        mutate(gestion3 = case_when(area %in% "Urbana" & gestion2 %in% c("Pública")  ~ "Pública urbana",
                                    gestion2 %in% c("Privada") ~ "Privada",
                                    TRUE ~ NA_character_))
      
      nom_strata <- bd1 %>% 
        select(starts_with("Estrato")) %>% 
        names()
      
      # Diseño muestral #
      #-----------------#
      
      options(survey.lonely.psu = "certainty")
      
      if (!nom_año %in% "2022"){
        
        dmuestral <- svydesign(id=~cod_mod8,
                               strata=as.formula(paste0("~", nom_strata)),
                               nest=TRUE,
                               weights=as.formula(paste0("~", nom_peso)),
                               data=bd1,
                               pps = "brewer")
        
      } else if (nom_año %in% "2022") {
        
        dmuestral <- svydesign(id=~cod_mod8+ID_SECCION,
                               strata=as.formula(paste0("~", nom_strata)),
                               nest=TRUE,
                               fpc=as.formula(paste0("~", nom_pikIE,"+",nom_prob_sec)),
                               data=bd1,
                               pps='brewer')
      }
      
      # Cálculo de resultados #
      #-----------------------#
      
      estratos <- list()
      
      for(i in 1:length(nom_estratos)){
        
        a1 <- svyby(
          as.formula(paste0("~",nom1_areaeval)),
          as.formula(paste0("~",nom_estratos[[i]])),
          design = dmuestral,
          svymean,
          na.rm = TRUE
        ) %>% 
          setNames(c("estrato2","valor","error")) %>% 
          mutate(valor = redondear(valor,8),
                 error = redondear(error,8)) %>% 
          mutate(estrato1_1 = nom_estratos[[i]],
                 estrato1_2 = nom_estratos[[i]],
                 tipo_esp = "Medida promedio",
                 area_eval = nom_area) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>%
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
        
        
        a2 <- svyby(
          as.formula(paste0("~",nom2_areaeval)),
          as.formula(paste0("~",nom_estratos[[i]])),
          design = dmuestral,
          svymean,
          na.rm = TRUE
        ) %>% 
          pivot_longer(
            cols = -!!sym(nom_estratos[[i]]),                                     # Todas las columnas excepto "sexo"
            names_to = c("categoria", "tipo_esp"),                  # Crear dos columnas: "categoria" y "niveles"
            names_pattern = "(se)?\\.?(.+)",                  # Captura el prefijo "se" opcionalmente
            values_drop_na = TRUE                             # Opcional: elimina valores NA si existen
          ) %>% 
          mutate(categoria = case_when(categoria %in% "se" ~ "error",
                                       TRUE ~ "valor")) %>%        # Asigna "valor" cuando "categoria" no tiene contenido
          pivot_wider(
            names_from = categoria,
            values_from = value
          ) %>% 
          mutate_at(vars(valor,error),
                    ~redondear(.*100,8)) %>% 
          mutate(tipo_esp = case_when(
            grepl("^M500_", tipo_esp) ~ "Medida promedio",                    # Usar grepl para verificar si comienza con "M500_"
            grepl("Previo al inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al inicio"
            grepl("Previo al Inicio$", tipo_esp) ~ "Previo al inicio",        # Usar grepl para verificar si termina con "Previo al Inicio"
            grepl("En inicio$", tipo_esp) ~ "En inicio",                      # Usar grepl para verificar si termina con "En inicio"
            grepl("En proceso$", tipo_esp) ~ "En proceso",                    # Usar grepl para verificar si termina con "En proceso"
            grepl("Satisfactorio$", tipo_esp) ~ "Satisfactorio",              # Usar grepl para verificar si termina con "Satisfactorio"
            TRUE ~ "Limpiar variable"
          )) %>% 
          setNames(c("estrato2","tipo_esp","valor","error")) %>% 
          mutate(estrato1_1 = nom_estratos[[i]],
                 estrato1_2 = nom_estratos[[i]],
                 area_eval = nom_area) %>% 
          mutate(tipo = case_when(tipo_esp %in% "Medida promedio" ~ "Medida promedio",
                                  TRUE ~ "Nivel de logro")) %>% 
          mutate(año = str_sub(nom1_areaeval, -7, -4),
                 grado_eval = case_when(nom_gradoeval %in% "2P" ~ "2.° grado de primaria",
                                        nom_gradoeval %in% "4P" ~ "4.° grado de primaria",
                                        nom_gradoeval %in% "6P" ~ "6.° grado de primaria",
                                        nom_gradoeval %in% "2S" ~ "2.° grado de secundaria",
                                        TRUE ~ "Limpiar variable")) %>% 
          select(grado_eval,area_eval,año,estrato1_1,estrato1_2,estrato2,tipo,tipo_esp,valor,error)
        
        
        estratos[[i]] <- rbind(a1,a2)
        
      }
      
      return(bind_rows(estratos) %>% 
               filter(
                 !(grado_eval %in% "2.° grado de secundaria" & estrato1_1 %in% "caracteristica2")
               ))
    }
    
  }
  
}

# Función que genera los gráficos tradicionales para las tablas de resultados nacionales #
#----------------------------------------------------------------------------------------#

graf_nac <- function(bd_datos){
  
  nom_grado <- unique(bd_datos$grado_eval)
  nom_area <- unique(bd_datos$area_eval)
  
  totniv <- bd_datos %>% 
    filter(tipo %in% "Nivel de logro") %>% 
    arrange(tipo_esp) %>% 
    select(tipo_esp) %>% 
    pull() %>% 
    length()
  
  bd1 <- bd_datos %>% 
    filter(tipo %in% "Nivel de logro") %>% 
    select(estrato2 ,tipo_esp,valor)
  
  if (totniv == 3) {
    
    # Cálculo de las posiciones de las etiquetas #
    #--------------------------------------------#
    
    pos <- bd1 %>% 
      pivot_wider(names_from = tipo_esp,
                  values_from = valor) %>% 
      rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
      mutate(pos_sa=case_when(n_sa >= 12 ~  n_sa/2,
                              TRUE ~ n_sa + 11),
             pos_pr=n_pr/2,
             pos_in=case_when(n_in >= 12 ~ n_pr+n_in/2,
                              TRUE ~ n_pr+n_in/2 + 11)) %>% 
      mutate(pos_pr=pos_pr*(-1),
             pos_in=pos_in*(-1)) %>% 
      pivot_longer(cols = 5:7,
                   names_to = "nivel",
                   values_to = "posit") %>% 
      mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                  nivel %in% "pos_pr" ~ "En proceso",
                                  TRUE ~ "Satisfactorio")) %>% 
      select(estrato2,tipo_esp,posit)
    
    # Cálculo de la medida promedio #
    #-------------------------------#
    
    mp_etiq <- bd_datos %>% 
      filter(!tipo %in% "Nivel de logro") %>% 
      select(estrato2 ,tipo_esp,valor)
    
    # Integración de las bases de logros y posición de etiquetas #
    #------------------------------------------------------------#
    
    niveles <- bd1 %>% 
      left_join(pos, by = c("estrato2","tipo_esp")) %>% 
      mutate(etiq = puntocoma2(valor,1),
             tipo_esp=factor(tipo_esp,
                             levels=c("En inicio",
                                      "En proceso",
                                      "Satisfactorio"),
                             labels=c("En inicio",
                                      "En proceso",
                                      "Satisfactorio")),
             valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                               TRUE ~ valor))
    
    ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) + 
      geom_col(width = 0.17) +
      geom_segment(aes(x=0.8,
                       xend=1.2,
                       y=0, 
                       yend=0), 
                   color="#252525",
                   lwd = 0.2) +
      scale_y_continuous(limits = c(-100,100)) + 
      scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) + 
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12,
                                       color = "#252525"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,
                                       color = "#252525")
      ) + 
      geom_text(aes(label = etiq,
                    y = posit),
                size = 4.5,
                color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                  TRUE ~ "#252525")) +
      geom_text(data = mp_etiq, 
                aes(label = puntocoma2(valor,0), 
                    y = 90, 
                    x = 1), 
                size = 4.0, 
                inherit.aes = FALSE) +
      geom_text(aes(x=0.69,
                    y=0.00,
                    label="Niveles de logro (%)"),
                size=3.0,
                alpha=0.2,
                angle = 90,
                color = "#252525") +
      geom_text(mapping=aes(x=0.70,
                            y=90,
                            label="Medida \npromedio"),
                size=3.0,
                alpha=0.2,
                color = "#252525")
    
  } else if (totniv == 4) {
    
    # Cálculo de las posiciones de las etiquetas #
    #--------------------------------------------#
    
    pos <- bd1 %>% 
      pivot_wider(names_from = tipo_esp,
                  values_from = valor) %>% 
      rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
      mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                TRUE ~ n_sa + 11),
             pos_pr = n_pr/2,
             pos_in = n_pr + n_in/2,
             pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                TRUE ~ n_pr + n_in + n_pi + 11)) %>%
      mutate(pos_pr = pos_pr*-1,
             pos_in = pos_in*-1,
             pos_pi = pos_pi*-1) %>% 
      pivot_longer(cols = 6:9,
                   names_to = "nivel",
                   values_to = "posit") %>% 
      mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                  nivel %in% "pos_in" ~ "En inicio",
                                  nivel %in% "pos_pr" ~ "En proceso",
                                  TRUE ~ "Satisfactorio")) %>% 
      select(estrato2,tipo_esp,posit)
    
    # Cálculo de la medida promedio #
    #-------------------------------#
    
    mp_etiq <- bd_datos %>% 
      filter(!tipo %in% "Nivel de logro") %>% 
      select(estrato2 ,tipo_esp,valor)
    
    # Integración de las bases de logros y posición de etiquetas #
    #------------------------------------------------------------#
    
    niveles <- bd1 %>% 
      left_join(pos, by = c("estrato2","tipo_esp")) %>% 
      mutate(etiq = puntocoma2(valor,1),
             tipo_esp=factor(tipo_esp,
                             levels=c("Previo al inicio",
                                      "En inicio",
                                      "En proceso",
                                      "Satisfactorio"),
                             labels=c("Previo al inicio",
                                      "En inicio",
                                      "En proceso",
                                      "Satisfactorio")),
             valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                               TRUE ~ valor))
    
    ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) + 
      geom_col(width = 0.17) +
      geom_segment(aes(x=0.8,
                       xend=1.2,
                       y=0, 
                       yend=0), 
                   color="#252525",
                   lwd = 0.2) +
      scale_y_continuous(limits = c(-100,100)) + 
      scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) + 
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12,
                                       color = "#252525"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,
                                       color = "#252525")
      ) + 
      geom_text(aes(label = etiq,
                    y = posit),
                size = 4.5,
                color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                  TRUE ~ "#252525")) +
      geom_text(data = mp_etiq, 
                aes(label = puntocoma2(valor,0), 
                    y = 90, 
                    x = 1), 
                size = 4.0, 
                inherit.aes = FALSE) +
      geom_text(aes(x=0.69,
                    y=0.00,
                    label="Niveles de logro (%)"),
                size=3.0,
                alpha=0.2,
                angle = 90,
                color = "#252525") +
      geom_text(mapping=aes(x=0.70,
                            y=90,
                            label="Medida \npromedio"),
                size=3.0,
                alpha=0.2,
                color = "#252525")
  }
}

# Función que genera los gráficos de niveles de logro y medida promedio para estratos calculados #
#------------------------------------------------------------------------------------------------#

graf_estrat <- function(bd_nac, bd_estrat, nom_tipo){
  
  if (nom_tipo ==  "Tipo 1") {
    
    ###########################
    # EVALUACIONES MUESTRALES #
    ###########################
    
    if (sum(is.na(bd_nac$error)) == 0) {
      
      # Diferencia estadísticamente significativa en nivel satisfactorio dentro de los estratos #
      #-----------------------------------------------------------------------------------------#
      
      val_dif_mp <- bd_estrat %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2") ) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        select(estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        select(-tipo_esp) %>% 
        pivot_wider(names_from = estrato2,
                    values_from = c(valor,error)) %>% 
        pivot_longer(
          cols = starts_with("valor") | starts_with("error"),
          names_to = c(".value", "grupo"),
          names_sep = "_"
        ) %>% 
        filter(!is.na(valor)) %>%  # Eliminar filas sin valores
        group_by(estrato1_1) %>%  # Asegurar estructura
        mutate(
          valor1 = valor[1],
          valor2 = valor[2],
          error1 = error[1],
          error2 = error[2]
        ) %>%
        select(estrato1_1, valor1, valor2, error1, error2) %>% 
        distinct() %>% 
        mutate(diferencia = valor1 - valor2) %>% 
        mutate(Zm = diferencia/(sqrt(error1^2 + error2^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,dif_est_sig) 
      
      val_dif <- bd_estrat %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        select(estrato1_1, estrato2, tipo_esp, valor, error) %>% 
        distinct() %>%  # <- Elimina filas duplicadas para evitar problemas en pivot_wider
        pivot_wider(
          names_from = estrato2,
          values_from = c(valor, error),
          names_glue = "{.value}_{estrato2}"
        ) %>% 
        pivot_longer(
          cols = -c(estrato1_1, tipo_esp),
          names_to = c("tipo", "categoria"),
          names_sep = "_",
          values_to = "valor",
          values_drop_na = TRUE
        ) %>% 
        pivot_wider(
          names_from = tipo,
          values_from = valor
        ) %>% 
        group_by(estrato1_1, tipo_esp) %>%
        mutate(estrato = paste0("estrato", row_number())) %>%
        pivot_wider(
          names_from = estrato,
          values_from = c(valor, error),
          names_sep = "_"
        ) %>% 
         select(estrato1_1, tipo_esp, 
               valor_estrato1, valor_estrato2, 
               error_estrato1, error_estrato2) %>% 
        group_by(estrato1_1, tipo_esp) %>% 
        summarise_at(vars(valor_estrato1:error_estrato2),
                     ~sum(., na.rm = T)) %>% 
        ungroup() %>% 
        mutate(diferencia = .[[4]] - .[[3]]) %>% 
              mutate(Zm = diferencia/(sqrt(.[[6]]^2 + .[[5]]^2))) %>% 
              mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                        TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
              mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                            TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,tipo_esp,dif_est_sig)
      
      # Datos del grado y el área evaluada #
      #------------------------------------#
      
      nom_grado <- unique(bd_estrat$grado_eval)
      nom_area <- unique(bd_estrat$area_eval)
      
      # Total de niveles de logro #
      #---------------------------#
      
      totniv <- bd_estrat %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        select(tipo_esp) %>%
        arrange(tipo_esp) %>%
        pull() %>% 
        unique() %>% 
        length()
      
      # Base de datos para generar gráfico #
      #------------------------------------#
      
      bd1 <- rbind(bd_nac,bd_estrat) %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        select(estrato1_1,estrato2,tipo_esp,valor)
      
      if (totniv == 4) {
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`,n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 7:10,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(estrato1_1,estrato2,tipo_esp,posit)
        
        # Cálculo de la medida promedio #
        #-------------------------------#
        
        mp_etiq <- rbind(bd_nac,bd_estrat)%>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(!tipo %in% "Nivel de logro") %>% 
          left_join(val_dif_mp, by = "estrato1_1") %>% 
          select(estrato1_1,estrato2,valor,dif_est_sig) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = case_when(estrato2 %in% c("Mujer","Pública","Urbana","Pública \nurbana") & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*"),
                                  TRUE ~ puntocoma2(valor,0)))
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        niveles <- bd1 %>% 
          left_join(pos, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          left_join(val_dif, by = c("estrato1_1","tipo_esp")) %>% 
          mutate(tipo_esp=factor(tipo_esp,
                                 levels=c("Previo al inicio",
                                          "En inicio",
                                          "En proceso",
                                          "Satisfactorio"),
                                 labels=c("Previo al inicio",
                                          "En inicio",
                                          "En proceso",
                                          "Satisfactorio"))) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(estrato_num = as.numeric(as.factor(estrato1_1))) %>% 
          mutate(etiq = case_when(estrato2 %in% c("Mujer","Pública","Urbana","Pública \nurbana") & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*"),
                                  TRUE ~ puntocoma2(valor,1)),
                 valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)
          )
        
        # Gráfico de niveles de logro y medida promedio #
        #-----------------------------------------------#
        
        ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1),
                     nrow = 1,
                     scales = "free_x") +
          geom_col(width = case_when(niveles$estrato1_1 %in% "Nacional" ~ 0.25,
                                     TRUE ~ 0.45)) +
          scale_y_continuous(limits = c(-100,100)) + 
          geom_segment(data = niveles %>% filter(!estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.4,
                           xend = 2.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          geom_segment(data = niveles %>% filter(estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.6,
                           xend = 1.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) + 
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12,
                                       color = "#252525"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,
                                       color = "#252525"),
            panel.spacing = unit(0, "lines"),
            strip.background = element_rect(fill = "transparent",
                                            color = NA),
            strip.text = element_text(size = 15, 
                                      color = "#252525",
                                      hjust = 0.5)
          ) + 
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = estrato2), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), 
                   aes(x=0.50,
                       y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), ,
                    mapping=aes(x=0.65,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525") +
          geom_vline(data = niveles %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(2.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) +
          geom_vline(data = niveles %>% filter(estrato1_1 %in% "Nacional"),
                     aes(xintercept = c(1.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) 
        
      } else if ( totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa=case_when(n_sa >= 12 ~  n_sa/2,
                                  TRUE ~ n_sa + 11),
                 pos_pr=n_pr/2,
                 pos_in=case_when(n_in >= 12 ~ n_pr+n_in/2,
                                  TRUE ~ n_pr+n_in/2 + 11)) %>% 
          mutate(pos_pr=pos_pr*(-1),
                 pos_in=pos_in*(-1)) %>% 
          pivot_longer(cols = 6:8,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(estrato1_1,estrato2,tipo_esp,posit)
        
        # Cálculo de la medida promedio #
        #-------------------------------#
        
        mp_etiq <- rbind(bd_nac,bd_estrat)%>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(!tipo %in% "Nivel de logro") %>% 
          left_join(val_dif_mp, by = "estrato1_1") %>% 
          select(estrato1_1,estrato2,valor,dif_est_sig) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = case_when(estrato2 %in% c("Mujer","Pública","Urbana","Pública \nurbana") & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*"),
                                  TRUE ~ puntocoma2(valor,0)))
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        niveles <- bd1 %>% 
          left_join(pos, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          left_join(val_dif, by = c("estrato1_1","tipo_esp")) %>% 
          mutate(tipo_esp=factor(tipo_esp,
                                 levels=c("En inicio",
                                          "En proceso",
                                          "Satisfactorio"),
                                 labels=c("En inicio",
                                          "En proceso",
                                          "Satisfactorio"))) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(estrato_num = as.numeric(as.factor(estrato1_1))) %>% 
          mutate(etiq = case_when(estrato2 %in% c("Mujer","Pública","Urbana","Pública \nurbana") & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*"),
                                  TRUE ~ puntocoma2(valor,1)),
                 valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)
          )
        
        # Gráfico de niveles de logro y medida promedio #
        #-----------------------------------------------#
        
        ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1),
                     nrow = 1,
                     scales = "free_x") +
          geom_col(width = case_when(niveles$estrato1_1 %in% "Nacional" ~ 0.25,
                                     TRUE ~ 0.45)) +
          scale_y_continuous(limits = c(-100,100)) + 
          geom_segment(data = niveles %>% filter(!estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.4,
                           xend = 2.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          geom_segment(data = niveles %>% filter(estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.6,
                           xend = 1.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) + 
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12,
                                       color = "#252525"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,
                                       color = "#252525"),
            panel.spacing = unit(0, "lines"),
            strip.background = element_rect(fill = "transparent",
                                            color = NA),
            strip.text = element_text(size = 15, 
                                      color = "#252525",
                                      hjust = 0.5)
          ) + 
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = estrato2), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), 
                   aes(x=0.50,
                       y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), ,
                    mapping=aes(x=0.65,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525") +
          geom_vline(data = niveles %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(2.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) +
          geom_vline(data = niveles %>% filter(estrato1_1 %in% "Nacional"),
                     aes(xintercept = c(1.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) 
      }
      
      #########################
      # EVALUACIONES CENSALES #
      #########################
      
    } else if (sum(is.na(bd_nac$error)) > 0){
      
      # Datos del grado y el área evaluada #
      #------------------------------------#
      
      nom_grado <- unique(bd_estrat$grado_eval)
      nom_area <- unique(bd_estrat$area_eval)
      
      # Total de niveles de logro #
      #---------------------------#
      
      totniv <- bd_estrat %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        select(tipo_esp) %>%
        arrange(tipo_esp) %>%
        pull() %>% 
        unique() %>% 
        length()
      
      # Base de datos para generar gráfico #
      #------------------------------------#
      
      bd1 <- rbind(bd_nac,bd_estrat) %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        select(estrato1_1,estrato2,tipo_esp,valor)
      
      if (totniv == 4) {
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`,n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 7:10,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(estrato1_1,estrato2,tipo_esp,posit)
        
        # Cálculo de la medida promedio #
        #-------------------------------#
        
        mp_etiq <- rbind(bd_nac,bd_estrat)%>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(!tipo %in% "Nivel de logro") %>% 
          select(estrato1_1,estrato2,valor) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = puntocoma2(valor,0))
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        niveles <- bd1 %>% 
          left_join(pos, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(tipo_esp=factor(tipo_esp,
                                 levels=c("Previo al inicio",
                                          "En inicio",
                                          "En proceso",
                                          "Satisfactorio"),
                                 labels=c("Previo al inicio",
                                          "En inicio",
                                          "En proceso",
                                          "Satisfactorio"))) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(estrato_num = as.numeric(as.factor(estrato1_1))) %>% 
          mutate(etiq = puntocoma2(valor,1),
                 valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)
          )
        
        # Gráfico de niveles de logro y medida promedio #
        #-----------------------------------------------#
        
        ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1),
                     nrow = 1,
                     scales = "free_x") +
          geom_col(width = case_when(niveles$estrato1_1 %in% "Nacional" ~ 0.25,
                                     TRUE ~ 0.45)) +
          scale_y_continuous(limits = c(-100,100)) + 
          geom_segment(data = niveles %>% filter(!estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.4,
                           xend = 2.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          geom_segment(data = niveles %>% filter(estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.6,
                           xend = 1.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) + 
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12,
                                       color = "#252525"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,
                                       color = "#252525"),
            panel.spacing = unit(0, "lines"),
            strip.background = element_rect(fill = "transparent",
                                            color = NA),
            strip.text = element_text(size = 15, 
                                      color = "#252525",
                                      hjust = 0.5)
          ) + 
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = estrato2), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), 
                   aes(x=0.50,
                       y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), ,
                    mapping=aes(x=0.65,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525") +
          geom_vline(data = niveles %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(2.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) +
          geom_vline(data = niveles %>% filter(estrato1_1 %in% "Nacional"),
                     aes(xintercept = c(1.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) 

        
      } else if ( totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa=case_when(n_sa >= 12 ~  n_sa/2,
                                  TRUE ~ n_sa + 11),
                 pos_pr=n_pr/2,
                 pos_in=case_when(n_in >= 12 ~ n_pr+n_in/2,
                                  TRUE ~ n_pr+n_in/2 + 11)) %>% 
          mutate(pos_pr=pos_pr*(-1),
                 pos_in=pos_in*(-1)) %>% 
          pivot_longer(cols = 6:8,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(estrato1_1,estrato2,tipo_esp,posit)
        
        # Cálculo de la medida promedio #
        #-------------------------------#
        
        mp_etiq <- rbind(bd_nac,bd_estrat)%>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(!tipo %in% "Nivel de logro") %>% 
          select(estrato1_1,estrato2,valor) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = puntocoma2(valor,0))
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        niveles <- bd1 %>% 
          left_join(pos, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(tipo_esp=factor(tipo_esp,
                                 levels=c("En inicio",
                                          "En proceso",
                                          "Satisfactorio"),
                                 labels=c("En inicio",
                                          "En proceso",
                                          "Satisfactorio"))) %>% 
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        estrato1_1 %in% "Nacional" ~ "Nacional",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Nacional",
                                                "Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área"))) %>% 
          mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(estrato_num = as.numeric(as.factor(estrato1_1))) %>% 
          mutate(etiq = puntocoma2(valor,1),
                 valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)
          )
        
        # Gráfico de niveles de logro y medida promedio #
        #-----------------------------------------------#
        
        ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1),
                     nrow = 1,
                     scales = "free_x") +
          geom_col(width = case_when(niveles$estrato1_1 %in% "Nacional" ~ 0.25,
                                     TRUE ~ 0.45)) +
          scale_y_continuous(limits = c(-100,100)) + 
          geom_segment(data = niveles %>% filter(!estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.4,
                           xend = 2.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          geom_segment(data = niveles %>% filter(estrato1_1 %in% levels(estrato1_1)[1]),
                       aes(y = 0,
                           yend = 0,
                           x = 0.6,
                           xend = 1.6),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) + 
          theme_bw() +
          theme(
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12,
                                       color = "#252525"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,
                                       color = "#252525"),
            panel.spacing = unit(0, "lines"),
            strip.background = element_rect(fill = "transparent",
                                            color = NA),
            strip.text = element_text(size = 15, 
                                      color = "#252525",
                                      hjust = 0.5)
          ) + 
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = estrato2), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), 
                   aes(x=0.50,
                       y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(data = niveles %>% filter(estrato1_1 == levels(estrato1_1)[1]), ,
                    mapping=aes(x=0.65,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525") +
          geom_vline(data = niveles %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(2.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) +
          geom_vline(data = niveles %>% filter(estrato1_1 %in% "Nacional"),
                     aes(xintercept = c(1.6)), 
                     linetype = "dashed",
                     color = "#252525",
                     show.legend = F) 

      }
    }

  } else if (nom_tipo == "Tipo 2") {
    
    ##############
    # MUESTRALES #
    ##############
    
    if (sum(is.na(bd_estrat$error)) == 0){
      
      val_dif_mp <- bd_estrat %>% 
        filter(tipo %in% "Medida promedio") %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        select(estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        select(-tipo_esp) %>% 
        pivot_wider(names_from = estrato2,
                    values_from = c(valor,error)) %>% 
        pivot_longer(
          cols = starts_with("valor") | starts_with("error"),
          names_to = c(".value", "grupo"),
          names_sep = "_"
        ) %>% 
        filter(!is.na(valor)) %>%  # Eliminar filas sin valores
        group_by(estrato1_1) %>%  # Asegurar estructura
        mutate(
          valor1 = valor[1],
          valor2 = valor[2],
          error1 = error[1],
          error2 = error[2]
        ) %>%
        select(estrato1_1, valor1, valor2, error1, error2) %>% 
        distinct() %>% 
        mutate(diferencia = valor1 - valor2) %>% 
        mutate(Zm = diferencia/(sqrt(error1^2 + error2^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,dif_est_sig)
      
      temp1 <- bd_estrat %>% 
        filter(tipo %in% "Medida promedio") %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        filter(!is.na(estrato2)) %>%
        left_join(val_dif_mp, by = "estrato1_1") %>% 
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1,
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(etiq = case_when(estrato2 %in% c("Mujer","Pública","Urbana","Pública \nurbana") & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*"),
                                TRUE ~ puntocoma2(valor,0)))
      
      # Etiquetas de texto con los nombres de los niveles de logro #
      #------------------------------------------------------------#
      
      text_sat <- textGrob("Satisfactorio", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_satisf
                           ),
                           just = "left")
      text_pro <- textGrob("En proceso", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_proces
                           ),
                           just = "left")
      text_ini <- textGrob("En inicio", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_inicio
                           ),
                           just = "left")
      text_pin <- textGrob("Previo al inicio", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_pre_in
                           ),
                           just = "left")
      
      # Identificadores para filtros #
      #------------------------------#
      
      nom_gradoeval <- unique(temp1$grado_eval) # Identificador de grado evaluado
      nom_area <- unique(temp1$area_eval) # Identificador de área evaluada
      color_puntos <- c("#0077E6","#B2D9FF","#0077E6","#B2D9FF","#0077E6","#B2D9FF","#0077E6","#B2D9FF")
      
      if (nom_gradoeval %in% "2.° grado de primaria" & nom_area %in% "Lectura"){
        
        limites <- c(400,650)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(458.39, 583.66) # (Menor a 458.39; mayor o igual a 458.39 y menor a 583.66; Mayor o igual a 583.66)

        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        annotation_pin <- NULL
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "2.° grado de primaria" & nom_area %in% "Matemática") {
        
        limites <- c(400,700)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(512.22, 639.21)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        annotation_pin <- NULL
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Lectura") {
        
        limites <- c(300,600)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(356.92, 444.72, 522.03)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Matemática") {
        
        limites <- c(300,600)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(351.90, 422.21, 526.46)

        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "6.° grado de primaria" & nom_area %in% "Lectura") {
      
      limites <- c(400,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(445.46, 522.17, 579.14)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "6.° grado de primaria" & nom_area %in% "Matemática") {
      
      limites <- c(400,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(432.32, 526.61, 603.41)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Lectura") {
        
        limites <- c(400,700)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(505.14, 580.61, 641.25)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Matemática") {
        
        limites <- c(400,700)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(519.67, 595.96, 649.38)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Ciencias sociales") {
      
      limites <- c(350,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(425.93, 500.05, 607.28)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Ciencia y tecnología") {
      
      limites <- c(300,700)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(374.60, 509.58, 628.48)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    }

      # Gráfico #
      #---------#

      ggplot(temp1, aes(x = estrato2, y = valor, color = estrato2)) + 
        facet_wrap(vars(estrato1_1),
                   nrow = 1,
                   scales = "free_x") + 
        geom_errorbar(aes(x = estrato2,
                          ymin = valor - 1.96*error, 
                          ymax = valor + 1.96*error),
                      width = 0.13,
                      linewidth = 0.25) +
        geom_point(size = 3.2) + 
        hlines +
        scale_y_continuous(limits = limites,
                           breaks = c(redondear(puntos_corte,0))) +
        scale_color_manual(values = color_puntos,
                         breaks = c("Hombre","Mujer","Privada","Pública","Rural","Urbana","Privada","Pública \nurbana")) +
        theme_bw() + 
        theme(
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 14,
                                     color = "#252525"),
          axis.text.y = element_text(size = 14,
                                     color = "#252525",
                                     vjust = 0.5),
          strip.background = element_rect(fill = "transparent",
                                          color = NA),
          strip.text = element_text(size = 15, 
                                    color = "#252525",
                                    hjust = 0.5),
          plot.margin = unit(c(0, 1.65, 0, 0.50), 
                             "inches")
        ) + 
        geom_text(aes(label = etiq,
                      vjust = case_when(error >= 2.0 ~ -2.0 ,
                                        TRUE ~ -1.0)),
                  color = "#252525",
                  size = 5.0) + 
        geom_vline(data = temp1 %>% filter(!estrato1_1 %in% limit_estrato),
                   aes(xintercept = c(2.58)), 
                   linetype = "dashed",
                   lwd = 0.4,
                   color = "#252525",
                   alpha = 0.3,
                   show.legend = F) +
        annotation_sat +
        annotation_pro + 
        annotation_ini + 
        annotation_pin +
        coord_cartesian(clip = "off")
      
      ############
      # CENSALES #
      ############
      
    } else if (sum(is.na(bd_estrat$error)) > 0){
      
      temp1 <- bd_estrat %>% 
        filter(tipo %in% "Medida promedio") %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        filter(!is.na(estrato2)) %>%
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1,
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(estrato2 = case_when(estrato2 %in% c("Pública urbana") ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(etiq = puntocoma2(valor,0))
      
      # Etiquetas de texto con los nombres de los niveles de logro #
      #------------------------------------------------------------#
      
      text_sat <- textGrob("Satisfactorio", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_satisf
                           ),
                           just = "left")
      text_pro <- textGrob("En proceso", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_proces
                           ),
                           just = "left")
      text_ini <- textGrob("En inicio", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_inicio
                           ),
                           just = "left")
      text_pin <- textGrob("Previo al inicio", 
                           gp = gpar(fontsize = 16.5, 
                                     col = color_pre_in
                           ),
                           just = "left")
      
      # Identificadores para filtros #
      #------------------------------#
      
      nom_gradoeval <- unique(temp1$grado_eval) # Identificador de grado evaluado
      nom_area <- unique(temp1$area_eval) # Identificador de área evaluada
      color_puntos <- c("#0077E6","#B2D9FF","#0077E6","#B2D9FF","#0077E6","#B2D9FF","#0077E6","#B2D9FF")
      
      if (nom_gradoeval %in% "2.° grado de primaria" & nom_area %in% "Lectura"){
        
        limites <- c(400,650)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(458.39, 583.66) # (Menor a 458.39; mayor o igual a 458.39 y menor a 583.66; Mayor o igual a 583.66)
        
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        annotation_pin <- NULL
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "2.° grado de primaria" & nom_area %in% "Matemática") {
        
        limites <- c(400,700)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(512.22, 639.21)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        annotation_pin <- NULL
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Lectura") {
        
        limites <- c(300,600)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(356.92, 444.72, 522.03)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Matemática") {
        
        limites <- c(300,600)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(351.90, 422.21, 526.46)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "6.° grado de primaria" & nom_area %in% "Lectura") {
      
      limites <- c(400,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(445.46, 522.17, 579.14)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "6.° grado de primaria" & nom_area %in% "Matemática") {
      
      limites <- c(400,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(432.32, 526.61, 603.41)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Lectura") {
        
        limites <- c(400,700)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(505.14, 580.61, 641.25)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Matemática") {
        
        limites <- c(400,700)
        limit_estrato <- "Gestión y área"
        puntos_corte <- c(519.67, 595.96, 649.38)
        
        annotation_sat <- annotation_custom(
          grob = text_sat,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
        ) 
        annotation_pro <- annotation_custom(
          grob = text_pro,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
        ) 
        annotation_ini <- annotation_custom(
          grob = text_ini,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
        )
        annotation_pin <- annotation_custom(
          grob = text_pin,
          xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
          ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
        )
        
        hlines <- list(
          geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                     color = color_inicio,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                     color = color_proces,
                     alpha = 0.5,
                     lwd = 0.2),
          geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                     color = color_satisf,
                     alpha = 0.5,
                     lwd = 0.2)
        )
        
      } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Ciencias sociales") {
      
      limites <- c(350,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(425.93, 500.05, 607.28)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Ciencia y tecnología") {
      
      limites <- c(300,700)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(374.60, 509.58, 628.48)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    }

      # Gráfico #
      #---------#

      ggplot(temp1, aes(x = estrato2, y = valor, color = estrato2)) + 
        facet_wrap(vars(estrato1_1),
                   nrow = 1,
                   scales = "free_x") +
        geom_point(size = 3.2) + 
        hlines +
        scale_y_continuous(limits = limites,
                           breaks = c(redondear(puntos_corte,0))) +
        scale_color_manual(values = color_puntos,
                         breaks = c("Hombre","Mujer","Privada","Pública","Rural","Urbana","Privada","Pública \nurbana")) +
        theme_bw() + 
        theme(
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 14,
                                     color = "#252525"),
          axis.text.y = element_text(size = 14,
                                     color = "#252525",
                                     vjust = 0.5),
          strip.background = element_rect(fill = "transparent",
                                          color = NA),
          strip.text = element_text(size = 15, 
                                    color = "#252525",
                                    hjust = 0.5),
          plot.margin = unit(c(0, 1.65, 0, 0.50), 
                             "inches")
        ) + 
        geom_text(aes(label = etiq,
                      vjust = -1.0),
                  color = "#252525",
                  size = 5.0) + 
        geom_vline(data = temp1 %>% filter(!estrato1_1 %in% limit_estrato),
                   aes(xintercept = c(2.58)), 
                   linetype = "dashed",
                   lwd = 0.4,
                   color = "#252525",
                   alpha = 0.3,
                   show.legend = F) +
        annotation_sat +
        annotation_pro + 
        annotation_ini + 
        annotation_pin +
        coord_cartesian(clip = "off")
      
    } 
    
  } else if (nom_tipo == "Tipo 3") {
    
    # Total de niveles de logro #
    #---------------------------#
    
    totniv <- bd_estrat %>% 
      filter(tipo %in% "Nivel de logro") %>% 
      select(tipo_esp) %>%
      arrange(tipo_esp) %>%
      pull() %>% 
      unique() %>% 
      length()
    
    # Niveles de logro #
    #------------------#
    
    bd1 <- rbind(bd_nac,bd_estrat) %>% 
      filter(tipo %in% "Nivel de logro") %>% 
      filter(estrato1_1 %in% c("nom_dre","Nacional")) %>% 
      select(estrato2,tipo_esp,valor) %>% 
      mutate(estrato2 = str_replace_all(estrato2, " ","")) %>% 
      mutate(estrato2 = case_when(estrato2 %in% "LaLibertad" ~ "La Libertad",
                                  estrato2 %in% "LimaMetropolitana" ~ "Lima Metropolitana",
                                  estrato2 %in% "LimaProvincias" ~ "Lima Provincias",
                                  estrato2 %in% "MadredeDios" ~ "Madre de Dios",
                                  estrato2 %in% "SanMartín" ~ "San Martín",
                                  TRUE ~ estrato2))
    
    # Cálculo de la medida promedio #
    #-------------------------------#
    
    mp_etiq <- rbind(bd_nac,bd_estrat)  %>% 
      filter(!tipo %in% "Nivel de logro") %>% 
      filter(estrato1_1 %in% c("nom_dre","Nacional")) %>%  
      select(estrato2 ,tipo_esp,valor) %>% 
      mutate(estrato2 = str_replace_all(estrato2, " ","")) %>% 
      mutate(estrato2 = case_when(estrato2 %in% "LaLibertad" ~ "La Libertad",
                                  estrato2 %in% "LimaMetropolitana" ~ "Lima Metropolitana",
                                  estrato2 %in% "LimaProvincias" ~ "Lima Provincias",
                                  estrato2 %in% "MadredeDios" ~ "Madre de Dios",
                                  estrato2 %in% "SanMartín" ~ "San Martín",
                                  TRUE ~ estrato2)) %>%
      mutate(etiq = puntocoma2(valor,0))
    
    if (totniv == 4) {
      
      # Posiciones de etiquetas #
      #-------------------------#
      
      pos <- bd1 %>% 
        pivot_wider(names_from = tipo_esp,
                    values_from = valor) %>% 
        rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
        mutate(pos_sa = case_when(n_sa >= 9 ~ n_sa/2,
                                  TRUE ~ n_sa + 8),
               pos_pr = n_pr/2,
               pos_in = n_pr + n_in/2,
               pos_pi = case_when(n_pi >= 9 ~ n_pr + n_in + n_pi/2,
                                  TRUE ~ n_pr + n_in + n_pi + 8)) %>%
        mutate(pos_pr = pos_pr*-1,
               pos_in = pos_in*-1,
               pos_pi = pos_pi*-1) %>% 
        pivot_longer(cols = 6:9,
                     names_to = "nivel",
                     values_to = "posit") %>% 
        mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                    nivel %in% "pos_in" ~ "En inicio",
                                    nivel %in% "pos_pr" ~ "En proceso",
                                    TRUE ~ "Satisfactorio")) %>% 
        select(estrato2,tipo_esp,posit) 
      
      # Integración de las bases de logros y posición de etiquetas #
      #------------------------------------------------------------#
      
      niveles <- bd1 %>% 
        left_join(pos, by = c("estrato2","tipo_esp")) %>% 
        mutate(etiq = puntocoma2(valor,1),
               tipo_esp=factor(tipo_esp,
                               levels=c("Previo al inicio",
                                        "En inicio",
                                        "En proceso",
                                        "Satisfactorio"),
                               labels=c("Previo al inicio",
                                        "En inicio",
                                        "En proceso",
                                        "Satisfactorio")),
               valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                 TRUE ~ valor)) %>% 
        mutate(estrato2 = str_replace_all(estrato2, " ","")) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "LaLibertad" ~ "La Libertad",
                                    estrato2 %in% "LimaMetropolitana" ~ "Lima Metropolitana",
                                    estrato2 %in% "LimaProvincias" ~ "Lima Provincias",
                                    estrato2 %in% "MadredeDios" ~ "Madre de Dios",
                                    estrato2 %in% "SanMartín" ~ "San Martín",
                                    TRUE ~ estrato2))
      
      # Gráfico #
      #---------#
      
      reg1 <- niveles %>% 
        filter(!estrato2 %in% "Nacional") %>% 
        select(estrato2) %>%
        mutate(orden_aux = stri_trans_general(estrato2, "Latin-ASCII")) %>%  # Crear columna auxiliar sin tildes
        arrange(orden_aux) %>%  # Ordenar por la versión sin tildes
        select(estrato2) %>%  # Eliminar la columna auxiliar para mantener los nombres originales
        pull() %>%
        unique()
      
      orden1 <- c("Nacional",reg1)
      
      ggplot(niveles, aes(x = factor(estrato2, levels = orden1), y=valor, fill = tipo_esp)) + 
        geom_col(width = 0.77) +
        coord_cartesian(ylim = c(-100, 100),
                        clip = "off") +
        geom_segment(aes(x=0.5,
                         xend=27.5,
                         y=0, 
                         yend=0), 
                     color="#252525",
                     lwd = 0.2) +
        annotate("segment",
                 x = 1.5,
                 xend = 1.5,
                 y = -200,  
                 yend = 100,  
                 linetype = "dashed",
                 color = "#6baed6",
                 lwd = 0.3) +  
        scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) +
        theme_bw() +
        theme(panel.border = element_blank(),
              panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 12,
                                         color = "#252525"),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 12,
                                         color = "#252525",
                                         angle = 90,
                                         hjust = 1,
                                         vjust = 0.5)
        ) +
        geom_text(aes(label = etiq,
                      y = posit),
                  size = 3.5,
                  color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 7 ~ "white",
                                    TRUE ~ "#252525")) + 
        geom_text(data = mp_etiq, 
                  aes(label = etiq, 
                      y = 90, 
                      x = factor(estrato2, levels = orden1)), 
                  size = 4.0, 
                  inherit.aes = FALSE) 
      
      
    } else if (totniv == 3){
      
      # Posiciones de etiquetas #
      #-------------------------#
      
      pos <- bd1 %>% 
        pivot_wider(names_from = tipo_esp,
                    values_from = valor) %>% 
        rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
        mutate(pos_sa = case_when(n_sa >= 9 ~ n_sa/2,
                                  TRUE ~ n_sa + 8),
               pos_pr = n_pr/2,
               pos_in = case_when(n_in >= 9 ~ n_pr + n_in/2,
                                  TRUE ~ n_pr + n_in/2 + 8)) %>%
        mutate(pos_pr = pos_pr*-1,
               pos_in = pos_in*-1) %>% 
        pivot_longer(cols = 5:7,
                     names_to = "nivel",
                     values_to = "posit") %>% 
        mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                    nivel %in% "pos_pr" ~ "En proceso",
                                    TRUE ~ "Satisfactorio")) %>% 
        select(estrato2,tipo_esp,posit)
      
      # Integración de las bases de logros y posición de etiquetas #
      #------------------------------------------------------------#
      
      niveles <- bd1 %>% 
        left_join(pos, by = c("estrato2","tipo_esp")) %>% 
        mutate(etiq = puntocoma2(valor,1),
               tipo_esp=factor(tipo_esp,
                               levels=c("En inicio",
                                        "En proceso",
                                        "Satisfactorio"),
                               labels=c("En inicio",
                                        "En proceso",
                                        "Satisfactorio")),
               valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                 TRUE ~ valor)) %>% 
        mutate(estrato2 = str_replace_all(estrato2, " ","")) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "LaLibertad" ~ "La Libertad",
                                    estrato2 %in% "LimaMetropolitana" ~ "Lima Metropolitana",
                                    estrato2 %in% "LimaProvincias" ~ "Lima Provincias",
                                    estrato2 %in% "MadredeDios" ~ "Madre de Dios",
                                    estrato2 %in% "SanMartín" ~ "San Martín",
                                    TRUE ~ estrato2))
      
      # Gráfico #
      #---------#
      
      reg1 <- niveles %>% 
        filter(!estrato2 %in% "Nacional") %>% 
        select(estrato2) %>%
        mutate(orden_aux = stri_trans_general(estrato2, "Latin-ASCII")) %>%  # Crear columna auxiliar sin tildes
        arrange(orden_aux) %>%  # Ordenar por la versión sin tildes
        select(estrato2) %>%  # Eliminar la columna auxiliar para mantener los nombres originales
        pull() %>%
        unique()
      
      orden1 <- c("Nacional",reg1)
      
      ggplot(niveles, aes(x = factor(estrato2, levels = orden1), y=valor, fill = tipo_esp)) + 
        geom_col(width = 0.77) +
        coord_cartesian(ylim = c(-100, 100),
                        clip = "off") +
        geom_segment(aes(x=0.5,
                         xend=27.5,
                         y=0, 
                         yend=0), 
                     color="#252525",
                     lwd = 0.2) +
        annotate("segment",
                 x = 1.5,
                 xend = 1.5,
                 y = -200,  
                 yend = 100,  
                 linetype = "dashed",
                 color = "#6baed6",
                 lwd = 0.3) +  
        scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) +
        theme_bw() +
        theme(panel.border = element_blank(),
              panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 12,
                                         color = "#252525"),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 12,
                                         color = "#252525",
                                         angle = 90,
                                         hjust = 1,
                                         vjust = 0.5)
        ) +
        geom_text(aes(label = etiq,
                      y = posit),
                  size = 3.5,
                  color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 7 ~ "white",
                                    TRUE ~ "#252525")) + 
        geom_text(data = mp_etiq, 
                  aes(label = etiq, 
                      y = 90, 
                      x = factor(estrato2, levels = orden1)), 
                  size = 4.0, 
                  inherit.aes = FALSE) 
    }
    
  } else {

    return("Prueba otra vez")

  }
}  

# Función que genera los gráficos de niveles de logro según estratos específicos #
#--------------------------------------------------------------------------------#

graf_spcf <- function(bd_datos){
  
  # Datos del grado y el área evaluada #
  #------------------------------------#
  
  nom_grado <- unique(bd_datos$grado_eval)
  nom_area <- unique(bd_datos$area_eval)
  
  # Total de niveles de logro #
  #---------------------------#
  
  totniv <- bd_datos %>% 
    filter(tipo %in% "Nivel de logro") %>% 
    select(tipo_esp) %>%
    arrange(tipo_esp) %>%
    pull() %>% 
    unique() %>% 
    length()
  
  # Base de datos para generar gráfico #
  #------------------------------------#
  
  bd1 <- bd_datos %>% 
    filter(tipo %in% "Nivel de logro") %>% 
    select(estrato1_1,estrato2,tipo_esp,valor)
  
  if (totniv == 4){
    
    # Cálculo de las posiciones de las etiquetas #
    #--------------------------------------------#
    
    pos <- bd1 %>% 
      pivot_wider(names_from = tipo_esp,
                  values_from = valor) %>% 
      rename(n_pi = `Previo al inicio`,n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
      mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                TRUE ~ n_sa + 11),
             pos_pr = n_pr/2,
             pos_in = case_when(n_in >= 12 ~ n_pr + n_in/2, 
                                TRUE ~  n_pr + n_in + n_pi + 11),
             pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                n_in < 12 ~ n_pr + n_in + n_pi + 22,
                                TRUE ~ n_pr + n_in + n_pi + 11)) %>%
      mutate(pos_pr = pos_pr*-1,
             pos_in = pos_in*-1,
             pos_pi = pos_pi*-1) %>% 
      pivot_longer(cols = 7:10,
                   names_to = "nivel",
                   values_to = "posit") %>% 
      mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                  nivel %in% "pos_in" ~ "En inicio",
                                  nivel %in% "pos_pr" ~ "En proceso",
                                  TRUE ~ "Satisfactorio")) %>% 
      select(estrato1_1,estrato2,tipo_esp,posit)
    
    # Cálculo de la medida promedio #
    #-------------------------------#
    
    mp_etiq <- bd_datos %>% 
      filter(!tipo %in% "Nivel de logro") %>% 
      select(estrato1_1,estrato2,valor) %>% 
      mutate(etiq = puntocoma2(valor,0))
    
    # Integración de las bases de logros y posición de etiquetas #
    #------------------------------------------------------------#
    
    niveles <- bd1 %>% 
      left_join(pos, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
      mutate(tipo_esp=factor(tipo_esp,
                             levels=c("Previo al inicio",
                                      "En inicio",
                                      "En proceso",
                                      "Satisfactorio"),
                             labels=c("Previo al inicio",
                                      "En inicio",
                                      "En proceso",
                                      "Satisfactorio"))) %>% 
      mutate(estrato_num = as.numeric(as.factor(estrato1_1))) %>% 
      mutate(etiq = puntocoma2(valor,1),
             valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                               TRUE ~ valor)
      )
    
    # Total de categorías para definir límites de la línea gris de Satisfactorio #
    #----------------------------------------------------------------------------#
    
    tot_cat <- length(unique(niveles$estrato2))
    
    # Gráfico #
    #---------#
    
    ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) +
      geom_col(width = 0.35) +
      scale_y_continuous(limits = c(-100,100)) + 
      geom_segment(aes(y = 0,
                       yend = 0,
                       x = 0.7,
                       xend = tot_cat + 0.3),
                   linetype = "solid",
                   color = "#252525",
                   lwd = 0.3,
                   alpha = 0.5) +
      scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) + 
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12,
                                   color = "#252525"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12,
                                   color = "#252525"),
        plot.margin = unit(c(0, 0.0, 0.0, 0.8), "inches"),
        strip.background = element_rect(fill = "transparent",
                                        color = NA),
        strip.text = element_text(size = 15, 
                                  color = "#252525",
                                  hjust = 0.5)
      ) +
      geom_text(aes(label = etiq,
                    y = posit),
                size = 4.5,
                color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                  TRUE ~ "#252525")) +
      geom_text(data = mp_etiq, 
                aes(label = etiq, 
                    y = 90, 
                    x = estrato2), 
                size = 4.5, 
                inherit.aes = FALSE) +
      annotate(geom="text", 
             x=0.5, 
             y=0.0, 
             label="Niveles de logro (%)",
             angle = 90,
             size=3.0,
             colour = "#636363") +
      annotate(geom="text", 
             x=0.48, 
             y=90, 
             label="Medida \npromedio",
             size=3.0,
             colour = "#636363") +
      coord_cartesian(clip = "off") 

  } else if (totniv == 3) {
    
    # Cálculo de las posiciones de las etiquetas #
    #--------------------------------------------#
    
    pos <- bd1 %>% 
      pivot_wider(names_from = tipo_esp,
                  values_from = valor) %>% 
      rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
      mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                TRUE ~ n_sa + 11),
             pos_pr = n_pr/2,
             pos_in = case_when(n_in >= 12 ~ n_pr + n_in/2, 
                                TRUE ~  n_pr + n_in + 11)) %>%
      mutate(pos_pr = pos_pr*-1,
             pos_in = pos_in*-1) %>% 
      pivot_longer(cols = 6:8,
                   names_to = "nivel",
                   values_to = "posit") %>% 
      mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                  nivel %in% "pos_pr" ~ "En proceso",
                                  TRUE ~ "Satisfactorio")) %>% 
      select(estrato1_1,estrato2,tipo_esp,posit)
    
    # Cálculo de la medida promedio #
    #-------------------------------#
    
    mp_etiq <- bd_datos %>% 
      filter(!tipo %in% "Nivel de logro") %>% 
      select(estrato1_1,estrato2,valor) %>% 
      mutate(etiq = puntocoma2(valor,0))
    
    # Integración de las bases de logros y posición de etiquetas #
    #------------------------------------------------------------#
    
    niveles <- bd1 %>% 
      left_join(pos, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
      mutate(tipo_esp=factor(tipo_esp,
                             levels=c("En inicio",
                                      "En proceso",
                                      "Satisfactorio"),
                             labels=c("En inicio",
                                      "En proceso",
                                      "Satisfactorio"))) %>% 
      mutate(estrato_num = as.numeric(as.factor(estrato1_1))) %>% 
      mutate(etiq = puntocoma2(valor,1),
             valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                               TRUE ~ valor)
      )
    
    # Total de categorías para definir límites de la línea gris de Satisfactorio #
    #----------------------------------------------------------------------------#
    
    tot_cat <- length(unique(niveles$estrato2))
    
    # Gráfico #
    #---------#
    
    ggplot(niveles, aes(x = estrato2, y = valor, fill = tipo_esp)) +
      geom_col(width = 0.35) +
      scale_y_continuous(limits = c(-100,100)) + 
      geom_segment(aes(y = 0,
                       yend = 0,
                       x = 0.7,
                       xend = tot_cat + 0.3),
                   linetype = "solid",
                   color = "#252525",
                   lwd = 0.3,
                   alpha = 0.5) +
      scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) + 
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12,
                                   color = "#252525"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12,
                                   color = "#252525"),
        strip.background = element_rect(fill = "transparent",
                                        color = NA),
        strip.text = element_text(size = 15, 
                                  color = "#252525",
                                  hjust = 0.5)
      ) +
      geom_text(aes(label = etiq,
                    y = posit),
                size = 4.5,
                color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                  TRUE ~ "#252525")) +
      geom_text(data = mp_etiq, 
                aes(label = etiq, 
                    y = 90, 
                    x = estrato2), 
                size = 4.5, 
                inherit.aes = FALSE) +
      annotate(geom="text", 
             x=0.5, 
             y=0.0, 
             label="Niveles de logro (%)",
             angle = 90,
             size=3.0,
             colour = "#636363") +
      annotate(geom="text", 
             x=0.48, 
             y=90, 
             label="Medida \npromedio",
             size=3.0,
             colour = "#636363") +
      coord_cartesian(clip = "off") 
  }
}

# Función que genera los gráficos de comparación en el tiempo #
#-------------------------------------------------------------#

graf_tiempo <- function(bd_año1,bd_año2,bd_año1_estrat, bd_año2_estrat, nom_tipo){
  
  if (nom_tipo == "Tipo 1"){
    
    # Valores fijos de referencia para filtros #
    #------------------------------------------#
    
    nom_año <- unique(bd_año2$año)
    
    totniv <- bd_año2 %>% 
      filter(tipo %in% "Nivel de logro") %>% 
      arrange(tipo_esp) %>% 
      select(tipo_esp) %>% 
      pull() %>% 
      length()
    
    # Base de datos inicial #
    #-----------------------# 
    
    bd1 <-  rbind(bd_año1,bd_año2) %>% 
      filter(tipo %in% "Nivel de logro") %>% 
      select(año,estrato2 ,tipo_esp,valor)
    
    # Identificadores de tipo de evaluación (muestral o censal) #
    #-----------------------------------------------------------#
    
    subt1 <- sum(is.na(bd_año1$error))
    subt2 <- sum(is.na(bd_año2$error))
    
    # Condicional según tipo de evaluaciones que se analizan #
    #--------------------------------------------------------#
    
    if (subt1 == 0 & subt2 == 0){
      
      # Diferencia estadísticamente significativa en nivel satisfactorio dentro de los estratos #
      #-----------------------------------------------------------------------------------------#
      
      val_dif_nl <- rbind(bd_año1,bd_año2) %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,tipo_esp,dif_est_sig) 
      
      val_dif_mp <- rbind(bd_año1,bd_año2) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,tipo_esp,dif_est_sig) 
      
      # Cálculo de las posiciones de las etiquetas #
      #--------------------------------------------#
      
      if (totniv == 4){
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 7:10,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,tipo_esp,posit)
        
      } else if (totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa=case_when(n_sa >= 12 ~  n_sa/2,
                                  TRUE ~ n_sa + 11),
                 pos_pr=n_pr/2,
                 pos_in=case_when(n_in >= 12 ~ n_pr+n_in/2,
                                  TRUE ~ n_pr+n_in/2 + 11)) %>% 
          mutate(pos_pr=pos_pr*(-1),
                 pos_in=pos_in*(-1)) %>% 
          pivot_longer(cols = 6:8,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,tipo_esp,posit)
        
      }
      
      # Cálculo de la medida promedio #
      #-------------------------------#
      
      mp_etiq <- rbind(bd_año1,bd_año2) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        select(año ,tipo_esp,valor)
      
      # Base integrada #
      #----------------#
      
      niveles <- rbind(bd_año1,bd_año2) %>%
        filter(tipo %in% "Nivel de logro") %>% 
        left_join(pos, by = c("año","tipo_esp")) %>% 
        left_join(val_dif_nl, by = c("tipo_esp")) %>% 
        mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*") ,
                                TRUE ~ puntocoma2(valor,1))) %>% 
        mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                 TRUE ~ valor)) %>% 
        mutate(tipo_esp = factor(tipo_esp, 
                                 levels = c("Previo al inicio",
                                            "En inicio",
                                            "En proceso",
                                            "Satisfactorio")))
      
      if (totniv == 3){
        
        # Gráfico #
        #---------#
        
        ggplot(niveles, aes(x = año, y = valor, fill = tipo_esp)) + 
          geom_col(width = 0.3) +
          geom_segment(aes(x=0.8,
                           xend=2.2,
                           y=0, 
                           yend=0), 
                       color="#252525",
                       lwd = 0.2) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) +
          scale_x_discrete(expand = expansion(mult = 0, add = c(2.5,2))) +
          scale_y_continuous(limits = c(-100,100)) + 
          theme_bw() +
          theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525")
          ) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.5,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = valor, 
                        y = 90, 
                        x = año), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(aes(x=0.69,
                        y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(mapping=aes(x=0.70,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525")
        
        
      } else if ( totniv == 4){
        
        # Gráfico #
        #---------#
        
        ggplot(niveles, aes(x = año, y = valor, fill = tipo_esp)) + 
          geom_col(width = 0.3) +
          geom_segment(aes(x=0.8,
                           xend=2.2,
                           y=0, 
                           yend=0), 
                       color="#252525",
                       lwd = 0.2) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) + 
          scale_x_discrete(expand = expansion(mult = 0, add = c(2.5,2))) +
          theme_bw() +
          theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525")
          ) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.5,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = valor, 
                        y = 90, 
                        x = año), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(aes(x=0.69,
                        y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(mapping=aes(x=0.70,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525")
        
      }
      
      
    } else if ((subt1 == 0 & subt2 > 0) | (subt1 > 0 & subt2 == 0)){
      
      # Diferencia estadísticamente significativa en nivel satisfactorio dentro de los estratos #
      #-----------------------------------------------------------------------------------------#
      
      val_dif_nl <- rbind(bd_año1,bd_año2) %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error)) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,tipo_esp,dif_est_sig) 
      
      val_dif_mp <- rbind(bd_año1,bd_año2) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error)) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,tipo_esp,dif_est_sig) 
      
      # Cálculo de las posiciones de las etiquetas #
      #--------------------------------------------#
      
      if (totniv == 4){
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 7:10,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,tipo_esp,posit)
        
      } else if (totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa=case_when(n_sa >= 12 ~  n_sa/2,
                                  TRUE ~ n_sa + 11),
                 pos_pr=n_pr/2,
                 pos_in=case_when(n_in >= 12 ~ n_pr+n_in/2,
                                  TRUE ~ n_pr+n_in/2 + 11)) %>% 
          mutate(pos_pr=pos_pr*(-1),
                 pos_in=pos_in*(-1)) %>% 
          pivot_longer(cols = 6:8,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,tipo_esp,posit)
        
      }
      
      # Cálculo de la medida promedio #
      #-------------------------------#
      
      mp_etiq <- rbind(bd_año1,bd_año2) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        select(año ,tipo_esp,valor)
      
      # Base integrada #
      #----------------#
      
      niveles <- rbind(bd_año1,bd_año2) %>%
        filter(tipo %in% "Nivel de logro") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error))  %>% 
        left_join(pos, by = c("año","tipo_esp")) %>% 
        left_join(val_dif_nl, by = c("tipo_esp")) %>% 
        mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*") ,
                                TRUE ~ puntocoma2(valor,1))) %>% 
        mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                 TRUE ~ valor)) %>% 
        mutate(tipo_esp = factor(tipo_esp, 
                                 levels = c("Previo al inicio",
                                            "En inicio",
                                            "En proceso",
                                            "Satisfactorio")))
      
      if (totniv == 3){
        
        # Gráfico #
        #---------#
        
        ggplot(niveles, aes(x = año, y = valor, fill = tipo_esp)) + 
          geom_col(width = 0.3) +
          geom_segment(aes(x=0.8,
                           xend=2.2,
                           y=0, 
                           yend=0), 
                       color="#252525",
                       lwd = 0.2) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) + 
          scale_x_discrete(expand = expansion(mult = 0, add = c(2.5,2))) +
          theme_bw() +
          theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525")
          ) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.5,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = valor, 
                        y = 90, 
                        x = año), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(aes(x=0.69,
                        y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(mapping=aes(x=0.70,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525")
        
        
      } else if ( totniv == 4){
        
        # Gráfico #
        #---------#
        
        ggplot(niveles, aes(x = año, y = valor, fill = tipo_esp)) + 
          geom_col(width = 0.3) +
          geom_segment(aes(x=0.8,
                           xend=2.2,
                           y=0, 
                           yend=0), 
                       color="#252525",
                       lwd = 0.2) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) + 
          scale_x_discrete(expand = expansion(mult = 0, add = c(2.5,2))) +
          theme_bw() +
          theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525")
          ) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.5,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = valor, 
                        y = 90, 
                        x = año), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(aes(x=0.69,
                        y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(mapping=aes(x=0.70,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525")
        
      }
      
    } else if (subt1 > 0 & subt2 > 0) {
      
      # Cálculo de las posiciones de las etiquetas #
      #--------------------------------------------#
      
      if (totniv == 4){
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 7:10,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,tipo_esp,posit)
        
      } else if (totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa=case_when(n_sa >= 12 ~  n_sa/2,
                                  TRUE ~ n_sa + 11),
                 pos_pr=n_pr/2,
                 pos_in=case_when(n_in >= 12 ~ n_pr+n_in/2,
                                  TRUE ~ n_pr+n_in/2 + 11)) %>% 
          mutate(pos_pr=pos_pr*(-1),
                 pos_in=pos_in*(-1)) %>% 
          pivot_longer(cols = 6:8,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,tipo_esp,posit)
        
      }
      
      # Cálculo de la medida promedio #
      #-------------------------------#
      
      mp_etiq <- rbind(bd_año1,bd_año2) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        select(año ,tipo_esp,valor)
      
      # Base integrada #
      #----------------#
      
      niveles <- rbind(bd_año1,bd_año2) %>%
        filter(tipo %in% "Nivel de logro") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error))  %>% 
        left_join(pos, by = c("año","tipo_esp")) %>% 
        mutate(etiq = puntocoma2(valor,1)) %>% 
        mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                 TRUE ~ valor)) %>% 
        mutate(tipo_esp = factor(tipo_esp, 
                                 levels = c("Previo al inicio",
                                            "En inicio",
                                            "En proceso",
                                            "Satisfactorio")))
      
      if (totniv == 3){
        
        # Gráfico #
        #---------#
        
        ggplot(niveles, aes(x = año, y = valor, fill = tipo_esp)) + 
          geom_col(width = 0.3) +
          geom_segment(aes(x=0.8,
                           xend=2.2,
                           y=0, 
                           yend=0), 
                       color="#252525",
                       lwd = 0.2) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) + 
          scale_x_discrete(expand = expansion(mult = 0, add = c(2.5,2))) +
          theme_bw() +
          theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525")
          ) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.5,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = valor, 
                        y = 90, 
                        x = año), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(aes(x=0.69,
                        y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(mapping=aes(x=0.70,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525")
        
        
      } else if ( totniv == 4){
        
        # Gráfico #
        #---------#
        
        ggplot(niveles, aes(x = año, y = valor, fill = tipo_esp)) + 
          geom_col(width = 0.3) +
          geom_segment(aes(x=0.8,
                           xend=2.2,
                           y=0, 
                           yend=0), 
                       color="#252525",
                       lwd = 0.2) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) + 
          scale_x_discrete(expand = expansion(mult = 0, add = c(2.5,2))) +
          theme_bw() +
          theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525")
          ) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.5,
                    color = case_when(niveles$tipo_esp %in% "En inicio" & abs(niveles$valor) >= 12 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = valor, 
                        y = 90, 
                        x = año), 
                    size = 4.0, 
                    inherit.aes = FALSE) +
          geom_text(aes(x=0.69,
                        y=0.00,
                        label="Niveles de logro (%)"),
                    size=3.0,
                    alpha=0.2,
                    angle = 90,
                    color = "#252525") +
          geom_text(mapping=aes(x=0.70,
                                y=90,
                                label="Medida \npromedio"),
                    size=3.0,
                    alpha=0.2,
                    color = "#252525")
        
      }
      
    }
    
  } else if (nom_tipo == "Tipo 2"){
    
    # Identificadores de tipo de evaluación (muestral o censal) #
    #-----------------------------------------------------------#
    
    subt1 <- sum(is.na(bd_año1_estrat$error))
    subt2 <- sum(is.na(bd_año2_estrat$error))
    
    # Base de datos inicial #
    #-----------------------# 
    
    bd1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
      filter(tipo %in% "Nivel de logro") %>% 
      filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
      select(año,estrato1_1,estrato2 ,tipo_esp,valor)
    
    # Identificador de cantidad de niveles de logro #
    #-----------------------------------------------#
    
    totniv <- bd1 %>% 
      arrange(tipo_esp) %>% 
      select(tipo_esp) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    nomniv <- bd1 %>% 
      arrange(tipo_esp) %>% 
      select(tipo_esp) %>% 
      pull() %>%
      unique()
    
    nom_año <- unique(bd_año2_estrat$año)
    
    # Condicional según tipo de evaluaciones que se analizan #
    #--------------------------------------------------------#
    
    if (subt1 == 0 & subt2 == 0){
      
      # Diferencia estadísticamente significativa en nivel satisfactorio dentro de los estratos #
      #-----------------------------------------------------------------------------------------#
      
      val_dif_nl <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,estrato2,tipo_esp,dif_est_sig) 
      
      val_dif_mp <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,estrato2,tipo_esp,dif_est_sig) 
      
      # Cálculo de las medidas promedios #
      #----------------------------------#
      
      mp_etiq <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        left_join(val_dif_mp, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
        select(año,estrato1_1, estrato2,valor, dif_est_sig) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1,
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*"),
                                TRUE ~ puntocoma2(valor,0)))
      
      if (totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = case_when(n_in >= 12 ~ n_pr + n_in/2, 
                                    TRUE ~  n_pr + n_in + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1) %>% 
          pivot_longer(cols = 7:9,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,estrato1_1,estrato2,tipo_esp,posit)
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        t1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(tipo %in% "Nivel de logro") %>% 
          left_join(val_dif_nl, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          left_join(pos, by = c("año","estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*"),
                                  TRUE ~ puntocoma2(valor,1))) %>% 
          mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)) %>% 
          mutate(tipo_esp = factor(tipo_esp,
                                   levels = c("En inicio",
                                              "En proceso",
                                              "Satisfactorio"))) %>%  
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área")))
        
        # Gráfico #
        #---------#
        
        ggplot(t1, aes(x = interaction(paste0(año,"\n"),estrato2), y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1), 
                     nrow = 1, 
                     scales = "free_x") + 
          geom_bar(stat = "identity",
                   width = 0.6) +
          guides(x = "axis_nested") + 
          theme_guide(bracket = element_blank()) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) +
          theme(panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525",
                                           margin = margin(t = 3, unit = "pt")),
                strip.background = element_rect(fill = "transparent",
                                                color = NA),
                strip.text = element_text(size = 15, 
                                          color = "#252525",
                                          hjust = 0.5),
                panel.spacing = unit(-3, "lines")
          ) +
          geom_segment(aes(y = 0,
                           yend = 0,
                           x = 0.0,
                           xend = 5),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) + 
          geom_vline(data = t1 %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(4.55)), 
                     linetype = "dashed",
                     lwd = 0.4,
                     color = "#252525",
                     alpha = 0.3,
                     show.legend = F) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(t1$tipo_esp %in% "En inicio" & abs(t1$valor) >= 7 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = interaction(paste0(año,"\n"),estrato2)), 
                    size = 4.0, 
                    inherit.aes = FALSE)
        
      } else if (totniv == 4){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 8:11,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,estrato1_1,estrato2,tipo_esp,posit)
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        t1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(tipo %in% "Nivel de logro") %>% 
          left_join(val_dif_nl, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          left_join(pos, by = c("año","estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*"),
                                  TRUE ~ puntocoma2(valor,1))) %>% 
          mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)) %>% 
          mutate(tipo_esp = factor(tipo_esp,
                                   levels = c("Previo al inicio",
                                              "En inicio",
                                              "En proceso",
                                              "Satisfactorio"))) %>%  
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área")))
        
        # Gráfico #
        #---------#
        
        ggplot(t1, aes(x = interaction(paste0(año,"\n"),estrato2), y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1), 
                     nrow = 1, 
                     scales = "free_x") + 
          geom_bar(stat = "identity",
                   width = 0.6) +
          guides(x = "axis_nested") + 
          theme_guide(bracket = element_blank()) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) +
          theme(panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525",
                                           margin = margin(t = 3, unit = "pt")),
                strip.background = element_rect(fill = "transparent",
                                                color = NA),
                strip.text = element_text(size = 15, 
                                          color = "#252525",
                                          hjust = 0.5),
                panel.spacing = unit(-3, "lines")
          ) +
          geom_segment(aes(y = 0,
                           yend = 0,
                           x = 0.0,
                           xend = 5),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) + 
          geom_vline(data = t1 %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(4.55)), 
                     linetype = "dashed",
                     lwd = 0.4,
                     color = "#252525",
                     alpha = 0.3,
                     show.legend = F) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(t1$tipo_esp %in% "En inicio" & abs(t1$valor) >= 7 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = interaction(paste0(año,"\n"),estrato2)), 
                    size = 4.0, 
                    inherit.aes = FALSE)
        
      }
      
    } else if ((subt1 == 0 & subt2 > 0) | (subt1 > 0 & subt2 == 0)){
      
      # Diferencia estadísticamente significativa en nivel satisfactorio dentro de los estratos #
      #-----------------------------------------------------------------------------------------#
      
      val_dif_nl <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(tipo %in% "Nivel de logro") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error)) %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,estrato2,tipo_esp,dif_est_sig) 
      
      val_dif_mp <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error)) %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,estrato2,tipo_esp,dif_est_sig) 
      
      # Cálculo de las medidas promedios #
      #----------------------------------#
      
      mp_etiq <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        left_join(val_dif_mp, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
        select(año,estrato1_1, estrato2,valor, dif_est_sig) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1,
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*"),
                                TRUE ~ puntocoma2(valor,0)))
      
      if (totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = case_when(n_in >= 12 ~ n_pr + n_in/2, 
                                    TRUE ~  n_pr + n_in + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1) %>% 
          pivot_longer(cols = 7:9,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,estrato1_1,estrato2,tipo_esp,posit)
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        t1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(tipo %in% "Nivel de logro") %>% 
          left_join(val_dif_nl, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          left_join(pos, by = c("año","estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*"),
                                  TRUE ~ puntocoma2(valor,1))) %>% 
          mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)) %>% 
          mutate(tipo_esp = factor(tipo_esp,
                                   levels = c("Previo al inicio",
                                              "En inicio",
                                              "En proceso",
                                              "Satisfactorio"))) %>%  
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área")))
        
        # Gráfico #
        #---------#
        
        ggplot(t1, aes(x = interaction(paste0(año,"\n"),estrato2), y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1), 
                     nrow = 1, 
                     scales = "free_x") + 
          geom_bar(stat = "identity",
                   width = 0.6) +
          guides(x = "axis_nested") + 
          theme_guide(bracket = element_blank()) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) +
          theme(panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525",
                                           margin = margin(t = 3, unit = "pt")),
                strip.background = element_rect(fill = "transparent",
                                                color = NA),
                strip.text = element_text(size = 15, 
                                          color = "#252525",
                                          hjust = 0.5),
                panel.spacing = unit(-3, "lines")
          ) +
          geom_segment(aes(y = 0,
                           yend = 0,
                           x = 0.0,
                           xend = 5),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) + 
          geom_vline(data = t1 %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(4.55)), 
                     linetype = "dashed",
                     lwd = 0.4,
                     color = "#252525",
                     alpha = 0.3,
                     show.legend = F) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(t1$tipo_esp %in% "En inicio" & abs(t1$valor) >= 7 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = interaction(paste0(año,"\n"),estrato2)), 
                    size = 4.0, 
                    inherit.aes = FALSE)
        
        
      } else if (totniv == 4){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 8:11,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,estrato1_1,estrato2,tipo_esp,posit)
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        t1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(tipo %in% "Nivel de logro") %>% 
          left_join(val_dif_nl, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
          left_join(pos, by = c("año","estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq = case_when(año %in% nom_año & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*"),
                                  TRUE ~ puntocoma2(valor,1))) %>% 
          mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)) %>% 
          mutate(tipo_esp = factor(tipo_esp,
                                   levels = c("Previo al inicio",
                                              "En inicio",
                                              "En proceso",
                                              "Satisfactorio"))) %>%  
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área")))
        
        # Gráfico #
        #---------#
        
        
        ggplot(t1, aes(x = interaction(paste0(año,"\n"),estrato2), y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1), 
                     nrow = 1, 
                     scales = "free_x") + 
          geom_bar(stat = "identity",
                   width = 0.6) +
          guides(x = "axis_nested") + 
          theme_guide(bracket = element_blank()) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) +
          theme(panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525",
                                           margin = margin(t = 3, unit = "pt")),
                strip.background = element_rect(fill = "transparent",
                                                color = NA),
                strip.text = element_text(size = 15, 
                                          color = "#252525",
                                          hjust = 0.5),
                panel.spacing = unit(-3, "lines")
          ) +
          geom_segment(aes(y = 0,
                           yend = 0,
                           x = 0.0,
                           xend = 5),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) + 
          geom_vline(data = t1 %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(4.55)), 
                     linetype = "dashed",
                     lwd = 0.4,
                     color = "#252525",
                     alpha = 0.3,
                     show.legend = F) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(t1$tipo_esp %in% "En inicio" & abs(t1$valor) >= 7 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = interaction(paste0(año,"\n"),estrato2)), 
                    size = 4.0, 
                    inherit.aes = FALSE)        
      }
      
    } else if (subt1 > 0 & subt2 > 0) {
      
      # Cálculo de las medidas promedios #
      #----------------------------------#
      
      mp_etiq <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(!tipo %in% "Nivel de logro") %>% 
        filter(!estrato1_1 %in% c("caracteristica2","nom_dre")) %>% 
        select(año,estrato1_1, estrato2,valor) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1,
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(etiq =puntocoma2(valor,0))
      
      if (totniv == 3){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = case_when(n_in >= 12 ~ n_pr + n_in/2, 
                                    TRUE ~  n_pr + n_in + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1) %>% 
          pivot_longer(cols = 7:9,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,estrato1_1,estrato2,tipo_esp,posit)
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        t1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(tipo %in% "Nivel de logro") %>% 
          left_join(pos, by = c("año","estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq =  puntocoma2(valor,1)) %>% 
          mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)) %>% 
          mutate(tipo_esp = factor(tipo_esp,
                                   levels = c("Previo al inicio",
                                              "En inicio",
                                              "En proceso",
                                              "Satisfactorio"))) %>%  
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área")))
        
        # Gráfico #
        #---------#
        
        ggplot(t1, aes(x = interaction(paste0(año,"\n"),estrato2), y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1), 
                     nrow = 1, 
                     scales = "free_x") + 
          geom_bar(stat = "identity",
                   width = 0.6) +
          guides(x = "axis_nested") + 
          theme_guide(bracket = element_blank()) +
          scale_fill_manual(values = c(color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) +
          theme(panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525",
                                           margin = margin(t = 3, unit = "pt")),
                strip.background = element_rect(fill = "transparent",
                                                color = NA),
                strip.text = element_text(size = 15, 
                                          color = "#252525",
                                          hjust = 0.5),
                panel.spacing = unit(-3, "lines")
          ) +
          geom_segment(aes(y = 0,
                           yend = 0,
                           x = 0.0,
                           xend = 5),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) + 
          geom_vline(data = t1 %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(4.55)), 
                     linetype = "dashed",
                     lwd = 0.4,
                     color = "#252525",
                     alpha = 0.3,
                     show.legend = F) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(t1$tipo_esp %in% "En inicio" & abs(t1$valor) >= 7 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = interaction(paste0(año,"\n"),estrato2)), 
                    size = 4.0, 
                    inherit.aes = FALSE)
        
      } else if (totniv == 4){
        
        # Cálculo de las posiciones de las etiquetas #
        #--------------------------------------------#
        
        pos <- bd1 %>% 
          pivot_wider(names_from = tipo_esp,
                      values_from = valor) %>% 
          rename(n_pi = `Previo al inicio`, n_in = `En inicio`,n_pr = `En proceso`, n_sa = `Satisfactorio`) %>% 
          mutate(pos_sa = case_when(n_sa >= 12 ~ n_sa/2,
                                    TRUE ~ n_sa + 11),
                 pos_pr = n_pr/2,
                 pos_in = n_pr + n_in/2,
                 pos_pi = case_when(n_pi >= 12 ~ n_pr + n_in + n_pi/2,
                                    TRUE ~ n_pr + n_in + n_pi + 11)) %>%
          mutate(pos_pr = pos_pr*-1,
                 pos_in = pos_in*-1,
                 pos_pi = pos_pi*-1) %>% 
          pivot_longer(cols = 8:11,
                       names_to = "nivel",
                       values_to = "posit") %>% 
          mutate(tipo_esp = case_when(nivel %in% "pos_pi" ~ "Previo al inicio",
                                      nivel %in% "pos_in" ~ "En inicio",
                                      nivel %in% "pos_pr" ~ "En proceso",
                                      TRUE ~ "Satisfactorio")) %>% 
          select(año,estrato1_1,estrato2,tipo_esp,posit)
        
        
        # Integración de las bases de logros y posición de etiquetas #
        #------------------------------------------------------------#
        
        t1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
          filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
          filter(tipo %in% "Nivel de logro") %>% 
          left_join(pos, by = c("año","estrato1_1","estrato2","tipo_esp")) %>% 
          mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                      TRUE ~ estrato2)) %>% 
          mutate(etiq =  puntocoma2(valor,1)) %>% 
          mutate(valor = case_when(!tipo_esp %in% "Satisfactorio" ~ valor*-1,
                                   TRUE ~ valor)) %>% 
          mutate(tipo_esp = factor(tipo_esp,
                                   levels = c("Previo al inicio",
                                              "En inicio",
                                              "En proceso",
                                              "Satisfactorio"))) %>%  
          mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                        estrato1_1 %in% "gestion2" ~ "Gestión",
                                        estrato1_1 %in% "area" ~ "Área",
                                        estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                        TRUE ~ NA_character_)) %>% 
          mutate(estrato1_1 = factor(estrato1_1,
                                     levels = c("Sexo",
                                                "Gestión",
                                                "Área",
                                                "Gestión y área")))
        
        # Gráfico #
        #---------#
        
        ggplot(t1, aes(x = interaction(paste0(año,"\n"),estrato2), y = valor, fill = tipo_esp)) + 
          facet_wrap(vars(estrato1_1), 
                     nrow = 1, 
                     scales = "free_x") + 
          geom_bar(stat = "identity",
                   width = 0.6) +
          guides(x = "axis_nested") + 
          theme_guide(bracket = element_blank()) +
          scale_fill_manual(values = c(color_pre_in,color_inicio,color_proces,color_satisf)) +
          scale_y_continuous(limits = c(-100,100)) +
          theme(panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12,
                                           color = "#252525"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 12,
                                           color = "#252525",
                                           margin = margin(t = 3, unit = "pt")),
                strip.background = element_rect(fill = "transparent",
                                                color = NA),
                strip.text = element_text(size = 15, 
                                          color = "#252525",
                                          hjust = 0.5),
                panel.spacing = unit(-3, "lines")
          ) +
          geom_segment(aes(y = 0,
                           yend = 0,
                           x = 0.0,
                           xend = 5),
                       linetype = "solid",
                       color = "#252525",
                       lwd = 0.3,
                       alpha = 0.5) + 
          geom_vline(data = t1 %>% filter(!estrato1_1 %in% "Gestión y área"),
                     aes(xintercept = c(4.55)), 
                     linetype = "dashed",
                     lwd = 0.4,
                     color = "#252525",
                     alpha = 0.3,
                     show.legend = F) +
          geom_text(aes(label = etiq,
                        y = posit),
                    size = 4.0,
                    color = case_when(t1$tipo_esp %in% "En inicio" & abs(t1$valor) >= 7 ~ "white",
                                      TRUE ~ "#252525")) +
          geom_text(data = mp_etiq, 
                    aes(label = etiq, 
                        y = 90, 
                        x = interaction(paste0(año,"\n"),estrato2)), 
                    size = 4.0, 
                    inherit.aes = FALSE)
        
      }
    }
  } else if(nom_tipo == "Tipo 3"){
    
    # Año más actual #
    #----------------#
    
    nom_añop <- unique(bd_año1_estrat$año)
    nom_añoa <- unique(bd_año2_estrat$año)
    
    # Datos del grado y el área evaluada #
    #------------------------------------#
    
    nom_gradoeval <- unique(bd_año2_estrat$grado_eval)
    nom_area <- unique(bd_año2_estrat$area_eval)
    
    # Etiquetas de texto con los nombres de los niveles de logro #
    #------------------------------------------------------------#
    
    text_sat <- textGrob("Satisfactorio", 
                         gp = gpar(fontsize = 16.5, 
                                   col = color_satisf
                         ),
                         just = "left")
    text_pro <- textGrob("En proceso", 
                         gp = gpar(fontsize = 16.5, 
                                   col = color_proces
                         ),
                         just = "left")
    text_ini <- textGrob("En inicio", 
                         gp = gpar(fontsize = 16.5, 
                                   col = color_inicio
                         ),
                         just = "left")
    text_pin <- textGrob("Previo al inicio", 
                         gp = gpar(fontsize = 16.5, 
                                   col = color_pre_in
                         ),
                         just = "left")
    
    # Patrón de colores #
    #-------------------#
    
    color_puntos <- c("#B2D9FF","#0077E6","#B2D9FF","#0077E6","#B2D9FF","#0077E6","#B2D9FF","#0077E6")
    
    # Identificadores de tipo de evaluación (muestral o censal) #
    #-----------------------------------------------------------#
    
    subt1 <- sum(is.na(bd_año1_estrat$error))
    subt2 <- sum(is.na(bd_año2_estrat$error))
    
    # Condicional según tipo de evaluaciones que se analizan #
    #--------------------------------------------------------#
    
    if (subt1 == 0 & subt2 == 0){
      
      # Diferencia estadísticamente significativa en nivel satisfactorio entre años dentro de los estratos #
      #----------------------------------------------------------------------------------------------------#
      
      val_dif_mp <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(tipo %in% "Medida promedio") %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        select(-tipo_esp) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[4]] - .[[3]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[6]]^2 + .[[5]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,estrato2,dif_est_sig) 
      
      # Base de datos integrada para graficar #
      #---------------------------------------#
      
      temp1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        filter(tipo %in% "Medida promedio") %>% 
        left_join(val_dif_mp, by = c("estrato1_1","estrato2")) %>% 
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1, 
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(etiq = case_when(año == nom_añoa & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*") ,
                                TRUE ~ puntocoma2(valor,0)))
      
    } else if ((subt1 == 0 & subt2 > 0) | (subt1 > 0 & subt2 == 0)){
      
      # Corrección de errores en la base cuando es censal #
      #---------------------------------------------------#
      
      bd_año1_mdf <- bd_año1_estrat %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error))
      
      bd_año2_mdf <- bd_año2_estrat %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error))
      
      # Diferencia estadísticamente significativa en nivel satisfactorio entre años dentro de los estratos #
      #----------------------------------------------------------------------------------------------------#
      
      val_dif_mp <- rbind(bd_año1_mdf,bd_año2_mdf) %>% 
        filter(tipo %in% "Medida promedio") %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        select(-tipo_esp) %>% 
        pivot_wider(
          names_from = año,               # Los valores únicos de 'año' formarán parte del nombre de las nuevas columnas
          values_from = c(valor, error),  # Los valores de 'valor' y 'error' serán distribuidos en las nuevas columnas
          names_glue = "{.value}_{año}"   # Controla el formato del nombre de las nuevas columnas: valor_2022, error_2022, etc.
        ) %>% 
        mutate(diferencia = .[[4]] - .[[3]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[6]]^2 + .[[5]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        select(estrato1_1,estrato2,dif_est_sig) 
      
      # Base de datos integrada para graficar #
      #---------------------------------------#
      
      temp1 <- rbind(bd_año1_mdf,bd_año2_mdf) %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        filter(tipo %in% "Medida promedio") %>% 
        left_join(val_dif_mp, by = c("estrato1_1","estrato2")) %>% 
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1, 
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(etiq = case_when(año == nom_añoa & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*") ,
                                TRUE ~ puntocoma2(valor,0)))
      
    } else if (subt1 > 0 & subt2 > 0) {

      # Corrección de errores en la base cuando es censal #
      #---------------------------------------------------#
      
      bd_año1_mdf <- bd_año1_estrat %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error))
      
      bd_año2_mdf <- bd_año2_estrat %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error))
      
      # Base de datos integrada para graficar #
      #---------------------------------------#
      
      temp1 <- rbind(bd_año1_mdf,bd_año2_mdf) %>% 
        filter(!estrato1_1 %in% c("nom_dre","caracteristica2")) %>% 
        filter(tipo %in% "Medida promedio") %>% 
        mutate(estrato1_1 = case_when(estrato1_1 %in% "sexo" ~ "Sexo",
                                      estrato1_1 %in% "gestion2" ~ "Gestión",
                                      estrato1_1 %in% "area" ~ "Área",
                                      estrato1_1 %in% "gestion3" ~ "Gestión y área",
                                      TRUE ~ NA_character_)) %>% 
        mutate(estrato1_1 = factor(estrato1_1, 
                                   levels = c("Sexo",
                                              "Gestión",
                                              "Área",
                                              "Gestión y área"))) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "Pública urbana" ~ "Pública \nurbana",
                                    TRUE ~ estrato2)) %>% 
        mutate(etiq = puntocoma2(valor,0))
      
    }
    
    # Argumentos condicionales al grado y el área evaluado #
    #------------------------------------------------------#
    
    if (nom_gradoeval %in% "2.° grado de primaria" & nom_area %in% "Lectura"){
      
      limites <- c(400,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(458.39, 583.66) # (Menor a 458.39; mayor o igual a 458.39 y menor a 583.66; Mayor o igual a 583.66)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      annotation_pin <- NULL
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de primaria" & nom_area %in% "Matemática") {
      
      limites <- c(400,700)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(512.22, 639.21)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (limites[[2]] - puntos_corte[[2]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      annotation_pin <- NULL
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Lectura") {
      
      limites <- c(300,600)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(356.92, 444.72, 522.03)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Matemática") {
      
      limites <- c(300,600)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(351.90, 422.21, 526.46)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "6.° grado de primaria" & nom_area %in% "Lectura") {
      
      limites <- c(400,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(445.46, 522.17, 579.14)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "6.° grado de primaria" & nom_area %in% "Matemática") {
      
      limites <- c(400,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(432.32, 526.61, 603.41)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Lectura") {
      
      limites <- c(400,700)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(505.14, 580.61, 641.25)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Matemática") {
      
      limites <- c(400,700)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(519.67, 595.96, 649.38)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Ciencias sociales") {
      
      limites <- c(350,650)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(425.93, 500.05, 607.28)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    } else if (nom_gradoeval %in% "2.° grado de secundaria" & nom_area %in% "Ciencia y tecnología") {
      
      limites <- c(300,700)
      limit_estrato <- "Gestión y área"
      puntos_corte <- c(374.60, 509.58, 628.48)
      
      annotation_sat <- annotation_custom(
        grob = text_sat,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 , ymax = puntos_corte[[3]] + (limites[[2]] - puntos_corte[[3]])/2 
      ) 
      annotation_pro <- annotation_custom(
        grob = text_pro,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 , ymax = puntos_corte[[2]] + (puntos_corte[[3]]- puntos_corte[[2]])/2 
      ) 
      annotation_ini <- annotation_custom(
        grob = text_ini,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2 , ymax = puntos_corte[[1]] + (puntos_corte[[2]]- puntos_corte[[1]])/2
      )
      annotation_pin <- annotation_custom(
        grob = text_pin,
        xmin = 5.2, xmax = 0, # Ajustar para que esté después del último facet
        ymin = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2 , ymax = limites[[1]] + (puntos_corte[[1]]- limites[[1]])/2
      )
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]], 0),
                   color = color_inicio,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]], 0),
                   color = color_proces,
                   alpha = 0.5,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]], 0),
                   color = color_satisf,
                   alpha = 0.5,
                   lwd = 0.2)
      )
      
    }
    
    # Gráfico #
    #---------#
    
    ggplot(temp1, aes(x = estrato2, y = valor, color = factor(año), group = interaction(estrato2, año))) +
      facet_wrap(vars(estrato1_1),
                 nrow = 1,
                 scales = "free_x") + 
  geom_point(position = position_dodge(width = 0.5), 
             size = 3.2) + 
      hlines +
      scale_y_continuous(limits = limites,
                         breaks = c(redondear(puntos_corte,0))) +
      scale_color_manual(values = color_puntos) +
      theme_bw() + 
      theme(panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14,
                                       color = "#252525"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 14,
                                     color = "#252525"),
            panel.spacing = unit(0, "lines"),
            strip.background = element_rect(fill = "transparent",
                                            color = NA),
            strip.text = element_text(size = 15, 
                                      color = "#252525",
                                      hjust = 0.5),
            plot.margin = unit(c(0, 1.85, 0, 0.50), 
                               "inches")
      ) + 
      geom_text(aes(label = etiq),
                vjust = -1.0,   
                hjust = case_when(temp1$año %in% nom_añoa ~ -0.1,
                                  TRUE ~ 1.2),
                size = 4.5,
                color = "#252525",
                parse = FALSE
      )  +
      geom_vline(data = temp1 %>% filter(!estrato1_1 %in% limit_estrato),
                 aes(xintercept = c(2.58)), 
                 linetype = "dashed",
                 lwd = 0.4,
                 color = "#252525",
                 alpha = 0.3,
                 show.legend = F) +
      annotation_sat +
      annotation_pro + 
      annotation_ini + 
      annotation_pin +
      coord_cartesian(clip = "off") 
    
  } else if (nom_tipo == "Tipo 4"){
    
    # Identificadores para filtros #
    #------------------------------#
    
    nom_gradoeval <- unique(bd_año2_estrat$grado_eval) # Identificador de grado evaluado
    nom_area <- unique(bd_año2_estrat$area_eval)       # Identificador de área evaluada
    
    nom_añop <- unique(bd_año1_estrat$año)
    nom_añoa <- unique(bd_año2_estrat$año)

   text_aum <- textGrob("▲ Aumentó", 
                         gp = gpar(fontsize = 13, 
                                   col = "#74c476"
                                   ),
                         just = "center")
        
    text_noc <- textGrob("─ Sin cambio", 
                             gp = gpar(fontsize = 13, 
                                       col = "#252525"
                             ),
                             just = "center")
        
    text_dis <- textGrob("▼ Disminuyó", 
                             gp = gpar(fontsize = 13, 
                                       col = "#fb6a4a"
                             ),
                             just = "center")
        
    annotation_aum <- annotation_custom(
                  grob = text_aum,
                  xmin = 20.0, xmax = 0,
                  ymin = -45, ymax = -45
                )
        
    annotation_noc <- annotation_custom(
                  grob = text_noc,
                  xmin = 27.5, xmax = 0, 
                  ymin = -45, ymax = -45
                ) 

    annotation_dis <- annotation_custom(
                  grob = text_dis,
                  xmin = 35.1, xmax = 0, 
                  ymin = -45, ymax = -45
                )
    
    if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Lectura"){
      
      limites <- c(250,600)
      puntos_corte <- c(356.92, 444.72, 522.03)
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]],0),
                   color = color_inicio,
                   alpha = 0.8,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]],0),
                   color = color_proces,
                   alpha = 0.8,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]],0),
                   color = color_satisf,
                   alpha = 0.8,
                   lwd = 0.2)
      )

      val_dif_mp <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(estrato1_1 %in% c("nom_dre")) %>% 
        filter(tipo %in% "Medida promedio") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error)) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               
          values_from = c(valor, error),  
          names_glue = "{.value}_{año}"   
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        mutate(indipos = case_when(.[[5]] - .[[4]] >= 0 ~ 1,
                                   TRUE ~ 0)) %>% 
        mutate(simbolo = case_when(diferencia > 0 & dif_est_sig %in% "Hay diferencia" ~ "▲", 
                                   diferencia < 0 & dif_est_sig %in% "Hay diferencia" ~ "▼",
                                   TRUE ~ "─" )) %>% 
        mutate(diferencia1 = redondear(.[[5]],0) - redondear(.[[4]],0)) %>% 
        select(estrato1_1,estrato2,tipo_esp,dif_est_sig,diferencia1,indipos,simbolo) 
      
      bd1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(estrato1_1 %in% c("nom_dre")) %>% 
        filter(tipo_esp %in% c("Medida promedio")) %>% 
        left_join(val_dif_mp, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
        mutate(etiq = case_when(año %in% nom_añoa & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*"),
                                TRUE ~ puntocoma2(valor,0))) %>% 
        mutate(estrato2 = str_replace_all(estrato2, " ","")) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "LaLibertad" ~ "La Libertad",
                                    estrato2 %in% "LimaMetropolitana" ~ "Lima Metropolitana",
                                    estrato2 %in% "LimaProvincias" ~ "Lima Provincias",
                                    estrato2 %in% "MadredeDios" ~ "Madre de Dios",
                                    estrato2 %in% "SanMartín" ~ "San Martín",
                                    TRUE ~ estrato2)) %>% 
        mutate(etiq_dif = abs(diferencia1))
      
      orden1 <- bd1 %>%
        filter(año %in% nom_añoa) %>% 
        select(estrato2) %>%
        mutate(orden_aux = stri_trans_general(estrato2, "Latin-ASCII")) %>%  # Crear columna auxiliar sin tildes
        arrange(orden_aux) %>%  # Ordenar por la versión sin tildes
        select(estrato2) %>%  # Eliminar la columna auxiliar para mantener los nombres originales
        pull() %>%
        unique()

      ggplot(bd1, aes(x = factor(estrato2,levels = orden1), y = valor, color = año)) + 
        hlines + 
        geom_point(size = 3.0) + 
        scale_y_continuous(limits = limites,
                           breaks = redondear(puntos_corte,0)) +
        scale_color_manual(values = c("#B2D9FF","#0077E6"),
                           breaks = c("2023","2024")) +
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 13,
                                         color = "#252525"),
              legend.margin = margin(t = 10, unit = "pt"),
              axis.title = element_blank(),
              axis.text.x = element_text(angle = 90,
                                         hjust = 1.0,
                                         vjust = 0.5,
                                         size = 13),
              axis.text.y = element_text(size = 12,
                                         color = "#252525"),
              axis.ticks = element_blank(),
              plot.margin = unit(c(0, 0, 1.5, 0), "cm")
        ) +
        geom_text(aes(label = etiq,
                      vjust = case_when(año %in% nom_añoa & indipos == 1 ~ -1.3,
                                        año %in% nom_añop & indipos == 0 ~ -1.3,
                                        TRUE ~ 2.3),
                      hjust = 0.5),
                  color = "#252525",
                  size = 4.0) + 
        geom_richtext(aes(label = puntocoma2(etiq_dif,0),
                          x = factor(estrato2,levels = orden1),
                          y = limites[[1]]),
                      color = case_when(bd1$diferencia > 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#74c476",
                                        bd1$diferencia < 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#fb6a4a",
                                        TRUE ~ "#252525"),
                      size = 3.5,
                      show.legend = F) +
        geom_text(aes(label = simbolo,
                      x = factor(estrato2,levels = orden1),
                      y = limites[[1]] + 30),
                  color = case_when(bd1$diferencia > 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#74c476",
                                    bd1$diferencia < 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#fb6a4a",
                                    TRUE ~ "#252525"),
                  size = 4.5,
                  show.legend = F) +
        annotation_aum +
        annotation_noc +
        annotation_dis +
        coord_cartesian(clip = "off") 
      
    } else if (nom_gradoeval %in% "4.° grado de primaria" & nom_area %in% "Matemática") {
      
      limites <- c(250,600)
      puntos_corte <- c(351.90, 422.21, 526.46)
      
      hlines <- list(
        geom_hline(yintercept = redondear(puntos_corte[[1]],0),
                   color = color_inicio,
                   alpha = 0.8,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[2]],0),
                   color = color_proces,
                   alpha = 0.8,
                   lwd = 0.2),
        geom_hline(yintercept = redondear(puntos_corte[[3]],0),
                   color = color_satisf,
                   alpha = 0.8,
                   lwd = 0.2)
      )
      
      val_dif_mp <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(estrato1_1 %in% c("nom_dre")) %>% 
        filter(tipo %in% "Medida promedio") %>% 
        mutate(error = case_when(is.na(error) ~ 0,
                                 TRUE ~ error)) %>% 
        select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
        pivot_wider(
          names_from = año,               
          values_from = c(valor, error),  
          names_glue = "{.value}_{año}"   
        ) %>% 
        mutate(diferencia = .[[5]] - .[[4]]) %>% 
        mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
        mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
        mutate(indipos = case_when(.[[5]] - .[[4]] >= 0 ~ 1,
                                   TRUE ~ 0)) %>% 
        mutate(simbolo = case_when(diferencia > 0 & dif_est_sig %in% "Hay diferencia" ~ "▲", 
                                   diferencia < 0 & dif_est_sig %in% "Hay diferencia" ~ "▼",
                                   TRUE ~ "─" )) %>% 
        mutate(diferencia1 = redondear(.[[5]],0) - redondear(.[[4]],0)) %>% 
        select(estrato1_1,estrato2,tipo_esp,dif_est_sig,diferencia1,indipos,simbolo) 
      
      bd1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
        filter(estrato1_1 %in% c("nom_dre")) %>% 
        filter(tipo_esp %in% c("Medida promedio")) %>% 
        left_join(val_dif_mp, by = c("estrato1_1","estrato2","tipo_esp")) %>% 
        mutate(etiq = case_when(año %in% nom_añoa & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,0),"*"),
                                TRUE ~ puntocoma2(valor,0))) %>% 
        mutate(estrato2 = str_replace_all(estrato2, " ","")) %>% 
        mutate(estrato2 = case_when(estrato2 %in% "LaLibertad" ~ "La Libertad",
                                    estrato2 %in% "LimaMetropolitana" ~ "Lima Metropolitana",
                                    estrato2 %in% "LimaProvincias" ~ "Lima Provincias",
                                    estrato2 %in% "MadredeDios" ~ "Madre de Dios",
                                    estrato2 %in% "SanMartín" ~ "San Martín",
                                    TRUE ~ estrato2)) %>% 
        mutate(etiq_dif = abs(diferencia1))
      
        orden1 <- bd1 %>%
        filter(año %in% nom_añoa) %>% 
        select(estrato2) %>%
        mutate(orden_aux = stri_trans_general(estrato2, "Latin-ASCII")) %>%  # Crear columna auxiliar sin tildes
        arrange(orden_aux) %>%  # Ordenar por la versión sin tildes
        select(estrato2) %>%  # Eliminar la columna auxiliar para mantener los nombres originales
        pull() %>%
        unique()
      
      ggplot(bd1, aes(x = factor(estrato2,levels = orden1), y = valor, color = año)) + 
        hlines + 
        geom_point(size = 3.0) + 
        scale_y_continuous(limits = limites,
                           breaks = redondear(puntos_corte,0)) +
        scale_color_manual(values = c("#B2D9FF","#0077E6"),
                           breaks = c("2023","2024")) +
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 13,
                                         color = "#252525"),
              legend.margin = margin(t = 10, unit = "pt"),
              axis.title = element_blank(),
              axis.text.x = element_text(angle = 90,
                                         hjust = 1.0,
                                         vjust = 0.5,
                                         size = 13),
              axis.text.y = element_text(size = 12,
                                         color = "#252525"),
              axis.ticks = element_blank(),
              plot.margin = unit(c(0, 0, 1.5, 0), "cm")
        ) +
        geom_text(aes(label = etiq,
                      vjust = case_when(año %in% nom_añoa & indipos == 1 ~ -1.3,
                                        año %in% nom_añop & indipos == 0 ~ -1.3,
                                        TRUE ~ 2.3),
                      hjust = 0.5),
                  color = "#252525",
                  size = 4.0) + 
        geom_richtext(aes(label = puntocoma2(etiq_dif,0),
                          x = factor(estrato2,levels = orden1),
                          y = limites[[1]]),
                      color = case_when(bd1$diferencia > 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#74c476",
                                        bd1$diferencia < 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#fb6a4a",
                                        TRUE ~ "#252525"),
                      size = 3.5,
                      show.legend = F) +
        geom_text(aes(label = simbolo,
                      x = factor(estrato2,levels = orden1),
                      y = limites[[1]] + 30),
                  color = case_when(bd1$diferencia > 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#74c476",
                                    bd1$diferencia < 0 & bd1$dif_est_sig %in% "Hay diferencia" ~ "#fb6a4a",
                                    TRUE ~ "#252525"),
                  size = 4.5,
                  show.legend = F) +
        annotation_aum +
        annotation_noc +
        annotation_dis +
        coord_cartesian(clip = "off") 
      
    }
  
  } else if (nom_tipo == "Tipo 5"){
    
    nom_añoa <- unique(bd_año2_estrat$año)
    nom_añop <- unique(bd_año1_estrat$año)
    
    # Diferencia estadísticamente significativa entre años en niveles de logro en regiones #
    #--------------------------------------------------------------------------------------#
    
    val_dif_nl <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
      filter(estrato1_1 %in% c("nom_dre")) %>% 
      filter(!tipo %in% "Medida promedio") %>% 
      mutate(error = case_when(is.na(error) ~ 0,
                               TRUE ~ error)) %>% 
      select(año,estrato1_1,estrato2,tipo_esp,valor,error) %>% 
      pivot_wider(
        names_from = año,               
        values_from = c(valor, error),  
        names_glue = "{.value}_{año}"   
      ) %>% 
      mutate(diferencia = .[[5]] - .[[4]]) %>% 
      mutate(Zm = diferencia/(sqrt(.[[7]]^2 + .[[6]]^2))) %>% 
      mutate(pvalor = case_when(Zm < 0 ~  2*pnorm(Zm),
                                  TRUE ~ 2*(1 - pnorm(Zm)))) %>% 
        mutate(dif_est_sig =case_when(pvalor < 0.05 ~ "Hay diferencia",
                                      TRUE ~ "Sin diferencia")) %>% 
      select(estrato2,tipo_esp,dif_est_sig) 
    
    # Niveles de logro #
    #------------------#
    
    bd1 <- rbind(bd_año1_estrat,bd_año2_estrat) %>% 
      filter(tipo %in% "Nivel de logro") %>% 
      filter(estrato1_1 %in% "nom_dre") %>% 
      select(año,estrato2,tipo_esp,valor) %>%
      left_join(val_dif_nl, by = c("estrato2","tipo_esp")) %>% 
      mutate(estrato2 = str_replace_all(estrato2, " ","")) %>% 
      mutate(estrato2 = case_when(estrato2 %in% "LaLibertad" ~ "La Libertad",
                                  estrato2 %in% "LimaMetropolitana" ~ "Lima Metropolitana",
                                  estrato2 %in% "LimaProvincias" ~ "Lima Provincias",
                                  estrato2 %in% "MadredeDios" ~ "Madre de Dios",
                                  estrato2 %in% "SanMartín" ~ "San Martín",
                                  TRUE ~ estrato2)) %>% 
      mutate(etiq = case_when(año %in% nom_añoa & dif_est_sig %in% "Hay diferencia" ~ paste0(puntocoma2(valor,1),"*"),
                              TRUE ~ puntocoma2(valor,1))) %>% 
      mutate(valor = case_when(año %in% nom_añop ~ valor*-1,
                               TRUE ~ valor)) %>% 
      mutate(tipo_esp = factor(tipo_esp,
                               levels = c("Previo al inicio",
                                          "En inicio",
                                          "En proceso",
                                          "Satisfactorio")))
    
    # Orden de las regiones #
    #-----------------------# 
    
    orden1 <- bd1 %>% 
      filter(año %in% nom_añoa) %>% 
      select(estrato2) %>%
      mutate(orden_aux = stri_trans_general(estrato2, "Latin-ASCII")) %>%  # Crear columna auxiliar sin tildes
      arrange(orden_aux) %>%  # Ordenar por la versión sin tildes
      select(estrato2) %>%  # Eliminar la columna auxiliar para mantener los nombres originales
      pull() %>%
      unique() %>%
      rev()
    
    # Gráfico #
    #---------#
    
    ggplot(bd1, aes(x = valor, y = factor(estrato2, levels = orden1), fill =  interaction(año,tipo_esp))) + 
      facet_wrap(vars(tipo_esp),
                 nrow = 1) + 
      geom_col(width = 0.7) + 
      geom_vline(xintercept = 0,
                 color = "#252525") +
      scale_x_continuous(limits = c(-120,120)) + 
      scale_fill_manual(values = c(adjustcolor(color_pre_in, alpha.f = 0.4),color_pre_in,
                                   adjustcolor(color_inicio, alpha.f = 0.4),color_inicio,
                                   adjustcolor(color_proces, alpha.f = 0.4),color_proces,
                                   adjustcolor(color_satisf, alpha.f = 0.4),color_satisf)) +
      theme(panel.background = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 13,
                                       color = "#252525",
                                       hjust = 1),
            strip.background = element_rect(fill = "transparent",
                                            color = NA),
            strip.text = element_text(size = 13,
                                      color = "#252525"),
            plot.margin = unit(c(0, 0, 8.0, 0), 
                               "points")
      ) + 
      guides(y.sec = guide_axis_label_trans(~paste("", .x))) +
      geom_text(aes(label = etiq,
                    hjust = case_when(año %in% "2023" ~ 1.2,
                                      TRUE ~ -0.2)),
                size = 4.5,
                color = "#252525") +
      annotate(geom="text", 
               x=60, 
               y=0.3, 
               label=nom_añoa,
               size=3.0,
               colour = "#636363") +
      annotate(geom="text", 
               x=-60, 
               y=0.3, 
               label=nom_añop,
               size=3.0,
               colour = "#636363") + 
      coord_cartesian(clip = "off") 
    
    
    
    
  } else {
    
    return("Sigue probando")
  }
  
}
