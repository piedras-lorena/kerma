
cambiar_porcent<- function(x){
  if(is.na(x)){
    valor <- x
  } else valor <- percent(x)
  return(valor)
}

crear_tabla_reporte <- function(id_despacho){
  id_despacho <- as.character(id_despacho)

  # Importamos todas las tablas
  encuesta <- read_csv('../../data/interim/tabla_reporte_prueba.csv',
                        col_types = cols(.default = 'c')) %>% filter(userId == id_despacho)
  resumenes <- read_csv('../../data/interim/tabla_resumen_final.csv',
                        col_types = cols(.default = 'c'))
  mapeo_nombres <- read_csv('../../data/interim/mappeo_nombres_resumen_reporte.csv',
                            col_types = cols(.default = 'c'))

# Porcentaje de la encuesta no contestado
  porcent_na <- nrow(encuesta[is.na(encuesta$valor),]) / nrow(encuesta) * 100
  print(paste('porcentaje de na',porcent_na))

# Guardamos el nombre del despacho  
  tabla_reporte <- encuesta %>% full_join(resumenes) %>% left_join(mapeo_nombres, by = c('seccion'='nombre_nuevo')) %>% 
                   dplyr::select(-userId,-respondido) %>% dplyr::select(seccion, nombre_viejo, everything())

  tabla_reporte <- tabla_reporte %>% mutate(cuantil = ifelse(as.numeric(valor) <= as.numeric(`25%`), '<25%',
                                                           ifelse(as.numeric(valor) > as.numeric(`25%`) & as.numeric(valor) <= as.numeric(`50%`),'25%',
                                                                  ifelse(as.numeric(valor) > as.numeric(`50%`) & as.numeric(valor) <= as.numeric(`75%`),'50%',
                                                                         ifelse(is.na(`25%`),NA,'>75%')))),
                                          `VS PROM` = ifelse(as.numeric(valor) < as.numeric(Promedio),'Abajo','Arriba'))
  filas_numeric = c('Promedio','25%','50%','75%','Alto','Bajo','Porcentaje','porcent_hombres','porcent_mujeres',#'H%','M%','valor_1','valor_2',
                    'Personal total','Edad','Años de exp.','Tarifa')
  tabla_reporte[,filas_numeric] <- sapply(tabla_reporte[,filas_numeric], function(x) round(as.numeric(x),digits = 2)) 
  filtro_percent <- c('porcent_hombres','porcent_mujeres')
  tabla_reporte[filtro_percent] <- apply(tabla_reporte[filtro_percent],2, function(x) mapply(cambiar_porcent,x))
  return(tabla_reporte)
}

tabla_promedio_normal<- function(seccion,subseccion,porcent_h_m = 0){
  nombre_acronimo <- tabla_nombres_despachos %>% filter(id == id_despacho)
  sec_subsec <- paste(seccion,subseccion,sep = '_')
  if(porcent_h_m == 1){
  dt <- tabla_reporte %>% filter(grepl(sprintf('^%s_',sec_subsec),seccion)) %>%
        dplyr::select(nombre_viejo,valor,porcent_hombres,porcent_mujeres,Promedio,
                      `H%`,`M%`,`25%`,`50%`,`75%`,Alto,Bajo,cuantil,`VS PROM`) %>% 
        plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo),
                       'porcent_hombres' = '%H', 'porcent_mujeres' = '%M'))
  }else{
    dt <- tabla_reporte %>% filter(grepl(sprintf('^%s_',sec_subsec),seccion)) %>%
      dplyr::select(nombre_viejo,valor,Promedio,`25%`,`50%`,`75%`,Alto,Bajo,cuantil,`VS PROM`)  %>% 
      plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo)))
  }
  return(dt)
  }

tabla_si_no <- function(seccion,subseccion,renglones = c(),aparece_sec = 1){
  nombre_acronimo <- tabla_nombres_despachos %>% filter(id == id_despacho)
  if(length(renglones) != 0){
    filtro <- sprintf('^%s_%s_(%s)$',seccion,subseccion,paste(renglones , collapse = '|'))
  } else{
    filtro <- sprintf('^%s_%s_',seccion,subseccion)
  }
  dt <- tabla_reporte %>% filter(grepl(filtro,seccion)) %>%
    dplyr::select(valor,valor_1,valor_2,seccion) %>% 
    plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo),
                   'valor_1' = 'SI', 'valor_2' = 'NO')) %>% left_join(nombres_preguntas) %>%
    dplyr::select(name,everything()) %>% 
    plyr::rename(c('name'=''))
  if(aparece_sec == 1){
    dt <- dt %>% dplyr::select(-seccion)
  }
  return(dt)
}

distribucion_tabla <- function(seccion,subseccion,renglones = c(),nombre_col = 'Periodicidad'){
  if(length(renglones) != 0){
    filtro <- sprintf('^%s_%s_(%s)$',seccion,subseccion,paste(renglones , collapse = '|'))
  } else{
    filtro <- sprintf('^%s_%s_',seccion,subseccion)
  }
  dt <- tabla_reporte %>% filter(grepl(filtro,seccion)) %>%
        dplyr::select(respuesta_unica,valor_1)  %>% 
        plyr::rename(c('valor_1' = 'Porcentajes','respuesta_unica' = nombre_col))  
  return(dt)
}

tabla_promedio_sr <- function(seccion,subseccion,renglones = c()){
  if(length(renglones) != 0){
    filtro <- sprintf('^%s_%s_(%s)',seccion,subseccion,paste(renglones , collapse = '|'))
  } else{
    filtro <- sprintf('^%s_%s_',seccion,subseccion)
  }
  dt <- tabla_reporte %>% filter(grepl(filtro,seccion)) %>%
    dplyr::select(seccion,Promedio)
  return(dt)
}

tabla_compensaciones <- function(seccion,subseccion,tipo = 'personal_edad_anios'){
  sec_subsec <- paste(seccion,subseccion,sep = '_')
  nombre_acronimo <- tabla_nombres_despachos %>% filter(id == id_despacho)
  if(tipo == 'personal_edad_anios'){
    dt <- tabla_reporte %>% filter(grepl(sprintf('^%s_',sec_subsec),seccion)) %>%
          dplyr::select(nombre_viejo,valor,Promedio,`Personal total`,Edad,`Años de exp.`,`25%`,`50%`,`75%`,Alto,Bajo,`VS PROM`) %>%
          plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo)))
  } else{
    dt <- tabla_reporte %>% filter(grepl(sprintf('^%s_',sec_subsec),seccion)) %>%
          dplyr::select(nombre_viejo,valor,Promedio,`Personal total`,Tarifa,Edad,`25%`,`50%`,`75%`,Alto,Bajo,`VS PROM`) %>%
          plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo)))    
  }
  return(dt)
}