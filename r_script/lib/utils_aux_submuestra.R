'%!in%' <- function(x,y)!('%in%'(x,y))
cambiar_porcent<- function(x){
  if(is.na(x)){
    valor <- x
  } else valor <- percent(x)
  return(valor)
}

crear_tabla_reporte <- function(id_despacho,submuestra = id_submuestra){
  id_despacho <- as.character(id_despacho)
  id_submuestra <- as.character(id_submuestra)

  # Importamos todas las tablas
  encuesta <- read_csv('../../data/interim/tabla_reporte_prueba.csv',
                        col_types = cols(.default = 'c')) %>% filter(userId == id_despacho)
  resumenes <- read_csv(sprintf('../../data/interim/submuestras/tabla_resumen_final_sub%s.csv',id_submuestra),
                        col_types = cols(.default = 'c'))
  mapeo_nombres <- read_csv(sprintf('../../data/interim/submuestras/mappeo_nombres_resumen_reporte_sub%s.csv',id_submuestra),
                            col_types = cols(.default = 'c'))
  tabla_nombres_despachos <- read_csv('../../data/interim/UsuariosActivos.csv',
                                      col_types = cols(.default = 'c'))

# Porcentaje de la encuesta no contestado
  porcent_na <- nrow(encuesta[is.na(encuesta$valor),]) / nrow(encuesta) * 100
  print(paste('porcentaje de na',porcent_na))

# Guardamos el nombre del despacho 
  nombre_acronimo <- tabla_nombres_despachos %>% filter(id == id_despacho)
  tabla_reporte <- encuesta %>% full_join(resumenes) %>% left_join(mapeo_nombres, by = c('seccion'='nombre_nuevo')) %>% 
                   dplyr::select(-userId,-respondido) %>% dplyr::select(seccion, nombre_viejo, everything())

  tabla_reporte <- tabla_reporte %>% mutate(cuantil = ifelse(as.numeric(valor) <= as.numeric(`25%`), '1er',
                                                           ifelse(as.numeric(valor) > as.numeric(`25%`) & as.numeric(valor) <= as.numeric(`50%`),'2do',
                                                                  ifelse(as.numeric(valor) > as.numeric(`50%`) & as.numeric(valor) <= as.numeric(`75%`),'3er',
                                                                         ifelse(is.na(`25%`),NA,'4to')))),
                                          `VS PROM` = ifelse(as.numeric(valor) < as.numeric(Promedio),'Abajo','Arriba'))
  filas_numeric = c('Promedio','25%','50%','75%','Alto','Bajo','Porcentaje','porcent_hombres','porcent_mujeres','Más alto','Más bajo',
                    #'H%','M%','valor_1','valor_2',
                    'Personal total','Edad','Años de exp.','Tarifa')
  tabla_reporte[,filas_numeric] <- sapply(tabla_reporte[,filas_numeric], function(x) round(as.numeric(x),digits = 2)) 
  filtro_percent <- c('porcent_hombres','porcent_mujeres')
  tabla_reporte[filtro_percent] <- apply(tabla_reporte[filtro_percent],2, function(x) mapply(cambiar_porcent,x))
  
  # Convertimos los nans a na
  tabla_reporte[is.na(tabla_reporte['Promedio']),'Promedio'] <- NA
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
                       'porcent_hombres' = '%H', 'porcent_mujeres' = '%M',
                     c('cuantil'=sprintf('Cuartil %s',nombre_acronimo$acronimo))))
  }else{
    dt <- tabla_reporte %>% filter(grepl(sprintf('^%s_',sec_subsec),seccion)) %>%
      dplyr::select(nombre_viejo,valor,Promedio,`25%`,`50%`,`75%`,Alto,Bajo,cuantil,`VS PROM`)  %>% 
      plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo),
                   c('cuantil'=sprintf('Cuartil %s',nombre_acronimo$acronimo))))
  }
  return(dt)
}

tabla_si_no <- function(seccion,subseccion,renglones = c(),aparece_sec = 1,incl_na = 0){
  nombre_acronimo <- tabla_nombres_despachos %>% filter(id == id_despacho)
  if(length(renglones) != 0){
    filtro <- sprintf('^%s_%s_(%s)$',seccion,subseccion,paste(renglones , collapse = '|'))
  } else{
    filtro <- sprintf('^%s_%s_',seccion,subseccion)
  }
  if(incl_na == 0){
  dt <- tabla_reporte %>% filter(grepl(filtro,seccion)) %>%
    dplyr::select(valor,valor_1,valor_2,seccion) %>% 
    plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo),
                   'valor_1' = 'SI', 'valor_2' = 'NO')) %>% left_join(nombres_preguntas) %>%
    dplyr::select(name,everything()) %>% 
    plyr::rename(c('name'=''))
  } else{
    dt <- tabla_reporte %>% filter(grepl(filtro,seccion)) %>%
      dplyr::select(valor,valor_1,valor_2,`N/A`,seccion) %>% 
      plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo),
                     'valor_1' = 'SI', 'valor_2' = 'NO')) %>% left_join(nombres_preguntas) %>%
      dplyr::select(name,everything()) %>% 
      plyr::rename(c('name'=''))   
  }
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
          dplyr::select(nombre_viejo,valor,Promedio,`Personal total`,Edad,`Años de exp.`,`25%`,`50%`,`75%`,Alto,Bajo,cuantil,`VS PROM`,
                        `Más alto`,`Más bajo`) %>%
          plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo),
                       'cuantil'=sprintf('Cuartil %s',nombre_acronimo$acronimo)))
  } else{
    dt <- tabla_reporte %>% filter(grepl(sprintf('^%s_',sec_subsec),seccion)) %>%
          dplyr::select(nombre_viejo,valor,Promedio,`Personal total`,Tarifa,Edad,`25%`,`50%`,`75%`,Alto,Bajo,cuantil,`VS PROM`,
                        `Más alto`,`Más bajo`) %>%
          plyr::rename(c('valor'= toupper(nombre_acronimo$acronimo),
                       'cuantil'=sprintf('Cuartil %s',nombre_acronimo$acronimo)))    
  }
  return(dt)
}

formato_primera_col <- function(dt,nombre_1,nombre_2,filtro_categoria){
  dt <- dt %>% mutate(col_2 = gsub(filtro_categoria,'',dt$nombre_viejo))
  filtro_nuevo <- sprintf('_%s$',paste(dt$col_2 %>% unique(),collapse = '$|_'))
  dt <- dt %>% mutate (col_1 = gsub('_',' ',gsub(filtro_nuevo,' ', nombre_viejo)),
                       col_2 = gsub('_',' ',col_2)) %>% 
    dplyr::select(col_1,col_2,everything(),-nombre_viejo)
  
  # Convertimos la primera letra en mayúscula
  dt <- dt %>% mutate (col_1 = gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2",col_1,perl = T),
                       col_2 = gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2",col_2,perl = T),
                       col_2 = ifelse(grepl('[0-9]+ [0-9]+',col_2),gsub(' ','-',col_2),
                                      col_2)) %>%
        plyr::rename(c('col_1'=nombre_1,'col_2'=nombre_2))
  return(dt)
}


bar_plot <- function(df,x,y,titulo_x,titulo_y,fill = c(),pallete = paleta_colores_1,tamanio_letra = 18,
                     nombre_cuadro = 'Categoría'){
  y <- sapply(gsub('%','',y),as.numeric)
  if(length(fill) != '0'){
    g <- ggplot(df, aes(x = reorder(x,y), y = y , fill = fill)) + geom_bar(position = 'dodge',stat="identity") + coord_flip()  + 
      labs(x = sprintf('%s\n\n',titulo_x), y = sprintf('\n\n%s',titulo_y)) +
      scale_fill_manual(values = pallete, name = nombre_cuadro) +  theme_bw(base_size = tamanio_letra) 
  } else{
    g <- ggplot(df, aes(x = reorder(x,y), y = y)) + geom_bar(position = 'dodge',stat="identity") + coord_flip()  + 
      labs(x = sprintf('%s\n\n',titulo_x), y = sprintf('\n\n%s',titulo_y)) +
      scale_fill_manual(values = pallete) +  theme_bw(base_size = tamanio_letra) 
  }
  return(g)
}

poner_comas <- function(df,cols){
  df[cols] <- sapply(df[cols],as.numeric)
  df[cols] <- apply(df[cols],2,function(x) format(round(x),big.mark=",", trim=TRUE) )
  return(df)
}