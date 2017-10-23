'%!in%' <- function(x,y)!('%in%'(x,y))

# Función para crear resúmenes de tablas diferentes

frecuencia <- function(x,total_res){
  #length(x %>% unique())/total_res
  length(x)/total_res
}

create_empty_df <- function(){
  resultado_resumen_diferentes <- data.frame(respuesta_unica = character(0),
                                             valor_1 = numeric(0), valor_2 = numeric(0),
                                             seccion = character(0), tipo_tabla = character(0),
                                             stringsAsFactors = F)
  return(resultado_resumen_diferentes)
}

resumenes_diferentes_func <- function(columna_1,seccion_subseccion,renglon, tipo_tabla){
  renglon <- as.numeric(renglon)
  columna_1_cp <- paste(columna_1,'possibleanswers',sep = '_')
  #tabla <- tabla_encuestas_f %>% filter(id_unico_pregunta == columna_1 & respuesta_unica != 'N/A' )
  tabla <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% c(columna_1,columna_1_cp) & respuesta_unica != 'N/A' )
  if(nrow(tabla) == 0){
    res_grupos <- data_frame(seccion= paste(seccion_subseccion,renglon, sep = '_'), valor_1 = NA)
    
  } else {
    if(tipo_tabla == 'distribucion'){
      total_res <- length(tabla$userId %>% unique())
      #total_res <- length(tabla['respuesta_unica'] %>% unique())
      res_grupos <- tabla %>% group_by(respuesta_unica) %>% 
                    dplyr::summarize(valor_1 = frecuencia(userId,total_res))
      id_row <- seq(renglon,length.out=nrow(res_grupos))
      res_grupos['row'] <- id_row
      res_grupos <- res_grupos %>% mutate(seccion = paste(seccion_subseccion,row,sep = '_')) %>% dplyr::select(-row)
      
    } else{ # si no
      total_res <- length(tabla$userId %>% unique())
      #total_res <- length(tabla['respuesta_unica'] %>% unique())
      res_grupos <- tabla %>% group_by(respuesta_unica) %>% 
                    dplyr::summarize(valor_1 = frecuencia(userId,total_res)) %>%
                    spread(respuesta_unica,valor_1)  %>% mutate(seccion = paste(seccion_subseccion,renglon,sep = '_')) %>%
                    rename(c('Si'='valor_1','No'='valor_2')) %>% mutate(respuesta_unica = NA)
    }
  }
  res_grupos['tipo_tabla'] <- tipo_tabla
  resultado_resumen_diferentes <<- resultado_resumen_diferentes %>% bind_rows(res_grupos)
}

convertir_vector<- function(row){
  unlist(strsplit(row, split=";"))
}

cambiar_porcent<- function(x){
  if(is.na(x)){
    valor <- x
  } else valor <- percent(x)
  return(valor)
}

seguros_si_no <- function(columna_1,seccion_subseccion,renglon, tipo_tabla){
  renglon <- as.numeric(renglon)
  columna_1_sp <- gsub('_possibleanswers$','',columna_1)
  tabla <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% c(columna_1,columna_1_sp) & respuesta_unica != 'N/A')
  if(nrow(tabla) == 0){
    res_grupos <- data_frame(seccion= paste(seccion_subseccion,renglon, sep = '_'), valor_1 = NA)
    
  } else {
      total_res <- length(tabla$userId %>% unique())
      #total_res <- length(tabla['respuesta_unica'] %>% unique())
      res_grupos <- tabla %>% group_by(respuesta_unica) %>% 
        dplyr::summarize(valor_1 = frecuencia(userId,total_res)) %>%
        spread(respuesta_unica,valor_1)  %>% mutate(seccion = paste(seccion_subseccion,renglon,sep = '_')) %>%
        rename(c('Si'='valor_1','No'='valor_2'))
  }
  res_grupos['tipo_tabla'] <- tipo_tabla
  resultado_resumen_diferentes <<- resultado_resumen_diferentes %>% bind_rows(res_grupos)
}
