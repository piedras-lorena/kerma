range_to_number<- function(rango){
  z<-strsplit(scan(text=rango,sep=",",wh="a"),split="-") 
  l <- lapply(z,as.numeric) 
  elem<-unlist(lapply(l,function(x){ 
    last <- x[length(x)]
    seq(from=as.numeric(x[1]),to=last)
  })) 
  return(elem)
}

convertir_vector<- function(row){
  unlist(strsplit(row, split=","))
}

importar_encuesta<-function(path_df){
  df<-read_csv(path_df,col_types=cols(.default = "c"),na = c("-1.00","","N/A")) %>% 
    dplyr::select(-matches('^X[0-9]+'))
}

operaciones_col <- function(columna_1,columna_2,columna_3,funcion,nombre_columna_nueva){
  i <- nrow(mapeo_nombres)+1
  mapeo_nombres[i,'nombre_viejo'] <<- ifelse(funcion == 'sueldo_base',gsub('^[0-9]+_','',columna_2),gsub('^[0-9]+_','',columna_1))
  mapeo_nombres[i,'nombre_nuevo'] <<- nombre_columna_nueva
  error_c1<-try(tabla_encuesta_wide[columna_1],TRUE)
  error_c2<-try(tabla_encuesta_wide[columna_2],TRUE)
  error_c3<-try(tabla_encuesta_wide[columna_3],TRUE)
  
  if(funcion == 'suma' | funcion == 'division' | funcion == 'sueldo_base') {
    
    if(inherits(error_c1,'try-error')==TRUE | inherits(error_c2,'try-error')==TRUE){
      tabla_reporte[nombre_columna_nueva] <<- NA
      mapeo_nombres[i,'respondido'] <<- 0
      
    } else{
      mapeo_nombres[i,'respondido'] <<- 1
      tabla_encuesta_wide[,c(columna_1,columna_2)] <<- sapply(tabla_encuesta_wide[,c(columna_1,columna_2)],
                                                              as.numeric)
      if(funcion=='suma'){
        tabla_reporte[nombre_columna_nueva] <<- rowSums(tabla_encuesta_wide[c(columna_1,columna_2)],na.rm = T)
        #tabla_encuesta_wide[columna_1] + tabla_encuesta_wide[columna_2]
        
      } else if(funcion == 'division'){
        tabla_reporte[nombre_columna_nueva] <<- apply(tabla_encuesta_wide,1,function(x) as.numeric(x[columna_2]) / as.numeric(x[columna_1]))
        
      } else{
        tabla_reporte[nombre_columna_nueva] <<- apply(tabla_encuesta_wide,1,function(x) as.numeric(x[columna_2]) * (12 + as.numeric(x[columna_1])/30))
      }
    }
  } else if(funcion == 'crecimiento' | funcion == 'rotacion'){
    if(inherits(error_c1,'try-error')==TRUE | inherits(error_c2,'try-error')==TRUE | inherits(error_c3,'try-error')==TRUE){
      tabla_reporte[nombre_columna_nueva] <<- NA
      mapeo_nombres[i,'respondido'] <<- 0
      
    } else{
      tabla_encuesta_wide[,c(columna_1,columna_2,columna_3)] <<- sapply(tabla_encuesta_wide[,c(columna_1,columna_2,columna_3)],
                                                                        as.numeric)
      if(funcion == 'crecimiento'){
      mapeo_nombres[i,'respondido'] <<- 1
      tabla_reporte[nombre_columna_nueva] <<- apply(tabla_encuesta_wide,1,function(x) (as.numeric(x[columna_1]) - as.numeric(x[columna_2]) + as.numeric(x[columna_3]))/as.numeric(x[columna_1])-1)
      } else{
      tabla_reporte[nombre_columna_nueva] <<- apply(tabla_encuesta_wide,1,function(x) as.numeric(x[columna_1]) / (2 * as.numeric(x[columna_1]) - as.numeric(x[columna_2]) + as.numeric(x[columna_3])))  
      }
    } 
  } else{
    if(inherits(error_c1,'try-error')==TRUE){
      tabla_reporte[nombre_columna_nueva] <<- NA
      mapeo_nombres[i,'respondido'] <<- 0
      
    } else{
      mapeo_nombres[i,'respondido'] <<- 1
      tabla_reporte[nombre_columna_nueva] <<- tabla_encuesta_wide[columna_1]
    }
    
  }
}

operaciones_col_1 <- function(columna_1,columna_2,nombre_columna_nueva){
  error_c1<-try(tabla_encuesta_wide[columna_1],TRUE)
  error_c2<-try(tabla_encuesta_wide[columna_2],TRUE)
  
  if(inherits(error_c1,'try-error')==TRUE | inherits(error_c2,'try-error')==TRUE){
    tabla_reporte_col_1[nombre_columna_nueva] <<- NA
    
  } else {
    tabla_encuesta_wide[,c(columna_1,columna_2)] <<- sapply(tabla_encuesta_wide[,c(columna_1,columna_2)],as.numeric)
    tabla_reporte_col_1[nombre_columna_nueva] <<- apply(tabla_encuesta_wide,1,function(x) as.numeric(x[columna_1])/sum(as.numeric(x[columna_1]),as.numeric(x[columna_2]),na.rm = T))
    
  }
}

operaciones_col_2 <- function(columna_1,columna_2,nombre_columna_nueva){
  error_c1<-try(tabla_encuesta_wide[columna_1],TRUE)
  error_c2<-try(tabla_encuesta_wide[columna_2],TRUE)
  
  if(inherits(error_c1,'try-error')==TRUE | inherits(error_c2,'try-error')==TRUE){
    tabla_reporte_col_2[nombre_columna_nueva] <<- NA
    
  } else {
    tabla_encuesta_wide[,c(columna_1,columna_2)] <<- sapply(tabla_encuesta_wide[,c(columna_1,columna_2)],as.numeric)
    tabla_reporte_col_2[nombre_columna_nueva] <<- apply(tabla_encuesta_wide,1,function(x) as.numeric(x[columna_1])/sum(as.numeric(x[columna_1]),as.numeric(x[columna_2]),na.rm = T))
    
  }
}