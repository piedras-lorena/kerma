#SCRIPT 2
#Este script genera una tabla para cada submuestra con las estadísticas descriptivas de cada pregunta

library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)
library(plyr)

source('Proyectos/Otros/kerma/r_script/lib/utils_script2.R')


# Importamos tablas -------------------------------------------------------
tabla_reporte <- read_csv('Proyectos/Otros/kerma/data/interim/tabla_reporte_prueba.csv',
                          col_types = list(
                            userId = col_integer(),
                            seccion = col_character(),
                            valor = col_character(),
                            porcent_hombres = col_double(),
                            porcent_mujeres = col_double()
                          )) %>% mutate(seccion_subseccion = gsub("_[0-9]*$",'',seccion),
                                        seccion_sola = gsub("_[0-9]*_[0-9]*$",'',seccion))

submuestras <- read_csv('Proyectos/Otros/kerma/data/interim/submuestras - Hoja 1.csv',
                        col_types = list(
                        userId = col_integer()
                        ))

diccionario_diferentes <- read_csv('Proyectos/Otros/kerma/data/interim/tablas_resumen_diferentes - Hoja 1.csv',
                              col_types = cols(.default = "c")) %>%
                          mutate(seccion_solo = ifelse(is.na(subseccion) & is.na(renglon),seccion,"0"),
                                 seccion_entera = ifelse(is.na(subseccion) & !is.na(renglon), paste(seccion,'0',renglon,sep='_'),
                                             ifelse(!is.na(subseccion) & !is.na(renglon),paste(seccion,subseccion,renglon,sep='_'),NA)),
                                 seccion_subseccion = ifelse(!is.na(subseccion) & is.na(renglon),paste(seccion,subseccion,sep='_'),NA))


preguntas_submuestras <- read_csv('Proyectos/Otros/kerma/data/interim/mappeo_preguntas - mappeo.csv', col_types =  cols(.default = "c")) %>%
                         mutate(seccion_subseccion = ifelse(is.na(subseccion_reporte),paste(seccion_reporte,'0',sep='_'),
                                                            paste(seccion_reporte,subseccion_reporte,sep='_')),
                                submuestra = ifelse(is.na(submuestra),0,submuestra)) %>%
                         select(seccion_subseccion,submuestra) %>% unique()

tabla_reporte <- tabla_reporte %>% left_join(preguntas_submuestras) 

# Resumen normal ----------------------------------------------------------
resumen_normal <- tabla_reporte %>% filter((seccion_sola %!in% diccionario_diferentes$seccion_solo) & (seccion_subseccion %!in% diccionario_diferentes$seccion_subseccion) & 
                                           (seccion %!in% diccionario_diferentes$seccion_entera))
resumen_normal$valor <- sapply(resumen_normal$valor,as.numeric)
resumen_diferentes <- tabla_reporte %>% filter((seccion_sola %in% diccionario_diferentes$seccion_solo) | (seccion_subseccion %in% diccionario_diferentes$seccion_subseccion) | 
                                          (seccion %in% diccionario_diferentes$seccion_entera))

# CANCELAMOS LO DE SUBMUESTRAS POR EL MOMENTO
# submuestras_lista <- unique(submuestras$submuestra)
# tabla_resumen <- data.frame(userId=tabla_encuesta_wide$userId)
# 
# for(i in 1:length(submuestras_lista)){
#   despachos <- submuestras[submuestras$submuestra == submuestras_lista[i],'userId']  
#   resumen_submuestra <- resumen_normal[resumen_normal$userId %in% despachos$userId,]
#   resumen_submuestra <- resumen_submuestra %>% group_by(seccion) %>%
#                         summarize(Promedio = mean(valor,na_rm = T), `25%`=quantile(valor, probs=0.25,na.rm = TRUE),
#                                   `50%`=quantile(valor, probs=0.5,na.rm = T),`75%`=quantile(valor, probs=0.75,na.rm = T),
#                                   Alto = max(valor,na.rm = T),Bajo = min(valor,na.rm = T))
# }

resumen_normal_r <- resumen_normal %>% dplyr::group_by(seccion) %>%
                    dplyr::summarize(Promedio = mean(valor,na.rm = T), `25%`=quantile(valor, probs=0.25,na.rm = TRUE),
                                `50%`=quantile(valor, probs=0.5,na.rm = T),`75%`=quantile(valor, probs=0.75,na.rm = T),
                                Alto = max(valor,na.rm = T),Bajo = min(valor,na.rm = T))


# Resumen diferentes -----------------------------------------------------------
lista_incluidas <- c('promedio_h_m','si_no_cr','personal_edad_anios','personal_tarifa_edad','valor_promedio')
diferentes_reporte <-   diccionario_diferentes %>% filter(tipo_tabla %in% lista_incluidas)

resumen_diferentes_r <- resumen_diferentes %>% left_join(diccionario_diferentes[c('seccion_solo','tipo_tabla')],by = c('seccion_sola'='seccion_solo')) %>%
                   left_join(diccionario_diferentes[c('seccion_subseccion','tipo_tabla')],by = 'seccion_subseccion') %>%
                   left_join(diccionario_diferentes[c('seccion_entera','tipo_tabla')],by = c('seccion'='seccion_entera')) %>%
                   mutate(tipo_tabla_fin = ifelse(!is.na(tipo_tabla.y),tipo_tabla.y,
                                                  ifelse(!is.na(tipo_tabla.x),tipo_tabla.x,tipo_tabla))) %>% select(-tipo_tabla,-tipo_tabla.x,-tipo_tabla.y) %>%
                   unique()


# Creamos las tablas resumen para las preguntas que no tienen respuesta del despacho, son puro 'resumen'
# Importamos la tabla de la encuesta raw
tabla_encuestas_f <- read_csv('Proyectos/Otros/kerma/data/interim/tabla_encuestas_f.csv',col_types = cols(.default = "c"))

# cambiamos el id unico en preguntas de seguros para que incluya el nombre (socio,abogado,etc), tabién a otras prestaciones   

# Pegar wildcart a 340-356
diccionario_solo_resumen <- diccionario_diferentes %>% filter(tipo_tabla %!in% lista_incluidas)
distribucion <- diccionario_solo_resumen[diccionario_solo_resumen$tipo_tabla == 'distribucion',]
distribucion <- distribucion %>% mutate(renglon = as.numeric(renglon)) %>% arrange(seccion,subseccion,renglon) %>%
                group_by(seccion,subseccion,columna_1) %>% slice(1) %>% mutate(renglon = as.character(renglon))

diccionario_solo_resumen <- diccionario_solo_resumen %>% filter(tipo_tabla != 'distribucion') %>% bind_rows(distribucion)
resultado_resumen_diferentes <- create_empty_df()


### Hacemos tabla para si_no y distribucion ###
filtro <- c('si_no','distribucion')

sub <- diccionario_solo_resumen %>% filter(tipo_tabla %in% filtro) %>%
       mutate(seccion_subseccion = paste(seccion,subseccion,sep='_')) %>% filter(!is.na(columna_1))

apply(sub,1, function(x) resumenes_diferentes_func(x['columna_1'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_1_resumen <- resultado_resumen_diferentes %>% as.data.frame()


# Hacemos la columna número dos

resultado_resumen_diferentes <- create_empty_df()

sub_2 <- sub %>% filter(!is.na(columna_2))
apply(sub_2,1, function(x) resumenes_diferentes_func(x['columna_2'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_2_resumen <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('respuesta_unica'= 'respuesta_unica_2',
                                                                                         'valor_1' = 'valor_1_2',
                                                                                         'valor_2' = 'valor_2_2',
                                                                                         'N/A' = 'N/A_2'))

# Hacemos la columna número tres

resultado_resumen_diferentes <- create_empty_df()
apply(sub_2,1, function(x) resumenes_diferentes_func(x['columna_3'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_3_resumen <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('respuesta_unica'= 'respuesta_unica_3',
                                                                                         'valor_1' = 'valor_1_3',
                                                                                         'valor_2' = 'valor_2_3',
                                                                                         'N/A' = 'N/A_3'))

# Hacemos la columna número cuatro

resultado_resumen_diferentes <- create_empty_df()
apply(sub_2,1, function(x) resumenes_diferentes_func(x['columna_4'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_4_resumen <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('respuesta_unica'= 'respuesta_unica_4',
                                                                                         'valor_1' = 'valor_1_4',
                                                                                         'valor_2' = 'valor_2_4',
                                                                                         'N/A' = 'N/A_4'))

# Hacemos la columna número cinco

resultado_resumen_diferentes <- create_empty_df()
apply(sub_2,1, function(x) resumenes_diferentes_func(x['columna_5'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_5_resumen <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('respuesta_unica'= 'respuesta_unica_5',
                                                                                         'valor_1' = 'valor_1_5',
                                                                                         'valor_2' = 'valor_2_5',
                                                                                         'N/A' = 'N/A_5'))


sub_total <- resultado_col_1_resumen %>% left_join(resultado_col_2_resumen) %>% left_join(resultado_col_3_resumen) %>%
             left_join(resultado_col_4_resumen) %>% left_join(resultado_col_5_resumen) 

# Hacemos distribucion_suma_asegurada, distribucion_anios,distribucion_edad
filtro <- c('distribucion_suma_asegurada','distribucion_anios','distribucion_edad')

distribuciones <- diccionario_solo_resumen %>% filter(tipo_tabla %in% filtro) %>% mutate(seccion_subseccion = paste(seccion,subseccion,sep='_'))
columnas <- c('columna_1','columna_2','columna_3')


col_1 <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% distribuciones$columna_1)
resultado_resumen_diferentes <- create_empty_df()
apply(distribuciones,1, function(x) resumenes_diferentes_func(x['columna_2'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
distribucion_col_2 <- resultado_resumen_diferentes %>% as.data.frame()

#columna 3
resultado_resumen_diferentes <- create_empty_df()
apply(distribuciones,1, function(x) resumenes_diferentes_func(x['columna_3'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
distribucion_col_3 <- resultado_resumen_diferentes %>% as.data.frame()

