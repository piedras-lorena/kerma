#SCRIPT 2
#Este script genera una tabla para cada submuestra con las estadísticas descriptivas de cada pregunta

library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)
library(plyr)
library(qpcR)
library(scales)
options(scipen = 999)

source('Proyectos/Otros/kerma/r_script/lib/utils_script2.R')


# Importamos tablas -------------------------------------------------------
tabla_reporte <- read_csv('Proyectos/Otros/kerma/data/interim/tabla_reporte_prueba.csv',
                          col_types = list(
                            userId = col_integer(),
                            seccion = col_character(),
                            valor = col_character(),
                            porcent_hombres = col_double(),
                            porcent_mujeres = col_double(),
                            sueldo_alto = col_double(),
                            sueldo_bajo = col_double())) %>% mutate(seccion_subseccion = gsub("_[0-9]*$",'',seccion),
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
                         dplyr::select(seccion_subseccion,submuestra) %>% unique()

tabla_reporte <- tabla_reporte %>% left_join(preguntas_submuestras) 
operaciones <- read_csv('Proyectos/Otros/kerma/data/interim/operaciones_preguntas - col_1.csv',
                        col_types =  cols(.default = "c")) %>%
              mutate(columna_1 = ifelse(is.na(categoria_pregunta_1),pregunta_1,paste(pregunta_1,categoria_pregunta_1,sep='_')),
              columna_2 = ifelse(!is.na(pregunta_2) & !is.na(categoria_pregunta_2),paste(pregunta_2,categoria_pregunta_2,sep='_'),
                            ifelse(!is.na(pregunta_2) & is.na(categoria_pregunta_2),pregunta_2,NA)),
              columna_3 = ifelse(!is.na(pregunta_3) & !is.na(categoria_pregunta_2),paste(pregunta_3,categoria_pregunta_2,sep='_'),
                            ifelse(!is.na(pregunta_3) & is.na(categoria_pregunta_2),pregunta_3,NA)),
              nombre_columna_nueva = ifelse(is.na(subseccion),paste(seccion,'0',renglon,sep='_') , paste(seccion,subseccion,renglon,sep='_'))) %>% 
              dplyr::select(columna_1,columna_2,columna_3,funcion,nombre_columna_nueva)

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

# Generamos las tablas resumen 'normales'
resumen_normal_r <- resumen_normal %>% dplyr::group_by(seccion) %>%
                    dplyr::summarize(Promedio = mean(valor,na.rm = T), `25%`=quantile(valor, probs=0.25,na.rm = TRUE),
                                `50%`=quantile(valor, probs=0.5,na.rm = T),`75%`=quantile(valor, probs=0.75,na.rm = T),
                                Alto = max(valor,na.rm = T),Bajo = min(valor,na.rm = T))


# Resumen diferentes sin respuesta -----------------------------------------------------------
lista_incluidas <- c('promedio_h_m','si_no_cr','personal_edad_anios','personal_tarifa_edad','valor_promedio')


# Creamos las tablas resumen para las preguntas que no tienen respuesta del despacho, son puro 'resumen'
# Importamos la tabla de la encuesta raw
tabla_encuestas_f <- read_csv('Proyectos/Otros/kerma/data/interim/tabla_encuestas_f.csv',col_types = cols(.default = "c"))

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
                                                                                         'valor_2' = 'valor_2_2'))

# Hacemos la columna número tres

resultado_resumen_diferentes <- create_empty_df()
apply(sub_2,1, function(x) resumenes_diferentes_func(x['columna_3'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_3_resumen <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('respuesta_unica'= 'respuesta_unica_3',
                                                                                         'valor_1' = 'valor_1_3',
                                                                                         'valor_2' = 'valor_2_3'))

# Hacemos la columna número cuatro

resultado_resumen_diferentes <- create_empty_df()
apply(sub_2,1, function(x) resumenes_diferentes_func(x['columna_4'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_4_resumen <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('respuesta_unica'= 'respuesta_unica_4',
                                                                                         'valor_1' = 'valor_1_4',
                                                                                         'valor_2' = 'valor_2_4'))

# Hacemos la columna número cinco

resultado_resumen_diferentes <- create_empty_df()
apply(sub_2,1, function(x) resumenes_diferentes_func(x['columna_5'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
resultado_col_5_resumen <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('respuesta_unica'= 'respuesta_unica_5',
                                                                                         'valor_1' = 'valor_1_5',
                                                                                         'valor_2' = 'valor_2_5'))


sub_total <- resultado_col_1_resumen %>% left_join(resultado_col_2_resumen) %>% left_join(resultado_col_3_resumen) %>%
             left_join(resultado_col_4_resumen) %>% left_join(resultado_col_5_resumen) 

# Hacemos distribucion_suma_asegurada, distribucion_anios,distribucion_edad
filtro <- c('distribucion_suma_asegurada','distribucion_anios','distribucion_edad')

distribuciones <- diccionario_solo_resumen %>% filter(tipo_tabla %in% filtro) %>% mutate(seccion_subseccion = paste(seccion,subseccion,sep='_'))

resultado_resumen_diferentes <- create_empty_df()
apply(distribuciones,1, function(x) seguros_si_no(x['columna_1'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
distribucion_col_1 <- resultado_resumen_diferentes %>% as.data.frame()  %>% rename(c('valor_1' = 'valor_1_1',
                                                                                     'valor_2' = 'valor_2_1'))
prueba <- distribuciones %>% filter(seccion == '6' &  subseccion == '2'&  renglon == '5')
seguros_si_no(prueba$columna_1,prueba$seccion_subseccion,prueba$renglon,prueba$tipo_tabla)
#columna 3
resultado_resumen_diferentes <- create_empty_df()
apply(distribuciones,1, function(x) seguros_si_no(x['columna_3'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
distribucion_col_3 <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('valor_1' = 'valor_1_3',
                                                                                    'valor_2' = 'valor_2_3'))


#columna 4
resultado_resumen_diferentes <- create_empty_df()
apply(distribuciones,1, function(x) seguros_si_no(x['columna_4'],x['seccion_subseccion'],x['renglon'],x['tipo_tabla']))
distribucion_col_4 <- resultado_resumen_diferentes %>% as.data.frame() %>% rename(c('valor_1' = 'valor_1_4',
                                                                                    'valor_2' = 'valor_2_4'))


# Calculamos la suma asegurada, edad y años promedio
# columna 2

filtro <- pull(distribuciones[! is.na (distribuciones$columna_2),],'columna_2')
tabla_encuestas_f$valueNumeric <- sapply(tabla_encuestas_f$valueNumeric,as.numeric)

distribucion_col_2 <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% filtro) %>% group_by(id_unico_pregunta) %>%
                      dplyr::summarize(Promedio = mean(valueNumeric,na.rm = T)) %>% 
                      left_join(distribuciones[c('seccion','subseccion','renglon','columna_2')],by = c('id_unico_pregunta'='columna_2')) %>%
                      mutate(seccion = paste(seccion,subseccion,renglon,sep = '_')) %>% dplyr::select(-subseccion,-renglon)

distribucion_total <- distribucion_col_1 %>% left_join(distribucion_col_2 %>% dplyr::select(-id_unico_pregunta)) %>% left_join(distribucion_col_3) %>%
                      left_join(distribucion_col_4)
  
mapeo_nombres_diferentes_1 <- distribucion_col_2 %>% dplyr::select(seccion,id_unico_pregunta) %>%
                            mutate(respondido = '1',id_unico_pregunta = gsub('^[0-9]+_','',id_unico_pregunta)) %>% 
                            rename(c('id_unico_pregunta' = 'nombre_viejo','seccion'='nombre_nuevo'))

mapeo_nombres_diferentes_2 <- diccionario_diferentes %>% filter(seccion == '6' & subseccion == '3') %>%
                              mutate(respondido = '1',nombre_viejo = gsub('^[0-9]+_','',columna_1)) %>% rename(
                                c('seccion_entera' = 'nombre_nuevo')) %>%
                              dplyr::select(nombre_nuevo,nombre_viejo,respondido)
# Importamos el archivo con el nombre de los renglones para pegarle el nombre de los renglones de las tablas diferentes
mapeo_nombres <- read_csv('Proyectos/Otros/kerma/data/interim/mapeo_nombres.csv',
                          col_types = cols(.default = "c")) %>% bind_rows(mapeo_nombres_diferentes_1) %>%
                          bind_rows(mapeo_nombres_diferentes_2)


# CALCULAMOS PROMEDIO_SR 
promedio_sr <- pull(diccionario_diferentes[diccionario_diferentes$tipo_tabla == 'promedio_sr',],'columna_1')

resumen_promedio_sr <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% promedio_sr) %>% group_by(id_unico_pregunta) %>%
                       dplyr::summarize(Promedio = mean(valueNumeric,na.rm = T)) %>% left_join(diccionario_diferentes[c('seccion','subseccion','renglon','columna_1')],
                                                                                                                      by = c('id_unico_pregunta' = 'columna_1')) %>%
                       mutate(seccion = paste(seccion,subseccion,renglon,sep = '_')) %>% dplyr::select(-subseccion,-renglon,-id_unico_pregunta)

# CALCULAMOS DISTRIBUCION CATEGORIAS
distrib_cat <- pull(diccionario_diferentes[diccionario_diferentes$tipo_tabla == 'distribucion_categorias',],'columna_1')

temp <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% distrib_cat) 
resumen_distribucion_cat_v <- convertir_vector(temp$possibleAnswers) %>% as.data.table() %>% rename(c('.'='respuesta_unica')) 

total <- length(tabla_encuestas_f$userId %>% unique())

resumen_distribucion_cat <- diccionario_diferentes[diccionario_diferentes$tipo_tabla == 'distribucion_categorias','seccion_entera'] %>%
                            qpcR:::cbind.na(resumen_distribucion_cat_v %>% group_by(respuesta_unica) %>%
                                      dplyr::summarize(Porcentaje = n()) %>%
                                      mutate(Porcentaje = Porcentaje/total)) %>% rename(c('seccion_entera' = 'seccion'))


# NORMAL SR
normal_sr <- pull(diccionario_diferentes[diccionario_diferentes$tipo_tabla == 'normal_sr',],'columna_1')
resumen_normal_sr <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% normal_sr) %>%
                     dplyr::select(id_unico_pregunta,valueNumeric) %>% 
                     left_join(diccionario_diferentes %>% dplyr::select(seccion_entera,columna_1),by = c('id_unico_pregunta'='columna_1')) %>%
                     dplyr::select(-id_unico_pregunta) %>% rename(c('seccion_entera'='seccion'))

resumen_normal_sr_r <-  resumen_normal_sr %>% dplyr::group_by(seccion) %>%
                        dplyr::summarize(Promedio = mean(valueNumeric,na.rm = T), `25%`=quantile(valueNumeric, probs=0.25,na.rm = TRUE),
                       `50%`=quantile(valueNumeric, probs=0.5,na.rm = T),`75%`=quantile(valueNumeric, probs=0.75,na.rm = T),
                        Alto = max(valueNumeric,na.rm = T),Bajo = min(valueNumeric,na.rm = T))

# DISTRIBUCION FECHA
filtro_distribucion_fecha <- pull(diccionario_diferentes[diccionario_diferentes$tipo_tabla == 'distribucion_fecha',],'columna_1') %>% unique()

meses <- data.frame(valueDate = c(1:12), mes = c('enero','febrero','marzo','abril','mayo','junio',
                                                'julio','agosto','septiembre','octubre','noviembre','diciembre'))
distribucion_fecha <- tabla_encuestas_f %>% filter(id_unico_pregunta %in% filtro_distribucion_fecha) %>%
                      mutate(valueDate = month(as.Date(valueDate))) %>% left_join(meses)
total_res <- distribucion_fecha %>% group_by(questionId) %>%
             dplyr::summarise(total = n())
distribucion_fecha_f <- distribucion_fecha %>%  group_by(questionId,mes,valueDate) %>%
                        dplyr::summarise(count = n()) %>% left_join(total_res) %>% 
                        mutate(valor_1 = count/total) %>% rename(c('mes'='respuesta_unica')) %>% arrange(questionId,valueDate) %>%
                        dplyr::select(respuesta_unica,valor_1,questionId)
renglon <- diccionario_diferentes[diccionario_diferentes$tipo_tabla == 'distribucion_fecha',] %>% group_by(columna_1) %>% 
           slice(1) %>% dplyr::select(renglon,columna_1) %>% rename(c('columna_1' = 'questionId'))
distribucion_fecha_f <-distribucion_fecha_f %>% left_join(renglon) %>%
                       mutate(seccion = paste(4,1,renglon,sep = '_')) %>%
                      dplyr::select(-renglon,-questionId)
# DISTRIBUCION FECHA 132
distribucion_fecha_132 <- tabla_encuestas_f %>% filter(questionId == 132) %>%
                          mutate(valueDate = month(as.Date(valueDate))) %>% left_join(meses)

distribucion_fecha_132_f <- distribucion_fecha_132 %>%  group_by(mes,valueDate) %>%
                            dplyr::summarise(count = n()) %>% mutate(valor_1 = count/sum(count)) %>% 
                            rename(c('mes'='respuesta_unica')) %>% arrange(valueDate) %>%
                            dplyr::select(respuesta_unica,valor_1)
distribucion_fecha_132_f <-distribucion_fecha_132_f %>% mutate(renglon = seq(1,nrow(distribucion_fecha_132_f)),
                                                       seccion = paste(3,'15.3',renglon,sep = '_')) %>%
                                                       dplyr::select(-renglon)

distribucion_fecha_todas <- distribucion_fecha_f %>% bind_rows(distribucion_fecha_132_f)

# Resumen diferentes con respuesta ----------------------------------------

resumen_diferentes_r <- resumen_diferentes %>% left_join(diccionario_diferentes[c('seccion_solo','tipo_tabla')],by = c('seccion_sola'='seccion_solo')) %>%
                        left_join(diccionario_diferentes[c('seccion_subseccion','tipo_tabla')],by = 'seccion_subseccion') %>%
                        left_join(diccionario_diferentes[c('seccion_entera','tipo_tabla')],by = c('seccion'='seccion_entera')) %>%
                        mutate(tipo_tabla_fin = ifelse(!is.na(tipo_tabla.y),tipo_tabla.y,
                                                  ifelse(!is.na(tipo_tabla.x),tipo_tabla.x,tipo_tabla))) %>% dplyr::select(-tipo_tabla,-tipo_tabla.x,-tipo_tabla.y) %>%
                        unique() %>% rename(c('tipo_tabla_fin'='tipo_tabla'))


#Resumen promedio hm
resumen_promedio_hm <- resumen_diferentes_r %>% filter(tipo_tabla == 'promedio_h_m') 
resumen_promedio_hm$valor <- sapply(resumen_promedio_hm$valor,as.numeric)

resumen_promedio_hm <- resumen_promedio_hm  %>% dplyr::group_by(seccion) %>%
                        dplyr::summarize(Promedio = mean(valor,na.rm = T),`25%`=quantile(valor, probs=0.25,na.rm = TRUE),
                                         `50%`=quantile(valor, probs=0.5,na.rm = T),`75%`=quantile(valor, probs=0.75,na.rm = T),
                                          Alto = max(valor,na.rm = T),Bajo = min(valor,na.rm = T))
operaciones_col1 <- operaciones %>% filter(grepl('^2_1_|^2_2_|^2_3_',nombre_columna_nueva)) %>% left_join(tabla_encuestas_f[c('id_unico_pregunta','valueNumeric','userId')],
                                    by = c('columna_1'='id_unico_pregunta')) %>% left_join(tabla_encuestas_f[c('id_unico_pregunta','valueNumeric','userId')],
                                    by = c('columna_2'='id_unico_pregunta','userId'='userId')) %>%
                    mutate(total = ifelse(is.na(valueNumeric.x),valueNumeric.y,
                                          ifelse(is.na(valueNumeric.y),valueNumeric.x,
                                                 valueNumeric.y+valueNumeric.x))) %>% group_by(nombre_columna_nueva) %>% 
                    dplyr::summarise(total = sum(total,na.rm = T), hombres = sum(valueNumeric.x,na.rm = T), 
                                     mujeres = sum(valueNumeric.y,na.rm = T)) %>%
                    plyr::rename(c('nombre_columna_nueva'='seccion')) %>%
                    mutate(`M%` = mujeres/total, `H%` = hombres/total) %>% dplyr::select(seccion,`H%`,`M%`)
resumen_promedio_hm <- resumen_promedio_hm  %>% left_join(operaciones_col1)
                        
#Resumen si no con respuesta
si_no_cr <- resumen_diferentes_r %>% filter(tipo_tabla == 'si_no_cr' )

total <- length(si_no_cr$userId  %>% unique())

si_no_cr_r <- si_no_cr %>% dplyr::group_by(seccion,valor) %>%
              dplyr::summarize(res = length(userId)/total) %>%
              spread(key = valor, value = res) %>%
              rename(c('Si'='valor_1','No'='valor_2',
                       '<NA>' = 'N/A'))


#Resumen personal edad años
personal_edad_anios <- resumen_diferentes_r %>% filter(tipo_tabla == 'personal_edad_anios') 

personal_edad_anios['valor'] <- sapply(personal_edad_anios$valor,as.numeric)
personal_edad_anios_r <- personal_edad_anios %>% dplyr::group_by(seccion) %>%
                         dplyr::summarize(Promedio = mean(valor,na.rm = T), `25%`=quantile(valor, probs=0.25,na.rm = TRUE),
                                        `50%`=quantile(valor, probs=0.5,na.rm = T),`75%`=quantile(valor, probs=0.75,na.rm = T),
                                         Alto = max(valor,na.rm = T),Bajo = min(valor,na.rm = T),
                                        `Más alto` = max(sueldo_alto,na.rm = T),`Más bajo` = min(sueldo_bajo,na.rm = T)) %>% 
                         left_join(mapeo_nombres %>%
                         dplyr::select(-respondido), by = c('seccion' = 'nombre_nuevo'))

# Encontramos las secciones donde viene edad (2_17 y 21_18)
edad <- resumen_normal_r %>% filter(grepl('^2_16|^2_17|^2_18',seccion)) %>% 
        left_join(mapeo_nombres, by = c('seccion' = 'nombre_nuevo')) %>% dplyr::select(nombre_viejo,Promedio) %>%
        rename(c('Promedio' = 'Edad'))

# Encontramos las secciones donde viene personal total  (2_1,2_2,2_3)

personal <- resumen_promedio_hm %>% filter(grepl('^2_1|^2_2|^2_3',seccion)) %>% 
            left_join(mapeo_nombres, by = c('seccion' = 'nombre_nuevo')) %>% dplyr::select(nombre_viejo,Promedio) %>%
            rename(c('Promedio' = 'Personal total'))

# Encontramos las secciones donde viene personal total  (2_7,2_8,2_9)
anios <- resumen_normal_r %>% filter(grepl('^2_7|^2_8|^2_9',seccion)) %>% 
         left_join(mapeo_nombres, by = c('seccion' = 'nombre_nuevo')) %>% dplyr::select(nombre_viejo,Promedio) %>%
         rename(c('Promedio' = 'Años de exp.'))

# Agregamos la parte de no abogados (164, 165, 174, 175)
no_abogados <- tabla_encuestas_f %>% filter(grepl('^164|^165|^174|^175',questionId)) %>% mutate(nombre_viejo = 
                                                                                      gsub('^[0-9]+_','',id_unico_pregunta))
no_abogados['respuesta_unica'] <- sapply(no_abogados$respuesta_unica,as.numeric)

no_abogados_g <- no_abogados %>% filter(grepl('^164|^165',questionId)) %>% group_by(nombre_viejo,userId) %>% 
                 dplyr::summarize(respuesta_unica = sum(respuesta_unica, na.rm = T)) %>% 
                 mutate(questionId = '164')
 
no_abogados_todos <- no_abogados %>% filter(grepl('^174|^175',questionId)) %>% dplyr::select(nombre_viejo,respuesta_unica,questionId,userId) %>%
                     bind_rows(no_abogados_g) %>% group_by(questionId,nombre_viejo) %>%
                     dplyr::summarize(promedio = mean(respuesta_unica,na.rm = T)) %>%
                     spread(key = questionId,value = promedio) %>% rename(c('164' = 'Personal total','174' = 'Años de exp.','175' = 'Edad'))

temporal_edad_anios <- personal %>% full_join(edad) %>% full_join(anios) %>% bind_rows(no_abogados_todos)

#Pegamos todo junto
personal_edad_anios_r <- personal_edad_anios_r %>% left_join(temporal_edad_anios) %>% dplyr::select(-nombre_viejo)

# PERSONAL TARIFA EDAD (metemos las preguntas 137,138,140,147)

anios_exp <- tabla_encuestas_f %>% filter(grepl('^137|^138|^140|^147',questionId)) %>% mutate(nombre_viejo = 
             gsub('^[0-9]+_','',id_unico_pregunta))

anios_exp['respuesta_unica'] <- sapply(anios_exp$respuesta_unica,as.numeric)
anios_exp_g <- anios_exp %>% filter(grepl('^137|^138',questionId)) %>% group_by(nombre_viejo,userId) %>% 
               dplyr::summarize(respuesta_unica = sum(respuesta_unica, na.rm = T)) %>% 
               mutate(questionId = '137')

anios_exp_todos <- anios_exp %>% filter(grepl('^140|^147',questionId)) %>% dplyr::select(nombre_viejo,respuesta_unica,questionId,userId) %>%
                    bind_rows(anios_exp_g) %>% group_by(questionId,nombre_viejo) %>%
                    dplyr::summarize(promedio = mean(respuesta_unica,na.rm = T)) %>%
                    spread(key = questionId,value = promedio) %>% rename(c('137' = 'Personal total','140' = 'Tarifa','147' = 'Edad'))


personal_tarifa_edad <- resumen_diferentes_r %>% filter(tipo_tabla == 'personal_tarifa_edad') 

personal_tarifa_edad['valor'] <- sapply(personal_tarifa_edad$valor,as.numeric)
personal_tarifa_edad_r <- personal_tarifa_edad %>% dplyr::group_by(seccion) %>%
                         dplyr::summarize(Promedio = mean(valor,na.rm = T), `25%`=quantile(valor, probs=0.25,na.rm = TRUE),
                        `50%`=quantile(valor, probs=0.5,na.rm = T),`75%`=quantile(valor, probs=0.75,na.rm = T),
                         Alto = max(valor,na.rm = T),Bajo = min(valor,na.rm = T),
                        `Más alto` = max(sueldo_alto,na.rm = T),`Más bajo` = min(sueldo_bajo,na.rm = T)) %>% 
                         left_join(mapeo_nombres %>%
                         dplyr::select(-respondido), by = c('seccion' = 'nombre_nuevo'))

personal_tarifa_edad_r <- personal_tarifa_edad_r %>% left_join(anios_exp_todos) %>% dplyr::select(-nombre_viejo)

valor_promedio <- resumen_diferentes_r %>% filter(tipo_tabla == 'valor_promedio') 
valor_promedio['valor'] <- sapply(valor_promedio$valor, as.numeric)
valor_promedio_r <- valor_promedio %>%
                    group_by(seccion) %>% dplyr::summarize(Promedio = mean(valor,na.rm = T))

# Convertimos a porcentaje
columnas_percent <- c("valor_1","valor_2","valor_1_2","valor_2_2","valor_1_3","valor_2_3",
                      "valor_1_4","valor_2_4","valor_1_5","valor_2_5")
sub_total[columnas_percent] <- apply(sub_total[columnas_percent],2, function(x) mapply(cambiar_porcent,x))

columnas_percent <- c("valor_1_1","valor_2_1","valor_1_3","valor_2_3",
                      "valor_1_4","valor_2_4")
distribucion_total[columnas_percent] <- apply(distribucion_total[columnas_percent],2, function(x) mapply(cambiar_porcent,x))

columnas_percent <- c('H%','M%')
resumen_promedio_hm[columnas_percent] <- apply(resumen_promedio_hm[columnas_percent],2, function(x) mapply(cambiar_porcent,x))

columnas_percent <- c('valor_1','valor_2','N/A')
si_no_cr_r[columnas_percent] <- apply(si_no_cr_r[columnas_percent],2, function(x) mapply(cambiar_porcent,x))
distribucion_fecha_todas['valor_1'] <- mapply(cambiar_porcent,distribucion_fecha_todas$valor_1)
  
# Pegamos todas las tablas en una
tabla_final <- rbindlist(list(resumen_normal_r,sub_total,distribucion_total,resumen_promedio_sr,resumen_distribucion_cat,resumen_normal_sr_r,resumen_promedio_hm,si_no_cr_r,
               personal_edad_anios_r,personal_tarifa_edad_r,valor_promedio_r,distribucion_fecha_todas),fill = T)



# Formateamos tabla -------------------------------------------------------
# Sustituimos los valores de infinito
tabla_final[mapply(is.infinite, Alto),'Alto'] <- NA
tabla_final[mapply(is.infinite, Bajo),'Bajo'] <- NA
tabla_final[mapply(is.infinite, `Más alto`),'Más alto'] <- NA
tabla_final[mapply(is.infinite, `Más bajo`),'Más bajo'] <- NA

write_csv(mapeo_nombres , 'Proyectos/Otros/kerma/data/interim/mappeo_nombres_resumen_reporte.csv')
write_csv(tabla_final , 'Proyectos/Otros/kerma/data/interim/tabla_resumen_final.csv')
