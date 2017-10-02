#SCRIPT 1
#Este script lee las tablas de los despachos, filtra las preguntas que van a ir en el reporte, las limpia,
#les hace las operaciones necesarias y les pega la sección del reporte en las que van a aparecer

library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)

source('Proyectos/Otros/kerma/R/utils_script1.R')


# Mapeo de preguntas ------------------------------------------------------

#Importamos tabla donde vienen las preguntas que se van a usar
mapeo_preguntas<-read_csv('Proyectos/Otros/kerma/data/interim/mappeo_preguntas - mappeo (1).csv')

#Filtramos y arreglamos rangos de números
mapeo_p_dash<-mapeo_preguntas %>% filter(grepl('-',questionId_tabla))
mapeo_preguntas<-mapeo_preguntas %>% filter(!grepl('-',questionId_tabla))

res<-apply(mapeo_p_dash['questionId_tabla'],1,function(x) range_to_number(x))

for(i in 1:nrow(mapeo_p_dash)){
  mapeo_p_dash$questionId_tabla[i]<-gsub(' ',',',paste(res[[i]],collapse=' '))
}

mapeo_preguntas<-mapeo_preguntas%>% bind_rows(mapeo_p_dash) %>%
                 mutate(questionId_tabla=gsub('[ \t\n\r\f\v]','',questionId_tabla))
rm(mapeo_p_dash)

#Hacemos un vector con las preguntas que nos interesan
preguntas<-na.omit(mapeo_preguntas['questionId_tabla'])
preguntas<-unique(unlist(strsplit(preguntas$questionId_tabla, split=",")))


# Tabla de respuestas -----------------------------------------------------
# Leemos todas las encuestas de los despachos y las pegamos en una tabla grande
tabla_encuestas<-read_csv('Proyectos/Otros/kerma/data/raw/respuestas_despachos/20170914_encuesta_prueba.csv')


filesEncuesta <- list.files(path = "Proyectos/Otros/kerma/data/raw/respuestas_despachos/", pattern= "20170914_")

#tabla_encuestas <- rbindlist(lapply(paste("Proyectos/Otros/kerma/data/raw/respuestas_despachos/",filesEncuesta, sep="/"), 
#                                    importar_encuesta))

# Filtramos las preguntas que nos interesan, asignamos id único y tipo de respuesta

tabla_encuestas$description_1_t<- tolower(str_replace_all(tabla_encuestas$description_1,c('-'='','  '=' ','   '=' ',' '='_')))
tabla_encuestas$description_2_t<- tolower(str_replace_all(tabla_encuestas$description_2,c('-'='','  '=' ','   '=' ',' '='_')))
tabla_encuestas$description_t<- tolower(str_replace_all(tabla_encuestas$description,c('-'='','  '=' ','   '=' ',' '='_')))

preguntas <- as.numeric(preguntas)
tabla_encuestas_f  <- tabla_encuestas %>% filter(questionId %in% preguntas) %>%
                      mutate(id_unico_pregunta = ifelse((is.na(description_2))&(is.na(description_1))&(is.na(description)), questionId,
                                                  ifelse((is.na(description_2))&(is.na(description_1))&(!is.na(description)),paste(questionId,description_t,sep='_'),
                                                    ifelse((is.na(description_2))&(!is.na(description_1))&(is.na(description)),paste(questionId,description_1_t,sep='_'),
                                                      ifelse((!is.na(description_2))&(is.na(description_1))&(is.na(description)),paste(questionId,description_2_t,sep='_'),
                                                        ifelse((!is.na(description_2))&(!is.na(description_1))&(is.na(description)),paste(questionId,description_1_t,description_2_t,sep='_'),
                                                          ifelse((!is.na(description_2))&(is.na(description_1))&(!is.na(description)),paste(questionId,description_t,description_2_t,sep='_'),
                                                            ifelse((is.na(description_2))&(!is.na(description_1))&(!is.na(description)),paste(questionId,description_t,description_1_t,sep='_'),
                                                                   paste(questionId,description_t,description_1_t,description_2_t,sep='_')))))))),
                             tipo_respuesta = ifelse(!is.na(valueNumeric)&!is.na(possibleAnswers),'numeric_possibleanswers',
                                                     ifelse(!is.na(valueNumeric)&is.na(possibleAnswers)&is.na(stringValue)&is.na(valueDate),'numeric',
                                                            ifelse(is.na(valueNumeric)&!is.na(possibleAnswers)&is.na(stringValue)&is.na(valueDate),'possibleanswers',
                                                                   ifelse(is.na(valueNumeric)&is.na(possibleAnswers)&!is.na(stringValue)&is.na(valueDate),'stringvalue',
                                                                          ifelse(is.na(valueNumeric)&is.na(possibleAnswers)&is.na(stringValue)&!is.na(valueDate),'valuedate',
                                                                                 ifelse(is.na(valueNumeric)&is.na(possibleAnswers)&is.na(stringValue)&is.na(valueDate),
                                                                                        'noanswer','others')))))),
                             respuesta_unica = ifelse(tipo_respuesta=='numeric',as.character(valueNumeric),
                                                      ifelse(tipo_respuesta=='possibleanswers',possibleAnswers,
                                                             ifelse(tipo_respuesta=='stringvalue',stringValue,
                                                                    ifelse(tipo_respuesta=='valuedate',valueDate,NA))))) %>%
                     select(-description_1_t,-description_2_t) %>% as.data.table()

# Checar qué aparecen en others
tabla_encuestas_f[tabla_encuestas_f$tipo_respuesta == 'others']

numeric_possibleanswers_1 <- tabla_encuestas_f[tabla_encuestas_f$tipo_respuesta=='numeric_possibleanswers'] %>%
                           mutate(num=1)
numeric_possibleanswers_2 <- numeric_possibleanswers_1 %>% as.data.frame() %>%
                             mutate(num=2)

numeric_possibleanswers <- numeric_possibleanswers_1 %>% rbind(numeric_possibleanswers_2) %>%
                           mutate(respuesta_unica = ifelse(num==1,valueNumeric,possibleAnswers),
                                  tipo_respuesta = ifelse(num==1,'numeric','possibleanswers'),
                                  id_unico_pregunta = ifelse(num==1,paste(id_unico_pregunta,'numeric',sep='_'),
                                                             paste(id_unico_pregunta,'possibleanswers',sep='_'))) %>%
                           select(-num)


tabla_encuestas_f <- tabla_encuestas_f %>% filter(tipo_respuesta!='numeric_possibleanswers') %>% 
                     rbind(numeric_possibleanswers)

tabla_encuesta_wide <- tabla_encuestas_f[c('userId','id_unico_pregunta','respuesta_unica')] %>% 
                       spread(key = id_unico_pregunta,value = respuesta_unica)


# Tabla reporte -----------------------------------------------------------
# COL_1
# Operaciones de columnas
# Importamos la tabla donde vienen las operaciones a realizar

operaciones <- read_csv('Proyectos/Otros/kerma/data/interim/operaciones_preguntas - col_1.csv') %>%
               mutate(columna_1 = ifelse(is.na(categoria_pregunta_1),pregunta_1,paste(pregunta_1,categoria_pregunta_1,sep='_')),
                      columna_2 = ifelse(!is.na(pregunta_2) & !is.na(categoria_pregunta_2),paste(pregunta_2,categoria_pregunta_2,sep='_'),
                                         ifelse(!is.na(pregunta_2) & is.na(categoria_pregunta_2),pregunta_2,NA)),
                      columna_3 = ifelse(!is.na(pregunta_3) & !is.na(categoria_pregunta_2),paste(pregunta_3,categoria_pregunta_2,sep='_'),
                                         ifelse(!is.na(pregunta_3) & is.na(categoria_pregunta_2),pregunta_3,NA)),
                      nombre_columna_nueva = ifelse(is.na(subseccion),paste(seccion,renglon,sep='_') , paste(seccion,subseccion,renglon,sep='_'))) %>% 
  select(columna_1,columna_2,columna_3,funcion,nombre_columna_nueva)

tabla_reporte <- data.frame(user_id=tabla_encuesta_wide$userId)
mapeo_nombres <- data.frame(nombre_viejo = character(0),
                            nombre_nuevo = character(0),
                            respondido = numeric(0), stringsAsFactors = F)


apply(operaciones,1,function(x) operaciones_col(x['columna_1'],x['columna_2'],x['columna_3'],x['funcion'],x['nombre_columna_nueva']))

tabla_reporte_l <- tabla_reporte %>% gather(key = seccion, value = valor,-user_id)

# Agarramos aleatoriamente 2 de cada operación para checar que estén bien hechas
chequeo <- mapeo_nombres %>% left_join(operaciones %>% select(nombre_columna_nueva,funcion),by = c('nombre_nuevo'='nombre_columna_nueva'))
table(chequeo$respondido)

chequeo_2 <- chequeo %>% filter(respondido == 1) %>% group_by(funcion) %>% sample_n(2)

reporte_chequeo <- tabla_reporte_l %>% filter(seccion %in% chequeo_2$nombre_nuevo)

merge_chequeo <- tabla_reporte_l %>% left_join(operaciones %>% select(nombre_columna_nueva,funcion),by = c('seccion'='nombre_columna_nueva'))

# COL_2 y # COL_3
# Las secciones 2.1,2.2,2.3 tienen tres columnas
operaciones_col1 <- operaciones %>% filter(grepl('^2_1_|^2_2_|^2_3_',nombre_columna_nueva))

tabla_reporte_col_1 <- data.frame(user_id=tabla_encuesta_wide$userId)

apply(operaciones_col1,1,function(x) operaciones_col_1(x['columna_1'],x['columna_2'],x['nombre_columna_nueva']))

tabla_reporte_col_2 <- data.frame(user_id=tabla_encuesta_wide$userId)

apply(operaciones_col1,1,function(x) operaciones_col_2(x['columna_2'],x['columna_1'],x['nombre_columna_nueva']))


# Tablas especiales -------------------------------------------------------


