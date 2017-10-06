#SCRIPT 1
#Este script lee las tablas de los despachos, filtra las preguntas que van a ir en el reporte, las limpia,
#les hace las operaciones necesarias y les pega la sección del reporte en las que van a aparecer

library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)

source('Proyectos/Otros/kerma/r_script/lib/utils_script1.R')


# Mapeo de preguntas ------------------------------------------------------

#Importamos tabla donde vienen las preguntas que se van a usar
mapeo_preguntas<-read_csv('Proyectos/Otros/kerma/data/interim/mappeo_preguntas - mappeo.csv', col_types = cols(.default = "c"))

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
tabla_encuestas<-read_csv('Proyectos/Otros/kerma/data/raw/respuestas_despachos/20170914_encuesta_prueba.csv', col_types = cols(.default = "c"))


filesEncuesta <- list.files(path = "Proyectos/Otros/kerma/data/raw/respuestas_despachos/", pattern= "20170914_")

#tabla_encuestas <- rbindlist(lapply(paste("Proyectos/Otros/kerma/data/raw/respuestas_despachos/",filesEncuesta, sep="/"), 
#                                    importar_encuesta))

# Filtramos las preguntas que nos interesan, asignamos id único y tipo de respuesta

tabla_encuestas$description_1_t<- tolower(str_replace_all(tabla_encuestas$description_1,c('-'='','  '=' ','   '=' ',' '='_')))
tabla_encuestas$description_2_t<- tolower(str_replace_all(tabla_encuestas$description_2,c('-'='','  '=' ','   '=' ',' '='_')))
tabla_encuestas$description_t<- tolower(str_replace_all(tabla_encuestas$description,c('-'='','  '=' ','   '=' ',' '='_')))

preguntas <- as.numeric(preguntas)
tabla_encuestas$questionId <- sapply(tabla_encuestas$questionId,as.numeric)
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

#Exportamos la tabla
write_csv(tabla_encuestas_f,'Proyectos/Otros/kerma/data/interim/tabla_encuestas_f.csv')

tabla_encuesta_wide <- tabla_encuestas_f[c('userId','id_unico_pregunta','respuesta_unica')] %>% 
                       spread(key = id_unico_pregunta,value = respuesta_unica)


# Tabla reporte -----------------------------------------------------------
# COL_1
# Operaciones de columnas
# Importamos la tabla donde vienen las operaciones a realizar

operaciones <- read_csv('Proyectos/Otros/kerma/data/interim/operaciones_preguntas - col_1.csv',
                        col_types =  cols(.default = "c")) %>%
               mutate(columna_1 = ifelse(is.na(categoria_pregunta_1),pregunta_1,paste(pregunta_1,categoria_pregunta_1,sep='_')),
                      columna_2 = ifelse(!is.na(pregunta_2) & !is.na(categoria_pregunta_2),paste(pregunta_2,categoria_pregunta_2,sep='_'),
                                         ifelse(!is.na(pregunta_2) & is.na(categoria_pregunta_2),pregunta_2,NA)),
                      columna_3 = ifelse(!is.na(pregunta_3) & !is.na(categoria_pregunta_2),paste(pregunta_3,categoria_pregunta_2,sep='_'),
                                         ifelse(!is.na(pregunta_3) & is.na(categoria_pregunta_2),pregunta_3,NA)),
                      nombre_columna_nueva = ifelse(is.na(subseccion),paste(seccion,'0',renglon,sep='_') , paste(seccion,subseccion,renglon,sep='_'))) %>% 
  select(columna_1,columna_2,columna_3,funcion,nombre_columna_nueva)

tabla_reporte <- data.frame(userId=tabla_encuesta_wide$userId)
mapeo_nombres <- data.frame(nombre_viejo = character(0),
                            nombre_nuevo = character(0),
                            respondido = numeric(0), stringsAsFactors = F)


apply(operaciones,1,function(x) operaciones_col(x['columna_1'],x['columna_2'],x['columna_3'],x['funcion'],x['nombre_columna_nueva']))

tabla_reporte_l <- tabla_reporte %>% gather(key = seccion, value = valor,-userId)

# Agarramos aleatoriamente 2 de cada operación para checar que estén bien hechas
chequeo <- mapeo_nombres %>% left_join(operaciones %>% select(nombre_columna_nueva,funcion),by = c('nombre_nuevo'='nombre_columna_nueva'))
table(chequeo$respondido)

chequeo_2 <- chequeo %>% filter(respondido == 1) %>% group_by(funcion) %>% sample_n(2)

reporte_chequeo <- tabla_reporte_l %>% filter(seccion %in% chequeo_2$nombre_nuevo)

merge_chequeo <- tabla_reporte_l %>% left_join(operaciones %>% select(nombre_columna_nueva,funcion),by = c('seccion'='nombre_columna_nueva'))

# COL_2 y # COL_3
# Las secciones 2.1,2.2,2.3 tienen tres columnas
operaciones_col1 <- operaciones %>% filter(grepl('^2_1_|^2_2_|^2_3_',nombre_columna_nueva))

tabla_reporte_col_1 <- data.frame(userId=tabla_encuesta_wide$userId)
apply(operaciones_col1,1,function(x) operaciones_col_1(x['columna_1'],x['columna_2'],x['nombre_columna_nueva']))
tabla_reporte_col_1_l <- tabla_reporte_col_1 %>% gather(key = seccion, value= porcent_hombres,-userId)

tabla_reporte_col_2 <- data.frame(userId=tabla_encuesta_wide$userId)
apply(operaciones_col1,1,function(x) operaciones_col_2(x['columna_2'],x['columna_1'],x['nombre_columna_nueva']))
tabla_reporte_col_2_l <- tabla_reporte_col_2 %>% gather(key = seccion, value= porcent_mujeres,-userId)

# Juntamos todas las tablas en una
tabla_reporte_todas <- tabla_reporte_l %>% left_join(tabla_reporte_col_1_l) %>% left_join(tabla_reporte_col_2_l)



# Tablas especiales -------------------------------------------------------

#TABLA ESPECIAL 2.19
tabla_encuestas_f['valueNumeric'] <- sapply(tabla_encuestas_f$valueNumeric,as.numeric)
abogados <- tabla_encuestas_f %>% filter(description == 'Abogado' & description_1 == 'Tarifa' & questionId %in% c(113,114)) %>%
            group_by(userId) %>% summarise(total_abogados = sum(valueNumeric,na.rm=T))

socios <- tabla_encuestas_f %>% filter(description == 'Socio' & questionId %in% c(96,97)) %>%
          group_by(userId) %>% summarise(total_socios = sum(valueNumeric,na.rm=T))

socios_propietarios <- tabla_encuestas_f %>% filter(groupHumanCapital == 4 & questionId %in% c(96,97)) %>%
                       group_by(userId) %>% summarise(total_socios_prop = sum(valueNumeric,na.rm=T))
cat_administrativo <- c('Gerente de Administración','Staff de Administración','Apoyo de Administración')
rsp_administrativo <- c(243,244,264,265,287,288)
administrativo <- tabla_encuestas_f %>% filter(description %in% cat_administrativo & questionId %in% rsp_administrativo) %>%
                  group_by(userId) %>% summarise(total_admin = sum(valueNumeric,na.rm=T))

rsp_profesional <- c(96,97,113,114,192,193)
profesional <- tabla_encuestas_f %>% filter(questionId %in% rsp_profesional) %>%
               group_by(userId) %>% summarise(total_profesional = sum(valueNumeric,na.rm=T))

pasantes <- tabla_encuestas_f %>% filter(questionId %in% c(192,193)) %>%
            group_by(userId) %>% summarise(total_pasantes = sum(valueNumeric,na.rm=T))

group_hc <- c(23,34,35,36)
secretarias <- tabla_encuestas_f %>% filter(groupHumanCapital %in% group_hc & questionId %in% c(264,265)) %>%
               group_by(userId) %>% summarise(total_secretarias = sum(valueNumeric,na.rm=T))

archivo <- tabla_encuestas_f %>% filter(groupHumanCapital == 24 & questionId %in% c(287,288)) %>%
           group_by(userId) %>% summarise(total_archivo = sum(valueNumeric,na.rm=T))

rsp_total <- c(96,97,113,114,192,193,243,244,264,265,287,288,164,165)
total <- tabla_encuestas_f %>% filter(questionId %in% rsp_total) %>%
         group_by(userId) %>% summarise(total = sum(valueNumeric,na.rm=T))

ingresos <- tabla_encuestas_f %>% filter(questionId == 12) %>%
           group_by(userId) %>% summarise(ingresos = sum(valueNumeric,na.rm=T))

razones <- total %>% full_join(archivo) %>% full_join(secretarias) %>% full_join(pasantes) %>% full_join(profesional) %>% full_join(administrativo) %>% 
           full_join(socios) %>% full_join(abogados) %>% full_join(socios_propietarios) %>% full_join(ingresos) %>%
           mutate('total_pasantes_abogados' = total_abogados + total_pasantes,
                  '2_19_1' = total_abogados/total_socios,
                  '2_19_2' = total_admin/total_profesional,
                  '2_19_3' = total_pasantes/total_socios,
                  '2_19_4' = total_secretarias/total_abogados,
                  '2_19_5' = total_secretarias/total_pasantes,
                  '2_19_6' = total_secretarias/total_socios,
                  '2_19_7' = total_archivo/total_abogados,
                  '2_19_8' = total_archivo/total,
                  '7_3_1' = ingresos/total_socios_prop,
                  '7_3_2' = ingresos/total_socios,
                  '7_3_3' = ingresos/total_abogados,
                  '7_3_4' = ingresos/total_pasantes_abogados) %>% 
           select(userId,`2_19_1`:`7_3_4`)

razones_l <- razones %>% gather(key = seccion, value= valor,-userId)
mapeo_nombres_razones <- data.frame(nombre_viejo = c('Abogados/Socios',
                                                     'Administrativo/Profesional',
                                                     'Pasantes/Socios',
                                                     'Secretarias/Abogados',
                                                     'Secretarias/Pasantes',
                                                     'Secretarias/Socios',
                                                     'Archivo/Abogados',
                                                     'Archivo/Total personal',
                                                     'Ingresos/Socios propietarios',
                                                     'Ingresos/Socios',
                                                     'Ingresos/Abogados',
                                                     'Ingresos/Abogados y pasantes'),
                                    nombre_nuevo = c('2_19_1','2_19_2','2_19_3',
                                                     '2_19_4','2_19_5','2_19_6',
                                                     '2_19_7','2_19_8',
                                                     '7_3_1','7_3_2','7_3_3',
                                                     '7_3_4'),
                                    respondido = rep(NA,12),
                                    stringsAsFactors = F)

mapeo_nombres <- mapeo_nombres %>% bind_rows(mapeo_nombres_razones)
tabla_reporte_todas <- rbindlist(list(tabla_reporte_todas, razones_l), fill=T)
write_csv(tabla_reporte_todas,'Proyectos/Otros/kerma/data/interim/tabla_reporte_prueba.csv')
write_csv(mapeo_nombres,'Proyectos/Otros/kerma/data/interim/mapeo_nombres.csv')
