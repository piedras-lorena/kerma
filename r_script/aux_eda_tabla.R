# autor: Lorena Piedras
# fecha: 2017/09/27

# EDA TABLA ENCUESTA
# La finalidad de este script es entender la numeración de las preguntas y saber si el questionId es único para cada pregunta.


library(dplyr)
library(tidyr)
library(readr)
library(data.table)

# Análisis encuesta
encuesta_prueba<-read_csv('Proyectos/Otros/kerma/data/raw/respuestas_despachos/20170914_encuesta_prueba.csv')
duplicadas<-encuesta_prueba[(encuesta_prueba$questionId %>% duplicated()),]
duplicadas<-duplicadas %>% filter(questionId %in% preguntas)  
numero_descrip<-duplicadas %>% group_by(questionId) %>% summarise(count= n_distinct(description_2))

#Hay preguntas que se repiten pero todas tienen la misma descripción y sólo cambia la descripción_1
dup_dif<-duplicadas[duplicadas$questionId=='264',]


duplicadas_2<-encuesta_prueba[((encuesta_prueba[c('questionId','description_2')] %>% duplicated())) & (!is.na(encuesta_prueba$description_2)),]
duplicadas_1<-encuesta_prueba[((encuesta_prueba[c('questionId','description_1')] %>% duplicated())) & (is.na(encuesta_prueba$description_2)),]
#Vemos en cuántas preguntas se contesta más de una categoría

rep_date<-encuesta_prueba[(!is.na(encuesta_prueba$stringValue)) & (!is.na(encuesta_prueba$valueDate)),]
rep_answer<-encuesta_prueba[(!is.na(encuesta_prueba$stringValue)) & (!is.na(encuesta_prueba$possibleAnswers)),]

#Numeric y possible answers se repiten en seguros
rep_numeric<-encuesta_prueba[(!is.na(encuesta_prueba$valueNumeric)) & (!is.na(encuesta_prueba$possibleAnswers)),]

rep_possible<-encuesta_prueba[(!is.na(encuesta_prueba$valueDate)) & (!is.na(encuesta_prueba$possibleAnswers)),]

str(encuesta_prueba)

date<-encuesta_prueba[!is.na(encuesta_prueba$valueDate),]
