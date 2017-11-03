  library(dplyr)
  library(tidyr)
  library(readr)
  library(data.table)
  library(stringr)
  options(scipen = 999)
  source('Proyectos/Otros/kerma/r_script/lib/utils_script1.R')
  
  
  tabla_encuestas <- read_csv('Proyectos/Otros/kerma/data/interim/tabla_encuestas_f.csv')
  
  tabla_encuestas_f <- tabla_encuestas %>% dplyr::select(userId,name,wildcard,questionId,id_unico_pregunta,respuesta_unica)
  dupl <- tabla_encuestas_f[tabla_encuestas_f[c('userId','id_unico_pregunta')] %>% duplicated(),]
  dupl_1 <- tabla_encuestas_f[tabla_encuestas_f %>% duplicated(),]
  tabla_encuestas_w <- tabla_encuestas_f %>% spread(key = userId,value = respuesta_unica)
  quitar_strings <- c('^gerente_de_administración_','^seguros_y_salud_','^estudios_','^jubilación_y_sucesión_')
  quitar_strings <- paste(quitar_strings,collapse = '|')
  tabla_encuestas_w['id_unico_pregunta'] = gsub('_',' ',gsub(quitar_strings,'',gsub('^_','',gsub('^[0-9]+','',tabla_encuestas_w$id_unico_pregunta))))
  tabla_encuestas_w <- tabla_encuestas_w %>% plyr::rename(c('id_unico_pregunta'='categoria_pregunta')) %>%
                        mutate(categoria_pregunta = ifelse(!is.na(wildcard),wildcard,categoria_pregunta)) %>%
                        select(questionId,everything(),-wildcard)
  tabla_encuestas_w <-tabla_encuestas_w %>% arrange(questionId,categoria_pregunta)
  
write.csv(tabla_encuestas_w,'Proyectos/Otros/kerma/data/processed/tabla_comparativa_despachos.csv',
          fileEncoding = 'iso-8859-1',quote = T,row.names = F)
