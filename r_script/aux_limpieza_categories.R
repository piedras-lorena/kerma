#AUX

library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)


tabla<-read_csv('Proyectos/Otros/kerma/data/raw/CategoryHumanCapital.csv')

columnas <- c("CategoryHumanCapitalDescription","GroupHumanCapitalDescription","RangeHumanCapitalDescription")
columnas_2 <- c("description","description_1","description_2")

formatear <- function(col){
  tolower(str_replace_all(col,c('-'='','  '=' ','   '=' ',' '='_')))
}
tabla[columnas_2]<-apply(tabla[columnas],2,formatear)

tabla <- tabla %>% mutate(id_unico_pregunta = ifelse((is.na(description_2))&(is.na(description_1))&(is.na(description)), NA,
                                                ifelse((is.na(description_2))&(is.na(description_1))&(!is.na(description)),paste(description,sep='_'),
                                                  ifelse((is.na(description_2))&(!is.na(description_1))&(is.na(description)),paste(description_1,sep='_'),
                                                    ifelse((!is.na(description_2))&(is.na(description_1))&(is.na(description)),paste(description_2,sep='_'),
                                                      ifelse((!is.na(description_2))&(!is.na(description_1))&(is.na(description)),paste(description_1,description_2,sep='_'),
                                                        ifelse((!is.na(description_2))&(is.na(description_1))&(!is.na(description)),paste(description,description_2,sep='_'),
                                                          ifelse((is.na(description_2))&(!is.na(description_1))&(!is.na(description)),paste(description,description_1,sep='_'),
                                                                  paste(description,description_1,description_2,sep='_')))))))))

write_csv(tabla,'Proyectos/Otros/kerma/data/interim/CategoryHumanCapital.csv')
