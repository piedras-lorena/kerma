#devtools::install_github("rstudio/rmarkdown")
library(knitr)
library(markdown)
library(rmarkdown)
library(readr)
library(dplyr)
library(tidyr)

id_submuestra <- '1'

setwd('/home/lorena/')
despachos <- read_csv('Proyectos/Otros/kerma/data/interim/submuestras.csv',
                      col_types = cols(.default = "c"))
despachos <- despachos %>% filter(id != '32' & id != '27' & submuestra == id_submuestra)

id_despacho <- despachos$id


# for each type of car in the data create a report
# these reports are saved in output_dir with the name specified by output_file
for (id in id_despacho){
  rmarkdown::render('Proyectos/Otros/kerma/r_script/reporte/0.06_aux_script_reporte_submuestra.Rmd',
                    output_format = 'pdf_document',
                    output_file = sprintf('reporte_submuestra_despacho_%s.pdf',id),
                    output_dir = 'Proyectos/Otros/kerma/reportes_submuestras_pdf/submuestra_1/')
  rm(list = ls())
}