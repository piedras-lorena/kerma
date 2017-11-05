#devtools::install_github("rstudio/rmarkdown")
library(knitr)
library(markdown)
library(rmarkdown)
library(readr)
library(dplyr)
library(tidyr)


despachos <- read_csv('Proyectos/Otros/kerma/data/interim/UsuariosActivos.csv',
                      col_types = cols(.default = "c"))
despachos <- despachos %>% filter(id != '32' & id != '27')

id_despacho <- despachos$id


# for each type of car in the data create a report
# these reports are saved in output_dir with the name specified by output_file
for (id in id_despacho){
  #rm(list = ls())
  print(id)
  rmarkdown::render('Proyectos/Otros/kerma/r_script/reporte/0.03_script3_generar_reporte_pdf.Rmd',
                    output_format = 'pdf_document',
                    output_file = sprintf('reporte_despacho_%s.pdf',id),
                    output_dir = 'Proyectos/Otros/kerma/reportes_pdf/')
  rm(list = ls())
}