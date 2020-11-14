
setwd("D:/Git projects/CEPER/ceper_rais")


library(tmap)
library(readxl)


# carregar shape file das regiões de governo

load('regions.rda')
region = polygons_regioes_gov_sp

rm(polygons_regioes_gov_sp)





## dados da rais
# por faixa etária

idade = read_xlsx('rais.xlsx', sheet = 5)
idade = data.frame(idade)





region$var = idade[,2]




  tm_shape(region) +
  tm_polygons('var',  textNA = 'Sem dados') +
  tm_compass(type = "8star", position = c("right", "bottom", size = 0.0)) +
  tm_scale_bar(breaks = c(0, 100, 200, 300), text.size = 0.6) +
  tm_layout(
    legend.text.size = 0.8,
    frame = T,
    legend.format = list(text.separator = "-"))






