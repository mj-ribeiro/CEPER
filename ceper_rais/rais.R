
setwd("D:/Git projects/CEPER/ceper_rais")


library(tmap)
library(readxl)


# carregar shape file das regiões de governo

load('regions.rda')
region = polygons_regioes_gov_sp

rm(polygons_regioes_gov_sp)



tm_shape(polygons_regioes_gov_sp) + tm_polygons()


## dados da rais
# por faixa etária

idade = read_xlsx('rais.xlsx', sheet = 5)
idade = data.frame(idade)



head(idade)











