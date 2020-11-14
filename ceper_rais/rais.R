

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


old_n = colnames(idade)


new_n =  c('Região', '10 a 14 anos', '15 a 17 anos',
                    '18 a 20 anos', '25 a 29 anos', 
                    '30 a 39 anos', '40 a 49 anos',
                    '50 a 64 anos', 'Mais de 65 anos', 'Total')


### mapas  salário por faixa etária


map = function(df, name){
  for(i in 2:length(df)){

  region$var = df[,i]

  g1 = tm_shape(region) +
  tm_polygons('var', title = name[i],  textNA = 'Sem dados') +
  tm_compass(type = "8star", position = c("right", "bottom", size = 0.0)) +
  tm_scale_bar(breaks = c(0, 100, 200, 300), text.size = 0.6) +
  tm_layout(
    legend.text.size = 1,
    frame = T,
    legend.format = list(text.separator = "-", 
                         fun=function(x) formatC(x, digits=0, format="d")))
  nam =  paste("G_", i, sep = "")
  assign(nam, g1)
  tmap_save(g1, filename = paste("G_", i, ".png", sep = "") )
}
}


map(idade, new_n)



#### Mapas: por sexo


sex = read_xlsx('rais.xlsx', sheet = 3)

sex = data.frame(sex)



new_n2 =  c('Região', 'Masculino', 'Feminino', 'Total')

map(sex, new_n2)



#### Mapas por escolaridade



esc = read_xlsx('rais.xlsx', sheet = 2)

esc = data.frame(esc)


new_n3 = c("Região", "Analfabeto",  "Até 5ª Incompleto",
           "5ª Completo Fundamental", "6ª a 9ª Fundamental",
           "Fundamental Completo" , "Médio Incompleto", "Médio Completo",
            "Superior Incompleto",  "Superior Completo", "Mestrado",                 
           "Doutorado", "Total" )                   



map(esc, new_n3)








