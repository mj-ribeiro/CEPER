

setwd("D:/Git projects/CEPER/ceper_rais")

library(ggplot2)
library(tidyverse)
library(tmap)
library(readxl)
library("cagedExplorer")


# carregar shape file das regiÃµes de governo ---------

load('regions.rda')
region = polygons_regioes_gov_sp

rm(polygons_regioes_gov_sp)



# funcao ----


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
                             fun=function(x) paste0(formatC(x, digits=2, format="f",
                                                            decimal.mark = ','), "%")) )
    nam =  paste("G_", i, sep = "")
    assign(nam, g1)
    tmap_save(g1, filename = paste("G_", i, ".png", sep = "") )
  }
}


map(idade, new_n)


# Mapas: faixa etaria  ----------



idade = read_xlsx('evol.xlsx', sheet = 2)
idade = data.frame(idade)

idade[,2:9] = idade[,2:9]*100



new_n =  c('Regiao', '10 a 14 anos', '15 a 17 anos',
           '18 a 24 anos', '25 a 29 anos', 
           '30 a 39 anos', '40 a 49 anos',
           '50 a 64 anos', 'Mais de 65 anos', 'Total')




##  Mapas:  sexo


sex = read_xlsx('evol.xlsx', sheet = 3)

sex = data.frame(sex)

sex[,2:6] = sex[,2:6]*100 



new_n2 =  c('Região', '2003', '2007', '2011', '2015', '2019')




map(sex, new_n2)



## Mapas: por escolaridade ----



esc = read_xlsx('evol.xlsx', sheet = 4)

esc = data.frame(esc)

esc[,2:13] = esc[,2:13]*100


colnames(esc)

new_n3 = c("Região", "Analfabeto",  "Até 5ª Incompleto",
           "5ª Completo Fundamental", "6ª a 9ª Fundamental",
           "Fundamental Completo" , "Médio Incompleto", "Médio Completo",
           "Superior Incompleto",  "Superior Completo", "Mestrado",                 
           "Doutorado", "Total" )                   



map(esc, new_n3)



# end ---


