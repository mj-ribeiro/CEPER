setwd("D:/Git projects/CEPER/ceper_rais")


library(ggplot2)
library(tidyverse)
library(tmap)
library(readxl)
library("cagedExplorer")
library(geobr)



# geocode ----

region = read_municipality(code_muni = 'SP')
region = region[order(region$name_muni), ]




# funcao ----


map = function(df, name, p){
  for(i in 2:length(df)){
    
    region$var = df[,i]
    
    g1 = tm_shape(region) +
      tm_polygons('var',
                  title = name[i-1], 
                  textNA = 'Sem dados',
                  palette =  "Reds") +
      tm_compass(type = "8star", position = c("right", "bottom", size = 0.0)) +
      tm_scale_bar(breaks = c(0, 100, 200, 300), text.size = 0.6) +
      tm_layout(
        legend.text.size = 1,
        frame = T,
        legend.format = list(text.separator = "a", 
                  fun=function(x) paste0(formatC(x, digits=2, format="f",
                                      decimal.mark = ','), if(p==T){"%"})) )
    nam =  paste("G_", i, sep = "")
    assign(nam, g1)
    tmap_save(g1, filename = paste("G_", i, ".png", sep = "") )
  }
}



# sex ---- 
# diferenca % entre homens e mulheres para os tres anos da analise


sex = read_xlsx('evol2_mun.xlsx', sheet = 'sex')
sex = data.frame( sex[order(sex$Município), ] )

sex[,2:4] = sex[,2:4]*100


# map

n_sex = c('2019',	'2010',	'2002')

map(sex, n_sex )



# idade remuneracao ----

# remuneracao pras faixas de idade para os tres anos da analise

# 2002

idade_rem_02 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_rem_02')
idade_rem_02 = data.frame( idade_rem_02[order(idade_rem_02$Município), ] )


# map

n_id_rem_02 = c('18 a 29', '30 a 64', '65 ou mais')
map(idade_rem_02, n_id_rem_02, F)




# 2010

idade_rem_10 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_rem_10')
idade_rem_10 = data.frame( idade_rem_10[order(idade_rem_10$Município), ] )


# map

n_id_rem_10 = c('18 a 29', '30 a 64', '65 ou mais')
map(idade_rem_10, n_id_rem_10, F)



# 2019

idade_rem_19 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_rem_19')
idade_rem_19 = data.frame( idade_rem_19[order(idade_rem_19$Município), ] )


# map

n_id_rem_19 = c('18 a 29', '30 a 64', '65 ou mais')
map(idade_rem_19, n_id_rem_19, F)



# idade_população  ----

# a composição percentual pras faixas de idade para os três anos escolaridade

# 2002

idade_pop_02 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_pop_02')
idade_pop_02 = data.frame( idade_pop_02[order(idade_pop_02$Município), ] )
idade_pop_02[,2:4] = idade_pop_02[,2:4]*100


# map

n_id_pop_02 = c('18 a 29', '30 a 64', '65 ou mais')
map(idade_pop_02, n_id_pop_02, T)


# 2010

idade_pop_10 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_pop_10')
idade_pop_10 = data.frame( idade_pop_10[order(idade_pop_10$Município), ] )
idade_pop_10[,2:4] = idade_pop_10[,2:4]*100


# map

n_id_pop_10 = c('18 a 29', '30 a 64', '65 ou mais')
map(idade_pop_10, n_id_pop_10, T)



# 2019

idade_pop_19 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_pop_19')
idade_pop_19 = data.frame( idade_pop_19[order(idade_pop_19$Município), ] )
idade_pop_19[,2:4] = idade_pop_19[,2:4]*100


# map

n_id_pop_19 = c('18 a 29', '30 a 64', '65 ou mais')
map(idade_pop_19, n_id_pop_19, T)




# escolaridade_remuneracao ----
# variacao da remuneracao - comparacao entre 2010 e 2019



esc_rem = read_xlsx('evol2_mun.xlsx', sheet = 'esc_rem')
esc_rem = data.frame( esc_rem[order(esc_rem$Município), ] )
esc_rem[,2:5] = esc_rem[,2:5]*100



# map


n_esc_rem = c('Analfabeto',	'Fundamental Completo',
              'Médio Completo',	'Superior Completo',
              'Total')


map(esc_rem, n_esc_rem, T)



# escolaridade populacao ----
# composição % da forca de trabalho para 2010 e 2019


# 2010

esc_pop_10 = read_xlsx('evol2_mun.xlsx', sheet = 'esc_pop_10')
esc_pop_10 = data.frame( esc_pop_10[order(esc_pop_10$Município), ] )
esc_pop_10[,2:5] = esc_pop_10[,2:5]*100



# map


n_esc_pop_10 = c('Analfabeto',	'Fundamental Completo',
              'Médio Completo',	'Superior Completo')


map(esc_pop_10, n_esc_pop_10, T)




# 2019

esc_pop_19 = read_xlsx('evol2_mun.xlsx', sheet = 'esc_pop_19')
esc_pop_19 = data.frame( esc_pop_19[order(esc_pop_19$Município), ] )
esc_pop_19[,2:5] = esc_pop_19[,2:5]*100



# map


n_esc_pop_19 = c('Analfabeto',	'Fundamental Completo',
                 'Médio Completo',	'Superior Completo')


map(esc_pop_19, n_esc_pop_19, T)



# cor_raça_SP ----
# rendimento por cor para 2010


cor1 = read_xlsx('evol2_mun.xlsx', sheet = 'cor')
cor1 = data.frame( cor1[order(cor1$muni), ] )



# map


n_cor1 = c('Total',	'Branca',	'Preta',
           'Amarela',	'Parda',
           'Indígena',	'Sem declaração', 'Branca + Amarela',
           'Negros')


map(cor1, n_cor1, F)





# Dif salarial por cor_raça_SP ----
# rendimento por cor para 2010


cor_dif = read_xlsx('evol2_mun.xlsx', sheet = 'cor_dif')
cor_dif = data.frame( cor_dif[order(cor_dif$muni), ] )
cor_dif[2] = cor_dif[2]*100



# map

n_cor_dif = c('Diferença % salarial entre Brancos/Amarelos e Pretos/Pardos')

map(cor_dif, n_cor_dif, T)
















