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




# breaks  ----


mybreaks1 = c(-50, -25, 0, 25, Inf)
mybreaks2 = c(0, 1000, 2000, 3000, Inf)
mybreaks3 = c(0, 10, 20, 30, 40, 50, 60, Inf)  
  
mybreaks4 = c(-100, -50, 0, 50, 100, Inf) 

mybreaks5 = c(0, 10, 20, 30, 40, 50, Inf)
mybreaks6 = c(0, 1000, 2000, 3000, 4000, Inf)

mybreaks7 = c(20, 40, 60, 80, Inf) 


# labels ----  
  
mylabels1 = c( '-50% a -25%',
              '-24% a 0%', 
              '1% a 25%',
              'mais de 25%'
               )


mylabels2 = c('0 a 1000',
               '1001 a 2000', 
               '2001 a 3000',
               '3001 ou mais')

mylabels3 = c('0% a 10%',
              '11% a 20%',
              '21% a 30%',
              '31% a 40%',
              '41% a 50%',
              '51% a 60%',
              'mais de 60%')

mylabels4 = c('-100% a -50%', 
              '-49% a 0%',
              '1% a 50%', 
              '51% a 100%',
              'mais de 100%')



mylabels5 = c('0% a 10%',
              '11% a 20%',
              '21% a 30%',
              '31% a 40%',
              '41% a 50%',
              'mais de 50%')

mylabels6 = c('0 a 1000', 
              '1001 a 2000',
              '2001 a 3000',
              '3001 a 4000',
              'mais de 4000')
  
  
  
  
mylabels7 = c('20% a 40%',
              '41% a 60%',
              '61% a 80%',
              'mais de 80%')
              
              
  
  


# colors ----


mycolor1 =c('darkorange4', 'darkorange2', 'orange', 'beige')               
mycolor2 = c('darkorange4', 'orange', 'burlywood1','beige')
mycolor3 = 'Reds'
mycolor4  = c('darkorange4','darkorange3', 'darkorange1',  'orange', 'beige')
mycolor5 = c('darkorange4','darkorange3', 'darkorange1',  'orange', 'burlywood1', 'beige')
mycolor6 = c('darkorange4', 'darkorange1', 'orange', 'burlywood1','beige')
mycolor7 = c('darkorange4', 'orange', 'burlywood1','beige')



mybreaks3 = function(i){
    if(i==2){
      mybreaks3.1
    }else if(i==3){
    mybreaks3.2
    }else if(i==4){
     mybreaks3.3
      }
  }




mybreaks3.1 = c(0,  10, 20, 30, 40, Inf)
mybreaks3.2 = c(40, 50, 60, 70, 80,  Inf)

mybreaks3.3 = c(0, 1, 2, 3, 4, Inf)





mylabels3 = function(i){
        if(i==2){
          mylabels3.1
        }else if(i==3){
          mylabels3.2
        }else if(i==4){
          mylabels3.3
        }
}


mylabels3.1 = c('0% a 10%',
                '11% a 20%',
                '21% a 30%',
                '31% a 40%',
                'mais de 40%')


mylabels3.2 = c('40% a 50%',
                '51% a 60%',
                '61% a 70%',
                '71% a 80%',
                'mais de 80%')


mylabels3.3 = c('0% a 1%',
                '1,1% a 2%',
                '2,1% a 3%',
                '3,1% a 4%',
                'mais de 4%')





mycolor3.0 = c('darkorange4', 'darkorange1', 'orange', 'burlywood1','beige')






map = function(df, name, br){
  for(i in 2:length(df)){
    region$var = df[,i]

    g1 = tm_shape(region) +
      tm_polygons('var',
                  title = name[i-1], 
                  breaks = if(br==1){mybreaks1}else if(br==2){mybreaks2}else if(br==3){mybreaks3(i)}else if(br==4){mybreaks4}else if(br==5){mybreaks5}else if(br==6){mybreaks6}else if(br==7){mybreaks7},
                  labels = if(br==1){mylabels1}else if(br==2){mylabels2}else if(br==3){mylabels3(i)}else if(br==4){mylabels4}else if(br==5){mylabels5}else if(br==6){mylabels6}else if(br==7){mylabels7},
                  textNA = 'Sem dados',
                  midpoint =NA,
                  palette = if(br==1){mycolor1}else if(br==2){mycolor2}else if(br==3){mycolor3.0}else if(br==4){mycolor4}else if(br==5){mycolor5}else if(br==6){mycolor6}else if(br==7){mycolor7},
                  style = "fixed",
) +
      tm_compass(type = "8star", position = c("right", "bottom", size = 0.0)) +
      tm_scale_bar(breaks = c(0, 100, 200, 300), text.size = 0.6) +
      tm_layout(
        legend.text.size = 1,
        frame = T,
        )
    nam =  paste("G_", i, sep = "")
    assign(nam, g1)
    tmap_save(g1, filename = paste("G_", i, ".png", sep = "") )
  }
}



# 1 - sex ---- 
# diferenca % entre homens e mulheres para os tres anos da analise


sex = read_xlsx('evol2_mun.xlsx', sheet = 'sex')
sex = data.frame( sex[order(sex$Município), ] )

sex[,2:4] = sex[,2:4]*100


# map

n_sex = c('2019',	'2010',	'2002')

map(sex, n_sex, br=1 )



# 2 - idade remuneracao ----

#  remuneracao pras faixas de idade para os tres anos da analise

# 2002

idade_rem_02 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_rem_02')
idade_rem_02 = data.frame( idade_rem_02[order(idade_rem_02$Município), ] )


# map

n_id_rem_02 = c('18 a 29 anos', '30 a 64 anos', '65 anos ou mais')
map(idade_rem_02, n_id_rem_02, 2)




# 2010

idade_rem_10 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_rem_10')
idade_rem_10 = data.frame( idade_rem_10[order(idade_rem_10$Município), ] )


# map

n_id_rem_10 = c('18 a 29 anos', '30 a 64 anos', '65 anos ou mais')
map(idade_rem_10, n_id_rem_10, 2)



# 2019

idade_rem_19 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_rem_19')
idade_rem_19 = data.frame( idade_rem_19[order(idade_rem_19$Município), ] )


# map

n_id_rem_19 = c('18 a 29 anos', '30 a 64 anos', '65 anos ou mais')
map(idade_rem_19, n_id_rem_19, 2)



# 3 - idade_população  ----

# a composição percentual pras faixas de idade para os três anos escolaridade

# 2002

idade_pop_02 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_pop_02')
idade_pop_02 = data.frame( idade_pop_02[order(idade_pop_02$Município), ] )
idade_pop_02[,2:4] = idade_pop_02[,2:4]*100




# map

n_id_pop = c('18 a 29 anos', '30 a 64 anos', '65 anos ou mais')
map(idade_pop_02, n_id_pop, 3)


# 2010

idade_pop_10 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_pop_10')
idade_pop_10 = data.frame( idade_pop_10[order(idade_pop_10$Município), ] )
idade_pop_10[,2:4] = idade_pop_10[,2:4]*100


# map

map(idade_pop_10, n_id_pop, 3)



# 2019

idade_pop_19 = read_xlsx('evol2_mun.xlsx', sheet = 'idade_pop_19')
idade_pop_19 = data.frame( idade_pop_19[order(idade_pop_19$Município), ] )
idade_pop_19[,2:4] = idade_pop_19[,2:4]*100


# map

map(idade_pop_19, n_id_pop, 3)




# 4 - escolaridade_remuneracao ----
# variacao da remuneracao - comparacao entre 2010 e 2019



esc_rem = read_xlsx('evol2_mun.xlsx', sheet = 'esc_rem')
esc_rem = data.frame( esc_rem[order(esc_rem$Município), ] )
esc_rem[,2:5] = esc_rem[,2:5]*100



# map


n_esc_rem = c('Analfabeto',	'Fundamental Completo',
              'Médio Completo',	'Superior Completo',
              'Total')


map(esc_rem, n_esc_rem, 4)



# 5 - escolaridade populacao ----
# composição % da forca de trabalho para 2010 e 2019


# 2010

esc_pop_10 = read_xlsx('evol2_mun.xlsx', sheet = 'esc_pop_10')
esc_pop_10 = data.frame( esc_pop_10[order(esc_pop_10$Município), ] )
esc_pop_10[,2:5] = esc_pop_10[,2:5]*100



# map


n_esc_pop = c('Analfabeto',	'Fundamental Completo',
              'Médio Completo',	'Superior Completo')


map(esc_pop_10, n_esc_pop, 5)




# 2019

esc_pop_19 = read_xlsx('evol2_mun.xlsx', sheet = 'esc_pop_19')
esc_pop_19 = data.frame( esc_pop_19[order(esc_pop_19$Município), ] )
esc_pop_19[,2:5] = esc_pop_19[,2:5]*100



# map


map(esc_pop_19, n_esc_pop, 5)




# 6 - cor_raça_SP ----
# rendimento por cor para 2010


cor1 = read_xlsx('evol2_mun.xlsx', sheet = 'cor')
cor1 = data.frame( cor1[order(cor1$muni), ] )



# map


n_cor1 = c('Total',	'Branca',	'Preta',
           'Amarela',	'Parda',
           'Indígena',	'Sem declaração', 'Branca + Amarela',
           'Negros')


map(cor1, n_cor1, 6)





# 7 - Dif salarial por cor_raça_SP ----
# rendimento por cor para 2010


cor_dif = read_xlsx('evol2_mun.xlsx', sheet = 'cor_dif')
cor_dif = data.frame( cor_dif[order(cor_dif$muni), ] )
cor_dif[2] = cor_dif[2]*100



# map

n_cor_dif = c('Proporção do salário dos negros \n em relação aos brancos')

map(cor_dif, n_cor_dif, 7)
















