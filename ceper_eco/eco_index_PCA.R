
library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(tidyverse)
library(dplyr)
library(RColorBrewer)



setwd("D:/Git projects/CEPER/ceper_eco")


# dataset ----

df =  readRDS('data.rds')


colnames(df)


xx = caret::preProcess(df, 'range')
df = predict(xx, df)


summary(df)

colnames(df)



crime = (1/3)*( (1-df$hom_cem) + (1-df$furto_cem) + (1-df$roubo_cem) )

riqueza = (1/2)*( df$c_elet_res + (1-df$pcad_cem) )

saude = (1/4)*(df$lsus_pmh + df$t_enf_pmh + df$enf_pmh + df$m_pmh )

long = (1/4)*( (1-df$tx_m_15)  + (1-df$tx_m_60)+ (1-df$tx_m_infantil) + (1-df$tx_m_peri) )

san = (1/3)*(df$IN015 + df$IN016 + df$IN055 )

educ = (1/2)*(df$ideb_fin + df$ideb_ini)



M = matrix(data=c(crime, educ, long, riqueza, san, saude), nrow=645, ncol = 6)
colnames(M) = c('crime', 'educ', 'long', 'riqueza', 'san', 'saude')


# tirar hab_veic

# correlação ----

R = cor(M)

R
corrplot::corrplot(R, type="lower", order="hclust",
                   col=brewer.pal(n=5, name="RdYlBu"))

corrplot::corrplot.mixed(R, lower='number')



# autovalores

ei = eigen(R)


# vou usar 5 autovalores (critério de Jollife)

vet = ei$vectors[,1:5]
vet = t(vet)


val = ei$values[1:5]



sq = sqrt(val)/sum(sqrt(val))



w = colSums(vet*sq)

w = w/(sum(w)) 




head(M)




a=matrix(data=c(2,3,4,5,6,1,2,3,4,5,6,1), 2,6)


w = c(0, -1, 2, 3, 1, -2)
a*w
a


