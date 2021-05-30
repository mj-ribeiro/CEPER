
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

source('call_f.R')



data = d('2018')

data=data[substr(data$ibge, 1,2) == '35' & data$anomes==201812, ]

head(data)


setwd("D:/Git projects/CEPER/ceper_eco")


# dataset ----

df =  readRDS('data.rds')


colnames(df)


xx = caret::preProcess(df, 'range')
df = predict(xx, df)

df$tx_m_15 = (1-df$tx_m_15)  
df$tx_m_60 = (1-df$tx_m_60)
df$tx_m_infantil = (1-df$tx_m_infantil) 
df$tx_m_peri = (1-df$tx_m_peri) 

df$roubo_cem = (1-df$roubo_cem) 
df$furto_cem = 1-df$furto_cem
df$hom_cem = 1-df$hom_cem
df$pcad_cem = (1-df$pcad_cem)


summary(df)

cor(df[,c(6:9,14:16)])




crime = (1/3)*( df$hom_cem + df$furto_cem + df$roubo_cem )

riqueza = (1/2)*( df$c_elet_res + df$pcad_cem )

saude = (1/4)*(df$lsus_pmh + df$t_enf_pmh + df$enf_pmh + df$m_pmh )

long = (1/4)*( df$tx_m_15  + df$tx_m_60 + df$tx_m_infantil + df$tx_m_peri )

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


w
R

colSums(vet*sq)/sum(
  colSums(vet*sq))
