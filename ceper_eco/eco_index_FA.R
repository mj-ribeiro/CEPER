
# Marcos Júnio Ribeiro

# SEE: https://academic.oup.com/heapol/article/21/6/459/612115

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

xx = caret::preProcess(df, 'range')
df = predict(xx, df)

drop = c("hom_cem", "tx_m_60","lsus_pmh")

df = df%>%
  select(-drop)


n_var = dim(df)[2]


df = df%>%
  mutate(pcad_cem=1-pcad_cem)




# correlação ----

R = cor(df)



corrplot::corrplot(R, type="lower", order="hclust",
         col=brewer.pal(n=5, name="RdYlBu"))




# KMO ----


Rp = ppcor::pcor(df)[1]$estimate
KMO = sum(R^2)/sum(R^2 + Rp^2)
KMO


# autovalores e autovetores ----

ei = eigen(R)

P = matrix(ei$vectors, nrow = n_var, ncol = n_var)

lambda = ei$values


LAMBDA = as.matrix(diag(lambda, n_var), n_var, n_var)


# cargas fatoriais ----

fat = 1

if(fat==1){
  A = -1*P[,1:fat]%*%sqrt(as.matrix(LAMBDA[1:fat,1:fat]))
}else{
  A = -1*P[,1:fat]%*%sqrt(LAMBDA[1:fat,1:fat])
}


# comunalidades ----


AA = A%*%t(A)


# especificidade ----


PSI = diag(diag(R - AA), n_var)



# resíduos ----

res = R - (AA + PSI)



# rotação ----



if(fat==1){
  rot = A
  F =  solve(t(rot)%*%rot)%*%t(rot)%*%t(as.matrix(df))  
}else{
  vmax = varimax( A, normalize = FALSE ) 
  rot = A%*%vmax$rotmat
  F =  solve(t(rot)%*%rot)%*%t(rot)%*%t(as.matrix(df))  
}


F = t(F)

F = data.frame(F)



teste = data.frame(df, F)


# pesos ----

xx = caret::preProcess(F, 'range')

F1 = predict(xx, F)


IND = rowSums(lambda[1:fat]*F1)/ sum(lambda[1:fat]) 


teste = data.frame(teste, IND)

df2 = data.frame(df, IND)



M <-cor(df2)

corrplot::corrplot(M, type="lower", order="hclust",
                   col=viridis::inferno(6, 1, 0, 1))


corrplot::corrplot.mixed(M, lower='number')




# join data ----


nome = readRDS('nome.rds')

nome['ind'] = IND

nome = nome[order(nome$ind, decreasing = T),]

nome$pos = seq(1, nrow(nome))

row.names(nome)=nome$Mun


nome['Vinhedo',]




fBasics::basicStats( nome$ind)




#geocode ----

sp = read_municipality(code_muni='SP')




# mapas ----



maps_f = function(sh, x, leg, fonte, type, breaks=NULL, 
                  labels=NULL, colors=NULL){
  g1 =  tm_shape(sh) +
    tm_polygons(x,  
                title=leg,  
                textNA = 'Sem dados',
                breaks = breaks,
                label = labels,
                palette=colors
    ) +
    tm_compass(type = "8star",
               position = c("right", "bottom", size = 0.0)) +
    tm_scale_bar(text.size = 0.6) +
    tm_layout(
      legend.text.size = 0.8,
      frame = T,
      legend.format = list(text.separator = "-"))
  tmap_save(g1, filename = paste(fonte,leg,type, sep = ""), width = 6,
            height = 4, units = 'in')
}



# merge dataset ----

df2 = merge(nome, sp, by.x='cod_muni', by.y='code_muni')

df2 = st_as_sf(df2)



mybreaks = seq(0,1,0.2)

mylabs = c('0,0 a 0,2',
           '0,2 a 0,4',
           '0,4 a 0,6',
           '0,6 a 0,8',
           '0,8 a 1,0')


mycolors =viridis::inferno(length(mylabs), alpha = 1,
                    begin = 0.15, end = 1, direction = 1)



maps_f(df2, 'ind',
       leg='Índice CEPER',
       fonte = 'ceper_',
       type = '.jpg',
       breaks=mybreaks,
       colors=mycolors,
       labels=mylabs)



nome['Salto', ]



BIND = rbind(head(nome[,-1], 10), tail(nome[,-1], 10) )

BIND = data.frame(BIND)

# Tabela -----

stargazer::stargazer(BIND, summary = FALSE, out = 'ceper.tex',
                     decimal.mark = ',',
                     digits.extra=0, digits=2,
                     rownames = FALSE
)







hist(nome$ind, col='darkred', probability=T )
abline(v=mean(nome$ind))
curve(dnorm(x, 0.45, 0.13), xlim = c(0,1), add=T, lwd=3, col= 'red')







