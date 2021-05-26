
# Marcos Júnio Ribeiro

# SEE: https://academic.oup.com/heapol/article/21/6/459/612115

library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(stringr)
library(tidyverse)
library(dplyr)
library(RColorBrewer)



setwd("D:/Git projects/CEPER/ceper_eco")



# dataset ----

nome = readRDS('nome.rds')
df =  readRDS('data.rds')

xx = caret::preProcess(df, 'range')
df = predict(xx, df)


M <-cor(df)

corrplot::corrplot(M, type="lower", order="hclust",
         col=brewer.pal(n=5, name="RdYlBu"))




# correlação ----

R = cor(df)

# KMO ----


Rp = ppcor::pcor(df)[1]$estimate
KMO = sum(R^2)/sum(R^2 + Rp^2)
KMO


# autovalores e autovetores ----

ei = eigen(R)

P = ei$vectors

lambda = ei$values

LAMBDA = diag(lambda, 19)



# cargas fatoriais ----

fat = 4

A = -1*P[,1:fat]%*%sqrt(LAMBDA[1:fat,1:fat])


# comunalidades ----


AA = A%*%t(A)


# especificidade ----


PSI = diag(diag(R - AA), 19)



# resíduos ----

res = R - (AA + PSI)



# rotação ----


vmax = varimax( A, normalize = FALSE ) 


rot = A%*%vmax$rotmat  # rot é tbm o loadings do vmax
# rot é a matriz A rotacionada pelo varimax


F =  solve(t(rot)%*%rot)%*%t(rot)%*%t(as.matrix(df))  

F = t(F)

F = data.frame(F)



teste = data.frame(df, F)


# pesos ----

xx = caret::preProcess(F, 'range')

F1 = predict(xx, F)


IND = rowSums(lambda[1:fat]*F1)/ sum(lambda[1:fat]) # o ind é uma soma ponderada
# dos fatores



teste = data.frame(teste, IND)



df2 = data.frame(df, IND)



M <-cor(df2)

corrplot::corrplot(M, type="lower", order="hclust",
                   col=brewer.pal(n=8, name="RdYlBu"))









