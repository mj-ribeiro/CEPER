
setwd("D:/Git projects/CEPER/ceper_eco")



library(tidyverse)
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




# Dados do cad

#dicionário

#https://dados.gov.br/dataset/cadastro-unico-familias-pessoas-cadastradas-por-faixas-de-renda

d = function(X){
  link = ('http://aplicacoes.mds.gov.br/sagi/servicos/misocial?q=*&fq=anomes_s:ANO*&fq=tipo_s:mes_mu&wt=csv&omitHeader=true&fl=ibge:codigo_ibge,anomes:anomes_s,cadunico_tot_fam:cadunico_tot_fam_i,cadunico_tot_pes:cadunico_tot_pes_i,cadunico_tot_fam_rpc_ate_meio_sm:cadunico_tot_fam_rpc_ate_meio_sm_i,cadunico_tot_pes_rpc_ate_meio_sm:cadunico_tot_pes_rpc_ate_meio_sm_i,cadunico_tot_fam_pob:cadunico_tot_fam_pob_i,cadunico_tot_pes_pob:cadunico_tot_pes_pob_i,cadunico_tot_fam_ext_pob:cadunico_tot_fam_ext_pob_i,cadunico_tot_pes_ext_pob:cadunico_tot_pes_ext_pob_i,cadunico_tot_fam_pob_e_ext_pob:cadunico_tot_fam_pob_e_ext_pob_i,cadunico_tot_pes_pob_e_ext_pob:cadunico_tot_pes_pob_e_ext_pob_i&rows=100000000&sort=anomes_s%20asc,%20codigo_ibge%20asc')
  data = read.table(gsub('ANO', X, link), sep=',', header = T, dec='.')
  data=data[substr(data$ibge, 1,2) == '35' & data$anomes==paste(X,12,sep=''), ]
}

                  
# stats ----

function(x){fBasics::basicStats(x) }



# substituir os NAs pela média ----


med = function(x){
  return(ifelse(is.na(x), mean(x, na.rm=T), x))
}



# make sub index ----


make_sub_index = function(year, df,y = 3,  x){
  R = cor(df[df[,'ano']==year, y:x])
  id = colnames(R)
  
  corrplot::corrplot(R, type="lower", method = c('number'),
                     order="hclust",
                     col=viridis::inferno(5))
  
  ei = eigen(R)
  nval = sum(ei$values>1)
  vet = ei$vectors[,1:nval]
  vet = t(vet)
  
  val = ei$values[1:nval]
  sq = sqrt(val)/sum(sqrt(val))
  w = colSums(vet*sq)
  w = matrix(w^1/(sum(w^1)), 1 )
  colnames(w) = id
  w
}



# calcular indice ----

make_var = function(year, df, y, x){
  w = make_sub_index(year, df, y, x)
  d = df[df$ano==year, ]
  d$ind = as.matrix(d[ ,y:x], 645, 1)%*%t(w) # 645 x 6     1 x 6
  print(w)
  return(d$ind)
}




# scatter ----

make_scatter = function(year, x, y, df ){
  x=enquo(x)
  y=enquo(y)
  df%>%
    filter(ano==year)%>%
    ggplot(aes(!!x, !!y)) +
    geom_point(color='darkred') +
    geom_smooth(method='lm')
}



