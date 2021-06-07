
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
library(viridis)
library(ggpubr)



# Dados do cad

#dicionário

#https://dados.gov.br/dataset/cadastro-unico-familias-pessoas-cadastradas-por-faixas-de-renda

d = function(X){
  link = ('http://aplicacoes.mds.gov.br/sagi/servicos/misocial?q=*&fq=anomes_s:ANO*&fq=tipo_s:mes_mu&wt=csv&omitHeader=true&fl=ibge:codigo_ibge,anomes:anomes_s,cadunico_tot_fam:cadunico_tot_fam_i,cadunico_tot_pes:cadunico_tot_pes_i,cadunico_tot_fam_rpc_ate_meio_sm:cadunico_tot_fam_rpc_ate_meio_sm_i,cadunico_tot_pes_rpc_ate_meio_sm:cadunico_tot_pes_rpc_ate_meio_sm_i,cadunico_tot_fam_pob:cadunico_tot_fam_pob_i,cadunico_tot_pes_pob:cadunico_tot_pes_pob_i,cadunico_tot_fam_ext_pob:cadunico_tot_fam_ext_pob_i,cadunico_tot_pes_ext_pob:cadunico_tot_pes_ext_pob_i,cadunico_tot_fam_pob_e_ext_pob:cadunico_tot_fam_pob_e_ext_pob_i,cadunico_tot_pes_pob_e_ext_pob:cadunico_tot_pes_pob_e_ext_pob_i&rows=100000000&sort=anomes_s%20asc,%20codigo_ibge%20asc')
  data = read.table(gsub('ANO', X, link), sep=',', header = T, dec='.')
  data=data[substr(data$ibge, 1,2) == '35' & data$anomes==paste(X,12,sep=''), ]
}

                  
# stats ----

stats = function(x){fBasics::basicStats(x) }



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
  cat('Autovalores:', ei$values, '\n')
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



# index of sub index


make_index = function(){
  R = cor(s[,2:6])
  id = colnames(R)
  
  corrplot::corrplot(R, type="lower", method = c('number'),
                     order="hclust",
                     col=viridis::inferno(5))
  
  ei = eigen(R)
  vet = ei$vectors[,1:3]
  vet = t(vet)
  
  val = ei$values[1:3]
  sq = sqrt(val)/sum(sqrt(val))
  w = colSums(vet*sq)
  w = matrix(w^1/(sum(w^1)), 1 )
  colnames(w) = id
  w
}




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






# view mapa ----




view_map = function(sh, x, leg, fonte, breaks=NULL, 
                  labels=NULL, colors=NULL){
  tmap_mode('view')
  g1 =  tm_shape(sh) +
    tm_polygons(x,  
                title=leg,  
                textNA = 'Sem dados',
                breaks = breaks,
                label = labels,
                palette=colors,
                id = 'muni'
    ) +
    tm_compass(type = "8star",
               position = c("right", "bottom", size = 0.0)) +
    tm_scale_bar(text.size = 0.6) +
    tm_layout(
      legend.text.size = 0.8,
      frame = T,
      legend.format = list(text.separator = "-"))
  return(g1)
}




# classificar CEPER -----

  
d = function(x){
  ifelse(x>=0 & x<=0.33, 'Baixo',
         ifelse(x>0.33 & x<=0.5, 'Regular',
         ifelse(x>0.5 & x<=0.66, 'Moderado', 
          ifelse(x>0.66 & x<=1, 'Alto', 'Não se aplica'))
         )  )
}


seq(1,6,1)/6

table(d(firjan$IFDM))




