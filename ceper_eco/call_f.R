
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
library(fBasics)


# dataset -----




iprs = read_excel('muni.xlsx', sheet = 'data_18')
iprs$iprs = rowSums(iprs[,4:6])*1/300


firjan = read_rds('FIRJAN.rds')

i17 = read_rds('s_17.rds')
i13 = read_rds('s_13.rds')
i15 = read_rds('s_15.rds')


# geocode ----


sp = read_municipality(code_muni = 'SP')
sp = sp[order(sp$name_muni), ]


# merges ----

df2 = merge(i17, firjan, by='code_muni')

df2 = merge(df2, iprs, by='code_muni')




# Dados do cad

#dicionário

#https://dados.gov.br/dataset/cadastro-unico-familias-pessoas-cadastradas-por-faixas-de-renda

d = function(X){
  link = ('http://aplicacoes.mds.gov.br/sagi/servicos/misocial?q=*&fq=anomes_s:ANO*&fq=tipo_s:mes_mu&wt=csv&omitHeader=true&fl=ibge:codigo_ibge,anomes:anomes_s,cadunico_tot_fam:cadunico_tot_fam_i,cadunico_tot_pes:cadunico_tot_pes_i,cadunico_tot_fam_rpc_ate_meio_sm:cadunico_tot_fam_rpc_ate_meio_sm_i,cadunico_tot_pes_rpc_ate_meio_sm:cadunico_tot_pes_rpc_ate_meio_sm_i,cadunico_tot_fam_pob:cadunico_tot_fam_pob_i,cadunico_tot_pes_pob:cadunico_tot_pes_pob_i,cadunico_tot_fam_ext_pob:cadunico_tot_fam_ext_pob_i,cadunico_tot_pes_ext_pob:cadunico_tot_pes_ext_pob_i,cadunico_tot_fam_pob_e_ext_pob:cadunico_tot_fam_pob_e_ext_pob_i,cadunico_tot_pes_pob_e_ext_pob:cadunico_tot_pes_pob_e_ext_pob_i&rows=100000000&sort=anomes_s%20asc,%20codigo_ibge%20asc')
  data = read.table(gsub('ANO', X, link), sep=',', header = T, dec='.')
  data=data[substr(data$ibge, 1,2) == '35' & data$anomes==paste(X,12,sep=''), ]
}


# padronização ----


normalit <- function(m){(m - min(m))/(max(m)-min(m))}

                  
# stats ----

#stats = function(x){fBasics::basicStats(x) }


stats = function (x, ci = 0.95) 
{
  y = as.matrix(x)
  if (is.null(colnames(y))) {
    Dim = dim(y)[2]
    if (Dim == 1) {
      colnames(y) = paste(substitute(x), collapse = ".")
    }
    else if (Dim > 1) {
      colnames(y) = paste(paste(substitute(x), collapse = ""), 
                          1:Dim, sep = "")
    }
  }
  cl.vals = function(x, ci) {
    x = x[!is.na(x)]
    n = length(x)
    if (n <= 1) 
      return(c(NA, NA))
    se.mean = sqrt(var(x)/n)
    t.val = qt((1 - ci)/2, n - 1)
    mn = mean(x)
    lcl = mn + se.mean * t.val
    ucl = mn - se.mean * t.val
    c(lcl, ucl)
  }
  nColumns = dim(y)[2]
  ans = NULL
  for (i in 1:nColumns) {
    X = y[, i]
    X.length = length(X)
    X = X[!is.na(X)]
    X.na = X.length - length(X)
    z = c(X.length,  min(X), max(X), diff(range(X)),  
          as.numeric(quantile(X, prob = 0.25, na.rm = TRUE)),
          as.numeric(quantile(X, prob = 0.75, na.rm = TRUE)), mean(X),
          median(X), var(X), sqrt(var(X)),(sqrt(var(X))/mean(X)) , skewness(X), 
          kurtosis(X))
    znames = c("Observações",  "Mínimo","Máximo", "Amplitude",  "1° Quartil", 
               "3° Quartil", "Média", "Mediana", "Variância",
               "Desvio Padrão", 'Coeficiente de Variação', "Assimetria", "Curtose")
    result = matrix(z, ncol = 1)
    row.names(result) = znames
    ans = cbind(ans, result)
  }
  colnames(ans) = colnames(y)
  data.frame(round(ans, digits = 6))
}





# substituir os NAs pela média ----


med = function(x){
  return(ifelse(is.na(x), mean(x, na.rm=T), x))
}



# make sub index ----


make_sub_index = function(year, df, y = 3,  x){
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
  return(g1)
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




# ceper shiny
# https://github.com/andrew-couch/Tidy-Tuesday/blob/master/Season%201/Apps/TidyTuesdayDashboardLevels/advanced_app.R

# https://www.youtube.com/watch?v=PHdIivFAq7Q&ab_channel=AndrewCouchAndrewCouc# h
















