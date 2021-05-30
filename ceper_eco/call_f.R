
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

                  

# substituir os NAs pela média ----


med = function(x){
  return(ifelse(is.na(x), mean(x, na.rm=T), x))
}





f1 = function(x){
  if(is.numeric(x) ){
    
    if( (max(x, na.rm=T)==min(x, na.rm=T) ) ){
      rep(0, length(x))
      
    }else if(sum(is.nan(x))==length(x) ){
      rep(0, length(x))
      
    }else{
      ( x - min(x, na.rm = T) )/ (max(x, na.rm = T) - min(x, na.rm = T))
      
    }
  }
}




f2 = function(x){
  if(is.numeric(x) ){
    
    if( (max(x, na.rm=T)==min(x, na.rm=T) ) ){
      rep(0, length(x))
      
    }else if(sum(is.nan(x))==length(x) ){
      rep(0, length(x))
      
    }else{
      ( max(x, na.rm = T) - x )/ (max(x, na.rm = T) - min(x, na.rm = T))
      
    }
  }
}









