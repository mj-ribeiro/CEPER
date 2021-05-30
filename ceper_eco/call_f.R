
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


d = function(X){
  link = ('http://aplicacoes.mds.gov.br/sagi/servicos/misocial?q=*&fq=anomes_s:ANO*&fq=tipo_s:mes_mu&wt=csv&omitHeader=true&fl=ibge:codigo_ibge,anomes:anomes_s,cadunico_tot_fam:cadunico_tot_fam_i,cadunico_tot_pes:cadunico_tot_pes_i,cadunico_tot_fam_rpc_ate_meio_sm:cadunico_tot_fam_rpc_ate_meio_sm_i,cadunico_tot_pes_rpc_ate_meio_sm:cadunico_tot_pes_rpc_ate_meio_sm_i,cadunico_tot_fam_pob:cadunico_tot_fam_pob_i,cadunico_tot_pes_pob:cadunico_tot_pes_pob_i,cadunico_tot_fam_ext_pob:cadunico_tot_fam_ext_pob_i,cadunico_tot_pes_ext_pob:cadunico_tot_pes_ext_pob_i,cadunico_tot_fam_pob_e_ext_pob:cadunico_tot_fam_pob_e_ext_pob_i,cadunico_tot_pes_pob_e_ext_pob:cadunico_tot_pes_pob_e_ext_pob_i&rows=100000000&sort=anomes_s%20asc,%20codigo_ibge%20asc')
  read.table(gsub('ANO', X, link), sep=',', header = T, dec='.')
}

                  

# substituir os NAs pela média ----


med = function(x){
  return(ifelse(is.na(x), mean(x, na.rm=T), x))
}



impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))






x=readxl::read_excel('snis2.xlsx', sheet='snis')  









z = x %>%
    group_by(muni) %>%
    fill(code_muni, .direction = "down")

head(z, 25)




