
# Marcos Júnio Ribeiro

# SEE: https://academic.oup.com/heapol/article/21/6/459/612115

library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(reshape2)
library(writexl)
library(viridis)
library(stringr)
library(tidyverse)
library(dplyr)
library(expss)
library(purrr)
library(corrplot)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)



setwd("D:/Git projects/CEPER/ceper_eco")



# crime ----


crime = read_excel('crime.xlsx')
                        



# saneamento ----


snis = read_excel('snis.xlsx')


# CAD ----

cad = read_excel('dados.xlsx', sheet = 'cad')

tail(cad)



cad$data = as.Date(cad$data,"%m/%d/%Y")


cad = cad%>%
  dplyr::filter(data=='2019-12-01')




# geocode ----


sp = read_municipality(code_muni='SP')




# longevidade ----


l1 = read_excel('dead_adult.xlsx')

l2 = read_excel('dead_child.xlsx')



# saúde ----

saude =  read_excel('saude.xlsx')


# eletricidade ----


elet = read_excel('elet.xlsx')




# IDEB ----



ideb_ini = read_excel('ideb.xlsx', sheet = 'inicial')

ideb_fin = read_excel('ideb.xlsx', sheet = 'final')




# merge datasets ----


df = list(elet, cad, crime, l1, l2, saude, snis, ideb_fin, ideb_ini) %>%
          reduce(left_join, by='cod_muni')




drop = c('RA', 'muni_elet', 'data', 'Pop_20', 'Fam_Cad', 
         'Fam_PBF', 'F_PBF_Domi', 'Domic_20', 'Map',
         'MapSeq', 'F_PBF_EP', 'F_CAD_EP', 'muni.y',
         'muni.x', 'muni', 'muni_s', 'muni', 'ES001', 
         'AG001', 'ano', 'muni_m', 'muni_ideb.y', 'muni_ideb.x' )





df = df %>%
    select(-drop)



df = df %>%
  mutate(pbf_cem = Pes_PBF*cem,
         pcad_cem = Pes_Cad*cem)


# substituir os NAs pela média ----


med = function(x){
  return(ifelse(is.na(x), mean(x, na.rm=T), x))
}


nome = df[,c('cod_muni', 'Mun')]



df = df[, - c(4, 5)]



df=sapply(df, med) 


df = data.frame(df)
rownames(df) = nome$Mun




drop2 = c('hab_aut', 'Pes_PBF', 'Pes_Cad', 'pop', 'cem',
          'tx_m_g', 'tx_m_inf', 'tx_m_neo', 'li_pmh', 'pbf_cem',
          'frv_cem')



tt=df %>%
  select(-drop2)




# riqueza     -> hab_veic, c_elet_res, pcad_cem

# crime       -> hom_cem, furto_cem, roubo_cem

# longevidade -> tx_m_15, tx_m_60, tx_m_infantil, tx_m_peri

# saúde       -> lsus_pmh, t_enf_pmh, enf_pmh, m_pmh

# saneamento  -> IN015, IN016, IN055

# educação    -> ideb_ini, ideb_fin



drop4 = c('hom_cem', 'furto_cem', 
          'roubo_cem', 'pcad_cem', 'c_elet_res')

tt3 = tt%>%
  select(-drop4)




M <-cor(tt3)

corrplot(M, type="lower", order="hclust",
         col=brewer.pal(n=5, name="RdYlBu"))



# PCA ----

library('Factoshiny')

pca = PCAshiny(tt)




summary(pca)

pca$scores[1,1]





nome['pca'] = p[,1]


nome=nome[order(nome$pca, decreasing = T),]

tail(nome,20)



tt['Santa Salete',]



# AF ----

fa <- factanal(tt, factors = 2)





saveRDS(tt, 'data.rds')
















df = apply_labels(df,
          
            hab_veic = 'Habitantes por veículo',
            c_elet_res = 'Consumo residencial de energia elétrica',
            pcad_cem = 'Pessoas cadastradas no cad pcmh')





