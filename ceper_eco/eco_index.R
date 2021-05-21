
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


setwd("D:/Git projects/CEPER/ceper_eco")



# crime ----


crime = tibble(read.csv('crime.csv', sep=';',
                        header = T,
                        fileEncoding="UTF-8-BOM") )



# saneamento ----


snis = tibble(read.csv('snis.csv', sep=';', header = T) )



# CAD ----

cad = read_excel('dados.xlsx', sheet = 'cad')

tail(cad)



cad$data = as.Date(cad$data,"%m/%d/%Y")


cad = cad%>%
  dplyr::filter(data=='2019-12-01')




# geocode ----


sp = read_municipality(code_muni='SP')




# longevidade ----


l1 = tibble(read.csv('dead_adult.csv', sep=';', header = T) )
  
l2 = tibble(read.csv('dead_child.csv', sep=';', header = T) )



# saúde ----

saude =  tibble(read.csv('saude.csv', sep=';', header = T) )



# eletricidade


elet = tibble(read.csv('elet.csv', sep=';', header = T) )




# merge datasets


df = list(elet, cad, crime, l1, l2, saude, snis) %>%
          reduce(left_join, by='cod_muni')




drop = c('RA', 'muni_elet', 'data', 'Pop_20', 'Fam_Cad', 'Fam_PBF',
         'F_PBF_Domi', 'Domic_20', 'Map',
         'MapSeq', 'F_PBF_EP', 'F_CAD_EP', 'muni.y',
         'muni.x', 'muni.x.x', 'muni.y.y', 'ano.x', 'ano.y',
         'ano.x.x', 'muni','ano.y.y', 'ES001', 'AG001' )

df = df %>%
    select(-drop)



df = df %>%
  mutate(pbf_cem = Pes_PBF*cem,
         pcad_cem = Pes_Cad*cem)












df = apply_labels(df,
             cod_muni = 'Código IBGE',
             Mun = 'Município',
             Pes_PBF = 'pessoas no bolsa família')





