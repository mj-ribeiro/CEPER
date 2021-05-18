
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


setwd("D:/Git projects/CEPER/ceper_eco")



# crime ----


crime = tibble(read.csv('crime.csv', sep=';',
                        header = T,
                        fileEncoding="UTF-8-BOM") )


crime = crime %>%
        filter(ano==2019)


head(crime)



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







