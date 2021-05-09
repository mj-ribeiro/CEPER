#############################################################################################
#                          Estudo dados socioêconomicos
#############################################################################################

# Marcos Júnio Ribeiro



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



setwd("D:/Git projects/CEPER/ceper_eco")



# Geocode ----


sp = read_municipality(code_muni = 'SP')
sp = sp[order(sp$name_muni), ]




b = c('á', 'é', 'í', 'ó', 'ú')
c = c('a', 'e', 'i', 'o', 'u')



sp$name_muni = str_replace(sp$name_muni, "-", " ")
sp$name_muni = str_replace(sp$name_muni, "'", " ")

for (i in 1:length(b)) {
  sp$name_muni = str_replace(sp$name_muni, b[i], c[i])
}

sp$name_muni = tolower(sp$name_muni)




# firjan ----

fir = readxl::read_excel('firjan.xlsx', sheet = 'd1')
str(fir)



fir$name_muni = str_replace(fir$name_muni, "-", " ")
fir$name_muni = str_replace(fir$name_muni, "'", " ")

for (i in 1:length(b)) {
  fir$name_muni = str_replace(fir$name_muni, b[i], c[i])
}

fir$name_muni = tolower(fir$name_muni)

fir = fir[order(fir$name_muni),]
fir[fir$name_muni!=sp$name_muni, 'name_muni'] = c('embu', 'moji mirim')



# merge dataset ----

df2 = merge(sp, fir, by='name_muni')

df2 = st_as_sf(df2)



# mapas ----




maps_f = function(sh, x, leg, fonte, breaks=NULL, 
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
  tmap_save(g1, filename = paste(fonte,leg,".eps", sep = ""), width = 6,
            height = 4, units = 'in')
}




mybreaks = c(seq(0.5, 0.9, 0.1), Inf)

mylabel = c('0,5 a 0,6',
             '0,6 a 0,7',
             '0,7 a 0,8',
             '0,8 a 0,9',
             'Mais de 0,9')



mycolors = inferno(length(mylabel), alpha = 1,
                    begin = 0.15, end = 1, direction = 1)



maps_f(sh=df2, x='IFDM',
       leg='IFDM', 
       fonte='FIR_',
       breaks = mybreaks,
       colors = mycolors,
       labels = mylabel
       )




