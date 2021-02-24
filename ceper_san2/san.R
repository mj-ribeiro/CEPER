#############################################################################################
#                                Estudo do  Saneamento
#############################################################################################

setwd("D:/Git projects/CEPER/ceper_san2")


library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(reshape2)
library(writexl)



# dados ----


df = read_xlsx('dados.xlsx')

df = as.data.frame(df)


str(df)

## rename ----

or_names = colnames(df)

substr(or_names, 1, 5)   # slice string

new_names = trimws( substring(or_names, 1, 5)) # remove white space in string



## print old and new names ----


pnames = function(){
  for (i in 1:length(or_names)) {
    cat( or_names[i], '----- Novo nome:', new_names[i], '\n')
  }
}


pnames()

colnames(df) = new_names




# geocode ----

sp = read_municipality(code_muni = 'SP')

sp = sp[order(sp$name_muni), ]



# verify if  town names are equal ----

pos = sp$name_muni != df[order(df$muni), 'muni' ]
df[pos, 'muni']
sp[pos, 'name_muni']



df$ano_==2001

# maps ----


maps_f = function(x, leg){
  #for (i in 0:18) {
  #  filt = df$ano == (2000+i)
    sp$var = df[ ,x ]
    
    g1 =  tm_shape(sp) +
      tm_polygons('var',  
                  title=toupper(leg),  
                  textNA = 'Sem dados',
                  breaks = c(0, 25, 50, 75, 100)) +
      tm_compass(type = "8star",
                 position = c("right", "bottom", size = 0.0)) +
      tm_scale_bar(text.size = 0.6) +
      tm_layout(
        legend.text.size = 0.8,
        frame = T,
        legend.format = list(text.separator = "-"))
    nam =  paste("G_", sep = "")
    assign(nam, g1)
    tmap_save(g1, filename = paste("G_",leg,  ".png", sep = "") )
#}
}




maps_f('IN015', 'ice')

summary(df$IN015)




























