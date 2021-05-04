#############################################################################################
#                          Estudo dados socioêconomicos
#############################################################################################

# Marcos Júnio Ribeiro



setwd("D:/Git projects/CEPER/ceper_eco")


library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(reshape2)
library(writexl)





# geocode ----


sp = read_municipality(code_muni = 'SP')

sp = sp[order(sp$name_muni), ]



# Dados Alesp ----

df = read_excel('muni.xlsx', sheet = 'data_18')


# Merge data ----

df2 = merge(df, sp, by.x='code_muni', by.y='code_muni')
df2 = st_as_sf(df2)


# ver os municípios cujos nomes estão dando conflito
df2[df2$name_muni.x != df2$name_muni.y, c('name_muni.x','name_muni.y')]




# mapas ----




maps_f = function(sh, x, leg, fonte){
  g1 =  tm_shape(sh) +
    tm_polygons(x,  
                title=toupper(leg),  
                textNA = 'Sem dados',
                style="pretty",
                palette="PuOr"
               # breaks = mybreaks,
                #label = mylabel,
                #palette='Reds'
               ) +
    tm_compass(type = "8star",
               position = c("right", "bottom", size = 0.0)) +
    tm_scale_bar(text.size = 0.6) +
    tm_layout(
      legend.text.size = 0.8,
      frame = T,
      legend.format = list(text.separator = "-"))
#  nam =  paste("G_", sep = "")
#  assign(nam, g1)
  tmap_save(g1, filename = paste(fonte,leg,".png", sep = ""), width = 12,
            height = 6, units = 'in')
}




maps_f(df2,'riqueza','Riqueza', 'Alesp_')




mybreaks = seq(0, 100, 10)

mylabel = c('0% a 10%',
            '10% a 20%',
            '20% a 30%',
            '30% a 40%',
            '40% a 50%',
            '50% a 60%',
            '60% a 70%',
            '70% a 80%',
            '80% a 90%',
            '90% a 100%'
)

mycolors = c('beige','burlywood1','orange','darkorange1', 'darkorange3')







