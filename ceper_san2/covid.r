############### Alien Economista



library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(reshape2)
library(writexl)



# covid ---- 



covid = read_xlsx('covid.xlsx')
covid = data.frame(covid)

covid = covid[order(covid$UF), ]
str(covid)

head(covid)



bra = read_state(code_state = 'all')
bra = bra[order(bra$abbrev_state), ]


bra$var =  covid$casos


mybreaks = seq(0, 2500000, 500000)
mycolors = c('beige','burlywood1','orange','darkorange1', 'darkorange3')
mylabels = c('0 a 500000',
             '500000 a 1000000',
             '1000000 a 1500000',
             '1500000 a 2000000',
             '2000000 a 2500000')



tm_shape(bra) +
  tm_polygons('var',  
              title=toupper('covid'),  
              textNA = 'Sem dados',
              label = mylabels,
              breaks = mybreaks,
              palette= mycolors)+
  tm_compass(type = "8star",
             position = c("right", "bottom", size = 0.0)) +
  tm_scale_bar(text.size = 0.6) +
  tm_layout(
    main.title = "Casos de Covid no Brasil", 
    main.title.position = "center",
    legend.text.size = 1,
    legend.width = 0.8,
    frame = T,
    legend.format = list(text.separator = "-")) 






















