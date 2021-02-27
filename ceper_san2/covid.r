
############### Alien Economista


setwd("D:/Git projects/CEPER/ceper_san2")


library(sf)
library(geobr)
library(readxl)
library(tmap)



# covid ---- 


covid = read_xlsx(file.choose())
covid = data.frame(covid)

covid = covid[order(covid$UF), ]
str(covid)

head(covid)


# geobr ----


br = read_state(code_state = 'all')
head(br)


br = br[order(br$abbrev_state), ]


br$casos =  covid$casos



# breaks, colors, labels ----


mybreaks = c(0, 1e5, 2e5, 3e5, 4e5, Inf)
mycolors = c('beige','burlywood1','orange','darkorange1', 'darkorange3')
mylabels = c('0 a 100000',
             '100000 a 200000',
             '200000 a 300000',
             '300000 a 400000',
             'Mais de 400000'
             )


# map ----


tm_shape(br) +
  tm_polygons('casos',  
              title='Covid 19',  
              textNA = 'Sem dados',
              label = mylabels,
              breaks = mybreaks,
              palette= mycolors)+
  tm_text(text = 'abbrev_state', size = 0.8)+
  tm_compass(type = "8star",
             position = c("right", "bottom", size = 0.0)) +
  tm_scale_bar(text.size = 0.9) +
  tm_layout(
    main.title = "Casos de Covid no Brasil", 
    main.title.position = "center",
    legend.text.size = 1,
    legend.width = 0.8,
    frame = T) 





#tmap_mode("view")






















