
############### Alien Economista


setwd("D:/Git projects/CEPER/pres")


# libraries ----

library(sf)
library(geobr)
library(readxl)
library(tmap)


# install.packages("tmap")


# dados covid ---- 

df = readRDS('df.rds')


df = df[order(df$UF), ]



# geobr ----

br = read_state(code_state = 'all')
head(br)


br = br[order(br$abbrev_state), ]


br$covid = df$covid



# mapa covid ----

mybreaks = c(0, 1e5, 2e5, 3e5, 4e5, Inf)
mycolors = c('beige', 'burlywood1', 'orange', 'darkorange1', 'darkorange3')
mylabes = c('0 a 100.000',
            '100.000 a 200.000',
            '200.000 a 300.000',
            '300.000 a 400.000',
            'Mais de 400.000')



g1 = tm_shape(br) +
  tm_polygons('covid',
              title = 'Casos  de Covid',
              breaks = mybreaks,
              label = mylabes,
              palette = mycolors) +
  tm_text(text = 'abbrev_state', size =0.8) +
  tm_compass( type = '8star', 
             position = c('right', 'bottom', size=0) ) +
  tm_scale_bar(text.size = 0.9) +
  tm_layout(main.title = 'Casos de Covid no Brasil',
            main.title.position = 'center',
            legend.text.size = 1,
            legend.width = 0.8,
            frame = T) 

tmap_save(g1, filename = 'covid.png')

            

# mapa renda ----
            
            
            
br$renda = df$renda


mybreaks1 = c(1e3, 1.5e3, 2e3, 2.5e3, 3e3, Inf)
mycolors1 = c('beige', 'burlywood1', 'orange', 'darkorange1', 'darkorange3')
mylabes1 = c('1.000 a 1.500',
            '1.500 a 2.000',
            '2.000 a 2.500',
            '2.500 a 3.000',
            'Mais de 3.000')

            

g2 = tm_shape(br) +
  tm_polygons('renda',
              title = 'Renda Média no Brasil',
              breaks = mybreaks1,
              label = mylabes1,
              palette = mycolors) +
  tm_text(text = 'abbrev_state', size =0.8) +
  tm_compass( type = '8star', 
              position = c('right', 'bottom', size=0) ) +
  tm_scale_bar(text.size = 0.9) +
  tm_layout(main.title = 'Renda média mensal no Brasil',
            main.title.position = 'center',
            legend.text.size = 1,
            legend.width = 0.8,
            frame = T)
tmap_save(g2, filename = 'renda.png')

  

            

# mapa tx de desocupação ----

br$desc = df$tx_des



mybreaks2 = c(seq(0.05, 0.2, 0.05), Inf)
mycolors2 = c('beige', 'burlywood1', 'orange', 'darkorange3')
mylabes2 = c('5% a 10%',
             '10% a 15%',
             '15% a 20%',
             'Mais de 20%')


g3 = tm_shape(br) +
  tm_polygons('desc',
              title = 'Taxa de desocupação',
              breaks = mybreaks2,
              label = mylabes2,
              palette = mycolors2) +
  tm_text(text = 'abbrev_state', size =0.8) +
  tm_compass( type = '8star', 
              position = c('right', 'bottom', size=0) ) +
  tm_scale_bar(text.size = 0.9) +
  tm_layout(main.title = 'Taxa de desocupação no Brasil',
            main.title.position = 'center',
            legend.text.size = 1,
            legend.width = 0.8,
            frame = T)
                   
tmap_save(g3, filename = 'desocup.png')


            
            
    
# mapa mortes por covid


br$dead = df$mortes


mybreaks3 = c(seq(0, 3e4, 1e4), Inf)
mycolors3 = c('beige', 'burlywood1', 'orange', 'darkorange3')
mylabes3 = c('0 a 10000',
             '10.000 a 20.000',
             '20.000 a 30.000',
             'Mais de 30.000')
            
g4 = tm_shape(br) +
  tm_polygons('dead',
              title = 'Mortes por Covid',
              breaks = mybreaks3,
              label = mylabes3,
              palette = mycolors3) +
  tm_text(text = 'abbrev_state', size =0.8) +
  tm_compass( type = '8star', 
              position = c('right', 'bottom', size=0) ) +
  tm_scale_bar(text.size = 0.9) +
  tm_layout(main.title = 'Mortes por Covid no Brasil',
            main.title.position = 'center',
            legend.text.size = 1,
            legend.width = 0.8,
            frame = T)

            
tmap_save(g4, filename = 'dead.png')

        
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            