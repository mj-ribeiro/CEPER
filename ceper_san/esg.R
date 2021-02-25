#############################################################################################
#                                Estudo do  Saneamento
#############################################################################################

setwd("D:/Git projects/CEPER/ceper_san")


library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(ggspatial)
library(magrittr)
library(raster)
library(tmap)
library(reshape2)
library("writexl")



## dados

df = read_excel('esg_agua.xlsx')

df = as.data.frame(df)




#### rename

or_names = colnames(df)


new_names = c('cod', 'mun', 'est', 'ano', 'prest', 'serv', 'pop_urb_a', 'pop_urb_e', 
  'pop_urb_aI', 'pop_urb_eI', 'pop_tot', 'pop_urb', 'pop_tot_a', 'pop_urb_a',
  'pop_tot_e', 'pop_urb_e', 'ice', 'ite', 'iaua', 'iauera', 'iauere', 'iata', 'iatera')




## print os velhos e novos nomes das colunas

pnames = function(){
  for (i in 1:length(or_names)) {
    cat( or_names[i], '----- Novo nome:', new_names[i], '\n')
  }
}


pnames()



# renomeando

for (i in 1:length(new_names)) {
  colnames(df)[i] = new_names[i]
}



df = df[order(df$mun), ]
df[df$mun=='Jundiaí'& df$ano==2007,'ite'] = 100



## change variables

#df$pop_tot_a  = df$pop_tot_a / df$pop_tot

#df$pop_tot_e  = df$pop_tot_e / df$pop_tot



# geocode

sp = read_municipality(code_muni = 'SP')

sp = sp[order(sp$name_muni), ]



## summary


explore = function(df, x){
  for(i in 0:18){
    cat('Ano:', 2000+i, '\n')
    print(summary(df[df$ano==2000+i, x ]))
  }
}



explore2 = function(df, x){
  a = matrix(nrow=19, ncol=8)
  for(i in 0:18){
    #cat('Ano:', 2000+i, '\n')
    a[i+1,1:7] = summary(df[df$ano==2000+i, x ])
    a[i+1,8] = 2000+i
  }
  a = data.frame(a)
  colnames(a) = c('Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max.', 'NAs', 'ano')
  return(a)
}

l = c('ite', 'iaua', 'iatera', 'iata')



for(x in l){
  k = data.frame( explore2(df, x) )
  write_xlsx(k,  paste("I_",x, ".xlsx", sep = ""))
}





### maps




# buiding box

box = st_bbox(sp$geom)

xrange <- box$xmax - box$xmin # range of x values
yrange <- box$ymax - box$ymin # range of y value

box[1] <- box[1] - (0.1 * xrange) # xmin - left
box[3] <- box[3] + (0.1 * xrange) # xmax - right
box[2] <- box[2] - (0.1 * yrange) # ymin - bottom
box[4] <- box[4] + (0.1 * yrange) # ymax - top

box =  box  %>% st_as_sfc()





maps_f = function(x){
  
  for (i in 0:18) {
    
    filt = df$ano == (2000+i)
    sp$var = df[filt,x ]
    
    g1 =  tm_shape(sp) +
      tm_polygons('var',  title=toupper(x),  textNA = 'Sem dados') +
      tm_compass(type = "8star", position = c("right", "bottom", size = 0.0)) +
      tm_scale_bar(breaks = c(0, 100, 200, 300), text.size = 0.6) +
      tm_layout(
        legend.text.size = 0.8,
        frame = T,
        legend.format = list(text.separator = "-"))
    nam =  paste("G_", i, sep = "")
    assign(nam, g1)
    tmap_save(g1, filename = paste("G_",x, i, ".png", sep = "") )
  }
  
}





pnames()

maps_f('iata')





## Histogram




# ite e ice


df2 =  df[df$ano==2018, c('ice', 'ite')]


data = melt(data = df2)

colnames(data)[1] = 'Índice'





g0 = ggplot(data = data, aes(x=value, fill=Índice)) 

g1 = g0 + geom_histogram(position = 'dodge',
                 colour='black',
                 breaks=seq(0, 100, 10),
                 binwidth=12) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  ylab('Quantidade de municípios') +
  xlab('Índices') +
  theme_minimal() +
  facet_wrap(vars(?ndice)) +
  theme(strip.text.x = element_blank()) +
  stat_bin( breaks=seq(0,100,10), binwidth=12, geom='text', color='black', aes(label=..count..),
            vjust=-0.5, hjust=0.5 ) +
  scale_fill_discrete( labels = c("ICE", "ITE"))




ggsave('g1.png')






# iata



df3 = data.frame( df[df$ano==2018, c('iata')] )


df3$?ndice = rep('IATA', nrow(df3))

colnames(df3)[1] = 'IATA'



g2 = ggplot(data = df3, aes(x=IATA)) 


g3 = g2 +  geom_histogram(fill='red',
                          position = 'dodge',
                          colour='black',
                          breaks=seq(0, 100, 10),
                          binwidth=12,) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  ylab('Quantidade de municípios') +
  xlab('Índices') +
  theme_minimal() +
  stat_bin( breaks=seq(0,100,10), binwidth=12, 
            geom='text', 
            color='black', 
            aes(label=..count..),
            vjust=-0.5, hjust=0.5 ) 

  
  
g3


ggsave('g3.png')











