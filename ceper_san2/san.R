#############################################################################################
#                          Estudo do  Saneamento
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



df$my_var = ifelse(df$my_var==1, 'Sabesp', 
                  ifelse(df$my_var==2, 'Autarquia empresa púb.',
                         ifelse(df$my_var==3, 'Empresa privada adm púb.',
                                ifelse(df$my_var==4, 'Sociedade de economia mista com adm. púb.',
                                       ifelse(df$my_var==5, 'Autarquia',df$my_va )))))


table(df$my_var)

## rename ----

or_names = colnames(df)

substr(or_names, 1, 5)   # slice string

new_names = trimws( substring(or_names, 1, 5)) # remove white space in string

colnames(df) = new_names


## print old and new names ----


pnames = function(){
  for (i in 1:length(or_names)) {
    cat( or_names[i], '----- Novo nome:', new_names[i], '\n')
  }
}


pnames()


summary(df$IN055)


# geocode ----


sp = read_municipality(code_muni = 'SP')

sp = sp[order(sp$name_muni), ]






# verify if  town names are equal ----

pos = sp$name_muni != df[order(df$muni), 'muni' ]
df[pos, 'muni']
sp[pos, 'name_muni']




# maps ----


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


maps_f = function(x, leg){
    sp$var = df[ ,x ]
    g1 =  tm_shape(sp) +
      tm_polygons('var',  
                  title=toupper(leg),  
                  textNA = 'Sem dados',
                  breaks = mybreaks,
                  label = mylabel,
                  palette='Reds') +
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
}




pnames()

maps_f('IN056', 'iatera')

summary(df$IN015)




## Histogram ----


pnames()

df2 =  df[c('IN015', 'IN016')]


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
  facet_wrap(vars(Índice)) +
  theme(strip.text.x = element_blank()) +
  stat_bin( breaks=seq(0,100,10), binwidth=12, geom='text', color='black', aes(label=..count..),
            vjust=-0.5, hjust=0.5 ) +
  scale_fill_discrete( labels = c("ICE", "ITE"))

g1


ggsave('g1.png', , width = 6, height = 4)


# iata ----



df3 = data.frame( df[ ,c('IN055') ] )


df3$Índice = rep('IATA', nrow(df3))

colnames(df3)[1] = 'IATA'



g2 = ggplot(data = df3, aes(x=IATA)) 


g3 = g2 +  geom_histogram(fill='red',
                          position = 'dodge',
                          colour='black',
                          breaks=seq(0, 100, 10),
                          binwidth=12,) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  ylab('Quantidade de municípios') +
  xlab('IATA') +
  theme_minimal() +
  stat_bin( breaks=seq(0,100,10), binwidth=12, 
            geom='text', 
            color='black', 
            aes(label=..count..),
            vjust=-0.5, hjust=0.5 ) 



g3

ggsave('g3.png', width = 6, height = 4)






# Histogram por prestador ---- 


df4 = data.frame( df[ ,c('IN055', 'my_va') ] )

df4$my_va = as.factor(df4$my_va)

colnames(df4) = c('IATA', 'my_va')


g4 = ggplot(data=df4, aes(x=IATA, fill=my_va, color=my_va ) )

g5 = g4 + geom_histogram(bins=30, alpha=0.5) +
  theme(axis.text.x = element_text( hjust = 1, size=20), 
        axis.text.y = element_text(size=20), 
        axis.title.x = element_text(colour = 'black', size=21),
        axis.title.y = element_text(colour = 'black', size=21),
        legend.text = element_text(size = 18),
        legend.position="bottom", 
        legend.title = element_blank()) +
 # ylim(0, 100) +
  labs(x = 'Índice de atendimento total de água',
       y = 'Número de municípios') 



g5

ggsave('g5.png', width = 12, height = 6)

 









 

 
 
 
 