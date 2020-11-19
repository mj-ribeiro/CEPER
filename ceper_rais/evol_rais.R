

setwd("D:/Git projects/CEPER/ceper_rais")


library(tmap)
library(readxl)


# carregar shape file das regiões de governo

load('regions.rda')
region = polygons_regioes_gov_sp

rm(polygons_regioes_gov_sp)




## dados da rais
# por faixa etária

idade = read_xlsx('evol.xlsx', sheet = 2)
idade = data.frame(idade)

idade[,2:9] = idade[,2:9]*100



new_n =  c('Região', '10 a 14 anos', '15 a 17 anos',
           '18 a 20 anos', '25 a 29 anos', 
           '30 a 39 anos', '40 a 49 anos',
           '50 a 64 anos', 'Mais de 65 anos', 'Total')




map = function(df, name){
  for(i in 2:length(df)){
    
    region$var = df[,i]
    
    g1 = tm_shape(region) +
      tm_polygons('var', title = name[i],  textNA = 'Sem dados') +
      tm_compass(type = "8star", position = c("right", "bottom", size = 0.0)) +
      tm_scale_bar(breaks = c(0, 100, 200, 300), text.size = 0.6) +
      tm_layout(
        legend.text.size = 1,
        frame = T,
        legend.format = list(text.separator = "-", 
                             fun=function(x) paste0(formatC(x, digits=2, format="f"), "%")) )
    nam =  paste("G_", i, sep = "")
    assign(nam, g1)
    tmap_save(g1, filename = paste("G_", i, ".png", sep = "") )
  }
}


map(idade, new_n)






#### Mapas: por sexo


sex = read_xlsx('evol.xlsx', sheet = 3)

sex = data.frame(sex)

sex[,2:6] = sex[,2:6]*100 



new_n2 =  c('Região', '2003', '2007', '2011', '2015', '2019')




map(sex, new_n2)



#### Mapas por escolaridade



esc = read_xlsx('evol.xlsx', sheet = 4)

esc = data.frame(esc)

esc[,2:13] = esc[,2:13]*100



new_n3 = c("Região", "Analfabeto",  "Até 5ª Incompleto",
           "5ª Completo Fundamental", "6ª a 9ª Fundamental",
           "Fundamental Completo" , "Médio Incompleto", "Médio Completo",
           "Superior Incompleto",  "Superior Completo", "Mestrado",                 
           "Doutorado", "Total" )                   



map(esc, new_n3)




##### diferença salarial entre homens e mulheres


# see: https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/

library(ggplot2)

head(sex)


g1 = ggplot(data=sex, aes(x=X2003, y=X2019)) +
  geom_point(size=0) +
#  theme_minimal() +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  geom_text(aes(label=regiao),hjust=0.1, vjust=0, size=4) +
  geom_abline(aes( slope=1, intercept=0, colour = "Linha de 45°"), show.legend =TRUE) +
  scale_color_manual( values=c("Linha de 45°"="black")) +
  labs(color='') +
  theme(legend.position="right",
        legend.key = element_rect(fill = NA, color = 'black'),
        legend.key.size = unit(0.3, "cm"),
        legend.key.width = unit(0.3,"cm"), 
        axis.text.x = element_text( size=15), 
        axis.text.y = element_text(size=15), 
        axis.title.x = element_text(colour = 'black', size=15),
        axis.title.y = element_text(colour = 'black', size=15)) +
  ylab('2019') +
  xlab('2003')





ggsave('g1.png')








