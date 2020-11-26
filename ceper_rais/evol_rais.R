

setwd("D:/Git projects/CEPER/ceper_rais")

library(ggplot2)
library(tidyverse)
library(tmap)
library(readxl)
library("cagedExplorer")


# carregar shape file das regiÃµes de governo ---------

load('regions.rda')
region = polygons_regioes_gov_sp

rm(polygons_regioes_gov_sp)



# funcao ----


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
                             fun=function(x) paste0(formatC(x, digits=2, format="f",
                                                            decimal.mark = ','), "%")) )
    nam =  paste("G_", i, sep = "")
    assign(nam, g1)
    tmap_save(g1, filename = paste("G_", i, ".png", sep = "") )
  }
}


map(idade, new_n)


# Mapas: faixa etaria  ----------



idade = read_xlsx('evol.xlsx', sheet = 2)
idade = data.frame(idade)

idade[,2:9] = idade[,2:9]*100



new_n =  c('Regiao', '10 a 14 anos', '15 a 17 anos',
           '18 a 24 anos', '25 a 29 anos', 
           '30 a 39 anos', '40 a 49 anos',
           '50 a 64 anos', 'Mais de 65 anos', 'Total')




##  Mapas:  sexo


sex = read_xlsx('evol.xlsx', sheet = 3)

sex = data.frame(sex)

sex[,2:6] = sex[,2:6]*100 



new_n2 =  c('Região', '2003', '2007', '2011', '2015', '2019')




map(sex, new_n2)



## Mapas: por escolaridade ----



esc = read_xlsx('evol.xlsx', sheet = 4)

esc = data.frame(esc)

esc[,2:13] = esc[,2:13]*100


colnames(esc)

new_n3 = c("Região", "Analfabeto",  "Até 5ª Incompleto",
           "5ª Completo Fundamental", "6ª a 9ª Fundamental",
           "Fundamental Completo" , "Médio Incompleto", "Médio Completo",
           "Superior Incompleto",  "Superior Completo", "Mestrado",                 
           "Doutorado", "Total" )                   



map(esc, new_n3)





# Diferenca salarial entre homens e mulheres -------


# see: https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/


head(sex)


g1 = ggplot(data=sex, aes(x=X2003, y=X2019)) +
  geom_point(size=0) +
#  theme_minimal() +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  geom_text(aes(label=regiao),hjust=0.1, vjust=0, size=4) +
  geom_abline(aes( slope=1, intercept=0, colour = "Linha de 45Â°"), show.legend =TRUE) +
  scale_color_manual( values=c("Linha de 45Â°"="black")) +
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

g1


# DiferenÃ§a salarial entre homens e mulheres (dotplot) 


sex2 = sex[,c(1,2,6)]


colnames(sex2) = c('regiao', '2003', '2019')

sex2$evol = sex2[,'2019'] - sex2[,'2003']

sex2$evol1 = abs(sex2[,'2019'] - sex2[,'2003'])


sex_panel = reshape2::melt(sex2, id.vars=c("regiao", 'evol', 'evol1'), 
                           variable.name="ano", 
                           value.name="index")



sex_panel = tibble(sex_panel)

sex_panel =  sex_panel %>%
   mutate(regiao2 = fct_reorder(regiao,
                                evol1,
                                last) )





breaks <- seq(-45,15,5)
palette <- c(reds_full[2:4], blues_full[3:8])



sex2 = tibble(sex2)

sex2 =
  sex2 %>%
  mutate(group = .bincode(evol1, breaks = breaks)) %>%
  mutate(regiao = fct_reorder(regiao,
                               evol1,
                               last) )





g4 = ggplot(sex2) +
  geom_point(aes(x = evol, y = regiao, fill=factor(group) ),
             size = 5, shape = 21, col = 'gray') +
  scale_x_continuous(labels = function(x) formatC(x, digits = -2, big.mark = '.', decimal.mark = ',')) +
   scale_fill_manual(values = palette) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  custom_theme() +
  theme(legend.position = 'none') +
  labs(x = 'DiferenÃ§a % entre a remuneraÃ§Ã£o de homens e mulheres entre 2003 e 2019',
       y = 'RegiÃ£o de Governo',
       title = '',
       subtitle = '',
       caption = '')
  
g4

ggsave(plot = g4,
       filename = 'g3.png',
       height = 8, width = 6.5)







# grafico de setas ----

   
g2 = ggplot(sex_panel) + 
     geom_path(aes(x = index, y = regiao2),
               arrow = arrow(length = unit(1.5, 'mm'), type = 'closed'),
               linejoin = "round", lineend = "butt") +
     geom_text(aes(x = index,
                   y = regiao2,
                   label = formatC(index, digits = 3, big.mark = '.', 
                                   decimal.mark = ','),
                   hjust= 0), )  +
     cagedExplorer::custom_theme() +
     scale_x_continuous(limits = c(0, 80)) +
     theme(panel.grid.major.x = element_blank(),
           axis.text.x = element_blank(),
           panel.grid.major.y = element_line(linetype = 'dotted'))+
     labs(
       x = 'DiferenÃ§a % entre a remuneraÃ§Ã£o de homens e mulheres em 2003 e 2019',
       y = 'RegiÃ£o de Governo',
       title = '',
       subtitle = '',
       caption = 'DireÃ§Ã£o da seta indica a evoluÃ§Ã£o de 2003 para 2019'
     ) 
   
   g2
   
   ggsave(plot = g2,
          filename = 'g3.png',
          height = 8, width = 15)
   
   