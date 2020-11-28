

setwd("D:/Git projects/CEPER/ceper_rais")

library(ggplot2)
library(tidyverse)
library(tmap)
library(readxl)
library("cagedExplorer")


# carregar shape file das regioes de governo ---------

load('regions.rda')
region = polygons_regioes_gov_sp

rm(polygons_regioes_gov_sp)


region$regiao_governo = as.character(region$regiao_governo)

region$regiao_governo[10] = c('B Paulista')
region$regiao_governo[35:38] = c('S J B Vista', 'S J Barra', 'SJ Rio Preto',
                                 'S J Campos')



region = region[order(region$regiao_governo), ]


# dados de genero

sex = read_xlsx('evol.xlsx', sheet = 3)

sex = data.frame(sex)

sex[,2:6] = sex[,2:6]*100 


sex2 = sex[,c(1,2,6)]


colnames(sex2) = c('regiao', '2003', '2019')

sex2$evol = sex2[,'2019'] - sex2[,'2003']

sex2$evol1 = abs(sex2[,'2019'] - sex2[,'2003'])



sex2 = tibble(sex2)

breaks <- seq(-45,15,5)


sex2 =
  sex2 %>%
  mutate(group = .bincode(evol, breaks = breaks)) %>%
  mutate(regiao = fct_reorder(regiao,
                              evol,
                              last) )



# Diferenca salarial entre homens e mulheres (dotplot) ----


palette <- c(reds_full[2:4], blues_full[3:8])


g4 = ggplot(sex2) +
  geom_point(aes(x = evol, y = regiao, fill=factor(group) ),
             size = 5, shape = 21, col = 'gray') +
  scale_x_continuous(labels = function(x) formatC(x, digits = -2, big.mark = '.', decimal.mark = ',')) +
  scale_fill_manual(values = palette) +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  custom_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Diferença % entre a remuneração média de homens e mulheres entre 2003 e 2019',
       y = 'Região de Governo',
       title = '',
       subtitle = '',
       caption = '')

g4

ggsave(plot = g4,
       filename = 'g3.png',
       height = 8, width = 6.5)





# Diferenca salarial entre homens e mulheres (grÃ¡fico de linha)-------


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




