

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




# Diferenca salarial entre homens e mulheres (dotplot) 


sex = read_xlsx('evol.xlsx', sheet = 3)

sex = data.frame(sex)

sex[,2:6] = sex[,2:6]*100 


sex2 = sex[,c(1,2,6)]


colnames(sex2) = c('regiao', '2003', '2019')

sex2$evol = sex2[,'2019'] - sex2[,'2003']

sex2$evol1 = abs(sex2[,'2019'] - sex2[,'2003'])




breaks <- seq(-45,15,5)
palette <- c(reds_full[2:4], blues_full[3:8])



sex2 = tibble(sex2)

sex2 =
  sex2 %>%
  mutate(group = .bincode(evol, breaks = breaks)) %>%
  mutate(regiao = fct_reorder(regiao,
                              evol,
                              last) )





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









