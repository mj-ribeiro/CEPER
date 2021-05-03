
setwd("D:/Git projects/CEPER/pres")


library(quantmod)
library(tidyverse)
library(zoo)

library(RcppRoll)

dados_covid = vroom::vroom("https://data.brasil.io/dataset/covid19/caso_full.csv.gz")

head(dados_covid)[1:5]




g1 = dados_covid%>%
  filter(place_type=='state') %>%
  group_by(date) %>%
  summarise(total = sum(new_confirmed)) %>%
  dplyr::mutate(casos = roll_meanr(total, n=7)) %>%
    ggplot(aes(x=date, y=casos)) + 
    geom_line(size=1.3) +
    scale_x_date("Data", breaks = "1 month", date_labels = "%b %y",  expand = c(0, 0)) +
    scale_y_continuous("Casos de Covid (em milhares)", breaks = seq(0, 140000, 20000), labels =
                       seq(0, 140, 20)) +
  theme_minimal() +
  labs(title=('Evolução do número de casos de \n Covid no Brasil')) +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 20),
        axis.text.x = element_text(angle =45,size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size=18)) 
  

g1


## Ativos




get_data = function(x, d= "2015-01-01"){
  z = getSymbols(x, src='yahoo', 
                 from= d, 
                 #  periodicity = "monthly",    
                 auto.assign = F)[,4]
  return(z)
}



my_plot = function(x, my_label, my_title){
  ativo = get_data(x)
  date = as.Date(index(ativo)) # pega o índice do bradesco
  ativo = tibble(date, ativo)
  ativo %>%
    filter(is.na(ativo)==F) %>%
    ggplot(aes(x=date, y=ativo)) +
    geom_line(size=1.2) +
    scale_x_date("Data", breaks = "1 year", date_labels = " %Y") +
    scale_y_continuous(my_label) +
    theme_minimal() +
    labs(title=my_title) +
    theme(plot.title = element_text(hjust = 0.5),
          title = element_text(size = 20),
          axis.text.x = element_text(angle =45,size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size=18)) 
  
}



g2 = my_plot('BZ=F', 
        'Preço do Barril de Petróleo Brent (U$$)',
        'Evolução do Preço do Barril de Petróleo \n Brent em dólares')



g4 = my_plot('GC=F', 
             'Preço do Ouro (U$$)',
             'Evolução do preço do ouro, por onça troy, \n em dólares')


ggsave('brent.png', g2, width = 10, height = 8)


g3



