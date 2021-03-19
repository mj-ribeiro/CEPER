
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



my_plot = function(x){
  ativo = get_data(x)
  date = as.Date(index(ativo)) # pega o índice do bradesco
  ativo = tibble(date, ativo)
  ativo %>%
    filter(is.na(ativo)==F) %>%
    ggplot(aes(x=date, y=ativo)) +
    geom_line(size=1.2) +
    scale_x_date("Data", breaks = "1 year", date_labels = " %y",  expand = c(0, 0)) +
    scale_y_continuous("ativo") +
    labs(title=('Evolução do número ')) +
    theme(plot.title = element_text(hjust = 0.5),
          title = element_text(size = 20),
          axis.text.x = element_text(angle =45,size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size=18)) 
  
}



my_plot('oil')









