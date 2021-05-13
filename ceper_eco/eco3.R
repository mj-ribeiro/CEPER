#############################################################################################
#                          Estudo dados socioêconomicos
#############################################################################################

# Marcos Júnio Ribeiro



library(sf)
library(geobr)
library(ggplot2)
library(readxl)
library(magrittr)
library(raster)
library(tmap)
library(reshape2)
library(writexl)
library(viridis)
library(stringr)
library(tidyverse)
library(dplyr)


setwd("D:/Git projects/CEPER/ceper_eco")


# população ----


pop = read.csv('pop.csv', sep=';', header = T)

pop2 =  pop[pop$ano==2020, c('muni', 'pop', 'grau_urb', 'code_muni')]



# dados ----

df = read_excel('dados.xlsx', sheet = 'cad')

tail(df)



df$data = as.Date(df$data,"%m/%d/%Y")


df = df%>%
     dplyr::filter(data=='2020-12-01')




#geocode ----

sp = read_municipality(code_muni='SP')



# merge ----


df2 = merge(sp, df, by='code_muni')

df2 = merge(pop2, df2, by='code_muni')

df2['ratio_bf'] = df2$Pes_PBF/df2$pop


df2 = st_as_sf(df2)


head(df2)



# mapas ----



maps_f = function(sh, x, leg, fonte, type, breaks=NULL, 
                  labels=NULL, colors=NULL){
  g1 =  tm_shape(sh) +
    tm_polygons(x,  
                title=leg,  
                textNA = 'Sem dados',
                breaks = breaks,
                label = labels,
                palette=colors
    ) +
    tm_compass(type = "8star",
               position = c("right", "bottom", size = 0.0)) +
    tm_scale_bar(text.size = 0.6) +
    tm_layout(
      legend.text.size = 0.8,
      frame = T,
      legend.format = list(text.separator = "-"))
  tmap_save(g1, filename = paste(fonte,leg,type, sep = ""), width = 6,
            height = 4, units = 'in')
}





# famílias beneficiadas pelo bolsa família


mybreaks = c(seq(0, 2100, 300), Inf)


mylabs = c('0 a 300',
           '300 a 600',
           '600 a 900',
           '900 a 1200',
           '1200 a 1500',
           '1500 a 1800',
           '1800 a 2100',
           'Mais de 2100')

  
mycolors = inferno(length(mylabs), alpha = 1,
                     begin = 0.15, end = 1, direction = 1)

  
sum(df2$Fam_PBF<300)


maps_f(sh=df2, x='Fam_PBF',
       leg='FBBF', 
       fonte='SE_',
       colors = mycolors,
       breaks = mybreaks,
       labels = mylabs
       )





# proporção da população beneficiada pelo bolsa família


mybreaks2 = c(seq(0.01, 0.24, 0.05), Inf)

mylabs2 = c('1% a 6%',
            '6% a 11%',
            '11% a 16%',
            '16% a 21%',
            'Mais de 21%')


mycolors2 = inferno(length(mylabs2), alpha = 1,
                   begin = 0.15, end = 1, direction = 1)




maps_f(sh=df2, x='ratio_bf',
       leg='Proporção da PBBF', 
       fonte='SE_', 
       type = '.eps',
       colors = mycolors2,
       breaks = mybreaks2,
       labels = mylabs2)





#  PIB Municipal ----

pib = read_excel('dados.xlsx', sheet = 'pib')

head(pib)


pib[pib['ano']==2018,]


pib = pib %>%
      dplyr::filter(ano==2018)

pib['soma']= pib %>%
    summarise(soma=100*pib/sum(pib) )


pib_m = merge(sp, pib, by='code_muni')

pib_m = st_as_sf(pib_m)




maps_f(sh=pib_m, x='soma',
       leg='Percentual do PIB', 
       fonte='SE_', 
       type = '.png',
       )


summary(pib_m$soma)

mybreaks3 =  c(0, 0.005, 0.01, 0.015, 0.06, 0.15, 0.25, Inf )

mylabs3 = c('0% a 0,005%',
              ' 0,005% a 0,01%',
              '0,01% a 0,015%',
              '0,015% a 0,06%',
              '0,06% a 0,15%',
              '0,15% a 0,25%',
              'Mais de 0,25%')


mycolors3 = inferno(length(mylabs3), alpha = 1,
                    begin = 0.15, end = 1, direction = 1)






maps_f(sh=pib_m, x='soma',
       leg='PIB relativo', 
       fonte='SE_', 
       type = '.eps',
       colors = mycolors3,
       breaks = mybreaks3,
       labels = mylabs3
)





