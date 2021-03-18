


setwd("D:/Git projects/CEPER/pres")


library(tidyverse)
library(survey)


pnad_covid = read_rds('pnad_covid')


pnadc_plano <-     
  svydesign(            
    ids = ~ UPA ,        # Declara a unidade amostral mais granular
    strata = ~ Estrato , # Declara a variável que contém os estratos
    weights = ~ V1031 ,  # Declara variável com pesos
    data = pnad_covid ,  # Declara base de microdados
    nest = TRUE          # Declara que os estratos podem conter identificações identicas para UPA's distintas
)


mediaRendaUF <- svyby(~VD4020, ~UF, pnadc_plano, svymean, na.rm = T)







