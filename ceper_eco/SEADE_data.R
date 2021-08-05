

setwd("D:/Git projects/CEPER/ceper_eco")


#source('call_f.R')

library(plm)
library(tidyverse)
library(glue)
library(MASS)



# dataset ----

seade =  readxl::read_excel('DADOS_INDEX.xlsx')
cad = read_rds('cad.rds')
snis = read_rds('san.rds')


seade=seade%>%
  mutate(my_code = as.numeric(substr(code_muni, 1, 6)) )

seade = merge(seade, cad, by=c('ano', 'my_code'))

seade = merge(seade, snis, by=c('ano', 'code_muni'))


seade = seade %>%
  dplyr::select(-muni.san)%>%
  dplyr::filter(ano %in% seq(2013, 2017, 2))%>%
  mutate(np = cut(pop, breaks=c(seq(0, 4e5, 1e4), Inf) ) )%>%
  group_by(np, ano)%>%
  #mutate_all( funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)) ) %>%
  mutate(hom_phm =1e3*hom/pop,
         furt_phm = 1e3*furt/pop,
         fv_phm = 1e3*fv/pop,
         roub_phm = 1e3*roub/pop,
         pib_pc = log(pib_pc),
         c_res_elet = log(c_res_elet),
         tot_pes_cad = 1e3*tot_pes_cad/pop, 
         pred = NaN)%>%
  ungroup()


saveRDS(seade, 'SEADE.rds')



