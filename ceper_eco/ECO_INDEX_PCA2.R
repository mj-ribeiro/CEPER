
# Marcos Júnio Ribeiro

# CEPER


setwd("D:/Git projects/CEPER/ceper_eco")


source('call_f.R')


# seade ----

seade =  readxl::read_excel('DADOS_INDEX.xlsx')

seade = seade %>%
        filter(ano>2008)



head(seade)


nomes = seade[,c('muni', "code_muni", 'ano')]


t=seade%>%
  group_by(muni)%>%
  summarise(med = mean(hom, na.rm=TRUE))%>%
  print(n=40)



seade <- seade %>%  
  group_by(muni) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))



seade = new_df %>%
  group_by(muni, ano) %>%
  mutate(hom_cem =1e5*hom/pop,
         furt_cem = 1e5*furt/pop,
         fv_cem = 1e5*fv/pop,
         roub_cem = 1e5*roub/pop,
         )




# CAD


data = d('2018')

data=data[substr(data$ibge, 1,2) == '35' & data$anomes==201812, ]

head(data)





