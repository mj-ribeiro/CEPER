
# Marcos Júnio Ribeiro

# CEPER


setwd("D:/Git projects/CEPER/ceper_eco")


source('call_f.R')


# seade ----

seade =  readxl::read_excel('DADOS_INDEX.xlsx')

seade = seade %>%
        filter(ano>2008)%>%
  mutate_all( funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)) ) %>%
  mutate(hom_cem =1e5*hom/pop,
         furt_cem = 1e5*furt/pop,
         fv_cem = 1e5*fv/pop,
         roub_cem = 1e5*roub/pop,
  )


nomes = seade[,c('muni', "code_muni", 'ano')]



# seade[,'row'] = paste(seade$muni, seade$ano, sep='_')
# 
# seade = seade%>%
#         tibble::column_to_rownames('row')
# 



# crime -----

crime = seade %>%
        dplyr::select(c(ano, muni, hom_cem, furt_cem, roub_cem) )%>%
        group_by(muni) %>%
        mutate_all(f2) %>%
        rowwise() %>%
        mutate(cr = mean(c(hom_cem, furt_cem, roub_cem), na.rm=T))

crime$ano = nomes$ano



# educ ----


educ = seade %>%
      dplyr::select(c(ano, muni, dist_id_serie, ideb_ini, ideb_fin) )%>%
  group_by(muni) %>%
  mutate_all(f1) %>%
  mutate(dist_id_serie = 1-dist_id_serie)%>%
  rowwise() %>%
  mutate(educ = mean(c(dist_id_serie, ideb_ini, ideb_fin), na.rm=T))

educ$ano = nomes$ano


educ%>%
  filter(ano==2017) %>%
  ggplot(aes(educ)) + geom_histogram(fill='blue', color='white')
  
  
  




colnames(seade)



# CAD


data = d('2018')

data=data[substr(data$ibge, 1,2) == '35' & data$anomes==201812, ]

head(data)





