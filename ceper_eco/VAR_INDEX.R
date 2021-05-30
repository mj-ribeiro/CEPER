
# Marcos Júnio Ribeiro

# CEPER


setwd("D:/Git projects/CEPER/ceper_eco")


source('call_f.R')


# seade ----

seade =  readxl::read_excel('DADOS_INDEX.xlsx')
cad = read_rds('cad.rds')
snis = read_rds('san.rds')


seade=seade%>%
  mutate(my_code = as.numeric(substr(code_muni, 1, 6)) )

seade = merge(seade, cad, by=c('ano', 'my_code'))


seade = merge(seade, snis, by=c('ano', 'code_muni'))



seade = seade %>%
  dplyr::select(-muni.san)%>%
  filter(ano>2008)%>%
  mutate_all( funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)) ) %>%
  mutate(hom_phm =1e3*hom/pop,
         furt_phm = 1e3*furt/pop,
         fv_phm = 1e3*fv/pop,
         roub_phm = 1e3*roub/pop,
         pib_pc = log(pib_pc),
         c_res_elet = log(c_res_elet),
         tot_pes_cad = 1e3*tot_pes_cad/pop)

colnames(seade)

nomes = seade[,c('muni', "code_muni", 'ano')]



# seade[,'row'] = paste(seade$muni, seade$ano, sep='_')
# 
# seade = seade%>%
#         tibble::column_to_rownames('row')
# 



# crime -----

crime = seade %>%
  dplyr::select(c(ano, muni, hom_phm, furt_phm, roub_phm) )%>%
  group_by(muni) %>%
  mutate_all(f2) %>%
  rowwise() %>%
  mutate(cr = mean(c(hom_phm, furt_phm, roub_phm), na.rm=T))

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




# longevidade


long = seade %>%
  dplyr::select(c(ano, muni, tx_m_15, tx_m_60, tx_m_infantil, tx_m_peri))%>%
  group_by(muni) %>%
  mutate_all(f1) %>%
  rowwise() %>%
  mutate(long = mean(c(tx_m_15, tx_m_60, tx_m_infantil, tx_m_peri), na.rm=T))

long$ano = nomes$ano



# long%>%
#   filter(ano==2017)%>%
#   ggplot(aes(long)) + geom_histogram(bins=20, fill='blue', color='white')



# saude ----

saude = seade %>%
  dplyr::select(c(ano, muni, l_sus_pmh, t_enf_phm, enf_phm, m_phm))%>%
  group_by(muni) %>%
  mutate_all(f1) %>%
  rowwise() %>%
  mutate(sau = mean(c(l_sus_pmh, t_enf_phm, enf_phm, m_phm), na.rm=T))


saude$ano = nomes$ano



# riqueza -----


riq = seade %>%
  dplyr::select(c(ano, muni, c_res_elet, pib_pc, tot_pes_cad ))%>%
  group_by(muni) %>%
  mutate_all(f1) %>%
  mutate(tot_pes_cad=1-tot_pes_cad) %>%
  rowwise() %>%
  mutate(riq = mean(c(c_res_elet, pib_pc, tot_pes_cad), na.rm=T))

riq$ano = nomes$ano


# saneamento ----


san = seade %>%
  group_by(muni) %>%
  dplyr::select(c( IN046_AE, IN055_AE, IN015_AE))%>%
  group_by(muni) %>%
  mutate_all(f1) %>%
  rowwise() %>%
  mutate(san = mean(c(IN046_AE, IN055_AE, IN015_AE), na.rm=T))


san$ano = nomes$ano


# get variables

nomes[,'crime'] =  crime$cr
nomes[,'educ']  =   educ$educ
nomes[,'long'] = long$long
nomes[,'sau'] = saude$sau
nomes[,'riq'] = riq$riq
nomes[,'san'] = san$san



saveRDS(nomes, 'VAR_INDEX.rds')






