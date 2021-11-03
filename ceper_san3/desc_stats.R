#########################################################################
#                      PRODUTIVIDADE ESTADUAL
#########################################################################

# here I will calculate descriptive statistics on the sanitation 
# situation in the municipalities of SÃ£o Paulo 

# 
# Sys.setenv(R_REMOTES_STANDALONE="true")
# remotes::install_github("mj-ribeiro/Microdados" )



setwd("D:/Git projects/CEPER/ceper_san3")


source('functions.R', encoding = 'UTF8')


# dataset ----

df = readxl::read_excel('snis2.xlsx')


# change colnames ---- 

old_n = colnames(df)

colnames(df) = substr(colnames(df), 1, 5)
colnames(df)[c(1, 2, 5)] = c('cod_ibge', 'cod_mun', 'ano')

new_n = colnames(df)


view_n = function(){
  for(o in 1:length(old_n)){
    cat(new_n[o], '---->', old_n[o], '\n')
  }
}


# create new variable ----

view_n()

tt = df %>%
  dplyr::filter(ano == 2019)%>%
  group_by(Natur)%>%
  summarise(n = mean(IN004) )

view(tt) 

df = df %>%
  mutate(my_code = case_when(
    Natur %in% tt$Natur[1:5]   ~ "Adm. pública direta",    # "Administração pública direta"
    Natur %in% tt$Natur[6:10]  ~ "Autarquia",                       # "Autarquia"
    Natur %in% tt$Natur[11:12] ~ "Empresa privada",                 # "Empresa privada"
    Natur %in% tt$Natur[13:14] ~ "Empresa pública",                 # "Empresa pública"
    Natur %in% tt$Natur[15:19] ~ "Mista",                           # "Sociedade de economia mista com administraÃ§Ã£o pÃºblica"
  )
) 


# descriptive stats ----

stats2 = function(data, x){
  x = enquo(x)
  z = data %>%
    dplyr::filter(ano==2019)%>%
    #group_by(my_code)%>%
    drop_na(everything())%>%
    summarise(Média = mean(!!x),
              Mediana = median(!!x),
              Máximo = max(!!x),
              Mínimo = min(!!x),
              Desvio = sd(!!x) )%>%
    as.data.frame() 
  colnames(z)[5] = 'Desvio Padrão'
  return(z)
}


vars = c('IN004', 'IN005', 'IN006', 'IN015', 'IN016', 
         'IN055', 'IN012', 'IN102', 'IN083', 'IN008', 'IN026')



descs = data.frame()


for (o in 1:length(vars)) {             
  descs = rbind(descs, stats2(df, eval(parse(text = vars[o])) ) ) # loop through variables names
}


rownames(descs) = vars
# 
# stargazer::stargazer(descs, summary = F, out = paste(path_tab, 'descs.tex'),
#                      decimal.mark = ',', digit.separator = '',
#                      digits.extra = 0, digits = 2,
#                      rownames = T, colnames = T)





view_n()


# Count table ----

prt = df %>%
  dplyr::filter(ano==2019)%>%
  dplyr::count(my_code)%>%
  mutate(prop = 100*prop.table(n))


# 
# stargazer::stargazer(data.frame(prt), summary = F, 
#                      out = paste(path_tab, 'prt', '.tex', sep = ''),
#                      decimal.mark = ',', digit.separator = '',
#                      digits.extra = 0, digits = 2,
#                      rownames = F, colnames = T)




# mean ----

ns = c()

c = 1
for (o in 1:22) {
  if(o%%2 != 0){
    c = c + 1
    ns[o] = c
  }  
  else{
    ns[o] = c + 11
  }
}



desc2 = df %>%
  dplyr::filter(ano==2019)%>%
  group_by(my_code) %>%
  summarise_at(vars, mean, na.rm = TRUE)%>%
  drop_na() %>% 
  tidyr::pivot_longer(-1) %>%
  tidyr::pivot_wider(names_from = 1, values_from = value)


rn = desc2$name

desc2 = desc2[,-1]

rownames(desc2) = rn

view(desc2)

round(desc2, 2)



# stargazer::stargazer(data.frame(desc2), summary = F, 
#                      out = paste(path_tab, 'desc2', '.tex', sep = ''),
#                      decimal.mark = ',', digit.separator = '',
#                      digits.extra = 0, digits = 2,
#                      rownames = T, colnames = T)




# Kruskal test ---- 


kt = c()

for (o in 1:length(vars)) {
  
kt[o] = kruskal.test(formula = my_code ~ eval(parse(text = vars[o])),
             data = df, subset= ano==2019 )$p.value
}

round(kt, 2)


ktest = data.frame(Índices = vars, 
            kruskal = kt,
            H0 = ifelse(kt>0.1, 'Aceita', 'Rejeita'))
            

# table

# stargazer::stargazer(ktest, summary = F, 
#                      out = paste(path_tab, 'ktest', '.tex', sep = ''),
#                      decimal.mark = ',', digit.separator = '',
#                      digits.extra = 0, digits = 2,
#                      rownames = F, colnames = T)
# 




df %>%
  dplyr::filter(ano==2019)%>%
  mutate(prest2 = as.numeric(substr(Prest, 2, 9) ) ) %>%
  dplyr::count(prest2)%>%
  mutate(prop = 100*prop.table(n)) %>%
  print(n=260)

z=df %>%
  dplyr::filter(ano==2019)%>%
  mutate(prest2 = as.numeric(substr(Prest, 2, 9) ) )%>%
  select(prest2, my_code, Prest)
  

which( table( z$prest2 ) != 1 )


view_n()



# correl graph ----


corr = df %>%
        dplyr::filter(ano==2019)%>%
        select(vars) %>%
  drop_na(everything()) %>%
  cor()



options(OutDec=",")


cg = ggcorrplot::ggcorrplot( as.data.frame(corr), hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 5, 
           method = "circle", 
           colors =c("red", "white", "chartreuse") 
            ) +
   theme(plot.margin=grid::unit(c(0, 0, 0, 0), "mm"),
         legend.text = element_text(size = 12))



ggsave(cg, file = paste(path_fig, 'corr.eps'),
       device="eps",  height=6, width=8, dpi = 300)


options(OutDec=".")


# ranking  ----


natur = c("Adm. pública direta", "Autarquia", "Empresa privada",
          "Empresa pública", "Mista")

desc2[, "max"] <- apply(desc2[, 1:5], 1, max)


posit = c()
for(i in 1:dim(desc2)[1]){
  posit[i] = which(desc2[i, 1:5] %in% desc2[i, 6])
}

desc2[,'rank'] = natur[posit]
rownames(desc2) = vars

colnames(desc2)[6:7] = c('Máximo', 'Ranking')


stargazer::stargazer(data.frame(desc2), summary = F,
                     #out = paste(path_tab, 'desc2', '.tex', sep = ''),
                     decimal.mark = ',', digit.separator = '',
                     digits.extra = 0, digits = 2,
                     rownames = T, colnames = T)


# regs -----

mod = lm(IN004 ~ IN083 + my_code, data = df, subset = ano==2019)

mod1 = lm(IN004 ~ IN102 + my_code, data = df, subset = ano==2019)

mod2 = lm(IN004 ~ IN055 + my_code, data = df, subset = ano==2019)



stargazer::stargazer(mod,mod1,mod2, type = "latex",  #we use html output to match our planned R Markdown output
          title = "My iris models")



df %>%
  dplyr::filter(ano==2019 )%>%
  ggplot(aes(x = IN083)) +
  geom_point(aes(y = IN004, pch = my_code, color = my_code), cex=4  ) +
  geom_smooth( aes(x = IN083, y = IN004), method = 'gam')
  
  
  
  
# maps ----




library(udunits2)

library(tmap)
library(geobr)
library(sf)

view(tt)

tt = df %>%
  dplyr::filter(ano==2019)


sp = read_municipality(code_muni = 'SP')

sp = sp[order(sp$name_muni), ]



ht = merge(tt, sp, by.x='cod_ibge', by.y = 'code_muni')

ht = st_as_sf(ht)

view(ht)

mycol = c('beige','burlywood1','orange','darkorange1', 'darkorange3', 'gray')


m1 = tm_shape(ht) +
  tm_polygons('my_code',
              title = 'Natureza',
              textNA = 'Sem dados',
              palette = mycol) +
  #tm_text(text = 'sig.x', size =0.8) +
  tm_compass( type = '8star', 
              position = c('right', 'top', size=0) ) +
  tm_scale_bar(text.size = 1, lwd=1.5, just = c('bottom', 'right') )  +
  tm_layout(main.title = '',
            main.title.position = 'center',
            legend.text.size = 0.6,
            legend.width = 0.9,
            frame = T) 


tmap_save(m1 ,
          filename = paste(path_fig, 'm1.eps', sep = ""),
          width = 5,
          height = 5, asp = 0, outer.margins=0)




