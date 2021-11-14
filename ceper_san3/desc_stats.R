#########################################################################
#                      PRODUTIVIDADE ESTADUAL
#########################################################################

# here I will calculate descriptive statistics on the sanitation 
# situation in the municipalities of São Paulo 




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
    Natur %in% tt$Natur[15:19] ~ "Mista",                           # "Sociedade de economia mista com administração pública"
  ),
  my_code2 = case_when(
    Natur %in% tt$Natur[1:5]   ~ "Adm. pública direta",    
    Natur %in% tt$Natur[6:10]  ~ "Adm. pública indireta",                       
    Natur %in% tt$Natur[11:12] ~ "Empresa privada",                 
    Natur %in% tt$Natur[13:14] ~ "Adm. pública indireta",                 
    Natur %in% tt$Natur[15:19] ~ "Adm. pública indireta",                           
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
# see https://www.ermontoro.com/post/teste-de-kruskal-wallis

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


# stargazer::stargazer(data.frame(desc2), summary = F,
#                      #out = paste(path_tab, 'desc2', '.tex', sep = ''),
#                      decimal.mark = ',', digit.separator = '',
#                      digits.extra = 0, digits = 2,
#                      rownames = T, colnames = T)


  
  
# maps ----





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



# regs -----

coef_names = c('Intercepto', 
               'Índice',
               'Autarquia',
               'Empresa privada',
               'Empresa pública',
               'Mista',
               'Autarquia*Índice',
               'Empresa privada*Índice',
               'Empresa pública*Índice',
               'Mista*Índice'
)


mod0 = lm(IN004 ~  IN015 + my_code + my_code*IN015, data = df, subset = ano==2019)
#summary(mod0)

mod1 = lm(IN004 ~ IN016 + my_code + my_code*IN016, data = df, subset = ano==2019)

mod2 = lm(IN004 ~ IN055 + my_code + my_code*IN055, data = df, subset = ano==2019)

mod3 = lm(IN004 ~ IN012 + my_code + my_code*IN012, data = df, subset = ano==2019)

mod4 = lm(IN004 ~ IN102 + my_code + my_code*IN102, data = df, subset = ano==2019)

mod5 = lm(IN004 ~ IN083 + my_code + my_code*IN083, data = df, subset = ano==2019)

mod6 = lm(IN004 ~ IN008 + my_code + my_code*IN008, data = df, subset = ano==2019)

mod7 = lm(IN004 ~ IN026 + my_code + my_code*IN026, data = df, subset = ano==2019)



# slopes 

t0 = test(emtrends(mod0, ~ my_code, var="IN015"))
t1 = test(emtrends(mod1, ~ my_code, var="IN016"))
t2 = test(emtrends(mod2, ~ my_code, var="IN055"))
t3 = test(emtrends(mod3, ~ my_code, var="IN012"))
t4 = test(emtrends(mod4, ~ my_code, var="IN102"))
t5 = test(emtrends(mod5, ~ my_code, var="IN083"))
t6 = test(emtrends(mod6, ~ my_code, var="IN008"))
t7 = test(emtrends(mod7, ~ my_code, var="IN026"))

sig(t7)

library('rstatix')

sig = function(x){
        add_significance(
          data = x,
          p.col = 'p.value',
          output.col = 'my',
          cutpoints = c(0.0, 0.01,  0.05, 0.1, 1),
          symbols = c("***", "**", "*", "ns")
        )
  }


slopes = round(cbind(t0$IN015.trend, t1$IN016.trend, t2$IN055.trend, t3$IN012.trend,
      t4$IN102.trend, t5$IN083.trend, t6$IN008.trend, t7$IN026.trend), 4 )


slopes = data.frame(slopes)

colnames(slopes) = vars[4:length(vars)]

rnames = c('Adm. pública direta',
            'Autarquia',
            'Empresa privada',
            'Empresa pública',
            'Mista')
rownames(slopes) = rnames



stargazer::stargazer(t(slopes), summary = F,
                     #out = paste(path_tab, 'desc2', '.tex', sep = ''),
                     decimal.mark = ',', digit.separator = '',
                     digits.extra = 0, digits = 4,
                     rownames = T, colnames = T)





# marginal means 

emmeans(mod0, ~ my_code, var="IN015")
emmeans(mod1, ~ my_code, var="IN016")
emmeans(mod2, ~ my_code, var="IN055")
emmeans(mod3, ~ my_code, var="IN012")
emmeans(mod4, ~ my_code, var="IN102")
emmeans(mod5, ~ my_code, var="IN083")
emmeans(mod6, ~ my_code, var="IN008")
emmeans(mod7, ~ my_code, var="IN026")



# change names of coefs

names(mod7$coefficients) = coef_names
names(mod6$coefficients) = coef_names
names(mod5$coefficients) = coef_names
names(mod4$coefficients) = coef_names
names(mod3$coefficients) = coef_names
names(mod2$coefficients) = coef_names
names(mod1$coefficients) = coef_names
names(mod0$coefficients) = coef_names


# table 

stargazer::stargazer(mod0, mod1, mod2, mod3, mod4,
                     mod5, mod6, mod7, type="text",
                     dep.var.labels=c("IN004"),
                     column.labels = c('IN015', 'IN016', 'IN055', 'IN012',
                                       'IN102', 'IN083', 'IN008', 'IN026'),
          out="models.text" )






#####

prest = df%>%
  dplyr::filter(ano==2019) %>%
  select(Prest, my_code) %>%
  mutate(p2 = as.numeric(substr(Prest, 2, 9) ) )
  


tab =table(prest$p2, prest$my_code)

print(tab)




# regiões de governo ----

load("D:/Git projects/CEPER/ceper_san3/polygons_regioes_adm_sp.rda")

rg = st_as_sf(polygons_regioes_adm_sp) %>%
      st_set_crs(4674) %>% 
      st_cast()

rg$regiao_administrativa[13] = "São J. do R. Preto"

m2 = tm_shape(rg) +
  tm_polygons('regiao_administrativa', 
              id = 'regiao_administrativa', 
              #palette ='Greens', 
              border.col = "#555555",
              legend.show = FALSE) +
  tm_text('regiao_administrativa', 
          size = 0.8,
          fontface = 'bold',
          col = 'black'
          ) + 
  tm_scale_bar(text.size = 0.60)  +
  tm_compass(position = c("right", "top"), size = 2)


tmap_save(m2,
          filename = paste(path_fig, 'm2.eps', sep = ""),
          width = 8,
          height = 5, asp = 0, outer.margins=0)











