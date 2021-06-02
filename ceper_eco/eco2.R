#############################################################################################
#       Estudo dados socioêconomicos FIRJAN
#############################################################################################

# Marcos Júnio Ribeiro


source('call_f.R')


setwd("D:/Git projects/CEPER/ceper_eco")



# Geocode ----


sp = read_municipality(code_muni = 'SP')
sp = sp[order(sp$name_muni), ]




b = c('á', 'é', 'í', 'ó', 'ú')
c = c('a', 'e', 'i', 'o', 'u')



sp$name_muni = str_replace(sp$name_muni, "-", " ")
sp$name_muni = str_replace(sp$name_muni, "'", " ")

for (i in 1:length(b)) {
  sp$name_muni = str_replace(sp$name_muni, b[i], c[i])
}

sp$name_muni = tolower(sp$name_muni)




# firjan ----

fir = readxl::read_excel('firjan.xlsx', sheet = 'd1')
str(fir)



fir$name_muni = str_replace(fir$name_muni, "-", " ")
fir$name_muni = str_replace(fir$name_muni, "'", " ")

for (i in 1:length(b)) {
  fir$name_muni = str_replace(fir$name_muni, b[i], c[i])
}

fir$name_muni = tolower(fir$name_muni)

fir = fir[order(fir$name_muni),]

fir[fir$name_muni!=sp$name_muni, 'name_muni'] = c('embu', 'moji mirim')





# merge dataset ----

df2 = merge(sp, fir, by='name_muni')

df2 = st_as_sf(df2)





# mapas ----


mybreaks = c(seq(0.5, 0.9, 0.1), Inf)

mylabel = c('0,5 a 0,6',
             '0,6 a 0,7',
             '0,7 a 0,8',
             '0,8 a 0,9',
             'Mais de 0,9')



mycolors = inferno(length(mylabel), alpha = 1,
                    begin = 0.15, end = 1, direction = 1)



maps_f(sh=df2, x='IFDM',
       leg='IFDM', 
       fonte='FIR_',
       breaks = mybreaks,
       colors = mycolors,
       labels = mylabel
       )




  
fir = tibble(df2)%>%
  dplyr::select(c('name_muni', 'code_muni',"IFDM", 
        "Emprego & Renda", "Educação","Saúde"))


saveRDS(fir, 'FIRJAN.rds')




