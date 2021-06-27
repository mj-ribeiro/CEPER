

#setwd("D:/Git projects/shiny_curso")

library(shiny)





#rsconnect::setAccountInfo(name='marcos-j-ribeiro', token='223F438182FAF60DF0AE4F4DFBAFA284', secret='UfdbIHFqhz98ZdI5o550aljdYSB78V2sSjZ5q2WW')

ui = fluidPage(

    sidebarLayout(
    
    sidebarPanel(
      
      
    ),
    
    mainPanel(
       tags$img(src="ceper.png", height=150, width=1000), 
      
      tabsetPanel(
        tabPanel('Indice 2017', DT::dataTableOutput("i17")),
        tabPanel('Índice 2015', DT::dataTableOutput("i15")),
        tabPanel('Índice 2013', DT::dataTableOutput("i13")),
        
        tabPanel('Ranking SubIndice-2017', DT::dataTableOutput("r17")),
        tabPanel('Ranking SubIndice-2015', DT::dataTableOutput("r15")),
        tabPanel('Ranking SubIndice-2013', DT::dataTableOutput("r13"))
        
      )
    ) 
  )
)




server = function(input, output){
  
  s_17 <- readRDS("./data/s_17.rds")
  s_17 <- round(s_17[, c(seq(1,7),11)], 4)
  output$i17 <- DT::renderDataTable(s_17)
  
   s_15 <- readRDS("./data/s_15.rds")
   s_15 <- round(s_15[, c(seq(1,7),11)], 4)
   output$i15 <- DT::renderDataTable(s_15)
   
    
    s_13 <- readRDS("./data/s_13.rds")
    s_13 <- round(s_13[, c(seq(1,7),11)], 4)
    output$i13 <- DT::renderDataTable(s_13)
    
    
    r_13 <- readRDS("./data/r13.rds")
    output$r13 <- DT::renderDataTable(r_13)
  
    r_15 <- readRDS("./data/r15.rds")
    output$r15 <- DT::renderDataTable(r_15)
    
    r_17 <- readRDS("./data/r17.rds")
    output$r17 <- DT::renderDataTable(r_17)
    
  
}



shinyApp(ui, server)








