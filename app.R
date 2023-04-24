library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)

#leer dataframe y quitarle columnas ----
dataset <- read.csv("/Cursos/R/Video_Games_Sales.csv")
dataset2 <- select(dataset, -(Critic_Score:User_Count))

#Encontrar el o los registros con max y min ventas a nivel global ----
max_sales <- max(dataset2$Global_Sales)
min_sales <- min(dataset2$Global_Sales)
#----

#Consultas para las variables selectInput -----
v_plat <- distinct(dataset2, Platform)
v_genre <- distinct(dataset2, Genre)
v_year <- distinct(dataset2, Year_of_Release)
dt_sales <- select(dataset2, NA_Sales, EU_Sales,
                   JP_Sales, Other_Sales, Global_Sales)

sales_col <- colnames(dt_sales)
#-----
#select para agrupar solo por plataformas y ventas y hacer sumatorias de estas (platform_sales)-----
platform <- select(dataset2, Platform, NA_Sales, EU_Sales,
                   JP_Sales, Other_Sales, Global_Sales)

platform_sales <- platform %>% group_by(Platform) %>% 
  summarise(ventas_NA = sum(NA_Sales), Ventas_EU = sum(EU_Sales),
            ventas_JP = sum(JP_Sales), ventas_Other = sum(Other_Sales),
            ventas_G = sum(Global_Sales))
#-----

#Seleccionar Data con Platf, Year y ventas----
plat_p_year <- select(dataset2, Platform, Year_of_Release,NA_Sales, EU_Sales,
                      JP_Sales, Other_Sales, Global_Sales)
#---
#Nuevo select por plataforma, year y genero -----
pgy_dt <- select(dataset2, Platform, Year_of_Release, Genre,NA_Sales, EU_Sales,
                 JP_Sales, Other_Sales, Global_Sales)
#Filtro agrupando y sumando 
v_pgy_sales <- pgy_dt %>% group_by(Platform, Year_of_Release, Genre) %>% 
  summarise(ventas_NA = sum(NA_Sales), ventas_EU = sum(EU_Sales),
            ventas_JP = sum(JP_Sales), ventas_Other = sum(Other_Sales),
            ventas_G = sum(Global_Sales)) 




#Select por generos y ventas----
gs_dt <- select(dataset2, Genre, Year_of_Release, NA_Sales, EU_Sales,
                JP_Sales, Other_Sales, Global_Sales)
#se agrupa por genero y se suman ventas
genre_sales <- gs_dt %>% group_by(Genre, Year_of_Release) %>% 
  summarise(ventas_NA = sum(NA_Sales), Ventas_EU = sum(EU_Sales),
            ventas_JP = sum(JP_Sales), ventas_Other = sum(Other_Sales),
            ventas_G = sum(Global_Sales))

#Select para año y ventas----
ys_dt <- select(dataset2,Year_of_Release, NA_Sales, EU_Sales,
                JP_Sales, Other_Sales, Global_Sales)
#se agrupa por genero y se suman ventas
year_sales <- ys_dt %>% group_by(Year_of_Release) %>% 
  summarise(ventas_NA = sum(NA_Sales), Ventas_EU = sum(EU_Sales),
            ventas_JP = sum(JP_Sales), ventas_Other = sum(Other_Sales),
            ventas_G = sum(Global_Sales))
#----

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Ventas de Videojuegos 1980-2015"),
  sidebarPanel(
    
    h3("Menu de Opciones:"),
    
    selectizeInput(
      "sel", "Acciones Predeterminadas", choices = c("","videoGames", "max", "min","ventas_platform",
                                     "ventas_year"),
      options = list(
        placeholder = "- Selecciona -",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      "platforms", "Plataformas", choices = unique(v_plat$Platform),
      options = list(
        placeholder = "- Selecciona -",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      "genres", "Generos", choices = unique(v_genre$Genre),
      options = list(
        placeholder = "- Selecciona -",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      "years", "Año de Lanzamiento", choices = unique(v_year$Year_of_Release),
      options = list(
        placeholder = "- Selecciona -",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      "ventas", "Ventas", choices = sales_col,
      options = list(
        placeholder = "- Selecciona -",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    #action buttons para accionar las consultas o graficas
    actionButton("show", "Show Data"),
    actionButton("show2", "Ventas por Plataforma"),
    actionButton("show3", "Venta generos/Plataforma"),
    actionButton("show4", "Grafica Ventas/Genero"),
    width = 4
    
    
  ),
  mainPanel(
    dataTableOutput("grafica"),
    dataTableOutput("consulta1"),
    dataTableOutput("consulta2"),
    dataTableOutput("consulta3"),
    dataTableOutput("consulta4"),
    plotOutput("distPlot"),
    plotOutput("distPlot2"),
    plotOutput("distPlot3"),
    width = 6,
    
  )
)

server <- function(input, output, session){
  
  #primer Output----
  output$grafica <- renderDataTable({
    if(input$sel == "max"){
      output$grafica <- renderDataTable({
        dataset2[dataset2$Global_Sales == max_sales, ]})
    }
    else if(input$sel == "min"){
      output$grafica <- renderDataTable({
        dataset2[dataset2$Global_Sales == min_sales, ]})
    }
    else if(input$sel == "videoGames"){
      output$grafica <- renderDataTable(dataset2)
    }
    else if(input$sel == "ventas_platform"){
      output$grafica <- renderDataTable(platform_sales)
      #output$distPlot <- renderPlot(hist(platform_sales))
    }
    else if(input$sel == "ventas_year"){
      output$grafica <- renderDataTable(year_sales)
    }
  })
  
  #filtro ventas en una año, de un genero de una plataforma ----
  filter1 <- eventReactive(eventExpr = input$show,
                           valueExpr = {dplyr::filter(dataset2,Platform == input$platforms,
                                                      Year_of_Release == input$years,
                                                      Genre == input$genres)})
  
  output$consulta1 <- renderDataTable(filter1())
  #aqui termina y se envia al dataTableOutput----
  
  #Filtro2 para mostrar ventas de pplataforma en un año especifico----
  filter2 <- eventReactive(eventExpr = input$show2,
                           valueExpr = {
                             plat_p_year %>% group_by(input$platforms, input$years) %>% 
                               summarise(ventas_NA = sum(NA_Sales), ventas_EU = sum(EU_Sales),
                                         ventas_JP = sum(JP_Sales), ventas_Other = sum(Other_Sales),
                                         ventas_G = sum(Global_Sales)) 
                           }
  )
  output$consulta2 <- renderDataTable(filter2()) 
  #
  
  #Filtro por plataforma y año, mostrando ventas de cada genero de una plataforma----
  filter3 <- eventReactive(eventExpr = input$show3,
                           valueExpr = {
                             filter(v_pgy_sales, Platform == input$platforms,
                                    Year_of_Release == input$years )
                           }
  )
  output$consulta3 <- renderDataTable(filter3())
  #
  
  #Ventas por genero en un año de cierta plataforma----
  output$distPlot <- renderPlot({
    if(input$ventas == "NA_Sales"){
      ggplot(data = filter3(), aes(x= Genre, y=ventas_NA)) +
        geom_bar(stat = "identity", aes(fill=Genre)) + ggtitle("Venta de Plataformas por año")
    }
    else if(input$ventas == "EU_Sales"){
      ggplot(data = filter3(), aes(x= Genre, y=ventas_EU)) +
        geom_bar(stat = "identity", aes(fill=Genre)) + ggtitle("Venta de Plataformas por año")
                                                                                       
    }
    else if(input$ventas == "JP_Sales"){
      ggplot(data = filter3(), aes(x= Genre, y=ventas_JP)) +
        geom_bar(stat = "identity", aes(fill=Genre))+ ggtitle("Venta de Plataformas por año")    
      }
    else if(input$ventas == "Other_Sales"){
      ggplot(data = filter3(), aes(x= Genre, y=ventas_Other)) +
        geom_bar(stat = "identity", aes(fill=Genre))+ ggtitle("Venta de Plataformas por año")  
    }
    else if(input$ventas == "Global_Sales"){
      ggplot(data = filter3(), aes(x= Genre, y=ventas_G)) +
        geom_bar(stat = "identity", aes(fill=Genre))+ ggtitle("Venta de Plataformas por año")  
    }
    
  })
  
  #filtro para mostrar ventas por año de un genero 
  filter4 <- eventReactive(eventExpr = input$show4,
                           valueExpr = {
                               filter(genre_sales,
                                      Year_of_Release == input$years)
                           })
  output$consulta4 <- renderDataTable(filter4())
  
  #Hacer grafica de ventas de generos en cierto pais por año ----
  output$distPlot2 <- renderPlot({
    if(input$ventas == "NA_Sales"){
      ggplot(data = filter4(), aes(x= Genre, y=ventas_NA)) +
        geom_bar(stat = "identity", aes(fill=Genre)) + ggtitle("Venta de Generos por pais y año")
    }
    else if(input$ventas == "EU_Sales"){
      ggplot(data = filter4(), aes(x= Genre, y=ventas_EU)) +
        geom_bar(stat = "identity", aes(fill=Genre)) + ggtitle("Venta de Generos por pais y año")
    }
    else if(input$ventas == "JP_Sales"){
      ggplot(data = filter4(), aes(x= Genre, y=ventas_JP)) +
        geom_bar(stat = "identity", aes(fill=Genre)) + ggtitle("Venta de Generos por pais y año")
    }
    else if(input$ventas == "Other_Sales"){
      ggplot(data = filter4(), aes(x= Genre, y=ventas_Other)) +
        geom_bar(stat = "identity", aes(fill=Genre)) + ggtitle("Venta de Generos por pais y año")
    }
    else if(input$ventas == "Global_Sales"){
      ggplot(data = filter4(), aes(x= Genre, y=ventas_G)) +
        geom_bar(stat = "identity", aes(fill=Genre)) + ggtitle("Venta de Generos por pais y año")
    }
    
  })

}
shinyApp(ui, server)
