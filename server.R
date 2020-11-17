##se cargan las librerias para utilizar sus funciones
library(shiny)
library(readxl)
library(googledrive)
library(googlesheets4)
library(tidyverse)


#shinyServer() sirve para desarrollar todas las funciones de la aplicacion.
shinyServer(function(input, output) {
  
  ##se crean las variables BDiniciativas y BDregiones, luego con read_sheet() se lee el excel desde google drive con el link, sheet es igual a la hoja que se esta cargando.
  BDiniciativas0 <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=0%22",
                               sheet = "iniciativas")
  BDregiones <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=0", 
                           sheet="poblacion potencial")
  BDavances <- read_sheet ("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=1496146068",
                           sheet="actividades y componentes")
  BDbeneficiarios <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=1120700756",
                                sheet = "seguimiento")
  
  
 
  
  ##output$ind... envia al ui.R las cajas de todos de los indicadores que se ven en infobox.
  output$indIniciativasAtrasadasReg <- renderInfoBox({
    infoBox(
      "Iniciativas atrasadas", "12%", icon = icon("fas fa-calendar-times"),
      width = 6, color = "red", fill = TRUE
    )
  })
  output$indIniciativasAtrasadasNac <- renderInfoBox({
    infoBox(
      "Iniciativas atrasadas", "15%", icon = icon("fas fa-calendar-times"),
      width = 6, color = "red", fill = TRUE
    )
  })
  
  numeroIniciativasTotales <- nrow(BDiniciativas0)
  iniciativasSectorPriorizado <- filter(BDiniciativas0, Sector == "Turismo" | Sector == "Agroindustria" | Sector == "Energía" | Sector == "Agroindustria/Industria")
  numeroIniciativasSectPr <- nrow(iniciativasSectorPriorizado)
  indSecPriorizado <- (numeroIniciativasSectPr/numeroIniciativasTotales)*100
  aproximacionSecPriorizado <- round(indSecPriorizado, 2)
  porcentaje <- str_c( aproximacionSecPriorizado, "%")
  output$indSectorPriorizadoReg <- renderInfoBox({
    infoBox(
      "Iniciativas en sector priorizado", porcentaje , icon = icon("fas fa-check-circle"),
      width = 6, color = "green", fill = TRUE
    )
  })
  output$indSectorPriorizadoNac <- renderInfoBox({
    infoBox(
      "Iniciativas en sector priorizado", "76%", icon = icon("fas fa-check-circle"),
      width = 6, color = "green", fill = TRUE
    )
  })
  
  output$indCrecimientoSPReg <- renderInfoBox({
    infoBox(
      "Crecimiento en sectores priorizados", "5%", icon = icon("fas fa-chart-line"),
      width = 6, color = "blue", fill = TRUE
    )
  })
  output$indCrecimientoSPNac <- renderInfoBox({
    infoBox(
      "Crecimiento en sectores priorizados", "2%", icon = icon("fas fa-chart-line"),
      width = 6, color = "blue", fill = TRUE
    )
  })
  
  output$indBeneficiariosReg <- renderInfoBox({
    infoBox(
      "Beneficiarios efectivos cubiertos", "80%", icon = icon("fas fa-users"),
      width = 6, color = "yellow", fill = TRUE
    )
  })
  output$indBeneficiariosNac <- renderInfoBox({
    infoBox(
      "Beneficiarios efectivos cubiertos", "68%", icon = icon("fas fa-users"),
      width = 6, color = "yellow", fill = TRUE
    )
  })
  
  output$indCrecimientoBenefReg <- renderInfoBox({
    infoBox(
      "Crecimiento beneficiarios cubiertos", "3%", icon = icon("fas fa-chart-line"),
      width = 6, color = "orange", fill = TRUE
    )
  })
  output$indCrecimientoBenefNac <- renderInfoBox({
    infoBox(
      "Crecimiento beneficiarios cubiertos", "1%", icon = icon("fas fa-chart-line"),
      width = 6, color = "orange", fill = TRUE
    )
  })
  
  ##con los siguientes 3 output$... se envian al ui.R los gráficos de barra.
  tablaEjecutor <- table(BDiniciativas0$`Tipo Ejecutor`)
  tablaEjecutor <- prop.table(tablaEjecutor)
  output$ejecutorPublicoVsPrivado <- renderPlot({
    barplot(tablaEjecutor,
            xlab = "Tipo de ejecutor", ylab = "Proporción", ylim = c(0, 0.6), 
            col = c("royalblue", "lightblue"))
  })
  
  tablaSector <- table(BDiniciativas0$Sector)
  tablaSector <- prop.table(tablaSector)
  output$iniciativasPorSector <- renderPlot({
    barplot(tablaSector,
            xlab = "", ylab = "Proporción", ylim = c(0, 0.3),
            col = "darkslategray3", las =2, cex.names = 0.7)
  })
  
  Porcentaje<- (BDregiones$PP16 / BDregiones$Población_Total)*100
  output$pPotencial <- renderPlot({
    
    ggplot(data=BDregiones, aes(x= Región, y= Porcentaje, fill= Región)) + 
      geom_bar(stat="identity", position="stack") +
      scale_x_discrete("Regiones") +
      scale_fill_manual(values=c("#CC0000","#FF6633","#CC9900","#996633","#FFFF33","#009900","#66FF66","#336666","#009999","#0033FF","3366FF","#000099","#FF66FF","#660099","#990066"))
    
  })
  
  ##se envia al ui.R la tabla interactiva de iniciativas con el nombre de iniciativax
  output$iniciativax <- renderDataTable(BDiniciativas0)
  
  ##cajitas de indicadores para la seccion del sidebar de indicadores por iniciativa.
  output$indTiempoTranscurrido <- renderInfoBox({
    infoBox(
      "Tiempo presupuestado transcurrido", "55%", icon = icon("fas fa-calendar-alt"),
      width = 4, color = "orange", fill = TRUE
    )
  })
  output$indAvanceActividades <- renderInfoBox({
    infoBox(
      "Avance según actividades", "60%", icon = icon("fas fa-clipboard"),
      width = 4, color = "green", fill = TRUE
    )
  })
  output$indAvanceComponentes <- renderInfoBox({
    infoBox(
      "Avance según componentes", "63%", icon = icon("fas fa-clipboard-list"),
      width = 4, color = "blue", fill = TRUE
    )
  })
  
  output$indBeneficiarios <- renderInfoBox({
    
    
    infoBox(
      "Beneficiarios efectivos cubiertos", "90%", icon = icon("fas fa-users"),
      width = 6, color = "yellow", fill = TRUE
    )
  })
  output$indBeneficiariosEmpresa <- renderInfoBox({
    
    
    infoBox(
      "Beneficiarios empresa cubiertos", "80%", icon = icon("fas fa-industry"),
      width = 6, color = "purple", fill = TRUE
    )
  })
  
  ##se crea la tabla interactiva para la seccion de indicadores por iniciativa.
  BDiniciativas1 <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=0%22", sheet = "actividades y componentes")
  tablaIniciativa1 <- BDiniciativas1[, 3:11]
  output$iniciativa1 <- renderDataTable(tablaIniciativa1)
  
  #2 intento de selección de iniciativas para los indicadores
  output$result <- renderText({
    
    paste("You chose", input$Iniciativas)
  })
})
