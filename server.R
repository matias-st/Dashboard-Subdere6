#shinyServer() sirve para desarrollar todas las funciones de la aplicacion.
shinyServer(function(input, output) {
    
  BDiniciativas0 <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=0",
                               sheet = "iniciativas")
  BDregiones <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=1288196927", 
                           sheet="poblacion potencial")
  BDseguimiento <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=1120700756",
                              sheet = "seguimiento")
  BDavances <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=1496146068",
                          sheet = "actividades y componentes")
    
    
    #funciona al colocar un nombre concreto
   
   
    
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
    porcentajeSectPrio <- str_c( aproximacionSecPriorizado, "%")
    output$indSectorPriorizadoReg <- renderInfoBox({
        infoBox(
            "Iniciativas en sector priorizado", porcentajeSectPrio , icon = icon("fas fa-check-circle"),
            width = 6, color = "green", fill = TRUE
        )
    })
    
    output$indSectorPriorizadoNac <- renderInfoBox({
        infoBox(
            "Iniciativas en sector priorizado", "76%", icon = icon("fas fa-check-circle"),
            width = 6, color = "green", fill = TRUE
        )
    })
    
    añoActual <- max(BDiniciativas0$Año)
    añoAnterior <- añoActual - 1
    iniciativasSectPrioAñoActual <- filter(BDiniciativas0, Año == añoActual & Sector == "Turismo" | Sector == "Agroindustria" | Sector == "Energía" | Sector == "Agroindustria/Industria")
    numeroIniciativasSectPrAñoActual <- nrow(iniciativasSectPrioAñoActual)
    iniciativasSectPrioAñoAnterior <- filter(BDiniciativas0, Año == añoAnterior & Sector == "Turismo" | Sector == "Agroindustria" | Sector == "Energía" | Sector == "Agroindustria/Industria")
    numeroIniciativasSectPrAñoAnterior <- nrow(iniciativasSectPrioAñoAnterior)
    totalIniciativasAñoAnterior <- filter(BDiniciativas0, Año == añoAnterior)
    numeroIniciativasAñoAnterior <- nrow(totalIniciativasAñoAnterior)
    
    indCrecSecPriorizado <- ((numeroIniciativasSectPrAñoActual-numeroIniciativasSectPrAñoAnterior)/numeroIniciativasAñoAnterior)*100
    aproximacionCrecSecPriorizado <- round(indCrecSecPriorizado, 2)
    porcentajeCrecSectPrio <- str_c( aproximacionCrecSecPriorizado, "%")
    output$indCrecimientoSPReg <- renderInfoBox({
        infoBox(
            "Crecimiento en sectores priorizados", porcentajeCrecSectPrio, icon = icon("fas fa-chart-line"),
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
      x <- input$Iniciativas
      fechasIniciativa <- filter(BDseguimiento, str_detect (BDseguimiento$`Nombre Proyecto`, x))
      View(fechasIniciativa)
      ddIniciativa <- select(fechasIniciativa, "Nombre Proyecto", "Fecha comienzo", "Fecha entrega")
      View(ddIniciativa)
      #mostrar solo 1 de los2 en caso de que exista mas de 1 // estos debe modificarse despues con el filtro del año
      ddIniciativa2 <- ddIniciativa[1,]
      View(ddIniciativa2)
      fechaCom <- select(ddIniciativa2, "Fecha comienzo")
      View(fechaCom)
      fechaEnt <- select(ddIniciativa2, "Fecha entrega")
      View(fechaEnt)
      tpoTotal <- (fechaEnt- fechaCom)
      View(tpoTotal)
      fechaActual <- as.Date(Sys.Date())
      View(fechaActual)
      ##este calculo no lo hace
      #Métodos incompatibles ("-.Date", "Ops.data.frame") para "-"
      #Warning: Error in -: argumento no-numérico para operador binario 
      tpoAvanzado <- (fechaActual - fechaCom)
      View(tpoAvanzado)
      
      indTpoTrans <- ((tpoAvanzado / tpoTotal)*100)
      indTpoTrans2 <- round(indTpoTrans)
      indTpoTrans3 <- str_c(indTpoTrans2, "%")
      
        infoBox(
            "Tiempo presupuestado transcurrido", indTpoTrans3, icon = icon("fas fa-calendar-alt"),
            width = 4, color = "orange", fill = TRUE
        )
    })

 

    #para actividades atrasadas
   
    
    output$indAvanceActividades <- renderInfoBox({
      ##Fórmula de indicador avance actividades
      #Obtengo los datos de las iniciativas
      x <- input$Iniciativas
      activ <- filter(BDavances, str_detect (BDavances$`Nombre Proyecto`, x))
      
      #elimino duplicados en caso de que existan
      activ <- activ[!duplicated(activ),]
      
      #para actividades realizadas
      #selecciono el estado de las actividades
      estadoAct <- select(activ, "Estado act")
      
      numActTotales <- nrow(estadoAct)
      
      actRealizadas <- filter(estadoAct, str_detect(activ$`Estado act`, "entregado") == TRUE)
      
      numActRealizadas <- nrow(actRealizadas)
      
      
      indAvanceAct <- ((numActRealizadas/numActTotales)*100)
      indAvanceAct <- round(indAvanceAct, 2)
      indAvanceAct <- str_c(indAvanceAct, "%")
        infoBox(
            "Avance según actividades", indAvanceAct, icon = icon("fas fa-clipboard"),
            width = 4, color = "green", fill = TRUE
        )
    })
    output$indAvanceComponentes <- renderInfoBox({
      ##Fórmula de indicador comp actividades
      #Obtengo los datos de las iniciativas
      x <- input$Iniciativas
      componentes <- filter(BDavances, str_detect (BDavances$`Nombre Proyecto`, x) == TRUE)
      
      #elimino duplicados en caso de que existan
      componentes <- componentes[!duplicated(componentes),]
      
      #para componentes realizados
      #selecciono el estado de los componentes
      estadoComp <- select(componentes, "Componente", "Estado comp")
      estadoComp2 <- distinct(estadoComp)
      estadoComp3 <- select(estadoComp2, "Estado comp")
      numCompTotales <- nrow(estadoComp2)
      
      compRealizados <- filter(estadoComp3, str_detect(estadoComp3$`Estado comp` , "entregado") == TRUE)
      numCompRealizados <- nrow(compRealizados)
      
      indAvanceComp <- ((numCompRealizados/numCompTotales)*100)
      indAvanceComp2 <- round(indAvanceComp, 2)
      indAvanceComp3 <- str_c(indAvanceComp2, "%")

        infoBox(
            "Avance según componentes", indAvanceComp3, icon = icon("fas fa-clipboard-list"),
            width = 4, color = "blue", fill = TRUE
        )
    })
    
  
    #intento de indicador beneficiarios
    output$indBeneficiarios <- renderInfoBox({
      x <- input$Iniciativas
      
      benefIniciativas <- filter (BDseguimiento, str_detect (BDseguimiento$`Nombre Proyecto`, x) == TRUE) 
      
      benefObj <- select(benefIniciativas, "beneficiariosObjetivos")
      benefEfe <- select(benefIniciativas, "beneficiariosEfectivos")
      
      indBeneficiarios <- ((benefEfe/benefObj)*100)
      indBeneficiarios <- round(indBeneficiarios,2)
      indBeneficiarios <- str_c(indBeneficiarios, "%")
      
      infoBox("Beneficiarios efectivos cubiertos",indBeneficiarios, icon = icon("fas fa-clipboard"),
              width = 4, color = "green", fill = TRUE
      )
    })
   
    
  
  
    output$indBeneficiariosEmpresa <- renderInfoBox({
        infoBox(
            "Beneficiarios empresa cubiertos", "14%", icon = icon("fas fa-industry"),
            width = 6, color = "purple", fill = TRUE
        )
    })
    
##se crea la tabla interactiva para la seccion de indicadores por iniciativa.
    BDiniciativas1 <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=0%22", sheet = "actividades y componentes")
    tablaIniciativa1 <- BDiniciativas1[, 3:11]
    output$iniciativa1 <- renderDataTable(tablaIniciativa1)
    
    
    
})
