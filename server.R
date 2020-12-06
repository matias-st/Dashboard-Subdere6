#shinyServer() sirve para desarrollar todas las funciones de la aplicacion.
shinyServer(function(input, output) {
  
  ##Indicadores que se ven en forma de infobox.
  
  output$indIniciativasAtrasadasReg <- renderInfoBox({
    añoGlob <- input$añoGlobal
    iniciativasAtrasadas <- filter(BDactividades, actAtrasada == "atrasado")
    iniciativasAtrasadas <- filter (iniciativasAtrasadas, str_detect (iniciativasAtrasadas$Año, añoGlob) == TRUE)
    numeroIniciativasAtrasadas <- nrow(iniciativasAtrasadas)
    indIniciativasAtrasadas <- (numeroIniciativasAtrasadas/numeroIniciativasTotales)*100
    aproximacionInicAtras <- round(indIniciativasAtrasadas, 2)
    porcentajeInicAtras <- str_c( aproximacionInicAtras, "%")
    infoBox(
      "Iniciativas atrasadas", porcentajeInicAtras, icon = icon("fas fa-calendar-times"),
      width = 6, color = "red", fill = TRUE
    )
  })
  output$indIniciativasAtrasadasNac <- renderInfoBox({
    # añoGlob <- input$añoGlobal
    # View(añoGlob)
    # iniciativasAtrasadasNac <- filter (BDactividades, str_detect (BDactividades$Año, añoGlob) == TRUE)
    # View(iniciativasAtrasadasNac)
    indIniciativasAtrasadasNacional <- mean(BDnacional$'Iniciativas atrasadas')
    aproximacionInicAtrasNac <- round(indIniciativasAtrasadasNacional, 2)
    porcentajeInicAtrasNac <- str_c( aproximacionInicAtrasNac, "%")
    infoBox(
      "Iniciativas atrasadas", porcentajeInicAtrasNac, icon = icon("fas fa-calendar-times"),
      width = 6, color = "red", fill = TRUE
    )
  })
  output$indSectorPriorizadoReg <- renderInfoBox({
    añoGlob <- input$añoGlobal
    iniciativasSectorPriorizado <- filter(BDiniciativas, Sector == "Turismo" | Sector == "Agroindustria" | Sector == "Energía" | Sector == "Agroindustria/Industria")
    iniciativasSectorPriorizado <- filter (iniciativasSectorPriorizado, str_detect (iniciativasSectorPriorizado$Año, añoGlob) == TRUE)
    numeroIniciativasSectPr <- nrow(iniciativasSectorPriorizado)
    indSecPriorizado <- (numeroIniciativasSectPr/numeroIniciativasTotales)*100
    aproximacionSecPriorizado <- round(indSecPriorizado, 2)
    porcentajeSectPrio <- str_c( aproximacionSecPriorizado, "%")
    infoBox(
      "Iniciativas en sector priorizado", porcentajeSectPrio , icon = icon("fas fa-check-circle"),
      width = 6, color = "green", fill = TRUE
    )
  })
  output$indSectorPriorizadoNac <- renderInfoBox({
    indSecPriorizadoNac <- mean(BDnacional$'Iniciativas en sector priorizado')
    aproximacionSecPriorizadoNac <- round(indSecPriorizadoNac, 2)
    porcentajeSectPrioNac <- str_c( aproximacionSecPriorizadoNac, "%")
    infoBox(
      "Iniciativas en sector priorizado", porcentajeSectPrioNac, icon = icon("fas fa-check-circle"),
      width = 6, color = "green", fill = TRUE
    )
  })
  
  #este probablemente se pueda trabajar con el añoGlobal
  output$indCrecimientoSPReg <- renderInfoBox({
    iniciativasSectPrioAñoActual <- filter(BDiniciativas, Año == añoActual & Sector == "Turismo" | Sector == "Agroindustria" | Sector == "Energía" | Sector == "Agroindustria/Industria")
    numeroIniciativasSectPrAñoActual <- nrow(iniciativasSectPrioAñoActual)
    iniciativasSectPrioAñoAnterior <- filter(BDiniciativas, Año == añoAnterior & Sector == "Turismo" | Sector == "Agroindustria" | Sector == "Energía" | Sector == "Agroindustria/Industria")
    numeroIniciativasSectPrAñoAnterior <- nrow(iniciativasSectPrioAñoAnterior)
    totalIniciativasAñoAnterior <- filter(BDiniciativas, Año == añoAnterior)
    numeroIniciativasAñoAnterior <- nrow(totalIniciativasAñoAnterior)
    indCrecSecPriorizado <- ((numeroIniciativasSectPrAñoActual-numeroIniciativasSectPrAñoAnterior)/numeroIniciativasAñoAnterior)*100
    aproximacionCrecSecPriorizado <- round(indCrecSecPriorizado, 2)
    porcentajeCrecSectPrio <- str_c( aproximacionCrecSecPriorizado, "%")
    infoBox(
      "Crecimiento en sectores priorizados", porcentajeCrecSectPrio, icon = icon("fas fa-chart-line"),
      width = 6, color = "blue", fill = TRUE
    )
  })
  output$indCrecimientoSPNac <- renderInfoBox({
    indCrecSecPriorizadoNac <- mean(BDnacional$'Crecimiento en sectores priorizados')
    aproximacionCrecSecPriorizadoNac <- round(indCrecSecPriorizadoNac, 2)
    porcentajeCrecSectPrioNac <- str_c( aproximacionCrecSecPriorizadoNac, "%")
    infoBox(
      "Crecimiento en sectores priorizados", porcentajeCrecSectPrioNac, icon = icon("fas fa-chart-line"),
      width = 6, color = "blue", fill = TRUE
    )
  })
  #no reconoce el benefAño
    output$indBeneficiariosReg <- renderInfoBox({
    añoGlob <- input$añoGlobal
    benefAño <- filter (BDseguimiento, str_detect (BDseguimiento$Año, añoGlob) == TRUE)
    
    #corrección del colSums
    sumaBenefObjetivos <- colSums (benefAño[ , 6, drop = FALSE])
    sumaBenefEfectivos <- colSums (benefAño[ , 7, drop = FALSE])
    indBenefEfectivos <- (sumaBenefEfectivos/sumaBenefObjetivos)*100
    aproximacionBenefEfectivos <- round(indBenefEfectivos, 2)
    porcentajeBenefEfectivos <- str_c( aproximacionBenefEfectivos, "%")
    infoBox(
      "Beneficiarios efectivos cubiertos", porcentajeBenefEfectivos, icon = icon("fas fa-users"),
      width = 6, color = "yellow", fill = TRUE
    )
  })
  output$indBeneficiariosNac <- renderInfoBox({
    indBenefEfectivosNac <- mean(BDnacional$'beneficiarios efectivos cubiertos')
    aproximacionBenefEfectivosNac <- round(indBenefEfectivosNac, 2)
    porcentajeBenefEfectivosNac <- str_c( aproximacionBenefEfectivosNac, "%")
    infoBox(
      "Beneficiarios efectivos cubiertos", porcentajeBenefEfectivosNac, icon = icon("fas fa-users"),
      width = 6, color = "yellow", fill = TRUE
    )
  })
  #este también podría trabajarse con el añoGlobal
  output$indCrecimientoBenefReg <- renderInfoBox({
    iniciativasAñoActual <- filter(BDseguimiento, Año == añoActual)
    iniciativasAñoAnterior <- filter(BDseguimiento, Año == añoAnterior)
    sumaBenefEfectivosAñoActual <- colSums (iniciativasAñoActual[ , 7])
    sumaBenefEfectivosAñoAnterior <- colSums (iniciativasAñoAnterior[ , 7])
    indCrecBeneficiarios <- ((sumaBenefEfectivosAñoActual-sumaBenefEfectivosAñoAnterior)/sumaBenefEfectivosAñoAnterior)*100
    aproximacionCrecBeneficiarios <- round(indCrecBeneficiarios, 2)
    porcentajeCrecBeneficiarios <- str_c( aproximacionCrecBeneficiarios, "%")
    infoBox(
      "Crecimiento beneficiarios cubiertos", porcentajeCrecBeneficiarios, icon = icon("fas fa-chart-line"),
      width = 6, color = "orange", fill = TRUE
    )
  })
  output$indCrecimientoBenefNac <- renderInfoBox({
    indCrecBeneficiariosNac <- mean(BDnacional$'Crecimiento beneficiarios cubiertos')
    aproximacionCrecBeneficiariosNac <- round(indCrecBeneficiariosNac, 2)
    porcentajeCrecBeneficiariosNac <- str_c( aproximacionCrecBeneficiariosNac, "%")
    infoBox(
      "Crecimiento beneficiarios cubiertos", porcentajeCrecBeneficiariosNac, icon = icon("fas fa-chart-line"),
      width = 6, color = "orange", fill = TRUE
    )
  })
  
  ##con los siguientes 3 output$... se envian al ui.R los gráficos de barra.
  
  output$ejecutorPublicoVsPrivado <- renderPlot({
    tablaEjecutor <- BDiniciativas[,3]
    ggplot(tablaEjecutor, aes(x = reorder(TipoEjecutor, -table(TipoEjecutor)[TipoEjecutor]), fill = TipoEjecutor)) + 
      geom_bar() +
      
      #facet_wrap(~Año, nrow = 1) +
      scale_x_discrete("Tipo Ejecutor") +     
      scale_y_continuous("Frecuencia") +
      coord_flip()
  })
  output$iniciativasPorSector <- renderPlot({
    tablaSector <- BDiniciativas[,8]
    ggplot(tablaSector, aes(x = reorder(Sector, -table(Sector)[Sector]), fill = Sector)) + 
      geom_bar() +
      
      #facet_wrap(~Año, nrow = 1) +
      scale_x_discrete("Sector") +     
      scale_y_continuous("Frecuencia") +
      coord_flip()
  })

  output$varX <- renderPlot({
    
    variableX <-input$varSeleccionada
    añoGlob <- input$añoGlobal
    añoGraf <- filter (BDiniciativas, str_detect (BDiniciativas$Año, añoGlob) == TRUE)
  
    if(variableX == 1){
      
      ggplot(añoGraf, aes(x = reorder(Destino, -table(Destino)[Destino]), fill = Destino)) + 
        geom_bar() +
        
        #facet_wrap(~Año, nrow = 1) +
        scale_x_discrete("Destinos") +     
        scale_y_continuous("Frecuencia") +
        coord_flip()
      
    } else {
      
      ggplot(añoGraf, aes(x = reorder(Sector, -table(Sector)[Sector]), fill = Sector)) + 
        geom_bar() +
        scale_x_discrete("Sectores") +    
        scale_y_continuous("Frecuencia") +
        coord_flip()
    }
    
    
  })
  
  ##se envia al ui.R la tabla interactiva de iniciativas con el nombre de iniciativax
  output$tablaIniciativa <- renderDataTable(BDiniciativas)
  
  
  output$selecAño <- renderUI({
    añoGlob <- input$añoGlobal
    selectInput(inputId = "Iniciativas", label = "Iniciativas:", 
                choices = unique(BDiniciativas[BDiniciativas$`Año`== añoGlob, "Nombre Proyecto"])
                
    )
  })
  
  ##cajitas de indicadores para la seccion del sidebar de indicadores por iniciativa.
  output$indTiempoTranscurrido <- renderInfoBox({
    x <- input$Iniciativas
    y <- input$añoGlobal
    activ <- filter(BDseguimiento, str_detect (BDseguimiento$`Nombre Proyecto`, x))
    activ <- filter(activ, str_detect (activ$`Año`, y) == TRUE)
    activ <- select(activ, "Fecha entrega")
    infoBox(
      "Fecha entrega iniciativa", activ, icon = icon("fas fa-calendar-alt"),
      width = 4, color = "orange", fill = TRUE
    )
  })
  output$indAvanceActividades <- renderInfoBox({
    ##Fórmula de indicador avance actividades
    #Obtengo los datos de las iniciativas
    x <- input$Iniciativas
    y <- input$añoGlobal
    activ <- filter(BDactividades, str_detect (BDactividades$`Nombre Proyecto`, x))
    activ <- filter (activ, str_detect (activ$`Año`, y) == TRUE)
    #elimino duplicados en caso de que existan
    activ <- activ[!duplicated(activ),]
    ##para actividades realizadas
    ##selecciono el estado de las actividades
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
    y <- input$añoGlobal
    componentes <- filter(BDactividades, str_detect (BDactividades$`Nombre Proyecto`, x) == TRUE)
    coponentes <- filter (componentes, str_detect (componentes$`Año`, y) == TRUE)
    
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
  output$indBeneficiarios <- renderInfoBox({
    x <- input$Iniciativas
    y <- input$añoGlobal
    
    benefIniciativas <- filter (BDseguimiento, str_detect (BDseguimiento$`Nombre Proyecto`, x) == TRUE)
    benefIniciativas <- filter (benefIniciativas, str_detect (benefIniciativas$`Año`, y) == TRUE)
    
    benefObj <- select(benefIniciativas, "beneficiariosObjetivos")
    benefEfe <- select(benefIniciativas, "beneficiariosEfectivos")
    
    indBeneficiarios <- ((benefEfe/benefObj)*100)
    indBeneficiarios <- round(indBeneficiarios,2)
    indBeneficiarios <- str_c(indBeneficiarios, "%")
    
    infoBox("Beneficiarios efectivos cubiertos",indBeneficiarios, icon = icon("fas fa-users"),
            width = 4, color = "yellow", fill = TRUE
    )
  })
  output$indBeneficiariosEmpresa <- renderInfoBox({
    infoBox(
      "Beneficiarios empresa cubiertos", "14%", icon = icon("fas fa-industry"),
      width = 6, color = "purple", fill = TRUE
    )
  })
  
  ##se crea la tabla interactiva para la seccion de indicadores por iniciativa.
  tablaIniciativa1 <- BDactividades[, 3:11]
  output$iniciativa1 <- renderDataTable(tablaIniciativa1)
  
})

