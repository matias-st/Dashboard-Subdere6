##ShinyUI muestra la parte visual de la pagina, y dashboardPage presenta el dashboard
shinyUI(dashboardPage(
    
    ##dashboardHeader muestra el encabezado del dashboard
    dashboardHeader(title = "Seguimiento técnico FIC-R"),
    
    ##dashboardSidebar muestra el menu lateral de la pagina
    dashboardSidebar(
        sidebarMenu(id = 'sidebarmenu',
                    menuItem("Estadisticas Región", tabName = "inicio"),
                    menuSubItem("Indicadores por iniciativa",tabName = "iniciativas")
                    
        ),
        #prueba de sidebarpanel
        
            selectInput(
                inputId =  "añoGlobal", 
                label = "Seleccione el año de los indicadores:", 
                choices = 2016:as.numeric(format(Sys.Date(),"%Y")),
                selected = 2016
            )
            
        
    ),
    
    ##dashboardBody muestra el cuerpo de la pagina
    dashboardBody(
        ## tabItems permite ver cada pagina del menu lateral
        tabItems(
            
            tabItem(tabName = "inicio",
                    
                    h2("Seguimiento iniciativas FIC-R Región"),
                    fluidRow(
                        box(h2("Regional")),
                        box(h2("Nacional")),
                    ),
                   
                    fluidRow(
                        infoBoxOutput("indIniciativasAtrasadasReg", width = 6),
                        infoBoxOutput("indIniciativasAtrasadasNac", width = 6),
                        infoBoxOutput("indSectorPriorizadoReg", width = 6),
                        infoBoxOutput("indSectorPriorizadoNac", width = 6),
                        infoBoxOutput("indCrecimientoSPReg", width = 6),
                        infoBoxOutput("indCrecimientoSPNac", width = 6),
                        infoBoxOutput("indBeneficiariosReg", width = 6),
                        infoBoxOutput("indBeneficiariosNac", width = 6),
                        infoBoxOutput("indCrecimientoBenefReg", width = 6),
                        infoBoxOutput("indCrecimientoBenefNac", width = 6)
                    ),
                    
                    fluidRow(
                        h2("Graficos"),
                    ),
                    
                    fluidRow(
                        box(title = "Ejecutor público vs privado", status = "primary", solidHeader = TRUE,
                            plotOutput("ejecutorPublicoVsPrivado")),
                        box(title = "Iniciativas por sector", status = "primary", solidHeader = TRUE,
                            plotOutput("iniciativasPorSector"))
                    ),
                    
                    "\n",
                    
                    fluidRow(
                        
                        box( radioButtons("varSeleccionada", h4("Seleccione la variable X del gráfico"),
                                          choices = list("Destino" = 1, "Sector" = 2),
                                          selected = 1),
                        ),
                        box(title= "Frecuencia de la variable seleccionada", status= "primary", solidHeader = TRUE,width = 12,
                            plotOutput("varX"))
                        
                    ),
                    
                    fluidRow(
                        h2("Todas las iniciativas"),
                    ),
                    
                    fluidRow(
                        dataTableOutput("tablaIniciativa")
                    )
            ),
            
            tabItem(tabName = "iniciativas",
                    
                    h2("Indicadores por iniciativa"),
                    fluidRow(
                        box(
                            #agrego el data input
                            selectInput(
                                inputId =  "Año", 
                                label = "Seleccione el año de las iniciativas:", 
                                choices = 2016:as.numeric(format(Sys.Date(),"%Y"))
                                
                            ),
                            uiOutput("selecAño")
                            
                            
                        )
                    ),
                    
                    fluidRow(
                        infoBoxOutput("indTiempoTranscurrido", width = 4),
                        infoBoxOutput("indAvanceActividades", width = 4),
                        infoBoxOutput("indAvanceComponentes", width = 4),
                        infoBoxOutput("indBeneficiarios", width = 6),
                        infoBoxOutput("indBeneficiariosEmpresa", width = 6)
                    ),
                    
                    fluidRow(
                        dataTableOutput("iniciativa1")
                    )
                    
            )
            
        )
    )
))

