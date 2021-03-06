##ShinyUI muestra la parte visual de la pagina, y dashboardPage presenta el dashboard
shinyUI(dashboardPage(
    
    ##dashboardHeader muestra el encabezado del dashboard
    dashboardHeader(title = "Seguimiento técnico"),
    
    ##dashboardSidebar muestra el menu lateral de la pagina
    dashboardSidebar(div ( style  =  ' overflow-y: scroll ' ),
                     sidebarMenu(id = 'sidebarmenu', 
                                 menuItem("Estadisticas Región", tabName = "estadisticasRegion", icon = icon("fas fa-user-circle")),
                                 menuSubItem("Indicadores por iniciativa",tabName = "indicadoresPorIniciativas", icon = icon("fas fa-user"))
                                 
                     ),
                     
                     br(),
                     
                     
                     selectInput(
                         inputId =  "añoGlobal", 
                         label = "Seleccione el año de los indicadores:", 
                         choices = 2016:añoActual,
                         selected = añoActual
                     ),
                     br(),
                     br(),
                     br(),
                     
                     tags$div(class="accesoBD", checked=NA,
                              tags$p(HTML( "&nbsp;&nbsp;&nbsp;¿Quieres acceder a la BD?")),
                              tags$a(href="https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=0",
                                     HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Base de Datos"))
                     ),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br()
                     #HTML('<center><img src="subdere.jpg" height="120" width="120" ></center>')
                     
    ),
    
    ##dashboardBody muestra el cuerpo de la pagina
    dashboardBody(
        ## tabItems permite ver cada pagina del menu lateral
        tabItems(
            
            tabItem(tabName = "estadisticasRegion",
                    
                    h2("Seguimiento iniciativas FIC-R Región"),
                    fluidRow(
                        box(h2("Regional")),
                        box(h2("Nacional"))
                    ),
                    
                    fluidRow(
                        sliderInput("critAtraso", "Seleccione el % crítico de atraso:",
                                    min = 0, max = 100, value = 20
                        ),
                        fluidRow(
                            infoBoxOutput("indIniciativasAtrasadasReg", width = 6),
                            infoBoxOutput("indIniciativasAtrasadasNac", width = 6)),
                        fluidRow(
                            infoBoxOutput("indSectorPriorizadoReg", width = 6),
                            infoBoxOutput("indSectorPriorizadoNac", width = 6)),
                        fluidRow(
                            infoBoxOutput("indCrecimientoSPReg", width = 6),
                            infoBoxOutput("indCrecimientoSPNac", width = 6)),
                        fluidRow(
                            infoBoxOutput("indBeneficiariosReg", width = 6),
                            infoBoxOutput("indBeneficiariosNac", width = 6)),
                        fluidRow(
                            infoBoxOutput("indCrecimientoBenefReg", width = 6),
                            infoBoxOutput("indCrecimientoBenefNac", width = 6))
                    ),
                    
                    fluidRow(
                        h2("Gráficos de seguimiento")
                    ),
                    
                    
                    
                    
                    fluidRow(
                        
                        box( radioButtons("varSeleccionada", h4("Seleccione el gráfico que desea visualizar:"),
                                          choices = list("Destino Apuntado" = 1, "Sector Priorizado" = 2, "Tipo de ejecutor" = 3),
                                          selected = 1)
                        ),
                        fluidRow(
                            box(title= "Frecuencia de la variable seleccionada", status= "primary", solidHeader = TRUE,width = 12,
                                plotOutput("varX"))
                        )
                    ),
                    
                    fluidRow(
                        h2("Todas las iniciativas")
                    ),
                    
                    fluidRow(
                        div ( style  =  ' overflow-x: scroll ' , dataTableOutput("tablaIniciativa"))
                    )
            ),
            
            tabItem(tabName = "indicadoresPorIniciativas",
                    
                    h2("Indicadores por iniciativa"),
                    fluidRow(
                        box(
                            
                            uiOutput("selecAño")
                            
                        )
                    ),
                    
                    fluidRow(
                        infoBoxOutput("fechInicio", width = 6),
                        infoBoxOutput("fechEntrega", width = 6),
                        infoBoxOutput("indAvanceActividades", width = 4),
                        infoBoxOutput("indAvanceComponentes", width = 4),
                        infoBoxOutput("indBeneficiarios", width = 4)
                        
                    ),
                    
                    fluidRow(
                        div ( style  =  ' overflow-x: scroll ' , dataTableOutput("iniciativa1"))
                    )
                    
            )
            
        )
    )
)
)