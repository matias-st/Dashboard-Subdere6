##Se cargan las librerias para utilizar sus funciones
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(stringr)

##ShinyUI muestra la parte visual de la pagina, y dashboardPage presenta el dashboard
shinyUI(dashboardPage(

    ##dashboardHeader muestra el encabezado del dashboard
    dashboardHeader(title = "Seguimiento técnico FIC-R"),
    
    ##dashboardSidebar muestra el menu lateral de la pagina
    dashboardSidebar(
        sidebarMenu(id = 'sidebarmenu',
        menuItem("Estadisticas Región", tabName = "inicio"),
            menuSubItem("Indicadores por iniciativa", tabName = "iniciativas")

        )
    ),
    
    ##dashboardBody muestra el cuerpo de la pagina
    dashboardBody(
        ## tabItems permite ver cada pagina del menu lateral
        tabItems(
        
            tabItem(tabName = "inicio",
        
            h2("Seguimiento iniciativas FIC-R Region"),
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
                box(title= "Población Potencial respecto del total de habitantes por Región", status= "primary", solidHeader = TRUE,width = 12,
                    plotOutput("pPotencial"))
            
            ),
        
            fluidRow(
                h2("Todas las iniciativas"),
            ),
        
            fluidRow(
                dataTableOutput("iniciativax")
            )
            ),
            
            tabItem(tabName = "iniciativas",
                    
                    h2("Indicadores por iniciativa"),
                    fluidRow(
                        box(selectInput(inputId = "Iniciativas", label = "Iniciativas:", 
                                        choices = unique(BDiniciativas0$`Nombre Proyecto`),
                                        selected= BDiniciativas0$`Nombre Proyecto`[1]
                                        )
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

