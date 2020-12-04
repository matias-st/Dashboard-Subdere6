##ee cargan las librerias para utilizar sus funciones
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(stringr)

##con read_sheet() se lee el excel desde google drive con el link, sheet es igual a la hoja que se esta cargando
BDiniciativas <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI",
                             sheet = "iniciativas")
BDseguimiento <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI", 
                             sheet = "seguimiento")
BDactividades <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI", 
                             sheet = "actividades y componentes")
BDnacional <- read_sheet("https://docs.google.com/spreadsheets/d/1IpawWdpO8MUhw3kALmePGO5yipD4yTT1HHBUJ0dAVEA", 
                             sheet = "Indicadores Regionales")

##con nrow() se cuenta el numero total de iniciativas
numeroIniciativasTotales <- nrow(BDiniciativas)
##con max() se obtiene el año mas alto de las iniciativas
añoActual <- max(BDiniciativas$Año)
##en añoAnterior se guarda el año anterior al año actual
añoAnterior <- añoActual - 1
