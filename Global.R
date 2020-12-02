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
##
##con read_sheet() se lee el excel desde google drive con el link, sheet es igual a la hoja que se esta cargando.
BDiniciativas0 <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=0%22",
                             sheet = "iniciativas")
BDseguimiento <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=1120700756", 
                             sheet = "seguimiento")
BDactividades <- read_sheet("https://docs.google.com/spreadsheets/d/1QkMjIkeZgyCdhZYTHwZai9BsjN2lamvf_8AgwSRS5XI/edit#gid=1120700756", 
                             sheet = "actividades y componentes")
BDnacional <- read_sheet("https://docs.google.com/spreadsheets/d/1IpawWdpO8MUhw3kALmePGO5yipD4yTT1HHBUJ0dAVEA/edit#gid=0", 
                             sheet = "Indicadores Regionales")

numeroIniciativasTotales <- nrow(BDiniciativas0)
añoActual <- max(BDiniciativas0$Año)
añoAnterior <- añoActual - 1