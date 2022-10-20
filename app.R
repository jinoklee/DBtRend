library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(semantic.dashboard)
library(shiny.semantic)
library(shinySignals) 
#devtools::install_github("hadley/shinySignals")
library(readr)  
library(htmlwidgets) 
library(shinyWidgets)
library(stringr)
library(heatmaply)

# Sys.setlocale('LC_ALL','C')
# set working directory
setwd("/data11/jinoklee/trend")

source("app/ui.R")
source("app/server.R")

shinyApp(ui = ui, server = server)

#runApp("app")
#################################################################################
# update R :2020.06
# error message : Warning: Error in Cairo: Graphics API version mismatch
# solve : re-inatll cairo package # install.packages("Cairo")
