# qPCR Analysis Tool - Main application entry point
library(shiny)

if (Sys.info()["sysname"] == "Darwin") {
  quartzFonts(chinese = c("STHeiti Light", "STHeiti", "Hiragino Sans GB", "PingFang SC"))
} else if (Sys.info()["sysname"] == "Windows") {
  windowsFonts(chinese = windowsFont("SimSun"))
}


source("ui.R")
source("server.R")

shinyApp(ui = shinyUI, server = shinyServer)