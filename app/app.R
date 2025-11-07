#################
# Career Survey - Stratification Simulation App
#################

# X:/Arian/Career_Survey/app.R
library(shiny)

# Source existing script

source("Career_Survey_StratDashboard.R", local = TRUE)

shinyApp(ui = ui, server = server)
