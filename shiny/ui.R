#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(theme = shinytheme("flatly"), 
        "Garcia vs DHCD",
               tabPanel(
                   "Disability-related claim"),
               # You would add your content within the parentheses above.
               tabPanel(
                   "Immediate placement claim"
               )
    ))