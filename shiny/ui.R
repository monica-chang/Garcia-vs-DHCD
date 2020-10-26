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
               tabPanel("About", 
                   p("For my final project, I am working with the Greater Boston
                   Legal Services Housing Unit to provide data analytics support
                   on their class action lawsuit",
                     em("(Garcia vs. DHCD)"), 
                     "against the MA Emergency Assistance Shelter system.",
                     "I am analyzing data specifically related to the lawsuit claim under the Americans With Disabilities Act.",
                     "Many families that request a disability-related accommodation are told that their request will be met when administratively feasible.",
                     "However, this can take months.", 
                     "I am cleaning and merging data provided by the MA Department of Housing and Community Development to analyze how long these delays take, and whether certain types of accommodation requests result in longer delays.",
                     "I am merging data for accommodation requests, shelter tranfers, shelter characteristics, and exit information to answer these questions.",
                     "I am also merging data across time since GBLS received one batch of data in 2018 and another batch in 2020."),
                   p("So far, I have cleaned and merged the ADA request data from 2018 and 2020. The bar graph under the Data tab shows the percentages of ADA requests by accommodation type."),
                   p(a("Here is a link to my Github repo!", href = "https://github.com/monica-chang/Garcia-vs-DHCD"))),
               
               tabPanel(
                   "Data",
                   plotOutput("request_types_plot"),
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("accommodation_select",
                                   "Choose a specific accommodation type:",
                                   choices = c("Placement Unit Close to Service Providers"= "Placement Unit Close to Service Providers",
                                               "Scattered Site Placement Unit / Co-housing Unit" = "Scattered Site Placement Unit / Co-housing Unit",
                                               "Assistance Animal within Placement Unit" = "Assistance Animal within Placement Unit",
                                               "First Floor or Elevator Access" = "First Floor or Elevator Access",
                                               "Non Carpeted Placement Unit" = "Non Carpeted Placement Unit",
                                               "Change in EA Regulation / Re-Housing Plan" = "Change in EA Regulation / Re-Housing Plan",
                                               "Physical Modification to Placement Unit" = "Physical Modification to Placement Unit",
                                               "Access to Full Cooking Facilities" = "Access to Full Cooking Facilities",
                                               "Wheelchair Accessible Placement Unit" = "Wheelchair Accessible Placement Unit",
                                               "Assigned Caretaker / Temporary Non-EA Household Member" = "Assigned Caretaker / Temporary Non-EA Household Member",
                                               "Additional bedrooms" = "Additional bedrooms",
                                               "AC Unit" = "AC Unit",
                                               "Other" = "Other"),
                                   selected = "Placement Unit Close to Service Providers",
                                   multiple = TRUE)
                     ),
                     
                     mainPanel(
                       plotOutput("request_time_plot")
                  )
                   
               )
    )))