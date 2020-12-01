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
library(rstanarm)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(theme = shinytheme("flatly"), 
        "Garcia vs DHCD",
               tabPanel(
                 "The Problem",
                 p("For my final project, I am working with the Greater Boston
                   Legal Services Housing Unit to provide data analytics support
                   on their class action lawsuit")),
                  
               tabPanel(
                   "ADA Request Overview",
                   
                   # Using subtabs at the top
                   
                   tabsetPanel(
                     
                     # Tab one: Approved requests
                     
                     tabPanel(
                       "Approved requests",
                       h2("Approved requests"),
                       p("Here's an overview of the types of accommodations that were most frequently approved from 08/04/15 - 12/25/19."),
                       fluidRow(style = 'padding:30px;',
                                column(1),
                                column(9,
                                       
                                       # Plot bar graph of accommodation type breakdown.
                                       
                                       plotOutput("all_requests_plot")),
                                column(1)),
                       fluidRow(style = 'padding:30px;',
                                column(2),
                                column(8,
                                       
                                       # Plot bar graph of accommodation type breakdown.
                                       
                                       dataTableOutput("all_requests_tbl")),
                                column(2))
                       ),
                     
                     # Tab two: Accommodation types
                     
                     tabPanel(
                       "Accommodation types",
                       h2("Accommodation types"),
                       p("Here's an overview of the types of accommodations that were most frequently requested in approved ADA requests from 08/04/15 - 12/25/19."),
                       fluidRow(style = 'padding:30px;',
                                column(6,
                                       
                                       # Plot bar graph of accommodation type breakdown.
                                       
                                       plotOutput("request_types_plot")),
                                column(
                                  6,
                                  dataTableOutput("request_types_tbl"))
                       )  
                     ),
                     
                     # Tab three: Reason types
                     
                     tabPanel(
                       "Reason types",
                       h2("Reason types"),
                       p("Here's an overview of the reasons that were most frequently cited for approved ADA requests from 08/04/15 - 12/25/19."),
                       fluidRow(style = 'padding:30px;',
                                column(7,
                                       
                                       # Plot bar graph of accommodation type breakdown.
                                       
                                       plotOutput("reason_types_plot")),
                                column(
                                  5,
                                  dataTableOutput("reason_types_tbl"))
                       ) 
                     )
                   )
               ),
              
              tabPanel(
                "ADA Request Database",
                titlePanel("ADA Request Database"),
                p("Here's a database that shows the date of application, date of decision, and date the accommodation was met for each approved ADA request:"),
                dataTableOutput("approved_adas")),
        
              tabPanel(
                "Delays in DHCD Accommodation",
                titlePanel("Delays in DHCD Accommodation"),
                p("Here's an interactive time plot that shows the proportion of approved ADA requests the DHCD meets over time."),
                   sidebarLayout(
                     
                     sidebarPanel(
                       
                       checkboxInput("interesting_select",
                                     "Only show unit type accommodations",
                                     TRUE),
                       checkboxInput("substitute_select",
                                     "Substitute missing values with transfer data",
                                     TRUE),
                       checkboxInput("hotel_select",
                                     "Assume hotel transfers meet unit type accommodations",
                                     TRUE),
                       dateRangeInput("date_range", "Date range:",
                                      start  = "2015-08-03",
                                      end    = "2019-12-26",
                                      min    = "2015-08-03",
                                      max    = "2019-12-26",
                                      format = "mm/dd/yy",
                                      separator = " - ")
                                ),
                     mainPanel(
                       plotOutput("request_time_plot")
                     )
                   
                )),
                
             tabPanel(
                  "Model",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("accommodation_select",
                                  "Choose a specific accommodation type:",
                                  choices = c("Scattered Site Placement Unit / Co-housing Unit" = "req_scattered_site_placement_unit_co_housing_unit",
                                              "First Floor or Elevator Access" = "req_first_floor_or_elevator_access",
                                              "Wheelchair Accessible Placement Unit" = "req_wheelchair_accessible_placement_unit"),
                                  selected = "Scattered Site Placement Unit / Co-housing Unit"),
                      selectInput("reason_select",
                                  "Choose a specific reason type:",
                                  choices = c("Mental Health" = "reason_mental_health",
                                              "Emotional Health" = "reason_emotional_health",
                                              "Physical Health" = "reason_physical_health",
                                              "Developmental Disability/Behavioral Health" = "reason_developmental_disability_behavioral"),
                                  selected = "Mental Health")
                    ),
                    mainPanel(
                      plotOutput("stan_model_plot")
                    )),
                  fluidRow(
                    dataTableOutput("stan_model_tbl")
                  )
             
            ),
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
                 p(a("Here is a link to my Github repo!", href = "https://github.com/monica-chang/Garcia-vs-DHCD")))
        
    ))