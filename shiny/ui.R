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
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("request_types_plot"), plotOutput("reason_types_plot"))
                   ),
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("accommodation_select",
                                   "Choose a specific accommodation type:",
                                   choices = c("Placement Unit Close to Service Providers"= "req_placement_unit_close_to_service_providers",
                                               "Scattered Site Placement Unit / Co-housing Unit" = "req_scattered_site_placement_unit_co_housing_unit",
                                               "Assistance Animal within Placement Unit" = "req_assistance_animal_within_placement_unit",
                                               "First Floor or Elevator Access" = "req_first_floor_or_elevator_access",
                                               "Non Carpeted Placement Unit" = "req_non_carpeted_placement_unit",
                                               "Change in EA Regulation / Re-Housing Plan" = "req_change_in_ea_regulation_re_housing_plan",
                                               "Physical Modification to Placement Unit" = "req_physical_modification_to_placement_unit",
                                               "Access to Full Cooking Facilities" = "req_access_to_full_cooking_facilities",
                                               "Wheelchair Accessible Placement Unit" = "req_wheelchair_accessible_placement_unit",
                                               "Assigned Caretaker / Temporary Non-EA Household Member" = "req_assigned_caretaker_temporary_non_ea_household_member",
                                               "Additional bedrooms" = "req_additional_bedrooms",
                                               "AC Unit" = "req_ac_unit",
                                               "Other" = "req_other"),
                                   selected = "Placement Unit Close to Service Providers",
                                   multiple = TRUE)
                     ),
                     
                     mainPanel(
                       plotOutput("request_time_plot")
                  )
                   
               )
    )))