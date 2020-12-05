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


shinyUI(
    navbarPage(theme = shinytheme("flatly"), 
        "Garcia vs. DHCD",
        
        # I create a tab to give an overview of the lawsuit and the
        # motivation for my final project. I also display the plot of 
        # what percentage of approved ADA requests are met over time. 
        # The user is able to select the data assumptions they wish to make.
        
        tabPanel(
            "Delays in Disability Accommodation",
            h3("The Problem: Justice Delayed is Justice Denied"),
            p("The Emergency Assistance (EA) program, run by the Department of Housing and Community Development (DHCD), is a shelter system for families and pregnant women experiencing homelessness in Massachusetts. Garcia vs. DHCD is an ongoing class action lawsuit that was first filed in 2016 by Greater Boston Legal Services, ACLU, and Ropes & Gray on behalf of homeless families. The lawsuit claims DHCD violates the Americans with Disabilities Act (ADA) by failing to promptly accommodate the the disabilities of homeless individuals."),
            p("In 2018, the Supreme Judicial Court concluded that more information was necessary to determine whether DHCD unreasonably delays implementing ADA accommodations it approves, and whether these delays are a result of DHCD's policy of transferring EA participants with disabilities only when ", tags$q("administratively feasible.")),
            h3("The Question"),
            p(strong(em("What percentage of approved disability requests are actually met? How long does it take DHCD to implement these approved accommodations? Is this happening within a reasonable timeframe? Do certain types of accommodations result in longer delays?"))),
            p("The goal of this project is to answer these questions using data on ADA requests (individual applications for disability accommodations), transfers within the shelter system, and characteristics of various shelter locations. This data was provided by the DHCD and ranges from 2015-2019."),
            p("Here is a plot that shows the percentage of approved ADA requests that are met over time based on certain data assumptions (see below for further explanation):"),
            br(),
            
            # I use the sidebarLayout to allows the user to input certain 
            # assumptions and view the corresponding plot.
            
            sidebarLayout(
              
              sidebarPanel(
                
                # The user can check certain data assumptions and input a 
                # date range.
                
                checkboxInput("interesting_select",
                              "Only show unit type accommodations",
                              TRUE),
                checkboxInput("substitute_select",
                              "Substitute missing values with transfer data",
                              TRUE),
                checkboxInput("hotel_select",
                              "Assume hotel transfers meet unit type accommodations",
                              TRUE),
                
                # The default date range is set to the earliest and latest 
                # date in the dataset.
                
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
              )),
            
            # I explain the data assumptions in more detail so that the 
            # user understands the choices provided in the sidebar more 
            # clearly.
            
            h3("Explaining Our Data Assumptions"),
            p("Unfortunately, the data provided by DHCD are insufficient to directly answer the above question. 70% of values are missing for the date each approved request was met. Given that I have no information about if/when/how approved accommodations were met 70% of the time, I make several reasonable assumptions to deduce this information:"),
            p(strong("Only show unit type accommodations: "),
              "Since I have information about where and when individuals were transferred within the EA Shelter system, I am able to deduce the date an accommodation was met for accommodations that ",
              em("require an internal or external transfer "),
              "to be met. This refers to the following accommodations: proximity to service providers, scattered site/co-housing, first floor/elevator access, wheelchair accessibility. Because I currently lack information about the zipcode of service providers, I look only at the following unit-type accommodations: scattered site/co-housing, first floor/elevator access, wheelchair accessibility."),
            p(strong("Substitute missing values with transfer: "),
              "For requests that include a unit type accommodation, I use data on shelter characteristics to determine if a transfer that occurred after the initial request met the disability need. If a transfer meets the need, I substitute missing values for the date an accommodation was met with the date of transfer."),
            p(strong("Assume hotel transfers meet unit type accommodations: "),
              "Since I have no information on hotel characteristics, I make the assumption that a hotel meets all unit type accommodations."),
            br()
          ),
        
        # I create a tab to display my linear regression model for 
        # predicting wait time.
        
        tabPanel(
          "Modelling Delays",
          h2("Modelling Delays in Accommodation"),
          br(),
          
          # I use the sidebarLayout to allow the user to input a predictor
          # and outcome and view the corresponding posterior distributions.
          
          sidebarLayout(
            sidebarPanel(
              selectInput("predictor_select",
                          "Choose a predictor:",
                          choices = c("Scattered Site Placement Unit / Co-housing Unit" = "req_scattered_site_placement_unit_co_housing_unit",
                                      "First Floor or Elevator Access" = "req_first_floor_or_elevator_access",
                                      "Wheelchair Accessible Placement Unit" = "req_wheelchair_accessible_placement_unit",
                                      "Mental Health" = "reason_mental_health",
                                      "Emotional Health" = "reason_emotional_health",
                                      "Physical Health" = "reason_physical_health",
                                      "Developmental Disability/Behavioral Health" = "reason_developmental_disability_behavioral"),
                          selected = "Scattered Site Placement Unit / Co-housing Unit"),
              selectInput("outcome_select",
                          "Choose an outcome:",
                          choices = c("Days Until Accommodation Met" = "days_until_accommodation_met"),
                          selected = "Days Until Accommodation Met")
            ),
            mainPanel(
              plotOutput("stan_model_plot")
            )),
          
          # I use fluidRow and padding to allow for a more flexible display.
          
          fluidRow(style = 'padding:30px;',
                   column(2,
                          dataTableOutput("stan_model_tbl")
                   ),
                   column(2),
                   
                   # I discuss my model creation and interpretation and major
                   # caveats to provide the user with enough information to
                   # understand my regression model.
                   
                   column(8,
                          h3("Model Creation & Interpretation"),
                          textOutput("stan_text"),
                          h3("Major Caveats"),
                          p("For this model, I use 2015-2019 data only containing requests with unit type accommodations, substitute missing values with transfer data, and assume hotel transfers meet unit type accommodations. As a result, I am only using 606/2108 (28.7%) of all approved ADA requests."),
                          p("Proximity to service providers is by far the most frequently requested accommodation (44.4% of all approved requests). However, because I currently lack information about the zipcode of service providers, I am unable to account for this group in my analysis."),
                          p("This model has a high RMSE value - meaning that there is a large range of uncertainty about the true median value. Considering our small sample size, incomplete information, and the multitude of factors that go into determining any particular individual's disability accommodation wait time, this is unsurprising.")
                   ) 
          )
          
        ),
      
    
          # I create a tab to give an overview of the ADA requests, 
          # explain the various accommodations that can be requested, and
          # provide summary graphs/tables.
        
          tabPanel(
               "ADA Request Overview",
               
               # I using subtabs at the top.
               
               tabsetPanel(
                 
                 # Tab one: Approved requests
                 
                 tabPanel(
                   "Approved requests",
                   h2("Approved requests"),
                   
                   # I use fluidRow and padding to allow for a more flexible display.
                   
                   p("Here is an overview of the types of accommodations that were most frequently approved from 08/04/15 - 12/25/19."),
                   fluidRow(style = 'padding:30px;',
                            column(1),
                            column(9,
                                   
                                   # Plot dodged bar graph of all requests and 
                                   # approved requests.
                                   
                                   plotOutput("all_requests_plot")),
                            column(1)),
                   fluidRow(style = 'padding:30px;',
                            column(2),
                            column(8,
                                   
                               # Plot table of all requests and approved requests.
                               
                               dataTableOutput("all_requests_tbl")),
                            column(2))
                   ),
                 
                 # TODO: Elaborate on accommodation types.
                 
                 # Tab two: Accommodation types
                 
                 tabPanel(
                   "Accommodation types",
                   h2("Accommodation types"),
                   p("Here is an overview of the types of accommodations that were most frequently requested in approved ADA requests from 08/04/15 - 12/25/19."),
                   
                   # I use fluidRow and padding to allow for a more flexible display.
                   
                   fluidRow(style = 'padding:30px;',
                            column(6,
                                   
                               # Plot bar graph of accommodation type breakdown.
                               
                               plotOutput("request_types_plot")),
                            column(6,
                                   
                               # Plot table of accommodation type breakdown. 
                               
                               dataTableOutput("request_types_tbl"))
                   )  
                 ),
                 
                 # Tab three: Reason types
                 
                 tabPanel(
                   "Reason types",
                   h2("Reason types"),
                   p("Here is an overview of the reasons that were most frequently cited for approved ADA requests from 08/04/15 - 12/25/19."),
                   
                   # I use fluidRow and padding to allow for a more flexible display.
                   
                   fluidRow(style = 'padding:30px;',
                            column(7,
                                   
                               # Plot bar graph of reason type breakdown.
                               
                               plotOutput("reason_types_plot")),
                            column(5,
                              
                              # Plot table of reason type breakdown. 
                                   
                              dataTableOutput("reason_types_tbl"))
                   ) 
                 )
               )
           ),
          
          # I create a tab to allow the GBLS team to search up any
          # individual by PEI and find their date of application, 
          # date of decision, and date the accommodation was met.
      
          tabPanel(
            "ADA Request Database",
            titlePanel("ADA Request Database"),
            p("Here is a database that shows the date of application, date of decision, and date the accommodation was met for each approved ADA request:"),
            dataTableOutput("approved_adas")),
      
          # I create a tab to provide some more information about me,
          # the project, and my data sources.
          
          tabPanel("About", 
               fluidRow(style = 'padding:30px;',
                        column(5,
                               h3("About Me"),
                               p("My name is Monica Chang, and I'm currently a sophomore at Harvard College. I plan on concentrating in Social Studies with a secondary in Computer Science. I'm interested in understanding the causes and consequences of societal inequality. I'm especially interested in housing justice, neighborhood effects, and upward mobility in urban America. You can email me at monica_chang@college.harvard.edu, check out my ",
                                 a(href = "https://github.com/monica-chang", "Github account"), 
                                 ", or connect with me on ",
                                 a(href = "https://www.linkedin.com/in/monica-yang-chang", "LinkedIn"), 
                                 ".")
                       ), 
                       column(1),
                       column(6,  
                              h3("About the Project"),
                              p("When is justice delayed justice denied? This fall, I worked with the Greater Boston Legal Services Housing Unit to provide support on their class action lawsuit against the Massachusetts Department of Housing and Community Development (DHCD). When a homeless family with disability needs makes a request for accommodation and the request is approved, the DHCD promises to implement the disability accommodation when ",
                              tags$q("administratively feasible."),
                              "In my project, I investigate whether the DHCD violates the Americans with Disabilities Act by failing to accommodate the disabilities of homeless individuals within a reasonable timeframe. You can check out the original code for this project at my ",
                              a(href = "https://github.com/monica-chang/Garcia-vs-DHCD", "Github repository"),
                              "."), 
                              br(),
                              h3("About the Data"),
                              p("I am using data provided by the MA Department of Housing and Community Development from 2015-2019. My analysis relies primarily upon 3 datasets:"),
                                p(strong("ADA Requests: "),
                                "This dataset contains information about the disability accommodations requested and the reasons for each individual application."),
                                p(strong("Transfers: "),
                                "This dataset contains information about external transfers (transfers from one shelter to another) and internal transfers (within one shelter) for every individual in the EA shelter system."),
                                p(strong("Shelter characteristics: "), 
                                "This dataset contains information about the characteristics of all the shelters within the EA system.")
                       )
                )
        )
          
  ))