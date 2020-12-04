# I read RDS files relevant to my Shiny app.

approved_adas <- readRDS(file = "data/approved_adas.rds") 
requests <- readRDS(file = "data/requests.rds")
approved_requests <- readRDS(file = "data/approved_requests.rds") 
approved_reasons <- readRDS(file = "data/approved_reasons.rds")
interesting_approved_adas <- readRDS(file = "data/approved_adas.rds") 
supplemented_interesting_ada_transfers_t <- readRDS(file = "data/supplemented_interesting_ada_transfers_t.rds") 
supplemented_interesting_ada_transfers_f <- readRDS(file = "data/supplemented_interesting_ada_transfers_f.rds") 


shinyServer(function(input, output) {
    
    # I create a data table for approved ADA requests - removing the name to
    # anonymize the data.
    
    output$approved_adas <- renderDataTable({
        datatable(approved_adas %>% select(pei, date_received:days_until_accommodation_met), 
                  options = list(pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        ) 
    })
    
    # I create a data table showing the total number of requests, approved
    # requests, and percent approved for each accommodation type.

    output$all_requests_tbl <- renderDataTable({
        datatable(requests, 
                  options = list(dom = 't',
                                 pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        )
    })
    
    # I create a plot showing the total number of requests and approved
    # requests.
    
    output$all_requests_plot <- renderPlot({
        requests %>%
            
            # I pivot my data longer to classify my data by approval status.
            
            pivot_longer(num_approved_requests:num_all_requests, names_to = "approved", values_to = "num_requests") %>%
            
            # I reorder the accommodations on my y-axis according to the
            # number of requests in each category.
            
            ggplot(aes(x = num_requests, y = fct_reorder(accommodation_type, num_requests), fill = approved)) +
            
                # I use a dodged bar graph to display this information.
            
                geom_col(position = "dodge") +
                theme_bw() +
                theme(legend.position = "bottom") +
            
                # I create a more descriptive legend title and labels.
            
                scale_fill_discrete(name = "Category", labels = c("All requests", "Approved requests")) +
                
                # I use shorter accommodation names for a more pleasing graph.
            
                scale_y_discrete(labels = c("Caretaker", "Additional bedrooms", "Wheelchair", "AC Unit", "Cooking facilities", "Physical modification", "Change in regulation/re-housing plan", "Non-carpeted", "First floor/elevator", "Assistance animal",  "Other", "Scattered site/Co-housing", "Service providers")) +
                labs(title = "Number of requests by accommodation type \nand approval status (2015-2019)",
                     subtitle = "Proximity to service providers and scattered site housing \nwere by far the most frequently requested accommodations.",
                     x = "Number of requests", 
                     y = "Accommodation type",
                     caption = "Source: Department of Housing & Community Development")
    })
    
    # I create a data table showing the approved ADA requests
    # broken down by accommodation type.
        
    output$request_types_tbl <- renderDataTable({
        datatable(approved_requests, 
                  options = list(dom = 't',
                                 pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        )
    })
        
    # I create a plot showing approved requests by accommodation type.
    
    output$request_types_plot <- renderPlot({
        
        approved_requests %>%
            
            # I reorder the accommodations on my y-axis according to the
            # percentage of requests in each category.
            
            ggplot(aes(x = percent, y = fct_reorder(accommodation_type, percent))) +
            geom_col() +
            theme_bw() +
            
            # I use shorter accommodation names for a more pleasing graph.
            
            scale_y_discrete(labels = c("Caretaker", "Additional bedrooms", "Change in regulation/re-housing plan", "Wheelchair", "AC Unit", "Cooking facilities", "Physical modification", "Non-carpeted", "Assistance animal", "First floor/elevator",  "Other", "Scattered site/Co-housing", "Service providers")) +
            labs(title = "Percentage of approved ADA requests \nby accommodation type (2015-2019)",
                 subtitle = "Proximity to service providers and scattered site housing \nwere by far the most frequently requested accommodations.",
                 x = "Percentage of approved requests", 
                 y = "Accommodation type",
                 caption = "Source: Department of Housing & Community Development")
    })
    
    # I create a data table showing the approved ADA requests
    # broken down by reason type.
    
    output$reason_types_tbl <- renderDataTable({
        datatable(approved_reasons, 
                  options = list(dom = 't',
                                 pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        )
    })
    
    # I create a plot showing approved requests by accommodation type.
    
    output$reason_types_plot <- renderPlot({
        
        approved_reasons %>%
            
            # I reorder the reasons on my y-axis according to the
            # percentage of requests in each category.
            
            ggplot(aes(x = percent, y = fct_reorder(reason_type, percent))) +
            geom_col() +
            theme_linedraw() +
            
            # I use shorter accommodation names for a more pleasing graph.
            
            scale_y_discrete(labels = c("Other", "Developmental/ \nbehavioral disability", "Emotional health", "Mental health", "Physical health")) +
            labs(title = "Percentage of approved ADA requests \nby reason type (2015-2019)",
                 subtitle = "Physical health and mental health were the most common reasons for an ADA request.",
                 x = "Percentage of approved requests", 
                 y = "Reason type",
                 caption = "Source: Department of Housing & Community Development")
    })
    
    # I create a Kaplan-Meier plot that shows the percentage of accommodations
    # met over time based on the user's inputted conditions.
    
    output$request_time_plot <- renderPlot({ 
        
        # This function creates a Kaplan-Meier plot showing the percentage of 
        # approved ADA requests met over time.
        
        create_kaplan_meier <- function(df){
            
            # I filter the data based on the date range inputted by the user.
            
            df_within_date <- df %>%
                filter(date_received >= input$date_range[1] & date_received <= input$date_range[2])
            
            df_within_date %>%
                
                # I eliminate outlier requests where wait time is negative 
                # since this doesn't make sense.
                
                filter(days_until_accommodation_met >= 0) %>% 
                group_by(days_until_accommodation_met) %>%
                summarize(num_requests_met = n(), .groups = "drop") %>%
                mutate(prop_requests_met = num_requests_met/nrow(df_within_date)) %>%
                
                # I use cumsum() to add up the % of requests met over time.
                
                mutate(cum_prop_requests = cumsum(prop_requests_met) * 100) %>%
                ggplot(aes(x = days_until_accommodation_met, y = cum_prop_requests)) +
                    geom_line() + 
                
                    # I use a red dotted line to indicate the 30-day mark.
                
                    geom_vline(xintercept = 30, color = "red", lty = "dashed") +
                    xlim(0, 400) +
                    ylim(0, 100) +
                    labs(title = "Percentage of approved ADA requests met over time", 
                         subtitle = "Red dotted line represents the 30-day mark.",
                         x = "Days until accommodation was met",
                         y = "Percentage of requests met",
                         caption = "Source: Department of Housing & Community Development")
        }
        
        # I create a tibble without the requests where wait time is negative. 
        
        approved_adas_no_neg <- approved_adas %>%
            filter(days_until_accommodation_met >= 0 | is.na(days_until_accommodation_met))
        
        # I use a series of if statements to create the appropriate 
        # Kaplan-Meier plot based on the user-inputted assumptions.
        # The cases go from most specific to most general.
        
        if(input$interesting_select == TRUE & 
           input$substitute_select == TRUE & 
           input$hotel_select == TRUE) {
            create_kaplan_meier(supplemented_interesting_ada_transfers_t)
        } else {
        if(input$interesting_select == TRUE & 
           input$substitute_select == TRUE & 
           input$hotel_select == FALSE) {
            create_kaplan_meier(supplemented_interesting_ada_transfers_f)
        } else {
        if(input$interesting_select == TRUE &
           input$substitute_select == FALSE & 
           input$hotel_select == FALSE) {
            create_kaplan_meier(interesting_approved_adas)
        } else {
            create_kaplan_meier(approved_adas_no_neg)
        }}}
        
        })
    
    output$stan_model_tbl <- renderDataTable({ 
      
      datatable(stan_glm(paste(input$outcome_select, " ~ ", input$predictor_select),
                         data = supplemented_interesting_ada_transfers_t,
                         refresh = 0,
                         seed = 9) %>%
                  
                # I create a tibble to more easily extract the median values from 
                # stan_glm.
                
                as_tibble() %>%
                rename(mu = `(Intercept)`,
                       predictor = input$predictor_select) %>%
                mutate(mu_median = median(mu)) %>%
                mutate(predictor_median = median(predictor)) %>%
                select(mu_median, predictor_median) %>%
                mutate(mu_median = round(mu_median, 2)) %>%
                mutate(predictor_median = round(predictor_median, 2)) %>%
                slice(1), 
                
                options = list(dom = 't',
                               pageLength = 1,
                               initComplete = JS("function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                 "}"))
      )
      
    })
      
    
    output$stan_text <- renderText({ 
        
        fit_obj <- stan_glm(paste(input$outcome_select, " ~ ", input$predictor_select),
                            data = supplemented_interesting_ada_transfers_t,
                            refresh = 0,
                            seed = 9)
        
        fit_obj_tbl <- as_tibble(fit_obj) %>%
            rename(mu = `(Intercept)`,
                   predictor = input$predictor_select) %>%
            mutate(mu_median = median(mu)) %>%
            mutate(predictor_median = median(predictor))
        
        # I create a tibble to extract the median values from stan_glm.
        
        fit_obj_stats <- fit_obj_tbl %>%  
            select(mu_median, predictor_median) %>%
            slice(1) %>%
            mutate(mu_median = round(mu_median, 2)) %>%
            mutate(predictor_median = round(predictor_median, 2))   
        
        rmse <- tibble(truth = supplemented_interesting_ada_transfers_t$days_until_accommodation_met, forecast = predict(fit_obj)) %>% 
          mutate(sq_diff = (forecast - truth)^2) %>% 
          summarize(rmse = sqrt(mean(sq_diff))) %>%
          mutate(rmse = round(rmse, 2))   
        
        outcome_choices <- c("Days Until Accommodation Met" = "days_until_accommodation_met")
        
        predictor_choices <- c("Scattered Site Placement Unit / Co-housing Unit" = "req_scattered_site_placement_unit_co_housing_unit",
                               "First Floor or Elevator Access" = "req_first_floor_or_elevator_access",
                               "Wheelchair Accessible Placement Unit" = "req_wheelchair_accessible_placement_unit",
                               "Mental Health" = "reason_mental_health",
                               "Emotional Health" = "reason_emotional_health",
                               "Physical Health" = "reason_physical_health",
                               "Developmental Disability/Behavioral Health" = "reason_developmental_disability_behavioral")
        
        paste("To compare whether requests that ask for certain accommodations or list certain reasons have longer or shorter delays in accommodation, I created a linear regression model. I use a Bayesian linear regression model to regress",
              (names(which(outcome_choices == input$outcome_select))),
              "on",
              (names(which(predictor_choices == input$predictor_select))),
              ".",
              "The predicted average wait time for requests without the predictor", 
              (names(which(predictor_choices == input$predictor_select))), 
              "is", 
              fit_obj_stats$mu_median, 
              "days. ",
              "The predicted change in average wait time for requests with the predictor", 
              (names(which(predictor_choices == input$predictor_select))), 
              "is", 
              fit_obj_stats$predictor_median, 
              "days.",
              "The RMSE value for this model is ",
              rmse,
              ".")
    })
  
    
    output$stan_model_plot <- renderPlot({ 
            
        # I use stan_glm to generate my linear regression model.
        
        fit_obj <- stan_glm(paste(input$outcome_select, " ~ ", input$predictor_select),
                            data = supplemented_interesting_ada_transfers_t,
                            refresh = 0,
                            seed = 9)
        
        fit_obj_tbl <- as_tibble(fit_obj) %>%
            rename(mu = `(Intercept)`,
                   predictor = input$predictor_select) %>%
            mutate(mu_median = median(mu)) %>%
            mutate(predictor_median = median(predictor))
        
        # I create a tibble to extract the median values from stan_glm.
        
        fit_obj_stats <- fit_obj_tbl %>%  
            select(mu_median, predictor_median) %>%
            slice(1)
        
        # I add mu to the predictor column so that the posterior now
        # represents the average wait time for individuals without the 
        # specified predictor instead of the average change in wait time.
            
        fit_obj_tbl %>%
            mutate(predictor = predictor + mu) %>%
            pivot_longer(cols = mu:predictor,
                         names_to = "parameter",
                         values_to = "wait_time") %>%
            ggplot(aes(x = wait_time, fill = parameter)) +
                
                # I use overlapping histograms to display my two posterior 
                # distributions.
                
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               alpha = 0.5, 
                               bins = 100, 
                               position = "identity") +
                
                # I create a more descriptive legend title and labels.
                
                scale_fill_discrete(name = "Group", 
                                    labels = c("Without Predictor", 
                                               "With Predictor")) +
                
                # I add two dotted lines to more clearly show the median value for each 
                # posterior distribution.
                
                geom_vline(xintercept = fit_obj_stats$mu_median, 
                           color = "red", 
                           lty = "dashed") +
                geom_vline(xintercept = fit_obj_stats$predictor_median + fit_obj_stats$mu_median, 
                           color = "blue", 
                           lty = "dashed") +
                labs(title = "Posterior probability distributions",
                     subtitle = "Predictive distributions of average wait time for individuals with and without the specified predictor",
                     x = "Average number of days until accommodation met",
                     y = "Probability")
            

    })

})
    
