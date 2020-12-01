approved_adas <- readRDS(file = "data/approved_adas.rds") 
requests <- readRDS(file = "data/requests.rds")
approved_requests <- readRDS(file = "data/approved_requests.rds") 
approved_reasons <- readRDS(file = "data/approved_reasons.rds")
interesting_approved_adas <- readRDS(file = "data/approved_adas.rds") 
supplemented_interesting_ada_transfers_t <- readRDS(file = "data/supplemented_interesting_ada_transfers_t.rds") 
supplemented_interesting_ada_transfers_f <- readRDS(file = "data/supplemented_interesting_ada_transfers_f.rds") 

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
    
    output$approved_adas <- renderDataTable({
        datatable(approved_adas %>% select(pei:days_until_accommodation_met), 
                  options = list(pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        ) 
    })

    output$all_requests_tbl <- renderDataTable({
        datatable(requests, 
                  options = list(dom = 't',
                                 pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        )
    })
    
    output$all_requests_plot <- renderPlot({
        requests %>%
            pivot_longer(num_approved_requests:num_all_requests, names_to = "approved", values_to = "num_requests") %>%
            ggplot(aes(x = num_requests, y = fct_reorder(accommodation_type, num_requests), fill = approved)) +
            geom_col(position = "dodge") +
            theme_bw() +
            theme(legend.position = "bottom") +
            scale_fill_discrete(name = "Category", labels = c("All requests", "Approved requests")) +
            scale_y_discrete(labels = c("Caretaker", "Additional bedrooms", "Wheelchair", "AC Unit", "Cooking facilities", "Physical modification", "Change in regulation/re-housing plan", "Non-carpeted", "First floor/elevator", "Assistance animal",  "Other", "Scattered site/Co-housing", "Service providers")) +
            labs(title = "Number of requests by accommodation type \nand approval status (2015-2019)",
                 subtitle = "Proximity to service providers and scattered site housing \nwere by far the most frequently requested accommodations.",
                 x = "Number of requests", 
                 y = "Accommodation type",
                 caption = "Source: Department of Housing & Community Development")
    })
        
    output$request_types_tbl <- renderDataTable({
        datatable(approved_requests, 
                  options = list(dom = 't',
                                 pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        )
    })
        
    output$request_types_plot <- renderPlot({
        
        # Create a plot showing the requests by accommodation type.
        
        approved_requests %>%
            ggplot(aes(x = percent, y = fct_reorder(accommodation_type, percent))) +
            geom_col() +
            theme_bw() +
            scale_y_discrete(labels = c("Caretaker", "Additional bedrooms", "Change in regulation/re-housing plan", "Wheelchair", "AC Unit", "Cooking facilities", "Physical modification", "Non-carpeted", "Assistance animal", "First floor/elevator",  "Other", "Scattered site/Co-housing", "Service providers")) +
            labs(title = "Percentage of approved ADA requests \nby accommodation type (2015-2019)",
                 subtitle = "Proximity to service providers and scattered site housing \nwere by far the most frequently requested accommodations.",
                 x = "Percentage of approved requests", 
                 y = "Accommodation type",
                 caption = "Source: Department of Housing & Community Development")
    })
    
    output$reason_types_tbl <- renderDataTable({
        datatable(approved_reasons, 
                  options = list(dom = 't',
                                 pageLength = 15,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}"))
        )
    })
    
    output$reason_types_plot <- renderPlot({
        
        # Create a plot showing the requests by reason type.
        
        approved_reasons %>%
            ggplot(aes(x = percent, y = fct_reorder(reason_type, percent))) +
            geom_col() +
            theme_linedraw() +
            scale_y_discrete(labels = c("Other", "Developmental/ \nbehavioral disability", "Emotional health", "Mental health", "Physical health")) +
            labs(title = "Percentage of approved ADA requests \nby reason type (2015-2019)",
                 subtitle = "Physical health and mental health were the most common reasons for an ADA request.",
                 x = "Percentage of approved requests", 
                 y = "Reason type",
                 caption = "Source: Department of Housing & Community Development")
    })
    
    output$request_time_plot <- renderPlot({ 
        
        create_kaplan_meier <- function(df){
            df_within_date <- df %>%
                filter(date_received >= input$date_range[1] & date_received <= input$date_range[2])
            
            df_within_date %>%
                filter(days_until_accommodation_met >= 0) %>% 
                group_by(days_until_accommodation_met) %>%
                summarize(num_requests_met = n(), .groups = "drop") %>%
                mutate(prop_requests_met = num_requests_met/nrow(df_within_date)) %>%
                mutate(cum_prop_requests = cumsum(prop_requests_met) * 100) %>%
                ggplot(aes(x = days_until_accommodation_met, y = cum_prop_requests)) +
                geom_line() + 
                geom_vline(xintercept = 30, color = "red", lty = "dashed") +
                xlim(0, 400) +
                ylim(0, 100) +
                labs(title = "Percentage of approved ADA requests that were met", 
                     subtitle = "Red dotted line represents the 30-day mark.",
                     x = "Days until accommodation was met",
                     y = "Percentage of requests met",
                     caption = "Source: Department of Housing & Community Development")
        }
        
        approved_adas_no_neg <- approved_adas %>%
            filter(days_until_accommodation_met >= 0 | is.na(days_until_accommodation_met))
        
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
                }
            }
        }
        
        })
    
    output$stan_model_tbl <- renderDataTable({ 
       
         reactive({
            stan_glm(reformulate(input$predictor_select, input$outcome_select),
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
            slice(1)
            
         })
        
    })
        
    output$stan_model_plot <- renderPlot({ 
        
        reactive({
            stan_glm(reformulate(input$predictor_select, input$outcome_select),
                     data = supplemented_interesting_ada_transfers_t,
                     refresh = 0,
                     seed = 9) %>%
            as_tibble() %>%
            rename(mu = `(Intercept)`,
                   predictor = input$predictor_select) %>%
            select(mu, predictor) %>%
                
            # I add mu to the predictor column so that the posterior now
            # represents the average wait time for individuals without the 
            # specified predictor instead of the average change in wait time.
                
            mutate(predictor = predictor + mu) %>%
            pivot_longer(cols = mu:predictor,
                         names_to = "parameter",
                         values_to = "wait_time") %>%
            ggplot(aes(x = wait_time, color = parameter)) +
                
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

})
    
