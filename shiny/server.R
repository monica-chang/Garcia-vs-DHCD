approved_adas <- readRDS(file = "data/approved_adas.rds") 
supplemented_interesting_ada_transfers <- readRDS(file = "data/supplemented_interesting_ada_transfers.rds") 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$request_types_plot <- renderPlot({

        approved_requests <- approved_adas %>%
            pivot_longer(cols = req_placement_unit_close_to_service_providers:req_assigned_caretaker_temporary_non_ea_household_member, 
                         names_to = "accommodation_type", 
                         values_to = "requests") %>%
            group_by(accommodation_type) %>%
            summarize(total_requests = sum(requests), .groups = "drop") %>%
            arrange(desc(total_requests)) %>%
            mutate(pct_requests = total_requests/sum(total_requests))
        
        # Create a plot showing the requests by accommodation type.
        
        approved_requests %>%
            ggplot(aes(x = pct_requests, y = fct_reorder(accommodation_type, pct_requests))) +
            geom_col() +
            theme_bw() +
            scale_y_discrete(labels = c("Caretaker", "Additional bedrooms", "Change in regulation/re-housing plan", "Wheelchair", "AC Unit", "Cooking facilities", "Physical modification", "Non-carpeted", "Assistance animal", "First floor/elevator",  "Other", "Scattered site/Co-housing", "Service providers")) +
            labs(title = "Percentage of approved ADA requests \nby accommodation type (2015-2019)",
                 subtitle = "Proximity to service providers and scattered site housing \nwere by far the most frequently requested accommodations.",
                 x = "Percentage of approved requests", 
                 y = "Accommodation type",
                 caption = "Source: Department of Housing & Community Development")
        
        
    })
    
    output$reason_types_plot <- renderPlot({
        
        # Create a plot showing the requests by reason type.
        
        approved_reasons %>%
            ggplot(aes(x = pct_requests, y = fct_reorder(reason_type, pct_requests))) +
            geom_col() +
            theme_linedraw() +
            scale_y_discrete(labels = c("Other", "Developmental/ \nbehavioral disability", "Emotional health", "Mental health", "Physical health")) +
            labs(title = "Percentage of approved ADA requests \nby reason type (2015-2019)",
                 x = "Percentage of approved requests", 
                 y = "Reason type",
                 caption = "Source: Department of Housing & Community Development")
    })
    
    output$request_time_plot <- renderPlot({ 
        
        approved_adas %>%
            filter(days_until_accommodation_met >= 0) %>% 
            group_by(days_until_accommodation_met) %>%
            summarize(num_requests_met = n(), .groups = "drop") %>%
            mutate(prop_requests_met = num_requests_met/nrow(approved_adas)) %>%
            mutate(cum_prop_requests = cumsum(prop_requests_met)) %>%
            ggplot(aes(x = days_until_accommodation_met, y = cum_prop_requests)) +
            geom_line() + 
            xlim(0, 100) +
            ylim(0, 1) +
            labs(title = "Proportion of approved ADA requests that were met (2015-2019)", 
                 x = "Days until accommodation was met",
                 y = "Proportion of requests met")
        })
    
    output$stan_model <- renderPrint({ 
        
        fit_obj <- stan_glm(days_until_accommodation_met ~ input$accommodation_select + input$reason_select,
                            data = supplemented_interesting_ada_transfers,
                            refresh = 0)
        
        print(fit_obj, digits = 4, detail = FALSE)
        
    })

})
    
