approved_adas <- readRDS(file = "data/approved_adas.rds") 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$request_types_plot <- renderPlot({

        approved_request_types <- approved_adas %>%
            group_by(request_type) %>%
            summarize(total_requests = n(), .groups = "drop") %>%
            arrange(desc(total_requests)) %>%
            mutate(pct_requests = total_requests/sum(total_requests))
        
        # Create a plot showing the requests by accommodation type.
        
        approved_request_types %>%
            ggplot(aes(x = pct_requests, y = fct_reorder(request_type, pct_requests))) +
            geom_col() +
            theme_linedraw() +
            scale_y_discrete(labels = c("Additional bedrooms", "Caretaker", "Change in regulation", "AC Unit", "Wheelchair",  "Cooking facilities", "Physical modification", "Non-carpeted", "Assistance animal", "First floor/elevator",  "Other", "Scattered site/Co-housing", "Service providers")) +
            labs(title = "Percentage of approved ADA requests \nby accomodation type (2015-2019)",
                 subtitle = "Proximity to service providers and scattered site housing \nwere by far the most frequently requested accommodations.",
                 x = "Percentage of requests", 
                 y = "Accomodation type",
                 caption = "Source: Department of Housing & Community Development")
    })
    output$request_time_plot <- renderPlot({ 
        
        approved_adas %>%
            filter(days_until_accommodation_met >= 0) %>% 
            filter(request_type == input$accommodation_select) %>%
            group_by(days_until_accommodation_met) %>%
            summarize(num_requests_met = n(), .groups = "drop") %>%
            mutate(prop_requests_met = num_requests_met/nrow(approved_adas)) %>%
            mutate(cum_prop_requests = cumsum(prop_requests_met)) %>%
            ggplot(aes(x = days_until_accommodation_met, y = cum_prop_requests)) +
            geom_line() + 
            xlim(0, 100) +
            labs(title = "Proportion of approved ADA requests that were met (2015-2019)", 
                 x = "Days until accommodation was met",
                 y = "Proportion of requests met")
        })

})
    
