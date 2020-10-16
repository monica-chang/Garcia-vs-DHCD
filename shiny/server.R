ada_request_2018_2020 <- readRDS(file = "data/ada_request_2018_2020.rds") 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$request_types_plot <- renderPlot({

        request_types <- ada_request_2018_2020 %>%
            pivot_longer(cols = req_a_anim:req_a_oth, names_to = "accomodation_type", values_to = "requests") %>%
            group_by(accomodation_type) %>%
            summarize(total_requests = sum(requests), .groups = "drop") %>%
            arrange(desc(total_requests)) %>%
            mutate(pct_requests = total_requests/sum(total_requests))
        
        # Create a plot showing the requests by accommodation type.
        
        request_types %>%
            ggplot(aes(x = pct_requests, y = fct_reorder(accomodation_type, pct_requests))) +
            geom_col() +
            theme_linedraw() +
            scale_y_discrete(labels = c("Caretaker", "Wheelchair", "Cooking facilities", "Physical modification", "Change in regulation", "Non-carpeted", "First floor/elevator", "Assistance animal", "Other", "Scattered site", "Service providers")) +
            labs(title = "Percentage of ADA requests by accomodation type (2015-2019)",
                 x = "Percentage of requests", 
                 y = "Accomodation type",
                 caption = "Source: Department of Housing & Community Development")

    })

})
