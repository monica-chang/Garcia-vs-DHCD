approved_adas <- readRDS(file = "data/approved_adas.rds") 
approved_requests <- readRDS(file = "data/approved_requests.rds") 
approved_reasons <- readRDS(file = "data/approved_reasons.rds")
interesting_approved_adas <- readRDS(file = "data/approved_adas.rds") 
supplemented_interesting_ada_transfers_t <- readRDS(file = "data/supplemented_interesting_ada_transfers_t.rds") 
supplemented_interesting_ada_transfers_f <- readRDS(file = "data/supplemented_interesting_ada_transfers_f.rds") 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$request_types_tbl <- renderTable({approved_requests})
        
    output$request_types_plot <- renderPlot({
        
        # Create a plot showing the requests by accommodation type.
        
        approved_requests %>%
            ggplot(aes(x = percent, y = fct_reorder(accommodation_type, percent))) +
            geom_col() +
            theme_bw() +
            scale_y_discrete(labels = c("Caretaker", "Additional bedrooms", "Change in regulation/re-housing plan", "Wheelchair", "AC Unit", "Cooking facilities", "Physical modification", "Non-carpeted", "Assistance animal", "First floor/elevator",  "Other", "Scattered site/Co-housing", "Service providers")) +
            labs(title = "Proportion of approved ADA requests \nby accommodation type (2015-2019)",
                 subtitle = "Proximity to service providers and scattered site housing \nwere by far the most frequently requested accommodations.",
                 x = "Proportion of approved requests", 
                 y = "Accommodation type",
                 caption = "Source: Department of Housing & Community Development")
    })
    
    output$reason_types_tbl <- renderTable({approved_reasons})
    
    output$reason_types_plot <- renderPlot({
        
        # Create a plot showing the requests by reason type.
        
        approved_reasons %>%
            ggplot(aes(x = percent, y = fct_reorder(reason_type, percent))) +
            geom_col() +
            theme_linedraw() +
            scale_y_discrete(labels = c("Other", "Developmental/ \nbehavioral disability", "Emotional health", "Mental health", "Physical health")) +
            labs(title = "Proportion of approved ADA requests \nby reason type (2015-2019)",
                 subtitle = "Physical health and mental health were the most common reasons for an ADA request.",
                 x = "Proportion of approved requests", 
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
                mutate(cum_prop_requests = cumsum(prop_requests_met)) %>%
                ggplot(aes(x = days_until_accommodation_met, y = cum_prop_requests)) +
                geom_line() + 
                geom_vline(xintercept = 30, color = "red", lty = "dashed") +
                xlim(0, 400) +
                ylim(0, 1) +
                labs(title = "Proportion of approved ADA requests that were met", 
                     x = "Days until accommodation was met",
                     y = "Proportion of requests met",
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
    
    output$stan_model <- renderPrint({ 
        
        fit_obj <- stan_glm(days_until_accommodation_met ~ input$accommodation_select + input$reason_select,
                            data = supplemented_interesting_ada_transfers,
                            refresh = 0)
        
        print(fit_obj, digits = 4, detail = FALSE)
        
    })

})
    
