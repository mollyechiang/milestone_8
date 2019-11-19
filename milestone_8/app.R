library(purrr)
library(shiny)
library(tidyverse)

ui <- navbarPage("New York City Housing Prices",
                 tabPanel("Graphs",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput(inputId = "neighborhood",
                                              label = "Neighborhood",
                                              choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
                                  
                                  radioButtons(inputId = "radio", 
                                               label = "Featured Neighborhoods",
                                               choices = list("All" = 1, "Most Expensive" = 2, "Least Expensive" = 3), selected = 1)
                              ),
                              
                              mainPanel(
                                  plotOutput("graph")
                              )
                          )
                 ),
                 tabPanel("Map",
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  radioButtons(inputId = "data", 
                                               label = "Data",
                                               choices = list("Median House Value" = 1, "Median Airbnb Price Per Night" = 2), selected = 1)
                              ),
                              
                              mainPanel(
                                  plotOutput("map")
                              )
                          )
                 ),
                 tabPanel("About",
                          h2("About The Data"),
                          p("The data for this animated map is from The Stanford Open Policing Project. The project is a free national repository aiming to standardize and centralize data on police and pedestrian interactions across the country."),
                          h2("About the Graphic"),
                          p("For this graphic, I specifically analyzed data on speeding violations (only violations at least 20 mph over the posted limit) from Oklahoma City, Oklahoma between Jan 1, 2011 and Sept 13, 2017.
                           Each point on the graph represents the location of someone who was speeding at least 20 mph over the speed limit. The size of the dot denotes how many miles per
                           hour above the speed limit they were driving and the color of the dot denotes the gender of the speeding driver.")
                 ))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$graph<- renderPlot({
       
         graph_data <- ny_data %>% 
            group_by(neighbourhood) %>%
            mutate(ave_price = ave(price)) %>%
            group_by(neighbourhood, neighbourhood_group, ave_price) %>%
            count() %>%
            filter(n>5) %>% 
            arrange(desc(ave_price)) %>%
            filter(neighbourhood_group == input$neighborhood)
        
        ggplot(graph_data, aes(x= neighbourhood, 
                               y= ave_price, 
                               fill = neighbourhood_group)) +
            geom_col() +
            labs(y = "Average Price Per Night", 
                 x = "Neighborhood", 
                 title = paste(input$neighborhood, "Airbnb Prices by Neighborhood", sep = " ")) +
            guides(fill = FALSE) +
            theme_classic() +
            theme(axis.text.x = element_text(angle=65, vjust=0.6))
    })
    
    output$map <- renderPlot({
    
        data <- switch(input$data, 
                       '1' = nyc_shapes_full$zhvi,
                       '2' = nyc_shapes_full$median_ppn)
        
        ggplot() + 
            geom_sf(data = nyc_shapes_full, aes(fill = data)) +
            scale_fill_gradient(low = "wheat1", high = "red")
    
    })
    
    
}


shinyApp(ui = ui, server = server)
