library(purrr)
library(shiny)
library(shinythemes)
library(tidyverse)

ui <- navbarPage(theme = shinytheme("sandstone"), "New York City Housing Prices",
                 tabPanel("Graphs",
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  selectInput(inputId = "neighborhood",
                                              label = "Borough",
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
                 tabPanel("Statistical Analysis",
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  radioButtons(inputId = "data", 
                                               label = "Data",
                                               choices = list("Median House Value" = 1, "Median Airbnb Price Per Night" = 2), selected = 1)
                              ),
                              
                              mainPanel(
                                  plotOutput("stats_map")
                                  )
                          )
                 ),
                 tabPanel("About",
                          h2("About Airbnb"),
                          p("Airbnb is a San Francisco based company founded in 2008. The company operates through a website 
                            and app that allows users to arrange and offer lodging. Guests can filter to look for different 
                            types of lodging, selecting different types (whole apartment, room in apartment, whole house, etc),
                            dates, prices, and locations. Airbnb allows hosts and guests to communicate before and during the 
                            stay, and after a stay guests can leave ratings about houses/hosts. The company started in San 
                            Francisco but is now worldwide with hosts in cities and towns on every continent using the app. 
                            The company is one of the biggest lodging platforms, with big implications on local hospitality 
                            industries."),
                          br(),
                          p("The data used for this project comes from Inside Airbnb (http://insideairbnb.com/about.html). 
                            Inside Airbnb is a set of tools and data created to help people explore how Airbnb is really 
                            being used around the world. The project is not associated with Airbnb and is a website that 
                            has compiled publicly available information about Airbnb listings in different cities around 
                            the world."),
                          br(),
                          p("This specific data contains lots of information. There is an id code for each listing, a name 
                            (created by the host), host id, host name, latitude, longitude, price per night, minimum nights 
                            for reservation, the number of reviews for the listing, the date of the latest review, the 
                            average number of reviews per month, the number of lists that host has, and the number of days 
                            of the year that the listing is available to rent. There are also neighborhood group and 
                            neighborhood which describe bigger neighborhood groupings and smaller ones. In cities in which 
                            there are not clear larger and smaller neighborhood groups the neighbourhood group column is empty."),
                          br(),
                          h2("About Zillow"),
                          p("Zillow is _____. Zillow data is from September 30th 2019.")
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
            filter(neighbourhood_group == input$neighborhood) %>%
            ungroup() %>%
            slice(switch(input$radio,
                         '1' = 1:n(),
                         '2' = 1:15,
                         '3' = -1:-(n()-15)
                
            ))
        
        ggplot(graph_data, aes(x = neighbourhood, 
                               y = ave_price, 
                               fill = neighbourhood_group)) +
            geom_col() +
            labs(y = "Average Price Per Night", 
                 x = "Neighborhood", 
                 title = paste(input$neighborhood, "Airbnb Prices by Neighborhood", sep = " ")) +
            guides(fill = FALSE) +
            theme_classic() +
            theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
            ylim(0,500)
    })
    
    output$map <- renderPlot({
        
        data <- switch(input$data, 
                       '1' = nyc_shapes_full$zhvi,
                       '2' = nyc_shapes_full$median_ppn)
        
        ggplot() + 
            geom_sf(data = nyc_shapes_full, aes(fill = data)) +
            scale_fill_gradient(low = "wheat1", high = "red")
        
    })
    
    output$stats_map <- renderPlot({
        
        
        ggplot() + 
            geom_sf(data = stats, aes(fill = slope)) +
            scale_fill_gradient(low = "wheat1", high = "red")
        
    })
    
    
}


shinyApp(ui = ui, server = server)

