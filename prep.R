library(janitor)
library(sf)
library(tidyverse)

raw_zillow <- 
  read_csv("http://files.zillowstatic.com/research/public/Neighborhood/Neighborhood_Zhvi_Summary_AllHomes.csv", col_types = 
             cols(
               Date = col_date(format = ""),
               RegionID = col_double(),
               RegionName = col_character(),
               State = col_character(),
               Metro = col_character(),
               County = col_character(),
               City = col_character(),
               SizeRank = col_double(),
               Zhvi = col_double(),
               MoM = col_double(),
               QoQ = col_double(),
               YoY = col_double(),
               `5Year` = col_double(),
               `10Year` = col_double(),
               PeakMonth = col_character(),
               PeakQuarter = col_character(),
               PeakZHVI = col_double(),
               PctFallFromPeak = col_double(),
               LastTimeAtCurrZHVI = col_character()
             )) %>%
  clean_names()

raw_airbnb <-
  read_csv("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-09-12/visualisations/listings.csv", col_types = 
             cols(
               id = col_double(),
               name = col_character(),
               host_id = col_double(),
               host_name = col_character(),
               neighbourhood_group = col_character(),
               neighbourhood = col_character(),
               latitude = col_double(),
               longitude = col_double(),
               room_type = col_character(),
               price = col_double(),
               minimum_nights = col_double(),
               number_of_reviews = col_double(),
               last_review = col_date(format = ""),
               reviews_per_month = col_double(),
               calculated_host_listings_count = col_double(),
               availability_365 = col_double()
             ))


nyc_shapes <- st_read("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-09-12/visualisations/neighbourhoods.geojson")
