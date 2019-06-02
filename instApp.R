##############################################
# Shiny app using html template from colorlib
# UI for the shiny app
#
# Author: Sam Parrott
# Date: 4 May 2019
##############################################

library(shiny)
library(leaflet)
library(sp)
library(tidyverse)
library(htmltools)
library(xts)
require(httr)
require(rjson)
require(RCurl)
library(instaR)
library(rworldmap)

# #####Get data from instagram API
# app_name <- "ShinyApp"
# client_id <- "cd886638948746f7a68d729d28eede56"
# client_secret <- "807e480d23ee46e497cdbf63b291774f"
# scope = "basic"
# instagram <- oauth_endpoint(
#   authorize = "https://api.instagram.com/oauth/authorize",
#   access = "https://api.instagram.com/oauth/access_token")
# myapp <- oauth_app(app_name, client_id, client_secret)
# #authenticate access
# ig_oauth <- oauth2.0_token(instagram, myapp,scope=scope,  type = "application/x-www-form-urlencoded",cache=FALSE)
# tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
# token <- tmp[[1]][4]

# token <- instaOAuth(client_id, client_secret)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points) {  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  # return the ADMIN names of each country
  indices$ADMIN  
}

token <- "1406202384.cd88663.ca32b81f941e4d809ba7fa8d3827d4b4"
#token <- read_file(file = ".credentials/Insta_token") %>% 
#         str_replace("\n", "")

owner_info_end_point = paste('https://api.instagram.com/v1/users/self/?access_token=',token,sep="")
data <- getURL(owner_info_end_point)
user_info <- fromJSON(data)
received_profile <- user_info$data[[1]]
user_id <- user_info$data$id
user_name <- user_info$data$username
media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',user_id,'/media/recent/?access_token=',token,sep="")))

df = data.frame(no = 1:length(media$data$id))

for(i in 1:length(media$data$location$name))
{
  #comments
  df$comments[i] <-media$data$comments$count[[i]]
  #likes:
  df$likes[i] <- media$data$likes$count[[i]]
  #location long
  df$longitude[i] <- media$data$location$longitude[[i]]
  #location lat
  df$latitude[i] <- media$data$location$latitude[[i]]
  #location name
  df$location_name[i] <- media$data$location$name[[i]]
  #Country
  location <- as.character(coords2country(df[i, c('longitude','latitude')]))
  if(is.na(location)){
    location = "New Zealand"
  } 
  df$country[i] <- location
  #date
  df$date[i] <- toString(as.POSIXct(as.numeric(media$data$created_time[[i]]), origin="1970-01-01"))
}

sorted_df <- df[with(df,order(date)),]

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # Number of posts
  number_of_posts = textOutput(
    "num_posts",
    inline = T
  ),
  
  # Total number of likes
  tot_likes = textOutput(
    "total_likes",
    inline = T
  ),
  
  # Highest number of likes
  maximum_likes = textOutput(
    "max_likes",
    inline = T
  ),
  
  # Total Comments
  total_comments = textOutput(
    "tot_comments",
    inline = T
  ),
  
  # Country Dropdown Box
  city_selector = selectInput(
    "country", 
    label = "Select Country", 
    choices = df$country %>% 
      unique(),
    selected = "New Zealand"
  ),
  
  # Leaflet map
  leaflet_map = leafletOutput("mymap")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Number of posts text in UI
  output$num_posts <- renderText({
    df %>% 
      nrow()
  })
  
  # Number of posts text in UI
  output$total_likes <- renderText({
    sum(df$likes)
  })
  
  # Number of posts text in UI
  output$max_likes <- renderText({
    max(df$likes)
  })
  
  # Number of posts text in UI
  output$tot_comments <- renderText({
    sum(df$comments)
  })
  
  points <- reactive({
    df %>%
    filter(country == input$country)
  })
  
  # Creating Map 
  output$mymap <- renderLeaflet({
    leaflet(data = points()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(~longitude, ~latitude, clusterOptions = markerClusterOptions(), popup = ~htmlEscape(paste("Location: ",location_name, "<br> Posted Time: ", date)))
  })
}

shinyApp(ui, server)
