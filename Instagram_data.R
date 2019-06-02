


#packages
require(httr)
require(rjson)
require(RCurl)

app_name <- "ShinyApp"
client_id <- "cd886638948746f7a68d729d28eede56"
client_secret <- "807e480d23ee46e497cdbf63b291774f"
scope = "basic"

instagram <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)

#scope <- NULL
ig_oauth <- oauth2.0_token(instagram, myapp,scope=scope,  type = "application/x-www-form-urlencoded",cache=FALSE)  
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]

owner_info_end_point = paste('https://api.instagram.com/v1/users/self/?access_token=',token,sep="")
data <- getURL(owner_info_end_point)
user_info <- fromJSON(data)
received_profile <- user_info$data[[1]]
user_id <- user_info$data$id
media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',user_id,'/media/recent/?access_token=',token,sep="")))
df = data.frame(no = 1:length(media$data))

for(i in 1:length(media$data))
{
  #comments
  df$comments[i] <-media$data[[i]]$comments$count
  
  #likes:
  df$likes[i] <- media$data[[i]]$likes$count
  
  #location lat
  df$latitude[i] <- media$data[[i]]$location$latitude
  #location long
  df$longitude[i] <- media$data[[i]]$location$longitude
  #location name
  df$location_name[i] <- media$data[[i]]$location$name
  
  #date
  df$date[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01"))
}
