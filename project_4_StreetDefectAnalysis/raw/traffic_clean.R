setwd('~/Documents/cuw4701_edav/final_project/')

library(ggmap)

traffic <- read.csv('Traffic_Volume_Counts.csv')
traffic_clean_by_street <- aggregate(cbind(X3.00.4.00AM,   X4.00.5.00AM,   X5.00.6.00AM,   X6.00.7.00AM,   X7.00.8.00AM,
          X8.00.9.00AM,   X9.00.10.00AM,  X10.00.11.00AM, X11.00.12.00PM, X12.00.1.00PM ,
          X1.00.2.00PM,   X2.00.3.00PM,   X3.00.4.00PM,   X4.00.5.00PM,   X5.00.6.00PM  ,
          X6.00.7.00PM,   X7.00.8.00PM,   X8.00.9.00PM,   X9.00.10.00PM,  X10.00.11.00PM, 
          X11.00.12.00AM) ~ Roadway.Name + From + To, data = traffic, FUN = mean)

cleanLocation <- function(data) {
  cnames = colnames(data)
  nr = dim(data)[1]
  from = geocode(paste(c(data$Roadway.Name[1], 'and', data$From[1], "New York, NY"), collapse = ' '))
  to = geocode(paste(c(data$Roadway.Name[1], 'and', data$To[1], "New York, NY"), collapse = ' '))
  
  for (i in 2:nr) {
    from = rbind(from, geocode(paste(c(data$Roadway.Name[i], 'and', data$From[i], "New York, NY"), collapse = ' ')))
    to = rbind(to, geocode(paste(c(data$Roadway.Name[i], 'and', data$To[i], "New York, NY"), collapse = ' ')))
  }
  data = cbind(data, from)
  data = cbind(data, to)
  colnames(data) = c(cnames, "from.lon", "from.lat", "to.lon", "to.lat")
  data
}

traffic_with_coord = cleanLocation(traffic_clean_by_street)

write.csv(traffic_with_coord, "traffic_with_cor.csv")
# 
library(leaflet)
data=read.csv('filtered_311.csv')

l<-leaflet()
l<-addTiles(l)
l<-addCircleMarkers(l
                    ,lng = data$Longitude
                    , lat = data$Latitude
                    , popup = paste("Time:", data$Created.Date, "<br>"
                                    , "Complaint Type:", data$Complaint.Type)
                    #, clusterOptions = markerClusterOptions()
                    , radius =0.1
                    , fillOpacity = 0.1
                    , color = 'red'
)

l

myLoc = "New York City, NY"

