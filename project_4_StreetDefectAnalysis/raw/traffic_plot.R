setwd("~/Documents/cuw4701_edav/final_project/")

library(maptools)
library(magrittr)
library(ggmap)
library(dplyr)
library(devtools)
library(choroplethr)
library(choroplethrZip)
install_github('arilamstein/choroplethrZip@v1.5.0')

# clean traffic to by zipcode
traffic <- read.csv("traffic_with_cor.csv")
service <- read.csv("cleaned_dot_2015.csv")

traffic_agg = traffic[,26:29]
traffic_agg$traffic = rowSums(traffic[,5:25])

  traffic_zip = data.frame(matrix(NA, nrow = 2*dim(traffic_agg)[1], ncol = 2))
  colnames(traffic_zip) = c("traffic", "zipcode")
  
  
  for (i in 380:dim(traffic_agg)[1]) {
    loc1 = as.numeric(c(traffic_agg$from.lon[i], traffic_agg$from.lat[i]))
    loc2 = as.numeric(c(traffic_agg$to.lon[i], traffic_agg$to.lat[i]))
    
    zip1 = revgeocode(loc1, output='more')
    zip2 = revgeocode(loc2, output='more')
    
    traffic_zip[2*i-1, 5] = traffic_agg[i,1]
    traffic_zip[2*i-1, 2] = as.numeric(as.character(zip1$postal_code[1]))
    traffic_zip[2*i, 5] = traffic_agg[i,1]
    traffic_zip[2*i, 2] = as.numeric(as.character(zip2$postal_code[1]))
  }

# add traffic & remove empty zipcodes  
  
traffic_zip$traffic = rep(traffic_agg$traffic, each=2)
write.csv(traffic_zip, "traffic_agg_zip.csv")

traffic_zip_cleaned = traffic_zip[!is.na(traffic_zip$zipcode),]

# aggregate by zip
traffic_zip_cleaned = aggregate(traffic~zipcode, traffic_zip_cleaned, sum)
traffic_zip_cleaned$traffic = traffic_zip_cleaned$traffic/10

# aggregate complaint data
complaint = data.frame(matrix(1, nrow = dim(service)[1], ncol = 2))
colnames(complaint) = c('zipcode', 'complaints')
complaint$zipcode = service$Incident.Zip
complaint = complaint[!is.na(complaint$zipcode),]

complaint_zip = aggregate(complaints~zipcode, complaint, sum)
complaint_zip = complaint_zip[-(1:3),]
complaint_zip$zipcode = as.factor(complaint_zip$zipcode)

# plot
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
plot_complaint = complaint_zip[! complaint_zip$zipcode %in% c(10000, 10041, 10048, 10107, 10129, 10281, 11241, 11249, 11695),]
colnames(plot_complaint) = c("region", "value")

missing_complaint = data.frame(matrix(c(10174, 10110, 10271, 11003, 10171, 10177, 10152,
                                        10279, 10115, 10167, 11351, 11424, 11425, 11451, 
                                        10169, 10311, 10154, 10199, 10165, 10168, 10278, 
                                        10173, 10170, 10172)))
missing_complaint$value = rep(mean(plot_complaint$value), each = 24)
colnames(missing_complaint) = c("region", "value")
missing_complaint$region = as.factor(missing_complaint$region)
plot_complaint = rbind(plot_complaint, missing_complaint)
plot_complaint = aggregate(value~region, plot_complaint, sum)

zip_choropleth(plot_complaint,
               county_zoom=nyc_fips,
               title="2015 New York City 311 Complaints of DOT",
               legend="Complaints")

zip_choropleth(plot_complaint,
               county_zoom=36061,
               title="2015 Manhattan 311 Complaints of DOT",
               legend="Complaints")



plot_traffic = traffic_zip_cleaned[! traffic_zip_cleaned$zipcode %in% c(10000, 10041, 10048, 10104, 10107, 10129, 10281, 11241, 11249, 11695),]
colnames(plot_traffic) = c("region", "value")

missing_traffic = data.frame(matrix(c(10280, 10174, 10119, 10031, 10013, 10110, 10004, 
                                      10271, 10171, 10009, 10069, 10162, 10177, 10152, 
                                      10038, 10279, 10115, 10005, 10112, 10167, 10006, 
                                      10169, 10103, 10153, 10154, 10199, 10002, 10012, 
                                      10282, 10168, 10278, 10044, 10173, 10040, 10007, 
                                      10170, 10172)))
missing_traffic$value = rep(sum(plot_traffic$value) * 0.75, each = 37)
colnames(missing_traffic) = c("region", "value")
plot_traffic = rbind(plot_traffic, missing_traffic)

plot_traffic$region = as.factor(plot_traffic$region)

zip_choropleth(plot_traffic,
               county_zoom=36061,
               title="2015 Manhattan Traffic Data",
               legend="Average Daily Traffic")

