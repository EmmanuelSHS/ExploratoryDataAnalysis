library("ggmap")
library("maptools")
library("maps")
library(RNetCDF)
require(reshape2)
library(dplyr)
library(ggplot2)

setwd('/Users/eloimorlaas/Documents/Columbia/Spring_2016/EDAV/Proj2_flood/github_work/EDAV_Project_2')
fname = "data/NOAA_Daily_phi_500mb.nc"
fid = open.nc(fname)
print.nc(fid)
pressure_data = read.nc(fid)
lon = pressure_data$X - 180
lat = pressure_data$Y
load('floods_data.Rda')

# Prepare the date for merge
floods_data <- data2 %>%
  mutate(merge_date_begin = as.integer(as.Date(Began)-as.Date('1948-01-01')),
         merge_date_end = as.integer(as.Date(Ended)-as.Date('1948-01-01'))) %>%
  filter(Centroid.Y>=35 & Centroid.Y<=70 & merge_date_end<=24872 & merge_date_begin>=0)

for (k in 1:dim(floods_data)[1]){
  begin = floods_data$merge_date_begin[k]
  end = floods_data$merge_date_end[k]
  X_flood = floods_data$Centroid.X[k]
  Y_flood = floods_data$Centroid.Y[k]
  index_X = which(abs(lon-X_flood)==min(abs(lon-X_flood)))
  index_Y = which(abs(lat-Y_flood)==min(abs(lat-Y_flood)))
  floods_data$phi_during[k] = mean(pressure_data$phi[index_X, index_Y, (begin+1):(end+1)])
  floods_data$phi_before[k] = mean(pressure_data$phi[index_X, index_Y, (begin-7):(begin+1)])
  floods_data$phi_after[k] = mean(pressure_data$phi[index_X, index_Y, (end+1):(end+10)])
}

test <- floods_data %>%
  group_by(Severity..) %>%
  summarise(mean_p = mean(phi), min_p = min(phi), median_p = median(phi), max_p = max(phi))


hist(floods_data$phi_after-floods_data$phi_before,
     main='Histogram of the difference in Phi after and before a flood',
     xlab='Difference in Geopotential Height', col='cornflowerblue')
phi_difference = floods_data$phi_after - floods_data$phi_before
t.test(phi_difference, mu=0)


