setwd("~/Documents/cuw4701_edav/final_project/")

#library
library(plyr)
library(ggplot2)
library(glmnet)
library(reshape2)

#
df = read.csv("filtered_311.csv")

minyear <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
  "Nov", "Dec")

# slice into 12 months
df$Created.Date = substring(df$Created.Date, 1, 10)

bymonth <- function(df, start, end) {
  res = df[df$Created.Date<end,]
  res = res[res$Created.Date>=start,]
  res
}

jan = bymonth(df, '2015-01-01', '2015-02-01')
feb = bymonth(df, '2015-02-01', '2015-03-01')
mar = bymonth(df, '2015-03-01', '2015-04-01')
apr = bymonth(df, '2015-04-01', '2015-05-01')
may = bymonth(df, '2015-05-01', '2015-06-01')
jun = bymonth(df, '2015-06-01', '2015-07-01')
jul = bymonth(df, '2015-07-01', '2015-08-01')
aug = bymonth(df, '2015-08-01', '2015-09-01')
sep = bymonth(df, '2015-09-01', '2015-10-01')
oct = bymonth(df, '2015-10-01', '2015-11-01')
nov = bymonth(df, '2015-11-01', '2015-12-01')
dec = bymonth(df, '2015-12-01', '2016-01-01')

getStreetCond <- function(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec) {
  res = data.frame(summary(jan$Complaint.Type))
  res = cbind(res, data.frame(summary(feb$Complaint.Type)))
  res = cbind(res, data.frame(summary(mar$Complaint.Type)))
  res = cbind(res, data.frame(summary(apr$Complaint.Type)))
  res = cbind(res, data.frame(summary(may$Complaint.Type)))
  res = cbind(res, data.frame(summary(jun$Complaint.Type)))
  res = cbind(res, data.frame(summary(jul$Complaint.Type)))
  res = cbind(res, data.frame(summary(aug$Complaint.Type)))
  res = cbind(res, data.frame(summary(sep$Complaint.Type)))
  res = cbind(res, data.frame(summary(oct$Complaint.Type)))
  res = cbind(res, data.frame(summary(nov$Complaint.Type)))
  res = cbind(res, data.frame(summary(dec$Complaint.Type)))

  res = rbind(res, colSums(res))
  colnames(res) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                    "Nov", "Dec")
  rownames(res)[6] = 'Complaints'
  res = t(res)
  res = cbind(data.frame(matrix(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                  "Nov", "Dec"))), res)
  colnames(res)[1] = 'Months'
  res
}

street_cond = getStreetCond(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

colnames(street_cond)
barplot(rowSums(street_cond))

# preci data
precipitation <- matrix(c(5.23, 2.04, 4.72, 2.08, 1.86, 4.79,
                          3.98, 2.35, 3.28, 3.91, 2.01, 4.72))
rownames(precipitation) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                            "Nov", "Dec")
colnames(precipitation) = 'Precipitation'

barplot(precipitation[,1])

# temp data
temperature <- matrix(c(29.9, 23.9, 38.1, 54.3, 68.5, 71.2,
                        78.8, 79.0, 74.5, 58.0, 52.8, 50.8 ))
rownames(temperature) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                           "Nov", "Dec")
colnames(temperature) = 'Temperature'
barplot(temperature[,1])


pRegData <- melt(regreData[,c(1, 7:9)])
#pRegData <- melt(regreData)
g1 <- ggplot(pRegData, 
             aes(x = Months, y = value, colour = variable, group = variable, label = variable)) + 
  geom_line() + facet_grid(variable ~., scales = 'free_y' ) + 
  scale_x_discrete(limits = minyear) +
  theme(axis.title.y = element_blank())
g1


