library(dplyr)
library(ggplot2)
require(lubridate)
require(XML)
require(RJSONIO)
require(igraph)
require(googleVis)

setwd('/Users/eloimorlaas/Documents/Columbia/Spring_2016/EDAV/Proj2_flood/github_work/EDAV_Project_2')
data = read.csv('data/Cleaned_main_cause.csv')
data <- data.frame(data)
data <- data[1:4319,]

# Include Continent information
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(long, lat){
  points = data.frame(long, lat)
  na = is.na(points)
  index = rowSums(na)>=1
  points[na] = 45
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  res = indices$REGION
  res[index] = NaN
  return(res)   # returns the continent (6 continent model)
}


data <- data %>%
  mutate(continent = coords2country(Centroid.X,Centroid.Y))



# Number death from floods per country
top_ten_countries <- data %>%
  group_by(Country) %>%
  summarise(dead=sum(Dead, rm.na=TRUE)) %>%
  rename(country=Country) %>%
  arrange(desc(dead)) %>%
  select(country) %>%
  slice(1:10)

deads_per_country <- data %>% 
  group_by(Country, Main.cause) %>%
  summarise(dead=sum(Dead, rm.na=TRUE)) %>%
  filter(Country %in% top_ten_countries$country) %>%
  filter(dead > 10)


# These 10 countries represent 85% of the total floods deaths accross the world 
deads_per_country <- transform(deads_per_country, 
                          Country = reorder(Country, order(dead, decreasing = TRUE)))

ggplot(deads_per_country, 
       aes(x = reorder(Country,-dead), y = dead, fill=Main.cause)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Top 10 deadliest countries for floods since 1985") +
  ylab('Number of deaths')+
  xlab('Country')


# Plot of the number of floods during the year
avg_floods_year <- data %>%
  mutate(month = month(as.Date(Began))) %>%
  group_by(month) %>%
  summarise(avg_severity=mean(Severity.., rm.na=TRUE)) %>%
  select(month, avg_severity) %>%
  filter(is.na(avg_severity)==FALSE)
  

ggplot(avg_floods_year, 
       aes(x = month, y = avg_severity, fill=avg_severity)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Average severity of the floods according to the month") +
  ylab('Average severity') +
  scale_x_discrete(limits=month.abb[avg_floods_year$month]) +
  coord_cartesian(ylim=c(1.2,1.25)) +
  theme_minimal()

# Same plot but for only Eurasian countries
avg_floods_year <- data %>%
  filter(continent=='Asia') %>%
  mutate(month = month(as.Date(Began))) %>%
  group_by(month) %>%
  summarise(avg_severity=mean(Severity.., rm.na=TRUE)) %>%
  select(month, avg_severity) %>%
  filter(is.na(avg_severity)==FALSE)
  
  
ggplot(avg_floods_year, 
       aes(x = month, y = avg_severity, fill=avg_severity)) + 
  geom_bar(stat = 'identity') +
  scale_colour_brewer(palette="Blues") +
  ggtitle("Average severity of the floods in Asia according to the month") +
  ylab('Average severity') +
  scale_x_discrete(limits=month.abb[avg_floods_year$month]) +
  coord_cartesian(ylim=c(1.05,1.25)) +
  theme_minimal()

# Same plot but with the number of floods
avg_floods_year <- data %>%
  filter(continent=='Asia') %>%
  mutate(month = month(as.Date(Began))) %>%
  group_by(month) %>%
  summarise(nb_floods=n()) %>%
  select(month, nb_floods) %>%
  filter(is.na(nb_floods)==FALSE)


ggplot(avg_floods_year, 
       aes(x = month, y = nb_floods, fill=nb_floods)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Total number of floods in Asia according to the month since 1985") +
  ylab('Total number of floods') +
  scale_x_discrete(limits=month.abb[avg_floods_year$month]) +
  #coord_cartesian(ylim=c(1.1,1.25)) +
  theme_minimal()



# Zoom on Thailand
avg_floods_year <- data %>%
  filter(Country=='Thailand') %>%
  mutate(month = month(as.Date(Began))) %>%
  group_by(month) %>%
  summarise(avg_severity=mean(Severity.., rm.na=TRUE)) %>%
  select(month, avg_severity) %>%
  filter(is.na(avg_severity)==FALSE)


ggplot(avg_floods_year, 
       aes(x = month, y = avg_severity, fill=avg_severity)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Average severity of the floods in Thailand according to the month") +
  ylab('Average severity') +
  scale_x_discrete(limits=month.abb[avg_floods_year$month]) +
  coord_cartesian(ylim=c(0.8,2.1)) +
  theme_minimal()



# Number of floods per continents
nb_floods_cont <- data %>%
  group_by(continent) %>%
  summarise(nb_floods=n()) %>%
  filter(is.na(continent)!=TRUE)

ggplot(nb_floods_cont, 
       aes(x = continent, y = nb_floods, fill=nb_floods)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Total Number of floods per continent") +
  ylab('Total Number of floods') +
  theme_minimal()


# Sankey diagram

# We add the variables short, middle and long floods
data2 <- mutate(data, short_flood=(duration<=4), middle_flood=(duration>4 & duration<=10), long_flood=duration>10)


test = c('Heavy Rain', 'Tropical Cyclone', 'Snow', 'Monsoonal Rain', 'Tide Surge', 'Tsunami')
for (j in 1:dim(data2)[1]) {
  cause = data2[j,'Main.cause']
  duration = data2[j, 'duration']
  aff = data2[j, 'Affected.sq.km']
  if (cause %in% test)   {
    data2$cause[j] = toString(cause)
  }
  else{
    data2$cause[j] = 'Other'
  }
  if (duration<=4 | is.na(duration)) {
    data2$dur[j] = 'short flood'
  }
  else if (duration > 4 & duration <= 10){
    data2$dur[j] = 'middle flood'
  }
  else {
    data2$dur[j] = 'long flood'
  }
  
  if (aff<=10000 | is.na(aff)) {
    data2$affected[j] = '< 10,000 sq km'
  }
  else if (aff > 10000 & aff <= 70000){
    data2$affected[j] = 'btw 10,000 & 70,000 sq km'
  }
  else {
    data2$affected[j] = '> 70,000 sq km'
  }
}


df <- data2 %>%
  group_by(continent, affected, cause) %>%
  summarise(flux = n()) %>%
  select(continent, cause, affected, flux) %>%
  filter(is.na(continent)!=TRUE)
df <- data.frame(df)
origin <- df[,c(1,2,4)]
origin <- rename(origin, origin=continent, visit=cause)
visit <- df[, c(2,3,4)]
visit <- rename(visit, origin=cause, visit=affected)
df1 <- rbind(origin, visit)

df <- data2 %>%
  group_by(continent, affected, cause) %>%
  summarise(flux = sum(Displaced)) %>%
  select(continent, cause, affected, flux) %>%
  filter(is.na(continent)!=TRUE)
df <- data.frame(df)
origin <- df[,c(1,2,4)]
origin <- rename(origin, origin=continent, visit=cause)
visit <- df[, c(2,3,4)]
visit <- rename(visit, origin=cause, visit=affected)
df1_bis <- rbind(origin, visit)

df <- data2 %>%
  group_by(continent, affected, dur) %>%
  summarise(flux = n()) %>%
  select(continent, dur, affected, flux) %>%
  filter(is.na(continent)!=TRUE)
df <- data.frame(df)
origin <- df[,c(1,2,4)]
origin <- rename(origin, origin=continent, visit=dur)
visit <- df[, c(2,3,4)]
visit <- rename(visit, origin=dur, visit=affected)
df2 <- rbind(origin, visit)

df3 <- data2 %>%
  group_by(continent, cause, dur) %>%
  summarise(flux = n()) %>%
  select(continent, dur, cause, flux) %>%
  filter(is.na(cause)!=TRUE & is.na(continent)!=TRUE)
df3 <- data.frame(df3)
origin <- df3[,c(1,2,4)]
origin <- rename(origin, origin=continent, visit=dur)
visit <- df3[, c(2,3,4)]
visit <- rename(visit, origin=dur, visit=cause)
df3 <- rbind(origin, visit)

df4 <- data2 %>%
  group_by(continent, cause, dur) %>%
  summarise(flux = sum(Dead)) %>%
  select(continent, dur, cause, flux) %>%
  filter(is.na(cause)!=TRUE & is.na(continent)!=TRUE)
df4 <- data.frame(df4)
origin <- df4[,c(1,2,4)]
origin <- rename(origin, origin=continent, visit=dur)
visit <- df4[, c(2,3,4)]
visit <- rename(visit, origin=dur, visit=cause)
df4 <- rbind(origin, visit)


plot(
  gvisSankey(df1, from="origin", 
             to="visit", weight="flux",
             options=list(
               sankey="{link: {colorMode: 'gradient', colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f','#cab2d6', '#ffff99', '#1f78b4', '#33a02c'] },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )


plot(
  gvisSankey(df1_bis, from="continent", 
             to="cause", weight="flux",
             options=list(
               sankey="{link: {colorMode: 'gradient', colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f','#cab2d6', '#ffff99', '#1f78b4', '#33a02c'] },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )

plot(
  gvisSankey(df2, from="continent", 
             to="cause", weight="flux",
             options=list(
               sankey="{link: {colorMode: 'gradient', colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f','#cab2d6', '#ffff99', '#1f78b4', '#33a02c'] },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )

plot(
  gvisSankey(df3, from="continent", 
             to="cause", weight="flux",
             options=list(
               sankey="{link: {colorMode: 'gradient', colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f','#cab2d6', '#ffff99', '#1f78b4', '#33a02c'] },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )

plot(
  gvisSankey(df4, from="continent", 
             to="cause", weight="flux",
             options=list(
               sankey="{link: {colorMode: 'gradient', colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f','#cab2d6', '#ffff99', '#1f78b4', '#33a02c'] },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )


