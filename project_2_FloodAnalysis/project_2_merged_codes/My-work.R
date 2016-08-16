library("ggmap")
library("maptools")
library("maps")
library(plyr)
library(plotly)

Sys.setenv("plotly_username"="raincoatrun")
Sys.setenv("plotly_api_key"="gonvxpd9qn")




setwd('/Users/haiyuancao/Documents/Columbia-Course/ExplotaryDataScience/HW2')
fname = 'cleaned_flood.csv'
dt = read.csv(fname)
colnames(dt)
data("wrld_simpl")

lon = dt$Centroid.X 
lat = dt$Centroid.Y
is.na(lon)
is.na(lat)
grep("Centroid.X", colnames(dt))
grep("Centroid.Y", colnames(dt))
#complete centroid.X and centroid.Y
dt2 = dt[complete.cases(dt[,14:15]),]
grep("Displaced", colnames(dt2))
grep("Country",colnames(dt2))
#complete displaced 
dt3 = dt2[complete.cases(dt2[,9]),]
#complete country
dt4 = dt3[complete.cases(dt3[,3]),]
grep("Magnitude", colnames(dt2))
dt5 = dt3[complete.cases(dt3[,13]),]
#####################################
#Plot Displacement 
disp <- dt4[, c(3, 9, 14, 15)]
lon1 = dt2$Centroid.X 
lat1 = dt2$Centroid.Y
disp2 <- rename(disp, c("Country" = "name", "Centroid.X" = "lon", "Displaced" = "displaced", "Centroid.Y" = "lat"))

disp2$hover <- paste(disp2$name, "Population", disp2$displaced, "people")
disp3<-disp2[!(disp2$displaced == 0),]

disp3$q <- with(disp3, cut(displaced, quantile(displaced)))
levels(disp3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
disp3$q <- as.ordered(disp3$q)
disp3 = disp3[complete.cases(disp3[,6]),]


g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p1 <- plot_ly(disp3, lon = lon, lat = lat,text = hover,
        marker = list(size = sqrt(displaced/10000) + 1),
        color = q, type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Displacement due to the flood <br>(Click legend to toggle)', geo = g)

plotly_POST(p1, filename = "r-docs/test1", world_readable=TRUE)

----------------------------
#####################################
#Plot Duration
grep("duration", colnames(dt2))
dt_dur = dt2[complete.cases(dt2[,7]),]
dt_durc = dt_dur[complete.cases(dt_dur[,3]),]
dur <- dt_durc[, c(3, 7, 14, 15)]

lon2 = dt2$Centroid.X 
lat2 = dt2$Centroid.Y
dur2<- rename(dur, c("Country" = "name", "Centroid.X" = "lon", "duration" = "duration", "Centroid.Y" = "lat"))

dur2$hover <- paste(dur2$name, "Flood duration", dur2$duration, "days")
dur3<-dur2[!(dur2$duration == 0),]
dur3$duration <- dur3$duration * 100
dur3$q <- with(dur3, cut(duration, quantile(duration)))
levels(dur3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
dur3$q <- as.ordered(dur3$q)
#disp3 = disp3[complete.cases(disp3[,6]),]


g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p2<-plot_ly(dur3, lon = lon, lat = lat,text = hover,
              marker = list(size = sqrt(duration/100) + 1),
              color = q, type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Duration time of each flood <br>(Click legend to toggle)', geo = g)

plotly_POST(p2, filename = "r-docs/test2", world_readable=TRUE)

----------------------------
########################
#Plot Dead
grep("Dead", colnames(dt2))
dt_dead = dt2[complete.cases(dt2[,8]),]
dt_deadc = dt_dead[complete.cases(dt_dead[,3]),]
dead <- dt_deadc[, c(3, 8, 14, 15)]

lon2 = dead$Centroid.X 
lat2 = dead$Centroid.Y
dead2<- rename(dead, c("Country" = "name", "Centroid.X" = "lon", "Dead" = "dead", "Centroid.Y" = "lat"))

dead2$hover <- paste(dead2$name, "Dead", dead2$dead, "people")
dead3<-dead2[!(dead2$dead == 0),]
#dead3$duration <- dur3$duration * 100
dead3$q <- with(dead3, cut(dead, quantile(dead)))
levels(dead3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
dead3$q <- as.ordered(dead3$q)
#disp3 = disp3[complete.cases(disp3[,6]),]


g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p3<-plot_ly(dead3, lon = lon, lat = lat,text = hover,
            marker = list(size = sqrt(dead/100) + 1),
            color = q, type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Number of Peolple Died Due to Flood <br>(Click legend to toggle)', geo = g)

plotly_POST(p3, filename = "r-docs/test3", world_readable=TRUE) 
----------------------------
########################
#Plot Affected Square
grep("Affected", colnames(dt2))
dt_sq = dt2[complete.cases(dt2[,12]),]
dt_sqc = dt_sq[complete.cases(dt_dead[,3]),]
sq <- dt_sqc[, c(3, 12, 14, 15)]

lon2 = sq$Centroid.X 
lat2 = sq$Centroid.Y
sq2<- rename(sq, c("Country" = "name", "Centroid.X" = "lon", "Affected.sq.km" = "sq", "Centroid.Y" = "lat"))

sq2$hover <- paste(sq2$name, "Affected", sq2$sq, "sq meters")
sq3<-sq2[!(sq2$sq == 0),]
#dead3$duration <- dur3$duration * 100
sq3$q <- with(sq3, cut(sq, quantile(sq)))
levels(sq3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
sq3$q <- as.ordered(sq3$q)
#disp3 = disp3[complete.cases(disp3[,6]),]


g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

p4<-plot_ly(sq3, lon = lon, lat = lat,text = hover,
            marker = list(size = sqrt(sq/10000) + 1),
            color = q, type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Affect Area Due to Flood <br>(Click legend to toggle)', geo = g)

plotly_POST(p4, filename = "r-docs/test4", world_readable=TRUE) 

  
  
----------------------------
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$hover <- paste(df$name, "Population", df$pop/1e6, " million")

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
#color = q, type = 'scattergeo', locationmode = 'USA-states') %>%
#plot_ly(df, lon = lon, lat = lat, text = hover,
plot_ly(df, lon = lon, lat = lat,
        marker = list(size = sqrt(pop/10000) + 1),
        color = q, type = 'scattergeo', locationmode='USA-states' ) %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
df$lon
df$lat


df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
show(df)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

plot_ly(df, z = GDP..BILLIONS., text = COUNTRY, locations = CODE, type = 'choropleth',
        color = GDP..BILLIONS., colors = 'Blues', marker = list(line = l),
        colorbar = list(tickprefix = '$', title = 'GDP Billions US$')) %>%
  layout(title = '2014 Global GDP<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">CIA World Factbook</a>',
         geo = g)

##########################

# Load animation and maps package
library(animation)
library(maps)

# Directory to store animated GIFs
ani.options(outdir = getwd())

###
# Animated GIF example
###

saveGIF({
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, movie.name = "brownian_motion.gif", interval = 0.1, nmax = 30,
ani.width = 600, ani.height = 600)


##################################################
library(maps)
library(mapproj)
library(maptools)
library(geosphere)
library(splancs)
library(foreign)
library("ggmap")
#library(ggplot2)
source("/Users/haiyuancao/Documents/Columbia-Course/ExplotaryDataScience/HW2/201403-density-tutorial/library/densityHelper.R")
par(mar=c(0,0,0,0))

map("world", col="gray90", bg="white", lwd=1.0)
points(dt2$Centroid.X, dt2$Centroid.Y, pch=21, cex=0.1, col="#00000020")

par(mar=c(0,0,0,0))
#m.proj <- map("world", proj="stereographic", param=c(39, 45), col="#000000", fill=FALSE, bg=NA, lwd=0.4)
m.proj <- map("world", proj="albers", param=c(0, 180),col="#000000", fill=FALSE, bg=NA, lwd=0.4)

loc.proj <- mapproject(dt2$Centroid.X, dt2$Centroid.Y)
pts <- as.points(list(x=loc.proj$x, y=loc.proj$y))
points(pts, pch=21, cex=0.1, col="#00000020")
blackWhite <- colorRampPalette(c("white", "black"))

m.proj <- map("world", proj="albers", param=c(0, 180), col=NA, fill=FALSE, bg=NA, lwd=0.4)
#m.proj <- map("world", proj="lagrange",col=NA, fill=FALSE, bg=NA, lwd=0.4)

#xlim <- match(NA, m.proj[["x"]]) - 1
#ylim <- match(NA, m.proj[["y"]]) - 1

#uspoly.proj <- as.matrix(cbind(m.proj[["x"]][1:xlim], m.proj[["y"]][1:ylim]))
uspoly.proj <- as.matrix(cbind(m.proj[["x"]], m.proj[["y"]]))
us.data <- as.data.frame(uspoly.proj)
us.comp = na.omit(us.data)
uspoly.proj <- as.matrix(us.comp)
# Density
smoo.proj <- kernel2d(pts, uspoly.proj, h0=0.1, nx=500, ny=500)

# Draw map
image(smoo.proj, uspoly.proj, add=TRUE, col=blackWhite(100))
map("world", proj="albers", param=c(0, 180), col="#999999", fill=FALSE, bg=NA, lwd=0.2, add=TRUE, resolution=1)
#map("world", proj="lagrange", col="#999999", fill=FALSE, bg=NA, lwd=0.2, add = TRUE, resolution = 1)


### Various colors
par(mfrow=c(1,1), mar=c(0,0,0,0))

# Lower resolution
smoo.proj <- kernel2d(pts, uspoly.proj, h0=0.5, nx=150, ny=150)

# White purple black
#map("world", proj="lagrange",fill=FALSE, col = NA, bg=NA, lwd=0.2)
map("world", proj="albers", param=c(0, 180), col="#999999", fill=FALSE, bg=NA, lwd=0.4)
purple <- colorRampPalette(c("white", "purple", "black"))
image(smoo.proj, uspoly.proj, add=TRUE, col=purple(100))

# Black blue white
#map("usa", proj="albers", param=c(39, 45), col=NA, fill=FALSE, bg=NA, lwd=0.4)
map("world", proj="albers", param=c(0, 180), col=NA, fill=FALSE, bg=NA, lwd=0.4)

blue <- colorRampPalette(c("black", "light blue", "blue", "white"))
image(smoo.proj, uspoly.proj, add=TRUE, col=blue(100))

# Red orange yellow
map("world", proj="albers", param=c(0, 180), col="black", fill=FALSE, bg=NA, lwd=0.4, resolution = 1)
#map("world", proj="albers", param=c(0, 180), col=NA, fill=FALSE, bg=NA, lwd=0.4)
redOrange <- colorRampPalette(c("white", "black", "red", "orange", "#e6df06"))
image(smoo.proj, uspoly.proj, add=TRUE, col=redOrange(100))

# Reverse of above
map("world", proj="albers", param=c(0, 180), col=NA, fill=FALSE, bg=NA, lwd=0.4)
redOrange.rev <- rev(redOrange(100))
image(smoo.proj, uspoly.proj, add=TRUE, col=redOrange.rev)



#
# Introduce other elements
#

# Washington bounding box
rg <- c(-125.31,45.36,-116.29,49.35)

# Highway shapefile
washington <- readShapeSpatial("/Users/haiyuancao/Documents/Columbia-Course/ExplotaryDataScience/HW2/201403-density-tutorial/data/tl_2012_53_prisecroads/tl_2012_53_prisecroads.shp", proj4string=CRS("+proj=longlat"))
plot(washington)

# Now plot Washington, the highways, and density
par(mar=c(0,0,0,0))
m.wash <- map("state", "Washington", fill=FALSE, col=NA)
stateNum <- 53
acc.wash <- subset(accidents, STATE==stateNum)
pts.wash <- as.points(list(x=acc.wash$LONGITUD, y=acc.wash$LATITUDE))

# A hack to use the Washington state border as the polygon
na.i <- which(is.na(m.wash$x))
start.i <- na.i[6] - 1
end.i <- na.i[5] + 1
start.i2 <- na.i[4] + 1
end.i2 <- na.i[5] - 1
poly.wash.x <- c(m.wash$x[start.i:end.i], m.wash$x[start.i2:end.i2])
poly.wash.y <- c(m.wash$y[start.i:end.i], m.wash$y[start.i2:end.i2])
poly.wash <- as.points( list(x = poly.wash.x, y = poly.wash.y ) )

# Find densities
smoo.wash <- kernel2d(pts.wash, poly.wash, h0=0.1, nx=350, ny=300)

# Draw it
image(smoo.wash, add=TRUE, col=redOrange(100))
plot(washington, lwd=0.2, add=TRUE, col="#E0E0C0")
lines(poly.wash)

###########################################################################################
fname2 = 'Cleaned_main_cause.csv'
dtnew = read.csv(fname2)
colnames(dtnew)
lon = dtnew$Centroid.X 
lat = dtnew$Centroid.Y
is.na(lon)
is.na(lat)
grep("Centroid.X", colnames(dtnew))
grep("Centroid.Y", colnames(dtnew))
grep("Main", colnames(dtnew2))
grep("Magnitude", colnames(dtnew2))
grep("Country", colnames(dtnew2))
grep("Dead", colnames(dtnew2))
grep("Displaced", colnames(dtnew2))
grep("Affected.sq.km", colnames(dtnew2))
grep('duration', colnames(dtnew2))
#complete centroid.X and centroid.Y
dtnew2 = dtnew[complete.cases(dtnew[,15:16]),]
dtnew_cause = dtnew2[complete.cases(dtnew2[,11]),]
dtnew_m_c = dtnew_cause[complete.cases(dtnew_cause[,14]),]
dtnew_m_c = dtnew_m_c[complete.cases(dtnew_m_c[,4]),]
dtnew_m_c = dtnew_m_c[complete.cases(dtnew_m_c[,9]),]
dtnew_m_c[dtnew_m_c$Main.cause == 'Avalance',]$Main.cause = 'Others'
dtnew_m_c[dtnew_m_c$Main.cause == 'Landslide',]$Main.cause = 'Others'
dtnew_m_c[dtnew_m_c$Main.cause == 'Tide Surge',]$Main.cause = 'Others'
dtnew_m_c[dtnew_m_c$Main.cause == 'Tsunami',]$Main.cause = 'Others'
dtnew_m_c[dtnew_m_c$Main.cause == 'Ice Movement',]$Main.cause = 'Others'
dtnew_m_c[dtnew_m_c$Main.cause == 'Storm',]$Main.cause = 'Others'

dtnew_m_c$Main.cause <- droplevels(dtnew_m_c$Main.cause)
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=dtnew_m_c$Centroid.X, y=dtnew_m_c$Centroid.Y, colour = factor(dtnew_m_c$Main.cause)), size = 0.8) +  scale_color_discrete(name  ="Cause")
mp <- mp + xlab('Longitude')+ylab('Latitude')+ ggtitle("Distribution of Flood with Different Causes in the World")  
mp
ggsave('plot1', plot = mp, device = 'pdf', path = '/Users/haiyuancao/Documents/Columbia-Course/ExplotaryDataScience/HW2', dpi = 500)
------------------------------------------------------------------------------------------------------------------------------
 
dead <- dtnew_m_c[, c(4, 9, 11, 15, 16)]

lon2 = dead$Centroid.X 
lat2 = dead$Centroid.Y
dead2<- rename(dead, c("Country" = "name", "Centroid.X" = "lon", "Dead" = "dead", "Centroid.Y" = "lat", "Main.cause" = "cause"))

dead2$hover <- paste(dead2$name, "Dead", dead2$dead, "people")
dead3<-dead2[!(dead2$dead == 0),]
#dead3$duration <- dur3$duration * 100
dead3$q <- with(dead3, cut(dead, quantile(dead)))
levels(dead3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
dead3$q <- as.ordered(dead3$q)
#disp3 = disp3[complete.cases(disp3[,6]),]


dead3[dead3$cause == 'Avalance',]$cause = 'Others'
dead3[dead3$cause == 'Landslide',]$cause = 'Others'
dead3[dead3$cause == 'Tide Surge',]$cause = 'Others'
dead3[dead3$cause == 'Tsunami',]$cause = 'Others'
dead3[dead3$cause == 'Ice Movement',]$cause = 'Others'

g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p3<-plot_ly(dead3, lon = lon, lat = lat,text = hover,
            marker = list(size = sqrt(dead/25) + 1),
            color = factor(dead3$cause), type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Number of Peolple Died in Each Flood Due To Different Causes <br>(Click the name of cause to toggle)', geo = g)

plotly_POST(p3, filename = "r-docs/flood-dead", world_readable=TRUE) 

table(dead3$cause)

------------------------------------------------------------------------
disp <- dtnew_m_c[, c(4, 10, 11, 15, 16)]

lon2 = disp$Centroid.X 
lat2 = disp$Centroid.Y
disp2<- rename(disp, c("Country" = "name", "Centroid.X" = "lon", "Displaced" = "disp", "Centroid.Y" = "lat", "Main.cause" = "cause"))

disp2$hover <- paste(disp2$name, "Displaced", disp2$disp, "people")
disp3<-disp2[!(disp2$disp == 0),]
#dead3$duration <- dur3$duration * 100
disp3$q <- with(disp3, cut(disp, quantile(disp)))
levels(disp3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
disp3$q <- as.ordered(disp3$q)
#disp3 = disp3[complete.cases(disp3[,6]),]


disp3[disp3$cause == 'Avalance',]$cause = 'Others'
disp3[disp3$cause == 'Landslide',]$cause = 'Others'
disp3[disp3$cause == 'Tide Surge',]$cause = 'Others'
disp3[disp3$cause == 'Tsunami',]$cause = 'Others'
disp3[disp3$cause == 'Ice Movement',]$cause = 'Others'

g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p3<-plot_ly(disp3, lon = lon, lat = lat,text = hover,
            marker = list(size = sqrt(disp/10000) + 1),
            color = factor(disp3$cause), type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Number of Peolple Displaced in Each Flood Due To Different Causes <br>(Click the name of cause to toggle)', geo = g)

plotly_POST(p3, filename = "r-docs/flood-disp", world_readable=TRUE) 

table(dead3$cause)

--------------------------------------------------------------------
affect <- dtnew_m_c[, c(4, 13, 11, 15, 16)]

lon2 = affect$Centroid.X 
lat2 = affect$Centroid.Y
affect2<- rename(affect, c("Country" = "name", "Centroid.X" = "lon", "Affected.sq.km" = "affect", "Centroid.Y" = "lat", "Main.cause" = "cause"))

affect2$hover <- paste(affect2$name, "Area Affeced", affect2$disp, "sq.km")
affect3<-affect2[!(disp2$disp == 0),]
#dead3$duration <- dur3$duration * 100
affect3$q <- with(affect3, cut(affect, quantile(disp)))
levels(affect3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
affect3$q <- as.ordered(affect3$q)
#disp3 = disp3[complete.cases(disp3[,6]),]


affect3[affect3$cause == 'Avalance',]$cause = 'Others'
affect3[affect3$cause == 'Landslide',]$cause = 'Others'
affect3[affect3$cause == 'Tide Surge',]$cause = 'Others'
affect3[affect3$cause == 'Tsunami',]$cause = 'Others'
affect3[affect3$cause == 'Ice Movement',]$cause = 'Others'

g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p3<-plot_ly(affect3, lon = lon, lat = lat,text = hover,
            marker = list(size = sqrt(affect/10000) + 1),
            color = factor(disp3$cause), type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Affected Area in Each Flood Due To Different Causes <br>(Click the name of cause to toggle)', geo = g)

plotly_POST(p3, filename = "r-docs/flood-affect", world_readable=TRUE) 




---------------------------------------------------------------------
duration <- dtnew_m_c[, c(4, 8, 11, 15, 16)]

lon2 = duration$Centroid.X 
lat2 = duration$Centroid.Y
duration2<- rename(duration, c("Country" = "name", "Centroid.X" = "lon", "duration" = "durt", "Centroid.Y" = "lat", "Main.cause" = "cause"))

duration2$hover <- paste(duration2$name, "Flood durates", affect2$disp, "days")
duration3<-duration2[!(duration2$durt == 0),]
#dead3$duration <- dur3$duration * 100
duration3$q <- with(duration3, cut(durt, quantile(durt)))
levels(duration3$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
duration3$q <- as.ordered(duration3$q)
#disp3 = disp3[complete.cases(disp3[,6]),]


duration3[duration3$cause == 'Avalance',]$cause = 'Others'
duration3[duration3$cause == 'Landslide',]$cause = 'Others'
duration3[duration3$cause == 'Tide Surge',]$cause = 'Others'
duration3[duration3$cause == 'Tsunami',]$cause = 'Others'
duration3[duration3$cause == 'Ice Movement',]$cause = 'Others'

g <- list(
  scope = 'world',
  projection = list(type = 'natural earth'),
  showland = TRUE,
  showocean = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p3<-plot_ly(duration3, lon = lon, lat = lat,text = hover,
            marker = list(size = sqrt(durt) + 1),
            color = factor(duration3$cause), type = 'scattergeo', locationmode='country name' ) %>%
  layout(title = 'Duration days of Each Flood Due To Different Causes <br>(Click the name of cause to toggle)', geo = g)

#plotly_POST(p3, filename = "r-docs/flood-duration", world_readable=TRUE) 


