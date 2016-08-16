#install.packages("ggmap")
#install.packages("maptools")
#install.packages("maps")
library("ggmap")
library("maptools")
library("maps")
library(RNetCDF)
require(reshape2)
setwd("/Users/vishaljuneja/Dropbox/EDAV/Repos/EDAV_Project_2/")

fname = "data/NOAA_Daily_phi_500mb.nc"
fid = open.nc(fname)
print.nc(fid)
dat = read.nc(fid)

################## make a simple plot
#map("world", fill=TRUE, col="white", ylim=c(-60, 90), mar=c(0,0,0,0))
data("wrld_simpl")
lon = dat$X - 180
lat = dat$Y
phi = dat$phi[,,1]
plot.new()
quartz(width=9, height=6)
#rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
#filled.contour(x=lon, y=rev(lat), z=phi, color.palette=rgb.palette, #levels=numeric,
#               plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
#               plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})

#bwr = colorRampPalette(c("blue", "white", "red"))
image(lon, rev(lat), phi, xlab="Longitude", ylab="Latitude", xlim=c(-180,180), ylim=c(-90,90))
 #     , col=terrain.colors(100))
#map("world", add = TRUE)
#box()
#contour(x, y, phi, add=TRUE, levels = seq(90, 200, by = 5))
#title(main="Phi")
plot(wrld_simpl, add=TRUE)
#map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0), add=T)
#mapWorld <- borders("world", colour="gray50", fill="gray50")
#mapWorld


################## try ggplot
df = data.frame(phi)
names(df) = lat
df$lon= lon
mdata = melt(df, id=c("lon"))
names(mdata) = c("lon", "lat", "x")
mdata$x = as.numeric(mdata$x)
mdata$lon = as.numeric(mdata$lon)
mdata$lat = as.numeric(as.character(mdata$lat))

wr <- map_data("world")
# Prepare a map of World
wrmap <- ggplot(wr, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  geom_point(data=mdata, inherit.aes=FALSE, aes(x=lon, y=lat, fill=x), size=3, shape=4) +
  #ggplot(mdata, inherit.aes=F, aes(lon, lat))
  scale_fill_gradient("Phi", limits=c(4500,6000)) +
  #scale_fill_brewer()
  theme_bw() +
  coord_equal()

wrmap

################# make an animation
# get an years data
lon = dat$X - 180
lat = dat$Y
a=16120
b=16130
for (i in c(a:b)) {
  plot.new()
  pdf(file=sprintf("temp%02d.pdf", i), width=9, height=4)
  phi = dat$phi[,,i]
  #quartz(width=11, height=4)
  #image(lon, rev(lat), phi, xlab="Longitude", ylab="Latitude")
  #title(main="Phi")
  #plot(wrld_simpl, add=TRUE)
  rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
  filled.contour(x=lon, y=rev(lat), z=phi, color.palette=rgb.palette, #levels=numeric,
                 plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
                 plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})
  dev.off()
}
system("convert -background white -alpha remove -layers OptimizePlus -delay 80 *.pdf thai_flood.gif")
file.remove(list.files(pattern="pdf"))


###### use animation package

library(animation)

#Set delay between frames when replaying
ani.options(interval = 0.05, ani.dev="png", ani.height=300, ani.width=900)
saveGIF({
  lon = dat$X - 180
  lat = dat$Y
  #a=16120
  #b=16130
  a = 15755
  b = 15765
  
  min_z = Inf
  max_z = 0
  for (i in c(a:b)) {
    phi = dat$phi[,,i]
    if (min_z > min(phi)) {min_z = min(phi)}
    if (max_z < max(phi)) {max_z = max(phi)}
  }
  min_z
  max_z
  rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
  for (i in c(a:b)) {
    phi = dat$phi[,,i]
    filled.contour(x=lon, y=rev(lat), z=phi, zlim=c(min_z,max_z), color.palette=rgb.palette, #levels=numeric,
                   plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
                   plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})
  }
},movie.name = "turkey_no_flood.gif")


saveVideo({
  #layout(matrix(c(1, rep(2, 5)), 6, 1))
  #par(mar=c(4,4,2,1) + 0.1)

  lon = dat$X - 180
  lat = dat$Y
  #a=16120
  #b=16130
  a = 15755
  b = 15765
  
  for (i in c(a:b)) {
    #plot.new()
    #pdf(file=sprintf("temp%02d.pdf", i), width=9, height=4)
    phi = dat$phi[,,i]
    rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
    filled.contour(x=lon, y=rev(lat), z=phi, color.palette=rgb.palette, #levels=numeric,
                   plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
                   plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})
    #dev.off()
  }
},video.name = "test_png.mp4", other.opts = "-b 1000k -s 1280x800")


############ PCA

start_date = as.Date("1948-01-01")
start_idx = as.numeric(as.Date("2015-01-01") - start_date)
end_idx = length(dat$T)
#end_idx = start_idx + 2
end_idx - start_idx

## rows are grid points (get similar times) 2160 x 24873
final = rbind(NULL)
for (x in c(1:length(dat$X))) {
  for (y in c(1:length(dat$Y))) {
      final = rbind(final, c(dat$phi[x,y,start_idx:end_idx]))
  }
}

pc1 = princomp(final, cor=TRUE, scores=TRUE)
plot(pc1, type="l")
biplot(pc1)

## rows are times (get similar grid points)
pc2 = princomp(t(final), cor=TRUE, scores=TRUE)
plot(pc2, type="l")
biplot(pc2)

## identify flooded points

df = read.csv("Cleaned_main_cause.csv", strip.white = TRUE)
df$Began = as.Date(df$Began, format = "%Y-%m-%d")
df$Ended = as.Date(df$Ended, format = "%Y-%m-%d")
df = df[df$Began > as.Date("2015-01-01"),]
df = df = df[complete.cases(df),]
df = df[df$Centroid.Y > 35 & df$Centroid.Y < 70,]

closest_location = function(lon, lat, X_flood, Y_flood) {
  index_X = which(abs(lon-X_flood)==min(abs(lon-X_flood)))
  index_Y = which(abs(lat-Y_flood)==min(abs(lat-Y_flood)))
  print(index_X)
  print(index_Y)
  index_X * length(lat) + index_Y
}

idx = apply(df[,c("Centroid.X","Centroid.Y")], 1, function(x) closest_location(lon, lat, x["Centroid.X"], x["Centroid.Y"]))

vec = rep(0, dim(final)[1])
vec[idx] = 1

pc3 = prcomp(final, scale. = TRUE, cor=TRUE, scores=TRUE)
dfpca = data.frame(pc3$x[,1], pc3$x[,2], vec)
names(dfpca) = c("a", "b", "c")
ggplot(data=dfpca) + geom_point(aes(x=a, y=b, colour=c))
plot(pc3, type="l")
biplot(pc3)

### scaled matrix
final_scaled = t(scale(t(final)))
pc3 = prcomp(final_scaled, cor=TRUE, scores=TRUE)
dfpca = data.frame(pc3$x[,1], pc3$x[,2], vec)
names(dfpca) = c("PC1", "PC2", "c")

g = ggplot(data=dfpca) + geom_point(aes(x=PC1, y=PC2, colour=c, size=c))
g = g + labs(title="Grid Points PCA")
g = g + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
g = g + theme(legend.position="none")
g = g + theme(axis.text.x = element_text(size=14, vjust=1.0))
g = g + theme(axis.text.y = element_text(size=14))
g = g + theme(axis.title.x = element_text(size=15, vjust=-0.5))
g = g + theme(axis.title.y = element_text(size=15))
g

plot(pc3, main = "PC.Variance", type="l")
biplot(pc3)



close.nc(fid)