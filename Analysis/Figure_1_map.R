# ------------------------------------------------------------------------------
# create map with the three regions
# ------------------------------------------------------------------------------

# load libraries
library(sf)
library(gridExtra)
library(marmap)
library(ggplot2)

# load function to add scalebar
source("Analysis/Function_add_scalebar.R")

# load polygon with countries and marine ecoregions
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
land  <- st_union(st_make_valid(ctrys))
shape <- st_read("Data/MEOW shapefiles/meow_ecos.shp")

# create map for NW-EU
NWEU <- subset(shape,shape$ECOREGION %in% c("Baltic Sea","North Sea","Celtic Seas"))
NWEU <- st_union(st_make_valid(NWEU))
NWEU <- st_difference(st_make_valid(NWEU),st_make_valid(land))
area_NWEU <- st_area(NWEU)

minlong <- -15.56598
maxlong <- 31.79023
minlat  <- 47 
maxlat  <- 67.83435
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

# load depth
papoue <- getNOAA.bathy(lon1 = -17, lon2 = 32,
                        lat1 = 47, lat2 = 68, resolution = 10)
# change raster to dataframe
dep <- data.frame(Longitude= rep(as.numeric(rownames(papoue)),ncol(papoue)),
                  Latitude=rep(as.numeric(colnames(papoue)),each=nrow(papoue)))
depth <- c()
for (j in 1:ncol(papoue)){
  depth <- c(depth,papoue[,j])
  }
dep$depth <- depth
dep$depth <- ifelse(dep$depth >= -1,-1,dep$depth)
dep$coldepth <- log10(abs(dep$depth))
dep$coldepth <- dep$coldepth*-1

depcont <- dep
depcont$xlong2 <- depcont$Longitude
depcont$ylat2 <- depcont$Latitude
depcont <- st_as_sf(depcont, coords = c("Longitude","Latitude"), remove = FALSE)
st_crs(depcont) = 4326
depin <- as.data.frame(st_intersects(depcont,NWEU))
depcont <- depcont[depin$row.id,]

# fraction of depth less 200m
nrow(subset(depcont,depcont$depth > -200))/nrow(depcont)

NWEU_map <- ggplot() +
  geom_point(data=dep,aes(x=Longitude,y=Latitude,col=coldepth),cex=0.35,pch=15)+
  geom_contour(data=depcont,aes(x=xlong2,y=ylat2,z=depth),colour = "grey",lty=1,size=0.25,
               breaks = c(-200))+
  geom_sf(data=ctrys,col="black",fill= "black") +
  geom_sf(data=NWEU,col="white",fill=NA, lwd=0.4)+
 coord_sf(xlim = c(minlong+1,maxlong-3),ylim=c(minlat+1,maxlat-2))+
  theme(legend.position = "none")  +  
  scaleBar(lon = 18.05, lat = 50, distanceLon = 500,
                 distanceLat = 10, distanceLegend = -50, dist.unit = "km", orientation =F)

# north east US
NEUS <- subset(shape,shape$ECOREGION %in% c("Gulf of Maine/Bay of Fundy","Virginian","Scotian Shelf"))
NEUS <- st_union(st_make_valid(NEUS))
NEUS <- st_difference(st_make_valid(NEUS),st_make_valid(land))
area_NEUS <- st_area(NEUS)

minlong <- -80
maxlong <- -54
minlat  <- 32 
maxlat  <- 49
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

# load depth
papoue <- getNOAA.bathy(lon1 = -81, lon2 = -53,
                        lat1 = 31, lat2 = 50, resolution = 10)
# change raster to dataframe
dep <- data.frame(Longitude= rep(as.numeric(rownames(papoue)),ncol(papoue)),
                  Latitude=rep(as.numeric(colnames(papoue)),each=nrow(papoue)))
depth <- c()
for (j in 1:ncol(papoue)){
  depth <- c(depth,papoue[,j])
}
dep$depth <- depth
dep$depth <- ifelse(dep$depth >= -1,-1,dep$depth)
dep$coldepth <- log10(abs(dep$depth))
dep$coldepth <- dep$coldepth*-1

depcont <- dep
depcont$xlong2 <- depcont$Longitude
depcont$ylat2 <- depcont$Latitude
depcont <- st_as_sf(depcont, coords = c("Longitude","Latitude"), remove = FALSE)
st_crs(depcont) = 4326
depin <- as.data.frame(st_intersects(depcont,NEUS))
depcont <- depcont[depin$row.id,]

# fraction of depth less 200m
nrow(subset(depcont,depcont$depth > -200))/nrow(depcont)

NEUS_map <- ggplot() +
            geom_point(data=dep,aes(x=Longitude,y=Latitude,col=coldepth),cex=0.5,pch=15)+
            geom_contour(data=depcont,aes(x=xlong2,y=ylat2,z=depth),colour = "grey",lty=1,size=0.25,
                         breaks = c(-200))+
            geom_sf(data=ctrys,col="black",fill= "black") +
            geom_sf(data=NEUS,col="white",fill=NA, lwd=0.4)+
            coord_sf(xlim = c(minlong+1,maxlong-1),ylim=c(minlat+1,maxlat-1)) +
            theme(legend.position = "none")+
                  scale_y_continuous(breaks = c(34,38,42,46)) +
            scaleBar(lon = -65, lat = 34, distanceLon = 500,
              distanceLat = 10, distanceLegend = -50, dist.unit = "km", orientation =F)


# Alaska
ALA <- subset(shape,shape$ECOREGION %in% c("Gulf of Alaska","Aleutian Islands","Eastern Bering Sea"))
ALA <- st_union(st_make_valid(ALA))
ALA <- st_difference(st_make_valid(ALA),st_make_valid(land))
area_ALA <- st_area(ALA)

minlong <- -180
maxlong <- -130
minlat  <- 45 
maxlat  <- 70
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

# load depth

papoue <- getNOAA.bathy(lon1 = -180, lon2 = -120,
                        lat1 = 45, lat2 = 70, resolution = 10)
# change raster to dataframe
dep <- data.frame(Longitude= rep(as.numeric(rownames(papoue)),ncol(papoue)),
                  Latitude=rep(as.numeric(colnames(papoue)),each=nrow(papoue)))
depth <- c()
for (j in 1:ncol(papoue)){
  depth <- c(depth,papoue[,j])
}
dep$depth <- depth
dep$depth <- ifelse(dep$depth >= -1,-1,dep$depth)
dep$coldepth <- log10(abs(dep$depth))
dep$coldepth <- dep$coldepth*-1

depcont <- dep
depcont$xlong2 <- depcont$Longitude
depcont$ylat2 <- depcont$Latitude
depcont <- st_as_sf(depcont, coords = c("Longitude","Latitude"), remove = FALSE)
st_crs(depcont) = 4326
depin <- as.data.frame(st_intersects(depcont,ALA))
depcont <- depcont[depin$row.id,]

# fraction of depth less 200m, need to add below
idx1 <- nrow(subset(depcont,depcont$depth > -200))
idx2 <- nrow(depcont)

library(ggplot2)
al1 <- ggplot() +
  geom_point(data=dep,aes(x=Longitude,y=Latitude,col=coldepth),cex=0.3,pch=15)+
  geom_contour(data=depcont,aes(x=xlong2,y=ylat2,z=depth),colour = "grey",lty=1,size=0.25,
               breaks = c(-200))+
  geom_sf(data=ctrys,col="black",fill= "black") +
  geom_sf(data=ALA,col="white",fill=NA, lwd=0.4)+
  coord_sf(xlim = c(minlong+2.5,maxlong),ylim=c(minlat+1,maxlat-1)) +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none",
        plot.margin = unit(c(10, 10, 10, 0), "pt"))+
  scale_x_continuous(breaks = c(-170,-150,-130)) +
  scaleBar(lon = -140, lat = 48, distanceLon = 500,
           distanceLat = 10, distanceLegend = -50, dist.unit = "km", orientation =F)

minlong <- 165
maxlong <- 180
minlat  <- 45 
maxlat  <- 70
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

# load depth
papoue <- getNOAA.bathy(lon1 = 165, lon2 = 180,
                        lat1 = 45, lat2 = 70, resolution = 10)
# change raster to dataframe
dep <- data.frame(Longitude= rep(as.numeric(rownames(papoue)),ncol(papoue)),
                  Latitude=rep(as.numeric(colnames(papoue)),each=nrow(papoue)))
depth <- c()
for (j in 1:ncol(papoue)){
  depth <- c(depth,papoue[,j])
}
dep$depth <- depth
dep$depth <- ifelse(dep$depth >= -1,-1,dep$depth)
dep$coldepth <- log10(abs(dep$depth))
dep$coldepth <- dep$coldepth*-1

depcont <- dep
depcont$xlong2 <- depcont$Longitude
depcont$ylat2 <- depcont$Latitude
depcont <- st_as_sf(depcont, coords = c("Longitude","Latitude"), remove = FALSE)
st_crs(depcont) = 4326
depin <- as.data.frame(st_intersects(depcont,ALA))
depcont <- depcont[depin$row.id,]

# fraction of depth less 200m
(nrow(subset(depcont,depcont$depth > -200))+idx1)/(nrow(depcont)+idx2)

al2 <- ggplot() +
  geom_point(data=dep,aes(x=Longitude,y=Latitude,col=coldepth),cex=0.2,pch=15)+
  geom_contour(data=depcont,aes(x=xlong2,y=ylat2,z=depth),colour = "grey",lty=1,size=0.25,
               breaks = c(-200))+
  geom_sf(data=ctrys,col="black",fill= "black") +
  geom_sf(data=ALA,col="white",fill=NA, lwd=.4)+
  coord_sf(xlim = c(minlong+1,maxlong-0.8),ylim=c(minlat+1,maxlat-1)) +
  theme(legend.position = "none", axis.title.x=element_blank(),
        plot.margin = unit(c(5, 0, 18, 5.5), "pt"))+
  scale_x_continuous(breaks = c(170)) + 
  scale_y_continuous(breaks = c(46,53,60,67))

pdf("Output/Figure_1_map.pdf",width = 11, height = 5)
grid.arrange(NWEU_map,NEUS_map,al2,al1, nrow = 1,widths = c(2.5,2.35,.88,1.77))
dev.off()

