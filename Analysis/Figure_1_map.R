# ------------------------------------------------------------------------------
# Figure 1: map with the three regions
# ------------------------------------------------------------------------------

library(sf)
library(marmap)
library(ggplot2)
library(patchwork)

sf_use_s2(FALSE)

# ---- helper functions -------------------------------------------------------

# longitude reached when travelling `dist_km` due east from (lon, lat)
dest_lon <- function(lon, lat, dist_km) {
  R    <- 6371.0088
  d    <- dist_km / R
  phi  <- lat * pi / 180
  lam  <- lon * pi / 180
  phi2 <- asin(sin(phi) * cos(d) + cos(phi) * sin(d) * cos(pi / 2))
  lam2 <- lam + atan2(sin(pi / 2) * sin(d) * cos(phi), cos(d) - sin(phi) * sin(phi2))
  lam2 * 180 / pi                      # no wrapping, so it also works for 0-360 longitudes
}

# white scale bar from 0 to `dist_km`, with labels below the bar
scale_bar <- function(lon, lat, dist_km = 500, label_offset = 0.5, size = 2.6) {
  lon_end <- dest_lon(lon, lat, dist_km)
  list(
    annotate("segment", x = lon, xend = lon_end, y = lat, yend = lat,
             colour = "white", linewidth = 0.6),
    annotate("text", x = lon, y = lat - label_offset, label = "0 km",
             colour = "white", size = size, fontface = "bold", vjust = 1),
    annotate("text", x = lon_end, y = lat - label_offset, label = paste(dist_km, "km"),
             colour = "white", size = size, fontface = "bold", vjust = 1)
  )
}

# panel letter in the top-left corner of the map
panel_tag <- function(label, lon, lat) {
  annotate("text", x = lon, y = lat, label = label, colour = "white",
           size = 4.5, hjust = 0, vjust = 1)
}

# bathymetry as a data frame (shift_360 = TRUE for maps across the dateline)
get_depth <- function(lon1, lon2, lat1, lat2, antimeridian = FALSE, shift_360 = FALSE) {
  b <- getNOAA.bathy(lon1 = lon1, lon2 = lon2, lat1 = lat1, lat2 = lat2,
                     resolution = 10, antimeridian = antimeridian)
  dep <- as.xyz(b)
  names(dep) <- c("Longitude", "Latitude", "depth")
  if (shift_360) dep$Longitude <- ifelse(dep$Longitude < 0, dep$Longitude + 360, dep$Longitude)
  dep$depth    <- ifelse(dep$depth >= -1, -1, dep$depth)
  dep$coldepth <- -log10(abs(dep$depth))
  dep
}

# depth points inside a region (for the 200 m contour and the shelf fraction)
depth_in_region <- function(dep, region) {
  pts <- st_as_sf(dep, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
  pts[lengths(st_intersects(pts, region)) > 0, ]
}

# common map layers
map_layers <- function(dep, depcont, land, region) {
  list(
    geom_tile(data = dep, aes(x = Longitude, y = Latitude, fill = coldepth)),
    geom_contour(data = depcont, aes(x = Longitude, y = Latitude, z = depth),
                 colour = "grey", linewidth = 0.25, breaks = -200),
    geom_sf(data = land, col = "black", fill = "black"),
    geom_sf(data = region, col = "white", fill = NA, linewidth = 0.4),
    labs(x = "Longitude", y = "Latitude"),
    theme(legend.position = "none")
  )
}

# ---- data ---------------------------------------------------------------------
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
land  <- st_union(st_make_valid(ctrys))
shape <- st_read("Data/MEOW shapefiles/meow_ecos.shp")

# ---- A: Northwest Europe -------------------------------------------------------
NWEU <- subset(shape, shape$ECOREGION %in% c("Baltic Sea", "North Sea", "Celtic Seas"))
NWEU <- st_union(st_make_valid(NWEU))
NWEU <- st_difference(st_make_valid(NWEU), st_make_valid(land))
area_NWEU <- st_area(NWEU)

dep_NWEU  <- get_depth(-17, 32, 47, 68)
cont_NWEU <- depth_in_region(dep_NWEU, NWEU)
mean(cont_NWEU$depth > -200)                         # fraction of the region < 200 m

NWEU_map <- ggplot() +
  map_layers(dep_NWEU, cont_NWEU, ctrys, NWEU) +
  coord_sf(xlim = c(-16.8, 29), ylim = c(47.5, 66), expand = FALSE) +
  panel_tag("A", -16, 65.3) +
  scale_bar(lon = 18, lat = 50) +
  theme(axis.title.x = element_blank())

# ---- B: Northwest Atlantic -----------------------------------------------------
NEUS <- subset(shape, shape$ECOREGION %in% c("Gulf of Maine/Bay of Fundy", "Virginian", "Scotian Shelf"))
NEUS <- st_union(st_make_valid(NEUS))
NEUS <- st_difference(st_make_valid(NEUS), st_make_valid(land))
area_NEUS <- st_area(NEUS)

dep_NEUS  <- get_depth(-81, -53, 31, 50)
cont_NEUS <- depth_in_region(dep_NEUS, NEUS)
mean(cont_NEUS$depth > -200)

NEUS_map <- ggplot() +
  map_layers(dep_NEUS, cont_NEUS, ctrys, NEUS) +
  coord_sf(xlim = c(-79, -55), ylim = c(33, 48), expand = FALSE) +
  scale_y_continuous(breaks = c(34, 38, 42, 46)) +
  panel_tag("B", -78.2, 47.4) +
  scale_bar(lon = -63, lat = 34.6, label_offset = 0.35) +
  theme(axis.title.x = element_blank())

# ---- C: Alaska (US waters only, plotted across the dateline) ---------------
ALA <- subset(shape, shape$ECOREGION %in% c("Gulf of Alaska", "Aleutian Islands", "Eastern Bering Sea"))
ALA <- st_union(st_make_valid(ALA))
ALA <- st_difference(st_make_valid(ALA), st_make_valid(land))

eez <- st_read("Data/EEZ_land_union/EEZ_land_v2_201410.shp")
rus <- st_union(st_make_valid(eez[eez$Country == "Russia", ]))
rus <- st_union(st_buffer(st_make_valid(rus), 0.01))      # closes the gap at the dateline
ALA <- st_difference(st_make_valid(ALA), rus)
area_ALA <- st_area(ALA)                                  # area before any plotting buffers

ALA360 <- st_shift_longitude(st_make_valid(ALA))
ALA360 <- st_union(st_buffer(ALA360, 0.01))               # dissolve the dateline seam
ALA360 <- st_buffer(st_buffer(ALA360, -0.05), 0.05)       # remove slivers

ctrys_valid <- st_geometry(st_make_valid(ctrys))
ctrys_east  <- st_crop(ctrys_valid, xmin = 160,  xmax = 180,  ymin = 40, ymax = 75)
ctrys_west  <- st_crop(ctrys_valid, xmin = -180, xmax = -125, ymin = 40, ymax = 75)
ctrys360    <- c(ctrys_east, st_shift_longitude(ctrys_west))

dep_ALA  <- get_depth(165, -125, 45, 70, antimeridian = TRUE, shift_360 = TRUE)
cont_ALA <- depth_in_region(dep_ALA, ALA360)
mean(cont_ALA$depth > -200)

lon_lab <- function(x) {
  x <- ifelse(x > 180, x - 360, x)
  paste0(abs(x), "°", ifelse(x < 0, "W", "E"))
}

ALA_map <- ggplot() +
  map_layers(dep_ALA, cont_ALA, ctrys360, ALA360) +
  coord_sf(xlim = c(166, 231), ylim = c(45, 69), expand = FALSE) +
  scale_x_continuous(breaks = c(170, 190, 210, 230), labels = lon_lab) +
  scale_y_continuous(breaks = c(46, 53, 60, 67)) +
  panel_tag("C", 167.5, 68) +
  scale_bar(lon = 214, lat = 48, label_offset = 0.5)

# ---- combine ------------------------------------------------------------------
asp <- 0.75                                               # same height/width for all maps

fig1 <- (NWEU_map + theme(aspect.ratio = asp)) /
  (NEUS_map + theme(aspect.ratio = asp)) /
  (ALA_map  + theme(aspect.ratio = asp))

pdf("Output/Figure_1_map.pdf", width = 5, height = 10.5)
print(fig1)
dev.off()