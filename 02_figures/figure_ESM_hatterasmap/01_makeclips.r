###
# 01_makeclips.r
# clip shapefiles for hatteras map

# libs
library(rgeos)
library(rgdal)
library(sp)

usgsproj <- sp::CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

# make a clip boundary
clip_poly <- rgeos::readWKT("POLYGON((-79 33, -73 33, -74 38, -79 38, -79 33))")
sp::proj4string(clip_poly) <- usgsproj

# load us coastline shp and clip
us <- rgdal::readOGR("nos80k/NOS80k.shp")
us_clip <- rgeos::gIntersection(us, clip_poly, byid = TRUE, drop_lower_td = TRUE)

# output result
us_clip <- as(us_clip, "SpatialPolygonsDataFrame")
rgdal::writeOGR(us_clip, "nos80k_clip", driver = "ESRI Shapefile", layer = "nos80k_clip")
