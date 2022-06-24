#' {raster}
#' Geographic Data Analysis and Modeling
library(raster)

#' Read a GeoTIFF file from disk
r <- raster("amro1k.tif")

#' Print info about the raster object
print(r)

#' Coordinate reference
crs(r)

#' Default plot: try resizing the window
plot(r)

#' Fancy colors
pal <- hcl.colors(25, "spectral", rev = TRUE)
plot(r, col = pal)

#' Create a 10x coarser version & plot it
r10 <- aggregate(r, 10)
plot(r10, col = pal)

#' Check the values
summary(values(r10))
dim(r10)
res(r10) # not too informative for long/lat

#' Understanding how the values are organized
m10 <- matrix(values(r10), nrow(r10), ncol(r10))
image(m10) # Hmmm...

m10 <- matrix(values(r10), nrow(r10), ncol(r10), byrow = TRUE)
image(m10) # Getting there ...

image(t(m10), ylim=c(1,0), xlim=c(0,1), col = pal, asp = 1)

#' Manipulating the values
values(r10) <- -1 * values(r10)
values(r10)[is.na(values(r10))] <- 1
plot(r10)

#' BUT: https://r-spatial.org/r/2022/04/12/evolution.html

#' {terra}
#' Spatial Data Analysis
#' https://rspatial.org/terra/pkg/index.html
library(terra)

rt <- rast("amro1k.tif")

print(rt)

plot(rt)

#' Multiple layers
rs <- c(rt, rt * 2, rt * -1)
dim(rs)
nlyr(rs)
res(rs)

plot(rs)

hist(rs)
density(rs)

persp(rt)
contour(rt)


#' {leaflet}
#' Create Interactive Web Maps with the JavaScript 'Leaflet' Library
#' https://rstudio.github.io/leaflet/
library(leaflet)

#' Blank page
leaflet()

#' Show me YEG
m <- leaflet() |>
  setView(lng = -113.4938, lat = 53.5461, zoom = 12) |>
  addTiles()
m

#' Provider tiles
head(providers)
m |> addProviderTiles(providers$CartoDB.Positron)

#' Markers: popup and label
str(quakes)
leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat,
    popup = ~as.character(mag), 
    label = ~as.character(mag))

#' Color functions
pal <- colorNumeric(
  palette = "viridis", 
  domain = quakes$mag)
#' Read about the arguments
?colorNumeric

leaflet(quakes) |>
  addTiles() |>
  addCircleMarkers(
    stroke = FALSE,
    fillOpacity = 1,
    color = ~pal(mag))

#' Cluster the points
leaflet(quakes) |>
  addTiles() |>
  addCircleMarkers(
    stroke = FALSE,
    fillOpacity = 1,
    color = ~pal(mag),
    clusterOptions = markerClusterOptions())

#' Add raster to leaflet
r <- raster("amro1k.tif")
r <- aggregate(r, 10)
pal <- colorNumeric(
  palette = "magma", 
  domain = values(r),
  na.color = "transparent")

#' Add the raster to the map
m <- leaflet() |>
  addTiles() |>
  addRasterImage(r, 
    colors = pal, 
    opacity = 0.8)
m

# m <- leaflet()
# m <- addTiles(m)
# m <- addRasterImage(m, r, ...)

#' Add legend
m |>
  addLegend(pal = pal, 
    values = values(r),
    title = "Abundance")

#' {tiler}
#' Create Geographic and Non-Geographic Map Tiles
#' https://cran.r-project.org/web/packages/tiler/vignettes/tiler-intro.html

#' Install tiler dependencies
#'
# sudo add-apt-repository ppa:ubuntugis/ppa && sudo apt-get update
# sudo apt-get update
# sudo apt-get install gdal-bin
# sudo apt-get install libgdal-dev
# export CPLUS_INCLUDE_PATH=/usr/include/gdal
# export C_INCLUDE_PATH=/usr/include/gdal
# apt install python3-pip
# pip install GDAL
# sudo apt install python-is-python3
# sudo apt install gdal-bin python3-gdal python3-gdal
# apt install r-base-core r-base-dev
# apt install r-cran-rgdal r-cran-raster r-cran-png
# R -q -e 'install.packages("tiler")'

library(tiler)

pal <- grDevices::hcl.colors(101, "spectral", rev = TRUE)

tile(
  file = "amro1k.tif", 
  tiles = "tiles",
  zoom = "0-7",
  col = pal)
# Reprojecting raster...
# Coloring raster...
# Preparing for tiling...
# [1] "/var/folders/wm/3rsg6gxd2r16r5thmb03nt7w0000gn/T//RtmpyBOtOy"
# Creating tiles. Please wait...
# Generating Base Tiles:
# 0...10...20...30...40...50...60...70...80...90...100
# Generating Overview Tiles:
# 0...10...20...30...40...50...60...70...80...90...100
# Creating tile viewer...
# Complete.
#'
#' now explore the contents of the tiles directory
#' check the xml file and preview.html
#' 
#' Also see: 
#' https://wiki.osgeo.org/wiki/Tile_Map_Service_Specification
#' https://www.google.com/search?q=tms+tiles
?tile
#' explain {z}/{x}/{y}.png vs. {z}/{x}/{-y}.png

#' TMS tiles from server:
#' https://wbi-nwt.analythium.app/api/v1/public/wbi-nwt/elements/bird-amro/landr-scfm-v4/2011/
tiles <- paste0("https://wbi-nwt.analythium.app/api/v1/public/",
  "wbi-nwt/elements/bird-amro/landr-scfm-v4/2011/",
  "tiles/{z}/{x}/{-y}.png")

leaflet::leaflet() |> 
  leaflet::addProviderTiles(
    provider = 'Esri.WorldImagery', 
    group = "ESRI",
    options = leaflet::providerTileOptions(
      zIndex = 200)) |>
  leaflet::setView(-120, 65, 5) |>
  leaflet::addTiles(
    urlTemplate = tiles,
    options = leaflet::tileOptions(
      maxNativeZoom = 10,
      opacity = 0.8,
      zIndex = 400))
#' Talk about zIndex

#' What if we want to change the palette?
#' Well...

#' {leafem}
#' 'leaflet' Extensions for 'mapview'
#' https://r-spatial.github.io/leafem/
library(leafem)

leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addGeotiff(
    url = "https://peter.solymos.org/testapi/amro1k.tif",
    project = FALSE,
    opacity = 0.8,
    colorOptions = colorOptions(
        palette = hcl.colors(50, palette = "inferno"), 
        domain = c(0, 0.62),
        na.color = "transparent"))

#' The issue was a tricky one: floating point representation of -Inf
#' https://github.com/r-spatial/leafem/issues/54

#' {stars}
#' Spatiotemporal Arrays, Raster and Vector Data Cubes
#' https://r-spatial.github.io/stars/
library(stars)
s <- st_as_stars(r)

print(s)

plot(s)

# write_stars(s, "amro1k-stars.tif", options = c("COMPRESS=LZW"))

#' Now back to leaflet: yess!
leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addGeotiff(
    url = "https://peter.solymos.org/testapi/amro1k-stars-small.tif",
    project = FALSE,
    opacity = 0.8,
    colorOptions = colorOptions(
        palette = hcl.colors(50, palette = "inferno"), 
        domain = c(0, 0.62),
        na.color = "transparent"))

#' Check out the Shiny app that uses GeoTIFF:
shiny::runApp("palettes.R")

#' Discussion points:
#' 
#' - What are the advantages disadvantages of 
#'   TMS vs GeoTIFF in interactive context?
#' - What can we tell about situations where
#'   comparing multiple rasters is needed?

#' What if I told you that
#' you can have the best of both worlds?
#' 
#' COG = Cloud Optimized GeoTIFF
#' https://www.cogeo.org/
#' https://r-spatial.org/r/2021/04/23/cloud-based-cubes.html
#' https://gdal.org/drivers/raster/cog.html

#' Can't write COGs from R:
#' https://tmieno2.github.io/R-as-GIS-for-Economists/read-write-stars.html
sf::st_drivers(what = "raster")["COG",]

#' Following https://geoexamples.com/other/2019/02/08/cog-tutorial.html/
#' Save GeoTIFF with right NA coding
# r <- raster("amro1k.tif")
# s <- st_as_stars(r)
# write_stars(s, "amro1k-over.tif", options = c("COMPRESS=LZW"))
#' Create overviews
# gdaladdo -r average amro1k-over.tif 2 4 8 16
#' Tiled and compressed GeoTIFF
# gdal_translate amro1k-over.tif amro1k-cog.tif -co COMPRESS=LZW -co TILED=YES

#' Still experimental: leafem:::addCOG()
#' https://github.com/r-spatial/mapview/issues/400

leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addGeotiff(
    url = "https://peter.solymos.org/testapi/amro1k-cog.tif",
    project = FALSE,
    opacity = 0.8,
    autozoom = FALSE,
    colorOptions = colorOptions(
        palette = hcl.colors(50, palette = "inferno"), 
        domain = c(0, 0.62),
        na.color = "transparent")) |>
  leaflet::setView(-120, 65, 10)

#' Cleanup
# unlink("tiles", recursive = TRUE)
# unlink("preview.html")
# unlink("amro1k-over.tif")
# unlink("amro1k-cog.tif")
