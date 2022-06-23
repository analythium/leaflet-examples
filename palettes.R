library(shiny)
library(leaflet)
library(leafem)

get_pal <- function(n=50, type=c("viridis", "rdylbu", "spectral", "bam")) {
  switch(match.arg(type),
    "viridis" = viridis::viridis_pal(option = "D")(n),
    "rdylbu" = grDevices::hcl.colors(n, "RdYlBu", rev = TRUE),
    "spectral" = grDevices::hcl.colors(n, "spectral", rev = TRUE),
    "bam" = grDevices::colorRampPalette(
        c("#FFFACD", "lemonchiffon","#FFF68F", "khaki1","#ADFF2F", 
        "greenyellow", "#00CD00", "green3", "#48D1CC", "mediumturquoise", 
        "#007FFF", "blue"), space="Lab", bias=0.5)(n))
}
base <- function(type) {
  map_attr = "© <a href='https://www.esri.com/en-us/home'>ESRI</a> © <a href='https://www.google.com/maps/'>Google</a>"
  leaflet() |>
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",
          group  =  "Google") |>
  addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
  addProviderTiles("OpenStreetMap", group = "Open Street Map") |>
  addProviderTiles('Esri.WorldImagery', group = "ESRI") |>
  addTiles(urlTemplate = "", attribution = map_attr) |>
  addLayersControl(
    baseGroups = c("ESRI", "Open Street Map", "CartoDB", "Google"),
    options = layersControlOptions(collapsed = FALSE)) |>
  setView(-120, 65, 5)

}
lfun <- function(map, type="viridis", opacity=0.8) {
  addGeotiff(map,
      url = "https://peter.solymos.org/testapi/amro1k-stars-small.tif",
      project = FALSE,
      opacity = opacity,
      autozoom = FALSE,
      layerId = "raster",
      colorOptions = colorOptions(
        palette = get_pal(50, type), 
        domain = c(0, 0.62),
        na.color = "transparent")) |>
    addLegend(pal = colorNumeric(palette=get_pal(50, type), 
                                 domain = c(0, 0.62)), 
      values = c(0, 0.62),
      opacity = opacity,
      title = "Abundance")
}

shinyApp(
  ui=fluidPage(
    sidebarLayout(
      sidebarPanel(width=4,
        h1("Color Palettes"),
        p("Try the palettes and change the base map."),
        sliderInput("opacity", label = "Opacity:", 
          min = 0, value = 0.8, max = 1),
        selectInput("type", "Color palette:", 
          c("viridis", "rdylbu", "spectral", "bam"))
      ),
       mainPanel(width=8,
        leafletOutput("map", width = "100%", height = 600)
       )
    )
  ),
  server=function(input, output) {
    output$map <- renderLeaflet({
      base() |>
        lfun(input$type, input$opacity) |>
        htmlwidgets::onRender("
          function(el, x) {
            this.on('baselayerchange', function(e) {
              e.layer.bringToBack();
            })
          }
        ")
    })
  }
)

