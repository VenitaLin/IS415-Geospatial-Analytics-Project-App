# Imports

packages = c('rgdal', 'sf', 'spdep', 'tmap', 'tidyverse', 'maptools', 'raster', 'spatstat','shiny','leaflet')
for (p in packages){
    library(p,character.only = T)
}


mpsz <- st_read(dsn = "data/geospatial/master-plan-2014-boundary/MP14_SUBZONE_WEB_PL.shp",
                layer = "MP14_SUBZONE_WEB_PL")

shinyServer(function(input, output, session) {
    
    # Sample function
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    

    output$kernelDensityMap <- renderLeaflet({
        # kernelDensityMap <- tm_shape(mpsz)
        kernelDensityMap <- qtm(mpsz)
        tmap_leaflet(kernelDensityMap)
    })

    output$spatPointMap <- renderLeaflet({
        # spatPointMap <- tm_shape(mpsz)
        spatPointMap <- qtm(mpsz)
        tmap_leaflet(spatPointMap)
    })
    
    output$accessibilityMap <- renderLeaflet({
        # accessibilityMap <- tm_shape(mpsz)
        accessibilityMap <- qtm(mpsz)
        tmap_leaflet(accessibilityMap)
    })

})
