# IMPORTS
#===================================================================================================
packages = c('rgdal', 'sf', 'spdep', 'tmap', 
             'tidyverse', 'maptools', 'raster', 
             'spatstat', 'shiny','leaflet', 'spData'
             )
for (p in packages){
    library(p,character.only = T)
}
#===================================================================================================



# COASTAL OUTLINE
#===================================================================================================
sg <- readOGR(dsn = "data/geospatial/coastal-outline", layer="CostalOutline")
sg_sp <- as(sg, "SpatialPolygons")
#===================================================================================================



# MPSZ
#===================================================================================================
mpsz <- readOGR(dsn = "data/geospatial/master-plan-2014-boundary",
                layer = "MP14_SUBZONE_WEB_PL")
#===================================================================================================



# GYM
#===================================================================================================
gym <- readOGR(dsn = "data/geospatial/gymssg/gyms-sg.shp", layer = "gyms-sg")

gym_sp <- as(gym, "SpatialPoints")

gym_ppp <- as(gym_sp, "ppp")
gym_ppp <- rjitter(gym_ppp, retry=TRUE, nsim=1, drop=TRUE)
#===================================================================================================


# EATERIES
#===================================================================================================

eat <- readOGR(dsn = "data/geospatial/healthier-eateries/healthier-eateries.shp",
               layer = "healthier-eateries")

eat_sp <- as(eat, "SpatialPoints")

eat_ppp <- as(eat_sp, "ppp")
eat_ppp <- rjitter(eat_ppp, retry=TRUE, nsim=1, drop=TRUE)

#===================================================================================================



# PLACES
#===================================================================================================
places <- readOGR(dsn = "data/geospatial/Singapore-shp/shape/places.shp",
                  layer = "places")

places_sp <- as(places, "SpatialPoints")
places_sp <- spTransform(places_sp, crs(eat_sp))

places_ppp <- as.ppp.SpatialPoints(places_sp)

places_ppp <- rjitter(places_ppp, retry=TRUE, nsim=1, drop=TRUE)
#===================================================================================================






# SHINY SERVER
#===================================================================================================

shinyServer(function(input, output, session) {
    
    # SAMPLE
    #===============================================================================================
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    #===============================================================================================

    # KERNEL DENSITY
    #===============================================================================================
    output$kernelDensityMap <- renderLeaflet({


        # SELECT REGION
        #===========================================================================================
        if (input$kernelDensitySelectRegion == "all") {
            selected_owin <- as(sg_sp, "owin")
        } 
        else {
            region <- mpsz[mpsz@data$REGION_N == input$kernelDensitySelectRegion,]
            region_sp <- as(region, "SpatialPolygons")
            selected_owin <- as(region_sp, "owin")   
        }
        #===========================================================================================
        
        
        # SELECT AMENITY
        #===========================================================================================
        if (input$kernelDensitySelectAmenity == "gym") { 
            ppp_selected <- rescale(gym_ppp[selected_owin], 1000, "km")
        } 
        else if (input$kernelDensitySelectAmenity == "eat") { 
            ppp_selected <- rescale(eat_ppp[selected_owin], 1000, "km")
        }
        else if (input$kernelDensitySelectAmenity == "places") { 
            ppp_selected <- rescale(places_ppp[selected_owin], 1000, "km")
        }
        #===========================================================================================
        
        
        # SELECT BANDWIDTH
        #===========================================================================================
        if (input$kernelDensitySelectBandwidth == "manual") {
            kde <- density(ppp_selected, 
                           sigma=(input$kernelDensitySliderBandwidth/1000), edge=TRUE, kernel=input$kernelDensitySelectKernelMethod)   
        }
        else if (input$kernelDensitySelectBandwidth == "adaptive") {
            kde <- adaptive.density(ppp_selected, method="kernel")   
        }
        else if (input$kernelDensitySelectBandwidth == "bw.diggle") {
            kde <- density(ppp_selected, 
                           sigma=bw.diggle, edge=TRUE, kernel=input$kernelDensitySelectKernelMethod) 
        }
        else if (input$kernelDensitySelectBandwidth == "bw.scott") {
            kde <- density(ppp_selected, 
                           sigma=bw.scott, edge=TRUE, kernel=input$kernelDensitySelectKernelMethod) 
        }
        else if (input$kernelDensitySelectBandwidth == "bw.ppl") {
            kde <- density(ppp_selected, 
                           sigma=bw.ppl, edge=TRUE, kernel=input$kernelDensitySelectKernelMethod) 
        }
        #===========================================================================================

                
        gridded_kde <- as.SpatialGridDataFrame.im(kde)
        kde_raster <- raster(gridded_kde)

        kernelDensityMap <- 
            tm_shape(kde_raster) + 
            tm_raster("v") 
        tmap_leaflet(kernelDensityMap)
        
        
    })
    #===============================================================================================

    # SPATIAL POINT
    #===============================================================================================
    output$spatPointMap <- renderLeaflet({
        # spatPointMap <- tm_shape(mpsz)
        spatPointMap <- qtm(mpsz)
        tmap_leaflet(spatPointMap)
    })
    #===============================================================================================

    
    # ACCESSIBILITY
    #===============================================================================================
    output$accessibilityMap <- renderLeaflet({
        # accessibilityMap <- tm_shape(mpsz)
        accessibilityMap <- qtm(mpsz)
        tmap_leaflet(accessibilityMap)
    })
    #===============================================================================================

})

#===================================================================================================