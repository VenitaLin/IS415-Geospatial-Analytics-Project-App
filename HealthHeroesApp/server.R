# IMPORTS
#===================================================================================================
library('rgdal')
library('sf')
library('spdep')
library('tmap')
library('dplyr')
library('tidyverse')
library('maptools')
library('raster')
library('broom')
library('spatstat')
library('shiny')
library('leaflet')
library('spData')
library('DT')
#===================================================================================================



# COASTAL OUTLINE
#===================================================================================================
sg <- readOGR(dsn = "data/geospatial/coastal-outline", layer="CostalOutline")
sg_sp <- as(sg, "SpatialPolygons")
#===================================================================================================



# MPSZ
#===================================================================================================
mpsz <- readRDS("data/rds/mpsz.RDS")
# mpsz <- readOGR(dsn = "data/geospatial/master-plan-2014-boundary",
#                 layer = "MP14_SUBZONE_WEB_PL")
# save(mpsz, file="data/rds/mpsz.RDS")
#===================================================================================================



# GYM
#===================================================================================================
gym <- readOGR(dsn = "data/geospatial/gymssg/gyms-sg.shp", layer = "gyms-sg")

gym <- gym[ , !(names(gym) %in% c("descriptio", "timestamp", "begin", "end", "altitudeMo", 
                                  "drawOrder", "icon", "HYPERLINK", "PHOTOURL", "snippet", "tessellate",
                                  "extrude", "visibility", "LANDYADDRE", "LANDXADDRE", "ADDRESSTYP", "ADDRESSFLO", 
                                  "INC_CRC", "FMEL_UPD_D", "ADDRESSUNI"))]

gym_sp <- as(gym, "SpatialPoints")

gym_ppp <- as(gym_sp, "ppp")
gym_ppp <- rjitter(gym_ppp, retry=TRUE, nsim=1, drop=TRUE)
#===================================================================================================


# EATERIES
#===================================================================================================
eat <- readOGR(dsn = "data/geospatial/healthier-eateries/healthier-eateries.shp",
               layer = "healthier-eateries")

eat <- eat[ , !(names(eat) %in% c("descriptio", "timestamp", "begin", "end", "altitudeMo", 
                                  "drawOrder", "icon", "HYPERLINK", "PHOTOURL", "snippet", "tessellate",
                                  "extrude", "visibility", "LANDYADDRE", "LANDXADDR"))]
eat <- eat[!grepl("McDonald's", eat$Name),]
eat <- eat[!grepl("Mcdonald's", eat$Name),]
eat <- eat[!grepl("KOI", eat$Name),]

eat_sp <- as(eat, "SpatialPoints")

eat_ppp <- as(eat_sp, "ppp")
eat_ppp <- rjitter(eat_ppp, retry=TRUE, nsim=1, drop=TRUE)
#===================================================================================================



# PLACES
#===================================================================================================
places <- readOGR(dsn = "data/geospatial/Singapore-shp/shape/places.shp",
                  layer = "places")

print(summary(places))
# print(unique(places$Name))

places_sp <- as(places, "SpatialPoints")
places_sp <- spTransform(places_sp, crs(eat_sp))

places_ppp <- as.ppp.SpatialPoints(places_sp)

places_ppp <- rjitter(places_ppp, retry=TRUE, nsim=1, drop=TRUE)
#===================================================================================================






# SHINY SERVER
#===================================================================================================

shinyServer(function(input, output, session) {

    
    updateSelectInput(session, "secondOrderSelectPlanningArea",
                      choices = sort(unique(mpsz$PLN_AREA_N)))

    
    # SUMMARY
    #===============================================================================================
    output$summaryDataTable <- renderDataTable(datatable({
        
        # SELECT AMENITY
        #===========================================================================================
        if (input$summaryPointSelectAmenity == "gym") { as.data.frame(gym) } 
        else if (input$summaryPointSelectAmenity == "eat") { as.data.frame(eat) }
        else if (input$summaryPointSelectAmenity == "places") { as.data.frame(places) }
        #===========================================================================================
        
    }))
    
    output$summaryPointMap <- renderLeaflet({
        
        # SELECT AMENITY
        #===========================================================================================
        if (input$summaryPointSelectAmenity == "gym") { 
            pointsToShow <- gym
            pointColor <- "red"
        } 
        else if (input$summaryPointSelectAmenity == "eat") { 
            pointsToShow <- eat
            pointColor <- "blue" 
        }
        else if (input$summaryPointSelectAmenity == "places") { 
            pointsToShow <- places
            pointColor <- "green" 
        }
        #===========================================================================================
        
         summaryPointMap <-
             tm_shape(mpsz) +
                tm_polygons() +
                tm_facets("REGION_N",
                          as.layers = T) +
             
             tm_shape(pointsToShow) +
                tm_bubbles(col=pointColor, size=0.15, border.col="black", border.lwd=1)
             
         tmap_leaflet(summaryPointMap)
        
    })
    #===============================================================================================
    
    
    # CHOROPLETH
    #===============================================================================================
    output$choroplethMap <- renderLeaflet({
        choroplethMap <- qtm(mpsz)
        tmap_leaflet(choroplethMap)
    })
    #===============================================================================================
    
    
    # KERNEL DENSITY
    #===============================================================================================
    output$kernelDensityMap <- 
        
        renderPlot({
        # renderLeaflet({

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
        spplot(gridded_kde)
    })
    
    #===============================================================================================

    
    
    
    # 2ND ORDER
    #===============================================================================================
    
    output$secondOrderEstimationPlot <- renderPlot({
        
        # SELECT PLANNING AREA
        #===========================================================================================
        area <- mpsz[mpsz@data$PLN_AREA_N == input$secondOrderSelectPlanningArea,]
        area_sp <- as(area, "SpatialPolygons")
        selected_owin <- as(area_sp, "owin")
        #===========================================================================================
        
        # SELECT AMENITY
        #===========================================================================================
        if (input$secondOrderSelectAmenity == "gym") { 
            ppp_selected <- rescale(gym_ppp[selected_owin], 1000, "km")
        } 
        else if (input$secondOrderSelectAmenity == "eat") { 
            ppp_selected <- rescale(eat_ppp[selected_owin], 1000, "km")
        }
        else if (input$secondOrderSelectAmenity == "places") { 
            ppp_selected <- rescale(places_ppp[selected_owin], 1000, "km")
        }
        #===========================================================================================
        
        # SELECT FUNCTION
        #===========================================================================================
        if (input$secondOrderSelectFunction == "G") {
            G_ppp = Gest(ppp_selected, correction = "border")
            plot(G_ppp)
            
        }
        else if (input$secondOrderSelectFunction == "F") {
            F_ppp = Fest(ppp_selected)
            plot(F_ppp)
        }
        else if (input$secondOrderSelectFunction == "K") {
            K_ppp = Kest(ppp_selected, correction = "Ripley")
            plot(K_ppp, . -r ~ r, ylab= "K(d)-r", xlab = "d(m)")
        }
        else if (input$secondOrderSelectFunction == "L") {
            L_ppp = Lest(ppp_selected, correction = "Ripley")
            plot(L_ppp, . -r ~ r, ylab= "L(d)-r", xlab = "d(m)")
        }
        #===========================================================================================
        
    })
    
    output$secondOrderCompleteSpatRandPlot <- renderPlot({
        
        # SELECT PLANNING AREA
        #===========================================================================================
        area <- mpsz[mpsz@data$PLN_AREA_N == input$secondOrderSelectPlanningArea,]
        area_sp <- as(area, "SpatialPolygons")
        selected_owin <- as(area_sp, "owin")
        #===========================================================================================
        
        # SELECT AMENITY
        #===========================================================================================
        if (input$secondOrderSelectAmenity == "gym") { 
            ppp_selected <- rescale(gym_ppp[selected_owin], 1000, "km")
        } 
        else if (input$secondOrderSelectAmenity == "eat") { 
            ppp_selected <- rescale(eat_ppp[selected_owin], 1000, "km")
        }
        else if (input$secondOrderSelectAmenity == "places") { 
            ppp_selected <- rescale(places_ppp[selected_owin], 1000, "km")
        }
        #===========================================================================================
        
        # SELECT FUNCTION
        #===========================================================================================
        if (input$secondOrderSelectFunction == "G") {
            G_ppp.csr <- envelope(ppp_selected, Gest, correction = "all", nsim = input$secondOrderSliderNsim_1)
            plot(G_ppp.csr)
        }
        else if (input$secondOrderSelectFunction == "F") {
            F_ppp.csr <- envelope(ppp_selected, Fest, nsim = input$secondOrderSliderNsim_1)
            plot(F_ppp.csr)
        }
        else if (input$secondOrderSelectFunction == "K") {
            K_ppp.csr <- envelope(ppp_selected, Kest, nsim = input$secondOrderSliderNsim_2, rank = 1, glocal=TRUE)
            plot(K_ppp.csr, . - r ~ r, xlab="d", ylab="K(d)-r")
        }
        else if (input$secondOrderSelectFunction == "L") {
            L_ppp.csr <- envelope(ppp_selected, Lest, nsim = input$secondOrderSliderNsim_2, rank = 1, glocal=TRUE)
            plot(L_ppp.csr, . - r ~ r, xlab="d", ylab="L(d)-r")
        }
        #===========================================================================================
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
