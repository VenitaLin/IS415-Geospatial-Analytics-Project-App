# IMPORTS
#===================================================================================================
library('rgdal')
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
library("tmaptools")
library("ggthemes")
library("ggplot2")
#===================================================================================================

# COASTAL OUTLINE
#===================================================================================================
sg_sp <- readRDS("data/rds/sg_sp.rds")
#===================================================================================================



# MPSZ
#===================================================================================================
mpsz <- readRDS("data/rds/mpsz.rds")
mpsz_sp <- readRDS("data/rds/mpsz_sp.rds")
mpsz_svy21 <- readRDS("data/rds/mpsz_svy21.rds")
#===================================================================================================


# HDB
#===================================================================================================
hdb_sf <- readRDS("data/rds/hdb_sf.rds")
#===================================================================================================


# GYM
#===================================================================================================
gym <- readRDS("data/rds/gym.rds")
gym_ppp <- readRDS("data/rds/gym_ppp.rds")
gym_sf <- readRDS("data/rds/gym_sf.rds")
#===================================================================================================


# EATERIES
#===================================================================================================
eat <- readRDS("data/rds/eat.rds")
eat_ppp <- readRDS("data/rds/eat_ppp.rds")
eat_sf <- readRDS("data/rds/eat_sf.rds")
hdb_hansen_eat_sub <- readRDS("data/rds/hdb_hansen_eat_sub.rds")
#===================================================================================================



# PLACES
#===================================================================================================
# places <- readOGR(dsn = "data/geospatial/Singapore-shp/shape/places.shp",
#                   layer = "places")
# saveRDS(places, "data/rds/places.rds")
places <- readRDS("data/rds/places.rds")
# places_sp <- as(places, "SpatialPoints")
# places_sp <- spTransform(places_sp, crs(eat_sp))
# places_ppp <- as.SpatialPoints.ppp(places_sp)
# places_ppp <- as(places_sp, "ppp")
# 
# places_ppp <- rjitter(places_ppp, retry=TRUE, nsim=1, drop=TRUE)
# 
# saveRDS(places_ppp, "data/rds/places_ppp.rds")
places_ppp <- readRDS("data/rds/places_ppp.rds")
#===================================================================================================






# SHINY SERVER
#===================================================================================================

shinyServer(function(input, output, session) {
    
    
    updateSelectInput(session, "secondOrderSelectPlanningArea",
                      choices = c("(All)", sort(unique(mpsz$PLN_AREA_N))))
    
    
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
            tm_shape(pointsToShow) +
            tm_bubbles(col=pointColor, size=0.15, border.col="black", border.lwd=1) +
            tmap_options(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldTopoMap")) +
            tm_view(set.zoom.limits = c(11, 14))
        
        tmap_leaflet(summaryPointMap)
        
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
            # mpsz_bbox <- st_bbox(sg_sp)
            # osm <- read_osm(mpsz_bbox, ext=1.1)
            # kde_raster <- raster(gridded_kde)
            # projection(kde_raster) <- crs("+init=EPSG:3414 +datum=WGS84 +units=km")
            # kmap <- tm_shape(osm) +
            #     tm_rgb() +
            #     tm_shape(kde_raster) +
            #     # tm_shape(sz) +
            #     # tm_borders(col = "black", lwd = 2, lty="longdash") +
            #     tm_raster("v", alpha=0.5, palette = "BuPu") +
            #     tm_view(set.zoom.limits = c(11, 14))
            # kmap
        })
    
    #===============================================================================================
    
    
    
    
    # 2ND ORDER
    #===============================================================================================
    
    output$secondOrderEstimationPlot <- renderPlotly({
        
        # SELECT PLANNING AREA
        #===========================================================================================
        if (input$secondOrderSelectPlanningArea == "(All)") {
            area <- mpsz
        }else{
            area <- mpsz[mpsz@data$PLN_AREA_N == input$secondOrderSelectPlanningArea,]
        }
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
            G_df <- as.data.frame(G_ppp)
            plt <- ggplot(G_df, aes(r, km)) +
                geom_line(colour=c("#4d4d4d")) +
                geom_line(aes(r,theo), colour="#e8381e", linetype = "dashed") +
                geom_line(aes(r,rs), colour="#0d7d25", linetype = "dotted") +
                xlab("d(m)") +
                ylab("G(r)") +
                theme_tufte()
        }
        else if (input$secondOrderSelectFunction == "F") {
            F_ppp = Fest(ppp_selected)
            F_df <- as.data.frame(F_ppp)
            plt <- ggplot(F_df, aes(r, km)) +
                geom_line(colour=c("#4d4d4d")) +
                geom_line(aes(r,rs), colour="#e8381e", linetype = "dashed") +
                geom_line(aes(r,cs), colour="#0d7d25", linetype = "twodash") +
                geom_line(aes(r,theo), colour="#381ee8", linetype = "dotted") +
                xlab("d(m)") +
                ylab("F(r)") +
                theme_tufte()
        }
        else if (input$secondOrderSelectFunction == "K") {
            K_ppp = Kest(ppp_selected, correction = "Ripley")
            K_df <- as.data.frame(K_ppp)
            plt <- ggplot(K_df, aes(r, theo-r)) +
                geom_line(aes(r, iso-r), colour=c("#4d4d4d")) +
                geom_line(aes(r, theo-r), colour="red", linetype = "dashed") +
                xlab("d(m)") +
                ylab("K(d)-r") +
                theme_tufte()
        }
        else if (input$secondOrderSelectFunction == "L") {
            L_ppp = Lest(ppp_selected, correction = "Ripley")
            L_df <- as.data.frame(L_ppp)
            plt <- ggplot(L_df, aes(r, theo-r)) +
                geom_line(aes(r, iso-r), colour=c("#4d4d4d")) +
                geom_line(aes(r, theo-r), colour="red", linetype = "dashed") +
                xlab("d(m)") +
                ylab("L(d)-r") +
                theme_tufte()
        }
        ggplotly(plt)
        #===========================================================================================
        
    })
    
    output$secondOrderCompleteSpatRandPlot <- renderPlotly({
        
        # SELECT PLANNING AREA
        #===========================================================================================
        if (input$secondOrderSelectPlanningArea == "(All)") {
            area <- mpsz
        }else{
            area <- mpsz[mpsz@data$PLN_AREA_N == input$secondOrderSelectPlanningArea,]
        }
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
        colour=c("#0D657D","#ee770d","#D3D3D3")
        # SELECT FUNCTION
        #===========================================================================================
        if (input$secondOrderSelectFunction == "G") {
            G_ppp.csr <- envelope(ppp_selected, Gest, correction = "all", nsim = input$secondOrderSliderNsim_1)
            G_df <- as.data.frame(G_ppp.csr)
            plt <- ggplot(G_df, aes(r, obs)) +
                geom_line(colour=c("#4d4d4d")) +
                geom_line(aes(r,theo), colour="red", linetype = "dashed") +
                geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.1, colour=c("#91bfdb")) +
                xlab("d") +
                ylab("G(r)") +
                theme_tufte()
        }
        else if (input$secondOrderSelectFunction == "F") {
            F_ppp.csr <- envelope(ppp_selected, Fest, nsim = input$secondOrderSliderNsim_1)
            F_df <- as.data.frame(F_ppp.csr)
            plt <- ggplot(F_df, aes(r, obs)) +
                geom_line(colour=c("#4d4d4d")) +
                geom_line(aes(r,theo), colour="red", linetype = "dashed") +
                geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.1, colour=c("#91bfdb")) +
                xlab("d") +
                ylab("F(r)") +
                theme_tufte()
        }
        else if (input$secondOrderSelectFunction == "K") {
            K_ppp.csr <- envelope(ppp_selected, Kest, nsim = input$secondOrderSliderNsim_2, rank = 1, glocal=TRUE)
            K_df <- as.data.frame(K_ppp.csr)
            plt <- ggplot(K_df, aes(r, obs-r)) +
                geom_line(colour=c("#4d4d4d")) +
                geom_line(aes(r,theo), colour="red", linetype = "dashed") +
                geom_ribbon(aes(ymin=lo-r,ymax=hi-r),alpha=0.1, colour=c("#91bfdb")) +
                xlab("d") +
                ylab("K(d)-r") +
                theme_tufte()
        }
        else if (input$secondOrderSelectFunction == "L") {
            L_ppp.csr <- envelope(ppp_selected, Lest, nsim = input$secondOrderSliderNsim_2, rank = 1, glocal=TRUE)
            L_df <- as.data.frame(L_ppp.csr)
            plt <- ggplot(L_df, aes(r, obs-r)) +
                geom_line(colour=c("#4d4d4d")) +
                geom_line(aes(r,theo-r), colour="red", linetype = "dashed") +
                geom_ribbon(aes(ymin=lo-r,ymax=hi-r),alpha=0.1, colour=c("#91bfdb")) +
                xlab("d") +
                ylab("L(d)-r") +
                theme_tufte()
        }
        ggplotly(plt)
        #===========================================================================================
    })
    #===============================================================================================
    
    
    
    # ACCESSIBILITY
    #===============================================================================================
    output$accessibilityMap <- renderLeaflet({
        # SELECT AMENITY
        #===========================================================================================
        if (input$accSelectAmenity == "gym") {
            acc_map <- readRDS("data/rds/acc_gym_map.rds")
        } else if (input$accSelectAmenity == "eat") {
            acc_map <- readRDS("data/rds/acc_eat_map.rds")
        }
        #===========================================================================================
        tmap_leaflet(acc_map)
    })
    
    output$accessibilityBox <- renderPlotly({
        # SELECT AMENITY
        #===========================================================================================
        if (input$accSelectAmenity == "gym"){
            acc_box <- readRDS("data/rds/acc_gym_box.rds")
        }else if (input$accSelectAmenity == "eat") {
            acc_box <- readRDS("data/rds/acc_eat_box.rds")
        }
        #===============================================================================================
        ggplotly(acc_box)
    })
    #===============================================================================================
    
    
})