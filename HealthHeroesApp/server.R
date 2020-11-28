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
library("OpenStreetMap")
library("tmaptools")
library("ggthemes")
library("ggplot2")
library("plotly")
library("prettydoc")
library("sp")
library("SpatialAcc")
library("geosphere")
library("ggstatsplot")
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
mpsz_st <- st_read(dsn = "data/geospatial/master-plan-2014-boundary", 
                layer = "MP14_SUBZONE_WEB_PL")
# mpsz_3414 <- st_set_crs(mpsz_st,3414)
# mpsz_sp <- as_Spatial(mpsz_3414)
mpsz_svy21 <- st_transform(mpsz_st, 3414)
mpsz_sp <- as_Spatial(mpsz_svy21)
#===================================================================================================


# HDB
#===================================================================================================
hdb <- read_csv("data/aspatial/hdb_data.csv") %>%
    mutate(Total = rowSums(.[2:11]))
hdb$Postcode <- as.character(hdb$Postcode)
hdb_sf <- st_as_sf(hdb, 
                   coords = c('X', 'Y'),
                   crs = "+init=epsg:3414")
#===================================================================================================

completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
}

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

gym_st <- st_read(dsn = "data/geospatial/gymssg/gyms-sg.shp",
                  layer = "gyms-sg")
gym_3414 <- st_set_crs(gym_st,3414)
gym_sp_3414 <- as_Spatial(gym_3414)
gym_hdb_dist <- read_csv("data/aspatial/gym_hdb_dis.csv", skip = 0)
gym_as <- read_csv("data/aspatial/gym.csv", skip = 0)
gym_spdf <- SpatialPointsDataFrame(gym_sp_3414,data.frame(id=1:159))
gym_res <- over(gym_spdf, mpsz_sp)
mpsz_gym <- left_join(mpsz_st, gym_res, by = c("SUBZONE_N" = "SUBZONE_N")) %>%
    distinct_at(vars(OBJECTID.x), .keep_all = TRUE)
# filter_gym <- mpsz_gym %>%
#     distinct_at(vars(OBJECTID.x), .keep_all = TRUE)
filter_gym_sub <- left_join(gym_as, mpsz_gym, by = c("Id" = "OBJECTID.y")) %>%
    distinct_at(vars(SUBZONE_NO.y), .keep_all = TRUE)
# filter_gym_sub <- gym_join %>%
#     distinct_at(vars(SUBZONE_NO.y), .keep_all = TRUE)
filter_gym_sub$ADDRESSPOS <- as.character(filter_gym_sub$ADDRESSPOS)
gym_sf <- st_as_sf(filter_gym_sub,
                   coords = c('X', 'Y'),
                   crs = "+init=epsg:3414")
gym_hdb <- filter_gym_sub %>% left_join(gym_hdb_dist, by = c("Id" = "origin_id")) %>% 
    left_join(hdb, by = c("destination_id" = "id" )) %>%
    dplyr::select(ADDRESSPOS,Y.x,X.x,SUBZONE_NO.x,SUBZONE_N,REGION_N.x,network_cost,Postcode,Total)
# gym_hdb <- gym_hdb %>% left_join(hdb, by = c("destination_id" = "id" ))
gym_hdb$ADDRESSPOS <- as.character(gym_hdb$ADDRESSPOS)
gym_hdb_filter2 <- gym_hdb %>%
    dplyr::select(ADDRESSPOS,Y.x,X.x,SUBZONE_NO.x,SUBZONE_N,REGION_N.x,network_cost,Postcode,Total)
gym_hdb_filter2 <- completeFun(gym_hdb_filter2, "SUBZONE_N")
gym_hdb_filter2 <- gym_hdb_filter2 %>% left_join(hdb_sf, by = c("Postcode" = "Postcode" ))
hdb_sf_gym <- hdb_sf %>% left_join(gym_hdb_filter2, by = c("Postcode" = "Postcode" ))
distmat_km_gym<-as.matrix(gym_hdb_filter2$network_cost/1000)
acc_Hansen_gym <- data.frame(ac(gym_hdb_filter2$Total,
                                gym_hdb_filter2$`SUBZONE_NO.x`,
                                distmat_km_gym,
                                d0 = 30000,
                                power = 2,
                                family = "Hansen"))
colnames(acc_Hansen_gym) <- "accHansen"
acc_Hansen <- tbl_df(acc_Hansen_gym)
hdb_Hansen_gym <- bind_cols(hdb_sf_gym, acc_Hansen_gym)
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

eat_st <- st_read(dsn = "data/geospatial/healthier-eateries/healthier-eateries.shp",
                  layer = "healthier-eateries")
eat_as <- read_csv("data/aspatial/eat.csv", skip = 0)
eat_3414 <- st_set_crs(eat_st,3414)
eat_sp_3414 <- as_Spatial(eat_3414)
eat_spdf <- SpatialPointsDataFrame(eat_sp_3414,data.frame(id=1:1810))
eat_res <- over(eat_spdf, mpsz_sp)
mpsz_eat <- left_join(mpsz_st, eat_res, by = c("SUBZONE_N" = "SUBZONE_N"))
filter_eat <- mpsz_eat %>%
    distinct_at(vars(OBJECTID.x), .keep_all = TRUE)
eat_join <- left_join(eat_as, filter_eat, by = c("Id" = "OBJECTID.y"))
filter_eat_sub <- eat_join %>%
    distinct_at(vars(SUBZONE_NO.y), .keep_all = TRUE)
filter_eat_sub$ADDRESSPOS <- as.character(filter_eat_sub$ADDRESSPOS)
eat_sf <- st_as_sf(filter_eat_sub,
                   coords = c('X', 'Y'),
                   crs = "+init=epsg:3414")
eat_hdb_dist <- read_csv("data/aspatial/eat_hdb_dis.csv", skip = 0)
eat_hdb <- filter_eat_sub %>% left_join(eat_hdb_dist, by = c("Id" = "origin_id"))
eat_hdb <- eat_hdb %>% left_join(hdb, by = c("destination_id" = "id" ))
eat_hdb$ADDRESSPOS <- as.character(eat_hdb$ADDRESSPOS)
eat_hdb_filter2 <- eat_hdb %>%
    dplyr::select(ADDRESSPOS,Y.x,X.x,SUBZONE_NO.x,SUBZONE_N,REGION_N.x,X_ADDR.x,Y_ADDR.x,network_cost,Postcode,X.y,Y.y,Latitude,Longitude,Total)
eat_hdb_filter2 <- completeFun(eat_hdb_filter2, "SUBZONE_N")
eat_hdb_filter2 <- eat_hdb_filter2 %>% left_join(hdb_sf, by = c("Postcode" = "Postcode" ))
hdb_sf_eat <- hdb_sf %>% left_join(eat_hdb_filter2, by = c("Postcode" = "Postcode" ))
distmat_km_eat<-as.matrix(eat_hdb_filter2$network_cost/1000)
acc_Hansen_eat <- data.frame(ac(eat_hdb_filter2$Total,
                                eat_hdb_filter2$`SUBZONE_NO.x`,
                                distmat_km_eat,
                                d0 = 30000,
                                power = 2,
                                family = "Hansen"))
colnames(acc_Hansen_eat) <- "accHansen"
acc_Hansen <- tbl_df(acc_Hansen_eat)
hdb_Hansen_eat <- bind_cols(hdb_sf_eat, acc_Hansen_eat)
#===================================================================================================



# PLACES
#===================================================================================================
places <- readOGR(dsn = "data/geospatial/Singapore-shp/shape/places.shp",
                  layer = "places")

print(summary(places))
print(unique(places$Name))

places_sp <- as(places, "SpatialPoints")
# places_sp <- spTransform(places_sp, crs(eat_sp))
# places_ppp <- as.SpatialPoints.ppp(places_sp)
places_ppp <- as(places_sp, "ppp")

places_ppp <- rjitter(places_ppp, retry=TRUE, nsim=1, drop=TRUE)
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
            am_sf <- gym_sf
            pointColor <- "red"
            hdb_acc <- hdb_Hansen_gym
        } 
        else if (input$accSelectAmenity == "eat") { 
            am_sf <- eat_sf
            pointColor <- "blue"
            hdb_acc <- hdb_Hansen_eat
        }
        #===========================================================================================
        accessibilityMap <- tm_shape(am_sf) +
            tm_bubbles(col = pointColor, size=0.15, border.col="black", border.lwd=1) +
            tm_shape(hdb_acc) + 
            tm_bubbles(col = "accHansen",
                       n = 5,
                       style = "quantile",
                       size = 0.001,
                       border.col = "black",
                       border.lwd = 0.1)
        tmap_leaflet(accessibilityMap)
    })
    
    output$plot2 <- renderPlotly({
        # SELECT AMENITY
        #===========================================================================================
        if (input$accSelectAmenity == "gym"){
            hdb_acc <- st_transform(hdb_Hansen_gym, 3414)
        }else if (input$accSelectAmenity == "eat") {
            hdb_acc <- st_transform(hdb_Hansen_eat, 3414)
        }
        hdb_acc <- st_join(hdb_acc, mpsz_svy21, join = st_intersects)
        title <- "Hansen's accessibility values by HDB location and by Region"
        caption <-  "Hansen's values"
        hdb_acc$log_acc <- log(hdb_acc$`accHansen`)
        #===============================================================================================
        plt <- ggplot(data=hdb_acc, 
               aes(y = accHansen, 
                   x= REGION_N)) +
            geom_boxplot() +
            geom_point(stat="summary", 
                       fun.y="mean", 
                       colour ="red", 
                       size=4)
        ggplotly(plt)
    })
    #===============================================================================================

})
