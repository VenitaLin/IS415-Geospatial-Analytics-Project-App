# IMPORTS
#===================================================================================================
packages = c('shiny','shinythemes','leaflet')
for (p in packages){
    library(p,character.only = T)
}
#===================================================================================================


# SAMPLE
#===================================================================================================
testSliderUI <- sidebarLayout(
    sidebarPanel(
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
    ),
    mainPanel(
        plotOutput("distPlot")
    )
)
#===================================================================================================


# KERNEL DENSITY
#===================================================================================================
kernelDensityUI <- sidebarLayout(
    sidebarPanel(
        selectInput("kernelDensitySelectAmenity", 
                    "Amenity Type:",
                    c("Gyms" = "gym",
                      "Eateries" = "eat",
                      "Places" = "places")
        ),
        selectInput("kernelDensitySelectRegion", 
                    "Region:",
                    c("All Regions" = "all",
                      "Central Region" = "CENTRAL REGION",
                      "West Region" = "WEST REGION",
                      "East Region" = "EAST REGION",
                      "North-East Region" = "NORTH-EAST REGION",
                      "North Region" = "NORTH REGION")
        ),
        selectInput("kernelDensitySelectBandwidth", 
                    "Selected Bandwidth:",
                    c("Manual (Fastest)" = "manual",
                      "Adaptive" = "adaptive",
                      "bw.diggle" = "bw.diggle",
                      "bw.scott" = "bw.scott",
                      "bw.ppl" = "bw.ppl")
        ),
        conditionalPanel(
            condition = "input.kernelDensitySelectBandwidth == 'manual'",
            sliderInput("kernelDensitySliderBandwidth",
                        "Bandwidth",
                        min = 100,
                        max = 1000,
                        value = 600,
                        ticks = F
            ),  
        ),
        conditionalPanel(
            condition = "input.kernelDensitySelectBandwidth != 'adaptive'",
            selectInput("kernelDensitySelectKernelMethod", 
                        "Kernel Method:",
                        c("Gaussian" = "gaussian",
                          "Epanechnikov" = "epanechnikov",
                          "Quartic" = "quartic",
                          "Disc"="disc")
            )
        )
    ),
    mainPanel(
        leafletOutput("kernelDensityMap")
    )
)
#===================================================================================================


# SPATIAL POINT
#===================================================================================================
spatPointUI <- sidebarLayout(
    sidebarPanel(
        dateInput("spatPointDateInputStart",
                    "Start Date:"
        ),
        dateInput("spatPointDateInputEnd",
                  "End Date:"
        ),
    ),
    mainPanel(
        leafletOutput("spatPointMap")
    )
)
#===================================================================================================


# ACCESSIBILITY
#===================================================================================================
accessibilityUI <- sidebarLayout(
    sidebarPanel(
        sliderInput("accessibiltyMetersSlider",
                    "Year:",
                    min = 50,
                    max = 2500,
                    value = 100,
                    ticks = F
        ),
    ),
    mainPanel(
        leafletOutput("accessibilityMap")
    )
)
#===================================================================================================




# MAIN
#===================================================================================================
shinyUI(navbarPage(
    "HealthHeroes",
    theme = shinytheme("yeti"),
    tabPanel(title = "Overview", value = "overview", testSliderUI),
    tabPanel(title = "Kernel Density Analysis", value = "kernelDensity", kernelDensityUI),
    tabPanel(title = "Spatial Point Pattern Analysis", value = "spatPoint", spatPointUI),
    tabPanel(title = "Accessibility Analysis", value = "accessibility", accessibilityUI)
))
#===================================================================================================


