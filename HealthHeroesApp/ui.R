# IMPORTS
#===================================================================================================
packages = c('shiny','shinythemes','leaflet','shinycssloaders')
for (p in packages){
    library(p,character.only = T)
}
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
        plotOutput("kernelDensityMap") %>% withSpinner(color="#0dc5c1")
    )
)
#===================================================================================================


# SECOND ORDER
#===================================================================================================
secondOrderUI <- sidebarLayout(
    sidebarPanel(
        selectInput("secondOrderSelectAmenity", 
                    "Amenity Type:",
                    c("Gyms" = "gym",
                      "Eateries" = "eat",
                      "Places" = "places")
        ),
        selectInput("secondOrderSelectPlanningArea", 
                    "Planning Area:",
                    c("Loading..." = "ANG MO KIO") # Placeholder
        ),
        selectInput("secondOrderSelectFunction", 
                    "Function:",
                    c("G-Function" = "G",
                      "F-Function" = "F",
                      "K-Function" = "K",
                      "L-Function" = "L")
        )
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Function Estimation", plotOutput("secondOrderEstimationPlot") %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Complete Spatial Randomness Test", plotOutput("secondOrderCompleteSpatRandPlot") %>% withSpinner(color="#0dc5c1"))
        )
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
    tabPanel(title = "First Order (Kernel Density) Analysis", value = "kernelDensity", kernelDensityUI),
    tabPanel(title = "Second Order Analysis", value = "secondOrder", secondOrderUI),
    tabPanel(title = "Accessibility Analysis", value = "accessibility", accessibilityUI)
))
#===================================================================================================


