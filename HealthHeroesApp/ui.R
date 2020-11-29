# IMPORTS
#===================================================================================================
library('shiny')
library('shinythemes')
library('leaflet')
library('shinycssloaders')
library('DT')
library('plotly')
#===================================================================================================

# SUMMARY
#===================================================================================================
summaryUI <- sidebarLayout(
    sidebarPanel(
        selectInput("summaryPointSelectAmenity", 
                    "Amenity Type:",
                    c("Gyms" = "gym",
                      "Eateries" = "eat",
                      "Places" = "places")
        ),
        
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Point Map", leafletOutput("summaryPointMap") %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Data Table", 
                             style = "overflow-y:scroll; max-height: 400px",
                             dataTableOutput("summaryDataTable") %>% withSpinner(color="#0dc5c1")
                    )
        )
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
                        max = 2000,
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
        ),
        conditionalPanel(
            condition = "input.tabselected == 'secondOrderUITab2'",
            conditionalPanel(
                condition = "input.secondOrderSelectFunction == 'G' || input.secondOrderSelectFunction == 'F'",
                sliderInput("secondOrderSliderNsim_1",
                            "Number of Simulations",
                            min = 10,
                            max = 999,
                            value = 10,
                            ticks = F
                )
            ),
            conditionalPanel(
                condition = "input.secondOrderSelectFunction == 'K' || input.secondOrderSelectFunction == 'L'",
                sliderInput("secondOrderSliderNsim_2",
                            "Number of Simulations",
                            min = 10,
                            max = 99,
                            value = 10,
                            ticks = F
                )
            )  
        ),
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    id = "tabselected",
                    tabPanel("Function Estimation", value="secondOrderUITab1", plotlyOutput("secondOrderEstimationPlot") %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Complete Spatial Randomness Test", value="secondOrderUITab2", plotlyOutput("secondOrderCompleteSpatRandPlot") %>% withSpinner(color="#0dc5c1"))
        )
    )
)
#===================================================================================================


# ACCESSIBILITY
#===================================================================================================
accessibilityUI <- sidebarLayout(
    sidebarPanel(
        selectInput("accSelectAmenity", 
                    "Amenity Type:",
                    c("Gyms" = "gym",
                      "Eateries" = "eat")
        ),
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Mapping Hansen accessibility", leafletOutput("accessibilityMap") %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Statistical graphic visualisation", plotlyOutput("accessibilityBox") %>% withSpinner(color="#0dc5c1"))
        )
    )
)
#===================================================================================================




# MAIN
#===================================================================================================
shinyUI(navbarPage(
    "HealthHeroes",
    theme = shinytheme("yeti"),
    tabPanel(title = "Data", value = "summary", summaryUI),
    tabPanel(title = "First Order Analysis", value = "kernelDensity", kernelDensityUI),
    tabPanel(title = "Second Order Analysis", value = "secondOrder", secondOrderUI),
    tabPanel(title = "Accessibility Analysis", value = "accessibility", accessibilityUI)
))
#===================================================================================================
