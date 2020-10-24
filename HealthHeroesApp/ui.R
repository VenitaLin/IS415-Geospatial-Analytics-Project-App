# Imports

packages = c('shiny','shinythemes','leaflet')
for (p in packages){
    library(p,character.only = T)
}


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


kernelDensityUI <- sidebarLayout(
    sidebarPanel(
        sliderInput("kernelDensitySliderYear",
                    "Year:",
                    min = 2010,
                    max = 2020,
                    value = 2010,
                    ticks = F
        ),
        sliderInput("kernelDensitySliderMonth",
                    "Month:",
                    min = 1,
                    max = 12,
                    value = 1,
                    ticks = F
                    
        ),
        checkboxGroupInput("kernelDensityCheckboxSubzone", 
                           "Regions:",
                           c("CENTRAL REGION", "WEST REGION", "EAST REGION", "NORTH REGION")
        ),
    ),
    mainPanel(
        leafletOutput("kernelDensityMap")
    )
)

spatPointUI <- sidebarLayout(
    sidebarPanel(
        dateInput("spatPointDateInputStart",
                    "Start Date:"
        ),
        dateInput("spatPointDateInputEnd",
                  "End Date:"
        ),
        checkboxGroupInput("kernelDensityCheckboxSubzone", 
                           "Regions:",
                           c("CENTRAL REGION", "WEST REGION", "EAST REGION", "NORTH REGION")
        ),
    ),
    mainPanel(
        leafletOutput("spatPointMap")
    )
)

accessibilityUI <- sidebarLayout(
    sidebarPanel(
        sliderInput("accessibiltyMetersSlider",
                    "Year:",
                    min = 50,
                    max = 2500,
                    value = 100,
                    ticks = F
        ),
        checkboxGroupInput("kernelDensityCheckboxSubzone", 
                           "Amenity Type:",
                           c("Eateries", "Gyms", "Parks")
        ),
    ),
    mainPanel(
        leafletOutput("accessibilityMap")
    )
)


shinyUI(navbarPage(
    "HealthHeroes",
    theme = shinytheme("yeti"),
    tabPanel(title = "Overview", value = "overview", testSliderUI),
    tabPanel(title = "Kernel Density Analysis", value = "kernelDensity", kernelDensityUI),
    tabPanel(title = "Spatial Point Pattern Analysis", value = "spatPoint", spatPointUI),
    tabPanel(title = "Accessibility Analysis", value = "accessibility", accessibilityUI)
))



