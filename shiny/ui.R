#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(bs4Dash)
library(leaflet.extras)
library(plotly)
library(ECharts2Shiny)
library(echarts4r)
library(tidyverse)
library(shinyWidgets)
library(FraNchEstYN)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))




dashboardPage(
  title = "FRaNchEstYn",
  header = dashboardHeader(title = "FRaNchEstYn"),
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    sidebarUserPanel(
      name = uiOutput("plop"),  # Use uiOutput for dynamic content
      image = NULL),
    sidebarMenu(
      menuItem("Global simulations", tabName = "globalSim", icon = icon("map-marker-alt"))
    )
  ),
  body = dashboardBody(
    tags$style(HTML("
    /* Slider bar style for Simulation Settings */
    .slider-simulation .irs-bar {
      background: linear-gradient(90deg, #ff8c00, #ffd700); /* Orange to yellow gradient */
      border-top: 1px solid #ffd700;
      border-bottom: 1px solid #ff8c00;
    }
    .slider-simulation .irs-single {
      background: #ffa500; /* Orange handle */
      border-color: #cc8400;
    }

    /* Slider bar style for Crop Parameters */
    .slider-crop .irs-bar {
      background: linear-gradient(90deg, #32cd32, #3cb371); /* Light green to medium sea green */
      border-top: 1px solid #3cb371;
      border-bottom: 1px solid #32cd32;
    }
    .slider-crop .irs-single {
      background: #228b22; /* Forest green handle */
      border-color: #196619;
    }

    /* Slider bar style for Disease Parameters */
    .slider-disease .irs-bar {
      background: linear-gradient(90deg, #1e90ff, #4682b4); /* Dodger blue to steel blue */
      border-top: 1px solid #4682b4;
      border-bottom: 1px solid #1e90ff;
    }
    .slider-disease .irs-single {
      background: #00bfff; /* Deep sky blue handle */
      border-color: #009acd;
    }
    .slider-disease .irs-slider.from {
      background: #00bfff; /* Deep sky blue handle */
      border-color: #009acd;
    }
    .slider-disease .irs-slider.to {
      background: #00bfff; /* Deep sky blue handle */
      border-color: #009acd;
    }
    .header1 {
      text-align: center;
      color: #ffa500; /* Color for the first header */
      font-size: 24px; /* Font size for the first header */
      font-weight: bold; /* Make text bold */
      margin-bottom: 0px; /* Space below the header */
    }
    .header2 {
      text-align: center;
      color:#228b22; /* Color for the first header */
      font-size: 24px; /* Font size for the first header */
      font-weight: bold; /* Make text bold */
      margin-bottom: 0px; /* Space below the header */
    }
    .header3 {
      text-align: center;
      color:#00bfff; /* Color for the first header */
      font-size: 24px; /* Font size for the first header */
      font-weight: bold; /* Make text bold */
      margin-bottom: 0px; /* Space below the header */
    }
    .green-icon .fa-seedling {
      color: #28a745; /* Green color for the seedling icon */
    }
  ")),
    tabItems(
      # Where and Who tab content
      tabItem(
        tabName = "globalSim",
        # Top Row with map and sliders
        fluidRow(
          # Left half: Leaflet map
          column(4,
                 leafletOutput("map",height = "350px")),
          # Left half: Leaflet map
          column(2,
                 h4("Simulation Settings", class = 'header1'),
                 fluidRow(
                   chooseSliderSkin("Flat"),
                   column(6,tags$div(class = "slider-simulation",
                          sliderInput("year_range", "year range:",
                               min = 2015,
                               max = 2024,
                               value = c(2023, 2024),
                               step = 1,
                               sep = ""))),
                   column(6,tags$div(class = "slider-simulation",
                          sliderInput("sowing_date", "sowing date:",
                             min = 1,
                             max = 365,
                             value = 300,
                             step = 1,
                             sep = ""))),
                   column(12, tags$div(class = "slider-simulation",
                                       sliderInput("LW_threshold", "humidity threshold for leaf wetness:",
                             min = 70,
                             max = 100,
                             value = 85,
                             step = 1,
                             sep = ""))))
          ),
          # Right half: TabPanel with sliders
          column(3,
                 h4("Crop Parameters",class='header2'),
                 tabsetPanel(
                   tabPanel("Phenology", icon =  icon("seedling", class = "green-icon"),
                     fluidRow(
                        column(6, tags$div(class = "slider-crop",
                                           sliderInput("CropTemperatures", "temperature range (°C)",
                                           min = 0, max = 45, value =c(3,35),step=0.5))),
                        column(6, tags$div(class = "slider-crop",
                                           sliderInput("CropOptTemperature", "optimum temperature (°C)",
                                           min = 15, max = 35, value = 25,step=0.5))),
                        column(6, tags$div(class = "slider-crop",
                                           sliderInput("CycleLength", "cycle length (degree days)",
                                           min = 1000, max = 3000, value =1500,step=50))),
                        column(6, tags$div(class = "slider-crop",
                                           sliderInput("FloweringStart", "flowering start (%)",
                                           min = 15, max = 80, value = 50,step=1))))),
                 tabPanel("Canopy", icon = icon("leaf"),
                      fluidRow(
                          column(6, tags$div(class = "slider-crop",
                                             sliderInput("SlopeGrowth", "slope growth phase (-)",
                                                  min = 0.01, max = 0.05, value = 0.025,step=0.001))),
                          column(6, tags$div(class = "slider-crop",
                                             sliderInput("HalfIntGrowth", "half growth phase (%)",
                                                min = 10, max = 50, value =25,step=1))),
                          column(6, tags$div(class = "slider-crop",
                                             sliderInput("SlopeSenescence", "slope senescence phase (-)",
                                                min = 0.01, max =  0.05, value = 0.025,step=0.001))),
                          column(6, tags$div(class = "slider-crop",
                                             sliderInput("HalfIntSenescence", "half senescence phase (%)",
                                               min = 60, max = 100, value =80,step=1))))),
                 tabPanel("Yield", icon = icon("wheat-awn"),
                          fluidRow(
                            column(6, tags$div(class = "slider-crop",
                                                 sliderInput("RadiationUseEfficiency", "RUE maximum (g/MJ)",
                                                  min = 2, max = 5, value =3,step=0.1))),
                            column(6, tags$div(class = "slider-crop",
                                                 sliderInput("PartitioningMaximum", "partitioning max (fraction)",
                                                  min = 0.5, max = 1, value = 0.75,step=0.05))),
                            column(6, tags$div(class = "slider-crop",
                                               sliderInput("RemobilizationCoefficient", "remobilization (fraction)",
                                                  min = 0, max = 0.01, value = 0.005,step=0.0001))),
                            column(6, tags$div(class = "slider-crop",
                                                 sliderInput("VarietalResistance", "disease resistance (fraction)",
                                                  min = 0, max = 1, value = .5,step=0.05))))
                 )
              )
          ),
          column(3,
                 h4("Disease Parameters",class='header3'),
                 tabsetPanel(
                   tabPanel("Infection", icon = icon("virus"),
                            fluidRow(
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("DiseaseTemperatures", "temperature range (°C)",
                                                    min = 0, max = 45, value =c(3,35),step=0.5))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("DiseaseOptTemperature", "optimum temperature (°C)",
                                                    min = 15, max = 35, value = 22,step=0.5))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("WetnessRange", "wetness range (hours)",
                                                    min = 1, max = 36, value = c(3,16),step=1))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("D50", "dry interruption (hours)",
                                                    min = 0, max = 24, value = 10,step=1))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("RelativeHumidityCritical", "critical humidity (%)",
                                                  min = 50, max = 100, value = 75,step=1))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("HydroThermalTimeOnset", "disease onset (hydroTime)",
                                                    min = 0, max = 100, value = 5,step=0.5))))),
                   tabPanel("Epidemiology", icon = icon("clock"),
                            fluidRow(
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("LatencyDuration", "latency duration (days)",
                                                    min = 1, max = 30, value =10,step=0.5))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("PathogenInfectivity", "pathogen infectivity (fraction)",
                                                    min = 0.1, max = 1, value = 1,step=0.01))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("SporulationDuration", "sporulation duration (days)",
                                                    min = 1, max = 30, value = 15,step=.5))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("PathogenSpread", "pathogen spread (fraction)",
                                                    min = 0.01, max = 3, value = 1,step=0.001))))),
                   tabPanel("Impact", icon = icon("bolt-lightning"),
                            fluidRow(
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("virtualVisualLesion", "virtual/visual lesion (-)",
                                                    min = 1, max = 3, value =1,step=0.1))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("RUEreductionFactor", "RUE reduction (-)",
                                                    min = 0.01, max = 1, value = 0.3,step=0.01))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("senescenceAccelerationMax", "senescence acceleration (%)",
                                                    min = 0, max = 30, value = 10,step=1))),
                              column(6, tags$div(class = "slider-disease",
                                                 sliderInput("assimilateSappersMax", "assimilate sappers (fraction)",
                                                    min = 0, max = 50, value = 20,step=0.1))))
                   )
                 )
        )),

        # Bottom Row with Chart
        fluidRow(
          column(12, echarts4rOutput("chart",height = "300px")))
        #fluidRow(
        #  column(3, echarts4rOutput("box1",height = "250px")),
        #  column(3, echarts4rOutput("box3",height = "250px")),
        #  column(3, echarts4rOutput("box4",height = "250px")),
        #  column(3, echarts4rOutput("box2",height = "250px"))

        #,
        # column(12, echarts4rOutput("chart2")))
      #)

    )
  )
 )
)
