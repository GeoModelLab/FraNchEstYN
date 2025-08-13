library(shiny)
library(bs4Dash)
library(shinydashboard)
library(leaflet)
#devtools::install_github("JohnCoene/echarts4r")
library(echarts4r)
library(tidyverse)
#remotes::install_github("ColinFay/geoloc")
library()
source("..\\R\\Main.R")


# Define server logic required to draw a histogram
function(input, output, session) {

   # Placeholder for the dynamic message
   dynamic_message <- reactiveVal("Click on the map to get NASA weather")

   # Method to update weatherData based on some logic
   updateWeatherData <- function(new_data) {
     weatherData(new_data)
   }

   # Initialize weatherData as a reactive value
   weatherData <- reactiveVal(NULL)

   # Initialize modelOutput as a reactive value
   modelOutput <- reactiveVal(NULL)

   # Method to update weatherData based on some logic
   updateModelOutput <- function(new_data) {
     modelOutput(new_data)
   }

   # Update dynamic message
   output$dynamic_message <- renderText({
     dynamic_message()
   })

    bbox <- reactive({
      req(input$map_draw_new_feature)  # Ensure the feature is drawn

      # Extract coordinates from the drawn feature
      point_result <- input$map_draw_new_feature$geometry$coordinates

      # Process to get bbox (as in your example)
      if (length(point_result) == 2) {
        # For a point
        c(as.numeric(point_result))
      } else if (is.list(point_result) && length(point_result) > 0 && is.list(point_result[[1]])) {
        # For a rectangle or polygon
        longitudes <- sapply(point_result[[1]], function(coord) coord[1])
        latitudes <- sapply(point_result[[1]], function(coord) coord[2])
        c(min(unlist(longitudes)), min(unlist(latitudes)), max(unlist(longitudes)), max(unlist(latitudes)))
      } else {
        stop("Invalid input type. Input must be a point, rectangle, or polygon.")
      }
    })

    # Create a basic Leaflet map
    output$map <- renderLeaflet({
      leaflet() %>%
        clearControls() %>%
        clearMarkers() %>%
        clearShapes() %>%   # Clears all circles, rectangles, and polygons
        addProviderTiles("CyclOSM") %>%
        fitBounds(8, 48, 16.5, 38) %>%
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(edit = FALSE)
        )
    })

    start_date <- reactive({
      today <-  as.Date(Sys.Date(), format="%Y-%m-%d %H:%M:%S")

    })

    end_date <- reactive({
      today <- Sys.Date()
      # Check if the second value in the year range is the current year
      if (input$year_range[[2]] == lubridate::year(today)) {
        as.character(today - 5)  # Subtract 5 years from today
      } else {
        paste(input$year_range[[2]], "-12-31", sep = "")  # End of the selected year
      }
    })

    # Output the calculated end date for testing purposes
    output$end_date_text <- renderText({
      end_date()  # Reactive output of the end date
    })

    date_range <- reactive({
      req(input$year_range)  # Ensure year_range is defined

      start_date <- as.Date(paste(input$year_range[[1]], "01-01", sep = "-"))
      end_date <- as.Date(paste(input$year_range[[2]], "12-31", sep = "-"))
      c(start_date, end_date)
    })

    # Observe leaflet marker click to update selected location
    # Observe the new feature drawn on the map and update the message and fetch weather data
    observeEvent(c(input$map_draw_new_feature,
                  input$year_range ), {

      # Update the message to indicate data is being fetched
      dynamic_message("Downloading weather data...")

      req(input$map_draw_new_feature)

      point_result <- input$map_draw_new_feature$geometry$coordinates
      bounds <- bbox()  # Assuming bbox() is defined elsewhere
      dates <- date_range()  # Assuming date_range() is defined elsewhere

      # Process the bounds and dates (adjusting them as necessary)
      if (length(bounds) > 2) {
        dates[1] <- paste0(substring(dates[2], 0, 4), '-01-01')
        latRange <- bounds[4] - bounds[2]
        if (latRange < 2) { bounds[4] <- bounds[2] + 2 }
        lonRange <- bounds[3] - bounds[1]
        if (lonRange < 2) { bounds[3] <- bounds[1] + 2 }
      }

      today <- Sys.Date()
      if (substring(dates[2], 0, 4) == year(today)) {
        dates[2] <- as.character(today - 5)
      }

      # Fetch weather data with a progress bar
      new_data <- nasapower::get_power(
            community = "ag",
            lonlat = bounds,
            dates = dates,
            temporal_api = "daily",
            pars = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR"))

       # Process new_data and update as necessary
      updateWeatherData(new_data)  # Assuming updateWeatherData() is defined elsewhere

      # Once done, update the message
      dynamic_message("Weather data successfully retrieved.")
    })

    # Example chart output (just a placeholder for now)
    output$chart <- renderEcharts4r({
      req(weatherData())

      thisWeather_data <- as.data.frame(weatherData())
      thisWeather_data$grid <- paste0(thisWeather_data$LAT, thisWeather_data$LON)

      dates <- date_range()
      yearStart <- lubridate::year(dates[[1]])
      yearEnd<-year(dates[[2]])
      thisWeather_data <- thisWeather_data |>
        filter(YEAR>=yearStart & YEAR<=yearEnd)


      max_year <- lubridate::year(max(thisWeather_data$YYYYMMDD))

      if(max_year!=lubridate::year(Sys.Date()) && length(unique(thisWeather_data$grid)>1))
      {
        bounds <- bbox()
        length(bounds)
        if(length(bounds)>2)
        {
          dates[1]<-paste0(substring(dates[2],0,4),'-01-01')
          latRange<- bounds[4]-bounds[2]
          if(latRange<2){bounds[4]<-bounds[2]+2}

          lonRange<-bounds[3]-bounds[1]
          if(lonRange<2){bounds[3]<-bounds[1]+2}
        }
        new_data <- nasapower::get_power(
          community = "ag",
          lonlat = bounds,
          dates =dates,  # Use reactive expressions
          temporal_api = "daily",
          pars = c("T2M_MAX","T2M_MIN", "PRECTOTCORR")
        )

        thisWeather_data <- as.data.frame(new_data) # Call the update function with new data
      }

      # paramCrop dataframe----
      paramCrop<-data.frame(
        Model = c(rep("crop",11)),
        Parameter =  c("TbaseCrop", "ToptCrop", "TmaxCrop", "CycleLength", "FloweringStart",
                       "HalfIntGrowth", "HalfIntSenescence", "SlopeGrowth",
                       "SlopeSenescence", "RadiationUseEfficiency", "PartitioningMaximum"),
        description = c(rep("desc",11)),
        unit = c(rep("unit",11)),
        min = c(rep(1,11)),
        max = c(rep(40,11)),
        value = c(input$CropTemperatures[[1]],input$CropOptTemperature,
                  input$CropTemperatures[[2]], input$CycleLength, input$FloweringStart,
                  input$HalfIntGrowth, input$HalfIntSenescence,input$SlopeGrowth,
                  input$SlopeSenescence, input$RadiationUseEfficiency, input$PartitioningMaximum),
        calibration = c(rep("",11))
      )

      # paramDisease dataframe----
      paramDisease<-data.frame(
        Model = c(rep("disease",20)),
        Parameter =  c("IsSplashBorne", "OuterInoculum", "PathogenSpread", "WetnessDurationOptimum",
                       "WetnessDurationMinimum", "DryCriticalInterruption", "Tmin",
                       "Topt","Tmax",
                       "RelativeHumidityCritical","Rain50Detachment", "RelativeHumidityNotLimiting",
                       "HydroThermalTimeOnset","CyclePercentageOnset",
                       "LatencyDuration","SporulationDuration", "VirtualVisualLesion",
                       "RUEreductionFactor","SenescenceAccelerationMax", "AssimilateSappersMax"),
        description = c(rep("desc",20)),
        unit = c(rep("unit",20)),
        min = c(rep(1,20)),
        max = c(rep(40,20)),
        value = c(1, input$PathogenInfectivity,input$PathogenSpread,input$WetnessRange[[2]],
                  input$WetnessRange[[1]],input$D50, input$DiseaseTemperatures[[1]],
                  input$DiseaseOptTemperature, input$DiseaseTemperatures[[2]],
                  input$RelativeHumidityCritical,10,80, input$HydroThermalTimeOnset,10,
                  input$LatencyDuration,input$SporulationDuration,input$virtualVisualLesion,
                  input$RUEreductionFactor,input$senescenceAccelerationMax,input$assimilateSappersMax),
        calibration = c(rep("",20))
      )

      df<-rbind(paramCrop,paramDisease)
      # Helper: build one parameter set
      make_param_list <- function(data) {
        setNames(
          lapply(seq_len(nrow(data)), function(i) {
            list(
              description = data$Description[i],
              unit        = data$unit[i],
              min         = data$min[i],
              max         = data$max[i],
              value       = data$value[i],
              calibration = tolower(data$calibration[i]) %in% c("x","true","1","yes")
            )
          }),
          data$Parameter
        )
      }



      # Split by model and set name
      split_sets <- function(df, model) {
        sub <- df[tolower(df$Model) == tolower(model), ]
        if (!nrow(sub)) return(list())
        if (!"Set" %in% names(sub)) sub$Set <- "Default"

        sets <- split(sub, sub$Set)
        lapply(sets, make_param_list)
      }

      thisCropParameters    <- split_sets(df, "crop")
      thisDiseaseParameters <- split_sets(df, "disease")


      # Function to fill NA with average of last and next available values----
      fill_with_avg <- function(x) {
        n <- length(x)
        for (i in 1:n) {
          if (is.na(x[i])) {
            # Find previous available value
            prev_val <- tail(x[1:(i-1)], n = 1)
            # Find next available value
            next_val <- head(x[(i+1):n], n = 1)

            # Calculate average if both values are available
            if (!is.na(prev_val) && !is.na(next_val)) {
              x[i] <- (prev_val + next_val) / 2
            } else if (!is.na(prev_val) && is.na(next_val)) {
              # If no next value, just carry forward the last available value
              x[i] <- prev_val
            } else if (is.na(prev_val) && !is.na(next_val)) {
              # If no previous value, take the next value (not typical in real-world cases)
              x[i] <- next_val
            }
          }
        }
        return(x)
      }

      # Prepare inputs----
      dfInput<-thisWeather_data |>
        filter(lubridate::year(YYYYMMDD)>=lubridate::year(as.Date(dates[[1]])) &
                 lubridate::year(YYYYMMDD)<=lubridate::year(as.Date(dates[[2]]))) |>
        mutate(grid = paste0(LAT, '_',LON)) |>
        rename(Site = grid,TMAX = T2M_MAX,TMIN=T2M_MIN,RAIN=PRECTOTCORR,DATE=YYYYMMDD) |>
        select(Site,TMAX,TMIN,RAIN,DATE,LAT) |>
        mutate(
          TMAX = fill_with_avg(TMAX),
          TMIN = fill_with_avg(TMIN),
          RAIN = tidyr::replace_na(RAIN, 0)
        )

      # call fRanchestyn_daily ----
      management_data <- data.frame(
        crop        = c("wheat"),
        variety     = c("Generic"),
        resistance  = c(0),
        sowingDOY   = c(300),
        year        = c("All"),
        stringsAsFactors = FALSE
      )

      dfInput<-dfInput |>
        mutate(year= year(DATE),
               month = month(DATE),
               day=day(DATE),
               rad=TMAX) |>
        select(-c(DATE,LAT))

      reference_data <- data.frame(
        crop        = c("wheat"),
        variety     = c("Generic"),
        Septoria  = c(0),
        doy   = c(300),
        year  = c(2023),
        stringsAsFactors = FALSE
      )


      outputs<- franchestyn(dfInput,
                            management_data,
                            reference_data,
                            cropParameters=cropParameters,
                            diseaseParameters=diseaseParameters,
                            calibration = 'none',
                            disease='Septoria',
                            start_end = c(year(dates[[1]]),year(dates[[2]])))

      # join model results and weather data----
      outputs<-outputs$outputs$simulation |>
        dplyr::mutate(fIntPotState=round(LightInterception*100,1),
                      fIntDisState = round(LightIntHealthy*100,1),
                      yieldPotState=round(Yield/100,1),
                      yieldDisState = round(YieldHealthy/100,1),
                      bioPotState=round(AGB/100,1),
                      bioDisState = round(AGBHealthy/100,1),
                      PathogenSuitRate=round(HTtimeR*100,1),
                      TMAX=round(Tmax,1),
                      TMIN=round(Tmin,1),
                      HTaffected=round(Affected*100,1),
                      HTlatent=round(Latent*100,1),
                      HTsporulating=round(Sporulating*100,1),
                      HTdead=round(Dead*100,1),
                      DATE=as.Date(Date,format='%m/%d/%Y'))

      #update data structure
      updateModelOutput(outputs)

      # Get the unique years
      years <- unique(outputs$year)

      chart <- outputs %>%
        e_charts(x = Date,draw=T) %>%
        e_datazoom(
          type = "inside",
          toolbox = F,
          bottom = -5
        ) %>%
        e_tooltip() %>%
        e_x_axis(DATE, axisPointer = list(show = TRUE))

      chart %>%
        # e_bar(p,
        #       name = "rain mm (y2)",
        #       y_index =1,
        #       animation=F,
        #       itemStyle=list(color='grey44')) |>
        e_line(TMAX,
               name = "t max °C (y2)",
               y_index =1,
               itemStyle = list(color = "red",borderWidth = 0),
               lineStyle = list(width = 1),
               showSymbol=F,
               animation=F) |>
        e_line(TMIN,
               name = "t min °C (y2)",
               y_index =1,
               itemStyle = list(color = "cyan",borderWidth = 0),
               lineStyle = list(width = 1),
               showSymbol=F,
               animation=F) |>
        e_line(fIntPotState,
               name = "canopy pot % (y1)",
               itemStyle = list(color = "green",borderWidth = 0),
               lineStyle = list(width = 1),
               showSymbol = FALSE,
               animation = FALSE,
               legend = FALSE
        ) |>
        e_area(fIntDisState,
               name = "canopy lim % (y1)",
               itemStyle = list(color = "green",borderWidth = 1),
               lineStyle = list(width = 0),  # Controls the border width of the area
               areaStyle = list(opacity = 0.8),  # Customize the area appearance (e.g., opacity)
               showSymbol = FALSE,
               animation = F
        ) |>
        e_line(bioPotState,
               name = "biomass pot q/ha (y1)",
               itemStyle = list(color = "deepskyblue",borderWidth = 0),
               lineStyle = list(width = 1, linetype=2),
               showSymbol = F,
               animation = F,
               legend = T
        ) |>

        e_area(bioDisState,
               name = "biomass lim q/ha (y1)",
               itemStyle = list(color = "deepskyblue",borderWidth = 0),
               lineStyle = list(width = 0,  linetype=2),
               showSymbol = F,
               animation = F
        ) |>
        e_line(yieldPotState,
               name = "yield pot q/ha (y1)",
               itemStyle = list(color = "blue",borderWidth = 0),
               lineStyle = list(width = 1.5),
               showSymbol = FALSE,
               animation = FALSE,
               legend = FALSE
        ) |>
        e_area(yieldDisState,
               name = "yield lim q/ha (y1)",
               itemStyle = list(color = "blue",borderWidth = 0),
               lineStyle = list(width = 1.5),
               showSymbol = FALSE,
               animation = T,
               legend = T
        ) |>
        e_area(PathogenSuitRate,
               name = "disease suit % (y1)",
               itemStyle = list(color = "orange"),
               lineStyle = list(width = 0),
               showSymbol = FALSE,
               animation=F,
               areaStyle = list(opacity = 1)) |>
        e_line(HTaffected,
               name = "disease sev (y1)",
               itemStyle = list(color = "red4",borderWidth = 1),
               lineStyle = list(width = 1.5),
               showSymbol = F,
               animation =F
        ) |>
        e_area(HTlatent,
               name = "HT latent (y1)",
               itemStyle = list(color = "grey77",borderWidth = 0),
               lineStyle = list(width = .5),
               showSymbol = F,
               animation =T
        ) |>
        e_area(HTsporulating,
               name = "HT spor (y1)",
               itemStyle = list(color = "grey55",borderWidth = 0),
               lineStyle = list(width = .5),
               showSymbol = F,
               animation =T
        ) |>
        e_area(HTdead,
               name = "HT dead (y1)",
               itemStyle = list(color = "black",borderWidth = 0),
               lineStyle = list(width = .5),
               showSymbol = F,
               animation =T
        ) |>
        e_legend(selected = list("t max °C (y2)" = F, "t min °C (y2)"=F,"rain mm (y2)"=F,
                                 "HT latent (y1)" = F, "HT spor (y1)"=F,"HT dead (y1)"=F,
                                 "biomass pot q/ha (y1)" = F, "biomass lim q/ha (y1)"=F),
                 type="scroll") |>  # Show by default
        e_y_axis(
          index=0,
          name = "yield and biomass (q/ha), disease outputs (%) ",
          position = "left",
          min = 0,  # Fixed minimum limit for primary y-axis
          max = 100  # Fixed maximum limit for primary y-axis (adjust as needed)
        ) |>
        e_y_axis(
          index=1,
          name = "temperatures (°C) and precipitation (mm)",
          position = "right",
          min = 0,  # Fixed minimum limit for primary y-axis
          max = 50  # Fixed maximum limit for primary y-axis (adjust as needed)
        ) |>
        e_theme("auritus")
    })

    output$box1 <- renderEcharts4r({
      validate(
        need(modelOutput(), "Model output is not available yet!")
      )
      if(!is.null(modelOutput()))
      {
        thisOutputs<-as.data.frame(modelOutput())

        syntheticOutputs<-thisOutputs |>
          group_by(year) |>
          filter(phenoCode==2) |>
          summarise(bioPotState = max(bioPotState/10),
                    bioDisState = max (bioDisState/10),
                    yieldPotState = max(yieldPotState/10),
                    yieldDisState = max(yieldDisState/10),
                    canopyPotState = max(fIntPotState),
                    canopyDisState = max(fIntDisState)) |>
          ungroup() |>
          summarise(bioDisState = (mean(bioPotState)-mean(bioDisState))/mean(bioPotState),
                    bioPotState = 1,
                    yieldDisState = (mean(yieldPotState)-mean(yieldDisState))/mean(yieldPotState),
                    yieldPotState = 1,
                    canopyDisState = (mean(canopyPotState)-mean(canopyDisState))/mean(canopyPotState),
                    canopyPotState =1)

        df <- data.frame(
          var = c('potential', 'limited'),
          biomass = c(round(syntheticOutputs$bioPotState*100,1), round(syntheticOutputs$bioDisState*100,1)),
          yield = c(round(syntheticOutputs$yieldPotState*100,1),round(syntheticOutputs$yieldDisState*100,1)),
          canopy = c(round(syntheticOutputs$canopyPotState*100,1),round(syntheticOutputs$canopyDisState*100,1))
        )

        df |>
          e_charts(var) |>
          e_pie(biomass,animation = FALSE) |>
          e_labels() |>
          e_tooltip() |>
          e_title(text="biomass reduction")|>
          e_legend(left='right')|>
          e_theme("auritus")

      }

    })

    output$box3 <- renderEcharts4r({
      validate(
        need(modelOutput(), "Model output is not available yet!")
      )
      if(!is.null(modelOutput()))
      {
        thisOutputs<-as.data.frame(modelOutput())

        syntheticOutputs<-thisOutputs |>
          group_by(year) |>
          filter(phenoCode==2) |>
          summarise(bioPotState = max(bioPotState/10),
                    bioDisState = max (bioDisState/10),
                    yieldPotState = max(yieldPotState/10),
                    yieldDisState = max(yieldDisState/10),
                    canopyPotState = max(fIntPotState),
                    canopyDisState = max(fIntDisState)) |>
          ungroup() |>
          summarise(bioDisState = (mean(bioPotState)-mean(bioDisState))/mean(bioPotState),
                    bioPotState = 1,
                    yieldDisState = (mean(yieldPotState)-mean(yieldDisState))/mean(yieldPotState),
                    yieldPotState = 1,
                    canopyDisState = (mean(canopyPotState)-mean(canopyDisState))/mean(canopyPotState),
                    canopyPotState =1)

        df <- data.frame(
          var = c('potential', 'limited'),
          biomass = c(round(syntheticOutputs$bioPotState*100,1), round(syntheticOutputs$bioDisState*100,1)),
          yield = c(round(syntheticOutputs$yieldPotState*100,1),round(syntheticOutputs$yieldDisState*100,1)),
          canopy = c(round(syntheticOutputs$canopyPotState*100,1),round(syntheticOutputs$canopyDisState*100,1))
        )

        df |>
          e_charts(var) |>
          e_pie(yield,animation = FALSE) |>
          e_labels() |>
          e_tooltip() |>
          e_title(text="yield reduction")|>
          e_legend(left='right')|>
          e_theme("auritus")

      }

    })

    output$box4 <- renderEcharts4r({
      validate(
        need(modelOutput(), "Model output is not available yet!")
      )
      if(!is.null(modelOutput()))
      {
        thisOutputs<-as.data.frame(modelOutput())

        syntheticOutputs<-thisOutputs |>
          group_by(year) |>
          filter(phenoCode==2) |>
          summarise(bioPotState = max(bioPotState/10),
                    bioDisState = max (bioDisState/10),
                    yieldPotState = max(yieldPotState/10),
                    yieldDisState = max(yieldDisState/10),
                    canopyPotState = max(fIntPotState),
                    canopyDisState = max(fIntDisState)) |>
          ungroup() |>
          summarise(bioDisState = (mean(bioPotState)-mean(bioDisState))/mean(bioPotState),
                    bioPotState = 1,
                    yieldDisState = (mean(yieldPotState)-mean(yieldDisState))/mean(yieldPotState),
                    yieldPotState = 1,
                    canopyDisState = (mean(canopyPotState)-mean(canopyDisState))/mean(canopyPotState),
                    canopyPotState =1)

        df <- data.frame(
          var = c('potential', 'limited'),
          biomass = c(round(syntheticOutputs$bioPotState*100,1), round(syntheticOutputs$bioDisState*100,1)),
          yield = c(round(syntheticOutputs$yieldPotState*100,1),round(syntheticOutputs$yieldDisState*100,1)),
          canopy = c(round(syntheticOutputs$canopyPotState*100,1),round(syntheticOutputs$canopyDisState*100,1))
        )

        df |>
          e_charts(var) |>
          e_pie(canopy,animation = FALSE) |>
          e_labels() |>
          e_tooltip() |>
          e_title(text="canopy reduction") |>
          e_legend(left='right')|>
          e_theme("auritus")

      }

    })

    output$box2 <- renderEcharts4r({
      validate(
        need(modelOutput(), "Model output is not available yet!")
      )
      if(!is.null(modelOutput()))
      {
        thisOutputs<-as.data.frame(modelOutput())

        syntheticOutputs<-thisOutputs |>
          group_by(year) |>
          filter(phenoCode==2) |>
          summarise(bioPotState = max(bioPotState/10),
                    bioDisState = max (bioDisState/10),
                    yieldPotState = max(yieldPotState/10),
                    yieldDisState = max(yieldDisState/10),
                    canopyPotState = max(fIntPotState),
                    canopyDisState = max(fIntDisState)) |>
          ungroup() |>
          summarise(bioPotState = mean(bioPotState),
                    bioDisState = mean (bioDisState),
                    yieldPotState = mean(yieldPotState),
                    yieldDisState = mean(yieldDisState),
                    canopyPotState = mean(canopyPotState)/100,
                    canopyDisState = mean(canopyDisState)/100)

        df <- data.frame(
          var = c('yield (Mg/ha)', 'biomass (Mg/ha)', 'canopy (%)'),
          potential = c(round(syntheticOutputs$yieldPotState,1),
                        round(syntheticOutputs$bioPotState,1),round(syntheticOutputs$canopyPotState,1)),
          limited = c(round(syntheticOutputs$yieldDisState,1),
                      round(syntheticOutputs$bioDisState,1),round(syntheticOutputs$canopyDisState,1))
        )

        df |>
          e_charts(var) |>
          e_bar(potential, stack = "grp1",animation=F) |>
          e_bar(limited, stack = "grp2",animation=F) |>
          e_labels() |>
          e_tooltip() |>
          e_y_axis(
            index=0,
            name = "",
            position = "left",
            min = 0  # Fixed minimum limit for primary y-axis
          )|>
          e_theme("auritus")
      }

    })

    output$plop <- renderPrint({
      if(!is.null(bbox()))
      {
        thisBbox<-bbox()
        s<-tidygeocoder::reverse_geo(long = thisBbox[[1]], thisBbox[[2]],
                                     method = "osm")
        # Split the string by commas
        address_parts <- strsplit(s$address, ", ")[[1]]

        # Extract the city name
        city_name <- paste0(address_parts[length(address_parts)-3],
                            ', ',address_parts[length(address_parts)])

        # Print the city name
        print( HTML(city_name))
      }
    })
}


