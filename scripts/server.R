library(shiny)
library(DT)
library(magrittr)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(lattice)
library(fmsb)
library(gsubfn)
library(proto)
library(igraph)
library(sqldf)
library(RSQLite)
library(shinyBS)
library(shinyjs)
library(rhandsontable)
library(jsonlite)
library(ggplot2)
library(devtools)
library(arcdiagram)
library(reshape2)
library(gridExtra)
library(data.table)
library(sp)
library(geojsonio)
library(rgdal)
library(maptools)

# set operating system
opSys <- "mac"
if (opSys == "Windows") {
  pathSlash <- "\\"
} else {
  pathSlash <- "/"
}

# increase file upload size limit to 10GB
options(shiny.maxRequestSize=10000*1024^2)
rootPath <- getwd()

# AS = all stations, SS = selected station
# AR = all routes, SR = selected routes
# AZ = all zones, SS = selected zone
# AH = all hours, SH - selected hour

# BS = Buses in Stations
# PB = Passengers in Bays
# PT = Passengers in Turnstiles

# stn = Station
# onOff = On-Off Counts
# seg = Route Segments
# corridorSeg = Corridor Segments
# odm = O-D Matrix

# see saveRDS for saving versions of results: http://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/

shinyServer(function(input, output, session) {

runcodeServer()
  
# Import functions from other scripts
source('plots.R', local = TRUE)$value
source('leaflet.R', local = TRUE)$value
source('workingTables.R', local = TRUE)$value
source('prettyTables.R', local = TRUE)$value
source('login.R', local = TRUE)$value
#source('SE_integration.R', local = TRUE)$value

  ## Tutorial Videos
  observe({
    output$tutorials <- renderUI({
      # selectedTutorial <- input$selectTutorial
      # if(!is.null(selectedTutorial)){
      #   link = cases(
      #     "Gyrfsrd4zK0" = selectedTutorial == 1
      #   )
      link = "v2fnaBUa9i0"
      # Instagram
      # HTML(paste0('<iframe width="400" height="270" src="http://instagram.com/p/', "BTVhC1VAaTA", "/embed",'" frameborder="0" allowfullscreen></iframe>'))
      # Youtube
      HTML(paste0('<iframe width="1600" height="800" src="https://www.youtube.com/embed/', link ,'" frameborder="0" allowfullscreen></iframe>'))
      # }
    })
  })

# Reactive values object
values <- reactiveValues()
  
### Login

USER <- reactiveValues(Logged = FALSE , session = session$user) 

observe({
if (USER$Logged == TRUE) {

# Create user folder if does not already exist
values$userName <- USER$name
print(values$userName)
userList <- as.list(list.dirs(path = paste0(rootPath, pathSlash, "data"), full.names = FALSE, recursive = FALSE))

if (!(values$userName %in% userList)) {
  dir.create(paste0(rootPath, pathSlash, "data", pathSlash, values$userName))
  dir.create(paste0(rootPath, pathSlash, "data", pathSlash, values$userName, pathSlash, "scenarios"))
}

# Initialize hrs
timeInterval <- 0.5
systemOpen <- 3.5
systemClose <- 26.5
values$hrs <- seq(systemOpen, systemClose, timeInterval)

# update scenario list
scenarioList <- as.list(list.dirs(path = paste0(rootPath, pathSlash, "data", pathSlash, values$userName, pathSlash, "scenarios", pathSlash), full.names = FALSE, recursive = FALSE))
names(scenarioList) <- scenarioList
updateSelectInput(session, "selectScenario",
                  choices = scenarioList
)

observeEvent(input$deleteScenario, {
  
  inputScenarioPath <- paste0(rootPath, pathSlash, "data", pathSlash, values$userName, pathSlash, "scenarios", pathSlash, input$selectScenario)
  unlink(inputScenarioPath, recursive = TRUE)
  
  scenarioList <- as.list(list.dirs(path = paste0(rootPath, pathSlash, "data", pathSlash, values$userName, pathSlash, "scenarios", pathSlash), full.names = FALSE, recursive = FALSE))
  names(scenarioList) <- scenarioList
  updateSelectInput(session, "selectScenario",
                    choices = scenarioList
  )
})

### Import data
  
## Import processed data
observeEvent(input$importScenario, {
  withProgress(message = 'Importing Scenario', value = 0, {
    
    setProgress(0.1, detail = "Scenario")
    if (!is.null(input$inputScenario)) {
      # unzip
      inputScenarioName <- gsub(".zip", "", input$inputScenario$name)
      inputScenarioPath <- paste0(rootPath, pathSlash, "data", pathSlash, values$userName, pathSlash, "scenarios", pathSlash, inputScenarioName)

      savedDataZip <- input$inputScenario
      savedData <- unzip(savedDataZip$datapath, 
                         files = NULL, 
                         list = FALSE, 
                         overwrite = TRUE, 
                         junkpaths = TRUE,
                         exdir = inputScenarioPath,
                         unzip = "internal")
      
      # read files
      values$stationInventory <- read.csv(paste0(inputScenarioPath, pathSlash, "stationInventory.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$stationInventory) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stationInventory))
      values$routeData <- read.csv(paste0(inputScenarioPath, pathSlash, "route_data.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$routeData) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$routeData))
      values$stops <- read.csv(paste0(inputScenarioPath, pathSlash, "stops.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$stops) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stops))
      values$stop_times <- read.csv(paste0(inputScenarioPath, pathSlash, "stop_times.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$stop_times) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stop_times))
      values$trips <- read.csv(paste0(inputScenarioPath, pathSlash, "trips.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$trips) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$trips))
      values$odm_AZ_AS_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "odm_AZ_AS_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$odm_AZ_AS_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$odm_AZ_AS_AH))
      values$odm_AZ_AS_AH_10 <- read.csv(paste0(inputScenarioPath, pathSlash, "odm_AZ_AS_AH_10.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$odm_AZ_AS_AH_10) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$odm_AZ_AS_AH_10))
      values$stn_AS_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "stn_AS_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$stn_AS_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stn_AS_AH))
      values$seg_AR_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "seg_AR_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$seg_AR_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$seg_AR_AH))
      values$onOff_AS_AR_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "onOff_AS_AR_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$onOff_AS_AR_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$onOff_AS_AR_AH))
      values$corridorSeg_AZ_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "corridorSeg_AZ_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$corridorSeg_AZ_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$corridorSeg_AZ_AH))
      values$GeoJSONroutes <- rgdal::readOGR(paste0(inputScenarioPath, pathSlash, "routes.geojson"), "OGRGeoJSON", stringsAsFactors = FALSE)
      values$GeoJSONstops <- rgdal::readOGR(paste0(inputScenarioPath, pathSlash, "stops.geojson"), "OGRGeoJSON", stringsAsFactors = FALSE)
      
      # update scenario list
      scenarioList <- as.list(list.dirs(path = paste0(rootPath, pathSlash, "data", pathSlash, values$userName, pathSlash, "scenarios", pathSlash), full.names = FALSE, recursive = FALSE))
      names(scenarioList) <- scenarioList
      updateSelectInput(session, "selectScenario",
                        choices = scenarioList
      )
    }
    setProgress(0.3, detail = "Station Inventory")
    if (!is.null(input$inputStationInventory1)) {
      values$stationInventory <- read.csv(input$inputStationInventory1$datapath, header = TRUE, fileEncoding = "UTF-8")
      names(values$stationInventory) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stationInventory))
    }
    # Station input list
    stationList <- values$stationInventory$station_id
    names(stationList) <- as.character(values$stationInventory$station_name)
    stationList <- stationList[order(values$stationInventory$station_name)]
    
    updateSelectizeInput(session, "stationToDraw",
                      choices = stationList
    )
    updateSelectizeInput(session, "selectStation",
                      choices = stationList
    )
    # Update Corridor List
    newCorridorList <- unique(values$stationInventory$corridor)
    
    updateSelectInput(session, "selectCorridorsForSeg", 
                      choices = newCorridorList)
    # Reposition map to centroid of system
    leafletProxy("map") %>%
      setView(lng = mean(values$stationInventory$station_lon),
              lat = mean(values$stationInventory$station_lat),
              zoom = 12)
    setProgress(0.4, detail = "Complete O-D Matrix")
    if (!is.null(input$inputCompleteODM)) {
      values$odm_AZ_AS_AH <- read.csv(input$inputCompleteODM$datapath)
    }      
    setProgress(0.5, detail = "Filtered O-D Matrix")
    if (!is.null(input$inputFilteredODM)) {
      values$odm_AZ_AS_AH_10 <- read.csv(input$inputFilteredODM$datapath)
    }
    setProgress(0.6, detail = "Station Data")
    if (!is.null(input$inputStnData)) {
      values$stn_AS_AH <- read.csv(input$inputStnData$datapath)
    }
    values$hrs <- sort(unique(values$stn_AS_AH$hr))
    setProgress(0.7, detail = "Route Data")
    if (!is.null(input$inputRouteSegData)) {
      values$seg_AR_AH <- read.csv(input$inputRouteSegData$datapath)
    }
    setProgress(0.8, detail = "On-Off Data")
    if (!is.null(input$inputOnOffData)) {
      values$onOff_AS_AR_AH <- read.csv(input$inputOnOffData$datapath)
    }
    setProgress(0.9, detail = "Corridor Saturation")
    if (!is.null(input$inputCorridorSegSat)) {
      values$corridorSeg_AZ_AH <- read.csv(input$inputCorridorSegSat$datapath) 
    }
    
    # Service list
    serviceList <- unique(values$trips$service_id)
    updateSelectInput(session, "service_id",
                      choices = serviceList)
    
    # Emme route list
    routeSegList <- as.list(as.character(values$seg_AR_AH$route_id))
    names(routeSegList) <- as.character(paste(values$seg_AR_AH$route_short_name, " | ", values$seg_AR_AH$route_id))
    routeSegList <- routeSegList[order(values$seg_AR_AH$route_short_name)]
    routeSegList <- routeSegList[!duplicated(routeSegList)]
    updateSelectInput(session, "selectRouteSeg", 
                      choices = routeSegList)
    
    # Filtered Stops dataframes
    values$GeoJSONstops_stations <- values$GeoJSONstops[values$GeoJSONstops$location_type == 1, ]
    values$GeoJSONstops_platforms <- values$GeoJSONstops[!is.na(values$GeoJSONstops$parent_station), ]
    values$GeoJSONstops_stops <- values$GeoJSONstops[(values$GeoJSONstops$location_type == 0 && is.na(values$GeoJSONstops$parent_station)), ]
    #values$GeoJSONstops <- geojsonio::geojson_read(input$inputGeoJSONstops$datapath, what = "sp")
    
    # GeoJSON stop list
    geojsonStopList <- as.list(as.character(values$GeoJSONstops$stop_id))
    names(geojsonStopList) <- as.character(paste(values$GeoJSONstops$stop_name, " | ", values$GeoJSONstops$stop_id))
    geojsonStopList <- geojsonStopList[order(values$GeoJSONstops$stop_name)]
    geojsonStopList <- geojsonStopList[!duplicated(geojsonStopList)]
    updateSelectInput(session, "selectGeoJSONstop",
                      choices = geojsonStopList)
    # Stops At list
    updateSelectInput(session, "selectStopsAt",
                      choices = geojsonStopList)
    
    setProgress(1.0, detail = "Complete")
  })
})

## Switch scenario
observeEvent(input$switchScenario, {
  inputScenarioPath <- paste0(rootPath, pathSlash, "data", pathSlash, values$userName, pathSlash, "scenarios", pathSlash, input$selectScenario)
  switchScenario(inputScenarioPath)
})

observeEvent(input$demo, {
  inputScenarioPath <- paste0(rootPath, pathSlash, "data", pathSlash, "Demo", pathSlash, "scenarios", pathSlash, "Demo")
  switchScenario(inputScenarioPath)
  updateTabsetPanel(session, "nav",
                    selected = 'map')
})
  
switchScenario <- function(inputScenarioPath){
  withProgress(message = 'Switching Scenario', value = 0, {
    setProgress(0.1, detail = "Reading custom files")
    values$stationInventory <- read.csv(paste0(inputScenarioPath, pathSlash, "stationInventory.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$stationInventory) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stationInventory))
    values$routeData <- read.csv(paste0(inputScenarioPath, pathSlash, "route_data.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$routeData) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$routeData))
    setProgress(0.2, detail = "Reading GTFS")
    values$stops <- read.csv(paste0(inputScenarioPath, pathSlash, "stops.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$stops) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stops))
    values$stop_times <- read.csv(paste0(inputScenarioPath, pathSlash, "stop_times.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$stop_times) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stop_times))
    values$trips <- read.csv(paste0(inputScenarioPath, pathSlash, "trips.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$trips) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$trips))
    setProgress(0.4, detail = "Reading working tables")
    values$odm_AZ_AS_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "odm_AZ_AS_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$odm_AZ_AS_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$odm_AZ_AS_AH))
    values$odm_AZ_AS_AH_10 <- read.csv(paste0(inputScenarioPath, pathSlash, "odm_AZ_AS_AH_10.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$odm_AZ_AS_AH_10) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$odm_AZ_AS_AH_10))
    values$stn_AS_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "stn_AS_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$stn_AS_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stn_AS_AH))
    values$seg_AR_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "seg_AR_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$seg_AR_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$seg_AR_AH))
    values$onOff_AS_AR_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "onOff_AS_AR_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$onOff_AS_AR_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$onOff_AS_AR_AH))
    values$corridorSeg_AZ_AH <- read.csv(paste0(inputScenarioPath, pathSlash, "corridorSeg_AZ_AH.csv"), header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(values$corridorSeg_AZ_AH) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$corridorSeg_AZ_AH))
    setProgress(0.5, detail = "Reading geojsons")
    values$GeoJSONroutes <- rgdal::readOGR(paste0(inputScenarioPath, pathSlash, "routes.geojson"), "OGRGeoJSON", stringsAsFactors = FALSE)
    values$GeoJSONstops <- rgdal::readOGR(paste0(inputScenarioPath, pathSlash, "stops.geojson"), "OGRGeoJSON", stringsAsFactors = FALSE)
    
    setProgress(0.6, detail = "Updating input lists")
    # Station input list
    stationList <- values$stationInventory$station_id
    names(stationList) <- as.character(values$stationInventory$station_name)
    stationList <- stationList[order(values$stationInventory$station_name)]
    
    updateSelectizeInput(session, "stationToDraw",
                         choices = stationList
    )
    updateSelectizeInput(session, "selectStation",
                         choices = stationList
    )
    # Update Corridor List
    newCorridorList <- unique(values$stationInventory$corridor)
    
    updateSelectInput(session, "selectCorridorsForSeg", 
                      choices = newCorridorList)
    # Reposition map to centroid of system
    leafletProxy("map") %>%
      setView(lng = mean(values$stationInventory$station_lon),
              lat = mean(values$stationInventory$station_lat),
              zoom = 12)
    
    # Service list
    serviceList <- unique(values$trips$service_id)
    updateSelectInput(session, "service_id",
                      choices = serviceList)
    
    # Emme route list
    routeSegList <- as.list(as.character(values$seg_AR_AH$route_id))
    names(routeSegList) <- as.character(paste(values$seg_AR_AH$route_short_name, " | ", values$seg_AR_AH$route_id))
    routeSegList <- routeSegList[order(values$seg_AR_AH$route_short_name)]
    routeSegList <- routeSegList[!duplicated(routeSegList)]
    updateSelectInput(session, "selectRouteSeg", 
                      choices = routeSegList)
    
    # Filtered Stops dataframes
    values$GeoJSONstops_stations <- values$GeoJSONstops[values$GeoJSONstops$location_type == 1, ]
    values$GeoJSONstops_platforms <- values$GeoJSONstops[!is.na(values$GeoJSONstops$parent_station), ]
    values$GeoJSONstops_stops <- values$GeoJSONstops[(values$GeoJSONstops$location_type == 0 && is.na(values$GeoJSONstops$parent_station)), ]
    #values$GeoJSONstops <- geojsonio::geojson_read(input$inputGeoJSONstops$datapath, what = "sp")
    
    # GeoJSON stop list
    geojsonStopList <- as.list(as.character(values$GeoJSONstops$stop_id))
    names(geojsonStopList) <- as.character(paste(values$GeoJSONstops$stop_name, " | ", values$GeoJSONstops$stop_id))
    geojsonStopList <- geojsonStopList[order(values$GeoJSONstops$stop_name)]
    geojsonStopList <- geojsonStopList[!duplicated(geojsonStopList)]
    updateSelectInput(session, "selectGeoJSONstop",
                      choices = geojsonStopList)
    # Stops At list
    updateSelectInput(session, "selectStopsAt",
                      choices = geojsonStopList)
    
    setProgress(1, detail = "Switch complete")
  })
}

## Create scenario
observeEvent(input$processRaw, {
  
  withProgress(message = 'Processing Raw Inputs', value = 0, {
    
    ## GTFS
    setProgress(0.1, detail = "GTFS")

    # Import
    if (!is.null(input$inputGTFS)) {
      gtfs <- unzip(input$inputGTFS$datapath, 
                    files = NULL, 
                    list = FALSE, 
                    overwrite = TRUE, 
                    exdir = paste0("data", pathSlash, values$userName, pathSlash, "gtfs"),
                    unzip = "internal"
      )
      
      calendar <- read.csv(paste0("data", pathSlash, values$userName, pathSlash, "gtfs", pathSlash, "calendar.txt"), header = TRUE, stringsAsFactors = FALSE)
      routes <- read.csv(paste0("data", pathSlash, values$userName, pathSlash, "gtfs", pathSlash, "routes.txt"), header = TRUE, stringsAsFactors = FALSE)
      shapes <- read.csv(paste0("data", pathSlash, values$userName, pathSlash, "gtfs", pathSlash, "shapes.txt"), header = TRUE, stringsAsFactors = FALSE)
      stop_times <- read.csv(paste0("data", pathSlash, values$userName, pathSlash, "gtfs", pathSlash, "stop_times.txt"), header = TRUE, 
                             colClasses = c("character", "character", "character", "character", "integer"))
      stops <- read.csv(paste0("data", pathSlash, values$userName, pathSlash, "gtfs", pathSlash, "stops.txt"), header = TRUE, stringsAsFactors = FALSE)
      trips <- read.csv(paste0("data", pathSlash, values$userName, pathSlash, "gtfs", pathSlash, "trips.txt"), header = TRUE, stringsAsFactors = FALSE)
      
      # Process
      processedGTFS <- process_gtfs(input$dayOfWeek, calendar, routes, trips, shapes, stops, stop_times)
      routes <- processedGTFS$routes
      shapes <- processedGTFS$shapes
      stops <- values$stops <- processedGTFS$stops
      stop_times <- values$stop_times <- processedGTFS$stop_times
      values$trips <- trips
      
      values$hrs <- sort(unique(stop_times$arrival_hr))
    }
  
    ## Station Inventory
    setProgress(0.2, detail = "Station Inventory")
    # FIX user constructs values$stationInventory from stops.txt, manual entry of # turnstiles, # entrances and station order
    if (!is.null(input$inputStationInventory2)) {
      values$stationInventory <- read.csv(input$inputStationInventory2$datapath, header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      names(values$stationInventory) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(values$stationInventory))
      
      # Station input list
      stationList <- values$stationInventory$station_id
      names(stationList) <- as.character(values$stationInventory$station_name)
      stationList <- stationList[order(values$stationInventory$station_name)]
      
      updateSelectizeInput(session, "stationToDraw",
                           choices = stationList
      )
      updateSelectizeInput(session, "selectStation",
                           choices = stationList
      )
      
      # Update Corridor List
      newCorridorList <- unique(values$stationInventory$corridor)
      
      updateSelectInput(session, "selectCorridorsForSeg", 
                        choices = newCorridorList)
      
      # Reposition map to centroid of system
      leafletProxy("map") %>%
        setView(lng = mean(values$stationInventory$station_lon),
                lat = mean(values$stationInventory$station_lat),
                zoom = 12)
    }

    # stops2stations # FIX try to eliminate need for stops2stations
    if (!is.null(input$inputGTFS) || !is.null(input$inputStationInventory2)) {
      stationInventory <- values$stationInventory
      stops2stations <- sqldf(c("
                                SELECT
                                stops.stop_id,
                                stops.stop_code,
                                stops.stop_name,
                                parents.stop_id AS station_id,
                                parents.stop_name AS station_name,
                                stationInventory.station_id_emme,
                                stationInventory.corridor
                                FROM stops
                                INNER JOIN stops AS parents
                                ON stops.parent_station = parents.stop_id
                                INNER JOIN stationInventory
                                ON parents.stop_id = stationInventory.station_id"
      ))
    }
    ## Route Data
    setProgress(0.25, detail = "Route Data")
    
    if (!is.null(input$inputRouteData)) {
      
      routeData <- read.csv(input$inputRouteData$datapath, encoding = "UTF-8", sep = ",", stringsAsFactors = FALSE)
      names(routeData) = gsub(pattern = "X.U.FEFF.", replacement = "", x = names(routeData))
      routeData[is.na(routeData)] <- 0
      routeData[routeData == "#N/A"] <- 0
      routeData[routeData == ""] <- 0
      routeData[routeData == Inf] <- 0
      routeData[routeData == -Inf] <- 0
      routeData$ridership <- as.numeric(routeData$ridership)
      routeData$IPB <- as.numeric(routeData$IPB)
      routeData$IPK <- as.numeric(routeData$IPK)
      routeData$completion <- as.numeric(routeData$completion)
      routeData$headway_am <- as.numeric(routeData$headway_am)
      routeData$headway_op <- as.numeric(routeData$headway_op)
      routeData$headway_pm <- as.numeric(routeData$headway_pm)
      routeData$route_short_name <- sub('_', '', routeData$route_short_name)
      
      values$routeData_FR <- values$routeData <- routeData
      
      # GeoJSON agency list
      geojsonAgencyList <- unique(as.character(routeData$agency_id))
      updateSelectInput(session, "selectGeoJSONagency",
                        choices = geojsonAgencyList)
      
      # GeoJSON origin zones
      oZoneList <- unique(as.character(routeData$zone_o))
      updateSelectInput(session, "originZone",
                        choices = oZoneList)
      
      # GeoJSON destination zones
      dZoneList <- unique(as.character(routeData$zone_d))
      updateSelectInput(session, "destinationZone",
                        choices = dZoneList)
      
      # GeoJSON operators
      operatorList <- unique(as.character(routeData$operator))
      updateSelectInput(session, "operator",
                        choices = operatorList)
      
      # GeoJSON route list
      geojsonRouteList <- as.list(as.character(routeData$route_id))
      names(geojsonRouteList) <- as.character(paste(routeData$route_short_name, " | ", routeData$route_id))
      geojsonRouteList <- geojsonRouteList[order(routeData$route_short_name)]
      geojsonRouteList <- geojsonRouteList[!duplicated(geojsonRouteList)]
      updateSelectInput(session, "selectGeoJSONroute",
                        choices = geojsonRouteList)
      
      if (!is.null(input$inputGTFS)) {
        # Update routes
        routes <- sqldf(c(
          "UPDATE
          routes
          SET 
          emme_id = (
          SELECT emme_id
          FROM routeData
          WHERE routes.route_id = routeData.route_id),
          vehicleCapacity = (
          SELECT vehicleCapacity
          FROM routeData
          WHERE routes.route_id = routeData.route_id),
          platformUse = (
          SELECT platformUse
          FROM routeData
          WHERE routes.route_id = routeData.route_id)",
          "SELECT * 
          FROM routes"
        ))
      }
    }

    ## Emme Output
    setProgress(.3, detail = "Emme Results")
    if (!is.null(input$inputEmme)) {
      
      fname <- input$inputEmme$datapath
      
      emmeTables <- processEmme(fname)
      assignment <- emmeTables$assignment
      routeDataEmme <- emmeTables$routeDataEmme
      
    }
    
    ## Make O-D Matrix
    setProgress(.5, detail = "O-D Matrices")
    
    if (!is.null(input$inputTDM) || !is.null(input$inputTLM) || !is.null(input$inputADM) || !is.null(input$inputALM)) {
      odm <- make_odm(values$stationInventory, values$hrs)
    
      # Transit Demand Matrix      
      if (!is.null(input$inputTDM)) {
        tdm <- read.csv(input$inputTDM$datapath, stringsAsFactors = FALSE)
        keep <- c("O", "D", "hr", "trips")
        tdm <- tdm[keep]
        tdm$O <- as.integer(tdm$O)
        tdm$D <- as.integer(tdm$D)
        setnames(tdm, "trips", "transit_trips")
        odm <- update_odm(odm, tdm, "transit_trips", TRUE)
      }
      
      # Transit LOS Matrix
      if (!is.null(input$inputTLM)) {
        tlm <- read.csv(input$inputTLM$datapath, stringsAsFactors = FALSE)
        keep <- c("O", "D", "hr", "travel_time")
        tlm <- tlm[keep]
        tlm$O <- as.integer(tlm$O)
        tlm$D <- as.integer(tlm$D)
        setnames(tlm, "travel_time", "TT_transit")
        #tlm$pair_id <- do.call(paste, c(tlm[c("O", "D")], sep = "-"))
        odm <- update_odm(odm, tlm, "TT_transit", TRUE)
      }
      
      # Auto Demand Matrix
      if (!is.null(input$inputADM)) {
        adm <- read.csv(input$inputADM$datapath, stringsAsFactors = FALSE)
        keep <- c("O", "D", "hr", "trips")
        adm <- adm[keep]
        adm$O <- as.integer(adm$O)
        adm$D <- as.integer(adm$D)
        setnames(adm, "trips", "auto_trips")
        odm <- update_odm(odm, adm, "auto_trips", FALSE)
      }
      
      # Auto LOS Matrix
      if (!is.null(input$inputALM)) {
        alm <- read.csv(input$inputALM$datapath, stringsAsFactors = FALSE)
        keep <- c("O", "D", "hr", "travel_time")
        alm <- alm[keep]
        alm$O <- as.integer(alm$O)
        alm$D <- as.integer(alm$D)
        setnames(alm, "travel_time", "TT_auto")
        # FIX add checkbox for if matrix is by hour or not.
        odm <- update_odm(odm, alm, "TT_auto", FALSE)
      }
      
      # Calculated fields
      odm$pop_TT_transit <- with(odm, transit_trips * TT_transit)
      odm$pop_TT_auto <- with(odm, auto_trips * TT_auto)
      odm$TT_ratio <- ifelse(odm$TT_auto == 0, 0, odm$TT_transit / odm$TT_auto)
      odm$TT_lost <- (odm$TT_transit - odm$TT_auto) * odm$transit_trips
      
      values$odm_AZ_AS_AH <- odm
      
      # Filtered O-D Matrix
      values$odm_AZ_AS_AH_10 <- make_odm_10(values$odm_AZ_AS_AH, values$stationInventory)
    }
      
    ## Station Drawings
    setProgress(.6, detail = "Station Drawings")
    
    if (!is.null(input$inputStationDrawings)) {
      stationDrawings <- unzip(input$inputStationDrawings$datapath, 
                               files = NULL,
                               list = FALSE,
                               overwrite = TRUE,
                               exdir = "www",
                               unzip = "internal"
      )
    }
    
    ## Conveyal Scenario
    setProgress(.7, detail = "Conveyal Scenario")
    
    if (!is.null(input$inputConveyalScenario) && !is.null(input$inputGTFS)) {
      conveyal <- read.json(input$inputConveyalScenario$datapath)
      conveyalApplied <- applyConveyal(calendar, routes, stop_times, stops, trips, conveyal)
    }
    
    ## GeoJSON
    setProgress(.8, detail = "GeoJSON files")
    
    # Routes
    if(!is.null(input$inputGeoJSONroutes)) {
      GeoJSONroutes <- values$GeoJSONroutes <- rgdal::readOGR(input$inputGeoJSONroutes$datapath, "OGRGeoJSON", stringsAsFactors = FALSE)
    }
    
    # Stops
    if(!is.null(input$inputGeoJSONstops)) {
      values$GeoJSONstops <- rgdal::readOGR(input$inputGeoJSONstops$datapath, "OGRGeoJSON")
      values$GeoJSONstops_stations <- values$GeoJSONstops[values$GeoJSONstops$location_type == 1, ]
      values$GeoJSONstops_platforms <- values$GeoJSONstops[!is.na(values$GeoJSONstops$parent_station), ]
      #values$GeoJSONstops_stops <- values$GeoJSONstops[(values$GeoJSONstops$location_type == 0 && is.na(values$GeoJSONstops$parent_station)), ]
      values$GeoJSONstops_stops <- values$GeoJSONstops[(values$GeoJSONstops$location_type == 0), ]
      #values$GeoJSONstops <- geojsonio::geojson_read(input$inputGeoJSONstops$datapath, what = "sp")
      
      # GeoJSON stop list
      geojsonStopList <- as.list(as.character(values$GeoJSONstops$stop_id))
      names(geojsonStopList) <- as.character(paste(values$GeoJSONstops$stop_name, " | ", values$GeoJSONstops$stop_id))
      geojsonStopList <- geojsonStopList[order(values$GeoJSONstops$stop_name)]
      geojsonStopList <- geojsonStopList[!duplicated(geojsonStopList)]
      updateSelectInput(session, "selectGeoJSONstop",
                        choices = geojsonStopList)
      # Stops At list
      updateSelectInput(session, "selectStopsAt",
                        choices = geojsonStopList)
    }
    
    ## Update working tables
    setProgress(.9, detail = "Processing")
    
    if(!is.null(input$inputGTFS)) {
      # Stations
      values$stn_AS_AH <- make_stn(values$stationInventory, values$hrs)
      values$stn_AS_AH <- update_BS_saturation(values$stn_AS_AH, stops2stations, calendar, routes, stops, stop_times, trips, input$dayOfWeek, input$timeInterval)
      
      # Service list
      serviceList <- unique(values$trips$service_id)
      updateSelectInput(session, "service_id",
                        choices = serviceList)
    }
    # if O-D matrices imported
    if (!is.null(input$inputTDM) && !is.null(input$inputTLM)) {
      values$stn_AS_AH <- update_odSums(values$stn_AS_AH, values$odm_AZ_AS_AH)
    }
    
    if(!is.null(input$inputEmme)) {
      
      # On Off Counts
      values$onOff_AS_AR_AH <- make_onOff(values$stationInventory, routes, assignment, routeDataEmme)
      
      # PB Saturation
      if (!is.null(input$inputTDM)) {
        values$stn_AS_AH <- update_PB_saturation(values$stn_AS_AH, values$onOff_AS_AR_AH)
      }
      
      # Route Segment Saturation
      if (!is.null(values$onOff_AS_AR_AH) && nrow(values$onOff_AS_AR_AH) > 0) {
        values$seg_AR_AH <- make_seg(values$onOff_AS_AR_AH)
        
        # Corridor Segment Saturation
        values$corridorSeg_AZ_AH <- make_corridorSeg(values$stationInventory, values$seg_AR_AH, values$hrs)
      }
      # Emme route list
      routeSegList <- as.list(as.character(values$seg_AR_AH$route_id))
      names(routeSegList) <- as.character(paste(values$seg_AR_AH$route_short_name, " | ", values$seg_AR_AH$route_id))
      routeSegList <- routeSegList[order(values$seg_AR_AH$route_short_name)]
      routeSegList <- routeSegList[!duplicated(routeSegList)]
      updateSelectInput(session, "selectRouteSeg", 
                        choices = routeSegList)
    }
    
    setProgress(1, detail = "Complete")
  })
})

#observeEvent(c(input$importScenario, input$processRaw, input$switchScenario), ignoreInit = TRUE, {

## Export Data
# Scenario
observe({
  output$download_scenario <- downloadHandler(
    filename = function() {
      if (!is.null(input$scenarioName)) {
        paste0(input$scenarioName, ".zip")
      } else {
        "scenario.zip"
      }
    },
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tmpdir)
      write.csv(values$stationInventory, "stationInventory.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$routeData, "route_data.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$stops, "stops.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$stop_times, "stop_times.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$trips, "trips.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$odm_AZ_AS_AH, "odm_AZ_AS_AH.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$odm_AZ_AS_AH_10, "odm_AZ_AS_AH_10.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$stn_AS_AH, "stn_AS_AH.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$seg_AR_AH, "seg_AR_AH.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$onOff_AS_AR_AH, "onOff_AS_AR_AH.csv", row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(values$corridorSeg_AZ_AH, "corridorSeg_AZ_AH.csv", row.names = FALSE, fileEncoding = "UTF-8")
      geojson_write(values$GeoJSONroutes, file = "routes.geojson", precision = 8)
      geojson_write(values$GeoJSONstops, file = "stops.geojson", precision = 8)
      zip(zipfile=fname, files = c("stationInventory.csv", "route_data.csv", "stops.csv", "stop_times.csv", "trips.csv", 
                                   "odm_AZ_AS_AH.csv", "odm_AZ_AS_AH_10.csv", "stn_AS_AH.csv", "seg_AR_AH.csv", 
                                   "onOff_AS_AR_AH.csv", "corridorSeg_AZ_AH.csv", "routes.geojson", "stops.geojson"), 
                                    zip = Sys.getenv("R_ZIPCMD", "zip"))
    },
    contentType = paste0("application", pathSlash, "zip")
  )
  outputOptions(output, 'download_scenario', suspendWhenHidden=FALSE)
  
  # Station Inventory
  output$download_stationInventory <- downloadHandler(
    filename = function() { 'stationInventory.csv' },
    content = function(file) {
      write.csv(values$stationInventory, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  # Complete O-D Matrix
  output$download_odm_AZ_AS_AH <- downloadHandler(
    filename = function() { 'odm_AZ_AS_AH.csv' },
    content = function(file) {
      write.csv(values$odm_AZ_AS_AH, file, row.names = FALSE)
    }
  )
  # Filtered O-D Matrix
  output$download_odm_AZ_AS_AH_10 <- downloadHandler(
    filename = function() { 'odm_AZ_AS_AH_10.csv' },
    content = function(file) {
      write.csv(values$odm_AZ_AS_AH_10, file, row.names = FALSE)
    }
  )
  # Station Data
  output$download_stn_AS_AH <- downloadHandler(
    filename = function() { 'stn_AS_AH.csv' },
    content = function(file) {
      write.csv(values$stn_AS_AH, file, row.names = FALSE)
    }
  )
  # Route Data
  output$download_seg_AR_AH <- downloadHandler(
    filename = function() { 'seg_AR_AH.csv' },
    content = function(file) {
      write.csv(values$seg_AR_AH, file, row.names = FALSE)
    }
  )
  # On-Off Data
  output$download_onOff_AS_AR_AH <- downloadHandler(
    filename = function() { 'onOff_AS_AR_AH.csv' },
    content = function(file) {
      write.csv(values$onOff_AS_AR_AH, file, row.names = FALSE)
    }
  )
  # Corridor Data
  output$download_corridorSeg_AZ_AH <- downloadHandler(
    filename = function() { 'corridorSeg_AZ_AH.csv' },
    content = function(file) {
      write.csv(values$corridorSeg_AZ_AH, file, row.names = FALSE)
    }
  )
})
  
## Clean working tables

# observe({
#   
#   values$stn_AS_AH <- values$stn_AS_AH[is.na(values$stn_AS_AH)] <- 0
#   do.call(data.frame, lapply(values$stn_AS_AH, function(x) replace(x, is.infinite(x), 0)))
#   
#   values$onOff_AS_AR_AH <- values$onOff_AS_AR_AH[is.na(values$onOff_AS_AR_AH)] <- 0
#   do.call(data.frame, lapply(values$onOff_AS_AR_AH, function(x) replace(x, is.infinite(x), 0)))
#   
#   values$seg_AR_AH <- values$seg_AR_AH[is.na(values$seg_AR_AH)] <- 0
#   do.call(data.frame, lapply(values$seg_AR_AH, function(x) replace(x, is.infinite(x), 0)))
#   
#   values$corridorSeg_AZ_AH <- values$corridorSeg_AZ_AH[is.na(values$corridorSeg_AZ_AH)] <- 0
#   do.call(data.frame, lapply(values$corridorSeg_AZ_AH, function(x) replace(x, is.infinite(x), 0)))
#   
#   values$odm_AZ_AS_AH_10 <- values$odm_AZ_AS_AH_10[is.na(values$odm_AZ_AS_AH_10)] <- 0
#   do.call(data.frame, lapply(values$odm_AZ_AS_AH_10, function(x) replace(x, is.infinite(x), 0)))
# })

## Make Pretty Tables

observe({
  # Station table
  if (!is.null(values$stn_AS_AH)) {
    output$stnSatTable <- make_stn_pretty(values$stn_AS_AH)
  }
  # Routes table
  if (!is.null(values$seg_AR_AH)) {
    output$routeSegmentTable <- make_seg_pretty(values$seg_AR_AH)
  }
  # O-D table
  if (!is.null(values$odm_AZ_AS_AH_10)) {
    output$odTable <- make_odm_10_pretty(values$odm_AZ_AS_AH_10)
  }
  # Corridor table
  if (!is.null(values$corridorSeg_AZ_AH)) {
    output$corridorSegmentTable <- make_corridorSeg_pretty(values$corridorSeg_AZ_AH)
  }
})

## Make sub tables
  
observeEvent(input$timeSlider, {

  values$stn_AS_SH <- values$stn_AS_AH[values$stn_AS_AH$hr == input$timeSlider,]
  values$stn_SS_SH <- values$stn_SS_AH[values$stn_SS_AH$hr == input$timeSlider,]
  values$onOff_AS_AR_SH <- values$onOff_AS_AR_AH[values$onOff_AS_AR_AH$hr == input$timeSlider,]
  values$onOff_SS_AR_SH <- values$onOff_SS_AR_AH[values$onOff_SS_AR_AH$hr == input$timeSlider,]
  values$seg_AR_SH <- values$seg_AR_AH[values$seg_AR_AH$hr == input$timeSlider,]
  values$seg_SR_SH <- values$seg_SR_AH[values$seg_SR_AH$hr == input$timeSlider,]
  values$corridorSeg_AZ_SH <- values$corridorSeg_AZ_AH[values$corridorSeg_AZ_AH$hr == input$timeSlider,]

  # if(input$studyElement == "odPairs") {
  #   values$odm_AZ_AS_SH_10 <- values$odm_AZ_AS_AH_10[values$odm_AZ_AS_AH_10$hr == input$timeSlider,]
  #   values$odm_SZ_AS_SH_10 <- values$odm_SZ_AH_10[values$odm_SZ_AH_10$hr == input$timeSlider,]
  # }
})

observeEvent(input$selectStation, {

  values$stn_SS_AH <- values$stn_AS_AH[values$stn_AS_AH$station_id == input$selectStation,]
  values$stn_SS_SH <- values$stn_AS_SH[values$stn_AS_SH$station_id == input$selectStation,]
  values$onOff_SS_AR_AH <- values$onOff_AS_AR_AH[values$onOff_AS_AR_AH$station_id == input$selectStation,]
  values$onOff_SS_AR_SH <- values$onOff_AS_AR_SH[values$onOff_AS_AR_SH$station_id == input$selectStation,]

  values$stn_name <- values$stationInventory$station_name[values$stationInventory$station_id == input$selectStation]
})

observeEvent(input$selectRouteSeg, {

  values$seg_SR_AH <- values$seg_AR_AH[values$seg_AR_AH$route_id == input$selectRouteSeg,]
  values$seg_SR_SH <- values$seg_AR_SH[values$seg_AR_SH$route_id == input$selectRouteSeg,]
})

observeEvent(input$selectCorridorsForSeg, {

  corridorSeg_temp <- values$corridorSeg_AZ_SH[values$corridorSeg_AZ_SH$corridorA == input$selectCorridorsForSeg, ]
  values$corridorSeg_SZ_SH <- corridorSeg_temp[corridorSeg_temp$corridorB == input$selectCorridorsForSeg, ]
})

# Filter GeoJSON routes
observeEvent(input$selectGeoJSONagency, ignoreInit = TRUE, {
  # agencies
  if (!is.null(input$selectGeoJSONagency)) {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO <- values$routeData_SA_SS <- values$routeData_SA <- values$routeData[values$routeData$agency_id %in% input$selectGeoJSONagency, ]
  } else {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO <- values$routeData_SA_SS <- values$routeData_SA <- values$routeData
  }

  # update origin zones list
  oZoneList <- unique(as.character(values$routeData_FR$zone_o))
  updateSelectInput(session, "originZone",
                    choices = oZoneList)

  # update destination zones list
  dZoneList <- unique(as.character(values$routeData_FR$zone_d))
  updateSelectInput(session, "destinationZone",
                    choices = dZoneList)

  # update operators list
  operatorList <- unique(as.character(values$routeData_FR$operator))
  updateSelectInput(session, "operator",
                    choices = operatorList)
})

observeEvent(input$selectStopsAt, ignoreInit = TRUE, {
  # stops at
  if (!is.null(input$selectStopsAt)) {
    stops_in_stations <- values$stops[values$stops$parent_station %in% input$selectStopsAt, ]
    stop_times_SS <- values$stop_times[(values$stop_times$stop_id %in% input$selectStopsAt) | 
                                         (values$stop_times$stop_id %in% stops_in_stations$stop_id), ]
    trips_SS <- values$trips[values$trips$trip_id %in% stop_times_SS$trip_id, ]
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO <- values$routeData_SA_SS <- values$routeData_SA <- values$routeData[values$routeData$route_id %in% trips_SS$route_id, ]
  } else {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO <- values$routeData_SA_SS <- values$routeData_SA <- values$routeData
  }
  
  # update origin zones list
  oZoneList <- unique(as.character(values$routeData_FR$zone_o))
  updateSelectInput(session, "originZone",
                    choices = oZoneList)
  
  # update destination zones list
  dZoneList <- unique(as.character(values$routeData_FR$zone_d))
  updateSelectInput(session, "destinationZone",
                    choices = dZoneList)
  
  # update operators list
  operatorList <- unique(as.character(values$routeData_FR$operator))
  updateSelectInput(session, "operator",
                    choices = operatorList)
})

observeEvent(input$originZone, ignoreInit = TRUE, {
  # origin zones
  if (!is.null(input$originZone)) {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO <- values$routeData_SA_SS <- values$routeData_SA[values$routeData_SA$zone_o %in% input$originZone, ]
  } else {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO <- values$routeData_SA_SS <- values$routeData_SA
  }
  # update destination zones list
  dZoneList <- unique(as.character(values$routeData_FR$zone_d))
  updateSelectInput(session, "destinationZone",
                    choices = dZoneList)

  # update operators list
  operatorList <- unique(as.character(values$routeData_FR$operator))
  updateSelectInput(session, "operator",
                    choices = operatorList)
})

observeEvent(input$destinationZone, ignoreInit = TRUE, {
  # destination zones
  if (!is.null(input$destinationZone)) {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO[values$routeData_SA_SS_SO$zone_d %in% input$destinationZone, ]
  } else {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD <- values$routeData_SA_SS_SO
  }
  # update operators list
  operatorList <- unique(as.character(values$routeData_FR$operator))
  updateSelectInput(session, "operator",
                    choices = operatorList)
})

observeEvent(input$operator, ignoreInit = TRUE, {
  # operators
  if (!is.null(input$operator)) {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD[values$routeData_SA_SS_SO_SD$operator %in% input$operator, ]
  } else {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp <- values$routeData_SA_SS_SO_SD
  }
})

observeEvent(input$service_id, ignoreInit = TRUE, {
  # service id
  if (!is.null(input$service_id)) {
    routeList_DW <- values$trips[values$trips$service_id %in% input$service_id, ]
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp[values$routeData_SA_SS_SO_SD_SOp$route_id %in% routeList_DW$route_id, ]
  } else {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW <- values$routeData_SA_SS_SO_SD_SOp
  }
})

observeEvent(c(input$indicator, input$indicatorPercentiles), ignoreInit = TRUE, {
  # indicator
  if (!is.null(input$indicator) && input$indicator != "route_color") {

    # percentiles
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW[order(values$routeData_SA_SS_SO_SD_SOp_SDW[[input$indicator]]),]
    lowerBound <- round(input$indicatorPercentiles[1] / 100 * nrow(values$routeData_FR))
    upperBound <- round(input$indicatorPercentiles[2] / 100 * nrow(values$routeData_FR))
    values$routeData_FR <- values$routeData_FR[lowerBound:upperBound,]
  } else {
    values$routeData_FR <- values$routeData_SA_SS_SO_SD_SOp_SDW
  }
})

observe({
  
  if (!is.null(values$routeData_FR) && nrow(values$routeData_FR) > 0) {
    # GeoJSON route list
    geojsonRouteList <- as.list(as.character(values$routeData_FR$route_id))
    names(geojsonRouteList) <- as.character(paste(values$routeData_FR$route_short_name, " | ", values$routeData_FR$route_id))
    geojsonRouteList <- geojsonRouteList[order(values$routeData_FR$route_short_name)]
    geojsonRouteList <- geojsonRouteList[!duplicated(geojsonRouteList)]
    updateSelectInput(session, "selectGeoJSONroute",
                      choices = geojsonRouteList)
  }
})


### Make plots
# FIX add condition: if relevant plot window is open
#observeEvent(c(input$selectStation, input$map_shape_click, input$timeSlider), {
observe({  
  # Saturation Bullet Graph
  #output$stnSatRadar <- stnSatRadar <- plotRadar(input$selectStation, values$stn_SS_SH)
  if (!is.null(values$stn_SS_AH) && nrow(values$stn_SS_AH) > 0) {
    output$stnSatBG <- values$stnSatBG <- makeBulletGraph(values$stn_SS_AH, values$stn_SS_SH, values$stn_name, satBins, satPal)
  }
  # Station Saturation Timeline
  if (!is.null(values$stn_SS_AH) && nrow(values$stn_SS_AH) > 0) {
    output$stnSatTimeline <- values$stnSatTimeline <- makeStnSatTimeline(values$stn_SS_AH, input$colorVar)
  }
  # Route segment saturation profile
  if (!is.null(values$seg_SR_SH) && nrow(values$seg_SR_SH) > 0) {
    output$segSatProfile <- values$segSatProfile <- makeSegSatProfile(values$seg_SR_SH)
  }
  # Boarding/alighting bar chart
  if (!is.null(values$onOff_SS_AR_SH) && nrow(values$onOff_SS_AR_SH) > 0) { 
    output$boardingSatBC <- values$boardingSatBC <- makeBoardingSatBC(values$onOff_SS_AR_SH, values$stn_name)
  }
  # Corridor segment saturation profile
  if (!is.null(values$corridorSeg_SZ_SH) && nrow(values$corridorSeg_SZ_SH) > 0) { 
    output$corridorSegSatProfile <- values$corridorSegSatProfile <- makeCorridorSegSatProfile(values$corridorSeg_SZ_SH)
  }
  # Recommendations
  # Print recommendations FIX make responsive to saturation results. Store all recommendations in vector of strings.
  # FIX allow user to add an improvement, checkboxes to specify cases when it is appropriate. Alternatively, show editable table of improvements.
  # FIX alternative method of choosing recs: list recs for 3 sat types. for each rec, also identify which sat it can worsen. Display separate list of follow-up recs.
  if (!is.null(values$stn_SS_AH) && nrow(values$stn_SS_AH) > 0) { 
    output$recommendations <- values$recommendations <- makeRecsTable(values$stn_SS_AH)
  }
})
  
### Map 
  
# Initial map

# Themes
satBins <- c(0, 0.5, 0.75, 1, 1.25, 1.5, 2, 4)
satPal <- rev(brewer.pal(11,"RdYlGn"))[c(1, 3, 5, 8, 9, 10, 11)]

output$map <- makeInitialMap()

# Manage layers according to StudyElement
observeEvent(c(input$studyElement, input$displayStationSpider, input$displayTransitDemand), {
  manageLayers(input)
})

# Show pop-up
observeEvent(input$selectStation, {
  if (!is.null(values$stn_SS_SH) && nrow(values$stn_SS_SH) > 0) { 
    showStationPopup(values$stn_SS_SH, input$colorVar)
  }
})

# When map is clicked, update station input
observeEvent(input$map_shape_click, {
  event <- input$map_shape_click
  updateSelectizeInput(session, "stationToDraw", selected = event$id)
  updateSelectizeInput(session, "selectStation", selected = event$id)
  updateSelectInput(session, "selectStopsAt", selected = event$id)
})
  
# Show station name when hovering

# observeEvent(input$map_shape_mouseover, {
# 
#   event <- input$map_shape_mouseover
# 
#   stn_name <- values$stationInventory$station_name[values$stationInventory$station_id == event$id]
# 
#   if (!is.null(stn_name)) {
#     leafletProxy("map") %>%
#       clearGroup("Station Labels") %>%
#       addPopups(event$lng, event$lat,
#                 stn_name, layerId = event$id, group = "Station Labels")
#   }
# })

# Station Circles
#observeEvent(c(input$timeSlider, input$colorVar, input$sizeVar), ignoreInit = TRUE, {
observe({
  if (!is.null(values$stn_AS_SH) && nrow(values$stn_AS_SH) > 0) { 
    makeStationCircles(values$stn_AS_SH, values$stn_AS_AH, input$colorVar, input$sizeVar, satBins, satPal)
  }
})

# Route Segments
observe({
  if (!is.null(values$seg_SR_SH) && nrow(values$seg_SR_SH) > 0) {
    mapSegSatPath(values$seg_SR_SH, values$seg_AR_AH, input$sizeVar, satBins, satPal)
  }
})

# GeoJSON Routes

observeEvent(input$clearGeoJSONroutes, {
  
  leafletProxy("map") %>%
    clearGroup(c("Routes", "Stops"))
  
  updateSelectInput(session, "selectGeoJSONroute",
                    selected = NULL)
})
  
observeEvent(input$displayGeoJSONroutes, {
  
  leafletProxy("map") %>%
    clearGroup("Routes")

  values$GeoJSONroutes_FR <- values$GeoJSONroutes[values$GeoJSONroutes$route_id %in% values$routeData_FR$route_id, ]

  if (length(input$selectGeoJSONroute) > 0) {
    
    values$GeoJSONroutes_SR <- values$GeoJSONroutes_FR[values$GeoJSONroutes_FR$route_id %in% input$selectGeoJSONroute, ]
    values$routeData_SR <- values$routeData_FR[values$routeData_FR$route_id %in% input$selectGeoJSONroute, ]
    
    # Filter stop_times
    trips_SR <- values$trips[values$trips$route_id %in% values$routeData_SR$route_id, ]
    stop_times_SR <- values$stop_times[values$stop_times$trip_id %in% trips_SR$trip_id, ]

    # Map routes
    if (!is.null(values$GeoJSONroutes_SR) && nrow(values$GeoJSONroutes_SR) > 0) {
      mapGeoJSONroutes(values$GeoJSONroutes_SR, values$routeData_SR, input$indicator)
    }
  } else {
    
    # Filter stop_times
    trips_FR <- values$trips[values$trips$route_id %in% values$routeData_FR$route_id, ]
    stop_times_SR <- values$stop_times[values$stop_times$trip_id %in% trips_FR$trip_id, ]
    
    # Map routes
    if (!is.null(values$GeoJSONroutes_FR) && nrow(values$GeoJSONroutes_FR) > 0) {
      mapGeoJSONroutes(values$GeoJSONroutes_FR, values$routeData_FR, input$indicator)
    }
  }
  
  # Map routeStops
  if (input$routeStops == 'Stops') {
    values$GeoJSONstops_SR <- values$GeoJSONstops[values$GeoJSONstops$stop_id %in% stop_times_SR$stop_id, ]
  } else if (input$routeStops == 'Stations') {
    stops_SR <- values$stops[values$stops$stop_id %in% stop_times_SR$stop_id, ]
    values$GeoJSONstops_SR <- values$GeoJSONstops[values$GeoJSONstops$stop_id %in% stops_SR$parent_station, ]
  } else if (input$routeStops == 'None') {
    values$GeoJSONstops_SR <- NULL
}

  if (!is.null(values$GeoJSONstops_SR) && nrow(values$GeoJSONstops_SR) > 0) {
    mapGeoJSONstops(values$GeoJSONstops_SR)
  } else {
    leafletProxy("map") %>% 
      clearGroup("Stops")
  }
})

# GeoJSON Stops
observeEvent(input$selectGeoJSONstop, {
  leafletProxy("map") %>%
    clearGroup("Stops")
  
  values$GeoJSONstops_SS <- values$GeoJSONstops[values$GeoJSONstops$stop_id %in% input$selectGeoJSONstop, ]
  
  if (!is.null(values$GeoJSONstops_SS)) {
    mapGeoJSONstops(values$GeoJSONstops_SS)
  }
})

observeEvent(input$selectGeoJSONstopType, {
  
  leafletProxy("map") %>%
    clearGroup("Stops")
  
  if ("station" %in% input$selectGeoJSONstopType) {
    mapGeoJSONstops(values$GeoJSONstops_stations)
  }
  if ("platform" %in% input$selectGeoJSONstopType) {
    mapGeoJSONstops(values$GeoJSONstops_platforms)
  }
  if ("stop" %in% input$selectGeoJSONstopType) {
    mapGeoJSONstops(values$GeoJSONstops_stops)
  }
  
})

# Corridors
#observeEvent(c(input$selectCorridorsForSeg, input$timeSlider), ignoreInit = TRUE, {
observe({
  if (!is.null(values$corridorSeg_AZ_SH) && nrow(values$corridorSeg_AZ_SH) > 0) { 
    mapCorridorSegSatPath(values$corridorSeg_AZ_SH, values$corridorSeg_AZ_AH, input$sizeVar, satBins, satPal)
  }
})

# FIX consider doing this in sub table update chunk
# FIX also try not requiring action button
observeEvent(input$displayStationSpider, {

  leafletProxy("map") %>%
    clearGroup(group = "O-D Pairs")
  
  odm_AZ_AS_SH <- values$odm_AZ_AS_AH_10[values$odm_AZ_AS_AH_10$hr == input$timeSlider,] 
  
  if(input$attractions){
    odm_SS_SH <- odm_AZ_AS_SH[odm_AZ_AS_SH$D == input$selectStation,]
  } else {
    odm_SS_SH <- odm_AZ_AS_SH[odm_AZ_AS_SH$O == input$selectStation,]
  }
  
  odm_SS_SH$DL_var <-  odm_SS_SH$transit_trips
  
  if (!is.null(odm_SS_SH) && nrow(odm_SS_SH) > 0) { 
    makeSpider(odm_SS_SH, input$inputSpiderPercentile)

    ## Arc Diagram
    odm_temp <- odm_SS_SH[c("O", "D", "DL_var")]
    values$odArcDiagram <- makeArcDiagram(odm_temp, values$stationInventory)
  }
})

observeEvent(input$displayStationDemandDistributionCircles, {
  
  odm_AZ_AS_SH <- values$odm_AZ_AS_AH_10[values$odm_AZ_AS_AH_10$hr == input$timeSlider,] 
  
  if (input$attractions) {
    odm_SS_SH <- odm_AZ_AS_SH[odm_AZ_AS_SH$D == input$selectStation,]
  } else {
    odm_SS_SH <- odm_AZ_AS_SH[odm_AZ_AS_SH$O == input$selectStation,]
  }
  
  odm_SS_SH$DL_var <-  odm_SS_SH$transit_trips
  
  if (!is.null(odm_SS_SH) && nrow(odm_SS_SH) > 0) {
    if (input$attractions) {
      stn_heat <- merge(values$stationInventory, odm_SS_SH, by.x = "station_id", by.y = "O", all.x = TRUE)
    } else {
      stn_heat <- merge(values$stationInventory, odm_SS_SH, by.x = "station_id", by.y = "D", all.x = TRUE)
    }
    sizeVar <- colorVar <- "transit_trips"
    satPal <- satBins <- NULL
    makeStationCircles(stn_heat, stn_heat, colorVar, sizeVar, NULL, NULL)

    ## Arc Diagram
    odm_temp <- odm_SS_SH[c("O", "D", "DL_var")]
    values$odArcDiagram <- makeArcDiagram(odm_temp, values$stationInventory)
  }
})

observeEvent(input$clearDesireLines, {
  leafletProxy("map") %>%
    clearGroup(group = "O-D Pairs")
})

observeEvent(input$displayDesireLines, {
  
  leafletProxy("map") %>%
    clearGroup(group = "O-D Pairs")

  odm_AZ_SH <- values$odm_AZ_AS_AH_10[values$odm_AZ_AS_AH_10$hr == input$timeSlider, ]
  
  if(!is.null(input$selectCorridorsForDL)){
    
    numCorridors <- length(input$selectCorridorsForDL)
    
    if (input$unidirectional) {
      if (numCorridors == 1) {
        odm_SZ_SH <- odm_AZ_SH[odm_AZ_SH$corridorO == input$selectCorridorsForDL[1], ]
      } else {
        odm_SZ_SH <- odm_AZ_SH[0, ]
        for (i in 1:(numCorridors - 1)) {
          odm_SZ_SH_temp <- odm_AZ_SH[odm_AZ_SH$corridorO == input$selectCorridorsForDL[i], ]
          odm_SZ_SH_temp <- odm_SZ_SH_temp[odm_SZ_SH_temp$corridorD == input$selectCorridorsForDL[i + 1], ]
          odm_SZ_SH <- rbind(odm_SZ_SH, odm_SZ_SH_temp)
        }
      }
    } else {
      if (numCorridors == 1) {
        odm_SZ_SH <- odm_AZ_SH[(odm_AZ_SH$corridorO == input$selectCorridorsForDL[1]) | (odm_AZ_SH$corridorD == input$selectCorridorsForDL[1]), ]
      } else {
        odm_SZ_SH <- odm_AZ_SH[0, ]
        for (j in 1:2) {
          if (j == 2) {
            selectedCorridors <- rev(input$selectCorridorsForDL)
          } else {
            selectedCorridors <- input$selectCorridorsForDL
          }
          for (i in 1:(numCorridors - 1)) {
            odm_SZ_SH_temp <- odm_AZ_SH[odm_AZ_SH$corridorO == selectedCorridors[i], ]
            odm_SZ_SH_temp <- odm_SZ_SH_temp[odm_SZ_SH_temp$corridorD == selectedCorridors[i + 1], ]
            odm_SZ_SH <- rbind(odm_SZ_SH, odm_SZ_SH_temp)
          }
        }
        
      }
    }
    
  } else {
    odm_SZ_SH <- odm_AZ_SH
  }
  
  if (input$includeIntraZonal == FALSE) {
    odm_SZ_SH <- odm_SZ_SH[(odm_SZ_SH$corridorO != odm_SZ_SH$corridorD), ]
  }
  
  if(input$selectDLvar == "transit_trips") {
    odm_SZ_SH$DL_var <-  odm_SZ_SH$transit_trips
  } else if (input$selectDLvar == "auto_trips") {
    odm_SZ_SH$DL_var <-  odm_SZ_SH$auto_trips
  } else if (input$selectDLvar == "TT_ratio") {
    odm_SZ_SH$DL_var <-  odm_SZ_SH$TT_ratio
  } else if (input$selectDLvar == "TT_lost") {
    odm_SZ_SH$DL_var <-  odm_SZ_SH$TT_lost
  }
  
  odm_SZ_SH[is.na(odm_SZ_SH)] <- 0
  odm_SZ_SH <- odm_SZ_SH[odm_SZ_SH$DL_var != 0, ]
  odm_SZ_SH <- odm_SZ_SH[order(odm_SZ_SH$DL_var, decreasing = TRUE), ]
  
  if (!is.null(odm_SZ_SH) && nrow(odm_SZ_SH) > 0) { 
    # Desire Lines
    makeDesireLines(odm_SZ_SH, input$desireLinePercentiles, input$desireLineColor)
    
    # Arc Diagram
    odm_temp <- odm_SZ_SH[c("O", "D", "DL_var")]
    odArcDiagram <- makeArcDiagram(odm_temp, values$stationInventory)
  }
})
  
## Plots tab
observe({

  if(input$nav == "plots"){
    if(input$selectPlot == "stnSatBG" && !is.null(values$stnSatBG)){
      output$bigPlot <- values$stnSatBG
    } else if(input$selectPlot == "stnSatTimeline" && !is.null(values$stnSatTimeline)){
      output$bigPlot <- values$stnSatTimeline
    } else if(input$selectPlot == "segSatProfile" && !is.null(values$segSatProfile)){
      output$bigPlot <- values$segSatProfile
    } else if(input$selectPlot == "corridorSegSatProfile" && !is.null(values$corridorSegSatProfile)){
      output$bigPlot <- values$corridorSegSatProfile
    } else if(input$selectPlot == "boardingSatBC" && !is.null(values$boardingSatBC)){
      output$bigPlot <- values$boardingSatBC
    } else if(input$selectPlot == "odArcDiagram" && !is.null(values$odArcDiagram)){
      output$bigPlot <- values$odArcDiagram
    } else {
      return()
    }
  }
})
   
## Station Drawings
observe({
  
  if(input$nav == "stationDesign"){
    srcDrawing <- paste0("stationDrawings", pathSlash, input$stationToDraw, input$drawingFile, ".pdf#view=FitH")
    output$stationDrawing <- renderUI(
     tags$iframe(height="650", width="100%", scrolling = "no",
                 seamless = "seamless", src=srcDrawing)
    )
  }
})
}
})
})