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
library(rhandsontable)
library(stringr)
library(jsonlite)
library(ggplot2)
library(devtools)
library(arcdiagram)
library(reshape2)
library(gridExtra)
library(data.table)

# Initial map
makeInitialMap <- function() {
  
  renderLeaflet({
    leaflet() %>%
      setView(lng = -74.12, #if(!is.null(input$processRaw)){mean(stops$stop_lon)} else {-74.05},
              lat = 4.65, #if(!is.null(input$processRaw)){mean(stops$stop_lat)} else {4.65},
              zoom = 12) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Base Map"),
        overlayGroups = c("Pop-ups", "Routes", "Stops", "Stations", "Route Segments", "Corridors", "O-D Pairs", "Drawn"),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)
      ) %>%
      hideGroup(c("Route Segments", "Corridors", "O-D Pairs", "Routes", "Stops")) %>%
      addDrawToolbar(
        targetGroup='Drawn',
        position = "bottomleft",
        polylineOptions = drawPolylineOptions(shapeOptions = list(weight = 8, opacity = 0.8, clickable = TRUE), repeatMode = TRUE),
        markerOptions = drawMarkerOptions(markerIcon = makeAwesomeIcon(icon = "fa-bus", library = "fa"), repeatMode = TRUE),
        circleOptions = drawCircleOptions(showRadius = TRUE, shapeOptions = list(stroke = FALSE), repeatMode = TRUE),
        polygonOptions = FALSE,
        rectangleOptions = FALSE,
        editOptions = editToolbarOptions(edit = TRUE, remove = TRUE, allowIntersection = TRUE))  %>%
      addStyleEditor(position = "bottomleft", openOnLeafletDraw = FALSE)
  })
  
}

# Station pop-up
showStationPopup <- function(stn_SS_SH, colorVar) {

  leafletProxy("map") %>% 
    clearPopups()
  
  stn_id <- stn_SS_SH$station_id
  lat <- stn_SS_SH$station_lat
  lng <- stn_SS_SH$station_lon
  
  if(colorVar == "PB"){
    content <- as.character(tagList(
      tags$h4(stn_SS_SH$station_name),
      tags$strong(HTML(sprintf("%s", "Pax in Bays"))), tags$br(),
      sprintf("Demand: %s", stn_SS_SH$PB_demand), tags$br(),
      sprintf("Capacity: %s", stn_SS_SH$PB_capacity), tags$br(),
      sprintf("Saturation: %s", percent(stn_SS_SH$PB_saturation)), tags$br()
    ))
  }
  if(colorVar == "BS"){
    content <- as.character(tagList(
      tags$h4(stn_SS_SH$station_name),
      tags$strong(HTML(sprintf("%s", "Buses in Station"))), tags$br(),
      sprintf("Demand: %s", stn_SS_SH$BS_demand), tags$br(),
      sprintf("Capacity: %s", stn_SS_SH$BS_capacity), tags$br(),
      sprintf("Saturation: %s", percent(stn_SS_SH$BS_saturation)), tags$br()
    ))
  }
  if(colorVar == "PT"){
    content <- as.character(tagList(
      tags$h4(stn_SS_SH$station_name),
      tags$strong(HTML(sprintf("%s", "Pax in Turnstiles"))), tags$br(),
      sprintf("Demand: %s", stn_SS_SH$PT_demand), tags$br(),
      sprintf("Capacity: %s", stn_SS_SH$PT_capacity), tags$br(),
      sprintf("Saturation: %s", percent(stn_SS_SH$PT_saturation)), tags$br()
    ))
  }
  
  leafletProxy("map") %>% 
    addPopups(lng, lat, content, layerId = stn_id, group = "Pop-ups")

}

# Manage map layers
manageLayers <- function(input) {
  
  if (input$studyElement == "stations") {
    leafletProxy("map") %>% 
      showGroup(c("Stations", "Pop-ups")) %>% 
      hideGroup(c("Route Segments", "Corridors", "Stops"))
  } else if (input$studyElement == "routeSeg") {
    leafletProxy("map") %>% 
      showGroup(c("Route Segments", "Stops")) %>% 
      hideGroup(c("Pop-ups", "Stations", "Corridors", "Routes"))
  } else if (input$studyElement == "corridors") {
    leafletProxy("map") %>% 
      showGroup("Corridors") %>% 
      hideGroup(c("Pop-ups", "Stations", "Route Segments", "Routes", "Stops"))
  } else if (input$studyElement == "odPairs") {
    leafletProxy("map") %>% 
      showGroup(c("O-D Pairs", "Stations"))  %>% 
      hideGroup(c("Pop-ups", "Stops"))
  } else if (input$studyElement == "routes") {
    leafletProxy("map") %>%
      showGroup(c("Routes", "Stops")) %>%
      hideGroup(c("Route Segments", "Pop-ups", "Corridors", "Stations"))
  } else if (input$studyElement == "stops") {
    leafletProxy("map") %>%
      showGroup("Stops") %>%
      hideGroup(c("Stations", "Route Segments", "Pop-ups", "Corridors"))
  }
  
  if (input$displayStationSpider) {
    leafletProxy("map") %>% 
      showGroup("O-D Pairs")
  }
}

# Station circles
makeStationCircles <- function(stn_AS_SH, stn_AS_AH, colorVar, sizeVar, satBins, satPal) {
  
  if (sizeVar == "transit_trips") {
    maxStnSizeVar <- max(stn_AS_AH[["transit_trips"]],1, na.rm = TRUE)
    radius <- stn_AS_SH[["transit_trips"]] / maxStnSizeVar * 500
    leafletProxy("map", data = stn_AS_SH) %>%
      clearGroup(group = "Stations") %>%
      addCircles(~station_lon, ~station_lat, radius=radius, layerId=~station_id,
                 stroke=FALSE, fillOpacity=0.6, #popup = ~station_name, 
                 fillColor="purple", group = "Stations") # %>% 
    # addLegend("topleft", pal= pal, values = colorData, title = paste0("Stations: ", colorVar),
    #           layerId = "stationLegend")
  } else {
    
    if (colorVar == "TT_avg") {
      colorData <- stn_AS_SH[["production_avg_transit_TT"]]
      pal <- colorNumeric(satPal, colorData, na.color = "#808080")
      
      maxStnSizeVar <- max(stn_AS_AH[["production_pop_transit_TT"]],1, na.rm = TRUE)
      radius <- stn_AS_SH[["production_pop_transit_TT"]] / maxStnSizeVar * 500
    } else {
      colorData <- stn_AS_SH[[paste0(colorVar,"_","saturation")]]
      pal <- colorBin(satPal, colorData, na.color = "#808080", bins = satBins, pretty = TRUE)
      
      maxStnSizeVar <- max(stn_AS_AH[[paste0(colorVar,"_",sizeVar)]],1, na.rm = TRUE)
      radius <- stn_AS_SH[[paste0(colorVar,"_",sizeVar)]] / maxStnSizeVar * 500
    }
  
    leafletProxy("map", data = stn_AS_SH) %>%
      clearGroup(group = "Stations") %>%
      addCircles(~station_lon, ~station_lat, radius=radius, layerId=~station_id,
                 stroke=FALSE, fillOpacity=0.6, #popup = ~station_name, 
                 fillColor=pal(colorData), group = "Stations") %>% 
      addLegend("topleft", pal= pal, values = colorData, title = paste0("Stations: ", colorVar),
                layerId = "stationLegend")
  }
}

# Station spider
makeSpider <- function(odm_SS_SH, percentiles) {

  odm_SS_SH <- odm_SS_SH[order(odm_SS_SH$transit_trips, decreasing = TRUE), ]
  
  lowerBound <- round(percentiles[1] / 100 * nrow(odm_SS_SH))
  upperBound <- round(percentiles[2] / 100 * nrow(odm_SS_SH))
  maxTrips <- max(odm_SS_SH$transit_trips[lowerBound:upperBound], na.rm = TRUE)
  minTrips <- min(odm_SS_SH$transit_trips[lowerBound:upperBound], na.rm = TRUE)
  
  for (i in lowerBound:upperBound) {
    
    line <- odm_SS_SH[i, ]
    #pal <- colorBin(satPal, segment$seg_saturation, na.color = "#808080", bins = satBins, pretty = TRUE)
    lineColor <- "blue" #pal(line$odm_saturation)
    lineWeight <- 3 #(line$transit_trips - minTrips) / (maxTrips - minTrips) * 12 + 2
    lineOpacity <- (line$transit_trips - minTrips) / (maxTrips - minTrips)
    
    leafletProxy("map", data = line) %>%
      addPolylines(c(line$lonO, line$lonD), 
                   c(line$latO, line$latD), 
                   layerId = line$pair_id,
                   color = lineColor, opacity = lineOpacity, weight = lineWeight, 
                   group = "O-D Pairs")
    
  }
}

# O-D Pairs
makeDesireLines <- function(odm_temp, percentiles, desireLineColor) {

  lowerBound <- round(percentiles[1] / 100 * nrow(odm_temp))
  upperBound <- round(percentiles[2] / 100 * nrow(odm_temp))
  
  
  lineWeight <- 3
  lineColor <- desireLineColor
  
  maxDL_var <- max(odm_temp$DL_var[lowerBound:upperBound], na.rm = TRUE)
  minDL_var <- min(odm_temp$DL_var[lowerBound:upperBound], na.rm = TRUE)
  
  for (i in lowerBound:upperBound) {
    
    line <- odm_temp[i,]
    lineOpacity <- (line$DL_var - minDL_var) / (maxDL_var - minDL_var)
    
    if (i == 1) {
      leafletProxy("map", data = line) %>%
        addPolylines(c(line$lonO_offset, line$lonD_offset), 
                     c(line$latO_offset, line$latD_offset), 
                     layerId = line$pair_id,
                     color = lineColor, opacity = lineOpacity, weight = lineWeight, 
                     group = "O-D Pairs") %>%
        addLegend(position = "topleft",
                  colors = c("white", lineColor),
                  labels = c(round(minDL_var), round(maxDL_var)),
                  title = paste0("O-D Pairs"),
                  layerId = "desireLineLegend")
    } else {
      leafletProxy("map", data = line) %>%
        addPolylines(c(line$lonO_offset, line$lonD_offset), 
                     c(line$latO_offset, line$latD_offset), 
                     layerId = line$pair_id,
                     color = lineColor, opacity = lineOpacity, weight = lineWeight, 
                     group = "O-D Pairs")
    }
    
  }
}

# Route segments
mapSegSatPath <- function(seg_SR_SH, seg_AR_AH, sizeVar, satBins, satPal) {

  maxSegSizeVar <- max(seg_AR_AH[[paste0("seg_",sizeVar)]],1, na.rm = TRUE)
  numSegments <- nrow(seg_SR_SH)

  for (i in 1:numSegments) {

    segment <- seg_SR_SH[i,]
    pal <- colorBin(satPal, segment$seg_saturation, na.color = "#808080", bins = satBins, pretty = TRUE)
    segColor <- pal(segment$seg_saturation)
    segWeight <- segment$seg_demand / maxSegSizeVar * 50

    if(i == 1){
      leafletProxy("map", data = segment) %>%
        clearGroup(group = "Route Segments") %>%
        addPolylines(c(segment$station_lonA, segment$station_lonB), 
                     c(segment$station_latA, segment$station_latB),
                     layerId = ~routeStnSeg_id,
                     color = segColor, opacity = 0.6, group = "Route Segments", weight = segWeight)
    } else {
      leafletProxy("map", data = segment) %>%
        addPolylines(c(segment$station_lonA, segment$station_lonB), 
                     c(segment$station_latA, segment$station_latB),
                     layerId = ~routeStnSeg_id,
                     color = segColor, opacity = 0.6, group = "Route Segments", weight = segWeight)
    }
  }
}

# GeoJSON routes
mapGeoJSONroutes <- function(GeoJSONroutes, routeData, indicator) {
  
  indicatorPal <- brewer.pal(11,"RdYlGn")

  if (indicator == "route_color") {
    
    pal <- as.character(paste0("#", GeoJSONroutes[["route_color"]]))
    
    leafletProxy("map", data = GeoJSONroutes) %>%
      addPolylines(layerId = ~route_id,
                   color = pal,
                   opacity = .6,
                   smoothFactor = 0.8,
                   group = "Routes",
                   weight = 5,
                   label = ~route_short_name)

  } else {
    
    numRoutes <- nrow(routeData)
    colorData <- routeData[[indicator]]
    pal <- colorNumeric(indicatorPal, colorData, na.color = "#808080")
    
    for (i in 1:numRoutes) {
      
      routeId <- routeData$route_id[i]
      routeColor <- pal(routeData[i, indicator])
      routeLabel <- paste0(sub('_', '', routeData$route_short_name[i]), " | ", signif(routeData[i, indicator], 3))
      GeoJSONroute <- GeoJSONroutes[GeoJSONroutes$route_id == routeId,]
      
      if (i == 1) {
        leafletProxy("map", data = GeoJSONroute) %>%
          addPolylines(layerId = routeId,
                       color = routeColor,
                       opacity = .6,
                       smoothFactor = 0.8,
                       group = "Routes",
                       weight = 5,
                       label = routeLabel) %>%
          addLegend(position = "topleft", 
                    pal = pal, 
                    values = colorData, 
                    na.label = "Data unavailable",
                    bins = 7,
                    opacity = 0.6,
                    title = paste0("Routes: ", indicator),
                    layerId = "routeLegend")
      } else {
        leafletProxy("map", data = GeoJSONroute) %>%
          addPolylines(layerId = routeId,
                       color = routeColor,
                       opacity = .6,
                       smoothFactor = 0.8,
                       group = "Routes",
                       weight = 5,
                       label = routeLabel)
      }
    }
  }
}

# GeoJSON stops
mapGeoJSONstops <- function(GeoJSONstops_SS) {
  
  leafletProxy("map", data = GeoJSONstops_SS) %>%
    clearGroup(group = "Stops") %>%
    addCircleMarkers(layerId = ~stop_id,
                 group = "Stops",
                 radius = ~ifelse((location_type == 1), 6, 4),
                 stroke = TRUE,
                 weight = 1,
                 color = "black", #~ifelse((location_type == 1), "red", ifelse(!is.na(parent_station), "black", "gray")),
                 opacity = 1,
                 fillColor = "white",
                 fillOpacity = 1,
                 label = ~stop_name)
}

# Corridor segments
mapCorridorSegSatPath <- function(corridorSeg_AZ_SH, corridorSeg_AZ_AH, sizeVar, satBins, satPal) {
  
  maxCorridorSegSizeVar <- max(corridorSeg_AZ_AH[[paste0("corridorSeg_",sizeVar)]],1, na.rm = TRUE)
  numSegments <- nrow(corridorSeg_AZ_SH)
  
  for (i in 1:numSegments) {
    
    segment <- corridorSeg_AZ_SH[i,]
    pal <- colorBin(satPal, segment$corridorSeg_saturation, na.color = "#808080", bins = satBins, pretty = TRUE)
    segColor <- pal(segment$corridorSeg_saturation)
    segWeight <- segment$corridorSeg_demand / maxCorridorSegSizeVar * 15
    
    if(i == 1){
      leafletProxy("map", data = segment) %>%
        clearGroup(group = "Corridors") %>%
        addPolylines(c(segment$station_lonA_offset, segment$station_lonB_offset), 
                     c(segment$station_latA_offset, segment$station_latB_offset), 
                     layerId = ~corridorSeg_id,
                     color = segColor, opacity = 0.6, group = "Corridors", weight = segWeight)
    } else {
      leafletProxy("map", data = segment) %>%
        addPolylines(c(segment$station_lonA_offset, segment$station_lonB_offset), 
                     c(segment$station_latA_offset, segment$station_latB_offset), 
                     layerId = ~corridorSeg_id,
                     color = segColor, opacity = 0.6, group = "Corridors", weight = segWeight)
    }
  }
}
