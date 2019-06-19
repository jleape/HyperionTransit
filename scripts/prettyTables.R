library(shiny)
library(DT)
library(magrittr)
library(leaflet)
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

# Pop-up
make_popUp <- function(station_id, stn_SS_SH) {
  
  # Read-only table
  
  if (is.null(stn_SS_SH)) {
    return(NULL)
  } else {
    demand <- c(stn_SS_SH$PB_demand, stn_SS_SH$BS_demand, stn_SS_SH$PT_demand)
    capacity <- c(stn_SS_SH$PB_capacity, stn_SS_SH$BS_capacity, stn_SS_SH$PT_capacity)
    saturation <- c(percent(stn_SS_SH$PB_saturation),
                    percent(stn_SS_SH$BS_saturation),
                    percent(stn_SS_SH$PT_saturation))
    stn_SS_SHDF <- rbind(demand,capacity,saturation)
    rownames(stn_SS_SHDF) <- c("Demand", "Capacity", "Saturation")
    colnames(stn_SS_SHDF) <- c("Pax in Bays", "Buses in Station", "Pax in Turnstiles")
    renderTable(stn_SS_SHDF)
  }
}

make_editable_popUp <- function(station_id, stn_SS_SH) {
  
  output$stn_SS_SHTable <- renderRHandsontable({

    if (is.null(stn_SS_SH)) {
      return(NULL)
    } else {
      demand <- c(stn_SS_SH$PB_demand, stn_SS_SH$BS_demand, stn_SS_SH$PT_demand)
      capacity <- c(stn_SS_SH$PB_capacity, stn_SS_SH$BS_capacity, stn_SS_SH$PT_capacity)
      saturation <- c(percent(stn_SS_SH$PB_saturation),
                      percent(stn_SS_SH$BS_saturation),
                      percent(stn_SS_SH$PT_saturation))
      stn_SS_SH_DF <- rbind(demand,capacity,saturation)
      rownames(stn_SS_SH_DF) <- c("Demand", "Capacity", "Saturation")
      colnames(stn_SS_SH_DF) <- c("Pax in Bays", "Buses in Station", "Pax in Turnstiles")
    }

    rhandsontable(stn_SS_SH_DF, height = 200, rowHeaderWidth = 100) %>%
      hot_cols(colWidths = 72)
  })
}

# Recommendations
makeRecsTable <- function(stn_SS_AH) {

  renderRHandsontable({
    recDF <- rbind.data.frame(
      c("Add turnstiles", c(0,0,1,0,1,1,1,0), 40, 0, 900),
      c("Add entrance, bridge and turnstiles", c(0,0,1,0,1,1,1,0), 1000, 0, 3600),
      c("Increase service frequency", c(1,0,0,1,0,1,1,0), 120, 0, 15),
      c("Rebuild megastation with staggered platforms", c(1,0,0,1,1,0,1,0), 13800, 0, 40000),
      c("Reduce service frequency", c(0,1,0,0,1,0,1,0), -120, 15, 0),
      c("Add platform and realign lanes", c(0,1,0,1,1,1,1,0), 3324, 0, 10000),
      c("Adapt platform for biarticulated buses", c(0,1,0,1,1,1,1,0), 100, 0, 400),
      c("Extend platform for biarticulated buses or double bays", c(0,1,0,1,1,1,1,0), 700, 0, 400),
      c("Reduce dwell time", c(0,1,0,1,1,0,0,0), 0, 0, 10),
      c("Increase dwell time", c(1,0,0,0,0,1,0,0), 0, 0, 100),
      c("Separate access and egress", c(0,0,1,0,0,1,1,0), 0, 200, 0),
      c("Maintain or replace doors", c(1,1,0,0,0,1,1,0), 15, 0, 500),
      c("Extend passing lane", c(0,1,0,1,1,0,1,0), 1000, 0, 30),
      c("Increase distance between platforms", c(0,1,0,1,1,0,1,0), 2000, 0, 60),
      c("No upgrade required", c(0,0,0,0,0,0,0,1), 0, 0, 0),
      stringsAsFactors = FALSE
    )
    
    colnames(recDF) <- c("Improvement", "PB", "BS", "PT", "PB-BS", "BS-PT", 
                         "PT-PB", "PB-BS-PT", "unSat", "Cost", "∆Demand", "∆Capacity")
    
    # identify satCombo
    if(max(stn_SS_AH$PB_saturation) > 1){
      satCombo <- 1
      if(max(stn_SS_AH$BS_saturation) > 1){
        satCombo <- 4
        if(max(stn_SS_AH$PT_saturation) > 1){
          satCombo <- 7
        }
      } else {
        if(max(stn_SS_AH$PT_saturation) > 1){
          satCombo <- 6
        }
      }
    } else {
      if(max(stn_SS_AH$BS_saturation) > 1){
        satCombo <- 2
        if(max(stn_SS_AH$PT_saturation) > 1){
          satCombo <- 5
        }
      } else {
        if(max(stn_SS_AH$PT_saturation) > 1){
          satCombo <- 3
        } else {
          satCombo <- 7
        }
      }
    }
    
    displayRecsDF <- recDF[(recDF[, 1 + satCombo] == 1), ]
    displayRecsDF <- displayRecsDF[, c(1, 10, 11, 12)]
    displayRecsDF$Quantity <- 0
    
    rhandsontable(displayRecsDF, width = 370, rowHeaders = NULL) %>%
      hot_cols(colWidths = c(120, 50, 70, 70, 60))
    
  })
}

# Stations
make_stn_pretty <- function(stn_AS_AH) {
  
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'ID'),
        th(rowspan = 2, 'Name'),
        th(rowspan = 2, 'Corridor'),
        th(rowspan = 2, 'Hour'),
        th(colspan = 3, 'Passengers in Bays'),
        th(colspan = 3, 'Buses in Stations'),
        th(colspan = 3, 'Passengers in Turnstiles')
      ),
      tr(
        lapply(rep(c('Demand', 'Capacity', 'Saturation'), 3), th)
      )
    )
  ))
  print(sketch)
  
  stnSatTable <- stn_AS_AH[, c("station_id", "station_name", "corridor", "hr", 
                                "PB_demand", "PB_capacity", "PB_saturation", 
                                "BS_demand", "BS_capacity", "BS_saturation", 
                                "PT_demand", "PT_capacity", "PT_saturation")]
  
  stnSatTable$PB_demand <- round(stnSatTable$PB_demand,digits=0)
  stnSatTable$BS_demand <- round(stnSatTable$BS_demand,digits=0)
  stnSatTable$PT_demand <- round(stnSatTable$PT_demand,digits=0)
  
  stnSatTable$PB_saturation <- round(stnSatTable$PB_saturation,digits=2)
  stnSatTable$BS_saturation <- round(stnSatTable$BS_saturation,digits=2)
  stnSatTable$PT_saturation <- round(stnSatTable$PT_saturation,digits=2)
  
  # colnames(stnSatTable) <- c("ID", "Name", "Corridor", "Hour",
  #                              "Pax in Bays - Demand", "Pax in Bays - Capacity", "Pax in Bays - Saturation",
  #                              "Buses in Stations - Demand", "Buses in Stations - Capacity", "Buses in Stations - Saturation",
  #                              "Pax in Turnstiles - Demand", "Pax in Turnstiles - Capacity", "Pax in Turnstiles - Saturation")
  
  DT::renderDataTable(stnSatTable, 
                                            filter = 'top',
                                            container = sketch,
                                            rownames = FALSE,#,
                                            options = list(columnDefs = list(list(className = 'dt-center', targets = 0:12))))
}

# Routes
make_seg_pretty <- function(seg_AR_AH) {
  
  routeSegmentTable <- seg_AR_AH[c("route_id", "route_short_name", "hr", "station_nameA", "station_nameB", 
                                   "boardings", "alightings", "seg_demand", "seg_capacity", "seg_saturation")]
  routeSegmentTable[c("boardings", "alightings", "seg_demand", "seg_capacity")] <- 
    round(routeSegmentTable[c("boardings", "alightings", "seg_demand", "seg_capacity")])
  routeSegmentTable[c("seg_saturation")] <- 
    round(routeSegmentTable[c("seg_saturation")], digits = 2)
  colnames(routeSegmentTable) <- c("ID", "Route", "Hour", "Station A", "Station B", "Boardings", "Alightings", 
                                   "Demand", "Capacity", "Saturation")
  DT::renderDataTable(routeSegmentTable, 
                      filter = 'top',
                      rownames = FALSE)
  
}

# O-D Pairs
make_odm_10 <- function(odm_AZ_AS_AH, stationInventory) {
  
  # Prepare O-D Matrix for Desire Lines
  odm_AZ_AS_AH_10 <- odm_AZ_AS_AH[odm_AZ_AS_AH$transit_trips > 10, ]
  stationInventory2 <- stationInventory
  
  # Add station names, corridors and coordinates
  odm_AZ_AS_AH_10 <- sqldf("
    SELECT
    pair_id,
    O,
    D,
    stationInventory.station_name AS O_name,
    stationInventory2.station_name AS D_name,
    hr,
    transit_trips,
    auto_trips,
    TT_transit,
    TT_auto,
    TT_ratio,
    TT_lost,
    stationInventory.corridor AS corridorO,
    stationInventory.station_lat AS latO,
    stationInventory.station_lon AS lonO,
    stationInventory2.corridor AS corridorD,
    stationInventory2.station_lat AS latD,
    stationInventory2.station_lon AS lonD
    FROM
    odm_AZ_AS_AH_10,
    stationInventory,
    stationInventory2
    WHERE
    odm_AZ_AS_AH_10.O = stationInventory.station_id AND
    odm_AZ_AS_AH_10.D = stationInventory2.station_id;"
  )
  
  # Offset coordinates for drawing
  odm_AZ_AS_AH_10$theta <- atan2((odm_AZ_AS_AH_10$lonD - odm_AZ_AS_AH_10$lonO), 
                                        (odm_AZ_AS_AH_10$latD - odm_AZ_AS_AH_10$latO))
  
  odm_AZ_AS_AH_10$lonO_offset <- odm_AZ_AS_AH_10$lonO + .001*cos(odm_AZ_AS_AH_10$theta)
  odm_AZ_AS_AH_10$lonD_offset <- odm_AZ_AS_AH_10$lonD + .001*cos(odm_AZ_AS_AH_10$theta)
  
  odm_AZ_AS_AH_10$latO_offset <- odm_AZ_AS_AH_10$latO - .001*sin(odm_AZ_AS_AH_10$theta)
  odm_AZ_AS_AH_10$latD_offset <- odm_AZ_AS_AH_10$latD - .001*sin(odm_AZ_AS_AH_10$theta)
  
  return(odm_AZ_AS_AH_10)
}

make_odm_10_pretty <- function(odm_AZ_AS_AH_10) {
  
  odTable <- odm_AZ_AS_AH_10[c("O_name", "D_name", "corridorO", "corridorD", "hr", "transit_trips", "auto_trips", "TT_transit", "TT_auto", "TT_ratio", "TT_lost")]
  odTable$transit_trips <- round(odTable$transit_trips)
  odTable$auto_trips <- round(odTable$auto_trips)
  odTable$TT_transit <- round(odTable$TT_transit)
  odTable$TT_auto <- round(odTable$TT_auto)
  odTable$TT_ratio <- round(odTable$TT_ratio)
  odTable$TT_lost <- round(odTable$TT_lost)
  colnames(odTable) <- c("Origin", "Destination", "Origin Corridor", "Destination Corridor", "Hour", 
                         "Transit Trips", "Auto Trips", "Transit Travel Time", "Auto Travel Time",
                         "Travel Time Ratio", "Travel Time Lost")
  DT::renderDataTable(odTable,
                      filter = 'top',
                      rownames = FALSE)
}

# Corridors
make_corridorSeg_pretty <- function(corridorSeg_AZ_AH) {
  
  corridorSegmentTable <- corridorSeg_AZ_AH[c("station_nameA", "station_nameB", "corridorA", "corridorB", "hr",
                                                     "station_orderA", "station_orderB", "corridorSeg_demand", "corridorSeg_capacity", "corridorSeg_saturation")]
  corridorSegmentTable[c("corridorSeg_demand", "corridorSeg_capacity")] <- 
    round(corridorSegmentTable[c("corridorSeg_demand", "corridorSeg_capacity")])
  corridorSegmentTable[c("corridorSeg_saturation")] <- 
    round(corridorSegmentTable[c("corridorSeg_saturation")], digits = 2)
  colnames(corridorSegmentTable) <- c("Station A", "Station B", "Corridor A", "Corridor B", "Hour", "Station Order A", "Station Order B", 
                                      "Demand", "Capacity", "Saturation")
  DT::renderDataTable(corridorSegmentTable, filter = 'top', rownames = FALSE)
  
}