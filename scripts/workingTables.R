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

process_gtfs <- function (dayOfWeek, calendar, routes, trips, shapes, stops, stop_times) {
  
  # stops
  stops$stop_lat<-as.numeric(stops$stop_lat)
  stops$stop_lon<-as.numeric(stops$stop_lon)
  
  
  # Convert arrival and departure times to seconds
  to.Seconds <- function(date) {
    spl.Date <- str_split_fixed(date,":",3)
    return(as.numeric(spl.Date[,1])*60*60+as.numeric(spl.Date[,2])*60+as.numeric(spl.Date[,3]))
  }
  
  stop_times$arrival_in_seconds <-sapply(stop_times$arrival_time,to.Seconds,USE.NAMES=FALSE)
  stop_times$departure_in_seconds <-sapply(stop_times$departure_time,to.Seconds,USE.NAMES=FALSE)
  
  stop_times$arrival_hr <- floor(2*(stop_times$arrival_in_seconds/3600))/2
  stop_times$departure_hr <- floor(2*(stop_times$departure_in_seconds/3600))/2 
  
  #stop_times <- merge(stop_times, trips, by = "trip_id", all.x = TRUE)

  keep <- c("trip_id", "stop_id", "stop_sequence", "arrival_in_seconds", "departure_in_seconds", "arrival_hr", "departure_hr")
  stop_times <- stop_times[keep]
  
  routes$emme_id <- routes$route_short_name
  routes$vehicleCapacity <- 160
  routes$platformUse <- 1
  
  # routes <- sqldf(paste0("
  #   SELECT
  #   routes.*,
  #   COUNT(trips.trip_id) AS numTrips
  #   FROM
  #   routes,
  #   trips,
  #   calendar
  #   WHERE
  #   trips.route_id = routes.route_id AND
  #   trips.service_id = calendar.service_id AND
  #   calendar.", dayOfWeek, " = 1
  #   GROUP BY routes.route_id;"
  #                 ))

  # geojson_write(shapes, lat = 'shape_pt_lat', lon = 'shape_pt_lon', geometry = 'polygon', group = 'shape_id', 
  #               file = 'shapes.geojson', overwrite = TRUE, precision = 6)
  

  return(list("routes"=routes, "shapes"=shapes, "stops"=stops, "stop_times"=stop_times))
  
}

processEmme <- function(fname) {
  
  routeDataEmme <- data.frame(
    "transit_line" = character(), 
    "mode" = character(), 
    "vehicle_type" = character(), 
    "no_of_passengers" = numeric(), 
    "headway" = numeric(), 
    "no_of_vehicles" = numeric(), 
    "avg_pass_speed.km.hr" = numeric(), 
    "line_length.km" = numeric(), 
    "avg_veh_speed.km.hr" = numeric(), 
    "pass_hours" = numeric(), 
    "line_time.min" = numeric(), 
    "max_load_factor" = numeric(), 
    "pass_km" = numeric(),
    stringsAsFactors=FALSE
  )
  
  assignment <- data.frame(
    "transit_line" = character(), 
    "from_node" = numeric(), 
    "to_node" = numeric(), 
    "length.km" = numeric(), 
    "time.min" = numeric(), 
    "speed.km.hr" = numeric(), 
    "load_factor" = numeric(), 
    "volume" = numeric(), 
    "stop" = character(), 
    "exits" = numeric(), 
    "boardings" = numeric(), 
    "seat_probability" = numeric(), 
    stringsAsFactors=FALSE   
  )
  
  tab_parse <- function(chunk) {
    top = grep("^ Transit line:", chunk)
    if (!length(top)) return()
    
    trans_line = sub("^\\s*Transit line:\\s*(.*?)\\s*\\(.*$", "\\1", chunk[top])
    
    tab1 = seq(top+3, len=4)
    tab2 = seq(top+15, length(chunk)-2)
    
    vals = strsplit(chunk[tab1], "(?<=.{35})", perl=T)
    vals1 = gsub('^[^:]*:\\s*(.*?)\\s*$', '\\1', unlist(vals))
    
    row = list()
    row[1:3] = c(trans_line, vals1[1:2])
    row[4:13] = as.numeric(gsub('\\.?[^\\d\\.]*$', '', vals1[3:12], perl=T))
    
    routeDataEmme[nrow(routeDataEmme)+1, ] <<- row
    
    
    pos <- c(7, 14, 22, 30, 38, 46, 55, 74, 82, 90, 99)
    for (str in chunk[tab2]) {
      vals2 = sapply(pos, function(x) return( gsub(' ', '', substr(str, x-6, x))) )
      row = list(trans_line)
      row[2:12] = as.numeric(gsub('\\.?[^\\d\\.]*$', '', vals2, perl=T))
      assignment[nrow(assignment)+1, ] <<- row
    }
  }
  fh <- file(fname, "r") #, encoding = "UTF-8")
  chunk <- character(0)
  while (TRUE) {
    line = readLines(fh, n = 1)
    if (!length(line)) break
    if (line == "\f") {
      if (length(chunk) > 0) {
        tab_parse(chunk)
        chunk = character(0)
      }
      next
    }
    chunk <- c(chunk, line)
  }
  
  close(fh)
  if (length(chunk) > 0) tab_parse(chunk)
  
  assignment$transit_line <- gsub("t", "", assignment$transit_line)
  routeDataEmme$transit_line <- gsub("t", "", routeDataEmme$transit_line)
  assignment$stop[is.na(assignment$stop)] <- assignment$from_node[is.na(assignment$stop)]
  assignment$exits[is.na(assignment$exits)] <- 0
  assignment$boardings[is.na(assignment$boardings)] <- 0
  assignment$volume[is.na(assignment$volume)] <- 0
  assignment$load_factor[is.na(assignment$load_factor)] <- 0
  assignment$hr <- 6.5 # FIX user input
  assignment$stop_sequence <- 1
  
  numStops <- nrow(assignment)
  
  for (i in 2:numStops){
    # Add Stop Sequence
    if(assignment$transit_line[i] == assignment$transit_line[i - 1]){
      assignment$stop_sequence[i] <- assignment$stop_sequence[i - 1] + 1
    } else {
      assignment$stop_sequence[i] <- 1
    }
    # Fill Seat Probability NAs
    if(is.na(assignment$seat_probability[i])){
      assignment$seat_probability[i] <- assignment$seat_probability[i - 1]
    }
  }
  return(list("assignment"=assignment, "routeDataEmme"=routeDataEmme))
}

make_stn <- function(stationInventory, hrs) {
  
  numStations <- nrow(stationInventory)
  numHrs <- length(hrs)
  stn <- stationInventory[rep(seq_len(numStations), each = numHrs), ]
  stn$hr <- sort(hrs, decreasing = FALSE)
  newColumns <- c("PB_demand", 
                  "BS_demand", 
                  "PT_demand", 
                  "PB_capacity", 
                  "PB_saturation", 
                  "BS_saturation", 
                  "PT_saturation",
                  "PB_unserved",
                  "boardings",
                  "alightings",
                  "occupancy",
                  "transit_productions",
                  "transit_attractions",
                  "auto_productions",
                  "auto_attractions",
                  "production_pop_TT_transit",
                  "attraction_pop_TT_transit",
                  "production_pop_TT_auto",
                  "attraction_pop_TT_auto",
                  "production_avg_TT_transit",
                  "attraction_avg_TT_transit",
                  "production_avg_TT_auto",
                  "attraction_avg_TT_auto")
  
  stn[newColumns] <- 0
  
  return(stn)
}

make_onOff <- function(stationInventory, routes, assignment, routeDataEmme) {
  
  # FIX add Emme capacity for comparison
  onOff <- sqldf("
    SELECT
    routes.route_id AS route_id,
    routes.route_short_name AS route_short_name,
    stationInventory.station_id AS station_id,
    stationInventory.station_name AS station_name,
    stationInventory.station_order AS station_order,
    stationInventory.station_lat AS station_lat,
    stationInventory.station_lon AS station_lon,
    assignment.stop_sequence AS station_sequence,
    assignment.hr AS hr,
    assignment.boardings AS boardings,
    assignment.exits AS alightings,
    assignment.volume AS seg_demand,
    60 / routeDataEmme.headway * routes.vehicleCapacity AS seg_capacity,
    60 / routeDataEmme.headway * routes.vehicleCapacity - assignment.volume + assignment.boardings AS vacancy,
    assignment.load_factor AS seg_saturation
    FROM routes,
    stationInventory,
    assignment,
    routeDataEmme
    WHERE
    stationInventory.station_id_emme = assignment.stop AND 
    assignment.transit_line = routes.emme_id AND 
    routeDataEmme.transit_line = routes.emme_id",
    stringsAsFactors = FALSE,
    row.names = FALSE
  )
  
  return(onOff)
}

make_seg <- function(onOff) {
  
  # Create seg_AR_AH
  onOff <- onOff[order(onOff$station_sequence), ]
  onOff <- onOff[order(onOff$hr), ]
  onOff <- onOff[order(onOff$route_short_name), ]
  
  numOnOff <- nrow(onOff)
  
  onOff_A <- onOff[1:(numOnOff - 1), ]
  
  duplicateColNames <- c("station_id", "station_name", "station_order",  
                         "station_lat", "station_lon", "station_sequence")
  
  onOff_B <- onOff[2:numOnOff, duplicateColNames]
  
  setnames(onOff_A, old = duplicateColNames, new = paste0(duplicateColNames,"A"))
  setnames(onOff_B, old = duplicateColNames, new = paste0(duplicateColNames,"B"))
  
  seg <- cbind.data.frame(c(onOff_A, onOff_B))
  seg <- seg[(seg$station_sequenceB - seg$station_sequenceA) == 1,]
  seg$routeStnSeg_id <- do.call(paste, c(seg[c("route_id", "station_idA", "station_idB")], sep = "_"))
  seg$routeStnSeg_name <- do.call(paste, c(seg[c("route_short_name", "station_nameA", "station_nameB")], sep = "_"))
 
  # Offset coordinates for drawing
  
  # seg$theta <- atan2((seg$station_lonB - seg$station_lonA), 
  #                            (seg$station_latB - seg$station_latA))
  # 
  # seg$station_lonA_offset <- seg$station_lonA + .001*cos(seg$theta)
  # seg$station_lonB_offset <- seg$station_lonB + .001*cos(seg$theta)
  # 
  # seg$station_latA_offset <- seg$station_latA - .001*sin(seg$theta)
  # seg$station_latB_offset <- seg$station_latB - .001*sin(seg$theta)
  
  return(seg) 
  
  # # Calculate route segment demand FIX only for onOffCounts
  # 
  # seg$demand_prevSegment <- 0
  # seg$seg_demand <- 0
  # seg$station_sequenceA[1] == 1
  # 
  # for (i in 1:numSegments) {
  #   if(seg$station_sequenceA[i] == 1) {
  #     seg$demand_prevSegment[i] <- 0
  #     seg$seg_demand[i] <- seg$boardings[i]
  #   } else {
  #     seg$demand_prevSegment[i] <- seg$seg_demand[i-1]
  #     seg$seg_demand[i] <- with(seg[i,], demand_prevSegment - alightings + boardings)
  #   }
  # }
  # 
  # # Calculate segment saturation
  # seg$seg_saturation <- with(seg, seg_demand/seg_capacity)
  
  # Update Route List
  #routeListDF <- seg[c("route_id", "route_short_name")]
  #routeListDF <- unique.data.frame(routeListDF)
  #routeList <- routeListDF$route_id
  #names(routeList) <- routeListDF$route_short_name
  
  #updateSelectInput(session, "selectRoute",
  #                 choices = routeList)
  
}

make_corridorSeg <- function(stationInventory, seg, hrs) {
  
  # Create corridorSeg structure from stationInventory
  
  stationInventory_temp <- stationInventory[c("station_id", "station_name", "corridor", 
                                              "station_order", "station_lat", "station_lon")]
  stationInventory_temp <- stationInventory_temp[order(stationInventory_temp$station_order), ]
  stationInventory_temp <- stationInventory_temp[order(stationInventory_temp$corridor), ]
  
  numStations <- nrow(stationInventory_temp)
  
  stationInventory_tempA <- stationInventory_temp[1:(numStations - 1), ]
  stationInventory_tempB <- stationInventory_temp[2:numStations, ]
  
  colnames(stationInventory_tempA) <- paste0(colnames(stationInventory_tempA),"A")
  colnames(stationInventory_tempB) <- paste0(colnames(stationInventory_tempB),"B")
  
  corridorSeg_outbound <- cbind.data.frame(c(stationInventory_tempA, stationInventory_tempB))
  corridorSeg_outbound <- corridorSeg_outbound[(corridorSeg_outbound$station_orderB - corridorSeg_outbound$station_orderA) == 1,]
  corridorSeg_outbound$direction <- "outbound"
  
  corridorSeg_inbound <- cbind.data.frame(c(stationInventory_tempB, stationInventory_tempA))
  corridorSeg_inbound$direction <- "inbound"
  colnames(corridorSeg_inbound) <- colnames(corridorSeg_outbound)
  corridorSeg_inbound <- corridorSeg_inbound[(corridorSeg_inbound$station_orderA - corridorSeg_inbound$station_orderB) == 1,]
  
  corridorSeg_AZ <- rbind(corridorSeg_outbound, corridorSeg_inbound)
  
  corridorSeg_AZ$corridorSeg_id <- do.call(paste, c(corridorSeg_AZ[c("station_idA", "station_idB")], sep = "_"))
  corridorSeg_AZ$corridorSeg_name <- do.call(paste, c(corridorSeg_AZ[c("station_nameA", "station_nameB")], sep = "_"))
  
  numSegments <- nrow(corridorSeg_AZ)
  numHrs <- length(hrs)
  corridorSeg <- corridorSeg_AZ[rep(seq_len(numSegments), each = numHrs), ]
  corridorSeg$hr <- sort(hrs, decreasing = FALSE)
  
  # Offset coordinates for drawing
  
  corridorSeg$theta <- atan2((corridorSeg$station_lonB - corridorSeg$station_lonA), 
                                          (corridorSeg$station_latB - corridorSeg$station_latA))
  
  corridorSeg$station_lonA_offset <- corridorSeg$station_lonA + .001*cos(corridorSeg$theta)
  corridorSeg$station_lonB_offset <- corridorSeg$station_lonB + .001*cos(corridorSeg$theta)
  
  corridorSeg$station_latA_offset <- corridorSeg$station_latA - .001*sin(corridorSeg$theta)
  corridorSeg$station_latB_offset <- corridorSeg$station_latB - .001*sin(corridorSeg$theta)
  
  # Aggregate route segments to corridor segments (where > <)
  corridorSeg <- sqldf(
    "SELECT
    corridorSeg.*,
    SUM(seg.seg_demand) AS corridorSeg_demand,
    SUM(seg.seg_capacity) AS corridorSeg_capacity
    FROM 
    seg,
    corridorSeg
    WHERE
    ((seg.station_orderA <= corridorSeg.station_orderA AND 
    seg.station_orderB >= corridorSeg.station_orderB) OR 
    (seg.station_orderA >= corridorSeg.station_orderA AND 
    seg.station_orderB <= corridorSeg.station_orderB)) AND
    seg.hr = corridorSeg.hr
    GROUP BY
    corridorSeg.corridorSeg_id,
    corridorSeg.hr",
    stringsAsFactors = FALSE,
    row.names = FALSE
  )

  # Update corridor saturation
  corridorSeg$corridorSeg_saturation <- ifelse(corridorSeg$corridorSeg_capacity == 0, 0, corridorSeg$corridorSeg_demand/corridorSeg$corridorSeg_capacity)

  return(corridorSeg)
}

make_odm <- function(stationInventory, hrs) {
  
  # Create Edge List
  O_list <- NULL
  D_list <- NULL
  stnList <- stationInventory$station_id
  
  for (i in 1:length(stnList)) {
    O_list <- c(O_list, stnList)
    D_list <- c(D_list, rep(stnList[i], length(stnList)))
  }
  
  edgeList <- data.frame(O = O_list, D = D_list)
  edgeList$pair_id <- do.call(paste, c(edgeList[c("O", "D")], sep = "_"))

  numPairs <- nrow(edgeList)
  numHrs <- length(hrs)
  odm <- edgeList[rep(seq_len(numPairs), each = numHrs), ]
  odm$hr <- sort(hrs, decreasing = FALSE)
  newColumns <- c("transit_trips",
                  "TT_transit",
                  "transit_GC",
                  "auto_trips",
                  "TT_auto",
                  "auto_GC",
                  "pop_TT_transit",
                  "pop_TT_auto",
                  "TT_ratio",
                  "TT_lost")

  odm[newColumns] <- 0
  
  return(odm)
}

update_BS_saturation <- function(stn, stops2stations, calendar, routes, stops, stop_times, trips, dayOfWeek, timeInterval) {

  BS_demand_gtfs <- sqldf(
    
    paste0("SELECT
           station_id,
           station_name,
           arrival_hr,
           0.5 * SUM(routes.platformUse)/", timeInterval, " AS BS_demand
           FROM
           stop_times,
           calendar,
           routes,
           trips,
           stops,
           stops2stations
           WHERE
           stop_times.trip_id = trips.trip_id AND
           trips.route_id = routes.route_id AND
           trips.service_id = calendar.service_id AND
           stop_times.stop_id = stops.stop_id AND ", 
           dayOfWeek, " = 1 AND
           stop_times.stop_id = stops2stations.stop_id
           GROUP BY
           station_id,
           station_name,
           arrival_hr;"),
    
    stringsAsFactors = FALSE,
    row.names = FALSE
    )
  
  stn <- sqldf(c("UPDATE
                            stn
                            SET BS_demand = (
                            SELECT 
                            BS_demand_gtfs.BS_demand
                            FROM
                            BS_demand_gtfs
                            WHERE
                            BS_demand_gtfs.station_id = stn.station_id AND
                            BS_demand_gtfs.arrival_hr = stn.hr)",
                            "SELECT
                            *
                            FROM
                            stn"
  ))
  
  # Update BS_saturation
  stn$BS_saturation <- ifelse(stn$BS_capacity == 0, 0, stn$BS_demand/stn$BS_capacity)
  
  return(stn)
}

update_PB_saturation <- function(stn, onOff) {
  
  stn <- subset(stn, select = -c(boardings, alightings, occupancy, PB_demand, PB_capacity, PB_unserved))
  
  # Aggregate on-off counts by station
  stn <- sqldf("
    SELECT
    stn.*,
    SUM(onOff.boardings) AS boardings,
    SUM(onOff.alightings) AS alightings,
    SUM(onOff.boardings + onOff.alightings) AS PB_demand,
    SUM(onOff.seg_demand - onOff.boardings) AS occupancy,
    SUM(MAX((onOff.seg_capacity - onOff.seg_demand + onOff.boardings), 0)) + (stn.PT_capacity - stn.transit_attractions) AS PB_capacity
    FROM
    stn,
    onOff
    GROUP BY
    stn.station_id,
    stn.hr"
  )
  
  # PB_saturation
  stn$PB_saturation <- ifelse(stn$PB_capacity == 0, 0, stn$PB_demand/stn$PB_capacity)

  return(stn)
}

update_odSums <- function(stn, odm) {

  # Productions
  stn_productionSums <- sqldf("
    SELECT
    stn.station_id,
    stn.hr,
    SUM(odm.transit_trips) AS transit_productions,
    SUM(odm.auto_trips) AS auto_productions,
    SUM(odm.transit_trips * odm.TT_transit) AS production_pop_TT_transit,
    SUM(odm.auto_trips * odm.TT_auto) AS production_pop_TT_auto,
    SUM(odm.transit_trips * odm.TT_transit) / SUM(odm.transit_trips) AS production_avg_TT_transit,
    SUM(odm.auto_trips * odm.TT_auto) / SUM(odm.auto_trips) AS production_avg_TT_auto
    FROM
    stn,
    odm
    WHERE
    stn.station_id = odm.O AND
    stn.hr = odm.hr
    GROUP BY
    odm.O,
    stn.hr;"
  )

  # Attractions
  stn_attractionSums <- sqldf("
    SELECT
    stn.station_id,
    stn.hr,
    SUM(odm.transit_trips) AS transit_attractions,
    SUM(odm.auto_trips) AS auto_attractions,
    SUM(odm.transit_trips * odm.TT_transit) AS attraction_pop_TT_transit,
    SUM(odm.auto_trips * odm.TT_auto) AS attraction_pop_TT_auto,
    SUM(odm.transit_trips * odm.TT_transit) / SUM(odm.transit_trips) AS attraction_avg_TT_transit,
    SUM(odm.auto_trips * odm.TT_auto) / SUM(odm.auto_trips) AS attraction_avg_TT_auto
    FROM
    stn,
    odm
    WHERE
    stn.station_id = odm.D AND
    stn.hr = odm.hr
    GROUP BY
    odm.D,
    stn.hr;"
  )
  
  stn <- subset(stn, select = -c(transit_productions, auto_productions, production_pop_TT_transit,
                                 production_pop_TT_auto, production_avg_TT_transit, production_avg_TT_auto))
  stn <- subset(stn, select = -c(transit_attractions, auto_attractions, attraction_pop_TT_transit,
                                 attraction_pop_TT_auto, attraction_avg_TT_transit, attraction_avg_TT_auto))
  
  stn <- sqldf("
      SELECT
      stn.*,
      p.transit_productions AS transit_productions,
      p.auto_productions AS auto_productions,
      p.production_pop_TT_transit AS production_pop_TT_transit,
      p.production_pop_TT_auto AS production_pop_TT_auto,
      p.production_avg_TT_transit AS production_avg_TT_transit,
      p.production_avg_TT_auto AS production_avg_TT_auto,
      a.transit_attractions AS transit_attractions,
      a.auto_attractions AS auto_attractions,
      a.attraction_pop_TT_transit AS attraction_pop_TT_transit,
      a.attraction_pop_TT_auto AS attraction_pop_TT_auto,
      a.attraction_avg_TT_transit AS attraction_avg_TT_transit,
      a.attraction_avg_TT_auto AS attraction_avg_TT_auto
      FROM stn 
      LEFT OUTER JOIN stn_productionSums AS p
      ON 
      stn.station_id = p.station_id AND 
      stn.hr = p.hr
      LEFT OUTER JOIN stn_attractionSums AS a
      ON
      stn.station_id = a.station_id AND 
      stn.hr = a.hr"
               )
  
  stn[is.na(stn)] <- 0
  
  # Weighted average travel times
  stn$production_avg_TT_transit <- ifelse(stn$transit_productions == 0, 0, stn$production_pop_TT_transit/stn$transit_productions)
  stn$production_avg_TT_auto <- ifelse(stn$auto_productions == 0, 0, stn$production_pop_TT_auto/stn$auto_productions)
  stn$attraction_avg_TT_transit <- ifelse(stn$transit_attractions == 0, 0, stn$attraction_pop_TT_transit/stn$transit_attractions)
  stn$attraction_avg_TT_auto <- ifelse(stn$auto_attractions == 0, 0, stn$attraction_pop_TT_auto/stn$auto_attractions)

  # Update PT_saturation
  stn$PT_demand <- with(stn, transit_productions + transit_attractions)
  stn$PT_saturation <- ifelse(stn$PT_capacity == 0, 0, stn$PT_demand/stn$PT_capacity)

  return(stn)
}

update_odm <- function(odm, new_matrix, new_column, by_hr) {

  # Add a column to the O-D Matrix
  # Data.Table
  
  odm[[new_column]] <- NULL
  
  if (by_hr == TRUE) {
    odm <- data.table::data.table(odm, key = c("O", "D", "hr"))
    new_matrix <- data.table::data.table(new_matrix, key = c("O", "D", "hr"))
    odm <- merge(odm, new_matrix, by = c("O", "D", "hr"), all = TRUE)
  } else {
    new_matrix$hr <- NULL
    odm <- data.table::data.table(odm, key = c("O", "D"))
    new_matrix <- data.table::data.table(new_matrix, key = c("O", "D"))
    odm <- merge(odm, new_matrix, by = c("O", "D"), all = TRUE)
  }

  # SQLDF
  # odm[[new_column]] <- NULL
  # 
  # if (by_hr == TRUE) {
  #   odm <- sqldf(paste0(
  #     "SELECT
  #     odm.*, ",
  #     matrix, ".", new_column, " AS ", new_column, " 
  #     FROM
  #     odm, ",
  #     matrix, " 
  #     WHERE
  #     odm.O = ", matrix, ".O AND
  #     odm.D = ", matrix, ".D AND
  #     odm.hr = ", matrix, ".hr;"
  #   ))
  # } else {
  #   odm <- sqldf(paste0(
  #     "SELECT
  #     odm.*, ",
  #     matrix, ".", new_column, " AS ", new_column, " 
  #     FROM
  #     odm, ",
  #     matrix, " 
  #     WHERE
  #     odm.O = ", matrix, ".O AND 
  #     odm.D = ", matrix, ".D;"
  #   ))
  # }
  
  return(odm)
}
