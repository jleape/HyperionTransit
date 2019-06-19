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

# Radar plot of Saturation
plotRadar <- function(station_id, stn_SS_SH) {
  
  renderPlot({
    minSat <- c(0,0,0)
    selectedSat <- data.frame(PT = stn_SS_SH$PT_saturation,
                              PB = stn_SS_SH$PB_saturation,
                              BS = stn_SS_SH$BS_saturation)
    maxSat <- c(3,3,3)
    radarDF <- rbind(maxSat, minSat, selectedSat)
    par(mar = c(0,0,1,0))
    radarchart(radarDF,
               axistype = 1,
               maxmin = TRUE,
               vlabels = c("Pax in Turnstiles", "Pax in Bays", "Buses in Station"),
               caxislabels = c("","100%","200%","300%"),
               axislabcol = 1,
               seg = 3,
               pty = 32,
               plwd = 3,
               pcol = 1,
               title = paste0("Saturation"),
               centerzero = TRUE,
               cglwd = 1.5
    )
  })
}

# Arc Diagram of O-D demand
makeArcDiagram <- function(odm_temp, stationInventory) {

  # Create Edge List
  O_list <- NULL
  D_list <- NULL
  stnList <- unique(c(odm_temp$O, odm_temp$D))
  
  print(odm_temp)
  
  for (i in 1:length(stnList)) {
    O_list <- c(O_list, stnList)
    D_list <- c(D_list, rep(stnList[i], length(stnList)))
  }
  
  edgeList <- data.frame(O = O_list, D = D_list)
  edgeList$pair_id <- do.call(paste, c(edgeList[c("O", "D")], sep = "_"))
  
  # Create Matrix
  stationInventory <- stationInventory
  stationInventory2 <- stationInventory
  
  odArc_DF <- sqldf("SELECT
                    edgeList.pair_id,
                    edgeList.O,
                    edgeList.D,
                    stationInventory.station_name AS O_name,
                    stationInventory2.station_name AS D_name
                    FROM 
                    edgeList
                    LEFT OUTER JOIN
                    stationInventory,
                    stationInventory2
                    ON
                    edgeList.O = stationInventory.station_id AND 
                    edgeList.D = stationInventory2.station_id"
  )
  
  odArc_DF$DL_var <- 0
  
  odArc_DF <- sqldf(c("UPDATE
                      odArc_DF
                      SET DL_var = (
                      SELECT 
                      odm_temp.DL_var
                      FROM
                      odm_temp
                      WHERE
                      odArc_DF.O = odm_temp.O AND
                      odArc_DF.D = odm_temp.D)",
                      "SELECT
                      *
                      FROM
                      odArc_DF;"
  ))
  
  odArc_DF$DL_var[is.na(odArc_DF$DL_var)] <- 0
  
  renderPlot({
    
    numStations <- length(unique(odArc_DF$O))
    arcMtx <- as.matrix(odArc_DF[c("O", "D")])
    arcplot(arcMtx, 
            labels = odArc_DF[1:numStations, "O_name"], 
            lwd.arcs = 15 * (odArc_DF$DL_var - min(odArc_DF$DL_var)) / (max(odArc_DF$DL_var) - min(odArc_DF$DL_var))
    )
    
  })
}

# Station Saturation Bullet Graph
makeBulletGraph <- function(stn_SS_AH, stn_SS_SH, stn_name, bins, pal) {
  # MIT License
  # Bob Rudis (@hrbrmstr) bob@rudis.net | http://rud.is/b | http://amzn.to/sudabook
  # http://bit.ly/1fs6ooC
  # Modified to use vector as input parameter for color ranges by Jonathan Hoagland Leape
  
  stnSatBG_DF <- data.frame(
    measure = c("Pax in Bays", "Buses in Station", "Pax in Turnstiles"),
    target = c(max(stn_SS_AH$PB_saturation), max(stn_SS_AH$BS_saturation), max(stn_SS_AH$PT_saturation)), # max of day
    value = c(
      if(is.null(stn_SS_SH$PB_saturation)){0} else {stn_SS_SH$PB_saturation},
      if(is.null(stn_SS_SH$BS_saturation)){0} else {stn_SS_SH$BS_saturation},
      if(is.null(stn_SS_SH$PT_saturation)){0} else {stn_SS_SH$PT_saturation}
    )
  )
    
  renderPlot({
    
    bins <- rev(bins)
    pal <- rev(pal)
    numBullets <- nrow(stnSatBG_DF)
    numBins <- length(bins)
    
    for (i in 1:numBins) {
      stnSatBG_DF[, 3 + i] <- bins[i]
    }
    
    gg <- ggplot(stnSatBG_DF) 
    
    for (i in 1:numBins) {
      gg <- gg + geom_bar(aes_string(colnames(stnSatBG_DF)[1], colnames(stnSatBG_DF)[3 + i]),
                          fill=pal[i], stat="identity", width=0.4, alpha=0.3)
    }
    
    gg <- gg + geom_bar(aes(measure, value), fill = "black",  stat = "identity", width = 0.2)
    gg <- gg + geom_errorbar(aes(measure, target, ymin = target, ymax = target), color = "black", width = 0.35)
    gg <- gg + geom_point(aes(measure, target), colour = "black", size = 2.5)
    gg <- gg + scale_y_continuous(breaks = seq(0,3,1))
    gg <- gg + labs(title=stn_name, x="Saturation")
    gg <- gg + coord_flip()
    gg <- gg + scale_colour_identity()
    gg <- gg + scale_fill_identity()
    gg <- gg + theme(axis.text.x=element_text(size = 15),
                     axis.title.x=element_blank(),
                     axis.line.y=element_blank(), 
                     axis.text.y=element_text(size = 15, hjust = 1, color="black"), 
                     axis.ticks.y=element_blank(),
                     axis.title.y=element_blank(),
                     legend.position="none",
                     panel.background=element_blank(), 
                     panel.border=element_blank(),
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),
                     plot.background=element_blank(),
                     plot.title=element_text(size = 24, hjust = 0))
    
    return(gg)
  })
}

# Station Saturation Timeline
makeStnSatTimeline <- function(stn_SS_AH, colorVar){
  
  vars_SatType <- c(
    "Buses in Stations" = "BS",
    "Pax in Turnstiles" = "PT",
    "Pax in Bays" = "PB"
  )

  renderPlot({
    
    plot.title <- "Saturation Timeline"
    y.label <- names(vars_SatType[vars_SatType == colorVar])
    station_name <- stn_SS_AH$station_name[1]
    Demand <- "Demand"
    Capacity <- "Capacity"
    
    gg <- ggplot(data=stn_SS_AH, aes(x=hr)) + 
      geom_line(aes_string(y = paste0(colorVar, "_demand"), color = "Demand"), size = 2) + 
      geom_line(aes_string(y = paste0(colorVar, "_capacity"), color = "Capacity"), size = 2) +
      scale_color_manual(values = c("darkred", "dodgerblue4")) +
      labs(title=bquote(atop(.(plot.title), atop(italic(.(station_name)), ""))),
           x="Time of Day",
           y=y.label) +
      scale_x_continuous(breaks = seq(4,24,2)) +
      coord_cartesian(xlim=c(4,24)) + 
      theme(legend.title=element_blank(),
            panel.background=element_rect(fill = 'white'),
            #plot.background=element_rect(fill = 'grey90')),
            panel.grid.major = element_line(color = 'grey90'), 
            panel.grid.minor = element_line(color = 'grey90', size = 0.25),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            legend.key=element_rect(fill='white'),
            legend.position="bottom"
      )
    
    
    return(gg)
  })
}

# Route Segment Saturation Profile
makeSegSatProfile <- function(seg_SR_SH){

  renderPlot({
    
    plot.title <- "Route-Segment Saturation"
    route_name <- seg_SR_SH$route_short_name[1]
    Demand <- "Demand"
    Capacity <- "Capacity"
    
    gg <- ggplot(data=seg_SR_SH, aes(x=station_sequenceA)) + 
      geom_line(aes(y = seg_demand, color = "Demand"), size = 2) + 
      geom_line(aes(y = seg_capacity, color = "Capacity"), size = 2) +
      scale_x_continuous(breaks=seg_SR_SH$station_sequenceA,
                         labels=seg_SR_SH$station_nameA) +
      scale_color_manual(values = c("darkred", "dodgerblue4")) +
      labs(title=bquote(atop(.(plot.title), atop(italic(.(route_name)), ""))),
           x="Station",
           y="Passengers per Hour"
      ) +
      theme(legend.title=element_blank(),
            panel.background=element_rect(fill = 'white'),
            panel.grid.major = element_line(color = 'grey90'), 
            panel.grid.minor = element_line(color = 'grey90', size = 0.25),
            axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle = 45, hjust = 1),
            axis.ticks.y=element_blank(),
            legend.key=element_rect(fill='white'),
            legend.position="bottom"
      )
   
    return(gg)
  })
}

# Boarding/Alighting Bar Chart
makeBoardingSatBC <- function(onOff_SS_SH, station_name){

  boardingSat_DF <- onOff_SS_SH[c("route_short_name", "boardings", "vacancy")]
  colnames(boardingSat_DF) <- c("Route", "Boardings", "Vacancy")
  
  boardingSat_DF.m <- melt(boardingSat_DF, id.vars='Route')
  
  plot.title <- "Boarding Saturation"
  
  renderPlot({
    
    gg <- ggplot(boardingSat_DF.m, aes(Route, value)) +   
      geom_bar(aes(fill = variable), width = 0.5, position = "dodge", stat="identity") +
      labs(title=bquote(atop(.(plot.title), atop(italic(.(station_name)), ""))),
           y="Passengers per Hour"
      ) +
      theme(legend.title=element_blank(),
            panel.background=element_rect(fill = 'white'),
            panel.grid.major = element_line(color = 'grey90'), 
            panel.grid.minor = element_line(color = 'grey90', size = 0.25),
            axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle = 90, hjust = 1),
            axis.ticks.y=element_blank(),
            legend.key=element_rect(fill='white'),
            legend.position="bottom"
      ) 
    
    return(gg)
  })
}

# Corridor Segment Saturation Profile
makeCorridorSegSatProfile <- function(corridorSeg_SZ_SH){
  
  renderPlot({

    corridor <- corridorSeg_SZ_SH$corridor[1]
    
    # Inbound
    corridorSeg_inbound <- corridorSeg_SZ_SH[corridorSeg_SZ_SH$direction == "inbound", ]
    
    plot.title <- paste0("Corridor ", corridor, ": Inbound")
    
    gg_inbound <- ggplot(data=corridorSeg_inbound, aes(x=station_orderA)) + 
      geom_line(aes(y = corridorSeg_demand, color = "Demand"), size = 2) +
      geom_line(aes(y = corridorSeg_capacity, color = "Capacity"), size = 2) +
      scale_x_continuous(breaks=corridorSeg_inbound$station_orderA,
                         labels=corridorSeg_inbound$station_nameA) +
      scale_color_manual(values = c("darkred", "dodgerblue4")) +
      labs(title=plot.title, y="Passengers per Hour", x = NULL) +
      theme(legend.title=element_blank(),
            panel.background=element_rect(fill = 'white'),
            panel.grid.major = element_line(color = 'grey90'),
            panel.grid.minor = element_line(color = 'grey90', size = 0.25),
            axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle = 45, hjust = 1),
            axis.ticks.y=element_blank(),
            legend.key=element_rect(fill='white'),
            legend.position="none"
      )
    
    # Outbound
    corridorSeg_outbound <- corridorSeg_SZ_SH[corridorSeg_SZ_SH$direction == "outbound", ]
    
    plot.title <- paste0("Corridor ", corridor, ": Outbound")
    
    gg_outbound <- ggplot(data=corridorSeg_outbound, aes(x=station_orderA)) + 
      geom_line(aes(y = corridorSeg_demand, color = "Demand"), size = 2) +
      geom_line(aes(y = corridorSeg_capacity, color = "Capacity"), size = 2) +
      scale_x_continuous(breaks=corridorSeg_outbound$station_orderA,
                         labels=corridorSeg_outbound$station_nameA) +
      scale_color_manual(values = c("darkred", "dodgerblue4")) +
      labs(title=plot.title, y="Passengers per Hour", x = NULL) +
      theme(legend.title=element_blank(),
            panel.background=element_rect(fill = 'white'),
            panel.grid.major = element_line(color = 'grey90'),
            panel.grid.minor = element_line(color = 'grey90', size = 0.25),
            axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle = 45, hjust = 1),
            axis.ticks.y=element_blank(),
            legend.key=element_rect(fill='white'),
            legend.position="bottom"
      )    
    
    gg <- grid.arrange(gg_inbound, gg_outbound, ncol = 1)
    
    return(gg)
  })    
}
