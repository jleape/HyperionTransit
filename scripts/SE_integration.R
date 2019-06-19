library(jsonlite)
library(sqldf)
library(stringr)
library(stringi)
library(geosphere)

to.Date <- function(secs) {
  h<-secs%/%(60*60)
  m<-secs%%(60*60)%/%60
  s<-secs%%(60*60)%%60
  return(paste0(h,":",m,":",s))
}

applyConveyal <- function(calendar, routes, stop_times, stops, trips, conveyal)

  conveyal<-conveyal$modifications
  
  spl.f <- function(string) {
    return(str_split_fixed(string,pattern=":",n=2)[,2])
  }
  
  # Iterate through json changes
  
  for (mods1 in conveyal) {
    ########## add-trips ##########
    if (mods1$type=="add-trips") {
      print("Mod: add-trips")
      
      #Collect all variables for mod
      if (!is.null(mods1$frequencies)) {
        if (length(mods1$frequencies)!=0) {
          frequencies_mod<-mods1$frequencies
        }
      }
      
      if (!is.null(mods1$stops)) {
        if (length(mods1$stops)!=0) {
          stops_mod<-mods1$stops
        }
      }
      
      if (!is.null(mods1$bidirectional)) {
        if (length(mods1$bidirectional)!=0) {
          bidirectional<-as.logical(mods1$bidirectional)
        } else {bidirectional<-FALSE}
      } else {bidirectional<-FALSE}
      
      
      #Cycle through all frequencies
      for (freq in frequencies_mod) {
        
        #Get all frequency variables
        start_t<-freq$startTime
        end_t<-freq$endTime
        headway<-freq$headwaySecs
        hops_mod<-c(freq$hopTimes,0)
        dwell_mod<-freq$dwellTimes
        week_days<-as.numeric(c(freq$monday,freq$tuesday,freq$wednesday,freq$thursday,freq$friday,freq$saturday,freq$sunday))
        
        found_service<-calendar[calendar$monday==week_days[1] &
                                  calendar$tuesday==week_days[2] &
                                  calendar$wednesday==week_days[3] &
                                  calendar$thursday==week_days[4] &
                                  calendar$friday==week_days[5] &
                                  calendar$saturday==week_days[6] &
                                  calendar$sunday==week_days[7],]
        #Find if days of week match with existing services, if not - create one.
        if (is.null(found_service) | nrow(found_service)==0) {
          service_selected<-paste0("SER-",stri_rand_strings(1, 2))
          calendar<-rbind(calendar,data.frame(service_id=service_selected,
                                              monday=week_days[1],
                                              tuesday=week_days[2],
                                              wednesday=week_days[3],
                                              thursday=week_days[4],
                                              friday=week_days[5],
                                              saturday=week_days[6],
                                              sunday=week_days[7],
                                              start_date=0,
                                              end_date=0))
        } else {
          service_selected<-found_service[1,"service_id"]
        }
        
        #Get stop ids.
        #For pair of coordinates, find if exact stop coordinates are allready exists and use it's id.
        #if not, then create new stop and find closest parent station to it
        stops_id<-c()
        for (s_pos in stops_mod) { 
          if (is.null(s_pos$id) | length(s_pos$id)==0) {
            s_id<-stops[stops$stop_lat==s_pos$lat & stops$stop_lon==s_pos$lon,]$stop_id
            if (is.null(s_id) | length(s_id)==0) {
              while(1) {
                s_id<-paste0("SE-",stri_rand_strings(1, 10))
                if (!(s_id %in% stops$stop_id)) {
                  break
                }
              }
              dist_range<-c()
              parents_stops<-stops[stops$location_type==1,]
              for (sID in 1:nrow(parents_stops)) {
                dist_range<-c(dist_range,distm (c(parents_stops[sID,]$stop_lon,parents_stops[sID,]$stop_lat), c(s_pos$lon, s_pos$lat), fun = distHaversine))
              }
              min_parent<-min(dist_range) 
              if (min_parent<500) {
                parent_selected<-parents_stops[which.min(dist_range),]$stop_id
              } else {parent_selected<-NA}
              stops<-rbind(stops,data.frame(stop_id=s_id,stop_code=NA,stop_name="",stop_desc="",stop_lat=s_pos$lat,stop_lon=s_pos$lon,location_type=0,parent_station=parent_selected,wheelchair_boarding=0))
              }
          } else {
            s_id<-spl.f(s_pos$id)
            if (!(s_id %in% stops$stop_id)) {
              while(1) { 
                s_id<-paste0("SE-",stri_rand_strings(1, 10))
                if (!(s_id %in% stops$stop_id)) {
                  break
                }
              }
              stops<-rbind(stops,data.frame(stop_id=s_id,stop_code=NA,stop_name="",stop_desc="",stop_lat=s_pos$lat,stop_lon=s_pos$lon,location_type=0,parent_station=NA,wheelchair_boarding=0))
            }
          }
          stops_id<-c(stops_id,s_id)
        }
        
        #Generate new ID for trips and routes
        while(1) {
          tr_id<-paste0("TI-",stri_rand_strings(1, 10))
          if (!(tr_id %in% trips$trip_id)) {
            break
          }
        }
        while(1) {
          ro_id<-paste0("RI-",stri_rand_strings(1, 10))
          if (!(ro_id %in% routes$route_id)) {
            break
          }
        }
        
        #Generate new trips, stop_times and routes
        headway_mod<-0
        trip_stops_stack<-data.frame()
        trips_stack<-data.frame()
        route_stack<-data.frame()
        trip_n<-0
        #Build stop_times from stops_id and frequency variables
        while(1) {
          trip_n<-trip_n+1
          new_trip_stops<-data.frame(trip_id=paste0(tr_id,"-",trip_n),
                                     arrival_time=0,
                                     departure_time=0,
                                     stop_id=stops_id,
                                     stop_sequence=0,
                                     shape_dist_traveled=0,
                                     timepoint=0,
                                     arrival_in_seconds=0,
                                     departure_in_seconds=0, 
                                     hops_mod=unlist(hops_mod),
                                     dwell_mod=unlist(dwell_mod))
          new_trip_stops[1,]$arrival_in_seconds<-start_t+headway_mod 
          for (i in 2:nrow(new_trip_stops)) {
            new_trip_stops[i,]$arrival_in_seconds<-new_trip_stops[i-1,]$arrival_in_seconds+new_trip_stops[i-1,]$hops_mod+new_trip_stops[i-1,]$dwell_mod
          }
          new_trip_stops$departure_in_seconds<-new_trip_stops$arrival_in_seconds+new_trip_stops[i,]$dwell_mod
          
          trip_stops_stack<-rbind(trip_stops_stack,new_trip_stops)
          
          headway_mod<-headway_mod+headway
          if (start_t+headway_mod>=end_t) {
            break
          }
        }
        #Create trips for new stop_time trips
        trips_stack<-rbind(trips_stack,data.frame(route_id=ro_id,
                                                  service_id=service_selected, 
                                                  trip_id=unique(trip_stops_stack$trip_id),
                                                  trip_headsign=NA,
                                                  shape_id=NA,
                                                  wheelchair_accessible=0,
                                                  bikes_allowed=0))
        #Create route for new trips
        route_stack<-rbind(route_stack,data.frame(route_id=ro_id,
                                                  agency_id=NA,
                                                  route_short_name="",
                                                  route_long_name="",
                                                  route_desc="",
                                                  route_type="",
                                                  route_color="FFFFFF",
                                                  route_text_color="000000"))
        
        trip_stops_stack$hops_mod<-NULL
        trip_stops_stack$dwell_mod<-NULL
        
        #Add new trips to existind GTFS data
        stop_times<-rbind(stop_times,trip_stops_stack)
        
        trips<-rbind(trips,trips_stack)
        
        routes<-rbind(routes,route_stack)
        
        #For bidirectional repeat generation process but with reverse stops order
        if (bidirectional) {
  
          tr_id<-paste0(tr_id,"-R")
  
          ro_id<-paste0(ro_id,"-R")
  
          headway_mod<-0
          trip_stops_stack<-data.frame()
          trips_stack<-data.frame()
          route_stack<-data.frame()
          trip_n<-0
          while(1) {
            trip_n<-trip_n+1
            new_trip_stops<-data.frame(trip_id=paste0(tr_id,"-",trip_n),
                                       arrival_time=0,
                                       departure_time=0,
                                       stop_id=rev(stops_id),
                                       stop_sequence=0,
                                       shape_dist_traveled=0,
                                       timepoint=0,
                                       arrival_in_seconds=0,
                                       departure_in_seconds=0, 
                                       hops_mod=rev(unlist(hops_mod)),
                                       dwell_mod=rev(unlist(dwell_mod))) 
            new_trip_stops[1,]$arrival_in_seconds<-start_t+headway_mod 
            for (i in 2:nrow(new_trip_stops)) {
              new_trip_stops[i,]$arrival_in_seconds<-new_trip_stops[i-1,]$arrival_in_seconds+new_trip_stops[i-1,]$hops_mod+new_trip_stops[i-1,]$dwell_mod
            }
            new_trip_stops$departure_in_seconds<-new_trip_stops$arrival_in_seconds+new_trip_stops[i,]$dwell_mod
            
            trip_stops_stack<-rbind(trip_stops_stack,new_trip_stops)
            
            headway_mod<-headway_mod+headway
            if (start_t+headway_mod>=end_t) {
              break
            }
          }
          
          trips_stack<-rbind(trips_stack,data.frame(route_id=ro_id,
                                                    service_id=service_selected, 
                                                    trip_id=unique(trip_stops_stack$trip_id),
                                                    trip_headsign=NA,
                                                    shape_id=NA,
                                                    wheelchair_accessible=0,
                                                    bikes_allowed=0))
          
          route_stack<-rbind(route_stack,data.frame(route_id=ro_id,
                                                    agency_id=NA,
                                                    route_short_name="",
                                                    route_long_name="",
                                                    route_desc="",
                                                    route_type="",
                                                    route_color="FFFFFF",
                                                    route_text_color="000000"))
          
          trip_stops_stack$hops_mod<-NULL
          trip_stops_stack$dwell_mod<-NULL
          
          stop_times<-rbind(stop_times,trip_stops_stack)
          
          trips<-rbind(trips,trips_stack)
          
          routes<-rbind(routes,route_stack)
          
        }
        
      }
      print("Mod: add-trips DONE")
    }
    ########## adjust-dwell-time ##########
    if (mods1$type=="adjust-dwell-time") {
      print("Mod: adjust-dwell-time")
  
      #Get all variables
      if (!is.null(mods1$dwellSecs)) {
        dwell_sec<-as.numeric(mods1$dwellSecs)
      } else {dwell_sec<-FALSE}
      
      if (!is.null(mods1$scale)) {
        dwell_scale<-as.numeric(mods1$scale)
      } else {dwell_scale<-FALSE}
      
      #Make a stop_times copy to filter rows with required routes and trips
      stop_t_work<-stop_times
      
      if (!is.null(mods1$routes)) {
        if (length(mods1$routes)!=0) {
          routes_id<-sapply(mods1$routes,spl.f,USE.NAMES = F)
          p_trips<-trips[trips$route_id %in% routes_id,]$trip_id 
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% p_trips,]
        }
      }
      
      if (!is.null(mods1$patterns)) {
        if (length(mods1$patterns)!=0) {
          patterns_id<-sapply(mods1$patterns,spl.f,USE.NAMES = F)
          routes_from_patterns<-unique(trips[grep(paste(patterns_id, collapse = "|"), trips$trip_id), ]$route_id)
          trip_patterns<-trips[trips$route_id %in% routes_from_patterns, ]$trip_id
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% trip_patterns,]
        }
      }
      
      if (!is.null(mods1$trips)) {
        if (length(mods1$trips)!=0) {
          trips_id<-sapply(mods1$trips,spl.f,USE.NAMES = F)
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% trips_id,]
        }
      }
      
      if (!is.null(mods1$stops)) {
        if (length(mods1$stops)!=0) {
          stops_id<-sapply(mods1$stops,spl.f,USE.NAMES = F)
        } else {stops_id<-FALSE}
      } else {stops_id<-FALSE}
      
      #Get all trips from filtered dataset
      trips_mod_list<-unique(stop_t_work$trip_id)
      
      #Cycle trip_ids
      for (trip_mod in trips_mod_list) {
        #Get stop list
        stops_to_mod<-stop_times[stop_times$trip_id==trip_mod, ]
        #Calculate hops and dwell times for unmodified data
        stops_to_mod$hops<-c(stops_to_mod[2:nrow(stops_to_mod),]$arrival_in_seconds-stops_to_mod[1:nrow(stops_to_mod)-1,]$departure_in_seconds,0)
        stops_to_mod$dwell<-stops_to_mod$arrival_in_seconds-stops_to_mod$departure_in_seconds
        stops_to_mod$dwell_mod<-stops_to_mod$dwell
        stops_to_mod$hops_mod<-stops_to_mod$hops 
        
        #If stops are provided then modify their dwell times. Else modify all dwell times for trip
        if (stops_id!=FALSE) {
          if (dwell_sec!=FALSE) {
            stops_to_mod[stops_to_mod$stop_id %in% stops_id,]$dwell_mod<-dwell_sec
          } else {
            stops_to_mod[stops_to_mod$stop_id %in% stops_id,]$dwell_mod<-stops_to_mod[stops_to_mod$stop_id %in% stops_id,]$dwell*dwell_scale
          }
        } else {
          if (dwell_sec!=FALSE) { 
            stops_to_mod$dwell_mod<-dwell_sec
          } else {
            stops_to_mod$dwell_mod<-stops_to_mod$dwell*dwell_scale
          }
        }
        #Calculate new arrival and departure times
        for (i in 2:nrow(stops_to_mod)) {
          stops_to_mod[i,]$arrival_in_seconds<-stops_to_mod[i-1,]$arrival_in_seconds+stops_to_mod[i-1,]$hops_mod+stops_to_mod[i-1,]$dwell_mod
        }
        stops_to_mod$departure_in_seconds<-stops_to_mod$arrival_in_seconds+stops_to_mod[i,]$dwell_mod
        stops_to_mod$hops<-NULL
        stops_to_mod$hops_mod<-NULL
        stops_to_mod$dwell<-NULL
        stops_to_mod$dwell_mod<-NULL
        
        #Replace old records with modified
        stop_times[(stop_times$stop_id %in% stops_to_mod$stop_id & 
                      stop_times$trip_id %in% stops_to_mod$trip_id),]<-stops_to_mod
        
        
      }
      print("Mod: adjust-dwell-time DONE")
    }
    
    ########## adjust-speed ##########
    
    if (mods1$type=="adjust-speed") {
      print("Mod: adjust-speed")
      
      #Get all variables
      speed_mod<-as.numeric(mods1$scale)
      
      if (!is.null(mods1$scaleDwells)) {
        scaleDwells<-as.logical(mods1$scaleDwells)
      } else {scaleDwells<-FALSE}
      
      #Make a stop_times copy to filter rows with required routes and trips
      stop_t_work<-stop_times
      
      if (!is.null(mods1$hops)) {
        if (length(mods1$hops)!=0) {
          hops_mod<-mods1$hops
        } else {hops_mod<-FALSE}
      } else {hops_mod<-FALSE}
      
      if (!is.null(mods1$routes)) {
        if (length(mods1$routes)!=0) {
          routes_id<-sapply(mods1$routes,spl.f,USE.NAMES = F)
          p_trips<-trips[trips$route_id %in% routes_id,]$trip_id 
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% p_trips,]
        }
      }
      
      if (!is.null(mods1$patterns)) {
        if (length(mods1$patterns)!=0) {
          patterns_id<-sapply(mods1$patterns,spl.f,USE.NAMES = F)
          routes_from_patterns<-unique(trips[grep(paste(patterns_id, collapse = "|"), trips$trip_id), ]$route_id)
          trip_patterns<-trips[trips$route_id %in% routes_from_patterns, ]$trip_id
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% trip_patterns,]
        }
      }
      
      if (!is.null(mods1$trips)) {
        if (length(mods1$trips)!=0) {
          trips_id<-sapply(mods1$trips,spl.f,USE.NAMES = F)
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% trips_id,] 
        }
      }
      
      if (!is.null(mods1$referenceStops)) {
        if (length(mods1$referenceStops)!=0) {
          stops_id<-sapply(mods1$referenceStops,spl.f,USE.NAMES = F)
        }
      }
      
      stop_t_work<-stop_t_work[order(stop_t_work$trip_id),]
      rownames(stop_t_work)<-NULL
      
      #Get all trips from filtered dataset
      trips_mod_list<-unique(stop_t_work$trip_id)
      
      #Cycle trip_ids
      for (trip_mod in trips_mod_list) {
        #Get stop list
        stops_to_mod<-stop_times[stop_times$trip_id==trip_mod, ]
        
          #Calculate hops and dwell times for unmodified data
          stops_to_mod$hops<-c(stops_to_mod[2:nrow(stops_to_mod),]$arrival_in_seconds-stops_to_mod[1:nrow(stops_to_mod)-1,]$departure_in_seconds,0)
          #Calculate modified hops time 
          if (hops_mod==FALSE) {
            stops_to_mod$hops_mod<-round(stops_to_mod$hops*speed_mod)
          }
          else{
            stops_to_mod$hops_mod<-stops_to_mod$hops
            for (hop_pair in hops_mod) {
              for (irow in 1:(nrow(stops_to_mod)-1)) {
                if (stops_to_mod[irow,]$stop_id==spl.f(hop_pair[[1]]) & stops_to_mod[irow+1,]$stop_id==spl.f(hop_pair[[2]]))
                  stops_to_mod[irow,]$hops_mod<-round(stops_to_mod[irow,]$hops*speed_mod)
              }
            }
          }
          stops_to_mod$dwell<-stops_to_mod$arrival_in_seconds-stops_to_mod$departure_in_seconds
          #Calculate modified dwell times
          if (scaleDwells) {
            stops_to_mod$dwell_mod<-stops_to_mod$dwell*speed_mod
          } else {
            stops_to_mod$dwell_mod<-stops_to_mod$dwell
          }
          #Calculate new arrival and departure times
          for (i in 2:nrow(stops_to_mod)) {
            stops_to_mod[i,]$arrival_in_seconds<-stops_to_mod[i-1,]$arrival_in_seconds+stops_to_mod[i-1,]$hops_mod+stops_to_mod[i-1,]$dwell_mod
          }
          stops_to_mod$departure_in_seconds<-stops_to_mod$arrival_in_seconds+stops_to_mod[i,]$dwell_mod
          stops_to_mod$hops<-NULL
          stops_to_mod$hops_mod<-NULL
          stops_to_mod$dwell<-NULL
          stops_to_mod$dwell_mod<-NULL
          
          #Replace old records with modified
          stop_times[(stop_times$stop_id %in% stops_to_mod$stop_id & 
                        stop_times$trip_id %in% stops_to_mod$trip_id),]<-stops_to_mod
        
      }
      print("Mod: adjust-speed DONE")
    }
    
    ########## adjust-frequency ##########
    
    if (mods1$type=="adjust-frequency") {
      print("Mod: adjust-frequency")
      #Get all variables
      if (!is.null(mods1$route)) {
        if (length(mods1$route)!=0) {
          route_mod<-spl.f(mods1$route)
        }
      }
      
      if (!is.null(mods1$entries)) {
        if (length(mods1$entries)!=0) {
          entries<-mods1$entries
        }
      }
      
      if (!is.null(mods1$retainTripsOutsideFrequencyEntries)) {
        if (length(mods1$retainTripsOutsideFrequencyEntries)!=0) {
          retainTripsOutsideFrequencyEntries<-as.logical(mods1$retainTripsOutsideFrequencyEntries)
        } else {retainTripsOutsideFrequencyEntries<-FALSE}
      } else {retainTripsOutsideFrequencyEntries<-FALSE}
      
      stop_times_new<-data.frame()
      trips_stack_new<-data.frame()
      
      #Cycle through entries
      for (entry in entries) {
        sourceTrip<-spl.f(entry$sourceTrip)
        start_t<-entry$startTime
        end_t<-entry$endTime
        headway<-entry$headwaySecs
        week_days<-as.numeric(c(entry$monday,entry$tuesday,entry$wednesday,entry$thursday,entry$friday,entry$saturday,entry$sunday))
        
        #Find matching service
        found_service<-calendar[calendar$monday==week_days[1] &
                                  calendar$tuesday==week_days[2] &
                                  calendar$wednesday==week_days[3] &
                                  calendar$thursday==week_days[4] &
                                  calendar$friday==week_days[5] &
                                  calendar$saturday==week_days[6] &
                                  calendar$sunday==week_days[7],]
        #If not found, create one
        if (is.null(found_service) | nrow(found_service)==0) {
          service_selected<-paste0("SER-",stri_rand_strings(1, 2))
          calendar<-rbind(calendar,data.frame(service_id=service_selected,
                                              monday=week_days[1],
                                              tuesday=week_days[2],
                                              wednesday=week_days[3],
                                              thursday=week_days[4],
                                              friday=week_days[5],
                                              saturday=week_days[6],
                                              sunday=week_days[7],
                                              start_date=0,
                                              end_date=0))
        } else {
          service_selected<-found_service[1,"service_id"]
        }
        #Get info on source trip
        stops_times_mod_data<-stop_times[stop_times$trip_id==sourceTrip,]
        if (nrow(stops_times_mod_data)==0) {next()}
        trip_mod_data<-trips[trips$trip_id==sourceTrip,]
        stops_id<-stops_times_mod_data$stop_id
        hops_mod<-c(stops_times_mod_data[2:nrow(stops_times_mod_data),]$arrival_in_seconds-stops_times_mod_data[1:nrow(stops_times_mod_data)-1,]$departure_in_seconds,0)
        dwell_mod<-stops_times_mod_data$arrival_in_seconds-stops_times_mod_data$departure_in_seconds
  
        tr_id<-sourceTrip
        
        ro_id<-route_mod
        
        #Generate new stop times
        headway_mod<-0
        trip_stops_stack<-data.frame()
        trips_stack<-data.frame()
        trip_n<-0
        while(1) {
          trip_n<-trip_n+1
          new_trip_stops<-data.frame(trip_id=paste0(tr_id,"-",trip_n),
                                     arrival_time=0,
                                     departure_time=0,
                                     stop_id=stops_id,
                                     stop_sequence=0,
                                     shape_dist_traveled=0,
                                     timepoint=0,
                                     arrival_in_seconds=0,
                                     departure_in_seconds=0, 
                                     hops_mod=hops_mod,
                                     dwell_mod=dwell_mod)
          new_trip_stops[1,]$arrival_in_seconds<-start_t+headway_mod 
          for (i in 2:nrow(new_trip_stops)) {
            new_trip_stops[i,]$arrival_in_seconds<-new_trip_stops[i-1,]$arrival_in_seconds+new_trip_stops[i-1,]$hops_mod+new_trip_stops[i-1,]$dwell_mod
          }
          new_trip_stops$departure_in_seconds<-new_trip_stops$arrival_in_seconds+new_trip_stops[i,]$dwell_mod
          
          trip_stops_stack<-rbind(trip_stops_stack,new_trip_stops)
          
          headway_mod<-headway_mod+headway
          if (start_t+headway_mod>=end_t) {
            break
          }
        }
        #Create new trips records
        trips_stack<-rbind(trips_stack,data.frame(route_id=ro_id,
                                                  service_id=service_selected, 
                                                  trip_id=unique(trip_stops_stack$trip_id),
                                                  trip_headsign=NA,
                                                  shape_id=trip_mod_data[1,]$shape_id,
                                                  wheelchair_accessible=trip_mod_data[1,]$wheelchair_accessible,
                                                  bikes_allowed=trip_mod_data[1,]$bikes_allowed))
  
        trip_stops_stack$hops_mod<-NULL
        trip_stops_stack$dwell_mod<-NULL
        
        stop_times_new<-rbind(stop_times_new,trip_stops_stack)
        trips_stack_new<-rbind(trips_stack_new,trips_stack)
   
      }
      #Write new trips
      if (retainTripsOutsideFrequencyEntries==FALSE) {
        trips_to_remove<-trips[trips$route_id==route_mod,]$trip_id
        trips<-trips[!(trips$trip_id %in% trips_to_remove),]
        stop_times<-stop_times[!(stop_times$trip_id %in% trips_to_remove),]
      }
      trips<-rbind(trips,trips_stack_new)
      stop_times<-rbind(stop_times,stop_times_new)
      print("Mod: adjust-frequency DONE")
    }
    
    
    ########## remove-trips ##########
    
    if (mods1$type=="remove-trips") {
      print("Mod: remove-trips")
      
      #Remove trips based on pattenrs
      if (!is.null(mods1$patterns)) {
        if (length(mods1$patterns)!=0) {
            patterns_id<-sapply(mods1$patterns,spl.f,USE.NAMES = F)
            routes_from_patterns<-unique(trips[grep(paste(patterns_id, collapse = "|"), trips$trip_id), ]$route_id)
            trip_patterns<-trips[trips$route_id %in% routes_from_patterns, ]$trip_id
            trips<-trips[!(trips$trip %in% trip_patterns),]
            stop_times<-stop_times[!(stop_times$trip_id %in% trip_patterns),]
          
        }
      }
      
      #Remove trips based on routes
      if (!is.null(mods1$routes)) {
        if (length(mods1$routes)!=0) {
          routes_id<-sapply(mods1$routes,spl.f,USE.NAMES = F)
          p_trips<-trips[trips$route_id %in% routes_id,]$trip_id
          trips<-trips[!(trips$route_id %in% routes_id),]
          stop_times<-stop_times[!(stop_times$trip_id %in% p_trips),] 
        }
      }
      
      #Remove trips based on trips
      if (!is.null(mods1$trips)) {
        if (length(mods1$trips)!=0) {
          trips_id<-sapply(mods1$trips,spl.f,USE.NAMES = F)
          trips<-trips[!(trips$trip_id %in% trips_id),]
          stop_times<-stop_times[!(stop_times$trip_id %in% trips_id),] 
        }
      }
      print("Mod: remove-trips DONE")
    }
    
    ########## remove-stops ##########
    
    if (mods1$type=="remove-stops") {
      print("Mod: remove-stops")
      #Get variables
      saved_sec<-as.numeric(mods1$secondsSavedAtEachStop)
      
      #Make a stop_times copy to filter rows with required routes and trips
      stop_t_work<-stop_times
      
      if (!is.null(mods1$routes)) {
        if (length(mods1$routes)!=0) { 
          routes_id<-sapply(mods1$routes,spl.f,USE.NAMES = F)
          p_trips<-trips[trips$route_id %in% routes_id,]$trip_id
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% p_trips,]
        }
      }
      
      if (!is.null(mods1$patterns)) {
        if (length(mods1$patterns)!=0) {
          patterns_id<-sapply(mods1$patterns,spl.f,USE.NAMES = F)
          routes_from_patterns<-unique(trips[grep(paste(patterns_id, collapse = "|"), trips$trip_id), ]$route_id)
          trip_patterns<-trips[trips$route_id %in% routes_from_patterns, ]$trip_id
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% trip_patterns,]
        }
      }
      
      if (!is.null(mods1$stops)) {
        if (length(mods1$stops)!=0) {
          stops_id<-sapply(mods1$stops,spl.f,USE.NAMES = F)
          stop_t_work<-stop_t_work[stop_t_work$stop_id %in% stops_id,]
          no_stops<-FALSE
        } else {no_stops<-TRUE}
      } else {no_stops<-TRUE}
      #Sort stops by arrival_in_seconds
      stop_t_work<-stop_t_work[order(stop_t_work$arrival_in_seconds),]
      rownames(stop_t_work)<-NULL
      
      
      if (no_stops) {
        #If stops to remove are not defined, then simply remove trips from stop_times
        trips_id<-unique(stop_t_work$trip_id)
        stop_times<-stop_times[!(stop_times$trip_id %in% trips_id),]
      } else {
        #Cycle through stops to remove
        for (i in 1:nrow(stop_t_work)) {
          stop_selected<-stop_t_work[i,]
          #get all stops from trip
          stops_to_mod<-stop_times[stop_times$trip_id==stop_selected$trip_id, ]
          #Just remove stop if it first in trip
          if (min(stops_to_mod$arrival_in_seconds)==stop_selected$arrival_in_seconds) {
            stop_times<-stop_times[!(stop_times$arrival_in_seconds==stop_selected$arrival_in_seconds & 
                                       stop_times$trip_id==stop_selected$trip_id),]
          } else {
            #Modify stop times of trip if removed stop is not first
            time_mod<-stop_selected$departure_in_seconds-stop_selected$arrival_in_seconds+saved_sec
            stops_to_mod<-stops_to_mod[stops_to_mod$arrival_in_seconds>stop_selected$arrival_in_seconds,]
            stops_to_mod$arrival_in_seconds<-stops_to_mod$arrival_in_seconds-time_mod
            stops_to_mod$arrival_time<-to.Date(stops_to_mod$arrival_in_seconds)
            stops_to_mod$departure_in_seconds<-stops_to_mod$departure_in_seconds-time_mod
            stops_to_mod$departure_time<-to.Date(stops_to_mod$departure_in_seconds)
            #remove stop from gtfs dataset
            stop_times<-stop_times[!(stop_times$arrival_in_seconds==stop_selected$arrival_in_seconds & 
                                       stop_times$trip_id==stop_selected$trip_id),]
            #Update stop times with remove stop
            stop_times[(stop_times$stop_id %in% stops_to_mod$stop_id & 
                          stop_times$trip_id %in% stops_to_mod$trip_id),]<-stops_to_mod
            
          }
          
        }
      }
      print("Mod: remove-stops DONE")
    }
    
    ########## reroute ##########
    
    if (mods1$type=="reroute") {
      print("Mod: reroute")
      
      #Make a stop_times copy to filter rows with required routes and trips
      stop_t_work<-stop_times
      
  
      if (!is.null(mods1$fromStop)) {
        fromStop<-spl.f(mods1$fromStop)
      } else {fromStop<-FALSE}
      
      if (!is.null(mods1$toStop)) {
        toStop<-spl.f(mods1$toStop)
      } else {fromStop<-FALSE}
      
      dwellTimes<-mods1$dwellTimes
      hopTimes<-mods1$hopTimes
      stops_pos<-mods1$stops
      
      if (!is.null(mods1$routes)) {
        if (length(mods1$routes)!=0) {
          routes_id<-sapply(mods1$routes,spl.f,USE.NAMES = F)
          p_trips<-trips[trips$route_id %in% routes_id,]$trip_id 
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% p_trips,]
        }
      }
      
      if (!is.null(mods1$patterns)) {
        if (length(mods1$patterns)!=0) {
          patterns_id<-sapply(mods1$patterns,spl.f,USE.NAMES = F)
          routes_from_patterns<-unique(trips[grep(paste(patterns_id, collapse = "|"), trips$trip_id), ]$route_id)
          trip_patterns<-trips[trips$route_id %in% routes_from_patterns, ]$trip_id
          stop_t_work<-stop_t_work[stop_t_work$trip_id %in% trip_patterns,]
        }
      }
      
      stop_t_work<-stop_t_work[order(stop_t_work$trip_id),]
      rownames(stop_t_work)<-NULL
      
      #Get trips to modify
      trips_mod_list<-unique(stop_t_work$trip_id)
      
      #Cycle through trips
      for (trip_mod in trips_mod_list) {
        dwellTimes<-mods1$dwellTimes
        hopTimes<-mods1$hopTimes
        #Get stop times from trip
        stops_to_mod<-stop_times[stop_times$trip_id==trip_mod, ]
        #Calculate hops and dwell times
        stops_to_mod$hops<-c(stops_to_mod[2:nrow(stops_to_mod),]$arrival_in_seconds-stops_to_mod[1:nrow(stops_to_mod)-1,]$departure_in_seconds,0)
        stops_to_mod$hops_mod<-stops_to_mod$hops
        stops_to_mod$dwell<-stops_to_mod$arrival_in_seconds-stops_to_mod$departure_in_seconds
        stops_to_mod$dwell_mod<-stops_to_mod$dwell
        #Cut trip stop times from start if fromStop is set
        if (fromStop!=FALSE) {
          fromID<-which(stops_to_mod$stop_id==fromStop)
          stops_to_mod[fromID,]$hops_mod<-hopTimes[[1]]
          hopTimes<-hopTimes[2:length(hopTimes)]
          stops_to_mod[fromID,]$dwell_mod<-dwellTimes[[1]]
          dwellTimes<-dwellTimes[2:length(dwellTimes)]
        } else {
          firstArrive<-stops_to_mod[1,]$arrival_time
          fromID<-FALSE
        }
        #Cut trip stop times from end if toStop is set
        if (toStop!=FALSE) {
          toID<-which(stops_to_mod$stop_id==toStop)
          stops_to_mod[toID,]$dwell_mod<-dwellTimes[[length(dwellTimes)]]
          dwellTimes<-dwellTimes[1:length(dwellTimes)-1]
        } else {
          toID<-FALSE
          hopTimes[length(hopTimes)+1]<-0
        }
        
        #Get stop ids.
        #For pair of coordinates, find if exact stop coordinates are allready exists and use it's id.
        #if not, then create new stop and find closest parent station to it
        stops_id<-c()
        for (s_pos in stops_pos) {
          if (is.null(s_pos$id) | length(s_pos$id)==0) {
            s_id<-stops[stops$stop_lat==s_pos$lat & stops$stop_lon==s_pos$lon,]$stop_id
            if (is.null(s_id) | length(s_id)==0) {
              while(1) {
                s_id<-paste0("SE-",stri_rand_strings(1, 10))
                if (!(s_id %in% stops$stop_id)) {
                  break
                }
              }
              dist_range<-c()
              parents_stops<-stops[stops$location_type==1,]
              for (sID in 1:nrow(parents_stops)) {
                dist_range<-c(dist_range,distm (c(parents_stops[sID,]$stop_lon,parents_stops[sID,]$stop_lat), c(s_pos$lon, s_pos$lat), fun = distHaversine))
              }
              min_parent<-min(dist_range)
              if (min_parent<500) {
                parent_selected<-parents_stops[which.min(dist_range),]$stop_id
              } else {parent_selected<-NA}
              stops<-rbind(stops,data.frame(stop_id=s_id,stop_code=NA,stop_name="",stop_desc="",stop_lat=s_pos$lat,stop_lon=s_pos$lon,location_type=0,parent_station=parent_selected,wheelchair_boarding=0))
            }
          } else {
            s_id<-spl.f(s_pos$id)
            if (!(s_id %in% stops$stop_id)) {
              while(1) {
                s_id<-paste0("SE-",stri_rand_strings(1, 10))
                if (!(s_id %in% stops$stop_id)) {
                  break
                }
              }
              stops<-rbind(stops,data.frame(stop_id=s_id,stop_code=NA,stop_name="",stop_desc="",stop_lat=0,stop_lon=0,location_type=0,parent_station=NA,wheelchair_boarding=0))
            }
          }
          stops_id<-c(stops_id,s_id)
        }
        
        #Create dataset with new stop to replace
        replace_ds<-data.frame(trip_id= stops_to_mod[1,]$trip_id, arrival_time=0,departure_time=0,stop_id= stops_id ,        
                               stop_sequence=0,shape_dist_traveled=0,timepoint=0,arrival_in_seconds=0,
                               departure_in_seconds=0,hops=0, hops_mod=unlist(hopTimes),dwell=0, dwell_mod=unlist(dwellTimes) ,stringsAsFactors = F)
        #Combine new and old stops
        if (fromID==FALSE) {
          replace_ds[1,]$arrival_time<-firstArrive
        } else {
          replace_ds<-rbind(stops_to_mod[1:fromID,],replace_ds)
        }
        
        if (toID==FALSE) {
          
        } else {
          replace_ds<-rbind(replace_ds,stops_to_mod[toID:nrow(stops_to_mod),])
        }
        #Calulate new arrival and departure times
        for (i in 2:nrow(replace_ds)) {
          replace_ds[i,]$arrival_in_seconds<-replace_ds[i-1,]$arrival_in_seconds+replace_ds[i-1,]$hops_mod+replace_ds[i-1,]$dwell_mod
        }
        replace_ds$departure_in_seconds<-replace_ds$arrival_in_seconds+replace_ds[i,]$dwell_mod
        replace_ds$hops<-NULL
        replace_ds$hops_mod<-NULL
        replace_ds$dwell<-NULL
        replace_ds$dwell_mod<-NULL
        stop_t_work<-stop_t_work[!(stop_t_work$trip_id==replace_ds[1,]$trip_id),]
        stop_t_work<-rbind(stop_t_work,replace_ds)
        
      }
      #Update stop_times GTFS dataset
      stop_times<-stop_times[!(stop_times$trip_id %in% unique(stop_t_work$trip_id)),]
      stop_times<-rbind(stop_times,stop_t_work)
      print("Mod: reroute DONE")
    }
    
  }
  #Return updated times to original format
  stop_times$arrival_time <-sapply(stop_times$arrival_in_seconds,to.Date,USE.NAMES=FALSE)
  stop_times$departure_time <-sapply(stop_times$departure_in_seconds,to.Date,USE.NAMES=FALSE)
  
  arrTimeSplit <- str_split_fixed(stop_times$arrival_time,":",3)
  stop_times$arrival_hour <- as.numeric(arrTimeSplit[,1])
  stop_times$arrival_min <- as.numeric(arrTimeSplit[,2])
  stop_times$arrival_sec <- as.numeric(arrTimeSplit[,3])
  stop_times$arrival_hr <- floor(2*(stop_times$arrival_hour + stop_times$arrival_min/60 + stop_times$arrival_sec/3600))/2
  stop_times$count <- 1
  
  BS_demand_gtfs <- sqldf(
    paste0("SELECT
      stops.parent_station AS station_id,
      stop_times.arrival_hr AS hr,
      SUM(stop_times.count) AS BS_demand
      FROM
      stop_times,
      calendar,
      trips,
      stops
      WHERE
      stop_times.trip_id = trips.trip_id AND
      trips.service_id = calendar.service_id AND
      stop_times.stop_id = stops.stop_id AND 
      calendar.wednesday = 1
      GROUP BY
      stops.parent_station,
      stop_times.arrival_hr;"),
    stringsAsFactors = FALSE,
    row.names = FALSE
  )

return("stops" = stops, "trips" = trips, "stop_times" = stop_times, "BS_demand_gtfs" = BS_demand_gtfs)
