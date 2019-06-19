library(shiny)
library(leaflet)
library(shinyBS)
library(rhandsontable)
library(shinyjs)
library(data.table)

# Features to add
  # AuthO
    #https://auth0.com/blog/adding-authentication-to-shiny-server/
  # Process raw inputs
    # Use Leaflet plugins
      # http://leafletjs.com/plugins.html
        # See Edit geometries, geoprocessing, DataViz
        # Leaflet.markercluster
        # KML and GPX support: https://github.com/shramov/leaflet-plugins
      # https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
    # make processed data frames reactive only at the end of observer (particularly unprocessed inputs observer)
    # Expand service boardings by station entrances for PB_saturation
    # Calculate accumulated unserved passengers in loading bays
    # Process GTFS, other inputs for all days of week (or atleast DLN, SAB, DOM)
    # Include field in stationInventory to identify vagones and stations adapted for biarticulated buses.
      # Show warning if GTFS + routeData show impossible assignment of biarticulated buses.
      # Identify allowable (and costly) interchanges (sequential station pairs), show warnings when violated.
    # Update PB_saturation with Emme output. --IN PROGRESS
      # See algorithms in Evernote. See transit assignment/loader algorithms.
    # Calculate BS_capacity with dwell time 
      # (departure time - arrival time in stop_times) and infrastructure inventory
    # Add support for Scenario Editor output
    # Add zone input, heatmaps, O-D, Accesibility output from Scenario Editor
  # Visualize saturation data
    # Add graph with alightings by route by station
    # Add arrows to polylines: https://github.com/bbecquet/Leaflet.PolylineDecorator
    # Segment layers
      # Corridor saturation (not by route)
    # Calculate saturation by O-D
      # Create station_times table to calculateodm_capacity
      # Color-code desire lines with same scheme
    # Queue lengths and wait times
  # Recommendations --IN PROGRESS
    # Allow user to add or modify recommendations
    # Allow user to see cost and saturation reduction of improvement scenarios.
    # Show comparison of old vs new saturation 
  # GTFS to JSON scripts
    # For GTFS-RT feed, see https://github.com/harrytruong/gtfs_realtime_json
    # Animate system: https://github.com/vasile/GTFS-viz
    # Visualize routes: https://github.com/cmichi/gtfs-visualizations
    # https://github.com/conveyal/gtfs2json

# Features for route design app

  # With station x station O-D Matrix, show desire lines for top X O-D pairs. Ranking determined by volume and LOS.
  # Translucency of desire lines is proportional to LOS (perhaps relative to auto LOS)
  # LOS matrix calculated with OTP
  # GoogleVis Sankey chart for corridor to corridor flows (demand and supply) to design main lines
  # Arc diagram for displaying O-D flows for a single corredor and designing subroutes
    # http://www.datavizcatalogue.com/methods/arc_diagram.html
    # http://gastonsanchez.com/software/arcdiagram_introduction.pdf
    # Top arcs for demand, bottom arcs for supply
    # Line bubbles for station indicators (boardings, alightings, bus arrivals, saturation, etc)
    # Click on bubbles for pop-up bullet chart
    # As with desire lines, transparency or color of arcs can indicate saturation or LOS

# set operating system
opSys <- "Windows"
if (opSys == "Windows") {
  pathSlash <- "\\"
} else {
  pathSlash <- "/"
}

# Choices for drop-downs

# opSysList <- c('Windows',
#             'macOS',
#             'Linux'
# )

dayList <- c('Sunday' = 'sunday',
             'Monday' = 'monday', 
             'Tuesday' = 'tuesday', 
             'Wednesday' = 'wednesday', 
             'Thursday' = 'thursday', 
             'Friday' = 'friday',
             'Saturday' = 'saturday'
)

serviceList <- c(
  'WKD',
  'SAT',
  'SUN'
)

scenarioList <- c('currentService' = 'currentService'
)

# vars_Color <- c(
#   'Buses in Stations' = 'BS',
#   'Pax in Turnstiles' = 'PT',
#   'Pax in Bays' = 'PB',
#   'Avg. Travel Time' = 'TT_avg'
# )

vars_Color <- c(
  'Buses in Stations' = 'BS',
  'Pax in Turnstiles' = 'PT'
)

# vars_Size <- c(
#   'Demand' = 'demand',
#   'Capacity' = 'capacity',
#   'Total Travel Time' = 'TT_total'
# )

vars_Size <- c(
  'Demand' = 'demand',
  'Capacity' = 'capacity'
)

stationList <- c(
  'Marly' = 1,
  'Portal Norte' = 2
)

routeSegList <- c(
  'A52 | T--2--156' = 'T--2--156',
  'B11 | T--43--629' = 'T--43--629',
  'B12 | T--7--810' = 'T--7--810'
)

geojsonAgencyList <- c(
  'T',
  'Z',
  'D',
  'A'
)

stopsAtList <- stationList

routeStopsList <- c(
  'None',
  'Stops',
  'Stations'
)

oZoneList <- c(
  'USA',
  'KN',
  'BO',
  'KN'
)

dZoneList <- oZoneList

operatorList <- c(
  'Consorcio Express',
  'Etib',
  'Masivo Capital'
)

indicatorList <- c(
  'route_color',
  'ridership',
  'IPB',
  'IPK',
  'completion',
  'headway_am',
  'headway_op',
  'headway_pm'
)

geojsonRouteList <- routeSegList

geojsonStopTypeList <- c(
  'Station' = 'station',
  'Platform' = 'platform',
  'Stop' = 'stop'
)

geojsonStopList <- stationList

plotList <- c(
  'Station Saturation - Bullet' = 'stnSatBG',
  'Station Saturation - Timeline' = 'stnSatTimeline',
  'Route Saturation - Profile' = 'segSatProfile',
  'Boarding Saturation - Bar Chart' = 'boardingSatBC',
  'Corridor Saturation - Profile' = 'corridorSegSatProfile',
  'O-D Demand - Arc' = 'odArcDiagram'
)

studyElementList <- c(
  'Routes' = 'routes',
  'Stops' = 'stops',
  'Stations' = 'stations',
  'Route Segments' = 'routeSeg',
  'Corridors' = 'corridors',
  'O-D Pairs' = 'odPairs'
)

corridorList <- c(LETTERS[1:8], LETTERS[10:13], 'S')

DL_varList <- c(
  'Transit Trips' = 'transit_trips',
  'Auto Trips' = 'auto_trips',
  'Travel Time Ratio' = 'TT_ratio',
  'Travel Time Lost' = 'TT_lost'
)

lineColorsList <- c(
  'red',
  'orange',
  'yellow',
  'green',
  'blue',
  'purple',
  'black'
)

drawingList <- c('', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')

tutorialList <- c(
  'Intro' = '1'#,
  #'Filter Routes' = '2',
  #'Desire Lines' = '3',
  #'Draw Routes'
)

shinyUI(navbarPage('HyperionTransit Beta', id='nav', collapsible = TRUE, inverse = FALSE, 
                   tabPanel('Login', value = 'login',
                            fluidPage(
                              
                              tagList(
                                tags$head(
                                  tags$link(rel='stylesheet', type='text/css', href='www/HTstyles.css')
                                )
                              ),
                              
                              div(class = 'login',
                                  uiOutput('uiLogin'),
                                  textOutput('pass'),
                                  tags$head(tags$style('#pass{color: red;'))
                              ),    
                              
                              fluidRow(
                                column(3,
                                       div(class = 'span1',      
                                           uiOutput('obs')
                                       )
                                ),
                                column(8,
                                       div(class = 'logininfo',
                                           uiOutput('userPanel')
                                       ),
                                       hr(),
                                       div(class = 'DataTable',      
                                           uiOutput('dataTable')
                                       )
                                )
                              ) #,
                            #   selectInput('selectOpSys', 'Operating System',
                            #               choices = opSysList)
                            )
                            ),
                   tabPanel('File I/O', value = 'inputOutput',
                            fluidRow(
                              column(1),
                              column(6,
                                     h2('Input', align = 'center'),
                                     hr(),
                                     fluidRow(
                                       column(1),
                                       column(5,
                                              fluidRow(
                                                column(7,
                                                       h4('Create Scenario')
                                                ),
                                                column(5,
                                                       actionButton('processRaw', 'Import', style='color: #fff; background-color: #337ab7; border-color: #2e6da4')
                                                )
                                              ),
                                              h4(),
                                              fluidRow(
                                                column(6,
                                                       selectInput('dayOfWeek', 'Day of Week', 
                                                                   choices = dayList, 
                                                                   selected = 'wednesday')
                                                ),
                                                column(6,
                                                       sliderInput('timeInterval', 'Time Interval (hours)', min = 0, 
                                                                   max = 1, value = 0.5, step = 0.05)
                                                )
                                              ),
                                              
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputGTFS', 'GTFS',
                                                                 accept=c('application', pathSlash, 'x-compressed',
                                                                          'application', pathSlash, 'x-zip-compressed',
                                                                          'application', pathSlash, 'zip',
                                                                          'multipart', pathSlash, 'x-zip',
                                                                          '.zip'))
                                                ),
                                                column(6,
                                                       fileInput('inputStationInventory2', 'Station Inventory',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputRouteData', 'Route Data',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputStationDrawings', 'Station Drawings',
                                                                 accept=c('application', pathSlash, 'x-compressed',
                                                                          'application', pathSlash, 'x-zip-compressed',
                                                                          'application', pathSlash, 'zip',
                                                                          'multipart', pathSlash, 'x-zip',
                                                                          '.zip'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputConveyalScenario', 'Conveyal Scenario',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputEmme', 'Emme Assignment',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputGeoJSONroutes', 'GeoJSON Routes')
                                                ),
                                                column(6,
                                                       fileInput('inputGeoJSONstops', 'GeoJSON Stops')
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputTDM', 'Transit Demand Matrix',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputTLM', 'Transit LOS Matrix',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputADM', 'Auto Demand Matrix',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputALM', 'Auto LOS Matrix',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputStationSurvey', 'Station Survey',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputRouteSurvey', 'Route Survey',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              )
                                              
                                       ),
                                       column(1),
                                       column(5,
                                              fluidRow(
                                                column(7,
                                                       h4('Upload Scenario')
                                                ),
                                                column(5,
                                                       actionButton('importScenario', 'Import', 
                                                                    style='color: #fff; background-color: #337ab7; border-color: #2e6da4')
                                                )
                                              ),
                                              h4(),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputScenario', 'Scenario',
                                                                 accept=c('application', pathSlash, 'x-compressed',
                                                                          'application', pathSlash, 'x-zip-compressed',
                                                                          'application', pathSlash, 'zip',
                                                                          'multipart', pathSlash, 'x-zip',
                                                                          '.zip'))
                                                ),
                                                column(6,
                                                       fileInput('inputStationInventory1', 'Station Inventory',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputCompleteODM', 'Complete O-D Matrix',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputFilteredODM', 'Filtered O-D Matrix',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputStnData', 'Station Data',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputRouteSegData', 'Route Segment Data',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                       fileInput('inputOnOffData', 'On-Off Data',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                ),
                                                column(6,
                                                       fileInput('inputCorridorData', 'Corridor Data',
                                                                 accept=c('text', pathSlash, 'csv',
                                                                          'text', pathSlash, 'comma-separated-values,text', pathSlash, 'plain',
                                                                          '.csv'))
                                                )
                                              ),
                                              hr(),
                                              fluidRow(
                                                column(7,
                                                       h4('Switch Scenario')
                                                ),
                                                column(5,
                                                       actionButton('switchScenario', 'Import', 
                                                                    style='color: #fff; background-color: #337ab7; border-color: #2e6da4')
                                                )
                                              ),
                                              h4(),
                                              selectInput('selectScenario', 'Scenario', 
                                                          choices = scenarioList),
                                              actionButton('deleteScenario', 'Delete Scenario',
                                                           style='color: #fff; background-color: #A00000; border-color: #900000')
                                              # hr() ,
                                              # h4('Debugging Code'),
                                              # runcodeUI(code = '', type = c('text'), width = NULL,
                                              #           height = NULL, includeShinyjs = TRUE)
                                       )
                                     )
                              ),
                              column(1),
                              column(3,
                                     h2('Output', align = 'center'),
                                     hr(),
                                     fluidRow(
                                       column(7,
                                          h4('Download Scenario')
                                       ),
                                       column(5,
                                              h4(' '),
                                              downloadButton('download_scenario', 'Scenario')
                                       )
                                     ),
                                     h4(),
                                     textInput('scenarioName', 
                                               label = 'Scenario Name', 
                                               value = '', 
                                               placeholder = 'E.g. currentOperation'),
                                     hr(),
                                     h4('Download Tables'),
                                     fluidRow(
                                       column(4),
                                       column(8,
                                              downloadButton('download_stationInventory', 'Station Inventory'),
                                              h4(' '),
                                              downloadButton('download_stnOD_AZ_AH', 'Full O-D Data'),
                                              h4(' '),
                                              downloadButton('download_stnOD_AZ_AH_10', 'Filtered O-D Data'),
                                              h4(' '),
                                              downloadButton('download_stn_AS_AH', 'Station Data'),
                                              h4(' '),
                                              downloadButton('download_seg_AR_AH', 'Route Segment Data'),
                                              h4(' '),
                                              downloadButton('download_onOff_AS_AH', 'On-Off Data'),
                                              h4(' '),
                                              downloadButton('download_corridorSeg_AZ_AH', 'Corridor Data')
                                       )
                                     ),
                                     hr(),
                                     h4('Download Reports'),
                                     fluidRow(
                                       column(4),
                                       column(7,
                                              downloadButton('download_diagnostic', 'Diagnostic'),
                                              h4(' '),
                                              downloadButton('download_investmentPlan', 'Investment Plan')
                                       ),
                                       column(1)
                                     )
                              ),
                              column(1)
                            )
                   ),
  tabPanel('Interactive map', value = 'map',
          div(class='outer',
              
              tags$head(
              # Include custom CSS, JS
                 includeCSS('www/HTstyles.css'),
                 includeScript('gomap.js')
              ),
              
              # CSS I don't understand to format slider
              # http://stackoverflow.com/questions/36138124/control-the-appearance-of-a-sliderinput-in-shiny
              # tags$style(type = 'text', pathSlash, 'css', '
              #            .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
              #            .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
              #            .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
              #            .irs-grid-text {display: none;}
              #            .irs-grid-pol {display: none;}
              #            .irs-max {font-family: 'arial'; color: black;}
              #            .irs-min {font-family: 'arial'; color: black;}
              #            .irs-single {color:black; background:#6666ff;}
              #            .irs-slider {width: 30px; height: 30px; top: 22px;}
              #            '),
              
              leafletOutput('map', width='100%', height='100%'),
              
              absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
                            draggable = TRUE, top = 60, left = 'auto', right = 10, bottom = 'auto',
                            width = 300, height = 'auto',
                            bsCollapse(id = 'controls_collapse', open = 'Controls', 
                                       bsCollapsePanel('Controls',
                                                       sliderInput('timeSlider', 'Time of Day',
                                                                   min = 4,
                                                                   max = 23.5,
                                                                   value = 6,
                                                                   step = 0.5,
                                                                   width = '100%',
                                                                   animate = TRUE,
                                                                   animationOptions(interval = 8000, loop = TRUE)
                                                       ),
                                                       hr(),
                                                       selectInput('studyElement', label = 'Study Element', 
                                                                    choices = studyElementList, 
                                                                    selected = 'stations'
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.studyElement == 'stations'",
                                                         selectizeInput('selectStation', 'Station', 
                                                                        choices = stationList),
                                                         fluidRow(
                                                           column(7,
                                                                  selectInput('colorVar', 'Color Variable', vars_Color, selected = 'BS')    
                                                                  ),
                                                           column(5,
                                                                  selectInput('sizeVar', 'Size Variable', vars_Size, selected = 'demand')
                                                                  )
                                                         ),
                                                         fluidRow(
                                                           column(7,
                                                                  h4('Pax Distribution')
                                                                  ),
                                                           column(5,
                                                                  checkboxInput('attractions', 
                                                                                label = 'Attractions', 
                                                                                value = FALSE) 
                                                                  )
                                                         ),
                                                         sliderInput('inputSpiderPercentile', 
                                                                     label = 'Percentile', 
                                                                     min = 0, max = 100, 
                                                                     value = c(0,50), 
                                                                     step = 5, round = TRUE
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  actionButton('displayStationDemandDistributionCircles', 'Circles')
                                                           ),
                                                           column(6,
                                                                  actionButton('displayStationSpider', 'Spider')
                                                           )
                                                         )
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.studyElement == 'routeSeg'",
                                                         selectInput('selectRouteSeg', 'Route', 
                                                                     choices = routeSegList)
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.studyElement == 'corridors'",
                                                         selectInput('selectCorridorsForSeg', 'Corridors', 
                                                                     choices = corridorList, 
                                                                     multiple = FALSE)
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.studyElement == 'odPairs'",
                                                         selectInput('selectCorridorsForDL', 'Corridors', 
                                                                     choices = corridorList, 
                                                                     multiple = TRUE),
                                                         fluidRow(
                                                           column(6,
                                                                  checkboxInput('includeIntraZonal', 
                                                                                label = 'Intra-zonal', 
                                                                                value = FALSE)
                                                                  ),
                                                           column(6,
                                                                  checkboxInput('unidirectional', 
                                                                                label = 'Unidirectional', 
                                                                                value = FALSE)
                                                                  )
                                                         ),
                                                         sliderInput('desireLinePercentiles', 
                                                                     label = 'Percentile', 
                                                                     min = 0, max = 100, 
                                                                     value = c(0,10), 
                                                                     step = 2, 
                                                                     round = TRUE,
                                                                     post = '%'
                                                         ),
                                                         fluidRow(
                                                           column(7,
                                                                  selectInput('selectDLvar', 'Variable',
                                                                              choices = DL_varList,
                                                                              selected = 'transit_trips')
                                                                  ),
                                                           column(5,
                                                                  selectInput('desireLineColor', 'Color',
                                                                              choices = lineColorsList,
                                                                              selected = 'blue')
                                                                  )
                                                         ),
                                                         fluidRow(
                                                           column(7,
                                                                  actionButton('displayDesireLines', 'Display Lines')
                                                           ),
                                                           column(5,
                                                                  actionButton('clearDesireLines', 'Clear Lines')
                                                           )
                                                         )
                                                         # fluidRow(
                                                         #   column(5,
                                                         #          actionButton('displayTransitDemand', 'Transit Demand')
                                                         #          ),
                                                         #   column(7,
                                                         #          actionButton('displayTTratio', 'Travel Time Ratio')
                                                         #          )
                                                         # ),
                                                         # fluidRow(
                                                         #   column(5,
                                                         #          actionButton('displayTimeLost', 'Time Lost')
                                                         #   ),
                                                         #   column(7,
                                                         #          actionButton('displayDemandRatio', 'Demand Ratio')
                                                         #   )
                                                         # )
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.studyElement == 'routes'",
                                                         fluidRow(
                                                           column(6,
                                                                  selectInput('selectGeoJSONagency', 'Agencies',
                                                                              choices = geojsonAgencyList,
                                                                              multiple = TRUE)
                                                             ),
                                                           column(6,
                                                                  selectInput('selectStopsAt', 'Stops At',
                                                                              choices = stopsAtList,
                                                                              multiple = TRUE)
                                                                  )
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  selectInput('originZone', 'Origin Zone',
                                                                              choices = oZoneList,
                                                                              multiple = TRUE)
                                                                  ),
                                                           column(6,
                                                                  selectInput('destinationZone', 'Destination Zone',
                                                                              choices = dZoneList,
                                                                              multiple = TRUE)
                                                                  )
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  selectInput('operator', 'Operator',
                                                                              choices = operatorList,
                                                                              multiple = TRUE)
                                                           ),
                                                           column(6,
                                                                  selectInput('service_id', 'Service ID',
                                                                              choices = serviceList,
                                                                              multiple = TRUE)
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  selectInput('indicator', 'Indicator',
                                                                              choices = indicatorList,
                                                                              selected = 'route_color',
                                                                              multiple = FALSE)
                                                           ),
                                                           column(6,
                                                                  radioButtons('routeStops', 'Show',
                                                                              choices = routeStopsList,
                                                                              selected = 'None')
                                                           )
                                                         ),
                                                         sliderInput('indicatorPercentiles', 'Indicator Percentiles',
                                                                     min = 0, max = 100, 
                                                                     value = c(0,10), 
                                                                     step = 2, 
                                                                     round = TRUE,
                                                                     post = '%'
                                                         ),
                                                         selectInput('selectGeoJSONroute', 'Routes',
                                                                     choices = geojsonRouteList,
                                                                     multiple = TRUE),
                                                         fluidRow(
                                                           column(6,
                                                                  actionButton('displayGeoJSONroutes', 'Display Routes')
                                                                  ),
                                                           column(6,
                                                                  actionButton('clearGeoJSONroutes', 'Clear Routes')
                                                           )
                                                         )
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.studyElement == 'stops'",
                                                         checkboxGroupInput('selectGeoJSONstopType', 'Stop Types', 
                                                                            choices = geojsonStopTypeList,
                                                                            inline = TRUE),
                                                         selectInput('selectGeoJSONstop', 'Stops',
                                                                     choices = geojsonStopList, 
                                                                     multiple = TRUE)
                                                       ),
                                                       style = 'default'
                                       )
                            )
              ),
              absolutePanel(id = 'plots', class = 'panel panel-default', fixed = TRUE,
                            draggable = TRUE, top = 60, left = 150, right = 'auto', bottom = 'auto',
                            width = 400, height = 'auto',
                            bsCollapse(id = 'plots',
                                      bsCollapsePanel('Station Saturation - Bullet',
                                                      plotOutput('stnSatBG', height = '400', width = '350'),
                                                      #plotOutput('stnSatRadar', height = '400', width = '350'),
                                                      #tableOutput('selectedStationTable'),
                                                      #rHandsontableOutput('selectedStationTable'),
                                                      style = 'default'
                                      ),
                                      bsCollapsePanel('Station Saturation - Timeline',
                                                      plotOutput('stnSatTimeline', height = '500', width = '350'),
                                                      style = 'default'
                                      ),
                                      bsCollapsePanel('Route Saturation - Profile',
                                                      plotOutput('segSatProfile', height = '500', width = '350'),
                                                      style = 'default'
                                      ),
                                      bsCollapsePanel('Boarding Saturation - Bar Chart',
                                                      plotOutput('boardingSatBC', height = '500', width = '350'),
                                                      style = 'default'
                                      ),
                                      bsCollapsePanel('Corridor Saturation - Profile',
                                                      plotOutput('corridorSegSatProfile', height = '500', width = '350'),
                                                      style = 'default'
                                      ),
                                      bsCollapsePanel('Recommendations',
                                                      rHandsontableOutput('recommendations'),
                                                      style = 'default'
                                      )
                            )
              ),
              tags$div(id='cite',
                       'Application developed by Jonathan Hoagland Leape.'
              )
          )
  ),
  tabPanel('Station Design', value = 'stationDesign',
          fluidRow(
            column(6,
              selectizeInput('stationToDraw', 'Station', 
                             choices = stationList)
            ),
            column(6,
              selectInput('drawingFile', 'Drawing', choices = drawingList, selectize = FALSE)    
            )
          ),
          uiOutput('stationDrawing')
  ),
  tabPanel('Plots', value = 'plots',
           fluidRow(
             column(3,
                    selectInput('selectPlot', 'Plot', choices = plotList, selectize = FALSE)
                    )
           ),
           plotOutput('bigPlot', height = '600', width = '100%')
  ),
  navbarMenu('Tables',
    tabPanel('Stations', value = 'stationData', 
             hr(),
             DT::dataTableOutput('stnSatTable')
    ),
    tabPanel('Route Segments', value = 'routeSegmentData', 
             hr(),
             DT::dataTableOutput('routeSegmentTable')
    ),
    tabPanel('Corridor Segments', value = 'corridorSegmentData', 
             hr(),
             DT::dataTableOutput('corridorSegmentTable')
    ),
    tabPanel('O-D Pairs', value = 'odPairData', 
             hr(),
             DT::dataTableOutput('odTable')
    )
  ),
  tabPanel('Tutorials', value = 'tutorials',
          h1("Video Tutorials"),
          selectInput('selectTutorial', 'Select Tutorial',
                      choices = tutorialList,
                      multiple = FALSE),
          uiOutput("tutorials"),
          hr(),
          includeMarkdown("README.md")
  )
))