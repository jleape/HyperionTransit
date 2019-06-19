---
title: "Instrucciones HyperionTransit"
author: "Jonathan H Leape"
date: "10/17/2017"
output:
  html_document: default
  pdf_document: default
---

HyperionTransit
==========

## Summary

HyperionTransit is a web-based application for integrating and visualizing data that transit agencies commonly maintain in order to inform operational, programmatical and infrastructural interventions. It was conceived and developed by Jonathan Hoagland Leape between 2016 and 2017 in Bogotá, Colombia for Transmilenio S.A.

The application processes input data into the following units of analysis: stations, stops, routes, route segments, corridor segments, and origin - destination pairs. Calculated attributes of each unit of analysis are summarized in a interactive map, plots, tables and reports. Processed data can be downloaded and uploaded in order to easily compare.

## Instructions

### 1) Log in

Write Jonathan Leape at jleape@gmail.com to request a username and password.

### 2) File I/O

#### Create Scenario

In this column the user can upload raw inputs to create a new scenario. 

* **Day of Week** - day of week of analysis.

* **Time Interval** - time interval of analysis. This must match the interval used in input matrices.

* **GTFS** - zip file of valid GTFS feed

* **Station Inventory** - csv file with key information about stations. The table must have the following columns:

| station_id | station_id_emme | station_id_transcad | station_name | corridor | station_order | station_lat | station_lon | turnstiles | stops | entrances | BS_capacity | PT_capacity | station_location | bike_parking | connection |

* **Route Data** - csv file with key information about routes. The table must have the following columns:

| route_id | agency_id | linea | ruta | route_short_name | route_long_name | route_desc | typology_id | trip_headsign | circular | emme_id | operator | zone_o | zone_d | ridership | IPB | IPK | completion | headway_am | headway_op | headway_pm | vehicleCapacity | platformUse | outOfService |

* **Station Drawings** - zip file of pdf files of CAD drawings of stations. The name of each pdf file must be the station_id, or a concatenation of station_id and a capital letter (A, B, C, etc) if there is more than one file for the station.

* **Conveyal Scenario** - json file of modifications made to the uploaded GTFS feed using Conveyal's Scenario Editor.

* **Emme Assignment** - txt file of standard output from Emme containing the results of a transit assignment simulation. The transit line IDs in this file must be associated with GTFS route_id in the **Route Data** file.

* **GeoJSON Routes** - geojson file of routes found in the GTFS feed created using [gtfs2geojson python script](https://github.com/vipassana/geojsontools)

* **GeoJSON Stops** - geojson file of stops found in the GTFS feed created using [gtfs2geojson python script](https://github.com/vipassana/geojsontools)

* **Transit Demand Matrix** - csv file of transit demand between stations found in **Station Inventory**. The table must have the following columns:

| O | D | hr | trips |

* **Transit LOS Matrix** - csv file of transit LOS between stations found in **Station Inventory**. The table must have the following columns:

| O | D | hr | travel_time | walk_distance | boardings |

* **Auto Demand Matrix** - csv file of auto demand between stations found in **Station Inventory**. The table must have the following columns:

| O | D | hr | trips |

* **Auto LOS Matrix - csv file of auto LOS between stations found in **Station Inventory**. The table must have the following columns:

| O | D | hr | travel_time |

* **Station Survey** - csv file of observed demand of vehicles serving the station, passengers passing through turnstiles and passengers boarding and alighting vehicles. Not currently supported.

* **Route Survey** - csv file of observed on-off counts by route. Not currently supported.

After uploading files, click **Import** to process the data.

#### Upload Scenario

* **Scenario** - zip file of a complete scenario downloaded from HyperionTransit.

* **Station Inventory** - csv file with key information about stations. Described above in the Create Scenario section.

* **Complete O-D Matrix** - O-D matrix with transit and auto demand and LOS downloaded from HyperionTransit.

* **Filtered O-D Matrix** - O-D matrix filtered to exclude rows with less than 10 transit trips, downloaded from HyperionTransit.

* **Station Data** - working table of station data downloaded from HyperionTransit.

* **Route Segment Data** - working table of route-segment data downloaded from HyperionTransit.

* **On-Off Data** - working table of on-off data downloaded from HyperionTransit.

* **Corridor Data** - working table of corridor segment data downloaded from HyperionTransit.

After uploading a scenario or HyperionTransit working tables, click **Import** to process the data.

#### Switch Scenario

* **Scenario** - select an existing scenario saved on the server.

Click **Switch** to change working tables to the new scenario.

#### Download Scenario

* **Scenario Name** - type a file name for the zip file of the scenario to be downloaded. 

After providing a scenario name, click **Scenario** to download the zip file.

#### Download Tables

* **Station Inventory** - csv file with key information about stations.

* **Complete O-D Matrix** - O-D matrix with transit and auto demand and LOS.

* **Filtered O-D Matrix** - O-D matrix filtered to exclude rows with less than 10 transit trips.

* **Station Data** - working table of station data.

* **Route Segment Data** - working table of route-segment data.

* **On-Off Data** - working table of on-off data.

* **Corridor Data** - working table of corridor segment data.

#### Download Reports

* **Diagnostic** - markdown, html or pdf file summarizing key performance indicators of the scenario. To be implemented.

* **Investment Plan** - markdown, html or pdf file summarizing recommended infrastructure improvements to alleviate saturation. To be implemented.

### 3) Interactive Map

#### Controls

The Controls panel dynamically adapts to only display inputs relevant to the selected study element. It can be dragged if it blocks a visualization on the map and automatically fades when the cursor is not hovering in order to reveal the map behind it.

* **Time of Day** - slider input to determine the time of day of data to be visualized on the map and in plots. This input applies to all study elements. Some study elements only have one time period (Ex. AM peak), and the slider must be set to this time for the data to appear. Click the play button to animate the slider, and the affected visualizations will update accordingly. Press the pause button to stop the animation.

* **Study Element** - select input to determine study element to be visualized on the map and in plots. The remaining section of the control panel will adapt according to the study element selected.

##### Stations

Display stations found in **Station Inventory** thematically.

* **Color Variable** - select the variable used to determine the color of the station circles.

* **Size Variable** - select the variable used to determine the size of the station circles. 

* **Station** - select input to focus on a particular station. Selecting a station will create a pop-up at the station on the map with key indicators. 

* **Spider** the user can draw desire lines to and from the selected station. The user must choose a percentile range in the **Percentile** slider input to determine how many desire lines to display. Checking the attractions box with show desire lines to the selected station. By default, trip productions are displayed. Click **Station Spider** action button to display desire lines.

##### Stops

Display stops found in **GTFS** feed thematically.

* **Stop Types** - checkbox input to select which types of stops found in the GTFS feed to show. Checking **Station** displays a circle for each parent station. Checking **Platform** shows a circle for each stop that is associated with a parent station. Checking **Stop** shows each road-side stop which is not associated with a parent station.

##### Routes

Display routes found in **GTFS** feed thematically.

* **Agencies** - select input to filter routes by agency.

* **Stops At** - select input to filter routes by stops served.

* **Origin Zone** - select input to filter routes by origin zone.

* **Destination Zone** - select input to filter routes by destination zone.

* **Operator** - select input to filter routes by operator.

* **Indicator** - select input to filter routes by indicator and determine mapping theme.

* **Indicator Percentiles** - slider input to determine the number of routes to be displayed according to their ranking for the chosen indicator.

* **Routes** - select input to select specific routes to be mapped. If none are selected, all routes that pass the previous filters will be mapped.

Click the **Display Routes** action button to map routes, and **Clear Routes** to clear them.


##### Route Segments

Display route-segment saturation according to Emme output or observed on-off counts thematically on the map.

* **Route** - select input to choose route.

##### Corridor Segments

Dispay corridor-segment saturation, the sum of demand over the sum of capacity across all routes for each segment, thematically on the map.

* **Corridors** - select input to determine which corridor is shown in the Corridor Saturation - Profile plot.

All corridors will be displayed on the map since they do not conflict spatially. Corridor segments are offset so that saturation can be visualized by direction.

##### O-D Pairs

Display desire lines between origin-destination pairs thematically on the map.

* **Corridors** - select multiple corridors for which desire lines will be displayed.

* **Intra-zonal** - when checked, desire lines connecting stations within the same corridor will be excluded.

* **Unidirectional** - when checked, desire lines will only be shown between corridors in the order that they were input in **Corridors**

* **Percentile** - slider to determine the range of desire lines to be displayed.

* **Variable** - select variable used in filtering and to determine opacity of desire lines. Transit trips and auto trips are in passengers per hour. Travel Time Ratio is the ratio of travel time by transit according to the GTFS feed, including access, transfer and egress times, and travel time by auto. Travel Time Lost is the difference between travel time by transit and travel time by auto, multiplied by the number of transit trips.

* **Color** - select color of desire lines.

* **Display Lines** - add desire lines to map.

* **Clear Lines** - remove desire lines from map.


#### Map

The map is a CartoDB rendering of OpenStreetMaps. The Leaflet library is used to display circles and lines thematically to visualize data. 

The map is automatically centered at the centroid of the stops found in the GTFS feed, and can be dragged. The map can be zoomed by scrolling or by clicking on the +/- buttons in the top left corner. 

Below the zoom buttons is the layer control, which will expand when the cursor hovers over it. HyperionTransit automatically selects which layers to show or hide for each study element, but the layers can be managed manually by checking and unchecking boxes in the layer control.

Below the layer control is the map legend for the themes of the selected study element.

In the bottom left corner is the drawing toolbox. 

* The star button is used to style (change color, opacity or stroke of) drawn lines.

* Below the star button are buttons for drawing polylines, which may represent proposed routes, circles, which may represent areas of influence, and markers, which may represent proposed stations, stops, interchanges, terminals, destinations, etc.

* Below the drawing buttons is a button for editing drawn objects and a button for deleting them.

Currently, objects drawn using this toolbox cannot be downloaded.


#### Plots

The Plots panel includes expandable windows, each containing a graphic relevant to the data filtered via the Control panel. It can be dragged if it blocks a visualization on the map and automatically fades when the cursor is not hovering in order to reveal the map behind it.

* **Station Saturation - Bullet** - bullet chart showing the three types of saturation for the selected station. The colored background matches the theme of the circles on the map. The horizontal black bar indicates the saturation at the selected time. The vertical black line indicates the maximum saturation of the station in the entire day.

* **Station Saturation - Timeline** - line chart of demand and capacity of the selected saturation type for the selected station over the course of the day.

* **Route Saturation - Profile** - line chart of the demand and capacity of the route selected under Route Segments in the Control panel.

* **Boarding Saturation - Bar Chart** - bar chart of boardings and vacancies at the seleted time for each route serving the selected station.

* **Corridor Saturation - Profile** - line chart of demand and capacity of the selected corridor selected under Corridors in the Control panel.

* **Recomendations** - table listing recommended interventions for the selected station, based on the three saturation types. The table is editable so the user can modify the cost, effectiveness and quantity of each intervention. The summarization of these inputs in the Investment Plan output is under development.

### 4) Station Design

The Station Design tab shows CAD drawings of stations so they can be reviewed when considering infrastructure interventions.

* **Station** - select the station.

* **Drawing** - select which drawing of the station to view.

### 5) Plots

The Plots tab displays the same plots found in the Plots panel of the Interactive Map, but larger so they can be studied in detail.

* **Plot** - select which plot to display.

### 6) Tables

Each working table can be viewed in the Tables tab to quickly find statistics. In the top left, the number of entries shown can be modified. In the top right, the user can search for elements of the table. The arrows next to each column header can be clicked to sort the table according the the values in that column. Below each column header is an input box to filter the table by searching in the specific column. Click rows to highlight them.

* **Stations** - Demand, Capacity and Saturation for each type (Buses in Stations, Passengers in Turnstiles, Passenger in Bays) by station by hour.

* **Stops** - to be added.

* **Routes** - to be added.

* **Route Segments** - Boardings and Alightings at beginning of segment and Demand, Capacity and Saturation of each segment by route by hour.

* **Corridor Segments** - demand, capacity and saturation by segment by corridor.

* **O-D Pairs** - Transit Trips, Auto Trips, Transit Travel Time, Auto Travel Time, Travel Time Ratio (ratio of transit travel time to auto travel time), Travel Time Lost (difference of Transit Travel Time and Auto Travel Time multiplied by Transit Trips)

HyperionTransit is property of Jonathan Hoagland Leape, licensed to Transmilenio S.A.
