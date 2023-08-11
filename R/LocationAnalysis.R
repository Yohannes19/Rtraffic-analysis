library(sf)
library(leaflet)
#' Displays all the accidents data
#'
#' This function displays all the accidents data  with the help of mapview
#' @param accidents_data A data frame containing the car accident data
#' @return All the acccidents data on top of osm
#' @export
Display_allaccidentdata <-function(accidents_data){
  ggplot(accidents2017) +
    ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0, quiet = TRUE) +

    geom_sf(alpha = 0.1,size=2) +
    theme_minimal()

}
#' Displays the serious injuries occurred in the area
#'
#' This function Displays serious accidents for the car accident data.
#'
#' @param accidents_data A data frame containing the car accident data
#' @return a map with serious accidents and the number of serious accidents in the area
#' @export
Display_Serious_injuries<-function(accidents_data){
  df_serious_injuries <- accidents_data[accidents_data$Serious.injuries > 0, ]
  barcelona_map <- leaflet(data = df_serious_injuries) %>%
    setView(lng = 2.15899, lat = 41.38879, zoom = 11) %>%
    addTiles()

  barcelona_map <- barcelona_map %>%
    addCircleMarkers(color = "red", fillColor = "red", fillOpacity = 0.5, label =~Serious.injuries)
  barcelona_map <- barcelona_map %>%
    addLegend(position = "topright", title = "Serious Accidents", colors = "red", labels = "Serious Injuries")
  study_area_boundary <- accidents_data$geometry

  barcelona_map
}
#' Display the cluster of the traffic car accident data
#'
#' This function displays and calculates clustering the serious injuries and count the number of accidents in the cluster.
#'
#' @param accidents_data A data frame containing the car accident data
#' @return a map displaying the clustering the serious accidents based on the area and the counts of the accidents
#' @export
Display_clusteraccident_serious<-function(accidents_data){

  df_serious_injuries <- accidents_data[accidents_data$Serious.injuries > 0, ]
  barcelona_map <- leaflet() %>%
    setView(lng = 2.15899, lat = 41.38879, zoom = 12) %>%
    addTiles()
  barcelona_map <- barcelona_map %>%
    addCircleMarkers(data = df_serious_injuries, color = "red", radius = 5,
                     clusterOptions = markerClusterOptions())

  barcelona_map
}
extract_lon_lat <- function(geometry_column) {
  coords <- st_coordinates(geometry_column)
  data.frame(lon = coords[, 1], lat = coords[, 2])
}



#' Animated Heatmap for the accidents data for 24 hours
#'
#' This function displays the animated heatmap for each hours in 24 hours.
#'
#' @import gifski
#' @param data_sf A data frame containing the car accident data
#' @return A map that shows the changes of the accident intensities for 24 hours
#' @export
Display_AnimatedHeatMap <- function(data_sf) {

  data_sf$datetime <- as.POSIXct(data_sf$datetime)
  tod_sf <- list()

  # Loop through 24 times
  for (i in 0:23) {
    # Define the filter criteria
    filter_criteria <- i
    hour_sf <- filter(data_sf,as.numeric(format(datetime,"%H")) == filter_criteria)
    hour_sf$datetime <- ymd_hms(sprintf("2017-01-01 %02d:00:00",i))
    hour_sf$datetime <- format(hour_sf$datetime, format = "%H:%M:%S")


    tod_sf[[i+1]] <- hour_sf
  }

  final_sf <- data.frame()
  final_sf <- bind_rows(tod_sf)

  heatmap_plot <- ggplot(data = final_sf, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], fill = after_stat(count), group = datetime)) +

     stat_bin_2d(bins = 20) +
     scale_fill_viridis_c(name = "accidents count", option = "plasma") +
     labs(x = "longitude", y = "latitude") +
     theme(legend.text = element_text(size = 16), plot.title = element_markdown()) +
     transition_states(datetime, transition_length=2,state_length=5) +
     ggtitle("Accidents data per hour : {closest_state}")

  # animate the heatmap using gganimate
    animated_heatmap <- animate(heatmap_plot,duration=20,nframes=100,renderer = gifski_renderer())
    #return(animated_heatmap)
  }

#' Visualize Areas with High Vehicle Involvement in Car Accidents
#'
#' This function creates a leaflet map to visualize areas where car accidents with a high number of vehicles involved have occurred.
#'
#'
#' @param accidents_data An sf data frame containing car accident data
#' @param vehicles_threshold Minimum number of vehicles involved to consider for visualization
#' @return A leaflet map showing markers for areas with high vehicle involvement in accidents
#' @export
perform_high_vehicleinvolvedareas <- function(accidents_data, vehicles_threshold) {
  # Aggregate data to get counts of accidents with the most vehicles involved at each location
  high_vehicles_aggregated <- accidents_data %>%
    filter(Vehicles.involved >= vehicles_threshold) %>%
    group_by(geometry) %>%
    summarise(num_accidents = n())
  # Create the map
  vehicle_map <- leaflet(high_vehicles_aggregated) %>%
    addTiles() %>%
    addMarkers(
      clusterOptions = markerClusterOptions(),
      popup = ~paste("Number of Accidents:", num_accidents)
    )

  # Display the map
  vehicle_map
}











