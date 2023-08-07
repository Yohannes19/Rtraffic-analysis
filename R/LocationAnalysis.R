#' Displays the study area for this project
#'
#' This function displays the study area for this data analysis in this case Barcelona.Spain
#'
#' @return Leaflet map with zoom 12 of the study area,barcelona
#' @export
Display_study_area<-function(){
  barcelona_data <- data.frame(lat = c(41.38879), lon = c(2.15899))
  barcelona_sf <- st_as_sf(barcelona_data, coords = c("lon", "lat"), crs = 4326)
  barcelona_map <- mapview(barcelona_sf, map.types = "OpenStreetMap", col.regions = "blue",zoom=12, legend = FALSE)
  barcelona_map
}
#' Displays all the acccidents data
#'
#' This function displays all the accdeints data  with the help of mapview
#'
#' @return All the acccidents data on top of osm
#' @export
Display_allaccidentdata <-function(accidents_data){
  mapview(accidents_data)

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
#' @param data A data frame containing the car accident data
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
     ggtitle("accidents data per hour : {closest_state}")

  # animate the heatmap using gganimate
    animated_heatmap <- animate(heatmap_plot,duration=20,nframes=100,renderer = gifski_renderer())
    #return(animated_heatmap)


  }













