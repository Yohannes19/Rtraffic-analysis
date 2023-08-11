#' Perform hotspot analysis for the car accident
#'
#' This function calculates the Getis–Ord Gi* statistic for each cell in a regular grid, while optionally estimating kernel density..
#'
#' @param data A data frame containing the car accident data
#' @return Accidents data with calculated density
#' @export
Perform_hotspot_gistar <-function(data){
  accidentsdata1 <- sf::st_transform(data,32615)
  accidents<-sfhotspot::hotspot_gistar(accidentsdata1)
  return(accidents)
}

#' Perform hotspot analysis of counting the number of points in each cell grid
#'
#' Count the number of points in each cell of a regular grid. Cell size can be set by the user or chosen automatically.
#'
#' @param data A data frame containing the car accident data
#' @return A list of hotspot counts
#' @export
perform_hotspot_count <- function(data){
  accidents <-sfhotspot::hotspot_count(data)
  return(accidents)

}

#' Perform kernel density estimation  hotpots analysis
#'
#' Estimate kernel density for each cell in a regular grid. Cell size and bandwidth can be set by the user or chosen automatically.
#'
#' @param data A data frame containing the car accident data
#' @return A named list of summary statistics
#' @export
perform_hotspot_kde <-function(data){
  accidents <-data |>sf::st_transform("EPSG:2843") |> sfhotspot::hotspot_kde()
  return(accidents)
}
