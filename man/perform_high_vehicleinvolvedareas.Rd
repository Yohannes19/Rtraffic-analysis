% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LocationAnalysis.R
\name{perform_high_vehicleinvolvedareas}
\alias{perform_high_vehicleinvolvedareas}
\title{Visualize Areas with High Vehicle Involvement in Car Accidents}
\usage{
perform_high_vehicleinvolvedareas(accidents_data, vehicles_threshold)
}
\arguments{
\item{accidents_data}{An sf data frame containing car accident data}

\item{vehicles_threshold}{Minimum number of vehicles involved to consider for visualization}
}
\value{
A leaflet map showing markers for areas with high vehicle involvement in accidents
}
\description{
This function creates a leaflet map to visualize areas where car accidents with a high number of vehicles involved have occurred.
}
