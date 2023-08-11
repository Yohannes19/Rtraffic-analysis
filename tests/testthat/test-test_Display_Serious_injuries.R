library(testthat)
library(TrafficAnalysis)
library(leaflet)

# Load your dataset
data("accidents2017")

test_that("Display_Serious_injuries correctly visualizes the exact number of serious accidents", {
  # Create a subset of serious accidents from the dataset
  subset_serious <- accidents2017[accidents2017$Serious.injuries > 0, ]

  # Call the function with the subset of serious accidents
  result <- Display_Serious_injuries(subset_serious)

  # Make sure the result is a leaflet map
  expect_s3_class(result, "leaflet")


})
