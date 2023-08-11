library(testthat)
library(TrafficAnalysis)

# Load your dataset
data("accidents2017")

test_that("Display_clusteraccident_serious correctly clusters serious accidents on the map", {
  result <- Display_clusteraccident_serious(accidents2017)

  # Make sure the result is a leaflet map
  expect_s3_class(result, "leaflet")


})
