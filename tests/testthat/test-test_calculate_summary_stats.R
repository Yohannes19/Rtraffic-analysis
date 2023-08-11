library(testthat)
library(TrafficAnalysis)

# Load your dataset
data("accidents2017")

test_that("calculate_summary_stats computes summary statistics correctly", {
  result <- calculate_summary_stats(accidents2017)

  expect_equal(result$TotalAccidents, nrow(accidents2017))
  expect_equal(result$TotalMildInjuries, sum(accidents2017$Mild.injuries))
  expect_equal(result$TotalSeriousInjuries, sum(accidents2017$Serious.injuries))
  expect_equal(result$minVictims, min(accidents2017$Victims))
  expect_equal(result$maxVictims, max(accidents2017$Victims))
  expect_equal(result$MaxVehiclesInvolved, max(accidents2017$Vehicles.involved))
  expect_equal(result$MinVehiclesInvolved, min(accidents2017$Vehicles.involved))
  expect_equal(result$TotalVictims, sum(accidents2017$Victims))

  # Calculate the expected average victims per accident manually
  expected_avg_victims_per_accident <- sum(accidents2017$Victims) / nrow(accidents2017)
  expect_equal(result$AvgVictimsPerAccident, expected_avg_victims_per_accident)

  # Calculate the expected proportion of mild injuries manually
  expected_prop_mild_injuries <- sum(accidents2017$Mild.injuries) / sum(accidents2017$Victims)
  expect_equal(result$PropMildInjuries, expected_prop_mild_injuries)

  # Calculate the expected proportion of serious injuries manually
  expected_prop_serious_injuries <- sum(accidents2017$Serious.injuries) / sum(accidents2017$Victims)
  expect_equal(result$PropSeriousInjuries, expected_prop_serious_injuries)

  # Calculate the expected average vehicles per accident manually
  expected_avg_vehicles_per_accident <- mean(accidents2017$Vehicles.involved)
  expect_equal(result$AvgVehiclesPerAccident, expected_avg_vehicles_per_accident)

})




