library(testthat)
library(TrafficAnalysis)

# Load your dataset
data("accidents2017")
#hether the output of the Display_allaccidentdata function is a ggplot object
test_that("Display_allaccidentdata produces a ggplot object", {
  result <- Display_allaccidentdata(accidents2017)
  expect_s3_class(result, "gg")
})

