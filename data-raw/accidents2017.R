library(sf)
library(tidyverse)
## code to prepare `accidents2017` dataset goes here

myfile<-system.file("extdata","accidents_2017.csv",package = "TrafficAnalysis")

#reading csv file
accidents2017 <-read.csv(myfile,stringsAsFactors = FALSE, encoding = "UTF-8")

accidents2017 <- st_as_sf(accidents2017, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )

#removing the unnecessary columns
accidents2017 <- select(accidents2017, -c("District.Name", "Neighborhood.Name", "Part.of.the.day"))

accidents2017$Year<-rep(2017,nrow(accidents2017));
unique_months <- unique(accidents2017$Month)
month_dict <- c("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6,
                "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12)
accidents2017$Month <- month_dict[accidents2017$Month]

#creating list
accidents2017$datetime <- as.POSIXct(paste(accidents2017$Day, accidents2017$Month,accidents2017$Year, accidents2017$Hour, sep = "-"), format = "%d-%m-%Y-%H")

#removing the columns after we combine the data
accidents2017 <- select(accidents2017,-c("Month","Day","Hour","Year","Weekday"))

#removing the duplicates
accidents2017<-distinct(accidents2017)
accidents2017 <- accidents2017 %>%
  filter(Serious.injuries > 0 | Mild.injuries > 0 | Vehicles.involved > 0)
#to create final data.rda file  in data folder
usethis::use_data(accidents2017, overwrite = TRUE)
