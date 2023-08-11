
#' Calculate summary statistics for car accident data
#'
#' This function calculates various summary statistics for the car accident data.
#'
#' @param data A data frame containing the car accident data
#' @return A named list of summary statistics
#' @export
  calculate_summary_stats <- function(data) {
  total_accidents <- nrow(data)
  min_victims <- min(data$Victims)
  max_victims <- max(data$Victims)
  max_vehicles_involved <- max(data$Vehicles.involved)
  min_vehicles_involved <- min(data$Vehicles.involved)
  total_victims <-sum(data$Victims)
  sum_serious_injuries<-sum(data$Serious.injuries)
  sum_mild_injuries<-sum(data$Mild.injuries)
  avg_victims_per_accident <- total_victims / total_accidents
  prop_mild_injuries <- sum_mild_injuries / total_victims
  prop_serious_injuries <- sum_serious_injuries / total_victims
  avg_vehicles_per_accident <- mean(data$Vehicles.involved)


  summary_stats <- list(
    TotalAccidents = total_accidents,
    TotalMildInjuries=sum_mild_injuries,
    TotalSeriousInjuries=sum_serious_injuries,
    minVictims = min_victims,
    maxVictims = max_victims,
    MaxVehiclesInvolved = max_vehicles_involved,
    MinVehiclesInvolved = min_vehicles_involved,
    TotalVictims = total_victims,
    AvgVictimsPerAccident = avg_victims_per_accident,
    PropMildInjuries = prop_mild_injuries,
    PropSeriousInjuries = prop_serious_injuries,
    AvgVehiclesPerAccident = avg_vehicles_per_accident
  )
  return(summary_stats)
  }

  #' Perform Monthly Analysis on Car Accident Data
  #'
  #' This function performs monthly analysis on car accident data, enabling you to explore accident patterns on a monthly basis.
  #'
  #' @param accidents_data A data frame containing car accident data with a 'datetime' column representing the date and time of each accident.
  #'
  #' @return A summarized data frame providing insights into the number of accidents that occurred in each month.
  #'
  #' @export
   perform_monthly_analysis <- function(accidents_data) {
    accidents_data$datetime <- as.POSIXct(accidents_data$datetime)

    # Extract month from the 'datetime' column
    accidents_data$month <- month(accidents_data$datetime)

    # Count the number of accidents per month
    accidents_month <- accidents_data %>%
      group_by(month) %>%
      summarize(num_accidents = n())

    # Replace the month integers by month names
    month_names <- month.name[1:12]
    accidents_month$month <- factor(month_names[accidents_month$month],levels = month_names)

 # print(accidents_month)
  p <- ggplot(accidents_month, aes(month,num_accidents)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.5,position="identity") +
    labs(title = "Accidents in Barcelona in 2017",
         x = "Month",
         y = "Number of accidents") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #scale_x_discrete(labels = month_names)  # Replace the x-axis labels with month names

  # Set the figure size
  options(repr.plot.width = 12, repr.plot.height = 7)
  print(p)
  #return(accidents_month)
  }


  #' Perform time analysis on car accident data in the weekdays
  #'
  #' This function performs time analysis on the car accident data, allowing you to analyze the accidents
  #' based on different time dimensions.
  #'
  #' @param accidents_data A data frame containing the car accident data
  #' @return A data frame summarizing the accidents based on the chosen time dimension
  #' @export
  perform_weekdays_analysis <- function(accidents_data) {
    accidents_data$datetime <- as.POSIXct(accidents_data$datetime)

    # Extract day of the week from the 'datetime' column (0: Sunday, 1: Monday, ..., 6: Saturday)
    accidents_data$day_of_week <- weekdays(accidents_data$datetime)

    # Count the number of accidents per day of the week
    accidents_day <- accidents_data %>%
      group_by(day_of_week) %>%
      summarize(num_accidents = n())

    accidents_day$day_of_week <- as.character(accidents_day$day_of_week)


    # Replace the day integers by day names
    day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    accidents_day$day_of_week <- factor(accidents_day$day_of_week, levels = day_names)
    return(accidents_day)


  }





  #' Perform time analysis on car accident data in the weekdays
  #'
  #' This function performs time analysis on the car accident data, allowing you to analyze the accidents
  #' based on different time dimensions.
  #'
  #' @param accidents_data A data frame containing the car accident data
  #' @return A data frame summarizing the accidents based on the chosen time dimension
  #' @export
  perform_eachday_year<-function(accidents_data){
    accidents_data$datetime <- as.POSIXct(accidents_data$datetime)

    # Count the number of accidents per day of the year
    accidents <- accidents_data %>%
      group_by(date = as.Date(datetime)) %>%
      summarize(num_accidents = n())

  # print(accidents)
return(accidents)

  }

  #' Perform accident type analysis for comparing the mid and serious injuries
  #'
  #' This function performs accident type analysis on the car accident data and visualizes the percentage of mild and serious injuries using a pie chart.
  #'
  #' @param accidents_data A data frame containing the car accident data
  #' @return A pie chart visualizing the percentage of mild and serious injuries
  #' @export
  perform_percentage_for_mid_serious<-function(accidents_data){
    # Compute the sum of mild injuries and serious injuries
    injuries <- accidents_data %>%
      summarise(mild_injuries = sum(`Mild.injuries`),
                serious_injuries = sum(`Serious.injuries`))

    # Create a new data frame for plotting
    injuries_data <- data.frame(
      Injury_Type = c("Mild Injuries", "Serious Injuries"),
      Count = c(injuries$mild_injuries, injuries$serious_injuries)
    )

    # Calculate the percentage and round to one decimal place
    injuries_data$Percentage <- round((injuries_data$Count / sum(injuries_data$Count)) * 100, 1)

    # Create the pie chart with percentages inside
    p <- ggplot(injuries_data, aes(x = "", y = Percentage, fill = Injury_Type)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "bottom") +
      geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5))

    # Set the figure size
    options(repr.plot.width = 7, repr.plot.height = 7)

    # Print the plot
    print(p)

}




















