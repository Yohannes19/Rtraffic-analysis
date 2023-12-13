---
title: "Traffic Accident Data Analysis for Barcelona in 2017"
author: Yohannes Abrha Mulaw
date: "13/08/2023"
output: 
   rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Traffic Accident Data Analysis}
  
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
##### github: https://github.com/Yohannes19/Rtraffic-analysis
#####  email: ymulaw@uni-muenster.de/joeabrha@gmail.com


```{r, include = FALSE,options(warn=0)}
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align='center',fig.width=8, fig.height=5,
  comment = "#>"
)

```
## Introduction
Traffic accidents are a significant concern in urban areas, often resulting in injuries, fatalities, and property damage. Understanding the factors that contribute to accidents and identifying patterns can provide valuable insights for improving road safety measures.

In this vignette, we focus on a dataset that contains information about accidents managed by the local police in the city of Barcelona during the year 2017. The dataset includes details such as the number of injuries by severity, the number of vehicles involved, the accident date, and the geographic location.

## Motivation

The analysis of traffic accident data is essential for several reasons:

- **Safety Improvement:** By identifying accident-prone areas and factors contributing to accidents, city planners and authorities can implement targeted safety measures to reduce the risk of accidents.

- **Resource Allocation:** Understanding the distribution of accident severity and the types of vehicles involved can help allocate emergency response resources effectively.

- **Policy Decision:** Insights from this analysis can inform policy decisions related to road infrastructure, traffic management, and public safety.


## Data Pre-Processing:
In the pre-processing phase, the traffic accident data for the year 2017 was loaded from the "accidents_2017.csv" file and converted into a spatial data frame by incorporating longitude and latitude coordinates with a specified coordinate reference system. Unnecessary columns like "District.Name," "Neighborhood.Name," and "Part.of.the.day" were removed to optimize the dataset.

The addition of a constant "Year" column with a value of 2017 and converting the "Month" column into numeric representation using a predefined dictionary ensured consistent time representation. A new "datetime" column was created by combining "Day," "Month," "Year," and "Hour" to facilitate time-based analysis. Redundant "Month," "Day," "Hour," "Year," and "Weekday" columns were eliminated. Additionally, duplicate records were removed, resulting in a refined and structured dataset, poised for further in-depth analysis of accident patterns and trends.



```{r warning=FALSE,message=FALSE}
#loading the necessary libraries
library(TrafficAnalysis)
library(sf)
library(leaflet)
library(tidyverse)
library(gganimate)
library(gifski)
library(ggtext)
```

## Dataset Overview

The dataset includes various attributes about each accident, such as:
- **Unique ID of the accident**
- **Number of injuries by severity**
- **Number of vehicles involved**
- **Street Name**
- **Number of serious and Mid injuries**
- **Date of the accident**
- **Geographic location**

Lets take look at the first 6 rows of the dataset
```{r message=FALSE }
head(accidents2017)
```
## Summary Stastics
This section provides the overall overview of the accidents data sets and manipulation to get some general insights about the dataset.
```{r summary_stats_chunk, results='asis'}
# Call the calculate_summary_stats function
summary_stats <- calculate_summary_stats(accidents2017)

```

```{r echo=FALSE}
cat("### Brief Overview of the accidents2017 dataset\n")
cat("- Total number of accidents: ", summary_stats$TotalAccidents, "\n")
cat("- Total number of mild injuries: ", summary_stats$TotalMildInjuries, "\n")
cat("- Total number of serious injuries: ", summary_stats$TotalSeriousInjuries, "\n")
cat("- Minimum number of victims in an accident: ", summary_stats$minVictims, "\n")
cat("- Maximum number of victims in an accident: ", summary_stats$maxVictims, "\n")
cat("- Maximum number of vehicles involved in an accident: ", summary_stats$MaxVehiclesInvolved, "\n")
cat("- Minimum number of vehicles involved in an accident: ", summary_stats$MinVehiclesInvolved, "\n")
cat("- Average number of victims per accident: ", summary_stats$AvgVictimsPerAccident, "\n")
cat("- Proportion of mild injuries among total victims: ", summary_stats$PropMildInjuries, "\n")
cat("- Proportion of serious injuries among total victims: ", summary_stats$PropSeriousInjuries, "\n")
cat("- Average number of vehicles involved per accident: ", summary_stats$AvgVehiclesPerAccident, "\n")
```


## Monthly Distrbution of the Accidents

```{r perform_monthly_analysis_chunk, results='asis'}
#This function visulizes the number of accidents in each months
perform_monthly_analysis(accidents2017)

```

The observed decrease in the number of accidents during August and December can be attributed to seasonal variations in traffic patterns. In August, the 'summer vacation effect' likely leads to fewer people commuting to work, resulting in reduced traffic volume and a lower likelihood of accidents. Similarly, in December, festive holidays and social gatherings may lead to decreased work-related travel and fewer vehicles on the road during specific days or hours. While weather conditions and law enforcement efforts may also influence accident rates during these months, the overall trend highlights the importance of considering seasonal factors in road safety measures. Policymakers and traffic safety experts should remain vigilant in adapting strategies to ensure road users' well-being during periods of varying traffic activity.

## Weekly Distribution of the accidents
Once the monthly accident analysis is complete, it becomes crucial to examine the distribution of accidents throughout the weekdays. This analysis aims to discern underlying patterns governing the occurrence of accidents. 

```{r  }
data<-perform_weekdays_analysis(accidents2017)
  ggplot(data, aes(x = day_of_week, y = num_accidents)) +
      geom_bar(stat = "identity", fill = "magenta", alpha = 0.5) +
      labs(title = "Accidents in Barcelona in 2017",
           x = "Day of the week",
           y = "Number of accidents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
 
```

The analysis of the weekly distribution of accidents provides interesting insights into how accident rates vary based on weekdays. It's evident that the number of accidents is notably lower during the weekends (Saturday and Sunday). This observation aligns with the understanding that fewer people are commuting to work or engaging in daily routines on weekends, leading to reduced traffic volume and a lower likelihood of accidents.

Conversely, the relatively higher accident rate on Fridays can be attributed to specific behavioral factors. Fridays often mark the end of the workweek and are associated with increased social activities and gatherings. As more individuals engage in social events, parties, and celebrations, the likelihood of accidents occurring may rise.

These findings underscore the influence of human behavior and activities on accident patterns. Policymakers and road safety experts should consider these insights when designing strategies to reduce accidents and promote safer driving behaviors.



From the above information, Sunday is the day where the lowest accident recorded and Fridays is the highest accident recorded, so it make sense to see the number of each accidents per each days of the year. 

```{r }
accidents_data <-perform_eachday_year(accidents2017)
    p <- ggplot(accidents_data, aes(x = date, y = num_accidents)) +
      geom_line(color = "blue") +
      geom_point(data = filter(accidents_data, weekdays(date) == "Sunday"), aes(color = "Sunday", shape = "Sunday"), size = 3, show.legend = TRUE) +
      geom_point(data = filter(accidents_data, weekdays(date) == "Friday"), aes(color = "Friday", shape = "Friday"), size = 3, show.legend = TRUE) +
      labs(title = "Accidents in Barcelona for each day of the year 2017",
           x = "Date",
           y = "Number of accidents per day",
           color = "Day of the Week",
           shape = "Day of the Week") +
      scale_color_manual(values = c("Sunday" = "green", "Friday" = "red")) +
      scale_shape_manual(values = c("Sunday" = 16, "Friday" = 17)) +
      theme_minimal()
    # Print the plot
    print(p)
```





## Mid - Serious Injuries 
Using a pie chart, its able to simply and effectively illustrate the percentage of both minor and serious injuries as follows:
```{r}
#Performing the percentage of the accidents using pie chart
perform_percentage_for_mid_serious(accidents2017)
```

According to the plot, just 2% of the injuries are considered to be significant injuries. Even though the majority of injuries sustained in car accidents were very minor, it would be interesting to investigate the conditions (time, date, and place) in which more severe injuries are more likely to occur.


## Location Based Analysis
The best way to visualize and analyze spatial data is by using maps, such leaflet and map view. so at first we can easily generate amap of the study area with all the accidents data visualized.

### Visulizing all the accidents data
```{r}
Display_allaccidentdata(accidents2017)

```



### Serious Injuries visulization
The following map shows the accidents where serious injuries were caused, displaying the number of serious injuries with a popup label.
```{r}
#Location based Analysis
Display_Serious_injuries(accidents2017)
```



### Clustering serious accidents based on their location

This function clusters the accidents ,The provided plot illustrates car accidents involving victims with severe injuries, similar to the previous depiction. However, in this instance, the accidents are organized into distinct clusters.

```{r}
Display_clusteraccident_serious(accidents2017)
```

This Function provides a visual representation of areas where car accidents with a high number of vehicles involved have occurred. This visualization can offer valuable insights to various stakeholders, including traffic analysts, urban planners, and policymakers. 


```{r perform_highvehicle_chunk, results='asis'}

perform_high_vehicleinvolvedareas(accidents2017,14)

```

## Hotspot analysis

In this section,by leverging the package called sfhotspot the hotspot analysis is performed, some of the functions or methods from the package sfhotspot are:
- **hotspot_count():**	Count the number of points in each cell of a regular grid. Cell size can be set by the user or chosen automatically.

- **hotspot_kde():** Estimate kernel density for each cell in a regular grid. Cell size and bandwidth can be set by the user or chosen automatically.

- **hotspot_gistar():** Calculate the Getis–Ord Gi* statistic for each cell in a regular grid, while optionally estimating kernel density. Cell size, bandwidth and neighbour distance can be set by the user or chosen automatically.

```{r }
#The function hotspot_count()  produces an SF object with counts for the number of points in (by default) each cell in a grid of cells.
#based on the output ,the n represents the number of accidents per grid cell
hotspotcount <- perform_hotspot_count(accidents2017)
ggplot() +
 ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0, quiet = TRUE) +
  geom_sf(
    mapping = aes(fill = n),
    data = hotspotcount,
    alpha = 0.75,
    colour = NA
  ) +
  scale_fill_distiller(direction = 1)
```
As shown in the output from the above function, the lots of accidents are occured in the middle part of the city. 

### hotspot_kde
This fucntion helps to visualize the hotspots by showing only those cells that have significantly more points than expected by chance. For those cells, show the estimated  density of accidents.
```{r message=FALSE,warning=FALSE}

output <- capture.output({
results<-Perform_hotspot_gistar(accidents2017)
results %>% 
  filter(gistar > 0, pvalue < 0.05) %>% 
  ggplot(aes(colour = kde, fill = kde)) +
  geom_sf() +
  scale_colour_distiller(aesthetics = c("colour", "fill"), direction = 1) +
  labs(title = "Density of Accidents in Barcelona, 2017") +
  theme_void()
})
clean_output <- output[!grepl("^Done:", output)]

# Print the cleaned output
#cat(clean_output, sep = "\n")

```

This function can be used to calculate kernel density estimates for each cell in a grid. The kernel density estimation (KDE) can be customised using the bandwidth and bandwidth_adjust arguments.If you do not specify any optional arguments,   hotspot_kde() will try to choose reasonable default values. 

The KDE(kernel density estimation) algorithm requires projected co-ordinates (i.e. not longitudes and latitudes), so we must first transform the data to use an appropriate local projected co-ordinate system.
Based on output from kernel density estimationa  Accidents are more concentrated on the central part of barcelona, which make sense that most of the traffic jams are occured.
```{r message=FALSE,warning=FALSE}

output1 <- capture.output({

  results <- perform_hotspot_kde(accidents2017)
  
  ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0, quiet = TRUE) +
    geom_sf(
      mapping = aes(fill = kde),
      data = results,
      alpha = 0.75,
      colour = NA
    ) +
    scale_fill_distiller(direction = 1)
})

# Filter out lines containing the progress information
clean_output <- output[!grepl("^Done:", output1)]

# Print the cleaned output
#cat(clean_output, sep = "\n")
```

### Hourly Accident Distribution Analysis for 24 hours

By examining the hourly distribution of accidents throughout the day, we can gain insights into how the number of accidents varies over different hours.

```{r gganimate_chunk, animation.fun = gifski_renderer()}
#to display the animated heatmap for the 
library(gifski)
library(gganimate)
library(ggtext)

#animation::use_renderer(gifski_renderer())
animated_heatmap<-Display_AnimatedHeatMap(accidents2017)

anim_save("heatmap_animation.gif", animation = animated_heatmap,
            ani.width = 800, ani.height = 600,
           title = "Accidents Heatmap Animation")
#my_options <- list(ani.width = 800, ani.height = 600)

knitr::include_graphics("heatmap_animation.gif")

```

The analysis of the hourly distribution of accidents reveals interesting patterns in how accident rates change over the course of a day. As shown in the plot, the number of accidents increases starting from 8 hours and remains relatively high until 21 hours, after which it starts to decrease. This observation suggests that there are specific hours during which accidents are more likely to occur.

The increase in accidents during the morning and afternoon hours may be attributed to higher traffic volume as people commute to work and run errands. The gradual decrease in accidents during the evening hours could be due to reduced traffic activity and decreased commuting.
