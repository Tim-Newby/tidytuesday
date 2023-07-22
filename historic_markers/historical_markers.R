library(tidytuesdayR)
library(tidyverse)


# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest


tuesdata <- tidytuesdayR::tt_load('2023-07-04')
historical_markers <- tuesdata$`historical_markers`
no_markers <- tuesdata$`no_markers`


# add column n = number of markers in each state
# change state names to lower case

markers_in_state <- historical_markers |> 
  mutate(region = str_to_lower(state_or_prov)) |> 
  group_by(region) |> 
  summarise(n = n())


## get map data for states and merge markers_in_state
centroids <- data.frame(region = tolower(state.name), state.abb, c_long = state.center$x, c_lat = state.center$y)
 labels <- merge(centroids, markers_in_state, by = "region") |> 
   filter(!state.abb == "HI" & !state.abb == "AK")
usa_markers <- map_data("state") |>
  left_join(markers_in_state)

 
  




#plot map of usa and points of historical interest, removing geographic outliers
  ggplot(usa_markers, aes(x = long, y = lat, group = group, alpha = n))  +
    geom_polygon(fill = "turquoise", colour = "grey") + 
    with(labels, 
         annotate(geom="text", x = c_long, y = c_lat, label = state.abb, 
                  size = 3,color = "black")
    ) +
    scale_alpha_binned(breaks = c(100, 1000, 10000), range = c(0.1, 1), labels = c("100", "1000" , "10000")) + 
    labs(title = "Texans love their history - but what's happening between Florida and Wisconsin? ",
         subtitle = "Number of historic markers (n) in the USA by state",
         caption = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-07-04",
         alpha = '  n' ) +
    theme_classic() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.line.x = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          legend.position = c(0.9, 0.2))
    
    

  
