# Create a base map (no data)
ggplot() +
  borders(
    database = "world",
    colour = "grey60",
    fill = "grey90") +
  ggtitle("Base Map of the World") +
  xlab("") +
  ylab("") +
  theme(
    panel.background = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())


# loading data and checking latitude and logitude ranges in data

hp <- read.csv("kc_house_data.csv", na.strings = "")
summary(hp$lat)
summary(hp$long)
library(ggplot2)

# Create a dot density map
ggplot(
 data = hp) +
  borders(
    database = "world",
    colour = "grey60",
    fill = "grey90")+
  
  geom_point(           # Use 'geom_ploygon(' for choropleth instead of geom_point(
    aes(
      x = long,
      y = lat)) +
  
  coord_cartesian(                 # used just to zoom in the map
    xlim = c(-122.4, -121.2), 
    ylim = c(47.1, 47.8)) +
  
  ggtitle("Houses King county") +        # for theme
  xlab("") +
  ylab("") +
  theme(
    panel.background = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())


