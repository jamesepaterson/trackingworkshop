# Animating tracking data
# Author: James Paterson
# Date created: 2020-02-02

# Read the csv file
# The file should be in your working directory.
turtles <- read.csv("tracking_sample.csv", 
                    stringsAsFactors = FALSE) 

# First time only, you will need to install any missing packages
# install.packages("missingpackagename")
library(sp)
library(gganimate)
library(ggmap)
library(dplyr)
library(magrittr)

# Make sure points are in order (in case they weren't before)
turtles <- turtles %>%
  arrange(id, date) %>% # arrange by animal, and ascending date
  filter(!is.na(x), !is.na(y)) # remove NA's

# Make spatial
turtlessp <- turtles
coordinates(turtlessp) <- c("x", "y")

# Set coordinate system. The sample data are in UTM zone 18, WGS84
proj4string(turtlessp) <- CRS( "+proj=utm +zone=18 +datum=WGS84 
                            +ellps=WGS84" )

# Transform to decimal degrees (from UTM)
# ggmap's bounding boxes are in EPSG:4326
turtlesspgeo <- spTransform(turtlessp, CRS("+init=EPSG:4326"))

# Make back into dataframe (but include date for our animation)
# ggmap and gganimate use dataframes for plotting
turtlesgeo <- as.data.frame(turtlesspgeo@coords)
turtlesgeo$id <- turtlesspgeo@data$id # add individual identifier
turtlesgeo$date <- as.Date(turtlesspgeo@data$date) # Important! the variable for revealing in the animation must be
# either integer, numberic, POSIXct, Date, difftime, or orhms. Here I made sure it is a date.

# Plot study site using ggmap
# Using stamen tiles because Google tiles now require an account to download
mybasemap <- get_stamenmap(bbox = c(left = min(turtlesspgeo@coords[,1])-0.005, 
                                    bottom = min(turtlesspgeo@coords[,2])-0.005, 
                                    right = max(turtlesspgeo@coords[,1])+0.005, 
                                    top = max(turtlesspgeo@coords[,2])+0.005), 
                           zoom = 13)

# Plot static imagery + points + paths
mymap.paths <-ggmap(mybasemap) + 
  geom_point(data = turtlesgeo, aes(x = x, y = y, colour = id)) +
  geom_path(data = turtlesgeo, aes(x = x, y = y, colour = id, group = id)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_colour_manual(name = "Turtle number",
                      # Adjust the number of values for how many animals you have
                      values = c("red", "blue", "purple", "green", "orange"), 
                      # Enough breaks for every animal in the data set
                      breaks = unique(turtles$id)) + 
  theme(legend.position = "bottom") 

# Static plot
mymap.paths

# Update plot to animate. I used 'transition_reveal' so that the path builds from the beginning to the end. Use 'transition_states' to show only one point at a time
path.animate.plot <- mymap.paths +
  transition_reveal(along = date) +
  labs(title = 'Date: {frame_along}')  # Add a label on top to say what date each frame is

# To display the animation, use `animate`.
# When using your own data, adjust frames per second (fps) to be as fast or slow as you like.
# Be patient at this stage! It will eventually render in your plotting window
animate(path.animate.plot,
        fps = 5)

# Save as gif. This may be a large file, depending on your data set! 
anim_save(path.animate.plot,
          file = "animatedpaths.gif")