# Formatting spatial data & mapping in ggmap
# Author: James Paterson
# Date created: 2018-04-11

# Create potential telemetry data points for 3 animals with 100 relocations each.
points.1.df <- data.frame(id = 1,  # identifies the animal
                          x = rnorm(100, mean = 284979, sd = 45),  # easting coordinates
                          y = rnorm(100, mean = 5081411, sd = 15), # northing coordinates
                          zone = 18, # UTM zone
                          date = seq(as.Date("2017/05/03"), as.Date("2017/08/10"), by = 1)) # sequence of dates

points.2.df <- data.frame(id = 2, 
                          x = rnorm(100, mean = 285910, sd = 30),
                          y = rnorm(100, mean = 5082848, sd = 50),
                          zone = 18,
                          date = seq(as.Date("2017/05/03"), as.Date("2017/08/10"), by = 1))

points.3.df <- data.frame(id = 3, 
                          x = rnorm(100, mean = 287448, sd = 70),
                          y = rnorm(100, mean = 5081395, sd = 45),
                          zone = 18,
                          date = seq(as.Date("2017/05/03"), as.Date("2017/08/10"), by = 1))

points.df <- rbind(points.1.df, points.2.df, points.3.df) # join the three animals' data together

# To format for spatial analyses, we need to remove NA values from the coordinate columns
# This isn't an issue for this simulated dataset
points.df <- points.df[!is.na(points.df$x) & !is.na(points.df$y),]

# The dataframe should only have 3 columns (x, y, and an identifier) 
# for calculating trajectories and home ranges.
points.sp <- points.df[, c("id", "x", "y")]

# Turn into a spatial points dataframe (class: SpatialPointsDataFrame)
library(sp)
coordinates(points.sp) <- c("x", "y")

# Examine the structure of our SpatialPointsDataFrame
str(points.sp)

# Set coordinate system & projection
proj4string(points.sp) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )

plot(points.sp, col = points.sp@data$id, pch = 16)

# Transform the point object (points.sp)
points.sp.geo <- spTransform(points.sp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

# Plot study site using ggmap
library(ggmap)
mybasemap <- get_stamenmap(bbox = c(left = min(points.sp.geo@coords[,1])-0.01, 
                                    bottom = min(points.sp.geo@coords[,2])-0.01, 
                                    right = max(points.sp.geo@coords[,1])+0.01, 
                                    top = max(points.sp.geo@coords[,2])+0.01), 
                           zoom = 12)

## For satellite imagery, use Google map tiles. However, you need to register a key.
## Guide on getting a key: https://www.r-bloggers.com/geocoding-with-ggmap-and-the-google-api/
# register_google(key = "yourkeyhere")
## The location argument can take a vector with latitude and longitude, or a character string. 
# mybasemap <- get_map(location = c(lon = mean(points.sp.geo@coords[,1]) , 
#                                   lat = mean(points.sp.geo@coords[,2])), 
#                      source = "google", zoom = 14, maptype = 'satellite')

# Google Map version
# trentumap <- get_map(location = "trent university, peterborough", zoom = 16, maptype = 'satellite')

# Stamen version
trentumap <- get_stamenmap(bbox = c(left = -78.29 - 0.01,
                       right = -78.29 + 0.01,
                       bottom = 44.36 - 0.01,
                       top = 44.36 + 0.01),
              zoom = 15)

ggmap(trentumap)

# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
points.geo <- data.frame(id = points.sp.geo@data$id, # add individual identifier
                         points.sp.geo@coords) # Add coordinates

# Plot imagery + points + paths
mymap.paths <-ggmap(mybasemap) + 
  geom_point(data = points.geo, aes(x = x, y = y, colour = as.factor(id))) +
  geom_path(data = points.geo, aes(x = x, y = y, colour = as.factor(id))) +
  theme(legend.position = c(0.15, 0.80)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_colour_manual(name = "Animal number",
                      values = c("black", "red", "green"))

mymap.paths
