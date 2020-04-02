# Creating minimum convex polygons from telemetry data
# Author: James Paterson (james.earle.paterson@gmail.com)
# Date created: 2018-04-26

# Read the csv file
turtles <- read.csv("tracking_sample.csv", 
                    stringsAsFactors = FALSE) 
# The file should be in your working directory.

# SpatialPointsDataFrame objects don't like missing values
# Remove two rows with NA's
turtles <- turtles[!is.na(turtles$x) & !is.na(turtles$y),]

# Only include three columns (id, x, and y coordinates) for making MCP's
turtles.sp <- turtles[, c("id", "x", "y")] 

# Create a SpatialPointsDataFrame by defining the coordinates
library(sp)
coordinates(turtles.sp) <- c("x", "y")

# Set the coordinate reference system (CRS)
# More information on CRS here: 
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
# The sample data are UTM points in WGS84 from zone 18N
proj4string(turtles.sp) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )

library(adehabitatHR) # Load library

# Calculate MCPs for each turtle
turtles.mcp <- mcp(turtles.sp, percent = 100)

# Examine output
turtles.mcp

# Plot
library(scales) # Helps make polygons partly transparent using the alpha argument below
plot(turtles.sp, col = as.factor(turtles.sp@data$id), pch = 16)
plot(turtles.mcp, col = alpha(1:5, 0.5), add = TRUE)

# Calculate the MCP by including 50 to 100 percent of points
hrs <- mcp.area(turtles.sp, percent = seq(50, 100, by = 5))

hrs # examine dataframe

# Transform the point and MCP objects. 
turtles.spgeo <- spTransform(turtles.sp, CRS("+proj=longlat"))
turtles.mcpgeo <- spTransform(turtles.mcp, CRS("+proj=longlat"))

# Download tiles using ggmap
library(ggmap)
# Google tiles (requires a key first)

# register_google(key = "mykeyhere")
# mybasemap <- get_map(location = c(lon = mean(turtles.spgeo@coords[,1]), 
#                                   lat = mean(turtles.spgeo@coords[,2])), 
#                      source = "google", 
#                      zoom = 14,
#                     maptype = 'satellite')

mybasemap <- get_stamenmap(bbox = c(left = min(turtles.spgeo@coords[,1])-0.005, 
                                    bottom = min(turtles.spgeo@coords[,2])-0.005, 
                                    right = max(turtles.spgeo@coords[,1])+0.005, 
                                    top = max(turtles.spgeo@coords[,2])+0.005), 
                           zoom = 12)

# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
turtles.geo <- data.frame(turtles.spgeo@coords, 
                          id = turtles.spgeo@data$id )

mymap.hr <- ggmap(mybasemap) + 
  geom_polygon(data = fortify(turtles.mcpgeo),  
               # Polygon layer needs to be "fortified" to add geometry to the dataframe
               aes(long, lat, colour = id, fill = id),
               alpha = 0.3) + # alpha sets the transparency
  geom_point(data = turtles.geo, 
             aes(x = x, y = y, colour = id))  +
  theme(legend.position = c(0.15, 0.80)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_fill_manual(name = "Turtle number", 
                    values = c("red", "blue", "purple", "green", "orange"),
                    breaks = c("T001", "T002", "T003", "T004", "T005")) +
  scale_colour_manual(name = "Turtle number", 
                      values = c("red", "blue", "purple", "green", "orange"),
                      breaks = c("T001", "T002", "T003", "T004", "T005"))
mymap.hr
