# Home range estimates with kernels
# Author: James Paterson (james.earle.paterson@gmail.com)
# Date created: 2018-11-07

# Read the csv file (should be in your working directory)
turtles <- read.csv("tracking_sample.csv", 
                    stringsAsFactors = FALSE) 

# SpatialPointsDataFrame objects don't like missing values
# Remove rows with NA's
turtles <- turtles[!is.na(turtles$x) & !is.na(turtles$y),]

# Create a copy of the object to make into a SpatialPointsDataFrame
# Only include three columns (id, x, and y coordinates) for estimating home ranges
turtles.sp <- turtles[, c("id", "x", "y")]

# Create a SpatialPointsDataFrame by defining the coordinates
library(sp)
coordinates(turtles.sp) <- c("x", "y")

# Set the coordinate reference system (CRS)
# More information on CRS here: 
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
# The sample data are UTM points in WGS84 from zone 18N
proj4string(turtles.sp) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )

library(adehabitatHR)
kernel.ref <- kernelUD(turtles.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref) # plot
kernel.ref[[1]]@h # The smoothing factor is stored for each animal in the "h" slot

kernel.lscv <- kernelUD(turtles.sp, h = "LSCV") # Least square cross validation
image(kernel.lscv) # plot

plotLSCV(kernel.lscv) # Look for a dip

turtle.kernel.poly <- getverticeshr(kernel.ref, percent = 95) 
print(turtle.kernel.poly)  # returns the area of each polygon

color <- rep("white", nrow(turtles.sp@data))
color[(turtles.sp@data$id == "T002")] <- "red"
color[(turtles.sp@data$id == "T003")] <- "green"
color[(turtles.sp@data$id == "T004")] <- "blue"
color[(turtles.sp@data$id == "T005")] <- "cyan"
plot(turtle.kernel.poly, col = turtle.kernel.poly@data$id)
plot(turtles.sp, add = TRUE, col = color, pch = 21)