# Example of kernel home ranges that equal MCP size
# Written by James E Paterson

##### 1. Load Example Data ---------------------------------------------------------------

# Read the csv file
tracking.df <- read.csv("tracking_sample.csv", 
                        stringsAsFactors = FALSE) 
# The file should be in your working directory.

# SpatialPointsDataFrame objects don't like missing values
# Remove two rows with NA's
tracking.df <- tracking.df[!is.na(tracking.df$x) & !is.na(tracking.df$y),]

# Make Spatial Points Data Frane
tracking.sp <- tracking.df 

# Define the coordinates
library(sp)
coordinates(tracking.sp) <- c("x", "y")

# Set the coordinate reference system (CRS)
# More information on CRS here: 
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
# The sample data are UTM points in WGS84 from zone 18N
proj4string(tracking.sp) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )

library(adehabitatHR) # Load library

# Calculate MCPs for each turtle
tracking.mcp <- mcp(tracking.sp[,"id"], percent = 100)
print(tracking.mcp)

##### 2. Apply function to predict the smoothing factor -----------------------

# Apply smoothing factor estimating function. system.time just provides time estimate
# Notice I save vector of smoothing factos to "h.pred". The tracking data should be in metres.
system.time(h.pred <- reptile.kd.h.fn(tracking.sp))  # The sp object should contain at least x,y, & id
# Takes: 10 - 15 sec for 5 animals

h.pred

##### 3. Apply function to create new kernel home ranges -------------------------

# Calculate kernel home ranges
system.time(test.homeranges <- reptile.kd.homerange.fn(tracking.sp, h.pred))
# Takes 0.3 sec for 5 animals

# test.homeranges is a list (each item is the polygon for one animal)

# Plot MCP, kernel, & points for first animal
plot(test.homeranges[[1]]) # Plot first home range
plot(tracking.sp[tracking.sp$id == "T001",], add = TRUE, pch = 16)
plot(tracking.mcp[1,], add = TRUE)

##### 4. Making sure MCP equals new kernel area -------------------------------

# Check that areas are perfectly correlated (and equal)
hr.comparison <- data.frame(id = tracking.mcp$id,
                            mcp = tracking.mcp$area,
                            kd = NA)

for(i in 1:length(test.homeranges)){
  hr.comparison[i,3] <- tryCatch(test.homeranges[[i]]$area, error = function(err) NA)
}

hr.lm <- lm(kd~mcp, hr.comparison) # Linear regression to compare kd area to mcp area

summary(hr.lm)

library(ggplot2)
ggplot(hr.comparison, aes(x = mcp, y = kd)) +
  geom_point(pch = 21) + 
  geom_abline(slope = 1, intercept = 0, colour = "blue") +
  labs(x =  "MCP area (ha)", y = "Kernel area (ha)")
# If all points fall on the line then the new kernels are equal in area to the 100% MCP.
# The accuracy may depend on the shape & size of your home ranges since h is an integer (no decimals)
