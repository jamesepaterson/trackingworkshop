# Kernel home ranges constrained by boundaries
# Author: James Paterson
# Date created: 2020-03-14

## ----loading libraries -----------------------------------
library(sp) # Spatial objects
library(rgdal) # If you want to read boundary as a shapefile
library(adehabitatHR) # For kernels
library(dplyr) # For filtering data
library(magrittr) # For piping

## ----load files-----------------------------------------------
turtles <- read.csv(file = "tracking_sample.csv",
                    stringsAsFactors = FALSE) %>%
  filter(id %in% c("T003", "T004"), # we will only use 2 individuals in example (T003 and T004)
         !is.na(x), # Remove NA coordinates
         !is.na(y)) 

# Make turtles a Spatial Point Dataframe
turtles.sp <- turtles
coordinates(turtles.sp) <- c("x", "y")

# Set CRS
turtles.sp@proj4string <- CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# You can import boundary line as a shapefile
bound <- rgdal::readOGR(dsn = "boundaryfolder", # Files in "boundaryfolder"
                           layer = "boundary_shape", # name of shapefile
                           encoding = "ESRI Shapefile", # It is an ESRI shapefile
                        verbose = FALSE) # Change to TRUE and it prints source and number of features

# Or, you can create a boundary line with a vector of coordinates to form vertices (commented here)
# bound <- structure(list(x = c(349801.0, 350097.4, 350323.9, 350143.9),
#                            y = c(4944954, 4945324, 4945503, 4946280)), .Names = c("x", "y"))
# bound <- do.call("cbind",bound)
# Slo1 <- Line(bound)
# Sli1 <- Lines(list(Slo1), ID="myboundary")
# bound <- SpatialLines(list(Sli1))

# Make sure bound CRS same as point CRS
bound@proj4string <- turtles.sp@proj4string

## ----plot points boundary------------------------------------
# Plot
plot(turtles.sp, 
     col = as.factor(turtles.sp$id),
     pch = 16)
plot(bound, 
     col = "blue", 
     lwd = 5,
     add = TRUE)

## ----make kernels---------------------------------------------------------
# Make normal kernel
trad.kernel <- kernelUD(turtles.sp[,"id"], # Only pass the "id" column and the coordinates
                        h = "href", # Use the reference bandwidth
                        grid = 1000)

trad.kernel.poly <- trad.kernel %>%
  getverticeshr(., percent = 95) # Get the vertices for the 95% contour line

# Plot
plot(bound, 
     col = "blue",
     lwd = 5)
plot(trad.kernel.poly,
     add = TRUE)
plot(turtles.sp,
     col = as.factor(turtles.sp$id),
     pch = 16,
     add = TRUE)

## ----kernels with a boundary------------------------------------------------------
# Make a kernel with boundary for T003
bound.kernel.t003 <- turtles.sp[turtles.sp$id == "T003","id"] %>%
                              kernelUD(.,
                                       # h must be numeric 
                                       # here I give the numeric argument for "href" from our estimate above
                                       h = trad.kernel$T003@h$h, 
                                       # If you don't set grid, the default often won't follow the boundary
                                        # I set grid to have 1000 x 1000 pixels
                                       # You may have to adjust (will depend on your scale)
                                       grid = 1000, 
                                        # The boundary argument takes SpatialLines objects
                                       boundary = bound) 

# Get the vertices for T003 and extract as a polygon
bound.kernel.t003.poly <- bound.kernel.t003 %>%
  getverticeshr(., percent = 95)

# Make a kernel with boundary for T004
bound.kernel.t004 <- turtles.sp[turtles.sp$id == "T004","id"] %>%
  kernelUD(.,
           h = trad.kernel$T004@h$h,
           grid = 1000,
           boundary = bound) 

# Get vertices for T004
bound.kernel.t004.poly <- bound.kernel.t004 %>%
  getverticeshr(., percent = 95)

## ----plot new kernels------
par(mfrow = c(1,2))
# Plot
plot(trad.kernel.poly[trad.kernel.poly$id == "T003",],
     main = "T003 home range") # Original kernel
plot(bound,  # Boundary line
     col = "blue", 
     add = TRUE,
     lwd=5)
plot(bound.kernel.t003.poly, # New, Restricted kernel
     col = "lightgray",
     add = TRUE)
plot(turtles.sp[turtles.sp$id == "T003",],
     col = "black",
     pch = 16,
     add = TRUE)

# T004
plot(trad.kernel.poly[trad.kernel.poly$id == "T004",],
     main = "T004 home range") # Original kernel
plot(bound,  # Boundary line
     col = "blue", 
     add = TRUE,
     lwd=5)
plot(bound.kernel.t004.poly, # New, Restricted kernel
     col = "lightgray",
     add = TRUE)
plot(turtles.sp[turtles.sp$id == "T004",],
     col = "black",
     pch = 16,
     add = TRUE)
par(mfrow = c(1,1))

