# Reptile kernel home range functions
# Author: James E Paterson james.earle.paterson@gmail.com

# Purpose:
# A function that will create kernel home ranges (95% density) that are equal in area to the 100% MCP
# This is based on the recommendation from Row & Blouin-Demers 2006

# I use 2 functions
# 1. "reptile.kd.h.fn" predicts the smoothing factor required for each animal. This function doees most of the work (be patient)
# 2. "reptile.kd.homerange.fn" creates the new home ranges using the vector of new smoothing factors from function 1



##### 1. "reptile.kd.h.fn" ----------------------------------------------------

# Define the function to calculate smoothing factor.
# This takes a tracking data set and calculates a smoothing factor for every individual to create 
# a 95% kernel polygon that equals the area of the 100% MCP. The tracking data should be in metres (eg. UTM).
# This function returns a vector of smoothing factors to use in creating a home range.
reptile.kd.h.fn <- function(trackingdatasp){  
  # trackingdatasp is a Spatial Points Dataframe containing: id, x, y
  trackingdatasp <- trackingdatasp[,c("id")]
  
  # Create datafrmae with MCP area object for all animals
  tracking.mcp <- mcp(trackingdatasp, percent = 100)@data
  
  # Put reference bandwidths & put in a vector
  href <- c()
  hpred <- c()
  
  # Loop for each individual
  for(i in 1:length(unique(trackingdatasp$id))){
    
    # Define spatial points
    id <- sort(unique(trackingdatasp@data$id))[i]
    id.datasp <- trackingdatasp[trackingdatasp$id == id,]
    
    # Define href for an individual
    kernel.ref <- kernelUD(id.datasp, h = "href")
    href[i] <- kernel.ref[[1]]@h$h
    
    # define limits of h to try
    htry <- seq(1, href[i] + 20, by = 2.5) 
    
    # Make a dataframe to hold areas
    areatoh.df <- data.frame()
    
    # Now do kernel homem range for each of htry
    # tryCatch makes sure the loop doesn't stop if there is an error
    for(k in 1:length(htry)){
      kernel.try <- tryCatch(kernelUD(id.datasp, h = htry[k]), error=function(err) NA)
      areatoh.df[k, 1] <- htry[k]
      areatoh.df[k, 2] <- tryCatch(getverticeshr(kernel.try, percent = 95)$area, error=function(err) NA)
      
    }
    
    names(areatoh.df) <- c("h", "area")
    
    # First get rid of any wonky values (curves) at low smoothing factors
    if(sum(is.na(areatoh.df$area)) > 0){
      last.h <- max(areatoh.df$h[is.na(areatoh.df$area)])
      areatoh.df$area[areatoh.df$h < last.h + 20] <- NA
    }
    
    # Try to fix NA values by expanding grid
    for(l in 1:length(htry)){
      if(is.na(areatoh.df[l, 2])){
        # Define grid
        # This avoids problems with "grid too small to estimate home range size" (most common kernel issue in R)
        x <- seq(min(id.datasp@coords[,1]) - 2000,
                 max(id.datasp@coords[,1]) + 2000,
                 by = 10.) # resolution is the pixel size you desire
        y <- seq(min(id.datasp@coords[,2]) - 2000,
                 max(id.datasp@coords[,2]) + 2000,
                 by = 10.)
        xy <- expand.grid(x = x, y = y)
        coordinates(xy) <- ~ x + y
        gridded(xy) <- TRUE
        
        kernel.try <- tryCatch(kernelUD(id.datasp, h = htry[l], grid = xy), error = function(err) NA) 
        areatoh.df[l, 2] <- tryCatch(getverticeshr(kernel.try, percent = 95)$area, error = function(err) NA)
      }
    }
    # Construct a loess model to predict area based on h
    x <- areatoh.df$area[!is.na(areatoh.df$h) & !is.na(areatoh.df$area)]
    y <- areatoh.df$h[!is.na(areatoh.df$h) & !is.na(areatoh.df$area)]
    area.h.loess <- loess(y~x)
    
    # Predict the correct h
    hpred[i] <- predict(area.h.loess, newdata  = tracking.mcp[i,]$area)
    print(hpred[i])
  }
  return(hpred)
}
  

##### 2. "reptile.kd.homerange.fn" --------------------------------------------

# Function to create new home ranges
# Returns a list. Each item of the list is one individual's 95% kernel home range.
reptile.kd.homerange.fn <- function(trackingdatasp, hvector){
  newkernel.poly <- list()
for(i in 1:length(unique(trackingdatasp@data$id))){
    # Define spatial points layer for an individual
    id <- sort(unique(trackingdatasp@data$id))[i]
    id.datasp <- trackingdatasp[trackingdatasp$id == id, "id"]
  
    #Create new kernel
    newkernel <-  tryCatch(kernelUD(id.datasp, h = hvector[i]), error = function(err) NA)
    # Extract vertices in a list of polygons
    newkernel.poly[[i]] <- tryCatch(getverticeshr(newkernel, percent = 95), error = function(err) NA)
    if(is.na(newkernel.poly[i])){
        # Define grid
        # This avoids problems with "grid too small to estimate home range size" (most common kernel issue in R)
        x <- seq(min(id.datasp@coords[,1]) - 2000,
                 max(id.datasp@coords[,1]) + 2000,
                 by = 10.) # resolution is the pixel size you desire
        y <- seq(min(id.datasp@coords[,2]) - 2000,
                 max(id.datasp@coords[,2]) + 2000,
                 by = 10.)
        xy <- expand.grid(x = x, y = y)
        coordinates(xy) <- ~ x + y
        gridded(xy) <- TRUE
        #Create new kernel
        newkernel <-  tryCatch(kernelUD(id.datasp, h = hvector[i], grid = xy), error = function(err) NA)
        # Extract vertices in a list of polygons
        newkernel.poly[[i]] <- tryCatch(getverticeshr(newkernel, percent = 95), error = function(err) NA)
      }
    }
  return(newkernel.poly)
}
