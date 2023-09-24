## ---------------------------
##
## Script name: 09_homerangeoverlap.R 
##
## Purpose of script: Estimate home range overlap using amt package
##
## Author: Dr. James Paterson
##
## Date Created: 2021-01-01
##
## 
## Email: james.earle.paterson@gmail.com
##
## ---------------------------

## ----loaddata--------------------------------------------

library(dplyr) # for data organization
library(ggplot2) # for plotting
# install.packages("amt") # first time only
library(amt) # for movement and overlap

# Load tracking data
turtles <- read.csv("tracking_sample.csv", 
                    stringsAsFactors = FALSE) 

# Make an amt `track` object with our sample data set
turtles_track <- turtles %>%
  # Remove NA x and y rows
  filter(!is.na(x), !is.na(y)) %>%
  # Add formatted date-time
  # This stage can be tricky, so double-check those date values in your own data.
  mutate(ts = as.POSIXct(paste(date, time, sep = " "))) %>%
  # Make track with coordinates, date-time, id
  make_track(x, y, ts, id = id,
             # Make sure to specify coordinate reference system (CRS)
             crs = sp::CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")) %>%
  # Use nest() to allow us to deal with multiple animals (5 in sample set)
  # Each animal's track is stored in a tibble (table) nested within the data column
  nest(data = -"id") %>%
  arrange(id)

# Examine object. It is a tibble with a row for each individual. 
#The value of 'data' contains the nested track_xyt object for each individual
turtles_track

# Examine one individual's track
head(turtles_track$data[1])

## ----examplehroverlap-----------------------------------------------------

# Add MCP list-column to turtles_track using `map`
turtles_track <- turtles_track %>% 
  mutate(mcp = map(data, function(x) 
    # levels are for which proportions (1.0 = 100%)
    x %>% hr_mcp(levels = c(1.0)))) 

# Each id's MCP is stored in the list column "mcp"
# Check by plotting the first one
plot(turtles_track$mcp[[1]])

# Calculate overlap between T002 (row 2) and T004 (row 4)
# labels are from ID in track object 
# overlap is fraction of overlap between two home ranges
hr_overlap(turtles_track$mcp[[2]], 
           turtles_track$mcp[[4]], 
           labels = turtles_track$id[c(2,4)],
           type = "hr")

# Calculate overlap between each of the five individuals
turtle_hr_overlap <- hr_overlap(turtles_track$mcp,
                                labels = turtles_track$id, 
                                which = "all", 
                                # alternative which = "consecutive",
                                # "one_to_all"
                                conditional = FALSE)

# Look at first 10 comparisons
head(turtle_hr_overlap, n = 10)

## ----vi_example------------------------------------------------------------

# Make a track of the data that is not nested.
turtles_track_nonest <- turtles %>%
  # Remove NA x and y rows
  filter(!is.na(x), !is.na(y)) %>%
  mutate(x_ = x,
         y_ = y,
         t_ = as.POSIXct(paste(date, time, sep = " "))) %>%
                           dplyr::select(x_, y_, t_, id) %>%
  make_track(x_, y_, t_, id = id,
             # Make sure to specify coordinate reference system (CRS)
             crs = sp::CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"))

# Make a base raster for whole study. Without this common raster, 
# each Kernel Density Estimator will likely use different pixel sizes and 
# output of hr_overlap will be blank.
base_trast <- make_trast(turtles_track_nonest,
                         res = 50)

# Calculate the kernel density estimator for two turtles at 95%
hr_T002 <- hr_kde(turtles_track$data[2][[1]], trast = base_trast, levels = 0.95)
hr_T004 <- hr_kde(turtles_track$data[4][[1]], trast = base_trast, levels = 0.95)

hr_overlap(hr_T002, hr_T004, type = "vi", conditional = FALSE)
