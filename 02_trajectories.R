# Creating trajectories from telemetry data
# Author: James Paterson (james.earle.paterson@gmail.com)
# Date created: 2018-04-15

# Read the csv file
turtles <- read.csv("tracking_sample.csv", 
                    stringsAsFactors = FALSE) 
# The file should be in your working directory.

# Examine the structure of the data. Note presence of some NA's
str(turtles)

plot(turtles$y~turtles$x, 
     col = as.factor(turtles$id), 
     pch = 16)

# Load library
library(adehabitatLT)  
# You will need to install the package the first time with: 
# install.packages("adehabitatLT")

# We need to make sure that date is correctly formatted, and that there is an ID column
turtles.ltraj <- as.ltraj(xy = turtles[,c("x", "y")], 
                          date =  as.POSIXct(paste(turtles$date, turtles$time, sep = " ")), 
                          id = turtles$id)

plot(turtles.ltraj) # Plots each animal's points with a path connecting them

turtles.ltraj  # data.ltraj is a list

# Each element of the list is a dataframe for one individual
head(turtles.ltraj[[1]])  # The first six locations of the first animal

# Create a dataframe to hold all of the contents of bltu.paths with a column for id. 
# Put first element into the dataframe
total.path.df <- data.frame(turtles.ltraj[[1]], id = attr(turtles.ltraj[[1]], "id"))
# Use a 'for' loop to fill the larger dataframe with the rest of the trajectories.
for(i in 2:length(turtles.ltraj)) {
  total.path.df <- rbind(total.path.df, 
                         data.frame(turtles.ltraj[[i]], id = attr(turtles.ltraj[[i]], "id")))
}

# Calculate distance travelled per day and add it to the dataframe
total.path.df$distperday <- total.path.df$dist / (total.path.df$dt/60/60/24)

# Aggregate to show mean distance per day for each turtle
path.summary <- aggregate(distperday~id, data = total.path.df, FUN = mean)
path.summary$sd <- aggregate(distperday~id, data = total.path.df, FUN = sd)$distperday

# Look at summmary dataframe
path.summary

# Make a graph to visualize data using ggplot
library(ggplot2)
# Create limits used for error bars in graph
limits <- aes(ymax = path.summary$distperday + path.summary$sd, 
              ymin = path.summary$distperday - path.summary$sd)

# Make plot. Choose the dataframe (data) and aesthetics (aes; for the x and y)
path.plot <- ggplot(data = path.summary, aes(x = id, y = distperday, colour = id)) + 
  geom_point(size = 3) + # add points
  geom_errorbar(limits, width = 0.2) + # adds error bars
  labs(x = "Animal number", 
       y = "Mean distance travelled per day (m)" ) + # Axis labels
  theme_classic() + # Make plot black and white with no background grid
  theme(legend.position = "none")
path.plot # call plot