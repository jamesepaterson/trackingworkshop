# Analyzing telemetry data in R

This repository holds the R code and sample data from my workshop on analyzing tracking data in R. Links go to tutorial posts on my blog.

Description of contents:

`tracking_sample.csv` contains simulated tracking data for five turtles. This is the data used in all code examples in this repository.

`boundaryfolder` contains the shapefiles for creating a kernel home range limited by a line.

`01_formattingandmapping.R` Formatting data & basic mapping (https://jamesepaterson.github.io/jamespatersonblog/01_trackingworkshop_formatting)

`02_trajectories.R` Creating trajectories (https://jamesepaterson.github.io/jamespatersonblog/02_trackingworkshop_trajectories)

`03_homerangesMCP.R` Home ranges with minimum convex polygons (https://jamesepaterson.github.io/jamespatersonblog/03_trackingworkshop_homeranges)

`04_homerangeskernels.R` Home ranges with kernel density estimators (https://jamesepaterson.github.io/jamespatersonblog/04_trackingworkshop_kernels)

`05_reptilehomerangefunctions.R` Functions to make 95% kernel contour area equal to the 100% minimum convex polygon area

`06_makingkernelsequalmcparea.R` Applying the functions to make 95% kernel contour area equal to the 100% minimum convex polygon area (https://jamesepaterson.github.io/jamespatersonblog/06_trackingworkshop_reptilekernels)

`07_animatingtrackingdata.R` Animating telemetry data using `gganimate`

`08_homerangeskernels_boundary.R` Home ranges with kernel density estimators with boundaries

# Citing this workshop?
The code and data for this workshop is archived on Zenodo and has a DOI you can cite:
DOI: 10.5281/zenodo.3557727
