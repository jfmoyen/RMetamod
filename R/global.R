library(shiny)

library(tidyverse)
library(magrittr)

library(ReacTran)

library(png)
library(grid)

library(bslib)
library(colourpicker)

library(shinyWidgets)

library(rlist)

#DEBUG
 options(shiny.reactlog = TRUE)

#########################
#### LOAD FUNCTIONS  ####
#########################

source("metamod_functions.R")

#####################
#### COMMON DEF  ####
#####################

# Conversion factors
# Variables are entered in the UI in units that are not SI. 
# Here we define UI to SI conversions as a named vector

UItoSI <- c(1000,1e-3,1e-6,1,1,1,1e-3 * 1e6)
names(UItoSI) <- c("z_moho","q_moho","HP","k","rho","Cp","u")
  
# Load backgrounds

faciesMeta = list(
  image = readPNG("faciesMeta.png"),
  Tmin = -7, 
  Tmax = 1180,
  zmin = 1,
  zmax = 73,
  Tpos = c(0,200,400,600,800,1000),
  zpos=c(70,60,50,40,30,20,10,0)
)

faciesUltraMeta = list(
  image = readPNG("faciesUltraMeta.png"),
  Tmin = -10,
  Tmax = 1195,
  zmin = -0.4,
  zmax = 149,
  Tpos = c(0,200,400,600,800,1000),
  zpos=c(120,90,60,30,0)
)

React = list(
  image = readPNG("React.png"),
  Tmin = 0,
  Tmax = 1164,
  zmin = 0,
  zmax = 70,
  Tpos = c(0,200,400,600,800,1000),
  zpos=c(60,50,40,30,20,10,0)
)

Ride = list(
  image = readPNG("Ride.png"),
  Tmin = 0,
  Tmax = 1987,
  zmin = 0,
  zmax = 183,
  Tpos = c(0,500,1000,1500),
  zpos=c(140,120,100,80,60,40,20,0)
)




