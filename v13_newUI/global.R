library(deSolve)
library(dplyr)
library(ggplot2)
library(Rcpp)
library(readxl)
library(rmarkdown)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(TSA)

# C++ script
sourceCpp("./www/scripts/modGMS.cpp")

# R script
source("./www/scripts/runGMS.R")

# table of parameters
meta <- read_excel("./www/data/parameters.xlsx", col_type="text")
meta <- as.data.frame(meta)

# for localisation
languages_short <- c("en", "th")
languages <- c("English", "ภาษาไทย")

flags <- c(
  "flags/gb.png",
  "flags/th.png"
)

# for bookmarking
enableBookmarking(store = "url")

# non-reactive parameters
# define the number of weeks to run the model
dt <- 1/12
startyear <- 2007
stopyear <- 2025
maxt <- (stopyear-startyear)
times <- seq(0, maxt, by = dt)
tsteps <- length(times)