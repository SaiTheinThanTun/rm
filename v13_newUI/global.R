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
