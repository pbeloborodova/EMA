# Load libraries -----------------------------------------------

setwd("/Users/polina/R/EMA")
library(reshape2)

# Load raw data ------------------------------------------------

uw2_raw <- read.csv("data/raw/uw/UWEXP_phaseII_mindfulness_fullMAAS.csv")

# Explore variables --------------------------------------------

str(uw2_raw)  # Data structure
max(uw2_raw$day)  # Number of observation days
max(uw2_raw$X)  # Number of observations