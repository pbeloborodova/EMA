# Load packages ----------------------------------------

options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors
library(readr)
library(psych)
library(dplyr)

# Load data --------------------------------------------

uw1_demo <- read_csv("data/raw/uw/UWEXP-demo-phaseI_prepped.csv")

# Descriptive stats ------------------------------------

# Age
describe(uw1_demo$age)

# Ethnicity
ethnicity_vars <- grep("(?=^((?!text).)*$)ethnicity", colnames(uw1_demo), perl=TRUE)
100*colSums(uw1_demo[,ethnicity_vars], na.rm = TRUE)/nrow(uw1_demo)

# Gender
uw1_demo %>%
  group_by(gender) %>%
  dplyr::summarize(n = n(), percent = 100*n/nrow(uw1_demo))
