# Load packages ----------------------------------------

options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors
library(haven)
library(psych)
library(dplyr)

# Load data --------------------------------------------

cmu2_demo <- read_sav("data/raw/cmu/Life@CMU Phase II pre-ira-post-EMAwide 070619dv_FINAL.sav")

demo_vars <- grep("^[aA]ge|[rR]ace|[gG]ender", colnames(cmu2_demo))  # Select variables
cmu2_demo <- cmu2_demo[,demo_vars]

# Descriptive stats ------------------------------------

# Age
age_labels <- attr(cmu2_demo$Age, "labels")  # Replace SPSS labels with age values
age_values <- as.numeric(names(age_labels))
cmu2_demo$Age[cmu2_demo$Age %in% age_labels] <- age_values[match(cmu2_demo$Age, age_labels)]

psych::describe(cmu2_demo$Age)  # Calculate descriptive stats

# Ethnicity
cmu2_demo[,3:10][cmu2_demo[,3:10] != 0] <- 1  # Replace different non-zero values with 1's
race_vars <- grep("(?=^((?!TEXT).)*$)race", colnames(cmu2_demo), perl=TRUE)
round(100*colSums(cmu2_demo[,race_vars], na.rm = TRUE)/nrow(cmu2_demo), 2)

# Gender
cmu2_demo %>%
  group_by(Gender_identity) %>%
  summarize(n = n(), percent = n/nrow(cmu2_demo))
