# Load libraries -----------------------------------------------

library(haven)

# Load raw data ------------------------------------------------

cmu_raw <- read_sav("data/raw/cmu/Life@CMU Phase II EMA_blank rows added 060719dv_LONG_MERGED.sav")
cmu <- cmu_raw

# Prepare mindfulness vars -------------------------------------

cmu$mindfulness <- rowMeans(cmu[,c("mindfulness_1",  # Calculate mean MAAS score
                                   "mindfulness_2",
                                   "mindfulness_3")],
                            na.rm = T)
cmu$mindfulness[is.na(cmu$mindfulness)] <- NA  # Replace NaN with NA

names(cmu)[names(cmu) == "mindfulness_4"] <- "mindwandering"  # Rename mind wandering var
names(cmu)[names(cmu) == "mindfulness_5"] <- "timefocus"  # Rename past/present/future focus var

# Select vars -------------------------------------------------

cmu_clean <- cmu[,-c(6,7,25,41:43,105)]

# Save data ----------------------------------------------------

save(cmu_clean, file = "data/cleaned/cmu/cmu2_clean.Rda")

