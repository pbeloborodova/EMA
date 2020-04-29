# Load libraries -----------------------------------------------

library(haven)
library(dplyr)

# Load raw data ------------------------------------------------

cmu2 <- read_sav("data/raw/cmu/Life@CMU Phase II EMA_blank rows added 060719dv_LONG_MERGED.sav")

# Create EMA index variable ------------------------------------

ema_index <- rep(0:83, 266)
cmu2 <- cbind(cmu2[order(cmu2$ID, cmu2$Day, cmu2$Timepoint),], ema_index)

# Prepare mindfulness vars -------------------------------------

cmu2$mindfulness <- rowMeans(cmu2[,c("mindfulness_1",  # Calculate mean MAAS score
                                   "mindfulness_2",
                                   "mindfulness_3")],
                            na.rm = T)
cmu2$mindfulness[is.na(cmu2$mindfulness)] <- NA  # Replace NaN with NA

ema_index <- rep(0:83, 266)  # Calculate average Cronbach alpha for mindfulness
cmu2_split <- split(cmu2[,c(41:43)], cmu2$ema_index)  # Split by EMA survey
cmu2_alphas_list <- sapply(cmu2_split, psych::alpha)  # Calculate alphas for each survey
cmu2_alphas_list <- cmu2_alphas_list[seq(1, 1176, 14)]  # Subset only relevant list elements
cmu2_alphas <- sapply(cmu2_alphas_list, '[[', 1)  # Get raw alpha values
mean(cmu2_alphas)  # Calculate average alpha

names(cmu2)[names(cmu2) == "mindfulness_4"] <- "mindwandering"  # Rename mind wandering var
names(cmu2)[names(cmu2) == "mindfulness_5"] <- "timefocus"  # Rename past/present/future focus var

# Select vars -------------------------------------------------

selected_vars <- grep("ID|SemesterWeek|Timepoint|^Day$|StartDate|affect|social|
                      |Belongingness|^mindfulness$|mindwandering|timefocus|ema", colnames(cmu2))

cmu2_clean <- cmu2[,selected_vars]

# Rename vars --------------------------------------------------

names(cmu2_clean) <- tolower(names(cmu2_clean))
names(cmu2_clean)[names(cmu2_clean) == "semesterweek"] <- "week"
names(cmu2_clean)[names(cmu2_clean) == "timepoint"] <- "time"
names(cmu2_clean)[names(cmu2_clean) == "startdate"] <- "date"
colnames(cmu2_clean)[6:11] <- c("feel_anxious", "feel_depressed", "feel_happy",
                            "feel_overwhelmed", "feel_lonely", "feel_connected")
names(cmu2_clean)[names(cmu2_clean) == "belongingness"] <- "feel_belonging"
colnames(cmu2_clean)[14:18] <- c("int_who","int_facetoface", "int_connected",
                                "int_feel", "int_wouldlike")

# Start time variables with zero -------------------------------

cmu2_clean$week[cmu2_clean$week %in% unique(cmu2_clean$week)] <-  # Week
  c(0,1,2)[match(cmu2_clean$week, unique(cmu2_clean$week))]
cmu2_clean$time <- cmu2_clean$time - 1  # Time of day

# Change date variable format ----------------------------------

cmu2_clean$date <- as.POSIXct(cmu2_clean$date, format = "%m/%d/%Y %H:%M")

# Create EMA index variable ------------------------------------

cmu2_clean <- cbind(cmu2_clean[order(cmu2_clean$id, cmu2_clean$day, cmu2_clean$time),], ema_index)

# Reverse-code mindfulness -------------------------------------

cmu2_clean$mindfulness <- 6 - cmu2_clean$mindfulness

# Remove SPSS labels -------------------------------------------

cmu2_clean <- zap_label(cmu2_clean)

# Save data ----------------------------------------------------

save(cmu2_clean, file = "data/cleaned/cmu/cmu2_clean.Rda")

