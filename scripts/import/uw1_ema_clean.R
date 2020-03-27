# Load libraries -----------------------------------------------

library(reshape2)

# Load raw data ------------------------------------------------

uw_raw <- read.csv("data/raw/uw/UWEXP_phaseI_mindfulness_fullMAAS.csv")

# Explore variables --------------------------------------------

str(uw_raw)  # Data structure
max(uw_raw$day)  # Number of observation days
max(uw_raw$X)  # Number of observations

# Select variables ---------------------------------------------

uw <- uw_raw[,c(1:4,12:39)]

# Identify EMA data collection weeks ---------------------------

uw$date <- as.POSIXct(uw$date)
uw$weekday <- weekdays(uw$date, abbreviate = T)
uw$ema <- apply(!is.na(uw[,c(5:32)]), 1, sum)
uw$ema <- ifelse(uw$ema > 0, 1, 0)

# Remove first two days so that data starts with Wed
uw <- subset(uw, date != "2018-01-01" & date != "2018-01-02")

# Add week numbers (week starts on Wed)
dates <- unique(as.character(uw$date))
weeknum <- rep(c(1:26), each = 7)
weeknum <- weeknum[1:179]
weeknumbers <- data.frame(cbind(dates, weeknum))
uw$weeknum <- weeknumbers$weeknum[match(as.character(uw$date), weeknumbers$dates)]
uw$weeknum <- as.numeric(as.character(uw$weeknum))

# Find weeks with data
ema_obs <- aggregate(uw$ema, by = list(uw$weeknum), FUN = sum)
ema_weeks <- ema_obs$Group.1[ema_obs$x > 1]

# Select data from data collection weeks -----------------------

uw_select <- uw[weeknum %in% ema_weeks,]

# Reshape to long ----------------------------------------------

feel_anxious <- colnames(uw_select[,c(5,12,19,26)])
feel_depressed <- colnames(uw_select[,c(6,13,20,27)])
feel_frustrated <- colnames(uw_select[,c(7,14,21,28)])
feel_overwhelmed <- colnames(uw_select[,c(8,15,22,29)])
feel_lonely <- colnames(uw_select[,c(9,16,23,30)])
feel_happy <- colnames(uw_select[,c(10,17,24,31)])
feel_connected <- colnames(uw[,c(11,18,25,32)])

affect <- list(feel_anxious,
               feel_depressed,
               feel_frustrated,
               feel_overwhelmed,
               feel_lonely,
               feel_happy,
               feel_connected)

time <- c("morning", "midday1", "midday2", "evening")

uw_long <- reshape(uw_select, varying = affect,
                   v.names = c("feel_anxious",
                               "feel_depressed",
                               "feel_frustrated",
                               "feel_overwhelmed",
                               "feel_lonely",
                               "feel_happy",
                               "feel_connected"),
                   timevar = "time",
                   times = time,
                   idvar = "X",
                   ids = row.names(uw_select),
                   direction = "long")

# Replace char values in time var with numbers
uw_long$time[uw_long$time == "morning"] <- 0
uw_long$time[uw_long$time == "midday1"] <- 1
uw_long$time[uw_long$time == "midday2"] <- 2
uw_long$time[uw_long$time == "evening"] <- 3

uw_long$time <- as.numeric(uw_long$time)

# Replace weeknum values with 0:4
uw_long$weeknum[uw_long$weeknum == 5] <- 0
uw_long$weeknum[uw_long$weeknum == 9] <- 1
uw_long$weeknum[uw_long$weeknum == 16] <- 2
uw_long$weeknum[uw_long$weeknum == 20] <- 3

# Replace day values with 0:27
day_old <- unique(uw_long$day)
day_new <- 0:27
uw_long$day[uw_long$day %in% day_old] <- day_new[match(uw_long$day, day_old)]

# Add each participant's time points variables
uw_long <- uw_long[order(uw_long$PID, uw_long$day, uw_long$time), ]
uw_long$timepoint <- rep(seq(0:111), length(unique(uw_long$PID)))
uw_long$timepoint <- uw_long$timepoint - 1

# Rename rows to row numbers
rownames(uw_long)<-c(1:nrow(uw_long))

# Select final set of variables --------------------------------

uw1_clean <- uw_long[,-c(1,5,6)]

# Rename variables ---------------------------------------------

names(uw1_clean) <- tolower(names(uw1_clean))
names(uw1_clean)[names(uw1_clean) == "pid"] <- "id"
names(uw1_clean)[names(uw1_clean) == "weeknum"] <- "week"
names(uw1_clean)[names(uw1_clean) == "timepoint"] <- "ema_index"

# Save data ----------------------------------------------------

save(uw1_clean, file = "data/cleaned/uw/uw1_clean.Rda")
