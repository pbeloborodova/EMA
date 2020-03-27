# Load packages ----------------------------------------

library(ggplot2)
library(psych)
library(dplyr)
library(zoo)

# Load data --------------------------------------------

load("~/R/EMA/data/cleaned/uw/uw1_clean.Rda")
uw1_exp <- uw1_clean

# Explore patterns in affect of a random participant ---

p1 <- subset(uw1_exp, id == sample(unique(uw1_exp$id), 1))  # Subset first random participant's data

# Loneliness

ggplot(p1,
       mapping =  aes(x = time, y = feel_lonely)) + 
  geom_point() +  geom_step() + 
  ylab("Loneliness") +
  xlab("Time") +
  scale_x_continuous() +
  facet_wrap(~ day, nrow = 3) +
  ggtitle("Random participant's daily changes in loneliness")

# Connectedness

ggplot(p1,
       mapping =  aes(x = time, y = feel_connected)) + 
  geom_point() +  geom_step() + 
  ylab("Connectedness") +
  xlab("Time") +
  scale_x_continuous() +
  facet_wrap(~ day, nrow = 3) +
  ggtitle("Random participant's daily changes in connectedness")

# Depression

ggplot(p1,
       mapping = aes(x = time, y = feel_depressed)) + 
  geom_point() + geom_step() + 
  ylab("Depression") +
  xlab("Time") +
  scale_x_continuous() +
  facet_wrap(~ day, nrow = 3) +
  ggtitle("Random participant's daily changes in depression")

# Explore missing values -------------------------------

# Add missed questions variable

uw1_exp$missed <- ifelse(rowSums(is.na(uw1_exp[,6:13])) == 7, 1, 0)

# Create data frame with missed values by date

missing <- uw1_exp[,13:14] %>% 
  group_by(ema_index) %>%
  summarize(n_missed = sum(missed))

missing$share_missed <- 100*missing$n_missed/length(unique(uw1_exp$id))

ggplot(missing[-59,], aes(x = ema_index, y = share_missed)) +
  geom_point() +
  ylab("% of missed prompts") +
  xlab("Time") +
  ggtitle("% of participats who missed prompts across time") +
  scale_x_continuous(breaks = c(14,42,70, 98),
                     labels = c("Week 1", "Week 2", "Week 3", "Week 4"))

# Explore patterns in affect of all participants -------

# Create data frame with scores averaged across participants at each time point
# and standardized scores

uw1_average <- uw1_exp[,6:13] %>%
  group_by(ema_index) %>%
  summarize_all(list(mean = mean), na.rm = TRUE)

# Add standardized variables

uw1_average <- uw1_average %>%
  mutate_at(vars(starts_with("feel")), list(z = ~as.vector(scale(.))))
  
# Calculate rolling means

feel_lonely_temp_zoo <- zoo(uw1_average$feel_lonely_mean_z, uw1_average$ema_index)  # Loneliness
feel_lonely_m_av1 <- rollmean(feel_lonely_temp_zoo, 3, fill = list(NA, NULL, NA))
uw1_average$feel_lonely_amb_av = coredata(feel_lonely_m_av1)

feel_connected_temp_zoo <- zoo(uw1_average$feel_connected_mean_z, uw1_average$ema_index)  # Connectedness
feel_connected_m_av1 <- rollmean(feel_connected_temp_zoo, 3, fill = list(NA, NULL, NA))
uw1_average$feel_connected_amb_av = coredata(feel_connected_m_av1)

feel_depressed_temp_zoo <- zoo(uw1_average$feel_depressed_mean_z, uw1_average$ema_index)  # Depression
feel_depressed_m_av1 <- rollmean(feel_depressed_temp_zoo, 3, fill = list(NA, NULL, NA))
uw1_average$feel_depressed_amb_av = coredata(feel_depressed_m_av1)

# Plot rolling mean of loneliness and connectedness

ggplot(uw1_average, aes(x = ema_index)) +
  geom_line(aes(y = feel_lonely_amb_av, color = "Loneliness")) +
  geom_line(aes(y = feel_connected_amb_av, color = "Connectedness")) +
  ggtitle("Loneliness and connectedness across time") +
  scale_x_continuous(breaks = c(14,42,70,98),
                     labels = c("Week 1", "Week 2", "Week 3", "Week 4")) +
  ylab("Variable (Z scores)") +
  xlab("Time") +
  scale_colour_manual("", 
                      breaks = c("Loneliness",
                                 "Connectedness"),
                      values = c("Loneliness"="blue",
                                 "Connectedness"="orange2"))

# Plot rolling mean of depression

ggplot(uw1_average, aes(x = ema_index, y = feel_depressed_amb_av)) +
  geom_line(color = "black") +
  ggtitle("Depression across time") +
  scale_x_continuous(breaks = c(14,42,70,98),
                     labels = c("Week 1", "Week 2", "Week 3", "Week 4")) +
  ylab("Depression (Z scores)") +
  xlab("Time")

