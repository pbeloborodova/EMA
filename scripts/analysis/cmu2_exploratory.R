# Load packages ----------------------------------------

options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors
library(ggplot2)
library(psych)
library(dplyr)
library(zoo)
library(Hmisc)

# Load data --------------------------------------------

load("~/R/EMA/data/cleaned/cmu/cmu2_clean.Rda")
cmu2_exp <- cmu2_clean
rm(cmu2_clean)

# Explore periods of the study -------------------------

cmu2_dates_summary <-
  cmu2_exp[cmu2_exp$id == 200,c(2,5)] %>% group_by(week) %>%
  summarize_all(list(min = min, max = max))

cmu2_dates_summary <- rename(cmu2_dates_summary, Week = week, Start = min, End = max)
cmu2_dates_summary$Week <- cmu2_dates_summary$Week + 1
cmu2_dates_summary$Start <- format(cmu2_dates_summary$Start,'%B %d')
cmu2_dates_summary$End <- format(cmu2_dates_summary$End,'%B %d')

print(cmu2_dates_summary)

# Explore missing values -------------------------------

# Add missed questions variable

cmu2_exp$missed <- ifelse(rowSums(is.na(cmu2_exp[,c(6:12,20)])) == 8, 1, 0)

# Create data frame with missed values by EMA measurement

cmu2_missing <- cmu2_exp[,c(6:12,20:22)] %>% 
  group_by(ema_index) %>%
  dplyr::summarize(n_missed = sum(missed))

cmu2_missing$share_missed <- 100*cmu2_missing$n_missed/length(unique(cmu2_exp$id))

# Find min, max and mean of completed EMA prompts
psych::describe(100 - cmu2_missing$share_missed)

# Plot % of people who missed EMA prompts

ggplot(cmu2_missing, aes(x = ema_index, y = share_missed)) +
  geom_point() +
  ylab("% of participants") +
  xlab("Time") +
  ggtitle("% of participats who missed prompts across time") +
  scale_x_continuous(breaks = c(14,42,70),
                     labels = c("Week 1", "Week 2", "Week 3"))

# Explore patterns in DVs of a random participant ------

cmu2_p1 <- subset(cmu2_exp, id == sample(unique(cmu2_exp$id), 1))  # Subset random participant's data
cat("ID of randomly selected participant:", unique(cmu2_p1$id))

# Loneliness

ggplot(cmu2_p1,
       mapping =  aes(x = time, y = feel_lonely)) + 
  geom_point() +  geom_step() + 
  ylab("Loneliness") +
  xlab("Time") +
  scale_x_continuous() +
  facet_wrap(~ day, nrow = 3) +
  ggtitle("Random participant's daily changes in loneliness")

# Connection

ggplot(cmu2_p1,
       mapping =  aes(x = time, y = feel_connected)) + 
  geom_point() +  geom_step() + 
  ylab("Connection") +
  xlab("Time") +
  scale_x_continuous() +
  facet_wrap(~ day, nrow = 3) +
  ggtitle("Random participant's daily changes in connection")

# Depression

ggplot(cmu2_p1,
       mapping = aes(x = time, y = feel_depressed)) + 
  geom_point() + geom_step() + 
  ylab("Depression") +
  xlab("Time") +
  scale_x_continuous() +
  facet_wrap(~ day, nrow = 3) +
  ggtitle("Random participant's daily changes in depression")

# Mindfulness

ggplot(cmu2_p1,
       mapping = aes(x = time, y = mindfulness)) +
  geom_point() + geom_step() + 
  ylab("Mindfulness") +
  xlab("Time") +
  scale_x_continuous() +
  facet_wrap(~ day, nrow = 3) +
  ggtitle("Random participant's daily changes in mindfulness")

# Descriptive stats ------------------------------------

cmu2_descriptive <- cmu2_exp[,c(1,6:20)] %>%
  group_by(id) %>%
  summarize_all(list(mean = mean), na.rm = TRUE)

# Descriptive stats
psych::describe(cmu2_descriptive$feel_lonely_mean)
psych::describe(cmu2_descriptive$feel_connected_mean)
psych::describe(cmu2_descriptive$feel_depressed_mean)
psych::describe(cmu2_descriptive$mindfulness_mean)

# Correlation
rcorr(as.matrix(cmu2_descriptive[,c(3,6,7,16)]))

# Explore patterns in DVs of all participants ----------

# Create data frame with scores averaged across participants at each time point
# and standardized scores

cmu2_average <- cmu2_exp[,c(6:11,20,21)] %>%
  group_by(ema_index) %>%
  summarize_all(list(mean = mean), na.rm = TRUE)

# Add standardized variables

cmu2_average <- cmu2_average %>%
  mutate_at(vars(starts_with("feel"), starts_with("mindfulness")),
            list(z = ~as.vector(scale(.))))

# Calculate rolling means

feel_lonely_temp_zoo <- zoo(cmu2_average$feel_lonely_mean_z, cmu2_average$ema_index)  # Loneliness
feel_lonely_m_av1 <- rollmean(feel_lonely_temp_zoo, 3, fill = list(NA, NULL, NA))
cmu2_average$feel_lonely_amb_av = coredata(feel_lonely_m_av1)

feel_connected_temp_zoo <- zoo(cmu2_average$feel_connected_mean_z, cmu2_average$ema_index)  # Connection
feel_connected_m_av1 <- rollmean(feel_connected_temp_zoo, 3, fill = list(NA, NULL, NA))
cmu2_average$feel_connected_amb_av = coredata(feel_connected_m_av1)

feel_depressed_temp_zoo <- zoo(cmu2_average$feel_depressed_mean_z, cmu2_average$ema_index)  # Depression
feel_depressed_m_av1 <- rollmean(feel_depressed_temp_zoo, 3, fill = list(NA, NULL, NA))
cmu2_average$feel_depressed_amb_av = coredata(feel_depressed_m_av1)

mindfulness_temp_zoo <- zoo(cmu2_average$mindfulness_mean_z, cmu2_average$ema_index)  # Mindfulness
mindfulness_m_av1 <- rollmean(mindfulness_temp_zoo, 3, fill = list(NA, NULL, NA))
cmu2_average$mindfulness_amb_av = coredata(mindfulness_m_av1)

# Plot rolling mean of loneliness, connection, and mindfulness

ggplot(cmu2_average, aes(x = ema_index)) +
  geom_line(aes(y = feel_lonely_amb_av, color = "Loneliness")) +
  geom_line(aes(y = feel_connected_amb_av, color = "Connection")) +
  geom_line(aes(y = mindfulness_amb_av, color = "Mindfulness")) +
  ggtitle("Loneliness, connection, and mindfulness across time") +
  scale_x_continuous(breaks = c(14,42,70),
                     labels = c("Week 1", "Week 2", "Week 3")) +
  ylab("Variable (Z scores)") +
  xlab("Time") +
  scale_colour_manual("", 
                      breaks = c("Loneliness",
                                 "Connection",
                                 "Mindfulness"),
                      values = c("Loneliness"="navyblue",
                                 "Connection"="orange2",
                                 "Mindfulness"="turquoise4"))

# Plot rolling mean of depression and mindfulness

ggplot(cmu2_average, aes(x = ema_index)) +
  geom_line(aes(y = feel_depressed_amb_av, color = "Depression")) +
  geom_line(aes(y = mindfulness_amb_av, color = "Mindfulness")) +
  ggtitle("Depression and mindfulness across time") +
  scale_x_continuous(breaks = c(14,42,70),
                     labels = c("Week 1", "Week 2", "Week 3")) +
  ylab("Variable (Z scores)") +
  xlab("Time") +
  scale_colour_manual("", 
                      breaks = c("Depression",
                                 "Mindfulness"),
                      values = c("Depression"="black",
                                 "Mindfulness"="turquoise4"))

# Plot mindwandering variable

cmu2_mindwandering <- cmu2_exp %>%
  group_by(ema_index) %>%
  summarize(mindwandering_n = sum(mindwandering, na.rm = TRUE))

cmu2_mindwandering$mindwandering_share <-
  100*cmu2_mindwandering$mindwandering_n/length(unique(cmu2_exp$id))

min(cmu2_mindwandering$mindwandering_share)
max(cmu2_mindwandering$mindwandering_share)

ggplot(cmu2_mindwandering, aes(x = ema_index, y = mindwandering_share)) +
  geom_point(color = "palevioletred3") +
  ylab("% of participants") +
  xlab("Time") +
  ggtitle("% of participats who reported mind wandering") +
  scale_x_continuous(breaks = c(14,42,70),
                     labels = c("Week 1", "Week 2", "Week 3"))

ggplot(cmu2_exp, aes(x = ema_index, y = mindwandering)) +
  geom_smooth(method = "loess", span = .05, se = FALSE, colour = "palevioletred3") +
  ylab("Mind wandering (0 to 1)") +
  xlab("Time") +
  ggtitle("Mind wandering across time") +
  scale_y_discrete(breaks = c(0,0.5,1)) +
  scale_x_continuous(breaks = c(14,42,70),
                     labels = c("Week 1", "Week 2", "Week 3"))

# Analyze social interaction variables -----------------

# Interactions diversity: daily

# Plot

# Interactions diversity: weekly

# Plot

# Interaction quality
