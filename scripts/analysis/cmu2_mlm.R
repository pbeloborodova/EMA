# Load packages ----------------------------------------

options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors
library(lme4)
library(lmerTest)
library(nlme)

# Load data --------------------------------------------

load("~/R/EMA/data/cleaned/cmu/cmu2_clean.Rda")
cmu2_mlm <- cmu2_clean
rm(cmu2_clean)

# Transform time varaibles -----------------------------

# Add sine and cosine terms for weekly cyclicity
cmu2_mlm$weekcycle_sin <- sin(2*pi*(cmu2_mlm$day + 1)/7)
cmu2_mlm$weekcycle_cos <- cos(2*pi*(cmu2_mlm$day + 1)/7)

# Add continuous time within person variable
cmu2_mlm <- cmu2_mlm %>% group_by(id) %>%
  mutate(ema_timecont = date - date[1],
         ema_timecont_cent = ema_timecont - mean(ema_timecont))  # Center within person

# Transform mindfulness variable -----------------------

cmu2_mlm <- cmu2_mlm %>% group_by(id) %>%
  mutate(mindfulness_cent = mindfulness - mean(mindfulness, na.rm = TRUE))  # Center within person

# MLM models -------------------------------------------

# Function for calculting % of variance between and within cases

icc_between <-
  function(model) {
    var_df <- as.data.frame(VarCorr(model, comp = "Variance"))
    result <- round(100*var_df[1,4] / (var_df[1,4] + var_df[var_df$grp == "Residual",4]), 2)
    cat("Variance between groups:", result, "%,",
        "variance within groups:", 100 - result, "%")
  }

# Function for calculating decrease of residual variance

decr_var <-
  function(model1, model2) {
    var1_df <- as.data.frame(VarCorr(model1, comp = "Variance"))
    var2_df <- as.data.frame(VarCorr(model2, comp = "Variance"))
    result <- round(100*(var1_df[var1_df$grp == "Residual",4] - var2_df[var2_df$grp == "Residual",4]) / var1_df[var1_df$grp == "Residual",4], 2)
    cat("Decrease in residual variance explained by additional predictors:", result, "%,")
  }

# (1) Loneliness

# (1.1) Unconditional

# Variance between and within participants
cmu2_lonely_null <- lme4::lmer(feel_lonely ~ 1 + (1|id),
                              REML = TRUE, data = cmu2_mlm)
summary(cmu2_lonely_null)
rand(cmu2_lonely_null)
icc_between(cmu2_lonely_null)

# Unstructured
cmu2_lonely_null_uc <- nlme::lme(feel_lonely ~ 1,
                                random = list(~1|id),
                                method = "REML",
                                data = cmu2_mlm,
                                na.action = na.exclude)

# Compound symmetry
cmu2_lonely_null_cs <- nlme::lme(feel_lonely ~ 1,
                                random = list(~1|id),
                                method = "REML",
                                data = cmu2_mlm,
                                na.action = na.exclude,
                                correlation = corCompSymm(form = ~1|id))

# Autoregressive
cmu2_lonely_null_ar <- nlme::lme(feel_lonely ~ 1,
                                random = list(~1|id),
                                method = "REML",
                                data = cmu2_mlm,
                                na.action = na.exclude,
                                correlation = corAR1(form = ~1|id))

# Compare models
anova(cmu2_lonely_null_uc, cmu2_lonely_null_cs, cmu2_lonely_null_ar)

# (1.2) Time variables as predictors

cmu2_lonely_full1 <- lme4::lmer(feel_lonely ~ 1 + ema_timecont_cent +
                                 weekcycle_cos + day + time +
                                 (1|id), REML = TRUE, data = cmu2_mlm)
summary(cmu2_lonely_full1)
rand(cmu2_lonely_full1)
decr_var(cmu2_lonely_null, cmu2_lonely_full1)
tab_model(cmu2_lonely_full1)

# (1.3) Time variables + mindfulness as predictors

cmu2_lonely_full2 <- lme4::lmer(feel_lonely ~ 1 + ema_timecont_cent +
                                 weekcycle_cos + day + time + mindfulness_cent +
                                 (1|id), REML = TRUE, data = cmu2_mlm)
summary(cmu2_lonely_full2)
rand(cmu2_lonely_full2)
decr_var(cmu2_lonely_full1, cmu2_lonely_full2)
tab_model(cmu2_lonely_full2)

# (2) Connection

# (2.1) Unconditional

# Variance between and within participants
cmu2_connected_null <- lme4::lmer(feel_connected ~ 1 + (1|id),
                               REML = TRUE, data = cmu2_mlm)
summary(cmu2_connected_null)
rand(cmu2_connected_null)
icc_between(cmu2_connected_null)

# Unstructured
cmu2_connected_null_uc <- nlme::lme(feel_connected ~ 1,
                                 random = list(~1|id),
                                 method = "REML",
                                 data = cmu2_mlm,
                                 na.action = na.exclude)

# Compound symmetry
cmu2_connected_null_cs <- nlme::lme(feel_connected ~ 1,
                                 random = list(~1|id),
                                 method = "REML",
                                 data = cmu2_mlm,
                                 na.action = na.exclude,
                                 correlation = corCompSymm(form = ~1|id))

# Autoregressive
cmu2_connected_null_ar <- nlme::lme(feel_connected ~ 1,
                                 random = list(~1|id),
                                 method = "REML",
                                 data = cmu2_mlm,
                                 na.action = na.exclude,
                                 correlation = corAR1(form = ~1|id))

# Compare models
anova(cmu2_connected_null_uc, cmu2_connected_null_cs, cmu2_connected_null_ar)

# (2.2) Time variables as predictors

cmu2_connected_full1 <- lme4::lmer(feel_connected ~ 1 + ema_timecont_cent +
                                  weekcycle_cos + day + time +
                                  (1|id), REML = TRUE, data = cmu2_mlm)
summary(cmu2_connected_full1)
rand(cmu2_connected_full1)
decr_var(cmu2_connected_null, cmu2_connected_full1)
tab_model(cmu2_connected_full1)

# (2.3) Time variables + mindfulness as predictors

cmu2_connected_full2 <- lme4::lmer(feel_connected ~ 1 + ema_timecont_cent +
                                  weekcycle_cos + day + time + mindfulness_cent +
                                  (1|id), REML = TRUE, data = cmu2_mlm)
summary(cmu2_connected_full2)
rand(cmu2_connected_full2)
decr_var(cmu2_connected_full1, cmu2_connected_full2)
tab_model(cmu2_connected_full2)

# (4) Mindfulness

# (2.1) Unconditional

# Variance between and within participants
cmu2_mindfulness_null <- lme4::lmer(mindfulness ~ 1 + (1|id),
                                  REML = TRUE, data = cmu2_mlm)
summary(cmu2_mindfulness_null)
rand(cmu2_mindfulness_null)
icc_between(cmu2_mindfulness_null)

# Unstructured
cmu2_mindfulness_null_uc <- nlme::lme(mindfulness ~ 1,
                                    random = list(~1|id),
                                    method = "REML",
                                    data = cmu2_mlm,
                                    na.action = na.exclude)

# Compound symmetry
cmu2_mindfulness_null_cs <- nlme::lme(mindfulness ~ 1,
                                    random = list(~1|id),
                                    method = "REML",
                                    data = cmu2_mlm,
                                    na.action = na.exclude,
                                    correlation = corCompSymm(form = ~1|id))

# Autoregressive
cmu2_mindfulness_null_ar <- nlme::lme(mindfulness ~ 1,
                                    random = list(~1|id),
                                    method = "REML",
                                    data = cmu2_mlm,
                                    na.action = na.exclude,
                                    correlation = corAR1(form = ~1|id))

# Compare models
anova(cmu2_mindfulness_null_uc, cmu2_mindfulness_null_cs, cmu2_mindfulness_null_ar)

# (2.2) Time variables as predictors

cmu2_mindfulness_full <- lme4::lmer(mindfulness ~ 1 + ema_timecont_cent +
                                     weekcycle_cos + day + time +
                                     (1|id), REML = TRUE, data = cmu2_mlm)
summary(cmu2_mindfulness_full)
rand(cmu2_mindfulness_full)
tab_model(cmu2_mindfulness_full)
decr_var(cmu2_mindfulness_null, cmu2_mindfulness_full)


