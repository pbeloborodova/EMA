# Adjust settings and load packages --------------------

options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors
library(lme4)
library(lmerTest)
library(nlme)
library(sjPlot)

# Load data --------------------------------------------

load("~/R/EMA/data/cleaned/uw/uw1_clean.Rda")
uw1_mlm <- uw1_clean[,-c(6,8,9,11)]  # Select variables
rm(uw1_clean)

# Transform time varaibles -----------------------------

# Add sine and cosine terms for weekly cyclicity
uw1_mlm$weekcycle_sin <- sin(2*pi*(uw1_mlm$day + 1)/7)
uw1_mlm$weekcycle_cos <- cos(2*pi*(uw1_mlm$day + 1)/7)

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
uw1_lonely_null <- lme4::lmer(feel_lonely ~ 1 + (ema_index|id),
                              REML = TRUE, data = uw1_mlm)
summary(uw1_lonely_null)
rand(uw1_lonely_null)
icc_between(uw1_lonely_null)

# Unstructured
uw1_lonely_null_uc <- nlme::lme(feel_lonely ~ 1,
                                random = list(~ema_index|id),
                                method = "REML",
                                data = uw1_mlm,
                                na.action = na.exclude)
summary(uw1_lonely_null_uc)

# Compound symmetry
uw1_lonely_null_cs <- nlme::lme(feel_lonely ~ 1,
                                random = list(~ema_index|id),
                                method = "REML",
                                data = uw1_mlm,
                                na.action = na.exclude,
                                correlation = corCompSymm(form = ~ema_index|id))

# Autoregressive
uw1_lonely_null_ar <- nlme::lme(feel_lonely ~ 1,
                                random = list(~ema_index|id),
                                method = "REML",
                                data = uw1_mlm,
                                na.action = na.exclude,
                                correlation = corAR1(form = ~ema_index|id))

# Compare models
anova(uw1_lonely_null_uc, uw1_lonely_null_cs, uw1_lonely_null_ar)

# (1.2) Time variables as predictors

uw1_lonely_full <- lme4::lmer(feel_lonely ~ 1 + weekcycle_cos + day + time +
                          (ema_index|id), REML = TRUE, data = uw1_mlm)
summary(uw1_lonely_full)
rand(uw1_lonely_full)
tab_model(uw1_lonely_full)

decr_var(uw1_lonely_null, uw1_lonely_full)

# (2) Connection

# (2.1) Unconditional

# Variance between and within participants
uw1_connected_null <- lme4::lmer(feel_connected ~ 1 + (ema_index|id),
                              REML = TRUE, data = uw1_mlm)
summary(uw1_connected_null)
rand(uw1_connected_null)
icc_between(uw1_connected_null)

# Unstructured
uw1_connected_null_uc <- nlme::lme(feel_connected ~ 1,
                                random = list(~ema_index|id),
                                method = "REML",
                                data = uw1_mlm,
                                na.action = na.exclude)

# Compound symmetry
uw1_connected_null_cs <- nlme::lme(feel_connected ~ 1,
                                random = list(~ema_index|id),
                                method = "REML",
                                data = uw1_mlm,
                                na.action = na.exclude,
                                correlation = corCompSymm(form = ~ema_index|id))

# Autoregressive
uw1_connected_null_ar <- nlme::lme(feel_connected ~ 1,
                                random = list(~ema_index|id),
                                method = "REML",
                                data = uw1_mlm,
                                na.action = na.exclude,
                                correlation = corAR1(form = ~ema_index|id))

# Compare models
anova(uw1_connected_null_uc, uw1_connected_null_cs, uw1_connected_null_ar)

# (2.2) Time variables as predictors

uw1_connected_full <- lme4::lmer(feel_connected ~ 1 + weekcycle_cos + day + time +
                                (ema_index|id), REML = TRUE, data = uw1_mlm)
summary(uw1_connected_full)
rand(uw1_connected_full)
tab_model(uw1_connected_full)

decr_var(uw1_connected_null, uw1_connected_full)

# (3) Depression

# (2.1) Unconditional

# Variance between and within participants
uw1_depressed_null <- lme4::lmer(feel_depressed ~ 1 + (ema_index|id),
                                 REML = TRUE, data = uw1_mlm)
summary(uw1_depressed_null)
rand(uw1_depressed_null)
icc_between(uw1_depressed_null)

# Unstructured
uw1_depressed_null_uc <- nlme::lme(feel_depressed ~ 1,
                                   random = list(~ema_index|id),
                                   method = "REML",
                                   data = uw1_mlm,
                                   na.action = na.exclude)

# Compound symmetry
uw1_depressed_null_cs <- nlme::lme(feel_depressed ~ 1,
                                   random = list(~ema_index|id),
                                   method = "REML",
                                   data = uw1_mlm,
                                   na.action = na.exclude,
                                   correlation = corCompSymm(form = ~ema_index|id))

# Autoregressive
uw1_depressed_null_ar <- nlme::lme(feel_depressed ~ 1,
                                   random = list(~ema_index|id),
                                   method = "REML",
                                   data = uw1_mlm,
                                   na.action = na.exclude,
                                   correlation = corAR1(form = ~ema_index|id))

# Compare models
anova(uw1_depressed_null_uc, uw1_depressed_null_cs, uw1_depressed_null_ar)

# (2.2) Time variables as predictors

uw1_depressed_full <- lme4::lmer(feel_depressed ~ 1 + weekcycle_cos + day + time +
                                   (ema_index|id), REML = TRUE, data = uw1_mlm)
summary(uw1_depressed_full)
rand(uw1_depressed_full)
tab_model(uw1_depressed_full)

decr_var(uw1_depressed_null, uw1_depressed_full)