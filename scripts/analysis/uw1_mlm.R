# Adjust settings and load packages --------------------

options(scipen=99)  # Change scientific notation to decimals
options(stringsAsFactors = FALSE)  # Stop conversion of strings to factors
library(lme4)
library(lmerTest)
library(nlme)

# Load data --------------------------------------------

load("~/R/EMA/data/cleaned/uw/uw1_clean.Rda")
uw1_mlm <- uw1_clean[,-c(6,8,9,11)]  # Select variables

# Transform time varaibles -----------------------------

# Add sine and cosine terms for weekly cyclicity
uw1_mlm$weekcycle_sin <- sin(2*pi*(uw1_mlm$day + 1)/7)
uw1_mlm$weekcycle_cos <- cos(2*pi*(uw1_mlm$day + 1)/7)

# Add squared day


# MLM models: social connection ------------------------

# (1) Loneliness (time variables as predictors)

# Unstructured
uw1_lonely_uc <- nlme::lme(feel_lonely ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm)

# Compound symmetry
uw1_lonely_cs <- nlme::lme(feel_lonely ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm,
                           correlation = corCompSymm(form = ~1|id))

# Autoregressive structure
uw1_lonely_ar <- nlme::lme(feel_lonely ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm,
                           correlation = corAR1(form = ~1|id))

# Compare models
anova(uw1_lonely_uc, uw1_lonely_cs)
anova(uw1_lonely_uc, uw1_lonely_ar)

# lme4 package

uw1_lonely_lme4 <- lme4::lmer(feel_lonely ~ 1 + weekcycle_cos + day + day*day + time +
                          (1|id), REML = TRUE, data = uw1_mlm)
summary(uw1_lonely_lme4)
rand(uw1_lonely_lme4)

# Summary of model with autoregressive variance-covariance structure (best fit)

summary(uw1_lonely_ar)

# (2) Connection (time variables as predictors)

# Unstructured
uw1_connected_uc <- nlme::lme(feel_connected ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm)

# Compound symmetry
uw1_connected_cs <- nlme::lme(feel_connected ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm,
                           correlation = corCompSymm(form = ~1|id))

# Autoregressive structure
uw1_connected_ar <- nlme::lme(feel_connected ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm,
                           correlation = corAR1(form = ~1|id))

# Compare models
anova(uw1_connected_uc, uw1_connected_cs)
anova(uw1_connected_uc, uw1_connected_ar)

# lme4 package

uw1_connected_lme4 <- lme4::lmer(feel_connected ~ 1 + weekcycle_cos + day + day*day + time +
                                (1|id), REML = TRUE, data = uw1_mlm)
summary(uw1_connected_lme4)
rand(uw1_connected_lme4)

# Summary of model with autoregressive variance-covariance structure (best fit)

summary(uw1_connected_ar)

# MLM models: depression -------------------------------

# Unstructured
uw1_depressed_uc <- nlme::lme(feel_depressed ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm)

# Compound symmetry
uw1_depressed_cs <- nlme::lme(feel_depressed ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm,
                           correlation = corCompSymm(form = ~1|id))

# Autoregressive structure
uw1_depressed_ar <- nlme::lme(feel_depressed ~ 1 + weekcycle_cos + day + day*day + time,
                           random = list(~1|id),
                           method = "REML",
                           na.action = na.omit,
                           data = uw1_mlm,
                           correlation = corAR1(form = ~1|id))

# Compare models
anova(uw1_depressed_uc, uw1_depressed_cs)
anova(uw1_depressed_uc, uw1_depressed_ar)

# lme4 package

uw1_depressed_lme4 <- lme4::lmer(feel_depressed ~ 1 + weekcycle_cos + day + day*day + time +
                                (1|id), REML = TRUE, data = uw1_mlm)
summary(uw1_depressed_lme4)
rand(uw1_depressed_lme4)

# Summary of model with autoregressive variance-covariance structure (best fit)

summary(uw1_depressed_ar)
