# Clear variables
rm(list = ls())

# Attach packages
library(here)
library(tidyverse)
library(glmmTMB)
library(patchwork)

# Define function to read and process metadata
read_metadata <- function(rawdata_df, mdata_path) {
  mdata_df <- read.csv(mdata_path)
  
  # Update mismatched names
  idx <- names(rawdata_df) != mdata_df[, 1]
  mdata_df[idx, 1] <- names(rawdata_df)[idx]
  
  # Convert metadata to list for easy lookup
  setNames(as.list(mdata_df[[2]]), mdata_df[[1]])
}

# Read data and metadata
d <- as_tibble(read.csv(here("data", "exam2023_data-2.csv"))) %>%
  drop_na()
mdata <- read_metadata(d, here("data", "exam2023_metadata-2.csv"))

# Make new "tall" and "short" columns
d$tall <- d$euc_sdlgs50cm.2m + d$euc_sdlgs.2m
d$short <- d$euc_sdlgs0_50cm

# Negative binomial mixed-effect models
m_tall <- glmmTMB(
  tall ~
    ExoticAnnualGrass_cover +
    ExoticPerennialGrass_cover +
    (1 | Property),
  family = nbinom2, data = d
)

m_short <- glmmTMB(
  short ~
    ExoticAnnualGrass_cover +
    ExoticPerennialGrass_cover +
    (1 | Property),
  family = nbinom2, data = d
)

# Define function to generate predictions
predict_custom <- function() {
  # Generate sequence to predict on
  x <- seq(0, 100)

  # Get model coefficients
  cf_tall <- summary(m_tall)$coefficients$cond[, 1]
  cf_short <- summary(m_short)$coefficients$cond[, 1]
  
  # Generate predictions for one variable while holding the other constant at its mean, mean + sd, mean - sd
  euc_tall_pred <- exp(cf_tall[1] + x * cf_tall[2] + mean(d$ExoticPerennialGrass_cover) * cf_tall[3])
  euc_tall_plus_sd <- exp(cf_tall[1] + x * cf_tall[2] + (mean(d$ExoticPerennialGrass_cover) + sd(d$ExoticPerennialGrass_cover)) * cf_tall[3])
  euc_tall_minus_sd <- exp(cf_tall[1] + x * cf_tall[2] + (mean(d$ExoticPerennialGrass_cover) - sd(d$ExoticPerennialGrass_cover)) * cf_tall[3])
  
  euc_short_pred <- exp(cf_short[1] + x * cf_short[2] + mean(d$ExoticPerennialGrass_cover) * cf_short[3])
  euc_short_plus_sd <- exp(cf_short[1] + x * cf_short[2] + (mean(d$ExoticPerennialGrass_cover) + sd(d$ExoticPerennialGrass_cover)) * cf_short[3])
  euc_short_minus_sd <- exp(cf_short[1] + x * cf_short[2] + (mean(d$ExoticPerennialGrass_cover) - sd(d$ExoticPerennialGrass_cover)) * cf_short[3])

  # Return dataframe with predictions
  lines = data.frame(
    x,
    euc_tall_pred, euc_tall_plus_sd, euc_tall_minus_sd,
    euc_short_pred, euc_short_plus_sd, euc_short_minus_sd
  )
}

# Generate predictions
preds <- predict_custom()

# Plot tall plant frequency over exotic annual grass cover with best fit lines
p_tall <- ggplot(d) +
  geom_point(aes(ExoticAnnualGrass_cover, tall), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_tall_pred)) +  # Perennial cover = mean
  geom_line(data = preds, aes(x, euc_tall_plus_sd)) +  # Perennial cover = mean + sd
  geom_line(data = preds, aes(x, euc_tall_minus_sd)) +  # Perennial cover = mean - sd
  ggtitle("A") +
  labs(x = "Exotic annual grass cover (%)", y = "Eucalypt seedlings\n> 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()
  
# Plot short plant frequency over exotic annual grass cover with best fit lines
p_short <- ggplot(d) +
  geom_point(aes(ExoticAnnualGrass_cover, short), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_short_pred)) +  # Perennial cover = mean
  geom_line(data = preds, aes(x, euc_short_plus_sd)) +  # Perennial cover = mean + sd
  geom_line(data = preds, aes(x, euc_short_minus_sd)) +  # Perennial cover = mean - sd  # geom_line(data = linedf, aes(linex, liney)) +
  ggtitle("B") +
  labs(x = "Exotic annual grass cover (%)", y = "Eucalypt seedlings\n< 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()

# Create plot layout
p_tall / p_short
