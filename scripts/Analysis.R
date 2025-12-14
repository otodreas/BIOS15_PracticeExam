# =====================
# Configure environment
# =====================

# Clear variables
rm(list = ls())

# Attach packages
library(glmmTMB)
library(here)
library(patchwork)
library(performance)
library(rlang)
library(tidyverse)


# ================
# Define functions
# ================

# Define function to read and process metadata
read_metadata <- function(mdata_path, rawdata_df) {
  mdata_df <- read.csv(mdata_path)
  
  # Update mismatched names
  idx <- names(rawdata_df) != mdata_df[, 1]
  mdata_df[idx, 1] <- names(rawdata_df)[idx]
  
  # Convert metadata to list for easy lookup
  setNames(as.list(mdata_df[[2]]), mdata_df[[1]])
}

# Define function to generate predictions
predict_custom <- function(m1, m2, col_a, col_b, data) {
  # "Freeze" column names
  col_a <- enquo(col_a)
  col_b <- enquo(col_b)

  # Get columns as vectors
  a <- eval_tidy(col_a, data)
  b <- eval_tidy(col_b, data)
  
  # Generate sequence to predict on
  x <- seq(0, 100)
  
  # Get model coefficients
  cf_tall <- summary(m1)$coefficients$cond[, 1]
  cf_short <- summary(m2)$coefficients$cond[, 1]
  
  # Generate predictions for one variable while holding the other constant at its mean
  euc_tall_annual_pred <- exp(cf_tall[1] + x * cf_tall[2] + mean(a) * cf_tall[3])
  euc_tall_perennial_pred <- exp(cf_tall[1] + mean(b) * cf_tall[2] + x * cf_tall[3])
  
  euc_short_annual_pred <- exp(cf_short[1] + x * cf_short[2] + mean(a) * cf_short[3])
  euc_short_perennial_pred <- exp(cf_short[1] + mean(b) * cf_short[2] + x * cf_short[3])
  
  # Return dataframe with predictions
  lines = data.frame(
    x,
    euc_tall_annual_pred, euc_tall_perennial_pred,
    euc_short_annual_pred, euc_short_perennial_pred
  )
}

group_model_table1 <- function(m1, m2, mname1, mname2) {
  prop_var <- c(icc(m1)[2], icc(m2)[2])
  response <- c(mname1, mname2)
  table <- data.frame(c(response, prop_var))
  print(table)
  write.csv(table, here("outputs", "group_model_table.csv"))
}

group_model_table2 <- function(mname1, mname2) {
  intercepts1 <- summary(exp(coef(m_tall)$cond$Property[,1]))[-4]
  intercepts2 <- summary(exp(coef(m_short)$cond$Property[,1]))[-4]
  
  table <- data.frame(rbind(intercepts1, intercepts2))
  table$model <- c(mname1, mname2)
  write.csv(table, here("outputs", "group_model_table2.csv"))
}

# Define funciton to create table
tables_custom <- function(predictor1, predictor2, response1, response2, data) {
  
  # "Freeze" column names
  predictor1 <- enquo(predictor1)
  predictor2 <- enquo(predictor2)
  response1 <- enquo(response1)
  response2 <- enquo(response2)
  
  # Get columns as vectors
  p1 <- eval_tidy(predictor1, data)
  p2 <- eval_tidy(predictor2, data)
  r1 <- eval_tidy(response1, data)
  r2 <- eval_tidy(response2, data)
  
  table1 <- data.frame()
  table2 <- data.frame()

  means <- c(mean())
  write.csv(table, here("outputs", "table.csv"))
}


# ==============
# Run operations
# ==============

# Read data and metadata
d <- as_tibble(read.csv(here("data", "exam2023_data-2.csv"))) |>
  drop_na()
mdata <- read_metadata(here("data", "exam2023_metadata-2.csv"), d)

# Make new "tall" and "short" columns
d$tall <- d$euc_sdlgs50cm.2m + d$euc_sdlgs.2m
d$short <- d$euc_sdlgs0_50cm

# Fit models
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
        
# Generate predictions
preds <- predict_custom(m_tall, m_short, ExoticPerennialGrass_cover, ExoticAnnualGrass_cover, d)
        
# Create tables
group_model_table1(m_short, m_tall, "Number of eucalypt seedlings <50 cm/quadrat", "Number of eucalypt seedlings >50 cm/quadrat")
group_model_table2("Tall", "Short")

# Plot tall plant frequency over exotic annual grass cover with best fit lines
p_annual_tall <- ggplot(d) +
  geom_point(aes(ExoticAnnualGrass_cover, tall), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_tall_annual_pred)) +  # Perennial cover = mean
  ggtitle("A") +
  labs(x = "Exotic annual grass cover (%)", y = "Eucalypt seedlings\n> 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()
  
# Plot short plant frequency over exotic annual grass cover with best fit lines
p_annual_short <- ggplot(d) +
  geom_point(aes(ExoticAnnualGrass_cover, short), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_short_annual_pred)) +  # Perennial cover = mean
  ggtitle("B") +
  labs(x = "Exotic annual grass cover (%)", y = "Eucalypt seedlings\n< 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()

# Plot tall plant frequency over exotic annual grass cover with best fit lines
p_perennial_tall <- ggplot(d) +
  geom_point(aes(ExoticPerennialGrass_cover, tall), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_tall_perennial_pred)) +  # Annual cover = mean
  ggtitle("C") +
  labs(x = "Exotic perennial grass cover (%)", y = "Eucalypt seedlings\n> 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()
  
# Plot short plant frequency over exotic annual grass cover with best fit lines
p_perennial_short <- ggplot(d) +
  geom_point(aes(ExoticPerennialGrass_cover, short), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_short_perennial_pred)) +  # Annual cover = mean
  ggtitle("D") +
  labs(x = "Exotic perennial grass cover (%)", y = "Eucalypt seedlings\n< 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()

# Create plot layout
(p_annual_tall | p_perennial_tall) /
(p_annual_short | p_perennial_short)
