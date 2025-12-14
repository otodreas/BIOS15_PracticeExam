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

# Get general summary statistics
general_table <- function() {
  # Create df to store stats in
  table <- data.frame(stats = c("mean", "sd"))
  
  # Create columns with mean and sd of each predictor and response
  table$annual <- c(mean(d$ExoticAnnualGrass_cover), sd(d$ExoticAnnualGrass_cover))
  table$perennial <- c(mean(d$ExoticPerennialGrass_cover), sd(d$ExoticPerennialGrass_cover))
  table$tall <- c(mean(d$tall), sd(d$tall))
  table$short <- c(mean(d$short), sd(d$short))

  # Pipe table into mutate and apply significant figure rounding
  table <- table |>
    mutate(across(where(is.numeric), ~ signif(.x, digits = 3)))

  # Write csv
  write.csv(table, here("outputs", "general_table.csv"))
}

# Create table with icc values to quantify %variance explained by random effects
icc_table <- function(m1, m2, mname1, mname2) {
  # Create a dataframe and store unadjusted iccs for each model in it
  table <- data.frame(unadj_icc = c(icc(m1)[, 2], icc(m2)[, 2]))
  table$model <- c(mname1, mname2)

  # Write csv
  write.csv(table, here("outputs", "icc.csv"))
}

# Create table with distributions of random effect model intercepts
group_dist <- function() {
  # Get summary of all intercepts in short and tall model, excluding mean
  short <- summary(exp(coef(m_short)$cond$Property[,1]))[-4]
  tall <- summary(exp(coef(m_tall)$cond$Property[,1]))[-4]

  # Insert each summary as a row in the dataframe table
  table <- data.frame(rbind(short, tall))

  # Pipe table into mutate and apply significant figure rounding
  table <- table |>
    mutate(across(where(is.numeric), ~ signif(.x, digits = 3)))

  # Write csv
  write.csv(table, here("outputs", "group_dist.csv"))
}

# Get % change in euc for each model range and write it into a text file
get_pred_ranges <- function() {
  # Get max and min for each prediction
  tamax <- preds$euc_tall_annual_pred[101]
  tamin <- preds$euc_tall_annual_pred[1]
  tpmax <- preds$euc_tall_perennial_pred[101]
  tpmin <- preds$euc_tall_perennial_pred[1]
  samax <- preds$euc_short_annual_pred[101]
  samin <- preds$euc_short_annual_pred[1]
  spmax <- preds$euc_short_perennial_pred[101]
  spmin <- preds$euc_short_perennial_pred[1]

  # Create lists and vectors to loop across when writing the text file
  params <- list(c(tamin, tamax), c(tpmin, tpmax), c(samin, samax), c(spmin, spmax))
  labels <- c("tall_annual: ", "tall_peren: ", "short_annual: ", "short_peren: ")

  # Produce the first line of the file containing an explanation of the file
  txt <- "Predicted change in response by predictor going from 0-100% for both models"

  # Loop through each combination of predictor and response, updating the text file with ranges and % change
  for (i in seq_along(labels)) {
    txt[i+1] <- (paste0(
      labels[i], signif(params[[i]][1], 2), "-", signif(params[[i]][2], 2), ", ",
      signif((params[[i]][2] - params[[i]][1]) / params[[i]][1] * 100, 2), "%"
    ))

  # Open, write, and close file
  writeLines(txt, here("outputs", "model_predictions.txt"))
  }
}




# ===========
# Run program
# ===========


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
        
# Create tables and model reports
general_table()
icc_table(m_short, m_tall, "Number of eucalypt seedlings <50 cm/quadrat", "Number of eucalypt seedlings >50 cm/quadrat")
group_dist()
get_pred_ranges()

# Plot tall plant frequency over exotic annual grass cover with best fit lines
p_annual_tall <- ggplot(d) +
  geom_point(aes(ExoticAnnualGrass_cover, tall), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_tall_annual_pred)) +  # Perennial cover = mean
  ggtitle("A") +
  labs(x = "Exotic annual grass cover (%)", y = "Eucalypt seedlings\n> 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()
  
# Plot tall plant frequency over exotic annual grass cover with best fit lines
p_perennial_tall <- ggplot(d) +
  geom_point(aes(ExoticPerennialGrass_cover, tall), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_tall_perennial_pred)) +  # Annual cover = mean
  ggtitle("B") +
  labs(x = "Exotic perennial grass cover (%)", y = "Eucalypt seedlings\n> 50 cm tall/quadrat") +
  xlim(0, 100) +
  ylim(0, 22) +
  theme_minimal()

# Plot short plant frequency over exotic annual grass cover with best fit lines
p_annual_short <- ggplot(d) +
  geom_point(aes(ExoticAnnualGrass_cover, short), alpha = 0.25) +
  geom_line(data = preds, aes(x, euc_short_annual_pred)) +  # Perennial cover = mean
  ggtitle("C") +
  labs(x = "Exotic annual grass cover (%)", y = "Eucalypt seedlings\n< 50 cm tall/quadrat") +
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
