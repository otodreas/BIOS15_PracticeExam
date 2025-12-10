# Clear variables
rm(list = ls())

# Mount libraries
library(tidyverse)
library(here)

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
d <- as_tibble(read.csv(here("data", "exam2023_data-2.csv")))
mdata <- read_metadata(d, here("data", "exam2023_metadata-2.csv"))