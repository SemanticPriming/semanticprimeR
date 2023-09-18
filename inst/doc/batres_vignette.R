## ----setup, include = FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette-setup--------------------------------
knitr::opts_chunk$set(echo = TRUE)

# Set a random seed
set.seed(5989320)

# Libraries necessary for this vignette
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(psych)
library(semanticprimeR)

# Function for simulation
item_power <- function(data, # name of data frame
                       dv_col, # name of DV column as a character
                       item_col, # number of items column as a character
                       sample_start = 20, 
                       sample_stop = 200, 
                       sample_increase = 5,
                       decile = .5){
  
  DF <- cbind.data.frame(
    "dv" = data[ , dv_col],
    "items" = data[ , item_col]
  )
  
  # just in case
  colnames(DF) <- c("dv", "items")
  
  # figure out the "sufficiently narrow" ci value
  SE <- tapply(DF$dv, DF$items, function (x) { sd(x)/sqrt(length(x)) })
  cutoff <- quantile(SE, probs = decile)
  
  # sequence of sample sizes to try
  samplesize_values <- seq(sample_start, sample_stop, sample_increase)

  # create a blank table for us to save the values in 
  sim_table <- matrix(NA, 
                      nrow = length(samplesize_values), 
                      ncol = length(unique(DF$items)))

  # make it a data frame
  sim_table <- as.data.frame(sim_table)

  # add a place for sample size values 
  sim_table$sample_size <- NA

  # loop over sample sizes
  for (i in 1:length(samplesize_values)){
      
    # temp that samples and summarizes
    temp <- DF %>% 
      group_by(items) %>% 
      sample_n(samplesize_values[i], replace = T) %>% 
      summarize(se = sd(dv)/sqrt(length(dv)))
    
    # dv on items
    colnames(sim_table)[1:length(unique(DF$items))] <- temp$items
    sim_table[i, 1:length(unique(DF$items))] <- temp$se
    sim_table[i, "sample_size"] <- samplesize_values[i]
    
  }

  # figure out cut off
  final_sample <- sim_table %>% 
    pivot_longer(cols = -c(sample_size)) %>% 
    rename(item = name, se = value) %>% 
    group_by(sample_size) %>% 
    summarize(percent_below = sum(se <= cutoff)/length(unique(DF$items))) 
  
  # multiply by correction 
  final_sample$new_sample <- round(39.369 + 0.700*final_sample$sample_size + 0.003*cutoff - 0.694*length(unique(DF$items)))
  
  return(list(
    SE = SE, 
    cutoff = cutoff, 
    DF = DF, 
    sim_table = sim_table, 
    final_sample = final_sample
  ))

}

## --------------------------------------------------
DF <- import("data/batres_data.sav")

str(DF)

## --------------------------------------------------
metadata <- import("data/batres_metadata.xlsx")

flextable(metadata) %>% autofit()

## --------------------------------------------------
# Reformat the data
DF_long <- pivot_longer(DF, cols = -c(Participant_Number)) %>% 
  rename(item = name, score = value)

flextable(head(DF_long)) %>% autofit()

## --------------------------------------------------
# Function for simulation
var1 <- item_power(data = DF_long, # name of data frame
            dv_col = "score", # name of DV column as a character
            item_col = "item", # number of items column as a character
            sample_start = 20, 
            sample_stop = 300, 
            sample_increase = 5,
            decile = .4)

## --------------------------------------------------
# individual SEs
var1$SE

var1$cutoff

## --------------------------------------------------
cutoff <- calculate_cutoff(population = DF_long, 
                           grouping_items = "item",
                           score = "score",
                           minimum = 1,
                           maximum = 7)
# showing how this is the same as the person calculated version versus semanticprimeR's function
cutoff$cutoff

## --------------------------------------------------
final_table <- calculate_correction(
  proportion_summary = var1$final_sample,
  pilot_sample_size = nrow(DF),
  proportion_variability = cutoff$prop_var
  )

flextable(final_table) %>% 
  autofit()

