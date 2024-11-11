## ----setup, include = FALSE---------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette-setup-----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# Set a random seed
set.seed(3898934)

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
                       nsim = 10, # small for cran 
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
                      nrow = length(samplesize_values)*nsim, 
                      ncol = length(unique(DF$items)))

  # make it a data frame
  sim_table <- as.data.frame(sim_table)

  # add a place for sample size values 
  sim_table$sample_size <- NA

  iterate <- 1
  
  for (p in 1:nsim){
    # loop over sample sizes
    for (i in 1:length(samplesize_values)){
        
      # temp that samples and summarizes
      temp <- DF %>% 
        group_by(items) %>% 
        sample_n(samplesize_values[i], replace = T) %>% 
        summarize(se = sd(dv)/sqrt(length(dv)))
      
      # dv on items
      colnames(sim_table)[1:length(unique(DF$items))] <- temp$items
      sim_table[iterate, 1:length(unique(DF$items))] <- temp$se
      sim_table[iterate, "sample_size"] <- samplesize_values[i]
      sim_table[iterate, "nsim"] <- p
      
      iterate <- iterate + 1
    }
  }

  # figure out cut off
  final_sample <- sim_table %>% 
    pivot_longer(cols = -c(sample_size, nsim)) %>% 
    dplyr::rename(item = name, se = value) %>% 
    group_by(sample_size, nsim) %>% 
    summarize(percent_below = sum(se <= cutoff)/length(unique(DF$items))) %>% 
    ungroup() %>% 
    # then summarize all down averaging percents
    dplyr::group_by(sample_size) %>% 
    summarize(percent_below = mean(percent_below)) %>% 
    dplyr::arrange(percent_below) %>% 
    ungroup()
  
  return(list(
    SE = SE, 
    cutoff = cutoff, 
    DF = DF, 
    sim_table = sim_table, 
    final_sample = final_sample
  ))

}

## -----------------------------------------------------------
#read in data
DF <- import("data/geller_data.xlsx") %>% 
  select(Experiment, Subject, `CueType[1Word,2Pic]`, Stimulus, EncodeJOL)
  
str(DF)

## -----------------------------------------------------------
metadata <- tibble::tribble(
             ~Variable.Name,                                                                  ~Variable.Description, ~`Type (numeric,.character,.logical,.etc.)`,
               "Experiment",                                                 "Experiment 1 (1) or 2 (2) ONLY USE 1",                                          NA,
                  "Subject",                                                                           "Subject ID",                                   "Numeric",
                  "CueType", "Whether participant was presented with word translation (1) or word with picture (2)",                                   "Numeric",
                 "Stimulus",                                                "Swahili words presented on each trail",                                 "Character",
                "EncodeJOL",                         "JOL (1-100) 1=not likely to recall 100=very likely to recall",                                   "Numeric"
             )

flextable(metadata) %>% autofit()

## -----------------------------------------------------------
DF <- DF %>% 
  filter(Experiment == 1) %>%
  filter(!is.na(EncodeJOL))

# Function for simulation
var1 <- item_power(data = DF, # name of data frame
            dv_col = "EncodeJOL", # name of DV column as a character
            item_col = "Stimulus", # number of items column as a character
            nsim = 10,
            sample_start = 20, 
            sample_stop = 100, 
            sample_increase = 5,
            decile = .4)

## -----------------------------------------------------------
# individual SEs
var1$SE

var1$cutoff

## -----------------------------------------------------------
cutoff <- calculate_cutoff(population = DF, 
                           grouping_items = "Stimulus",
                           score = "EncodeJOL",
                           minimum = as.numeric(min(DF$EncodeJOL)),
                           maximum = as.numeric(max(DF$EncodeJOL)))
# showing how this is the same as the person calculated version versus semanticprimeR's function
cutoff$cutoff

final_table <- calculate_correction(
  proportion_summary = var1$final_sample,
  pilot_sample_size = length(unique(DF$Subject)),
  proportion_variability = cutoff$prop_var
  )

flextable(final_table) %>% 
  autofit()

