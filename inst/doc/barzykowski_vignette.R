## ----setup, include = FALSE----------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette_setup, include = FALSE-------
knitr::opts_chunk$set(echo = TRUE)

# Libraries necessary for this vignette
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(semanticprimeR)
set.seed(583902)

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

## ------------------------------------------
DF <- import("data/barzykowski_data.xlsx") %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 2)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 3)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 4)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 5)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 6)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 7)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 8)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 9)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 10)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 11)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 12)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 13)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 14)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 15)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 16)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 17)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 18)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 19)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 20)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 21)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 22)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 23)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 24)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 25)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 26)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 27)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 28)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 29)) %>% 
  bind_rows(import("data/barzykowski_data.xlsx", sheet = 30)) 

str(DF)

## ------------------------------------------
metadata <- import("data/barzykowski_metadata.xlsx")

flextable(metadata) %>% autofit()

## ------------------------------------------
apply(DF[ , -c(1,2)], 2, sd)

## ------------------------------------------
# set seed
set.seed(8548)
# Function for simulation
var1 <- item_power(data = DF, # name of data frame
            dv_col = "How surprising", # name of DV column as a character
            item_col = "Cue no", # number of items column as a character
            nsim = 10,
            sample_start = 20, 
            sample_stop = 100, 
            sample_increase = 5,
            decile = .4)

var2 <- item_power(DF, # name of data frame
            "Personal nature", # name of DV column as a character
            item_col = "Cue no", # number of items column as a character
            nsim = 10, 
            sample_start = 20, 
            sample_stop = 100, 
            sample_increase = 5,
            decile = .4)

## ------------------------------------------
# individual SEs for how surprising 
var1$SE
# var 1 cut off
var1$cutoff

# individual SEs for personal nature
var2$SE
# var 2 cut off
var2$cutoff

# overall cutoff
cutoff <- mean(var1$cutoff, var2$cutoff)
cutoff

## ------------------------------------------
cutoff_personal <- calculate_cutoff(population = DF, 
                           grouping_items = "Cue no",
                           score = "Personal nature",
                           minimum = as.numeric(min(DF$`Personal nature`)),
                           maximum = as.numeric(max(DF$`Personal nature`)))
# showing how this is the same as the person calculated version versus semanticprimeR's function
cutoff_personal$cutoff

final_table_personal <- calculate_correction(
  proportion_summary = var1$final_sample,
  pilot_sample_size = length(unique(DF$`Participant's ID`)),
  proportion_variability = cutoff_personal$prop_var
  )

flextable(final_table_personal) %>% 
  autofit()

## ------------------------------------------
cutoff_surprising <- calculate_cutoff(population = DF, 
                           grouping_items = "Cue no",
                           score = "How surprising",
                           minimum = as.numeric(min(DF$`How surprising`)),
                           maximum = as.numeric(max(DF$`How surprising`)))
# showing how this is the same as the person calculated version versus semanticprimeR's function
cutoff_surprising$cutoff

final_table_surprising <- calculate_correction(
  proportion_summary = var2$final_sample,
  pilot_sample_size = length(unique(DF$`Participant's ID`)),
  proportion_variability = cutoff_surprising$prop_var
  )

flextable(final_table_surprising) %>% 
  autofit()

