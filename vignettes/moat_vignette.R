## ----setup, include = FALSE------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette-setup--------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# Libraries necessary for this vignette
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(semanticprimeR)
set.seed(92747)

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
    dv = data[ , dv_col],
    items = data[ , item_col]
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

## --------------------------------------------------------
DF <- import("data/moat_data.csv.zip") 
  
str(DF)

## --------------------------------------------------------
metadata <- tibble::tribble(
             ~Variable.Name,                                                                                                       ~Variable.Description, ~`Type.(numeric,.character,.logical,.etc.)`,
                      "Id",                                                                                                            "Participant ID",                                   "numeric",
                   "Domain",                                                    "Whether the trial is a claim about COVID ('covid') or TRIVIA ('trivia)",                                 "character",
                   "Medium", "Whether the trial appears as text alone ('claim'), text alongside an image ('photo'), or text alongside a video ('video')",                                 "character",
               "Trial_type",                                        "Whether the trial presents a claim that is TRUE ('target') or FALSE ('distractor')",                                 "character",
                   "Rating",                           "Paritcipant’s truth rating of the claim ranging from 1 (definitely false) to 6 (definitely tue)",                                   "numeric"
             )

flextable(metadata) %>% autofit()

## --------------------------------------------------------
# Function for simulation
var1 <- item_power(data = DF, # name of data frame
            dv_col = "rating", # name of DV column as a character
            item_col = "question_type", # number of items column as a character
            nsim = 10, 
            sample_start = 20, 
            sample_stop = 300, 
            sample_increase = 5,
            decile = .4)

## --------------------------------------------------------
var1$SE
var1$cutoff

cutoff <- var1$cutoff

# we can also use semanticprimer's function
cutoff_score <- calculate_cutoff(population = DF,
                                 grouping_items = "question_type",
                                 score = "rating",
                                 minimum = min(DF$rating),
                                 maximum = max(DF$rating))
cutoff_score$cutoff

## --------------------------------------------------------
flextable(var1$final_sample %>% head()) %>% 
  autofit()

final_table <- calculate_correction(
  proportion_summary = var1$final_sample,
  pilot_sample_size = DF %>% group_by(question_type) %>% 
    summarize(sample_size = n()) %>% ungroup() %>% 
    summarize(avg_sample = mean(sample_size)) %>% pull(avg_sample),
  proportion_variability = cutoff_score$prop_var
  )

flextable(final_table) %>% 
  autofit()

