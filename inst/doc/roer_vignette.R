## ----setup, include = FALSE----------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette-setup, include=FALSE---------
knitr::opts_chunk$set(echo = TRUE)

# Libraries necessary for this vignette
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(semanticprimeR)
set.seed(84930)

## ------------------------------------------
DF <- import("data/roer_data.xlsx")
drops <- c("Scenario")
DF <- DF[ , !(names(DF) %in% drops)]
DF <- cbind(Participant_Number = 1:nrow(DF) , DF)

str(DF)

## ------------------------------------------
metadata <- import("data/roer_metadata.xlsx")

flextable(metadata) %>% autofit()

## ------------------------------------------
DF_long <- pivot_longer(DF, cols = -c(Participant_Number)) %>% 
  dplyr:: rename(item = name, score = value)

flextable(head(DF_long)) %>% autofit()

## ------------------------------------------
# individual SEs
SE <- tapply(DF_long$score, DF_long$item, function (x) { sd(x)/sqrt(length(x)) })
SE

cutoff <- quantile(SE, probs = .40)
cutoff

# we could also use the cutoff score function in semanticprimeR
cutoff_score <- calculate_cutoff(population = DF_long,
                                 grouping_items = "item",
                                 score = "score",
                                 minimum = min(DF_long$score),
                                 maximum = max(DF_long$score))

cutoff_score$cutoff

## ------------------------------------------
# sequence of sample sizes to try
nsim <- 10 # small for cran
samplesize_values <- seq(20, 500, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(DF_long$item)))

# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA

iterate <- 1
for (p in 1:nsim){
  # loop over sample sizes
  for (i in 1:length(samplesize_values)){
      
    # temp dataframe that samples and summarizes
    temp <- DF_long %>% 
      group_by(item) %>% 
      sample_n(samplesize_values[i], replace = T) %>% 
      summarize(se = sd(score)/sqrt(length(score))) 
    
    colnames(sim_table)[1:length(unique(DF_long$item))] <- temp$item
    sim_table[iterate, 1:length(unique(DF_long$item))] <- temp$se
    sim_table[iterate, "sample_size"] <- samplesize_values[i]
    sim_table[iterate, "nsim"] <- p
    
    iterate <- iterate + 1
  }
}

final_sample <- 
  sim_table %>% 
  pivot_longer(cols = -c(sample_size, nsim)) %>% 
  dplyr::rename(item = name, se = value) %>% 
  group_by(sample_size, nsim) %>% 
  summarize(percent_below = sum(se <= cutoff)/length(unique(DF_long$item))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample %>% head()) %>% autofit()               

## ------------------------------------------
final_table <- calculate_correction(
  proportion_summary = final_sample,
  pilot_sample_size = DF_long %>% group_by(item) %>% 
    summarize(sample_size = n()) %>% ungroup() %>% 
    summarize(avg_sample = mean(sample_size)) %>% pull(avg_sample),
  proportion_variability = cutoff_score$prop_var
  )

flextable(final_table) %>% 
  autofit()

