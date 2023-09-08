## ----setup, include = FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette_setup, include=FALSE-----------------
knitr::opts_chunk$set(echo = TRUE)

# Libraries necessary for this vignette
library(rio)
library(flextable)
library(dplyr)
library(tidyr)

## --------------------------------------------------
DF <- import("data/roer_data.xlsx")
drops <- c("Scenario")
DF <- DF[ , !(names(DF) %in% drops)]
DF <- cbind(Participant_Number = 1:nrow(DF) , DF)

str(DF)

## --------------------------------------------------
metadata <- import("data/roer_metadata.xlsx")

flextable(metadata) %>% autofit()

## --------------------------------------------------
DF_long <- pivot_longer(DF, cols = -c(Participant_Number)) %>% 
  dplyr:: rename(item = name, score = value)

flextable(head(DF_long)) %>% autofit()

## --------------------------------------------------
# individual SEs
SE <- tapply(DF_long$score, DF_long$item, function (x) { sd(x)/sqrt(length(x)) })

SE

cutoff <- quantile(SE, probs = .50)
cutoff

## --------------------------------------------------
# sequence of sample sizes to try
samplesize_values <- seq(20, 300, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(DF_long$item)))

# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA

# loop over sample sizes
for (i in 1:length(samplesize_values)){
    
  # temp dataframe that samples and summarizes
  temp <- DF_long %>% 
    group_by(item) %>% 
    sample_n(samplesize_values[i], replace = T) %>% 
    summarize(se = sd(score)/sqrt(length(score))) 
  
  colnames(sim_table)[1:length(unique(DF_long$item))] <- temp$item
  sim_table[i, 1:length(unique(DF_long$item))] <- temp$se
  sim_table[i, "sample_size"] <- samplesize_values[i]
  }

final_sample <- 
  sim_table %>% 
  pivot_longer(cols = -c(sample_size)) %>% 
  dplyr::rename(item = name, se = value) %>% 
  group_by(sample_size) %>% 
  summarize(Percent_Below = sum(se <= cutoff)/length(unique(DF_long$item))) %>% 
  filter(Percent_Below >= .80) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff - 0.694*length(unique(DF_long$item))))

flextable(final_sample) %>% autofit()               

