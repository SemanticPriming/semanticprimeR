## ----setup, include = FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette-setup, include=FALSE-----------------
knitr::opts_chunk$set(echo = TRUE)

# Set a random seed
set.seed(43043423)
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(psych)
library(rio)
library(ggplot2)
library(reshape)
library(semanticprimeR)

## --------------------------------------------------
EAMMi2<- import("data/mcfall_data.sav") %>% 
  select(starts_with("moa1#"), starts_with("moa2#"))
str(EAMMi2)

## --------------------------------------------------
EAMMi2metadata <- import("data/mcfall_metadata.csv")
flextable(EAMMi2metadata) %>% autofit()

EAMMi2 <- EAMMi2[complete.cases(EAMMi2),]
EAMMi2long <- EAMMi2 %>% pivot_longer(cols = everything()) %>% 
  dplyr::rename(item = name, score = value) %>% 
  group_by(item) %>% 
  sample_n(size = 50)

flextable(head(EAMMi2long)) %>% autofit()

## --------------------------------------------------
SE <- tapply(EAMMi2long$score, EAMMi2long$item, function (x) { sd(x)/sqrt(length(x)) })
min(SE)
quantile(SE, probs = .4)
max(SE)

cutoff <- quantile(SE, probs = .4)

# we can also use semanticprimer's function
cutoff_score <- calculate_cutoff(population = EAMMi2long,
                                 grouping_items = "item",
                                 score = "score",
                                 minimum = min(EAMMi2long$score),
                                 maximum = max(EAMMi2long$score))
cutoff_score$cutoff

## --------------------------------------------------
# sequence of sample sizes to try
samplesize_values <- seq(20, 200, 5)

# create a blank table for us to save the values in 

sim_table <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(EAMMi2long$item)))

# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA

# loop over sample sizes
for (i in 1:length(samplesize_values)){
    
  # temp dataframe that samples and summarizes
  temp <- EAMMi2long %>% 
    group_by(item) %>% 
    sample_n(samplesize_values[i], replace = T) %>% 
    summarize(se = sd(score)/sqrt(length(score))) 
  
  colnames(sim_table)[1:length(unique(EAMMi2long$item))] <- temp$item
  sim_table[i, 1:length(unique(EAMMi2long$item))] <- temp$se
  sim_table[i, "sample_size"] <- samplesize_values[i]
  }

final_sample <- 
  sim_table %>% 
  pivot_longer(cols = -c(sample_size)) %>% 
  group_by(sample_size) %>% 
  summarize(percent_below = sum(value <= cutoff)/length(unique(EAMMi2long$item))) %>% ungroup()

flextable(final_sample %>% head()) %>% autofit()

## --------------------------------------------------
final_table <- calculate_correction(
  proportion_summary = final_sample,
  pilot_sample_size = EAMMi2long %>% group_by(item) %>% 
    summarize(sample_size = n()) %>% ungroup() %>% 
    summarize(avg_sample = mean(sample_size)) %>% pull(avg_sample),
  proportion_variability = cutoff_score$prop_var
  )

flextable(final_table) %>% 
  autofit()

