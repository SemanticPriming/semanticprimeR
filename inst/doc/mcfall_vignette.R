## ----setup, include = FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette_setup, include=FALSE-----------------
knitr::opts_chunk$set(echo = TRUE)

# Set a random seed
set.seed(667)
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(psych)
library(rio)
library(ggplot2)
library(reshape)

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
  sample_n(size = 200)

flextable(head(EAMMi2long)) %>% autofit()

## --------------------------------------------------
SE <- tapply(EAMMi2long$score, EAMMi2long$item, function (x) { sd(x)/sqrt(length(x)) })
min(SE)
quantile(SE, probs = .5)
max(SE)

cutoff <- quantile(SE, probs = .5)

## --------------------------------------------------
# sequence of sample sizes to try
samplesize_values <- seq(250, 550, 5)

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
  summarize(Percent_Below = sum(value <= cutoff)/length(unique(EAMMi2long$item))) %>% 
  filter(Percent_Below >= .80) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff - 0.694*length(unique(EAMMi2long$item))))

flextable(final_sample) %>% autofit()

