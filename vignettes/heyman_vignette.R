## ----setup, include = FALSE------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette-setup, include=FALSE-----------
knitr::opts_chunk$set(echo = TRUE)

# Set a random seed
set.seed(667)
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(reshape)
library(semanticprimeR)

## --------------------------------------------
HDHS<- read.csv("data/HDHSAIPE.txt", sep="")
str(HDHS)

## --------------------------------------------
metadata <- import("data/HDHSMeta.txt")

flextable(metadata) %>% autofit()

## --------------------------------------------
# pick only correct answers
HDHScorrect <- HDHS[HDHS$accTarget==1,] 
summary_stats <- HDHScorrect %>% #data frame
  select(RT, Target) %>% #pick the columns
  group_by(Target) %>% #put together the stimuli
  summarize(SES = sd(RT)/sqrt(length(RT)), samplesize = length(RT)) #create SE and the sample size for below
##give descriptives of the SEs
describe(summary_stats$SES)

##figure out the original sample sizes (not really necessary as all Targets were seen by 40 participants)
original_SS <- HDHS %>% #data frame
  count(Target) #count up the sample size
##add the original sample size to the data frame
summary_stats <- merge(summary_stats, original_SS, by = "Target")
##original sample size average
describe(summary_stats$n)

##reduced sample size
describe(summary_stats$samplesize)

##percent retained
describe(summary_stats$samplesize/summary_stats$n)

flextable(head(HDHScorrect)) %>% autofit()

## --------------------------------------------
SE <- tapply(HDHScorrect$RT, HDHScorrect$Target, function (x) { sd(x)/sqrt(length(x)) })
min(SE)
max(SE)

cutoff <- quantile(SE, probs = .4)
cutoff

## --------------------------------------------
# sequence of sample sizes to try
nsim <- 10 # small for cran
samplesize_values <- seq(20, 500, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(HDHS$Target)))

# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA

iterate <- 1

for (p in 1:nsim){
  
  # loop over sample sizes
  for (i in 1:length(samplesize_values)){
      
    # temp dataframe that samples and summarizes
    temp <- HDHScorrect %>% 
      group_by(Target) %>% 
      sample_n(samplesize_values[i], replace = T) %>% 
      summarize(se = sd(RT)/sqrt(length(RT))) 
    
    colnames(sim_table)[1:length(unique(HDHScorrect$Target))] <- temp$Target
    sim_table[iterate, 1:length(unique(HDHScorrect$Target))] <- temp$se
    sim_table[iterate, "sample_size"] <- samplesize_values[i]
    sim_table[iterate, "nsim"] <- p
    iterate <- 1 + iterate
  }
  
}

final_sample <- 
  sim_table %>% 
  pivot_longer(cols = -c(sample_size, nsim)) %>% 
  group_by(sample_size, nsim) %>% 
  summarize(percent_below = sum(value <= cutoff)/length(unique(HDHScorrect$Target))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample %>% head()) %>% autofit()

## --------------------------------------------
# use semanticprimer cutoff function for prop variance
cutoff <- calculate_cutoff(population = HDHScorrect, 
                           grouping_items = "Target",
                           score = "RT",
                           minimum = as.numeric(min(HDHScorrect$RT)),
                           maximum = as.numeric(max(HDHScorrect$RT)))
# showing how this is the same as the person calculated version versus semanticprimeR's function
cutoff$cutoff

final_table <- calculate_correction(
  proportion_summary = final_sample,
  pilot_sample_size = HDHScorrect %>% group_by(Target) %>% 
    summarize(sample_size = n()) %>% 
    ungroup() %>% summarize(avg_sample = mean(sample_size)) %>% 
    pull(avg_sample),
  proportion_variability = cutoff$prop_var
  )

flextable(final_table) %>% 
  autofit()

