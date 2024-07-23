## ----setup, include = FALSE------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette-setup, include=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE)

# Libraries necessary for this vignette
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(psych)
library(reshape)
library(reshape2)
library(semanticprimeR)
set.seed(483948)

## --------------------------------------------------------
DF <- import("data/ulloa_data.csv")
drops <- c("RT", "side", "aff-ness")
DF <- DF[ , !(names(DF) %in% drops)]
head(DF)

## --------------------------------------------------------
metadata <- import("data/ulloa_metadata.xlsx")

flextable(metadata) %>% autofit()

## ----subset and restructure------------------------------
### create  subset for valid cue-targeting
DF_valid <- subset(DF, congr == "valid") %>% 
  group_by(suj, item) %>% 
  summarize(liking = mean(liking, na.rm = T)) %>% 
  as.data.frame()

### create  subset for invalid cue-targeting
DF_invalid <- subset(DF, congr == "invalid") %>% 
  group_by(suj, item) %>% 
  summarize(liking = mean(liking, na.rm = T)) %>% 
  as.data.frame()

## ----compute se for separate-----------------------------
# individual SEs for valid cue-targeting condition 
SE1 <- tapply(DF_valid$liking, DF_valid$item, function (x) { sd(x)/sqrt(length(x)) })

SE1
cutoff1 <- quantile(SE1, probs = .4)
cutoff1

# individual SEs for invalid cue-targeting condition
SE2 <- tapply(DF_invalid$liking, DF_invalid$item, function (x) { sd(x)/sqrt(length(x)) })

SE2
cutoff2 <- quantile(SE2, probs = .4)
cutoff2

## ----power Two different conditions----------------------
# sequence of sample sizes to try
nsim <- 10 # small for cran
samplesize_values <- seq(25, 200, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(DF_valid$item)))
# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA
sim_table$var <- "liking"

# make a second table for the second variable
sim_table2 <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(DF_valid$item)))

# make it a data frame
sim_table2 <- as.data.frame(sim_table2)

# add a place for sample size values 
sim_table2$sample_size <- NA
sim_table2$var <- "liking"

iterate <- 1
for (p in 1:nsim){
  
  # loop over sample sizes for age and outdoor trait
  for (i in 1:length(samplesize_values)){
      
    # temp dataframe for age trait that samples and summarizes
    temp_valid <- DF_valid %>% 
      dplyr::group_by(item) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se1 = sd(liking)/sqrt(length(liking))) 
    
    # 
    colnames(sim_table)[1:length(unique(DF_valid$item))] <- temp_valid$item
    sim_table[iterate, 1:length(unique(DF_valid$item))] <- temp_valid$se1
    sim_table[iterate, "sample_size"] <- samplesize_values[i]
    sim_table[iterate, "nsim"] <- p
    
    # temp dataframe for outdoor trait that samples and summarizes
    
    temp_invalid <-DF_invalid %>% 
      dplyr::group_by(item) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se2 = sd(liking)/sqrt(length(liking))) 
  
    # 
    colnames(sim_table)[1:length(unique(DF_invalid$item))] <- temp_invalid$item
    sim_table2[iterate, 1:length(unique(DF_invalid$item))] <- temp_invalid$se2
    sim_table2[iterate, "sample_size"] <- samplesize_values[i]
    sim_table2[iterate, "nsim"] <- p
    
    iterate <- 1 + iterate
  }
  
}

## ----cutoff----------------------------------------------
cutoff_valid <- calculate_cutoff(population = DF_valid, 
                 grouping_items = "item",
                 score = "liking", 
                 minimum = min(DF_valid$liking),
                 maximum = max(DF_valid$liking))

# same as above
cutoff_valid$cutoff

cutoff_invalid <- calculate_cutoff(population = DF_invalid, 
                 grouping_items = "item",
                 score = "liking", 
                 minimum = min(DF_valid$liking),
                 maximum = max(DF_valid$liking))

cutoff_invalid$cutoff

## ----summary analysis part1------------------------------
### for valid cue-targeting condition
final_sample_valid <- 
  sim_table %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>%
  dplyr::rename(item = name, se = value) %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff1)/length(unique(DF_valid$item))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample_valid %>% head()) %>% 
  autofit()

## ----calculate correction--------------------------------
final_scores <- calculate_correction(proportion_summary = final_sample_valid,
                     pilot_sample_size = length(unique(DF$suj)),
                     proportion_variability = cutoff_valid$prop_var)

# only show first four rows since all 100
flextable(final_scores %>% 
            ungroup() %>% 
            slice_head(n = 4)) %>% autofit()

## ----summary analysis part2------------------------------
### for valid cue-targeting condition
final_sample_invalid <- 
  sim_table2 %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>%
  dplyr::rename(item = name, se = value) %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff2)/length(unique(DF_invalid$item))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample_invalid %>% head()) %>% 
  autofit()

## ----calculate correction2-------------------------------
final_scores2 <- calculate_correction(proportion_summary = final_sample_invalid,
                     pilot_sample_size = length(unique(DF$suj)),
                     proportion_variability = cutoff_invalid$prop_var)

# only show first four rows since all 100
flextable(final_scores2 %>% 
            ungroup() %>% 
            slice_head(n = 4)) %>% autofit()

