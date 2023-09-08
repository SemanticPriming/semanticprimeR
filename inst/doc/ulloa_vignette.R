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
library(psych)
library(reshape)
library(reshape2)

## --------------------------------------------------
DF <- import("data/ulloa_data.csv")
drops <- c("RT", "side", "aff-ness")
DF <- DF[ , !(names(DF) %in% drops)]
DF

## --------------------------------------------------
metadata <- import("data/ulloa_metadata.xlsx")

flextable(metadata) %>% autofit()

## ----subset and restructure------------------------
### create  subset for valid cue-targeting
DF_valid <- subset(DF, congr == "valid") %>% 
  group_by(suj, item) %>% 
  summarize(liking = mean(liking, na.rm = T))

### create  subset for invalid cue-targeting
DF_invalid <- subset(DF, congr == "invalid") %>% 
  group_by(suj, item) %>% 
  summarize(liking = mean(liking, na.rm = T))

## ----compute se for separate-----------------------
# individual SEs for valid cue-targeting condition 
SE1 <- tapply(DF_valid$liking, DF_valid$item, function (x) { sd(x)/sqrt(length(x)) })

SE1
cutoff1 <- quantile(SE1, probs = .5)
cutoff1

# individual SEs for invalid cue-targeting condition
SE2 <- tapply(DF_invalid$liking, DF_invalid$item, function (x) { sd(x)/sqrt(length(x)) })

SE2
cutoff2 <- quantile(SE2, probs = .5)
cutoff2

## ----power Two different conditions----------------
# sequence of sample sizes to try
samplesize_values <- seq(25, 400, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(DF_valid$item)))
# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA
sim_table$var <- "liking"

# make a second table for the second variable
sim_table2 <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(DF_valid$item)))

# make it a data frame
sim_table2 <- as.data.frame(sim_table2)

# add a place for sample size values 
sim_table2$sample_size <- NA
sim_table2$var <- "liking"

# loop over sample sizes for age and outdoor trait
for (i in 1:length(samplesize_values)){
    
  # temp dataframe for age trait that samples and summarizes
  temp_valid <- DF_valid %>% 
    dplyr::group_by(item) %>% 
    dplyr::sample_n(samplesize_values[i], replace = T) %>% 
    dplyr::summarize(se1 = sd(liking)/sqrt(length(liking))) 
  
  # 
  colnames(sim_table)[1:length(unique(DF_valid$item))] <- temp_valid$item
  sim_table[i, 1:length(unique(DF_valid$item))] <- temp_valid$se1
  sim_table[i, "sample_size"] <- samplesize_values[i]
  
  # temp dataframe for outdoor trait that samples and summarizes
  
  temp_invalid <-DF_invalid %>% 
    dplyr::group_by(item) %>% 
    dplyr::sample_n(samplesize_values[i], replace = T) %>% 
    dplyr::summarize(se2 = sd(liking)/sqrt(length(liking))) 

  # 
  colnames(sim_table)[1:length(unique(DF_invalid$item))] <- temp_invalid$item
  sim_table2[i, 1:length(unique(DF_invalid$item))] <- temp_invalid$se2
  sim_table2[i, "sample_size"] <- samplesize_values[i]
}


## ----summary analysis part1------------------------
### for valid cue-targeting condition
final_sample <- 
  sim_table %>%
  pivot_longer(cols = -c(sample_size, var))  %>% 
  dplyr::rename(item = name, se = value)   %>% 
  dplyr::group_by(sample_size, var)  %>% 
  dplyr::summarize(Percent_Below = sum(se <= cutoff1)/length(unique(DF_valid$item)))  %>% 
  dplyr::filter(Percent_Below >= .80) %>% 
  dplyr::arrange(Percent_Below) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff1 - 0.694*length(unique(DF_valid$item))))

flextable(final_sample) %>% autofit()

## ----summary analysis part2------------------------
### for invalid cue-targeting
final_sample2 <- 
  sim_table2 %>%
  pivot_longer(cols = -c(sample_size, var)) %>% 
  dplyr::rename(item = name, se = value)  %>% 
  dplyr::group_by(sample_size, var) %>% 
  dplyr::summarize(Percent_Below = sum(se <= cutoff2)/length(unique(DF_invalid$item))) %>% 
  dplyr::filter(Percent_Below >= .80) %>% 
  dplyr::arrange(Percent_Below) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff2 - 0.694*length(unique(DF_invalid$item))))

flextable(final_sample2) %>% autofit()

