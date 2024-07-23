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
library(semanticprimeR)
set.seed(4538939)

## --------------------------------------------------------
## Please set the work directory to the folder containing the scripts and data
face_data <- import("data/suchow_data.csv.zip")
str(face_data)

## --------------------------------------------------------
metadata <- import("data/suchow_metadata.xlsx")

flextable(metadata) %>% autofit()

## --------------------------------------------------------
# pick random faces
faces <- unique(face_data$stimulus)[sample(unique(face_data$stimulus), size = 50)]
# pick random traits
traits <- unique(face_data$trait)[sample(unique(face_data$trait), size = 10)]

face_data <- face_data %>% 
  filter(trait %in% traits) %>% 
  filter(stimulus %in% faces)

## ----sd analysis-----------------------------------------
# all SEs 
SE_full <- tapply(face_data$response, face_data$trait, function (x) { sd(x)/sqrt(length(x)) })
SE_full

## ----subset and restructure------------------------------
## smallest variance is trait 4
face_data_trait4_sub <- subset(face_data, trait == names(which.min(SE_full)))

## largest is trait 30
face_data_trait30_sub <- subset(face_data, trait == names(which.max(SE_full)))

## ----compute se for traits-------------------------------
# individual SEs for 4 trait 
SE1 <- tapply(face_data_trait4_sub$response, face_data_trait4_sub$stimulus, function (x) { sd(x)/sqrt(length(x)) })
quantile(SE1, probs = .4)

# individual SEs for 30 trait
SE2 <- tapply(face_data_trait30_sub$response, face_data_trait30_sub$stimulus, function (x) { sd(x)/sqrt(length(x)) })

quantile(SE2, probs = .4)

## ----power Two different traits--------------------------
# sequence of sample sizes to try
nsim <- 10 # small for cran 
samplesize_values <- seq(25, 100, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(face_data_trait4_sub$stimulus)))
# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA
sim_table$var <- "response"

# make a second table for the second variable
sim_table2 <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(face_data_trait30_sub$stimulus)))

# make it a data frame
sim_table2 <- as.data.frame(sim_table2)

# add a place for sample size values 
sim_table2$sample_size <- NA
sim_table2$var <- "response"

iterate <- 1
for (p in 1:nsim){
  # loop over sample sizes for age and outdoor trait
  for (i in 1:length(samplesize_values)){
      
    # temp dataframe for age trait that samples and summarizes
    temp7 <- face_data_trait4_sub %>% 
      dplyr::group_by(stimulus) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se1 = sd(response)/sqrt(length(response))) 
    
    # 
    colnames(sim_table)[1:length(unique(face_data_trait4_sub$stimulus))] <- temp7$stimulus
    sim_table[iterate, 1:length(unique(face_data_trait4_sub$stimulus))] <- temp7$se1
    sim_table[iterate, "sample_size"] <- samplesize_values[i]
    sim_table[iterate, "nsim"] <- p
    
    # temp dataframe for outdoor trait that samples and summarizes
    temp35 <-face_data_trait30_sub %>% 
      dplyr::group_by(stimulus) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se2 = sd(response)/sqrt(length(response))) 
    
    # 
    colnames(sim_table2)[1:length(unique(face_data_trait30_sub$stimulus))] <- temp35$stimulus
    sim_table2[iterate, 1:length(unique(face_data_trait30_sub$stimulus))] <- temp35$se2
    sim_table2[iterate, "sample_size"] <- samplesize_values[i]
    sim_table2[iterate, "nsim"] <- p
    
    iterate <- 1 + iterate
  
  }
  
}

## ----cutoff----------------------------------------------
cutoff_trait4 <- calculate_cutoff(population = face_data_trait4_sub, 
                 grouping_items = "stimulus",
                 score = "response", 
                 minimum = min(face_data_trait4_sub$response),
                 maximum = max(face_data_trait4_sub$response))

# same as above
cutoff_trait4$cutoff

cutoff_trait30 <- calculate_cutoff(population = face_data_trait30_sub, 
                 grouping_items = "stimulus",
                 score = "response", 
                 minimum = min(face_data_trait30_sub$response),
                 maximum = max(face_data_trait30_sub$response))

cutoff_trait30$cutoff

## ----summary analysis part1------------------------------
cutoff <- quantile(SE1, probs = .4)
final_sample <- 
  sim_table %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>% 
  dplyr::rename(item = name, se = value) %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff)/length(unique(face_data_trait4_sub$stimulus))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample %>% head()) %>% autofit()

## ----calculate correction--------------------------------
final_scores <- calculate_correction(proportion_summary = final_sample,
                     pilot_sample_size = face_data_trait4_sub %>% 
                       group_by(stimulus) %>% 
                       summarize(sample_size = n()) %>% 
                       ungroup() %>% 
                       summarize(avg_sample = mean(sample_size)) %>% 
                       pull(avg_sample),
                     proportion_variability = cutoff_trait4$prop_var)

flextable(final_scores) %>% autofit()

## ----summary analysis part2------------------------------
cutoff <- quantile(SE2, probs = .4) 
final_sample2 <- 
  sim_table2 %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>% 
  dplyr::rename(item = name, se = value)  %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff)/length(unique(face_data_trait30_sub$stimulus))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample2 %>% head()) %>% autofit()

## ----calculate correction2-------------------------------
final_scores2 <- calculate_correction(proportion_summary = final_sample2,
                     pilot_sample_size = face_data_trait30_sub %>% 
                       group_by(stimulus) %>% 
                       summarize(sample_size = n()) %>% 
                       ungroup() %>% 
                       summarize(avg_sample = mean(sample_size)) %>% 
                       pull(avg_sample),
                     proportion_variability = cutoff_trait30$prop_var)

flextable(final_scores2) %>% autofit()

