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

## --------------------------------------------------
## Please set the work directory to the folder containing the scripts and data
face_data <- import("data/suchow_data.csv")
str(face_data)

## --------------------------------------------------
metadata <- import("data/suchow_metadata.xlsx")

flextable(metadata) %>% autofit()

## --------------------------------------------------
# pick random faces
faces <- unique(face_data$stimulus)[sample(unique(face_data$stimulus), size = 50)]
# pick random traits
traits <- unique(face_data$trait)[sample(unique(face_data$trait), size = 10)]

face_data <- face_data %>% 
  filter(trait %in% traits) %>% 
  filter(stimulus %in% faces)

## ----sd analysis-----------------------------------
# all SEs 
SE_full <- tapply(face_data$response, face_data$trait, function (x) { sd(x)/sqrt(length(x)) })
SE_full

## ----subset and restructure------------------------
## smallest variance is trait 4
face_data_trait4_sub <- subset(face_data, trait == names(which.min(SE_full)))

## largest is trait 30
face_data_trait30_sub <- subset(face_data, trait == names(which.max(SE_full)))

## ----compute se for traits-------------------------
# individual SEs for 4 trait 
SE1 <- tapply(face_data_trait4_sub$response, face_data_trait4_sub$stimulus, function (x) { sd(x)/sqrt(length(x)) })
quantile(SE1, probs = .5)

# individual SEs for 30 trait
SE2 <- tapply(face_data_trait30_sub$response, face_data_trait30_sub$stimulus, function (x) { sd(x)/sqrt(length(x)) })

quantile(SE2, probs = .5)

## ----power Two different traits--------------------
# sequence of sample sizes to try
samplesize_values <- seq(25, 100, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(face_data_trait4_sub$stimulus)))
# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA
sim_table$var <- "response"

# make a second table for the second variable
sim_table2 <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(face_data_trait30_sub$stimulus)))

# make it a data frame
sim_table2 <- as.data.frame(sim_table2)

# add a place for sample size values 
sim_table2$sample_size <- NA
sim_table2$var <- "response"

# loop over sample sizes for age and outdoor trait
for (i in 1:length(samplesize_values)){
    
  # temp dataframe for age trait that samples and summarizes
  temp7 <- face_data_trait4_sub %>% 
    dplyr::group_by(stimulus) %>% 
    dplyr::sample_n(samplesize_values[i], replace = T) %>% 
    dplyr::summarize(se1 = sd(response)/sqrt(length(response))) 
  
  # 
  colnames(sim_table)[1:length(unique(face_data_trait4_sub$stimulus))] <- temp7$stimulus
  sim_table[i, 1:length(unique(face_data_trait4_sub$stimulus))] <- temp7$se1
  sim_table[i, "sample_size"] <- samplesize_values[i]
  
  # temp dataframe for outdoor trait that samples and summarizes
  temp35 <-face_data_trait30_sub %>% 
    dplyr::group_by(stimulus) %>% 
    dplyr::sample_n(samplesize_values[i], replace = T) %>% 
    dplyr::summarize(se2 = sd(response)/sqrt(length(response))) 
  
  # 
  colnames(sim_table2)[1:length(unique(face_data_trait30_sub$stimulus))] <- temp35$stimulus
  sim_table2[i, 1:length(unique(face_data_trait30_sub$stimulus))] <- temp35$se2
  sim_table2[i, "sample_size"] <- samplesize_values[i]

}

## ----summary analysis part1------------------------
cutoff <- quantile(SE1, probs = .5)
final_sample <- 
  sim_table %>%
  pivot_longer(cols = -c(sample_size, var))  %>% 
  dplyr::rename(item = name, se = value)   %>% 
  dplyr::group_by(sample_size, var)  %>% 
  dplyr::summarize(Percent_Below = sum(se <= cutoff)/length(unique(face_data_trait4_sub$stimulus)))  %>% 
  dplyr::filter(Percent_Below >= .80) %>% 
  dplyr::arrange(Percent_Below) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff - 0.694*length(unique(face_data_trait4_sub$stimulus))))

flextable(final_sample) %>% autofit()

## ----summary analysis part2------------------------
cutoff <- quantile(SE2, probs = .5) 
final_sample2 <- 
  sim_table2 %>%
  pivot_longer(cols = -c(sample_size, var)) %>% 
  dplyr::rename(item = name, se = value)  %>% 
  dplyr::group_by(sample_size, var) %>% 
  dplyr::summarize(Percent_Below = sum(se <= cutoff)/length(unique(face_data_trait30_sub$stimulus))) %>% 
  dplyr::filter(Percent_Below >= .80) %>% 
  dplyr::arrange(Percent_Below) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff - 0.694*length(unique(face_data_trait4_sub$stimulus))))

flextable(final_sample2) %>% autofit()

