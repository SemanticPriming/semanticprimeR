## ----setup, include = FALSE----------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette_setup, include=FALSE----
knitr::opts_chunk$set(echo = TRUE)

# Libraries necessary for this vignette
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(psych)
library(data.table)
library(semanticprimeR)
set.seed(48394)

## ----data read-in--------------------
### for typicality data -- cleaning and processing
typicality_fnames <- list.files(path = "data/vanpaemel_data",
                                full.names = TRUE)

typicality_dfs <- lapply(typicality_fnames, read.csv)

ID <- c(1:16)
typicality_dfs <- mapply(cbind, typicality_dfs, "SampleID" = ID, SIMPLIFY = F)

typicality_all_df <- bind_rows(typicality_dfs)
typicality_all_df_v2 <- typicality_all_df %>% 
                        unite("comp_group", X:X.1, remove = TRUE) %>% 
                        select(-c(30,31,32,33,34)) %>% 
                        drop_na(c(2:29)) %>%
                        filter_all(any_vars(!is.na(.))) %>%
                        dplyr::rename(compType = SampleID)
# typicality_all_df_v2
typicality_all_df_v3 <- typicality_all_df_v2 %>% 
  select(starts_with("X"), compType, comp_group) %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "participant", 
               values_to = "score")
                    
head(typicality_all_df_v3)

## ------------------------------------
metadata <- import("data/vanpaemel_metadata.xlsx")

flextable(metadata) %>% autofit()

## ----find comparison types with  highest and lowest var----
# individual SEs among different comparison group
SE <- tapply(typicality_all_df_v3$score, typicality_all_df_v3$compType, function (x) { sd(x)/sqrt(length(x)) })
SE

min(SE)
max(SE)

# comparison type 1: amphibians

## ----subset and restructure----------
typicality_data_gp1_sub <- subset(typicality_all_df_v3, compType == 1)

# individual SEs for  comparison type 1
SE1 <- tapply(typicality_data_gp1_sub$score, typicality_data_gp1_sub$comp_group, function (x) { sd(x)/sqrt(length(x)) })

SE1

## ----power Two different comparison types----
# sequence of sample sizes to try
nsim <- 10 # small for cran 
samplesize_values <- seq(5, 200, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(typicality_data_gp1_sub$comp_group)))
# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA
sim_table$var <- "score"

iterate <- 1
for (p in 1:nsim){
  
    # loop over sample sizes for comparison type 
  for (i in 1:length(samplesize_values)){
      
    # temp dataframe for comparison type 1 that samples and summarizes
    temp1 <- typicality_data_gp1_sub %>% 
      dplyr::group_by(comp_group) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se2 = sd(score)/sqrt(length(score))) 
    
    # add to table
    colnames(sim_table)[1:length(unique(typicality_data_gp1_sub$comp_group))] <- temp1$comp_group
    sim_table[iterate, 1:length(unique(typicality_data_gp1_sub$comp_group))] <- temp1$se2
    sim_table[iterate, "sample_size"] <- samplesize_values[i]
    sim_table[iterate, "nsim"] <- p
    
    iterate <- 1 + iterate 
  }
  
}

## ----cutoff--------------------------
cutoff <- calculate_cutoff(population = typicality_data_gp1_sub, 
                 grouping_items = "comp_group",
                 score = "score", 
                 minimum = min(typicality_data_gp1_sub$score),
                 maximum = max(typicality_data_gp1_sub$score))

cutoff$cutoff

## ----summary analysis part1  comparison type 10----
### for response outputs 
# figure out cut off
final_sample <- 
  sim_table %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>% 
  dplyr::rename(item = name, se = value) %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff$cutoff)/length(unique(typicality_data_gp1_sub$comp_group))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample) %>% autofit()

## ----calculate correction------------
final_scores <- calculate_correction(proportion_summary = final_sample,
                     pilot_sample_size = length(unique(typicality_data_gp1_sub$participant)),
                     proportion_variability = cutoff$prop_var)

flextable(final_scores) %>% autofit()

