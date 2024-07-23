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
library(semanticprimeR)
set.seed(0329032)

## --------------------------------------------------------
DF <- import("data/montefinese_data.csv") 

names(DF) <- make.names(names(DF),unique = TRUE)

names(DF)[names(DF) == 'ITEM..ITA.'] <- "item"

DF <- DF %>%
  filter(StimType != "") %>% 
  filter(Measure == "Valence") %>% # only look at valence score 
  arrange(item) %>% #orders the rows of the data by the target_name column
  group_by(item) %>% #group by the target name
  transform(items = as.numeric(factor(item)))%>% #transform target name into a item
  select(items, item, everything()
         ) #select all variables from items and target_name 

head(DF)

## --------------------------------------------------------
metadata <- import("data/montefinese_metadata.xlsx")

flextable(metadata) %>% autofit()

## ----subset and restructure------------------------------
### create  subset for REL+
DF_RELpos <- subset(DF, StimType == "REL+")

### create  subset for REL-
DF_RELneg <- subset(DF, StimType == "REL-")

### create  subset for UNREL
DF_UNREL <- subset(DF, StimType == "UNREL")

## ----compute se for REL+ and REL-------------------------

# individual SEs for REL+ condition 
cutoff_relpos <- calculate_cutoff(population = DF_RELpos,
                                  grouping_items = "item", 
                                  score = "Response", 
                                  minimum = min(DF_RELpos$Response),
                                  maximum = max(DF_RELpos$Response))

SE1 <- tapply(DF_RELpos$Response, DF_RELpos$item, function (x) { sd(x)/sqrt(length(x)) })
SE1
cutoff_relpos$cutoff

# individual SEs for REL- condition
cutoff_relneg <- calculate_cutoff(population = DF_RELneg,
                                  grouping_items = "item", 
                                  score = "Response", 
                                  minimum = min(DF_RELneg$Response),
                                  maximum = max(DF_RELneg$Response))

SE2 <- tapply(DF_RELneg$Response, DF_RELneg$item, function (x) { sd(x)/sqrt(length(x)) })
SE2
cutoff_relneg$cutoff

# individual SEs for UNREL condition
cutoff_unrel <- calculate_cutoff(population = DF_UNREL,
                                  grouping_items = "item", 
                                  score = "Response", 
                                  minimum = min(DF_UNREL$Response),
                                  maximum = max(DF_UNREL$Response))

SE3 <- tapply(DF_UNREL$Response, DF_UNREL$item, function (x) { sd(x)/sqrt(length(x)) })
SE3
cutoff_unrel$cutoff

## ----power three different conditions--------------------
# sequence of sample sizes to try
nsim <- 10 # small for cran
samplesize_values <- seq(25, 300, 5)

# create a blank table for us to save the values in positive ----
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(DF_RELpos$item)))
# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA
sim_table$var <- "Response"

# make a second table for negative -----
sim_table2 <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(DF_RELneg$item)))

# make it a data frame
sim_table2 <- as.data.frame(sim_table2)

# add a place for sample size values 
sim_table2$sample_size <- NA
sim_table2$var <- "Response"

# make a second table for unrelated -----
sim_table3 <- matrix(NA, 
                    nrow = length(samplesize_values)*nsim, 
                    ncol = length(unique(DF_UNREL$item)))

# make it a data frame
sim_table3 <- as.data.frame(sim_table3)

# add a place for sample size values 
sim_table3$sample_size <- NA
sim_table3$var <- "Response"

iterate <- 1

for (p in 1:nsim){
  
  # loop over sample size
  for (i in 1:length(samplesize_values)){
      
    # related positive temp variables ----
    temp_RELpos <- DF_RELpos %>% 
      dplyr::group_by(item) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se1 = sd(Response)/sqrt(length(Response))) 
    
    # put in table 
    colnames(sim_table)[1:length(unique(DF_RELpos$item))] <- temp_RELpos$item
    sim_table[iterate, 1:length(unique(DF_RELpos$item))] <- temp_RELpos$se1
    sim_table[iterate, "sample_size"] <- samplesize_values[i]
    sim_table[iterate, "nsim"] <- p
    
    # related negative temp variables ----
    temp_RELneg <-DF_RELneg %>% 
      dplyr::group_by(item) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se2 = sd(Response)/sqrt(length(Response))) 
  
    # put in table 
    colnames(sim_table2)[1:length(unique(DF_RELneg$item))] <- temp_RELneg$item
    sim_table2[iterate, 1:length(unique(DF_RELneg$item))] <- temp_RELneg$se2
    sim_table2[iterate, "sample_size"] <- samplesize_values[i]
    sim_table2[iterate, "nsim"] <- p
    
    # unrelated temp variables ----
    temp_UNREL <-DF_UNREL %>% 
      dplyr::group_by(item) %>% 
      dplyr::sample_n(samplesize_values[i], replace = T) %>% 
      dplyr::summarize(se3 = sd(Response)/sqrt(length(Response))) 
  
    # put in table 
    colnames(sim_table3)[1:length(unique(DF_UNREL$item))] <- temp_UNREL$item
    sim_table3[iterate, 1:length(unique(DF_UNREL$item))] <- temp_UNREL$se3
    sim_table3[iterate, "sample_size"] <- samplesize_values[i]
    sim_table3[iterate, "nsim"] <- p
    
    iterate <- iterate + 1
    
  }
}


## ----summary analysis part1------------------------------
# multiply by correction 
cutoff <- quantile(SE1, probs = .4)

final_sample <- 
  sim_table %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>% 
  dplyr::rename(item = name, se = value) %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff)/length(unique(DF_RELpos$item))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample %>% head()) %>% autofit()

## --------------------------------------------------------
final_table_pos <- calculate_correction(
  proportion_summary = final_sample,
  pilot_sample_size = length(unique(DF_RELpos$ssID)),
  proportion_variability = cutoff_relpos$prop_var
  )

flextable(final_table_pos) %>% 
  autofit()

## ----summary analysis part2------------------------------
cutoff <- quantile(SE2, probs = .4)

final_sample2 <- 
  sim_table2 %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>% 
  dplyr::rename(item = name, se = value)  %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff)/length(unique(DF_RELneg$item))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample2 %>% head()) %>% autofit()

## --------------------------------------------------------
final_table_neg <- calculate_correction(
  proportion_summary = final_sample2,
  pilot_sample_size = length(unique(DF_RELneg$ssID)),
  proportion_variability = cutoff_relneg$prop_var
  )

flextable(final_table_neg) %>% 
  autofit()

## ----summary analysis part3------------------------------
cutoff <- quantile(SE3, probs = .4)

final_sample3 <- 
  sim_table3 %>%
  pivot_longer(cols = -c(sample_size, var, nsim)) %>% 
  dplyr::rename(item = name, se = value) %>% 
  dplyr::group_by(sample_size, var, nsim) %>% 
  dplyr::summarize(percent_below = sum(se <= cutoff)/length(unique(DF_UNREL$item))) %>% 
  ungroup() %>% 
  # then summarize all down averaging percents
  dplyr::group_by(sample_size, var) %>% 
  summarize(percent_below = mean(percent_below)) %>% 
  dplyr::arrange(percent_below) %>% 
  ungroup()

flextable(final_sample3 %>% head()) %>% autofit()

## --------------------------------------------------------
final_table_unrel <- calculate_correction(
  proportion_summary = final_sample3,
  pilot_sample_size = length(unique(DF_UNREL$ssID)),
  proportion_variability = cutoff_unrel$prop_var
  )

flextable(final_table_unrel) %>% 
  autofit()

