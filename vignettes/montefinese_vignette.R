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

## --------------------------------------------------
DF <- import("data/montefinese_data.csv")

names(DF) <- make.names(names(DF),unique = TRUE)

names(DF)[names(DF) == 'ITEM..ITA.'] <- "item"

DF <- DF %>%
  arrange(item) %>% #orders the rows of the data by the target_name column
  group_by(item) %>% #group by the target name
  transform(items = as.numeric(factor(item)))%>% #transform target name into a item
  select(items, item, everything()
         ) #select all variables from items and target_name 

head(DF)

## --------------------------------------------------
metadata <- import("data/montefinese_metadata.xlsx")

flextable(metadata) %>% autofit()

## ----sd analysis-----------------------------------

# individual SEs for how surprising 
SE_full <- tapply(DF$Response, DF$StimType,  function (x) { sd(x)/sqrt(length(x)) })
SE_full
quantile(SE_full, probs = .5)

## ----subset and restructure------------------------
### create  subset for REL+
DF_RELpos <- subset(DF, StimType == "REL+")
DF_RELpos_re <- cast(DF_RELpos, ssID + item ~ StimType, value = "Response")

### create  subset for REL-
DF_RELneg <- subset(DF, StimType == "REL-")
DF_RELneg_re <- cast(DF_RELneg, ssID + item ~ StimType, value = "Response")

### create  subset for UNREL
DF_UNREL <- subset(DF, StimType == "UNREL")
DF_UNREL_re <- cast(DF_UNREL, ssID + item ~ StimType, value = "Response")

## ----compute se for REL+ and REL-------------------

# individual SEs for REL+ condition 
SE1 <- tapply(DF_RELpos$Response, DF_RELpos$item, function (x) { sd(x)/sqrt(length(x)) })

SE1
quantile(SE1, probs = .5)

# individual SEs for REL- condition
SE2 <- tapply(DF_RELneg$Response, DF_RELneg$item, function (x) { sd(x)/sqrt(length(x)) })

SE2
quantile(SE2, probs = .5)

# individual SEs for UNREL condition
SE3 <- tapply(DF_UNREL$Response, DF_UNREL$item, function (x) { sd(x)/sqrt(length(x)) })

SE3
quantile(SE3, probs = .5)


## ----power Two different conditions----------------
# sequence of sample sizes to try
samplesize_values <- seq(25, 300, 5)

# create a blank table for us to save the values in 
sim_table <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(DF_RELpos$item)))
# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA
sim_table$var <- "Response"

# make a second table for the second variable
sim_table2 <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(DF_RELpos$item)))


# make it a data frame
sim_table2 <- as.data.frame(sim_table2)

# add a place for sample size values 
sim_table2$sample_size <- NA
sim_table2$var <- "Response"

# loop over sample size
for (i in 1:length(samplesize_values)){
    
  # temp dataframe for age trait that samples and summarizes
  temp_RELpos <- DF_RELpos %>% 
    dplyr::group_by(item) %>% 
    dplyr::sample_n(samplesize_values[i], replace = T) %>% 
    dplyr::summarize(se1 = sd(Response)/sqrt(length(Response))) 
  
  # 
  colnames(sim_table)[1:length(unique(DF_RELpos$item))] <- temp_RELpos$item
  sim_table[i, 1:length(unique(DF_RELpos$item))] <- temp_RELpos$se1
  sim_table[i, "sample_size"] <- samplesize_values[i]
  
  # temp dataframe for  that samples and summarizes
  
  temp_RELneg <-DF_RELneg %>% 
    dplyr::group_by(item) %>% 
    dplyr::sample_n(samplesize_values[i], replace = T) %>% 
    dplyr::summarize(se2 = sd(Response)/sqrt(length(Response))) 

  # 
  colnames(sim_table)[1:length(unique(DF_RELneg$item))] <- temp_RELneg$item
  sim_table2[i, 1:length(unique(DF_RELneg$item))] <- temp_RELneg$se2
  sim_table2[i, "sample_size"] <- samplesize_values[i]
}


## ----summary analysis part1------------------------
# multiply by correction 
cutoff <- quantile(SE1, probs = .5)

final_sample <- 
  sim_table %>%
  pivot_longer(cols = -c(sample_size, var))  %>% 
  dplyr::rename(item = name, se = value)   %>% 
  dplyr::group_by(sample_size, var)  %>% 
  dplyr::summarize(Percent_Below = sum(se <= cutoff)/length(unique(DF_RELpos$item)))  %>% 
  dplyr::filter(Percent_Below >= .80) %>% 
  dplyr::arrange(Percent_Below) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff - 0.694*length(unique(DF_RELpos$items))))

flextable(final_sample) %>% autofit()

## ----summary analysis part2------------------------
cutoff <- quantile(SE2, probs = .5)
final_sample2 <- 
  sim_table2 %>%
  pivot_longer(cols = -c(sample_size, var)) %>% 
  dplyr::rename(item = name, se = value)  %>% 
  dplyr::group_by(sample_size, var) %>% 
  dplyr::summarize(Percent_Below = sum(se <= cutoff)/length(unique(DF_RELneg$item))) %>% 
  dplyr::filter(Percent_Below >= .80) %>% 
  dplyr::arrange(Percent_Below) %>% 
  mutate(new_sample = round(39.369 + 0.700*sample_size + 0.003*cutoff - 0.694*length(unique(DF_RELpos$items))))

flextable(final_sample2) %>% autofit()

