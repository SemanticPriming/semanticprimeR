## ----setup, include = FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vignette_setup, include=FALSE-----------------
knitr::opts_chunk$set(echo = TRUE)

# Set a random see
set.seed(667)
library(rio)
library(flextable)
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(reshape)

## --------------------------------------------------
HDHS<- read.csv("data/HDHSAIPE.txt", sep="")
str(HDHS)

## --------------------------------------------------
metadata <- import("data/HDHSMeta.txt")

flextable(metadata) %>% autofit()

## --------------------------------------------------
HDHScorrect <- HDHS[HDHS$accTarget==1,] 
summary_stats <- HDHScorrect %>% #data frame
  select(RT, Target) %>% #pick the columns
  group_by(Target) %>% #put together the stimuli
  summarize(SES = sd(RT)/sqrt(length(RT)), samplesize = length(RT)) #create SE and the sample size for below
##give descriptives of the SEs
describe(summary_stats$SES)

##figure out the original sample sizes (not really necessary as all Primes were seen by 40 participants)
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

##average SE for words with at least n = 30
summary_stats %>% #data frame
  filter(samplesize >=30) %>% #filter out lower sample sizes
  summarize(avgSES = mean(SES)) #create the mean

##pick all words with sample sizes above 30
targets <- summary_stats %>% #data frame
  filter(samplesize >=30) %>% #filter out sample sizes
  pull(Target) #return a vector
targets <- as.character(targets)

##this section creates a sequence of sample sizes to estimate at
#5, 10, 15, etc.
samplesize_values <- seq(5, 200, 5)
#create a blank table for us to save the values in
sim_table <- matrix(NA, nrow = length(samplesize_values), ncol = length(targets))
#create column names based on the current targets
colnames(sim_table) <- targets
#make it a data frame
sim_table <- as.data.frame(sim_table)
#add those sample size values
sim_table$sample_size <- samplesize_values
##loop over all the target words randomly selected
for (i in 1:length(targets)){
  ##loop over sample sizes
  for (q in 1:length(samplesize_values)){
    ##temporarily save a data frame of Zscores
    temp <- HDHScorrect %>% #data frame
      filter(Target == targets[i]) %>% #pick rows that are the current target word
      sample_n(samplesize_values[q], replace = T) %>% #select sample size number of rows
      pull(RT)
    #put that in the table
    #find the sample size row and column we are working with
    #calculate SE sd/sqrt(n)
    sim_table[sim_table$sample_size == samplesize_values[q], targets[i]] <- sd(temp)/sqrt(length(temp))
  }
}

##melt down the data into long format for ggplot2
sim_table_long <- melt(sim_table,
                       id = "sample_size")
##create a graph of the sample size by SE value
ggplot(sim_table_long, aes(sample_size, value)) +
  theme_classic() +
  xlab("Sample Size") +
  ylab("Standard Error") +
  geom_point() +
  geom_hline(yintercept = .14) #mark here .14 occurs

flextable(head(HDHScorrect)) %>% autofit()

## --------------------------------------------------
SE <- tapply(HDHScorrect$RT, HDHScorrect$Prime, function (x) { sd(x)/sqrt(length(x)) })
min(SE)
max(SE)

cutoff <- quantile(SE, probs = .5)

## --------------------------------------------------
# sequence of sample sizes to try
samplesize_values <- seq(20, 200, 5)

# create a blank table for us to save the values in 

sim_table <- matrix(NA, 
                    nrow = length(samplesize_values), 
                    ncol = length(unique(HDHS$Prime)))

# make it a data frame
sim_table <- as.data.frame(sim_table)

# add a place for sample size values 
sim_table$sample_size <- NA

# loop over sample sizes
for (i in 1:length(samplesize_values)){
    
  # temp dataframe that samples and summarizes
  temp <- HDHScorrect %>% 
    group_by(Prime) %>% 
    sample_n(samplesize_values[i], replace = T) %>% 
    summarize(se = sd(RT)/sqrt(length(RT))) 
  
  colnames(sim_table)[1:length(unique(HDHScorrect$Prime))] <- temp$Prime
  sim_table[i, 1:length(unique(HDHScorrect$Prime))] <- temp$se
  sim_table[i, "sample_size"] <- samplesize_values[i]
  }

final_sample <- 
  sim_table %>% 
  pivot_longer(cols = -c(sample_size)) %>% 
#  rename(item = variable, se = value) %>% 
  group_by(sample_size) %>% 
  summarize(Percent_Below = sum(value <= cutoff)/length(unique(HDHScorrect$Prime))) %>% 
  filter(Percent_Below >= .80)

  # multiply by correction 
  final_sample$new_sample <- round(39.369 + 0.700*final_sample$sample_size + 0.003*cutoff - 0.694*length(unique(HDHScorrect$Prime)))

flextable(final_sample) %>% autofit()

