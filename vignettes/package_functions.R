## ----setup, include = FALSE----------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------
library(dplyr)
library(semanticprimeR)
library(udpipe)
library(stopwords)
library(tibble)
library(rio)

## ------------------------------------------
df <- simulate_population(mu = 25, # mean priming in ms
                    mu_sigma = 5, # standard deviation of the item means
                    sigma = 10, # population standard devation for items
                    sigma_sigma = 3, # standard deviation of the standard deviation of items
                    number_items = 75, # number of priming items 
                    number_scores = 100, # a population of values to simulate
                    smallest_sigma = 1, # smallest possible item standard deviation 
                    min_score = -25, # min ms priming
                    max_score = 100, # max ms priming 
                    digits = 3)

head(df)

## ------------------------------------------
cutoff <- calculate_cutoff(population = df, # pilot data or simulated data
  grouping_items = "item", # name of the item indicator column
  score = "score", # name of the dependent variable column
  minimum = min(df$score), # minimum possible/found score
  maximum = max(df$score)) # maximum possible/found score
                           
cutoff$se_items # all standard errors of items
cutoff$sd_items # standard deviation of the standard errors
cutoff$cutoff # 40% decile score
cutoff$prop_var # proportion of possible variance 

## ----eval = F------------------------------
#  samples <- bootstrap_samples(start = 20, # starting sample size
#    stop = 400, # stopping sample size
#    increase = 5, # increase bootstrapped samples by this amount
#    population = df, # population or pilot data
#    replace = TRUE, # bootstrap with replacement?
#    nsim = 500, # number of simulations to run
#    grouping_items = "item") # item column label
#  
#  save(samples, file = "data/simulatePriming.RData")

## ------------------------------------------
# since that's a slow function, we wrote out the data and read it back in
load("data/simulatePriming.Rdata")
head(samples[[1]])

## ------------------------------------------
proportion_summary <- calculate_proportion(samples = samples, # samples list
  cutoff = cutoff$cutoff, # cut off score 
  grouping_items = "item", # item column name
  score = "score") # dependent variable column name 

head(proportion_summary)

## ------------------------------------------
corrected_summary <- calculate_correction(
  proportion_summary = proportion_summary, # prop from above
  pilot_sample_size = 100, # number of participants in the pilot data 
  proportion_variability = cutoff$prop_var, # proportion variance from cutoff scores
  power_levels = c(80, 85, 90, 95)) # what levels of power to calculate 

corrected_summary

## ------------------------------------------
data("subsData")
head(tibble(subsData))

## ----eval = F------------------------------
#  af_freq <- import_subs(
#    language = "af",
#    what = "subs_count"
#  )

## ----echo = F, message = F-----------------
af_freq <- import("subs_count/af/dedup.af.words.unigrams.txt")

## ------------------------------------------
head(af_freq)

## ------------------------------------------
# tag with udpipe
af_tagged <- udpipe(af_freq$unigram, 
                    object = subsData$udpipe_model[subsData$language_code == "af"], 
                    parser = "none")

head(tibble(af_tagged))

## ------------------------------------------
# word_choice
word_choice <- c("NOUN", "VERB", "ADJ", "ADV")

# lower case
af_tagged$lemma <- tolower(af_tagged$lemma)

# three characters 
af_tagged <- subset(af_tagged, nchar(af_tagged$lemma) >= 3)

# only nouns verbs, etc. 
af_tagged <- subset(af_tagged, upos %in% word_choice)

# removed stop words just in case they were incorrectly tagged
af_tagged <- subset(af_tagged, !(lemma %in% stopwords(language = "af", source = "stopwords-iso")))

# removed things with numbers
af_tagged <- subset(af_tagged, !(grepl("[0-9]", af_tagged$sentence)))

# merge frequency back into tagged list
# merge by sentence so one to one match
colnames(af_freq) <- c("sentence", "freq")
af_final <- merge(af_tagged, af_freq, by = "sentence", all.x = T)

head(tibble(af_final))

# eliminate duplicates by lemma
af_final <- af_final[order(af_final$freq, decreasing = TRUE) , ]
af_final <- af_final[!duplicated(af_final$lemma), ]

# grab top 10K
af_top <- af_final[1:10000 , ]

## ----eval = F------------------------------
#  af_dims <- import_subs(
#    language = "af",
#    what = "subs_vec"
#  )

## ----echo = F, message = F-----------------
af_dims <- read.table("subs_vec/af/subs.af.1e6.txt", quote = "\"")

## ------------------------------------------
head(tibble(af_dims))

## ------------------------------------------
# lower case
af_dims$V1 <- tolower(af_dims[ , 1]) # first column is always the tokens

# eliminate duplicates 
af_dims <- subset(af_dims, !duplicated(af_dims[ , 1]))

# make row names
rownames(af_dims) <- af_dims[ , 1]
af_dims <- af_dims[ , -1]
head(tibble(af_dims))

## ------------------------------------------
af_cosine <- 
  calculate_similarity(
    words = af_final$sentence, # the tokens you want to filter
    dimensions = af_dims, # the matrix of items 
    by = 1 # 1 for rows, 2 for columns 
)

## ------------------------------------------
# get the top 5 related words 
af_top_sim <- semanticprimeR::top_n(af_cosine, 6)
af_top_sim <- subset(af_top_sim, cue!=target)

head(af_top_sim)

## ------------------------------------------
af_top_sim$fake_cue <- fake_simple(af_top_sim$cue)
# you'd want to also do this based on target depending on your study 
head(af_top_sim)

## ----message=FALSE-------------------------
af_wuggy <- fake_Wuggy(
  wordlist = af_final$sentence, # full valid options in language
  language_hyp = "../inst/latex/hyph-af.tex", # path to hyphenation.tex 
  lang = "af", # two letter language code
  replacewords <- unique(af_top_sim$cue[1:20]) # words you want to create pseudowords for  
)

head(tibble(af_wuggy))

## ------------------------------------------
data("primeData")
head(primeData)

## ----eval = F------------------------------
#  es_words <- import_prime("es_words.csv")

## ----echo = F------------------------------
es_words <- import('procedure_stimuli/es/es_words.csv')

## ------------------------------------------
head(es_words)

## ------------------------------------------
data("labData")
head(tibble(labData))

# import_lab() also loads this dataset 

# ?labData # use this to learn about the dataset 

## ------------------------------------------
saved <- import_lab(language = "English", variables = c("aoa", "freq"))
# possible datasets that are English, aoa, and frequency
head(tibble(saved))

saved <- import_lab(language = "Spanish", variables = c("aoa"))

head(tibble(saved))

## ----eval = F------------------------------
#  es_aos <- import_lab(bibtexID = "Alonso2015", citation = TRUE)

## ----echo = F------------------------------
# save(es_aos, file = "lab_data/es_aos.Rdata")
load("lab_data/es_aos.Rdata")

## ------------------------------------------
es_aos$citation

head(tibble(es_aos$loaded_data))

## ----eval = F------------------------------
#  es_sim <- import_lab(bibtexID = "Cabana2024_R1", citation = TRUE)

## ----echo = F------------------------------
# save(es_sim, file = "lab_data/es_sim.Rdata")
load("lab_data/es_sim.Rdata")

## ------------------------------------------
es_sim$citation

head(tibble(es_sim$loaded_data))

## ------------------------------------------
es_words_merged <- es_words %>%
  # merge with the cue word (will be .x variables)
  left_join(es_aos$loaded_data, 
            by = c("es_cue" = "word_spanish")) %>% 
  # merge with the target word (will be .y variables)
  left_join(es_aos$loaded_data, 
            by = c("es_target" = "word_spanish")) %>% 
  # merge with free association similarity
  left_join(es_sim$loaded_data, 
            by = c("es_cue" = "cue",
                   "es_target" = "response"))

head(tibble(es_words_merged))

## ----eval = F------------------------------
#  df <- processData("data.sqlite")

