#' Simulate Normal Population Scores for Multiple Items
#'
#' This function allows you to create normal populations
#' for data that would include repeated measures items.
#' Additionally, the data can be rounded and/or truncated
#' to ensure it matches a target scale - for example, a
#' 1-7 type rating scale.
#'
#' @param samples The bootstrapped samples from your population
#' @param cutoff The cutoff score for an item to be well measured from
#' the standard errors of your items
#' @param grouping_items The names of columns to group your data
#' by for the cutoff calculation, usually this column is the item column
#' @param score The column of the score you wish to calculate for your
#' cutoff score SE
#'
#' @return
#' \itemize{
#'  \item{"summary_DF"}{Summary of the proportion of items
#' below the standard error cutoff score. }
#' }
#'
#' @export
#'
#' @examples
#' # step 1 create data like what I think I'll get or use your own
#' pops <- simulate_population(mu = 4, mu_sigma = .2, sigma = 2,
#'                             sigma_sigma = .2, number_items = 30, number_scores = 20,
#'                             smallest_sigma = .02, min_score = 1, max_score = 7, digits = 0)
#'
#' # step 2 calculate our cut off score
#' cutoff <- calculate_cutoff(population = pops,
#'  grouping_items = "item",
#'  score = "score",
#'  minimum = 1,
#'  maximum = 7)
#'
#' cutoff$se_items
#' cutoff$sd_items
#' cutoff$cutoff
#' cutoff$prop_var
#'
#' # step 3 simulate bootstrapped samples
#' samples <- bootstrap_samples(start = 20, stop = 100,
#'  increase = 5, population = pops,
#'  replace = TRUE, grouping_items = "item")
#'
#' # step 4 and 5
#' proportion_summary <- calculate_proportion(samples = samples,
#'  cutoff = cutoff$cutoff,
#'  grouping_items = "item",
#'  score = "score")
#' proportion_summary

calculate_proportion <- function(samples, cutoff, grouping_items = NULL,
                              score){

  if(is.null(samples)){ stop("You must include list of samples to examine.") }
  if(is.null(cutoff)){ stop("You must include the cutoff score for standard error.") }
  if(is.null(grouping_items)){ stop("You must include the grouping variable,
                                    which is normally the item number.") }
  if(is.null(score)){ stop("You must include the score or variable you are
                           estimating sample size for.") }

  summary_list <- list()
  score <- sym(score)

  # loop and calculate
  for (i in 1:length(samples)){

    summary_list[[i]] <- samples[[i]] %>%
      group_by(across(grouping_items)) %>%
      summarize(se = sd(score)/sqrt(n())) %>%
      ungroup() %>%
      summarize(percent_below = sum(se <= cutoff) / length(se),
                num_items = length(se)) %>%
      mutate(sample_size = nrow(samples[[i]]) / num_items)

  } # end loop and calculate

  # create end summary
  summary_DF <- bind_rows(summary_list) %>%
    select(-num_items)

  # return values
  return(summary_DF)

}
