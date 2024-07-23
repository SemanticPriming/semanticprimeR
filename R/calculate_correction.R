#' Bootstrap Sample from a Population Grouped by Items
#'
#' This function allows you to bootstrap samples across
#' various sample sizes when the data (optionally) has
#' repeated measures items.
#'
#' @param proportion_summary Summary from proportion of items below
#' the cutoff score.
#' @param pilot_sample_size Number of participants from the pilot sample,
#' note: not the number of items, but the number of people per item.
#' @param proportion_variability Proportion of variability found between
#' items standard errors.
#' @param power_levels Power levels to calculate the required sample size.
#'
#' @import dplyr
#'
#' @return
#' \describe{
#'  \item{corrected_summary}{The corrected sample size for the
#'  number of people needed for accurately measured items.}
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
#'
#' # step 6 final calculation
#' corrected_summary <- calculate_correction(proportion_summary = proportion_summary,
#'   pilot_sample_size = 20,
#'   proportion_variability = cutoff$prop_var,
#'   power_levels = c(80, 85, 90, 95))
#'
#' corrected_summary
#'
calculate_correction <- function(proportion_summary,
                                 pilot_sample_size,
                                 proportion_variability,
                                 power_levels = c(80, 85, 90, 95)){

  if(is.null(proportion_summary)){ stop("You must include the summarized percent
  summary scores.") }

  proportion_summary$percent_below <- proportion_summary$percent_below*100

  # find power
  summary_table <- list()
  for (i in 1:length(power_levels)){
    summary_table[[i]] <- proportion_summary %>%
      filter(percent_below >= power_levels[i]) %>%
      arrange(sample_size) %>%
      slice_head()
  }

  summary_table <- bind_rows(summary_table)

  summary_table$intercept <- 206.589
  summary_table$projected_ss <- 0.368 * summary_table$sample_size
  summary_table$pilot_ss <- -0.77 * pilot_sample_size
  summary_table$log_projected_ss <- 27.541000 * log2(summary_table$sample_size)
  summary_table$log_pilot_ss <- 2.583000 * log2(pilot_sample_size)
  summary_table$log_power <- -66.151000 * log2(summary_table$percent_below)
  summary_table$prop_var <- 16.40500000 * proportion_variability
  summary_table$log_prop_var <- -1.367000 * log2(proportion_variability)
  summary_table$power <- 1.088 * (summary_table$percent_below)

  summary_table$corrected_sample_size <-
    apply(summary_table %>% select(intercept:power), 1, sum)

  summary_table <- summary_table %>%
    select(percent_below, sample_size, corrected_sample_size)

  return(summary_table)
}
