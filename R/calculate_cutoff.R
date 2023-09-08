#' Calculate the Cutoff Score for SE of Items
#'
#' This function allows you to bootstrap samples across
#' various sample sizes when the data (optionally) has
#' repeated measures items.
#'
#' @param population The population data set or the pilot dataset
#' @param grouping_items The names of columns to group your data
#' by for the cutoff calculation, usually this column is the item column
#' @param score The column of the score you wish to calculate for your
#' cutoff score SE
#' @param minimum The minimum possible value for your score, used to
#' calculate the proportion of variability in your items
#' @param maximum The maximum possible value for your score, used to
#' calculate the proportion of variability in your items
#'
#' @import dplyr
#'
#' @return
#'
#' \itemize{
#'  \item{"se_items"}{The standard errors for each of your items.}
#'  \item{"sd_items"}{The standard deviation of the standard
#'  errors of your items.}
#'  \item{"cutoff"}{The cutoff score for your estimation of sample
#'  size by item.}
#'  \item{"prop_var"}{The proportion of variability found in your
#'  items, used to calculate the revised sample from simulations.}
#' }
#'
#' @export
#'
#' @examples
#' # step 1 create data like what I think I'll get or use your own
#' pops <- simulate_population(mu = 4, mu_sigma = .2, sigma = 2,
#'  sigma_sigma = .2, number_items = 30, number_scores = 20,
#'  smallest_sigma = .02, min_score = 1, max_score = 7, digits = 0)
#' # step 2 calculate our cut off score
#' cutoff <- calculate_cutoff(population = pops,
#'   grouping_items = "item",
#'   score = "score",
#'   minimum = 1,
#'   maximum = 7)
#'
#' cutoff$se_items
#' cutoff$sd_items
#' cutoff$cutoff
#' cutoff$prop_var
calculate_cutoff <- function(population, grouping_items,
                             score, minimum, maximum){

  if(is.null(population)){ stop("You must include the population or pilot data.") }
  if(is.null(grouping_items)){ stop("You must include name of the column for items.") }
  if(is.null(score)){ stop("You must include name of the column for the score.") }
  if(is.null(minimum)){ stop("You must include the minimum value for the possible score.") }
  if(is.null(maximum)){ stop("You must include the maximum value for the possible score.") }

  score <- sym(score)

  se_item <- population %>%
    filter(!is.na(score)) %>%
    group_by(across(grouping_items)) %>%
    summarize(se = sd(score)/sqrt(n())) %>%
    pull(se)
  cutoff <- quantile(se_item, probs = .4, na.rm = T)
  sd_items <- sd(se_item)
  prop_var <- sd_items / sqrt(((maximum - minimum)^2/4))

  # return information
  return(list(se_items = se_item,
              sd_items = sd_items,
              cutoff = cutoff,
              prop_var = prop_var))

}
