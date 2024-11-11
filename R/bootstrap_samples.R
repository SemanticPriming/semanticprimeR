#' Simulate Sample from a Population Grouped by Items
#'
#' This function allows you to simulate samples across
#' various sample sizes when the data (optionally) has
#' repeated measures items.
#'
#' @param start Sample size for the smallest potential sample
#' @param stop Sample size for the largest potential sample
#' @param increase Number to increase the sample size with for
#' each potential sample
#' @param population The population data set or the pilot dataset
#' @param replace A TRUE/FALSE value to simulate with replacement
#' @param nsim The number of simulations/samples you want to return
#' @param grouping_items The names of columns to group your data
#' by for the simulation, usually this column is the item column
#'
#' @import dplyr
#'
#' @return
#' \describe{
#'  \item{samples}{The simulated samples for each tested sample
#'  size. }
#' }
#'
#' @export
#'
#' @examples
#'
#' # step 1 create data like what I think I'll get or use your own
#' pops <- simulate_population(mu = 4, mu_sigma = .2, sigma = 2,
#'   sigma_sigma = .2, number_items = 30, number_scores = 20,
#'   smallest_sigma = .02, min_score = 1, max_score = 7, digits = 0)
#'
#' # step 3 simulate samples
#' samples <- simulate_samples(start = 20, stop = 100,
#'   increase = 5, population = pops,
#'   replace = TRUE, grouping_items = NULL)
#'
#' # notice just 20 items
#' samples[[1]]
#'
#' samples <- simulate_samples(start = 20, stop = 100,
#'   increase = 5, population = pops,
#'   replace = TRUE, grouping_items = "item")
#'
#' # notice 20 rows per item
#' samples[[1]]
#'
simulate_samples <- function(
    start = 20,
    stop = 100,
    increase = 5,
    population,
    replace = TRUE,
    nsim = 100,
    grouping_items = NULL){

  if(is.null(population)){ stop("You must include the population or pilot data.") }

  # save the samples
  boot.samples <- list()

  # create the list of sizes
  sizes <- seq(from = start, to = stop, by = increase)

  iterate <- 1
  # loop over number of sims
  for (p in 1:nsim){

  # loop over sizes and create those samples
    for (i in 1:length(sizes)){

      if (!is.null(grouping_items)){

        boot.samples[[iterate]] <- population %>%
          group_by(.data[[grouping_items]]) %>%
          slice_sample(n = sizes[i], replace = replace)
        iterate <- iterate + 1

      } else {

        boot.samples[[iterate]] <-  population %>%
          slice_sample(n = sizes[i], replace = replace)
        iterate <- iterate + 1

      }

    }

  }

  # return your values
  return(boot.samples)

}
