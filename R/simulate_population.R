#' Simulate Normal Population Scores for Multiple Items
#'
#' This function allows you to create normal populations
#' for data that would include repeated measures items.
#' Additionally, the data can be rounded and/or truncated
#' to ensure it matches a target scale - for example, a
#' 1-7 type rating scale.
#'
#' @param mu The population mean for the items.
#' @param mu_sigma The amount of variability for each of the
#' means (i.e., the standard deviation of the item's means)
#' @param sigma The population standard deviation for
#' each item.
#' @param sigma_sigma The standard deviation of the standard
#' deviations for the items (i.e., heterogeneity)
#' @param number_items The number of items to generate
#' @param number_scores The number of scores for each item to generate
#' @param smallest_sigma The smallest possible standard deviation
#' for an item that is acceptable (default is sigma_sigma/10)
#' @param min_score If you want to truncate scores, what is the
#' smallest possible score?
#' @param max_score If you want to truncate scores, what is the
#' largest possible score?
#' @param digits If you want to round scores, how many digits
#' should it be rounded to?
#'
#' @return
#' \describe{
#'  \item{"population"}{The population scores with the number of items
#' and scores specified}
#' }
#'
#' @export
#'
#' @examples
#' simulate_population(mu = 4, mu_sigma = .2, sigma = 2,
#'  sigma_sigma = .2, number_items = 30, number_scores = 1000,
#'  smallest_sigma = .02, min_score = 1, max_score = 7,
#'  digits = 0)
simulate_population <- function (
    mu = NULL,
    mu_sigma = NULL,
    sigma = NULL,
    sigma_sigma = NULL,
    number_items = NULL,
    number_scores = NULL,
    smallest_sigma = NULL,
    min_score = NULL,
    max_score = NULL,
    digits = NULL
){

  if(is.null(mu)){ stop("You must include a mean value for mu.") }
  if(is.null(mu_sigma)){ stop("You must include a variation for the item means value for mu_sigma.") }
  if(is.null(sigma)){ stop("You must include a population standard deviation sigma.") }
  if(is.null(sigma_sigma)){ stop("You must include a variability for the standard deviations (i.e., the standard deviation of standard deviations.") }
  if(is.null(number_items)){ stop("You must include the number of items to simulate.") }
  if(is.null(number_scores)){ stop("You must include the number of scores to simulate.") }

  if(is.null(smallest_sigma)){ smallest_sigma <- sigma_sigma/10 }

  mu_s <- stats::rnorm(number_items, mu, mu_sigma)
  sigma_s <- stats::rnorm(number_items, sigma, sigma_sigma)

  while(sum(sigma_s < smallest_sigma) > 0){
    sigma_s <- stats::rnorm(number_items, sigma, sigma_sigma)
  }

  population <- data.frame(
    item = rep(1:number_items, number_scores),
    score = stats::rnorm(number_items*number_scores, mean = mu_s, sd = sigma_s)
  )

  if (!is.null(digits)){
    population$score <- round(population$score, digits = digits)
  }

  if (!is.null(min_score)){
    population$score <- ifelse(population$score < min_score,
                               min_score,
                               population$score)
  }

  if (!is.null(max_score)){
    population$score <- ifelse(population$score > max_score,
                               max_score,
                               population$score)
  }

  # return populations
  return(population)
}
