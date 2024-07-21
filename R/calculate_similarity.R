#' Calculate the Cosine Similarity Between Vectors
#'
#' This function uses the `lsa` package to calculate the
#' `cos` between vectors. For this function, you should have
#' a matrix of tokens by dimensions, and you can specify
#' which direction you want to calculate the cosine (by
#' rows or columns).
#'
#'
#' @param words If you want to filter the matrix to only
#' a set of tokens, include a vector of tokens. The `by`
#' argument will be used to filter the data to only these
#' values. Note that if you want to filter by rows, the
#' row names should represent your tokens, not the first column.
#' @param dimensions Your matrix of numerical dimension values.
#' @param by use `1` to calculate by row and `2` to calculate
#' by column.
#'
#' @import lsa
#'
#' @return A matrix of cosine values between either rows
#' or columns.
#'
#' @export
#'
#' @examples
#' # af_cosine <- calculate_similarity(
#' #   words = af_final$sentence, # the tokens you want to filter
#' #   dimensions = af_dims, # the matrix of items
#' #   by = 1 # 1 for rows, 2 for columns)
#'

calculate_similarity <- function(
    words = NULL,
    dimensions,
    by = 1){

  if (!is.numeric(by)){ stop("by must be a numeric argument of 1 or 2.") }
  if (is.null(dimensions)){ stop("You must include a matrix of dimensions.") }

  if (by == 1){
    if (!is.null(words)){
      dimensions <- subset(dimensions, rownames(dimensions) %in% words)
    }
    # turn it because cosine is by columns
    dimensions <- t(dimensions)
    similarity <- lsa::cosine(as.matrix(dimensions))
  } else {

    if (!is.null(words)){
      dimensions <- dimensions[ , colnames(dimensions) %in% words ]
    }
    similarity <- lsa::cosine(as.matrix(dimensions))
  }

  return(similarity)
}
