#' Calculate the Top N Cosines
#'
#' This function takes a matrix of cosine similarity values,
#' where the row and column names are tokens. The values will
#' be calculated for the columns (so if you have a non-square)
#' matrix, make sure the columns are the values you want.
#'
#'
#' @param similarity the matrix of cosine values.
#' @param n the number of most related cosine values you want
#' to return. Remember that you will get token-token cosine back
#' as a value of 1, so use n+1 to get the number you want.
#'
#' @return A dataframe of token-token-cosine values.
#'
#' @export
#'
#' @examples
#' # af_top_sim <- top_n(af_cosine, 6)
#'
top_n <- function(similarity, n) {
  n_cols <- ncol(similarity)
  final_sims <- data.frame(
    cue = vector("character", n * n_cols),
    target = vector("character", n * n_cols),
    cosine = vector("numeric", n * n_cols)
  )
  columns <- colnames(similarity)
  rows <- rownames(similarity)
  indices <- apply(similarity, 2, function(x) order(x, decreasing = TRUE)[seq_len(n)])
  for (i in seq_len(n_cols)) {
    final_sims[(i - 1) * n + 1:n,] <- list(
      columns[i],
      rows[indices[,i]],
      similarity[indices[,i],i]
    )
  }
  return(final_sims)
}
