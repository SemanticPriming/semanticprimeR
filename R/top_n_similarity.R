# credit to Brenton Wiernik
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
