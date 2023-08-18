calculate_similarity <- function(words, vec){
  # reduce the data
  vec <- subset(vec, V1 %in% words)
  rownames(vec) <- vec$V1
  vec <- vec[ , -1]
  vec <- t(vec)
  similarity <- cosine(as.matrix(vec))
  return(similarity)
}
