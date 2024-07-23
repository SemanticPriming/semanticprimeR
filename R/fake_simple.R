#' Calculate pseudowords by replacing bigrams/single letters
#'
#' This function takes a list of tokens and returns a list
#' of potential generated pseudowords by replacing characters
#' based on legal bigrams within the word list set. Note: you
#' should check the list, as random generation can generate
#' a new real token.
#'
#' @param wordlist a list of tokens you want to use to create
#' your pseudowords.
#'
#' @importFrom quanteda tokens_ngrams
#' @importFrom quanteda tokens
#'
#' @return A dataset of original tokens and suggested pseudowords.
#'
#' @export
#'
#' @examples
#' # af_top_sim$fake_cue <- fake_simple(af_top_sim$cue)
#'
fake_simple <- function(wordlist){

  bigrams <- as.data.frame(
    table(unlist(
      quanteda::tokens_ngrams(
        quanteda::tokens(wordlist, what = "character"),
        concatenator = "")
    ))
    )

  new_words <- rep(NA, length(wordlist))

  for (i in 1:length(wordlist)){

    word <- wordlist[i]
    # pick a random letter
    num_replace <- sample(1:nchar(word), 1)
    char_replace <- substr(word, num_replace, num_replace)

    # if the first letter
    if (num_replace == 1){

      examine_replace <- substr(word, num_replace + 1, num_replace + 1)
      poss_replace <- sample(substr(
        bigrams$Var1[grepl(paste0(examine_replace, "$"), bigrams$Var1)], 1, 1),1)

      broken_down <- unlist(strsplit(word, split = ""))
      broken_down[num_replace] <- poss_replace
      new_words[i] <- paste(broken_down, collapse = "")

      # if the last letter
    } else if (num_replace == nchar(word)){

      examine_replace <- substr(word, num_replace - 1, num_replace - 1)
      poss_replace <- sample(substr(
        bigrams$Var1[grepl(paste0("^", examine_replace), bigrams$Var1)], 2, 2),1)

      broken_down <- unlist(strsplit(word, split = ""))
      broken_down[num_replace] <- poss_replace
      new_words[i] <- paste(broken_down, collapse = "")

      # if anything else
    } else {

      examine_replace_before <- substr(word, num_replace - 1, num_replace - 1)
      examine_replace_after <- substr(word, num_replace + 1, num_replace + 1)

      intersection <- intersect(substr(bigrams$Var1[
        grepl(paste0("^", examine_replace_before),
              bigrams$Var1)], 2, 2),
        substr(bigrams$Var1[
          grepl(paste0(examine_replace_after, "$"),
                bigrams$Var1)], 1, 1))

      if (length(intersection) > 0 ){
        poss_replace <- sample(intersection, 1)
      } else { poss_replace <- sample(substr(bigrams$Var1[
        grepl(paste0("^", examine_replace_before),
              bigrams$Var1)], 2, 2), 1)
      }

      broken_down <- unlist(strsplit(word, split = ""))
      broken_down[num_replace] <- poss_replace
      new_words[i] <- paste(broken_down, collapse = "")

    }
  }
  return(new_words)
}
