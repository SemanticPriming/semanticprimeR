#' Calculate pseudowords with the Wuggy Algorithm
#'
#' This function takes a list of tokens and returns a list
#' of potential generated pseudowords by using the Wuggy.
#' Note: you should check the list, as random generation
#' can generate a new real token. Note: this function is
#' fairly slow as the word list gets larger. This function
#' uses bigrams as syllables for one syllable words.
#'
#' @param wordlist A list of valid words from which to
#' calculate the frequencies of syllables and transition
#' ngrams from.
#' @param language_hyp The language hyphenation you want
#' to use. You can find them to download at https://hyphenation.org/.
#' Or check out the ones we used in our package /inst/latex.
#' @param lang The two letter language code for the language you
#' imported for hyphenation.
#' @param replacewords A list of tokens you want to use to create
#' your pseudowords.
#'
#' @import sylly dplyr tidyr tidytext stringr
#' @importFrom stringdist stringdist
#'
#' @return A dataset of original tokens and suggested pseudowords.
#' \itemize{
#'  \item{"word_id"}{Number id for each unique word}
#'  \item{"first"}{First syllable in pairs of syllables.}
#'  \item{"original_pair"}{Pair of syllables together.}
#'  \item{"second"}{Second syllable in the pairs of syllables.}
#'  \item{"syll"}{Number of syllables in the token.}
#'  \item{"original_freq"}{Frequency of the syllable pair.}
#'  \item{"replacement_pair"}{Replacement option wherein
#'  one of the syllables has been changed. }
#'  \item{"replacement_syll"}{The replacement syllable.}
#'  \item{"replacement_freq"}{The frequency of the replacement
#'  syllable pair. }
#'  \item{"freq_diff"}{The difference in frequency of the transition
#'  pair.}
#'  \item{"char_diff"}{Number of characters difference in the original
#'  pair and the replacement pair. }
#'  \item{"letter_diff"}{Number of letters difference in the original
#'  pair and the replacement pair. If the replacement includes the
#'  same letters, the difference would be zero. These values are
#'  excluded from being options.}
#'  \item{"original_word"}{The original token.}
#'  \item{"replacement_word"}{The final replacement token.}
#' }
#'
#' @export
#'
#' @examples
#' # af_wuggy <- fake_Wuggy(
#' # wordlist = af_final$sentence, # full valid options in language
#' # language_hyp = "../inst/latex/hyph-af.tex", # path to hyphenation.tex
#' # lang = "af", # two letter language code
#' # replacewords <- unique(af_top_sim$cue[1:20]) # words you want to create pseudowords for
#' # )
#'

fake_Wuggy <- function(wordlist,
                     language_hyp,
                     lang,
                     replacewords){

  requireNamespace("dplyr")

  if (is.null(wordlist)){ stop("You must include a wordlist
    of the valid tokens.") }
  if (is.null(language_hyp)){ stop("You must include a set of
    hyphenation patterns for your language.") }
  if (is.null(lang)){ stop("You must include the two letter
                           language code for the language
                           of hyphenation.") }
  if (is.null(replacewords)){ stop("You must include a set of
    words you want to create pseudowords for. ") }

  # figure out possible combinations ---
  # figure out the syllables
  language_hyp <- sylly::read.hyph.pat(language_hyp, lang = lang)
  hyp_words <- sylly::hyphen(as.character(wordlist), hyph.pattern = language_hyp)
  hyp_words <- hyp_words@hyphen
  hyp_words$word <- tolower(hyp_words$word)

  # for multiple syllable words
  multiple <- hyp_words %>% dplyr::filter(syll > 1)
  # create pattern [blank, first] [first, second] [second, blank]
  if (nrow(multiple) >= 1){
    multiple$word <- paste("FIRST_BLANK-", multiple$word, "-LAST_BLANK", sep = "")
  }

  # for single syllable words
  single <- hyp_words %>% filter(syll == 1)
  single$word <- gsub("(.{2})", "\\1-", single$word)
  single$word <- gsub("-$", "", single$word)
  single$word <- paste("FIRST_BLANK-", single$word, "-LAST_BLANK", sep = "")

  all <- rbind(single, multiple)
  all$word_id <- 1:nrow(all)

  # break them down
  replacements <- tidytext::unnest_tokens(all,
                                output = "syllable",
                                input = word,
                                token = strsplit, split = "-") %>%
    group_by(word_id) %>%
    mutate(pair = paste(syllable, lead(syllable, 1), sep = "-")) %>%
    filter(!grepl("NA", pair)) %>%
    ungroup %>%
    group_by(pair) %>%
    summarise(Frequency = n()) %>%
    tidyr::separate(pair, c("first", "second"), "-", remove = F) %>%
    mutate(first = gsub("\\s", "", first)) %>%
    filter(first != "") %>%
    filter(!grepl("^[[:space:]]", first))

  # break down replacement word list ---
  hyp_replace <- sylly::hyphen(as.character(replacewords), hyph.pattern = language_hyp)
  hyp_replace <- hyp_replace@hyphen
  hyp_replace$word <- tolower(hyp_replace$word)
  hyp_replace$word_id <- 1:nrow(hyp_replace)

  # for multiple syllable words
  multiple <- hyp_replace %>% dplyr::filter(syll > 1)

  if (nrow(multiple) >= 1){
    # create pattern [blank, first] [first, second] [second, blank]
    multiple$word <- paste("FIRST_BLANK-", multiple$word, "-LAST_BLANK", sep = "")
  }

  # for single syllable words
  single <- hyp_replace %>% dplyr::filter(syll == 1)
  single$word <- gsub("(.{2})", "\\1-", single$word)
  single$word <- gsub("-$", "", single$word)
  single$word <- paste("FIRST_BLANK-", single$word, "-LAST_BLANK", sep = "")

  all <- rbind(single, multiple)

  to_replace <- tidytext::unnest_tokens(all,
                              output = "syllable",
                              input = word,
                              token = strsplit, split = "-") %>%
    group_by(word_id) %>%
    mutate(pair = paste(syllable, lead(syllable, 1), sep = "-")) %>%
    filter(!grepl("NA", pair))

  # merge replacements with things to replace to find frequency
  to_replace_options <- merge(to_replace, replacements, by = "pair", all.x = T) %>%
    select(-syllable) %>%
    tidyr::separate(pair, c("first", "second"), "-", remove = FALSE)

  # now merge by first, then second to get possible replacements
  to_replace_options1 <- merge(to_replace_options, replacements,
                               by = "first", all.x = T)
  colnames(to_replace_options1) <- c("first", "original_pair", "second",
                                     "syll", "word_id", "original_freq",
                                     "replacement_pair", "replacement_syll",
                                     "replacement_freq")
  to_replace_options2 <- merge(to_replace_options, replacements,
                               by = "second", all.x = T)
  colnames(to_replace_options2) <- c("second", "original_pair", "first",
                                     "syll", "word_id", "original_freq",
                                     "replacement_pair", "replacement_syll",
                                     "replacement_freq")

  replace_options <- rbind(to_replace_options1, to_replace_options2)
  replace_options$original_freq[is.na(replace_options$original_freq)] <- 1

  replace_options$freq_diff <- abs(replace_options$original_freq - replace_options$replacement_freq)
  replace_options$char_diff <- abs(nchar(replace_options$original_pair) -
                                     nchar(replace_options$replacement_pair))

  replace_options$letter_diff <- stringdist::stringdist(a = replace_options$original_pair, b = replace_options$replacement_pair) - replace_options$syll

  # replacements can't be blank
  # can't be the same replacement
  replace_options <- replace_options %>%
    dplyr::filter(replacement_syll != "first_blank") %>%
    dplyr::filter(replacement_syll != "last_blank") %>%
    dplyr::filter(letter_diff > 0)

  # merge the words back in
  replacewords <- as.data.frame(replacewords)
  colnames(replacewords) <- "original_word"
  replacewords$word_id <- 1:nrow(replacewords)
  replace_options <- merge(replace_options, replacewords,
                           by = "word_id", all = T)
  replace_options <- merge(replace_options, all[ , -1],
                           by = "word_id", all = T)
  colnames(replace_options)[ncol(replace_options)] <- "replacement_word"
  replace_options$replacement_word <- tolower(replace_options$replacement_word)

  replace_options$replacement_word <- stringr::str_replace(
    string = replace_options$replacement_word,
    pattern = replace_options$original_pair,
    replacement = replace_options$replacement_pair)
  replace_options$replacement_word <- gsub("first_blank|last_blank|-", "", replace_options$replacement_word)

  # remove replacement words that are in the original
  replace_options <- replace_options %>%
    dplyr::filter(!(replacement_word %in% wordlist))

  # char diff to be zero
  # letter diff to be lowest to match syllable
  # frequency diff to be < 2 but lowest
  replace_options <- replace_options %>%
    group_by(original_word) %>%
    arrange(char_diff, freq_diff, letter_diff, .by_group = TRUE)

  new_words <- replace_options %>%
    dplyr::filter(!duplicated(original_word))

  return(new_words)
}
