% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fake_wuggy.R
\name{fake_Wuggy}
\alias{fake_Wuggy}
\title{Calculate pseudowords with the Wuggy Algorithm}
\usage{
fake_Wuggy(wordlist, language_hyp, lang, replacewords)
}
\arguments{
\item{wordlist}{A list of valid words from which to
calculate the frequencies of syllables and transition
ngrams from.}

\item{language_hyp}{The language hyphenation you want
to use. You can find them to download at https://hyphenation.org/.
Or check out the ones we used in our package /inst/latex.}

\item{lang}{The two letter language code for the language you
imported for hyphenation.}

\item{replacewords}{A list of tokens you want to use to create
your pseudowords.}
}
\value{
A dataset of original tokens and suggested pseudowords.
\describe{
 \item{word_id}{Number id for each unique word}
 \item{first}{First syllable in pairs of syllables.}
 \item{original_pair}{Pair of syllables together.}
 \item{second}{Second syllable in the pairs of syllables.}
 \item{syll}{Number of syllables in the token.}
 \item{original_freq}{Frequency of the syllable pair.}
 \item{replacement_pair}{Replacement option wherein
 one of the syllables has been changed. }
 \item{replacement_syll}{The replacement syllable.}
 \item{replacement_freq}{The frequency of the replacement
 syllable pair. }
 \item{freq_diff}{The difference in frequency of the transition
 pair.}
 \item{char_diff}{Number of characters difference in the original
 pair and the replacement pair. }
 \item{letter_diff}{Number of letters difference in the original
 pair and the replacement pair. If the replacement includes the
 same letters, the difference would be zero. These values are
 excluded from being options.}
 \item{original_word}{The original token.}
 \item{replacement_word}{The final replacement token.}
}
}
\description{
This function takes a list of tokens and returns a list
of potential generated pseudowords by using the Wuggy.
Note: you should check the list, as random generation
can generate a new real token. Note: this function is
fairly slow as the word list gets larger. This function
uses bigrams as syllables for one syllable words.
}
\examples{
# af_wuggy <- fake_Wuggy(
# wordlist = af_final$sentence, # full valid options in language
# language_hyp = "../inst/latex/hyph-af.tex", # path to hyphenation.tex
# lang = "af", # two letter language code
# replacewords <- unique(af_top_sim$cue[1:20]) # words you want to create pseudowords for
# )

}
