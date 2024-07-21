#' subs2vec Project Model and Frequency Data Downloads
#'
#' Dataset to use the \code{\link{import_subs}} function to
#' import subtitle fastText model outputs and frequency counts.
#' Includes information about matching `udpipe` models for
#' tagging.
#'
#' @docType data
#'
#' @usage data(subsData)
#'
#' @format A data frame of links and information about the
#' subs2vec project.
#'
#' language_code: the two letter language code of the model
#' subs_vec: a link to download the subtitle only fastText model
#' subs_count: a link to download the frequencies for the tokens
#' in the subtitle data
#' wiki_vec: a link to download the wikipedia only fastText model
#' wiki_count: a link to download the frequencies for the tokens
#' in the wikipedia data
#' files: the number of files in the OpenSubtitles data
#' tokens: the number of tokens in the OpenSubtitles data
#' sentences: the number of sentences in the OpenSubtitles data
#' language: the full name of the language for reference
#' udpipe_model: the matching `udpipe` model for download to
#' parse tokens
#'
#' @keywords datasets
#'
"subsData"
