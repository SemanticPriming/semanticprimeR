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
#' \itemize{
#'  \item{language_code}{the two letter language code of the model}
#'  \item{subs_vec}{a link to download the subtitle only fastText model}
#'  \item{subs_count}{a link to download the frequencies for the tokens
#'  in the subtitle data}
#'  \item{wiki_vec}{a link to download the wikipedia only fastText model}
#'  \item{wiki_count}{a link to download the frequencies for the tokens
#'  in the wikipedia data}
#'  \item{files}{the number of files in the OpenSubtitles data}
#'  \item{tokens}{the number of tokens in the OpenSubtitles data}
#'  \item{sentences}{the number of sentences in the OpenSubtitles data}
#'  \item{language}{the full name of the language for reference}
#'  \item{udpipe_model}{the matching `udpipe` model for download to
#'  parse tokens}
#' }
#'
#' @keywords datasets
#'
"subsData"
