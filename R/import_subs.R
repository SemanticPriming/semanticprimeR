#' Get subs2vec model dimensions or frequency counts
#'
#' This function allows you to import the current datasets available from
#' the subs2vec project. You can use `data("subsData")` to view the
#' list of available models.
#'
#' @param language Include a two letter code of the language you wish
#' to download. Note: some of the `_vec` files are very large. It
#' may take a while to download and import them. Use the tokens column
#' to get an idea of how big the data is.
#' @param what What would you like to download? Options are: `subs_vec`,
#' `subs_count`, `wiki_vec`, and `wiki_count`. The `subs` are models
#' based on the subtitles, the `wiki` are models based on Wikipedia data,
#' `_vec` indicates the fastText model dimensions of words by dimension
#' score, and the `_count` indicates the frequency counts for that
#' data.
#'
#' @return a dataset of either words by dimensions or the tokens
#' including the frequency counts.
#'
#' @keywords metadata, datasets, linguistic norms
#' @export
#' @examples
#' # af_dims <- import_subs(
#' #   language = "af",
#' #  what = "subs_vec"
#' # )

import_subs <- function(language, what){

  data("subsData")

  dir.create(paste(what), showWarnings = F)
  dir.create(paste0(what, "/", language), showWarnings = F)

  con <-
    paste0(subsData[subsData$language_code == language , what ])


  # goes into data / language
  download.file(
    con,
    destfile = paste0(what, '/', language, "/", language, ".zip"),
    mode = "wb")

  # unzip
  unzip(zipfile = paste0(what, '/', language, "/", language, ".zip"),
        exdir = paste0(what, '/', language))

  # rename
  files <- unzip(paste0(what, '/', language, "/", language, ".zip"),
                 list=T)$Name ## get their orig names
  suppressMessages(
    file.rename(from = paste0(what, '/', language, "/", files),
              to = paste0(what, '/', language, "/",
                          substr(x = files,
                                 start = 1,
                                 stop = nchar(files)-3), "txt"))
  )

  if (grepl("count", what)){
    imported <- rio::import(
      paste0(what, '/', language, "/",
             substr(x = files,
                    start = 1,
                    stop = nchar(files)-3), "txt")
    )

  } else {
    imported <- read.table(paste0(what, '/', language, "/",
                                  substr(x = files,
                                         start = 1,
                                         stop = nchar(files)-3), "txt"),
                           quote="\"")
  }

  return(imported)

}
