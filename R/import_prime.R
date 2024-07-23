#' Import the SPAML Data or Stimuli
#'
#' This function allows you to import the current datasets available from
#' the SPAML project. You can use `data("primeData")` to view the
#' list of available datasets/materials.
#'
#' @param filename Include the filename of the dataset you want to load.
#' Note: some of the `full_data` datasets are very large and require
#' a good bit of memory to load.
#'
#' @return a dataset or stimuli list in a dataframe.
#'
#' @import rio
#'
#' @keywords metadata, datasets, linguistic norms
#' @export
#' @examples
#' # ar_words <- import_prime("ar_words.csv")
#'

import_prime <- function(filename){

  data("primeData")
  con <- primeData$location[ primeData$filename == filename ]
  if (nchar(con) < 3) { stop("Please make sure you've specified the
                             filename correctly.")}

  type = primeData$type[ primeData$filename == filename ]
  language = primeData$language[ primeData$filename == filename ]

  dir.create(paste0(type), showWarnings = F)
  dir.create(paste0(type, "/", language), showWarnings = F)

  download.file(
    con,
    destfile = paste0(type, "/", language, "/", filename),
    mode = "wb")
  imported <- rio::import(
    paste0(type, "/", language, "/", filename)
  )

  return(imported)

}
