#' Import Datasets from the Linguistic Annotated Bibliography
#'
#' This function allows you to import the current datasets available from
#' \href{https://github.com/orgs/SemanticPriming/}{Semantic Priming GitHub
#' Group}. These datasets generally have psycholinguistic variables like
#' Age of Acquisition, length, neighborhoods, Familiarity, Concretenes,
#' and more. We will update datasets as we have them. All data belongs
#' to the person who published them.
#'
#' @param bibtexID The bibtex ID of the dataset you are trying to load.
#' You can leave all parameters blank to load just the metadata.
#' @param citation Include the citation for the dataset you loaded - will only
#' load if you include a bibtex ID, use `TRUE` to get the citation.
#' @param language If you include a bibtex ID, you will get back the language of
#' the dataset, if you do not include a bibtex ID, it will return a list of
#' datasets in that language.
#' @param variables If you include a bibtex ID, you will get back the variables
#' included the dataset, if you do not include a bibtex ID, it will return a list of
#' datasets that include that variable (can also be paired with language).
#' Use the column names from the metadata as your filter.
#' @return
#' \describe{
#'  \item{metadata}{The metadata list of avaliable datasets}
#'  \item{loaded_data}{The dataset you requested to load}
#'  \item{language}{The language of the dataset you requested to load}
#'  \item{variables}{The variables of the dataset you requested to load}
#'  \item{datasets}{Possible datasets based on your language
#'  and variable names}
#' }
#'
#' @import rio utils
#'
#' @keywords metadata, datasets, linguistic norms
#'
#' @export
#'
#' @examples
#'
#' # import_lab()
#' # import_lab(bibtexID = "Birchenough2017", citation = TRUE)
#' # import_lab(language = "English", variables = c("aoa", "freq"))


import_lab <- function( bibtexID = NULL,
                        citation = NULL,
                        language = NULL,
                        variables = NULL
                        ) {

  labData
  labData <- subset(labData, included == "yes")
  labData$link <- paste0("https://github.com/SemanticPriming/semanticprimeR/releases/download/v0.0.1/",
                         labData$bibtex, ".csv")
  metadata <- labData
  variable_return <- list()

  if (!is.null(bibtexID)) {

    dir.create(paste("lab_data"), showWarnings = F)
    con <- metadata$link[metadata$bibtex == bibtexID]
    download.file(
      con,
      destfile = paste0('lab_data/', bibtexID, ".csv"),
      mode = "wb")

    variable_return$loaded_data <- rio::import(paste0('lab_data/', bibtexID, ".csv"),
                                               stringsAsFactors = F)

    if (!is.null(citation)){

      variable_return$citation <- paste0(metadata$author[metadata$bibtex == bibtexID], ". (",
                        metadata$year[metadata$bibtex == bibtexID], "). ",
                        metadata$ref_title[metadata$bibtex == bibtexID], ". ",
                        metadata$ref_journal[metadata$bibtex == bibtexID], ", ",
                        metadata$ref_volume[metadata$bibtex == bibtexID], ", ",
                        metadata$ref_page[metadata$bibtex == bibtexID], ". doi: ",
                        metadata$ref_doi[metadata$bibtex == bibtexID]
      )
    }

    if (!is.null(language)){
      variable_return$language <- metadata$language[metadata$bibtex == bibtexID]
    }

    if (!is.null(variables)){
      temp <- metadata[metadata$bibtex == bibtexID, 25:ncol(metadata)]
      variable_return$variables <- colnames(temp)[temp == 1]
    }

  } else {

    if (!is.null(language) & !is.null(variables)) { #both

      temp <- metadata[ tolower(metadata$language) == tolower(language) , ]

      for (var in variables){
        if (var %in% colnames(metadata)){
          temp <- temp[ temp[ , var] == 1 , ]
          }
        }

      variable_return$datasets <- temp

    } else if (!is.null(language)){ #just language

      variable_return$datasets <- metadata[ tolower(metadata$language) == tolower(language) , ]

      } else if (!is.null(variables)){ #just variables

      temp <- metadata

      for (var in variables){
        if (var %in% colnames(metadata)){
          temp <- temp[ temp[ , var] == 1 , ]
        }
      }

      variable_return$datasets <- temp

    }

  }

  return(variable_return)
}
