#' Get the Model Card for a Dataset
#'
#' Returns the full metadata card for a dataset from the Linguistic
#' Annotated Bibliography, including verified citation, detected language,
#' variables, and any supplementary notes.
#'
#' @param bibtexID The bibtex ID of the dataset (e.g. `"Birchenough2017"`).
#'
#' @return A named list with fields: `bibtex`, `parent_bibtex` (if applicable),
#'   `citation`, `dataset`, and `variables`. Returns `NULL` invisibly if no
#'   card exists for the given ID.
#'
#' @keywords datasets, metadata, linguistic norms
#'
#' @export
#'
#' @examples
#' # card <- get_model_card("Birchenough2017")
#' # card$citation
#' # card$variables$flags

get_model_card <- function(bibtexID) {

  card_path <- system.file(
    "extdata", "model_cards", paste0(bibtexID, ".yaml"),
    package = "semanticprimeR"
  )

  if (card_path == "") {
    message("No model card found for: ", bibtexID)
    return(invisible(NULL))
  }

  yaml::read_yaml(card_path)
}
