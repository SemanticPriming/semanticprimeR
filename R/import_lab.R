LAB_RELEASES_API <- paste0(
  "https://api.github.com/repos/SemanticPriming/",
  "semanticprimeR/releases"
)

# Legacy data release, kept as a fallback for when the "latest data-*
# release" lookup fails (e.g. offline, GitHub API rate limit) or no
# data-* tagged release exists yet.
LEGACY_DATA_RELEASE_TAG <- "v0.0.1"

# Resolve the tag of the most recent release whose name starts with
# "data-" (e.g. "data-v0.0.2"). Falls back to the legacy tag on any
# failure so import_lab() keeps working offline / without GitHub access.
resolve_data_release_tag <- function() {
  tryCatch({
    releases <- jsonlite::fromJSON(LAB_RELEASES_API)
    data_tags <- releases$tag_name[
      startsWith(releases$tag_name, "data-")
    ]
    if (length(data_tags) > 0) {
      # GitHub returns releases newest-first
      return(data_tags[1])
    }
    LEGACY_DATA_RELEASE_TAG
  }, error = function(e) LEGACY_DATA_RELEASE_TAG)
}

#' Import Datasets from the Linguistic Annotated Bibliography
#'
#' Imports datasets available from the
#' \href{https://github.com/orgs/SemanticPriming/}{Semantic Priming GitHub
#' Group}. Datasets contain psycholinguistic variables such as Age of
#' Acquisition, frequency, neighborhood density, familiarity, concreteness,
#' and more. All data belongs to the original authors.
#'
#' @param bibtexID The bibtex ID of the dataset to load (e.g.
#'   `"Birchenough2017"`). Leave `NULL` to browse or filter without
#'   downloading.
#' @param citation If `TRUE` and a `bibtexID` is provided, also returns the
#'   formatted citation string from the dataset's model card.
#' @param language Language name to filter by (e.g. `"english"`). Without a
#'   `bibtexID`, returns all datasets in that language. With a `bibtexID`,
#'   returns the detected language of that dataset.
#' @param variables Character vector of flag category names to filter by
#'   (e.g. `c("aoa", "freq")`). Without a `bibtexID`, returns datasets that
#'   contain all of those variable types. With a `bibtexID`, returns which of
#'   those flags are present in that dataset. Use `import_lab()` with no
#'   arguments to see all available flag names.
#' @param release Which data release to download from. Defaults to
#'   `"latest"`, which resolves the most recent GitHub release tagged
#'   `data-*` (falling back to the legacy `v0.0.1` release if none is found
#'   or the lookup fails, e.g. when offline). Pass an explicit tag (e.g.
#'   `"data-v0.0.2"`) to pin to a specific data snapshot for reproducibility.
#'
#' @return A named list with one or more of:
#' \describe{
#'   \item{metadata}{Full index of available datasets (no-argument call)}
#'   \item{loaded_data}{The downloaded dataset}
#'   \item{citation}{Formatted citation string}
#'   \item{language}{Detected language of the requested dataset}
#'   \item{variables}{Flag categories present in the requested dataset}
#'   \item{datasets}{Filtered index based on language / variable criteria}
#' }
#'
#' @importFrom utils read.csv download.file
#' @importFrom jsonlite fromJSON
#' @import rio
#'
#' @keywords metadata, datasets, linguistic norms
#'
#' @export
#'
#' @examples
#' # import_lab()
#' # import_lab(bibtexID = "Birchenough2017", citation = TRUE)
#' # import_lab(language = "english", variables = c("aoa", "freq"))

import_lab <- function(bibtexID = NULL,
                       citation  = NULL,
                       language  = NULL,
                       variables = NULL,
                       release   = "latest") {

  index_path <- system.file(
    "extdata", "dataset_index.csv",
    package = "semanticprimeR"
  )
  index <- utils::read.csv(index_path, stringsAsFactors = FALSE)

  flag_cols <- setdiff(
    colnames(index),
    c("bibtex_id", "parent_bibtex", "doi", "language")
  )

  result <- list()

  if (!is.null(bibtexID)) {

    # --- Resolve release tag and download the dataset ---
    tag <- if (identical(release, "latest")) {
      resolve_data_release_tag()
    } else {
      release
    }
    release_url <- paste0(
      "https://github.com/SemanticPriming/semanticprimeR/",
      "releases/download/", tag, "/"
    )

    dir.create("lab_data", showWarnings = FALSE)
    url  <- paste0(release_url, bibtexID, ".csv")
    dest <- file.path("lab_data", paste0(bibtexID, ".csv"))
    utils::download.file(url, destfile = dest, mode = "wb")
    result$loaded_data <- rio::import(dest, stringsAsFactors = FALSE)

    # --- Citation from model card ---
    if (isTRUE(citation)) {
      card <- get_model_card(bibtexID)
      if (!is.null(card) && !is.null(card$citation)) {
        cit <- card$citation
        result$citation <- paste0(
          cit$author, ". (", cit$year, "). ",
          cit$title, ". ", cit$journal, ", ",
          cit$volume, ", ", cit$pages,
          ". doi: ", cit$doi
        )
      }
    }

    # --- Language from index ---
    if (!is.null(language)) {
      row <- index[index$bibtex_id == bibtexID, ]
      result$language <- if (nrow(row) > 0) row$language else NA
    }

    # --- Which requested variable flags are present ---
    if (!is.null(variables)) {
      row      <- index[index$bibtex_id == bibtexID, ]
      avail    <- intersect(variables, flag_cols)
      if (nrow(row) > 0 && length(avail) > 0) {
        result$variables <- avail[as.logical(row[1, avail])]
      }
    }

  } else {

    # --- Browsing / filtering mode ---
    filtered <- index

    if (!is.null(language)) {
      filtered <- filtered[
        grepl(language, filtered$language, ignore.case = TRUE), ]
    }

    if (!is.null(variables)) {
      for (var in variables) {
        if (var %in% colnames(filtered)) {
          filtered <- filtered[filtered[[var]] == TRUE, ]
        }
      }
    }

    if (is.null(language) && is.null(variables)) {
      result$metadata <- index
    } else {
      result$datasets <- filtered
    }

  }

  return(result)
}
