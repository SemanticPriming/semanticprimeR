#' Process labjs SQLite Files
#'
#' This function is a helper function to transform labjs
#' SQLite files into a long data file where each trial
#' is one row.
#'
#' @param database The filepath of the database you want to process.
#'
#' @return The experiment data in a long format.
#'
#' @import RSQLite dplyr DBI purrr jsonlite readr
#'
#' @keywords metadata, datasets, linguistic norms
#'
#' @export
#'
#' @examples
#' # df <- processData("filename")
#'

processData <- function(database) {

  require("DBI")
  require("RSQLite")
  require("dplyr")
  require("purrr")
  require("jsonlite")
  require("readr")

  con <- dbConnect(
    drv=RSQLite::SQLite(),
    dbname=database
  )

  # Extract main table
  if(con %>% DBI::dbExistsTable("labjs")){
    d <- dbGetQuery(
      conn=con,
      statement='SELECT * FROM labjs'
    )

    # Close connection
    dbDisconnect(
      conn=con
    )

    # Discard connection
    rm(con)

    d.meta <- map_dfr(d$metadata, fromJSON) %>%
      dplyr::rename(
        observation=id
      )

    d <- d %>%
      bind_cols(d.meta) %>%
      select(
        -metadata # Remove metadata column
      )

    # Remove temporary data frame
    rm(d.meta)

    count_unique <- function(x) {
      return(length(unique(x)))
    }

    information_preserved <- function(x, length) {
      return(
        count_unique(str_sub(x, end=i)) ==
          count_unique(x)
      )
    }

    # Figure out the length of the random ids needed
    # to preserve the information therein. (five characters
    # should usually be enougth, but better safe)
    for (i in 5:36) {
      if (
        information_preserved(d$session, i) &&
        information_preserved(d$observation, i)
      ) {
        break()
      }
    }

    d <- d %>%
      dplyr::mutate(
        session=str_sub(session, end=i),
        observation=str_sub(observation, end=i)
      )

    rm(i, count_unique, information_preserved)

    parseJSON <- function(input) {
      return(input %>%
               fromJSON(flatten=T) %>% {
                 # Coerce lists
                 if (class(.) == 'list') {
                   discard(., is.null) %>%
                     as_tibble()
                 } else {
                   .
                 } } %>%
               # Sanitize names
               janitor::clean_names() %>%
               # Use only strings for now, and re-encode types later
               mutate_all(as.character)
      )
    }

    d.full <- d %>%
      dplyr::filter(payload == 'full')

    if (nrow(d.full) > 0) {
      d.full %>%
        group_by(observation, id) %>%
        do(
          { map_dfr(.$data, parseJSON) } %>%
            bind_rows()
        ) %>%
        ungroup() %>%
        select(-id) -> d.full
    } else {
      # If there are no full datasets, start from an entirely empty df
      # in order to avoid introducing unwanted columns into the following
      # merge steps.
      d.full <- tibble()
    }

    d %>%
      dplyr::filter(payload %in% c('incremental', 'latest')) %>%
      group_by(observation, id) %>%
      do(
        { map_dfr(.$data, parseJSON) } %>%
          bind_rows()
      ) %>%
      ungroup() %>%
      select(-id) -> d.incremental

    if (nrow(d.full) > 0){

      d.output <- d.full %>%
        bind_rows(
          d.incremental %>% filter(!(observation %in% d.full$observation))
        ) %>%
        type_convert()

    } else {

      d.output <- d.incremental %>% type_convert()

    }

    d.output %>%
      group_by(observation) %>%
      fill(matches('code'), .direction='down') %>%
      fill(matches('code'), .direction='up') %>%
      ungroup() -> d.output

    return(d.output)
  } else {
    DF <- data.frame(url_lab = as.character())
    return(DF)
  }
}
