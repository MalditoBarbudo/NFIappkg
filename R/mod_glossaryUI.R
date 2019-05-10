#' @title mod_glossaryUI and mod_glossary
#'
#' @description Shiny module to generate the glossary
#'
#' @param id shiny id
#' @param nfidb pool object to access the database
#'
#' @export
mod_glossaryUI <- function(id, nfidb) {

  # ns
  ns <- shiny::NS(id)

  # ui, with uiOutput to use lang
  shiny::tagList(
    shiny::uiOutput(ns('mod_glossary_container'))
  )
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb pool with database connection object
#' @param var_thes variables thesaurus df
#' @param numerical_thes numerical variables df
#' @param texts_thes app translations df
#' @param lang reactive with the decalred lang
#'
#' @export
#'
#' @rdname mod_mapUI
mod_map <- function(
  input, output, session,
  nfidb, var_thes, numerical_thes, texts_thes, lang, data_inputs
) {

  # complete thes, outside because we dont need it to be reactive
  complete_thes <- var_thes %>%
    dplyr::left_join(numerical_thes)

  output$glossaryUI <- shiny::renderUI({

    # ns & lang
    ns <- session$ns
    lang_declared <- lang()

    # choices for variable selector
    var_choices <- complete_thes %>%
      dplyr::pull(var_id) %>%
      var_names_input_builder(
        lang(), var_thes, texts_thes,
        tables_to_look_at_helper(data_inputs), numerical_thes
      )

    # proper UI
    shiny::tagList(
      # variable selector
    )


  })

}