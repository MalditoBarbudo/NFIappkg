#' @title mod_glossaryUI and mod_glossary
#'
#' @description Shiny module to generate the glossary
#'
#' @param id shiny id
#'
#' @export
mod_glossaryUI <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # ui, with uiOutput to use lang
  shiny::tagList(
    shiny::modalDialog(
      shiny::uiOutput(ns('mod_glossary_container')),
      title = 'Variable glossary',
      easyClose = TRUE
    )

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
mod_glossary <- function(
  input, output, session,
  nfidb, var_thes, numerical_thes, texts_thes, lang, data_inputs
) {

  # complete thes, outside because we dont need it to be reactive
  complete_thes <- var_thes %>%
    dplyr::left_join(numerical_thes)

  output$mod_glossary_container <- shiny::renderUI({

    # browser()
    # ns & lang
    ns <- session$ns
    lang_declared <- lang()

    # choices for variable selector
    var_choices <- complete_thes %>%
      dplyr::filter(var_table %in% tables_to_look_at_helper(data_inputs)) %>%
      dplyr::pull(var_id) %>%
      unique() %>%
      var_names_input_builder(
        lang_declared, var_thes, texts_thes,
        tables_to_look_at_helper(data_inputs), numerical_thes
      )

    # proper UI
    shiny::tagList(
      # variable selector
      shinyWidgets::pickerInput(
        ns('variable_glossary'),
        # label_getter(nfidb, 'esp', 'col_vis_selector_label'),
        label = text_translate('col_vis_selector_input', lang_declared, texts_thes),
        choices = var_choices, multiple = FALSE,
        width = '90%',
        options = list(
          `actions-box` = FALSE,
          `live-search` = TRUE,
          `tick-icon` = 'glyphicon-tree-deciduous'
        )
      ),

      # output
      shiny::h4('Variable description'),
      shiny::textOutput(ns('glossary_description')),
      shiny::br(),
      shiny::h4('Variable units (if numeric)'),
      shiny::textOutput(ns('glossary_units'))
    )
  })



  variable_info <- shiny::reactive({
    var_selected <- input$variable_glossary
    complete_thes %>%
      dplyr::filter(
        var_table %in% tables_to_look_at_helper(data_inputs),
        var_id == var_selected
      )
  })

  output$glossary_description <- shiny::renderText({
    variable_info() %>%
      dplyr::pull(!! rlang::sym(glue::glue("var_description_{lang()}"))) %>%
      unique()
  })

  output$glossary_units <- shiny::renderText({
    variable_info() %>%
      dplyr::pull(var_units) %>%
      unique()
  })
}