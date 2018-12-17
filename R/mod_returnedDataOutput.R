#' @title mod_returnedDataOutput and mod_returnedData
#'
#' @description Shiny module to get the data as tbl_sql
#'
#' @param id
#'
#' @export
mod_returnedDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_returnedData server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_inputs reactives from dataInput module
#' @param custom_polygon custom_polygon sf object, if any
#' @param nfidb pool object to access the nfi db
#'
#' @export
#'
#' @rdname mod_returnedDataOuput
mod_returnedData <- function(
  input, output, session,
  data_inputs, custom_polygon = NULL, nfidb
) {

  # main data generator
  main_data <- shiny::eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = data_inputs$apply_data,
    valueExpr = {

      # browser()

      nfi <- data_inputs$nfi
      viz_shape <- data_inputs$viz_shape
      admin_div <- data_inputs$admin_div
      functional_group <- data_inputs$functional_group
      diameter_classes <- data_inputs$diameter_classes
      filter_vars <- data_inputs$filter_vars
      filter_expressions <- data_inputs$filter_expressions
      custom_polygon_fil_expr <- tidyNFI:::custom_polygon_filter_expr(custom_polygon, nfidb)

      selected_data <- tidyNFI::nfi_results_data(
        conn = nfidb,
        nfi = nfi,
        functional_group = functional_group,
        diameter_classes = diameter_classes,
        .collect = FALSE
      ) %>%
        tidyNFI::nfi_results_filter(
          variables = filter_vars,
          conn = nfidb,
          !!! filter_expressions,
          !!! custom_polygon_fil_expr,
          .collect = FALSE
        )

      if (viz_shape == 'polygon') {
        summarised_data <- selected_data %>%
          tidyNFI::nfi_results_summarise(
            polygon_group = admin_div,
            functional_group = functional_group,
            diameter_classes = diameter_classes,
            conn = nfidb,
            .collect = FALSE
          )
      } else {
        summarised_data <- selected_data
      }

      return(
        list(selected = selected_data, summarised = summarised_data)
      )
    }
  )

  returned_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    returned_data_reactives$main_data <- main_data()
  })

  return(returned_data_reactives)
}