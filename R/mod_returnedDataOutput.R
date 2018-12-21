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
#' @param map_inputs map input with custom_polygon sf object, if any
#' @param nfidb pool object to access the nfi db
#'
#' @export
#'
#' @rdname mod_returnedDataOuput
mod_returnedData <- function(
  input, output, session,
  data_inputs, map_inputs = NULL, nfidb
) {

  apply_reactives <- shiny::reactive({
    apply_reactives <- list()
    apply_reactives$apply_data <- data_inputs$apply_data
    apply_reactives$apply_viz <- data_inputs$apply_viz
  })

  # main data generator
  main_data <- shiny::eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = apply_reactives(),
    valueExpr = {

      # browser()

      nfi <- data_inputs$nfi
      viz_shape <- data_inputs$viz_shape
      admin_div <- data_inputs$admin_div
      functional_group <- data_inputs$functional_group
      diameter_classes <- data_inputs$diameter_classes
      filter_vars <- data_inputs$filter_vars
      filter_expressions <- data_inputs$filter_expressions
      # custom_polygon_fil_expr needs some extra checking:
      if (is.null(map_inputs$custom_polygon)) {
        custom_polygon_fil_expr <- rlang::quos()
      } else {
        # check if plot_id is already in the filter_vars
        if ('plot_id' %in% filter_vars) {
          # then we need to replace the filter expression adding the one created
          # by the custom_polygon_filter_expr function
          orig_expr <- rlang::quo_text(
            filter_expressions[[which(filter_vars == 'plot_id')]]
          )
          expr_to_add <- rlang::quo_text(
            tidyNFI:::custom_polygon_filter_expr(
              map_inputs$custom_polygon, nfidb
            )
          )
          filter_expressions[[which(filter_vars == 'plot_id')]] <- rlang::quo_set_expr(
            filter_expressions[[which(filter_vars == 'plot_id')]],
            rlang::expr(!!rlang::parse_expr(glue::glue(
              "{orig_expr} || {expr_to_add}"
            )))
          )
          custom_polygon_fil_expr <- rlang::quos()
        } else {
          filter_vars <- c('plot_id', filter_vars)
          custom_polygon_fil_expr <- tidyNFI:::custom_polygon_filter_expr(
            map_inputs$custom_polygon, nfidb
          )
        }
      }

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
          !!! custom_polygon_fil_expr,
          !!! filter_expressions,
          .collect = FALSE
        ) %>%
        dplyr::left_join(dplyr::tbl(nfidb, 'PLOTS'), by = 'plot_id')

      if (nfi %in% c('nfi_2', 'nfi_3', 'nfi_4')) {
        selected_data <- selected_data %>%
          dplyr::left_join(
            dplyr::tbl(nfidb, glue::glue("PLOTS_{toupper(nfi)}_DYNAMIC_INFO")),
            by = 'plot_id'
          )
      }


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