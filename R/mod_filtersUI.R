#' @title mod_filtersUI and mod_filters
#'
#' @description A shiny module to generate and process the filters
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi db
#'
#' @export
mod_filtersUI <- function(id, nfidb) {

  # ns
  ns <- shiny::NS(id)

  # ui
  shiny::tagList(
    shiny::fluidRow(
      shinyWidgets::pickerInput(
        ns('filter_vars'),
        'Select the variable/s to filter data by:',
        choices = '',
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = 'None selected...',
          `select-all-text` = 'All selected',
          `selected-text-format` = 'count',
          `count-selected-text` = "{0} variables selected (of {1})"
        )
      )
    ),
    shiny::uiOutput(ns('proper_filters'))
  )
}

#' mod_filters server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb pool object to access the nfi db
#' @param mod_data reactives from the mod_dataInput module, to know about which scenario
#'   we are
#'
#' @export
#'
#' @rdname mod_filtersUI
mod_filters <- function(
  input, output, session,
  nfidb, mod_data
) {

  #### Filter vars and update picker ####
  tables_to_look_at <- shiny::reactive({
    nfi <- mod_data$nfi
    functional_group <- mod_data$functional_group
    diameter_classes <- mod_data$diameter_classes

    if (isTRUE(diameter_classes)) {
      dc <- 'DIAMCLASS_'
    } else {
      dc <- ''
    }

    table_names <- c(
      glue::glue("{functional_group}_{nfi}_{dc}RESULTS"),
      'PLOTS',
      glue::glue("PLOTS_{nfi}_DYNAMIC_INFO")
    )
    return(table_names)
  })

  # we need to update the filter_vars with the variables based on the tables in the
  # scenario
  vars_to_filter_by <- shiny::reactive({
    table_names <- tables_to_look_at()
    dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
      dplyr::filter(var_table %in% table_names) %>%
      dplyr::pull(var_id)
  })

  # update the input picker with the options
  shiny::observeEvent(
    eventExpr = vars_to_filter_by(),
    handlerExpr = {
      shinyWidgets::updatePickerInput(
        session, 'filter_vars',
        choices = vars_to_filter_by(),
        label = 'Select the variable/s to filter data by:'
      )
    }
  )

  #### Proper filters UI ####
  output$proper_filters <- shiny::renderUI({

    ns <- session$ns

    # create the inputs for each varible selected
    filters_inputs <- shiny::reactive({

      lapply(
        input$filter_vars, function(var) {

          table_names <- tables_to_look_at()

          var_info <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
            dplyr::filter(var_id == var, var_table %in% table_names) %>%
            dplyr::select(var_id, var_type)

          if (var_info %>% dplyr::pull(var_type) %>% unique() == 'character') {
            var_values <- var_info %>%
              dplyr::left_join(dplyr::tbl(nfidb, 'VARIABLES_CATEGORICAL'), by = 'var_id') %>%
              dplyr::pull(var_values)

            shinyWidgets::pickerInput(
              ns(var), label = var,
              choices = var_values,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = 'None selected...',
                `select-all-text` = 'All selected',
                `selected-text-format` = 'count',
                `count-selected-text` = "{0} values selected (of {1})"
              )
            )
          } else {
            if (var_info %>% dplyr::pull(var_type) %>% unique() %in% c('numeric', 'integer')) {
              var_values <- var_info %>%
                dplyr::left_join(dplyr::tbl(nfidb, 'VARIABLES_NUMERICAL'), by = 'var_id') %>%
                dplyr::select(var_min, var_max) %>%
                dplyr::collect()

              shiny::sliderInput(
                ns(var), label = var,
                min = var_values[['var_min']],
                max = var_values[['var_max']],
                value = c(var_values[['var_min']], var_values[['var_max']]),
                width = '100%'
              )
            } else {

              if (var_info %>% dplyr::pull(var_type) %>% unique() == 'logical') {
                var_values <- var_info %>%
                  dplyr::left_join(dplyr::tbl(nfidb, 'VARIABLES_LOGICAL'), by = 'var_id') %>%
                  dplyr::collect()

                # TODO que hacemos con las lógicas???
              } else {
                var_values <- var_info %>%
                  dplyr::left_join(dplyr::tbl(nfidb, 'VARIABLES_DTTM'), by = 'var_id') %>%
                  dplyr::collect()

                # TODO que hacemos con las lógicas???
              }

            }
          }

        }
      )
    })

    # return the inputs as a tagList
    shiny::tagList(
      filters_inputs()
    )
  })
}