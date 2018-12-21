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
      shiny::column(
        9,
        shinyWidgets::pickerInput(
          ns('filter_vars'),
          'Select the variable/s to filter data by:',
          choices = '',
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = 'None selected...',
            `select-all-text` = 'All selected',
            `selected-text-format` = 'count > 3',
            `count-selected-text` = "{0} variables selected (of {1})",
            `size` = 10
          )
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
#' @param data_inputs reactives from the data_inputsInput module, to know about which scenario
#'   we are
#'
#' @importFrom dplyr between
#'
#' @export
#'
#' @rdname mod_filtersUI
mod_filters <- function(
  input, output, session,
  nfidb, data_inputs
) {

  #### Filter vars and update picker ####
  tables_to_look_at <- shiny::reactive({

    nfi <- data_inputs$nfi

    if (nfi == 'nfi_2_nfi_3') {
      nfi <- 'COMP_NFI2_NFI3'
    } else {
      if (nfi == 'nfi_3_nfi_4') {
        nfi <- 'COMP_NFI3_NFI4'
      } else {
        nfi <- toupper(nfi)
      }
    }

    functional_group <- data_inputs$functional_group %>% toupper()
    diameter_classes <- data_inputs$diameter_classes

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

      browser()

      lapply(
        input$filter_vars, function(var) {

          table_names <- tables_to_look_at()

          var_info <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
            dplyr::filter(var_id == var, var_table %in% table_names) %>%
            dplyr::select(var_id, var_table, var_type)

          # check for special case, plot_id which is present in all the tables,
          # in that case, we choose the results table, that is the one more
          # restrictive (less options)
          if (length(dplyr::pull(var_info, var_table)) > 1){
            var_info <- var_info %>%
              dplyr::filter(var_table == table_names[1])
          }

          if (var_info %>% dplyr::pull(var_type) %>% unique() == 'character') {
            var_values <- var_info %>%
              dplyr::left_join(
                dplyr::tbl(nfidb, 'VARIABLES_CATEGORICAL'), by = c('var_id', 'var_table')
              ) %>%
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
                `count-selected-text` = "{0} values selected (of {1})",
                `size` = 10
              )
            )
          } else {
            if (var_info %>% dplyr::pull(var_type) %>% unique() %in% c('numeric', 'integer')) {
              var_values <- var_info %>%
                dplyr::left_join(
                  dplyr::tbl(nfidb, 'VARIABLES_NUMERICAL'), by = c('var_id', 'var_table')
                ) %>%
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
                # var_values <- var_info %>%
                #   dplyr::left_join(dplyr::tbl(nfidb, 'VARIABLES_LOGICAL'), by = 'var_id') %>%
                #   dplyr::collect()
                # shinyWidgets::pickerInput(
                #   ns(var), label = var,
                #   choices = c('TRUE', 'FALSE'),
                #   multiple = FALSE,
                #   options = list(
                #     `actions-box` = TRUE,
                #     `deselect-all-text` = 'None selected...',
                #     `select-all-text` = 'All selected',
                #     `selected-text-format` = 'count',
                #     `count-selected-text` = "{0} values selected (of {1})",
                #     `size` = 10
                #   )
                # )

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

  ## Filter exprs generators ####
  data_filter_expressions <- shiny::reactive({

    # browser()

    # check the case of empty filter vars
    if (is.null(input$filter_vars) || input$filter_vars == '') {
      return(rlang::quos())
    }

    lapply(
      input$filter_vars, function(var) {
        table_names <- tables_to_look_at()

        var_info <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
          dplyr::filter(var_id == var, var_table %in% table_names) %>%
          dplyr::select(var_id, var_type)

        if (var_info %>% dplyr::pull(var_type) %>% unique() == 'character') {
          rlang::quo(
            !!rlang::sym(var) %in% !!input[[var]]
          )
        } else {
          if (var_info %>% dplyr::pull(var_type) %>% unique() %in% c('numeric', 'integer')) {
            rlang::quo(
              between(!!rlang::sym(var), !!input[[var]][1], !!input[[var]][2])
            )
          } else {

            if (var_info %>% dplyr::pull(var_type) %>% unique() == 'logical') {
              rlang::quo(

              )
              # TODO que hacemos con las lógicas???
            } else {
              rlang::quo(

              )
              # TODO que hacemos con las lógicas???
            }
          }
        }
      }
    )
  })

  filter_reactives <- shiny::reactiveValues()
  shiny::observe({
    # browser()
    filter_reactives$filter_expressions <- data_filter_expressions()
    filter_reactives$filter_vars <- input$filter_vars
  })

  return(filter_reactives)
}