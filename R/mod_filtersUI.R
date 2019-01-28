#' @title mod_filtersUI and mod_filters
#'
#' @description A shiny module to generate and process the filters
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi db
#'
#' @export
mod_filtersUI <- function(id, nfidb, lang, texts_thes) {

  # ns
  ns <- shiny::NS(id)

  # ui
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shinyWidgets::pickerInput(
          ns('fil_res_vars'),
          text_translate('fil_res_vars_input', lang, texts_thes),
          choices = '',
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
            `select-all-text` = text_translate('select-all-text', lang, texts_thes),
            `selected-text-format` = 'count > 3',
            `count-selected-text` = text_translate('count-selected-text-var', lang, texts_thes),
            `size` = 10
          )
        )
      ),
      shiny::column(
        4,
        shinyWidgets::pickerInput(
          ns('fil_clim_vars'),
          text_translate('fil_clim_vars_input', lang, texts_thes),
          choices = '',
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
            `select-all-text` = text_translate('select-all-text', lang, texts_thes),
            `selected-text-format` = 'count > 3',
            `count-selected-text` = text_translate('count-selected-text-var', lang, texts_thes),
            `size` = 10
          )
        )
      ),
      shiny::column(
        4,
        shinyWidgets::pickerInput(
          ns('fil_plot_vars'),
          text_translate('fil_plot_vars_input', lang, texts_thes),
          choices = '',
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
            `select-all-text` = text_translate('select-all-text', lang, texts_thes),
            `selected-text-format` = 'count > 3',
            `count-selected-text` = text_translate('count-selected-text-var', lang, texts_thes),
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
#' @param var_thes variables thesaurus df
#'
#' @importFrom dplyr between
#'
#' @export
#'
#' @rdname mod_filtersUI
mod_filters <- function(
  input, output, session,
  nfidb, data_inputs, var_thes, texts_thes, lang
) {

  #### Filter vars and update picker ####
  tables_to_look_at <- shiny::reactive({

    shiny::validate(
      shiny::need(data_inputs$nfi, 'No NFI version selected')
    )

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

    # activate if lang changes
    lang <- lang()

    table_names <- tables_to_look_at()
    vars_overall <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
      dplyr::filter(var_table %in% table_names) %>%
      dplyr::pull(var_id)

    climatic_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^clim_")
    ]

    plot_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^admin_|^feat_|^topo_")
    ]

    removed_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^old_|^coords_|^presence_|plot_id")
    ]

    res_vars <- vars_overall[
      !(vars_overall %in% c(climatic_vars, plot_vars, removed_vars))
    ]

    return(list(
      res_vars = res_vars,
      climatic_vars = climatic_vars,
      plot_vars = plot_vars
    ))

  })

  # update the input picker with the options
  shiny::observeEvent(
    eventExpr = vars_to_filter_by()$res_vars,
    handlerExpr = {
      shinyWidgets::updatePickerInput(
        session, 'fil_res_vars',
        choices = var_names_input_builder(vars_to_filter_by()$res_vars, lang(), var_thes, texts_thes) %>% sort(),
        label = text_translate('fil_res_vars_input', lang(), texts_thes)
      )
    }
  )
  shiny::observeEvent(
    eventExpr = vars_to_filter_by()$climatic_vars,
    handlerExpr = {
      shinyWidgets::updatePickerInput(
        session, 'fil_clim_vars',
        choices = var_names_input_builder(vars_to_filter_by()$climatic_vars, lang(), var_thes, texts_thes) %>% sort(),
        label = text_translate('fil_clim_vars_input', lang(), texts_thes)
      )
    }
  )
  shiny::observeEvent(
    eventExpr = vars_to_filter_by()$plot_vars,
    handlerExpr = {
      shinyWidgets::updatePickerInput(
        session, 'fil_plot_vars',
        choices = var_names_input_builder(vars_to_filter_by()$plot_vars, lang(), var_thes, texts_thes) %>% sort(),
        label = text_translate('fil_plot_vars_input', lang(), texts_thes)
      )
    }
  )

  filter_vars <- shiny::reactive({
    c(input$fil_res_vars, input$fil_clim_vars, input$fil_plot_vars)
  })

  #### Proper filters UI ####
  output$proper_filters <- shiny::renderUI({

    ns <- session$ns

    # create the inputs for each varible selected
    filters_inputs <- shiny::eventReactive(
      eventExpr = filter_vars(),
      valueExpr = {

        # browser()

        lapply(
          filter_vars(), function(var) {

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
                ns(var), label = names(var_names_input_builder(var, lang(), var_thes, texts_thes)),
                choices = var_values,
                selected = var_values[1], multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = text_translate('deselect-all-text', lang(), texts_thes),
                  `select-all-text` = text_translate('select-all-text', lang(), texts_thes),
                  `selected-text-format` = 'count',
                  `count-selected-text` = text_translate('count-selected-text-value', lang(), texts_thes),
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
                  ns(var), label = names(var_names_input_builder(var, lang(), var_thes, texts_thes)),
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
      }
    )

    # return the inputs as a tagList
    shiny::tagList(
      shiny::hr(),
      shiny::tags$strong(text_translate('filter_the_data', lang(), texts_thes)),
      shiny::br(), shiny::br(),
      filters_inputs()
    )
  })

  # reactive to activate the filter expressions generation
  on_the_fly_inputs <- shiny::reactive({
    lapply(
      filter_vars(), function(x) {
        input[[x]]
      }
    )
  })

  apply_reactives <- shiny::reactive({
    apply_reactives <- list()
    apply_reactives$apply_data <- data_inputs$apply_data
    apply_reactives$apply_viz <- data_inputs$apply_viz
  })

  ## Filter exprs generators ####
  data_filter_expressions <- shiny::eventReactive(
    eventExpr = on_the_fly_inputs(),
    valueExpr = {

      # check the case of empty filter vars
      if (is.null(filter_vars()) || filter_vars() == '') {
        return(rlang::quos())
      }

      lapply(
        filter_vars(), function(var) {

          if (is.null(input[[var]])) {
            return(rlang::quo(TRUE))
          }

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
    }
  )

  filter_reactives <- shiny::reactiveValues()
  shiny::observe({
    filter_reactives$filter_expressions <- data_filter_expressions()
    filter_reactives$filter_vars <- filter_vars()

    # inputs created on the fly
    filter_reactives$otf_filter_inputs <- on_the_fly_inputs() %>%
      magrittr::set_names(., filter_vars())
  })

  return(filter_reactives)
}