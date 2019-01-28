#' mod_vizInput and mod_viz
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#' @param nfidb pool object to access nfi db
#' @param lang lang value
#' @param text_thes textsx thesaurus df
#'
#' @export
mod_vizInput <- function(id, nfidb, lang, texts_thes) {

  # ns
  ns <- shiny::NS(id)

  ## precalculated choices
  statistic_choices <- c('_mean', '_se', '_min', '_max', '_n') %>%
    magrittr::set_names(c(
      text_translate('mean_stat', lang, texts_thes),
      text_translate('se_stat', lang, texts_thes),
      text_translate('min_stat', lang, texts_thes),
      text_translate('max_stat', lang, texts_thes),
      text_translate('n_stat', lang, texts_thes)
    ))

  # diameter_classes_choices <- seq(10, 70, 5) %>% as.character()

  # UI
  shiny::tagList(
    # color & palette settings
    shiny::fluidRow(
      shiny::column(
        8,
        shinyWidgets::pickerInput(
          ns('viz_color'),
          text_translate('viz_color_input', lang, texts_thes),
          choices = 'density',
          options = list(
            `size` = 10
          )
        ),

        # size
        shinyjs::hidden(
          shinyWidgets::pickerInput(
            ns('viz_size'), text_translate('viz_size_input', lang, texts_thes),
            choices = '',
            options = list(
              `size` = 10
            )
          )
        ),

        # statistic
        # shinyjs::hidden(
        #   shinyWidgets::pickerInput(
        #     ns('viz_statistic'), text_translate('viz_statistic_input', lang, texts_thes),
        #     choices = statistic_choices
        #   )
        # ),
        shinyWidgets::pickerInput(
          ns('viz_statistic'), text_translate('viz_statistic_input', lang, texts_thes),
          choices = statistic_choices
        ),

        # functional group value
        shinyjs::hidden(
          shinyWidgets::pickerInput(
            ns('viz_functional_group_value'), '',
            choices = '',
            options = list(
              `size` = 10
            )
          )
        ),

        # diameter class to visualize
        shinyjs::hidden(
          shinyWidgets::pickerInput(
            ns('viz_diamclass'), text_translate('viz_diamclass_input', lang, texts_thes),
            choices = '10'
          )
        )
      ),
      shiny::column(
        4,
        # low, normal or high palette
        shinyWidgets::radioGroupButtons(
          ns('viz_pal_config'),
          text_translate('viz_pal_config_input', lang, texts_thes),, size = 'sm',
          choices = c('low', 'normal', 'high') %>%
            magrittr::set_names(c(
              text_translate('pal_low', lang, texts_thes),
              text_translate('pal_normal', lang, texts_thes),
              text_translate('pal_high', lang, texts_thes)
            )),
          selected = 'normal', direction = 'vertical', status = 'warning'
        ),
        # reverse palette
        shinyWidgets::awesomeCheckbox(
          ns('viz_reverse_pal'),
          label = text_translate('viz_reverse_pal_input', lang, texts_thes),
          value = FALSE, status = 'info'
        )
      )
    ),

    mod_returnedDataOutput('ret_data_viz')
  )
}

#' mod_viz server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_inputs reactive with the reactive data and the data inputs
#' @param nfidb pool object to access nfi db
#' @param var_thes variables thesaurus df
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  data_inputs, nfidb, var_thes, texts_thes, lang
) {

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

  # we need the vars in the data to be able to show the names in the color and size inputs
  vars_to_viz_by <- shiny::reactive({

    table_names <- tables_to_look_at()

    all_variables <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
      dplyr::filter(var_table %in% table_names) %>%
      dplyr::pull(var_id)

    numeric_variables <- dplyr::tbl(nfidb, 'VARIABLES_NUMERICAL') %>%
      dplyr::filter(var_id %in% all_variables, var_table %in% table_names) %>%
      dplyr::pull(var_id)

    ## TODO when the theasurus is completed here we build the real list of variables

    if (data_inputs$viz_shape == 'plot') {
      return(all_variables)
    } else {
      return(numeric_variables)
    }
  })

  # color input updater
  shiny::observe({
    color_choices <- vars_to_viz_by()

    shiny::validate(
      shiny::need(color_choices, 'No variables to visualize')
    )

    # let's make density (or density_balance) the selected var
    if ('density' %in% color_choices) {
      selected_col <- 'density'
    } else {
      selected_col <- 'density_balance'
    }

    # browser()

    # update the pickerInput
    shinyWidgets::updatePickerInput(
      session, 'viz_color',
      choices = var_names_input_builder(color_choices, lang(), var_thes, texts_thes) %>% sort(),
      label = text_translate('viz_color_input', lang(), texts_thes),
      selected = selected_col
    )
  })

  # size input updater
  shiny::observe({

    shiny::validate(
      shiny::need(data_inputs$viz_shape, 'No visualization shape selected')
    )

    if (data_inputs$viz_shape == 'plot') {
      size_choices <- vars_to_viz_by()

      # update the pickerInput
      shinyWidgets::updatePickerInput(
        session, 'viz_size',
        choices = c('', var_names_input_builder(size_choices, lang(), var_thes, texts_thes) %>% sort()),
        label = text_translate('viz_size_input', lang(), texts_thes)
      )

      # show and enable
      shinyjs::show('viz_size')
    } else {
      shinyjs::hide('viz_size')
    }
  })

  # statistic input updater
  shiny::observe({

    shiny::validate(
      shiny::need(data_inputs$viz_shape, 'No visualization shape selected')
    )

    if (data_inputs$viz_shape != 'plot') {
      shinyjs::show('viz_statistic')
    } else {
      shinyjs::hide('viz_statistic')
    }
  })

  # functional group value updater
  shiny::observe({

    shiny::validate(
      shiny::need(data_inputs$functional_group, 'No breakdown selected')
    )

    if (data_inputs$functional_group != 'plot') {

      functional_group <- data_inputs$functional_group
      funct_group_var <- glue::glue('{functional_group}_id')
      table_names <- tables_to_look_at()
      fg_filter_vals <- data_inputs$otf_filter_inputs[[funct_group_var]]

      viz_functional_group_value_choices <- dplyr::tbl(nfidb, 'VARIABLES_CATEGORICAL') %>%
        dplyr::filter(var_id == funct_group_var, var_table %in% table_names) %>%
        dplyr::pull(var_values)

      if (!is.null(fg_filter_vals)) {
        viz_functional_group_value_choices <- viz_functional_group_value_choices[
          viz_functional_group_value_choices %in% fg_filter_vals
        ]
      }

      shinyWidgets::updatePickerInput(
        session, 'viz_functional_group_value',
        choices = viz_functional_group_value_choices,
        label = glue::glue(text_translate('viz_functional_group_value_input', lang(), texts_thes))
      )
      shinyjs::show('viz_functional_group_value')
    } else {
      shinyjs::hide('viz_functional_group_value')
    }
  })

  # diameter_classes
  shiny::observe({

    shiny::validate(
      shiny::need(data_inputs$diameter_classes, 'Diameter classes radio-button missing')
    )

    if (isTRUE(data_inputs$diameter_classes)) {

      diameter_classes_choices <- seq(10, 70, 5) %>% as.character()
      dc_filter_vals <- data_inputs$otf_filter_inputs[['diamclass_id']]

      if (!is.null(dc_filter_vals)) {
        diameter_classes_choices <- diameter_classes_choices[diameter_classes_choices %in% dc_filter_vals]
      }

      shinyWidgets::updatePickerInput(
        session, 'viz_diamclass',
        choices = diameter_classes_choices,
        label = glue::glue(text_translate('viz_diamclass_input', lang(), texts_thes))
      )
      shinyjs::show('viz_diamclass')
    } else {
      shinyjs::hide('viz_diamclass')
    }
  })

  # return the viz inputs
  viz_reactives <- shiny::reactiveValues()

  shiny::observe({
    viz_reactives$viz_color <- input$viz_color
    viz_reactives$viz_reverse_pal <- input$viz_reverse_pal
    viz_reactives$viz_size <- input$viz_size
    viz_reactives$viz_statistic <- input$viz_statistic
    viz_reactives$viz_functional_group_value <- input$viz_functional_group_value
    viz_reactives$viz_diamclass <- input$viz_diamclass
    viz_reactives$viz_pal_config <- input$viz_pal_config
  })

  return(viz_reactives)

}