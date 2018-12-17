#' mod_vizInput and mod_viz
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#' @param nfidb pool object to access nfi db
#'
#' @export
mod_vizInput <- function(id, nfidb) {

  # ns
  ns <- shiny::NS(id)

  ## precalculated choices
  statistic_choices <- c(
    'Mean' = '_mean',
    'SD' = '_sd',
    'Min' = '_min',
    'Max' = '_max'
  )

  diameter_classes_choices <- seq(10, 70, 5) %>% as.character()

  # UI
  shiny::tagList(
    shiny::h3('Visualization controls'),

    # color
    shinyWidgets::pickerInput(
      ns('viz_color'),
      'Color:',
      choices = ''
    ),

    # reverse palette
    shinyWidgets::awesomeCheckbox(
      ns('viz_reverse_pal'),
      label = 'Reverse the palette?',
      value = FALSE, status = 'info'
    ),

    # size
    shinyjs::hidden(
      shinyWidgets::pickerInput(
        ns('viz_size'), 'Size:',
        choices = ''
      )
    ),

    # statistic
    shinyjs::hidden(
      shinyWidgets::pickerInput(
        ns('viz_statistic'), 'Statistic:',
        choices = statistic_choices
      )
    ),

    # functional group value
    shinyjs::hidden(
      shinyWidgets::pickerInput(
        ns('viz_functional_group_value'), '',
        choices = ''
      )
    ),

    # diameter class to visualize
    shinyjs::hidden(
      shinyWidgets::pickerInput(
        ns('viz_diamclass'), 'Diameter class to visualize:',
        choices = diameter_classes_choices
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
#'
#' @export
#'
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  data_inputs, nfidb
) {

  returned_data_for_viz <-  shiny::callModule(
    mod_returnedData, 'ret_data_viz',
    data_inputs, NULL, nfidb
  )

  # we need the vars in the data to be able to show the names in the color and size inputs
  vars_to_viz_by <- shiny::eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = data_inputs$apply_data,
    valueExpr = {
      if (data_inputs$viz_shape == 'plot') {
        data_vars <- returned_data_for_viz$main_data[['selected']] %>%
          head(1) %>% dplyr::collect() %>% names()
      } else {
        data_vars <- returned_data_for_viz$main_data[['summarised']] %>%
          head(1) %>% dplyr::collect() %>% names() %>%
          stringr::str_remove('_[a-z]+$') %>% unique()
      }

      ## TODO when the theasurus is completed here we build the real list of variables

      return(data_vars)
    }
  )

  # color input updater
  shiny::observeEvent(
    eventExpr = data_inputs$apply_data,
    handlerExpr = {
      color_choices <- vars_to_viz_by()

      # update the pickerInput
      shinyWidgets::updatePickerInput(
        session, 'viz_color',
        choices = color_choices,
        label = 'Color:'
      )
    }
  )

  # size input updater
  shiny::observeEvent(
    eventExpr = data_inputs$apply_data,
    handlerExpr = {

      if (data_inputs$viz_shape == 'plot') {
        size_choices <- vars_to_viz_by()

        # update the pickerInput
        shinyWidgets::updatePickerInput(
          session, 'viz_size',
          choices = size_choices,
          label = 'Size:'
        )

        # show and enable
        shinyjs::show('viz_size')
      } else {
        shinyjs::hide('viz_size')
      }
    }
  )

  shiny::observeEvent(
    eventExpr = data_inputs$apply_data,
    handlerExpr = {

      if (data_inputs$viz_shape != 'plot') {
        shinyjs::show('viz_statistic')
      } else {
        shinyjs::hide('viz_statistic')
      }

    }
  )

  # functional group value updater
  shiny::observeEvent(
    eventExpr = data_inputs$apply_data,
    handlerExpr = {
      if (data_inputs$functional_group != 'plot') {

        functional_group <- data_inputs$functional_group
        funct_group_var <- glue::glue('{functional_group}_id')

        viz_functional_group_value_choices <- returned_data_for_viz$main_data[['selected']] %>%
          dplyr::pull(!! rlang::sym(funct_group_var)) %>%
          unique()

        shinyWidgets::updatePickerInput(
          session, 'viz_functional_group_value',
          choices = viz_functional_group_value_choices,
          label = glue::glue("{functional_group} to visualize:")
        )

        shinyjs::show('viz_functional_group_value')
      } else {
        shinyjs::hide('viz_functional_group_value')
      }
    }
  )

  shiny::observeEvent(
    eventExpr = data_inputs$apply_data,
    handlerExpr = {
      if (isTRUE(data_inputs$diameter_classes)) {
        shinyjs::show('viz_diamclass')
      } else {
        shinyjs::hide('viz_diamclass')
      }
    }
  )

  # return the viz inputs
  viz_reactives <- shiny::reactiveValues()

  shiny::observe({
    viz_reactives$color <- input$color
    viz_reactives$reverse_pal <- input$reverse_pal
    viz_reactives$size <- input$size
    viz_reactives$statistic <- input$statistic
    viz_reactives$viz_functional_group_value <- input$viz_functional_group_value
    viz_reactives$viz_diamclass <- input$viz_diamclass
  })

}