#' @title mod_tableOutput and mod_table
#'
#' @description A shiny module to generate the base IFN plots table
#'
#' @param id shiny id
#' @param nfidb pool object to access the database
#' @param lang lang value
#'
#' @export
mod_tableOutput <- function(id) {

  ns <- shiny::NS(id)

  # ui
  shiny::tagList(
    shiny::div(
      id = 'tableTabHeader',
      shiny::fluidRow(
        shiny::column(
          3,
          # shiny::br(),
          shinyWidgets::pickerInput(
            ns('col_vis_selector'),
            label = '',
            choices = '', multiple = TRUE,
            width = '90%',
            options = list(
              `actions-box` = FALSE,
              # `deselect-all-text` = text_translate('deselect-all-text', lang, nfidb),
              # `select-all-text` = text_translate('select-all-text', lang, nfidb),
              `selected-text-format` = 'count',
              # `count-selected-text` = text_translate('count-selected-text-value', lang, nfidb),
              `size` = 15,
              `max-options` = 50,
              # `max-options-text` = text_translate('max-options-text', lang, nfidb),
              `live-search` = TRUE
            )
          )
        ),
        shiny::column(
          2, offset = 2, align = 'center',
          shiny::br(),
          # shiny::p('Data info'),
          shinyWidgets::actionBttn(
            ns('show_hri'),
            style = 'material-circle',
            icon = shiny::icon('info-circle'),
            color = 'primary', size = 'sm'
          )
        ),
        shiny::column(
          2, offset = 3,
          # shiny::br(),
          shinyWidgets::downloadBttn(
            ns('dwl_csv_button'),
            'csv',
            style = 'material-flat', color = 'success', size = 'sm', block = TRUE
          ),
          shinyWidgets::downloadBttn(
            ns('dwl_xlsx_button'),
            'xlsx',
            style = 'material-flat', color = 'success', size = 'sm', block = TRUE
          ),
          shinyWidgets::downloadBttn(
            ns('dwl_sql_query'),
            'SQL query',
            style = 'material-flat', color = 'success', size = 'sm', block = TRUE
          )
        )
      )
    ),
    shiny::fluidRow(
      shinyWidgets::addSpinner(DT::DTOutput(ns('nfi_table')))
    )
  )
}

#' mod_table server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_inputs reactive with the reactive data and the data inputs
#' @param map_inputs reactive with the mod_map inputs, included main_data
#' @param nfidb db pool
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  data_inputs, map_inputs, nfidb, lang
) {

  # we need the data based on the viz shape selected (selected for plots,
  # summarised for polygons). All of this with a dedupe function, as it is
  # really expensive and we want to do it only when really necessary
  table_data <- dedupe(shiny::reactive({

    viz_shape <- shiny::isolate({data_inputs$viz_shape})

    if (any(is.null(viz_shape), is.null(map_inputs$main_data))) {
      return()
    }

    if (viz_shape == 'plot') {
      if (is.null(map_inputs$main_data[['selected']])) {
        return()
      } else {
        # start the progress
        shinyWidgets::progressSweetAlert(
          session = session, id = 'table_build_progress',
          title = text_translate('table_build_progress', lang(), nfidb), value = 75,
          display_pct = TRUE
        )
        res <- map_inputs$main_data[['selected']] %>%
          dplyr::collect()
        shinyWidgets::closeSweetAlert(session = session)
      }
    } else {
      if (is.null(map_inputs$main_data[['summarised']])) {
        return()
      } else {
        # start the progress
        shinyWidgets::progressSweetAlert(
          session = session, id = 'table_build_progress',
          title = text_translate('table_build_progress', lang(), nfidb), value = 75,
          display_pct = TRUE
        )
        res <- map_inputs$main_data[['summarised']] %>%
          dplyr::ungroup() %>%
          dplyr::collect()
        shinyWidgets::closeSweetAlert(session = session)
      }
    }
    return(res)
  }))

  # update the col vis selector input
  shiny::observe({
    col_vis_choices <- names(table_data())
    lang <- lang()

    shiny::validate(
      shiny::need(data_inputs$viz_shape, 'no data'),
      shiny::need(col_vis_choices, 'no data')
    )

    if (data_inputs$viz_shape == 'polygon') {
      summ <- TRUE
    } else {
      summ <- FALSE
    }

    shinyWidgets::updatePickerInput(
      session = session, 'col_vis_selector',
      label = text_translate('col_vis_selector_input', lang, nfidb),
      choices = var_names_input_builder(col_vis_choices, lang, nfidb, summ) %>% sort(),
      selected = col_vis_choices[1:7]
    )
  })

  # reactive to build the DT
  build_table <- shiny::reactive({

    shiny::validate(
      shiny::need(length(input$col_vis_selector) > 0, 'No data to show'),
      shiny::need(data_inputs$viz_shape, 'no data')
    )

    if (data_inputs$viz_shape == 'polygon') {
      summ <- TRUE
    } else {
      summ <- FALSE
    }

    numeric_vars <- table_data() %>%
      dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
      dplyr::select_if(is.numeric) %>%
      names()

    basic_table <- table_data() %>%
      dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
      # dplyr::mutate_if(is.character, forcats::as_factor) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = names(var_names_input_builder(names(.), lang(), nfidb, summ)),
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        options = list(
          pageLength = 15,
          dom = 'tip',
          autoWidth = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
            "$(this.api().table().body()).css({'font-family': 'Hack'});",
            "}"
          )
        )
      ) %>%
      DT::formatRound(
        columns = numeric_vars,
        digits = 2
      )
   return(basic_table)
  })

  # table per se.
  # output$nfi_table <- gt::render_gt({
  output$nfi_table <- DT::renderDT({
    build_table()
  })

  # download handlers
  output$dwl_csv_button <- shiny::downloadHandler(
    filename = function() {
      'NFI_data.csv'
    },
    content = function(file) {
      if (isTRUE(data_inputs$diameter_classes)) {
        shinyWidgets::sendSweetAlert(
          session, text_translate('sweet_alert_table_dc_title', lang(), nfidb),
          text = text_translate('sweet_alert_table_dc_text', lang(), nfidb)
        )
      }
      readr::write_csv(table_data(), file)
    }
  )

  output$dwl_xlsx_button <- shiny::downloadHandler(
    filename = function() {
      'NFI_data.xlsx'
    },
    content = function(file) {
      if (isTRUE(data_inputs$diameter_classes)) {
        shinyWidgets::sendSweetAlert(
          session, text_translate('sweet_alert_table_dc_title', lang(), nfidb),
          text = text_translate('sweet_alert_table_dc_text', lang(), nfidb)
        )
      }
      writexl::write_xlsx(table_data(), file)
    }
  )

  output$dwl_sql_query <- shiny::downloadHandler(
    filename = function() {
      'NFI_data_query.sql'
    },
    content = function(file) {
      if (data_inputs$viz_shape == 'plot') {
        query <- dbplyr::sql_render(map_inputs$main_data$selected)
      } else {
        query <- dbplyr::sql_render(map_inputs$main_data$summarised)
      }
      writeLines(query, con = file)
    }
  )

  shiny::observeEvent(
    eventExpr = input$show_hri,
    handlerExpr = {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = text_translate('sweet_alert_hri_title', lang(), nfidb),
        text = shiny::tags$div(hri_builder(data_inputs)),
        html = TRUE
      )
    }
  )
}