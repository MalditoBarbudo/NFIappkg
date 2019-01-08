#' @title mod_tableOutput and mod_table
#'
#' @description A shiny module to generate the base IFN plots table
#'
#' @param id shiny id
#'
#' @export
mod_tableOutput <- function(id) {

  ns <- shiny::NS(id)

  # ui
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shinyWidgets::pickerInput(
          ns('col_vis_selector'),
          # label_getter(nfidb, 'esp', 'col_vis_selector_label'),
          label = 'Choose the variables to show',
          choices = '', multiple = TRUE,
          width = '90%',
          options = list(
            `actions-box` = FALSE,
            `deselect-all-text` = 'None selected...',
            `select-all-text` = 'All selected',
            `selected-text-format` = 'count',
            `count-selected-text` = "{0} variables selected (of {1})",
            `size` = 15,
            `max-options` = 50,
            `max-options-text` = 'Select limit reached (50)',
            `live-search` = TRUE
          )
        )
      ),
      shiny::column(
        2,
        shinyWidgets::actionBttn(
          ns('show_hri'),
          'Info',
          style = 'unite'
        )
      ),
      shiny::column(
        2,
        shinyWidgets::downloadBttn(
          ns('dwl_sql_query'),
          'SQL query',
          color = 'default', size = 'sm', block = FALSE,
          style = 'minimal'
        )
      ),
      shiny::column(
        2,
        offset = 3,
        shinyWidgets::downloadBttn(
          ns('dwl_csv_button'),
          'csv',
          color = 'default', size = 'sm', block = FALSE,
          style = 'minimal'
        ),
        shinyWidgets::downloadBttn(
          ns('dwl_xlsx_button'),
          'xlsx',
          color = 'default', size = 'sm', block = FALSE,
          style = 'minimal'
        )
      )
    ),
    shiny::fluidRow(
      shinyWidgets::addSpinner(DT::DTOutput(ns('nfi_table')))
    )
    # shiny::fluidPage(
    #   shiny::sidebarLayout(
    #     position = 'left',
    #     sidebarPanel = shiny::sidebarPanel(
    #       width = 3,
    #       # inputs
    #       # Column visibility
    #       shiny::h4('Column visibility'),
    #       shinyWidgets::pickerInput(
    #         ns('col_vis_selector'),
    #         # label_getter(nfidb, 'esp', 'col_vis_selector_label'),
    #         label = 'Choose the variables to show',
    #         choices = '', multiple = TRUE,
    #         width = '90%',
    #         options = list(
    #           `actions-box` = FALSE,
    #           `deselect-all-text` = 'None selected...',
    #           `select-all-text` = 'All selected',
    #           `selected-text-format` = 'count',
    #           `count-selected-text` = "{0} variables selected (of {1})",
    #           `size` = 15,
    #           `max-options` = 50,
    #           `max-options-text` = 'Select limit reached (50)',
    #           `live-search` = TRUE
    #         )
    #       ),
    #       # mod_applyButtonInput(ns('mod_applyButtonInput_table')),
    #
    #       # Save buttons
    #       shiny::h4('Save the table'),
    #       shiny::fluidRow(
    #         shinyWidgets::downloadBttn(
    #           ns('dwl_csv_button'),
    #           'Save as csv',
    #           color = 'primary', size = 'sm', block = FALSE,
    #           style = 'stretch'
    #         ),
    #         shinyWidgets::downloadBttn(
    #           ns('dwl_xlsx_button'),
    #           'Save as xlsx',
    #           color = 'primary', size = 'sm', block = FALSE,
    #           style = 'stretch'
    #         )
    #       )
    #     ),
    #     mainPanel = shiny::mainPanel(
    #       width = 9,
    #       # gt::gt_output(ns('nfi_table'))
    #       shinyWidgets::addSpinner(DT::DTOutput(ns('nfi_table')))
    #     )
    #   )
    # )
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
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  data_inputs, map_inputs, nfidb
) {

  # we need the data based on the viz shape selected (selected for plots,
  # summarised for polygons). All of this with a dedupe function, as it is
  # really expensive and we want to do it only when really necessary
  table_data <- dedupe(shiny::reactive({

    # browser()

    if (any(is.null(data_inputs$viz_shape), is.null(map_inputs$main_data))) {
      return()
    }

    if (data_inputs$viz_shape == 'plot') {
      if (is.null(map_inputs$main_data[['selected']])) {
        return()
      } else {
        # start the progress
        shinyWidgets::progressSweetAlert(
          session = session, id = 'table_build_progress',
          title = 'Preparing table data', value = 75,
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
          title = 'Preparing table data', value = 75,
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

  # update the column visibility input
  shiny::observeEvent(
    eventExpr = table_data(),
    handlerExpr = {
      col_vis_choices <- names(table_data())

      shinyWidgets::updatePickerInput(
        session = session, 'col_vis_selector',
        label = 'Choose the variables to show',
        selected = col_vis_choices[1:5],
        choices = col_vis_choices
      )
    }
  )

  # apply_table <- shiny::callModule(
  #   mod_applyButton, 'mod_applyButtonInput_table'
  # )

  # reactive to build the DT
  build_table <- shiny::reactive({

    shiny::validate(
      shiny::need(length(input$col_vis_selector) > 0, 'No data to show')
    )

    numeric_vars <- table_data() %>%
      dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
      dplyr::select_if(is.numeric) %>%
      names()

    basic_table <- table_data() %>%
      dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
      dplyr::mutate_if(is.character, forcats::as_factor) %>%
      DT::datatable(
        rownames = FALSE,
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        options = list(
          pageLength = 15,
          dom = 'tip',
          autoWidth = FALSE
        )
      ) %>%
     DT::formatRound(
       columns = numeric_vars,
       digits = 2
     )
   # for (var in numeric_vars) {
   #   basic_table <- basic_table %>%
   #     DT::formatStyle(
   #       columns = var,
   #       background = DT::styleColorBar(table_data()[[var]],'#3fc380', 90),
   #       backgroundSize = '98% 88%',
   #       backgroundRepeat = 'no-repeat',
   #       backgroundPosition = 'center'
   #     )
   # }

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
      readr::write_csv(table_data(), file)
    }
  )

  output$dwl_xlsx_button <- shiny::downloadHandler(
    filename = function() {
      'NFI_data.xlsx'
    },
    content = function(file) {
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
        title = 'Data info',
        text = shiny::tags$div(hri_builder(data_inputs)),
        html = TRUE
      )
    }
  )

}