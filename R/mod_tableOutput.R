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
    shiny::fluidPage(
      shiny::sidebarLayout(
        position = 'left',
        sidebarPanel = shiny::sidebarPanel(
          width = 3,
          # inputs
          # Column visibility
          shiny::h4('Column visibility'),
          shinyWidgets::pickerInput(
            ns('col_vis_selector'),
            # label_getter(nfidb, 'esp', 'col_vis_selector_label'),
            label = 'Choose the variables to show',
            choices = '', multiple = TRUE,
            width = '90%',
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = 'None selected...',
              `select-all-text` = 'All selected',
              `selected-text-format` = 'count',
              `count-selected-text` = "{0} variables selected (of {1})"
            )
          ),
          # mod_applyButtonInput(ns('mod_applyButtonInput_table')),

          # Save buttons
          shiny::h4('Save the table'),
          shiny::fluidRow(
            shinyWidgets::downloadBttn(
              ns('dwl_csv_button'),
              'Save as csv',
              color = 'primary', size = 'sm', block = FALSE,
              style = 'stretch'
            ),
            shinyWidgets::downloadBttn(
              ns('dwl_xlsx_button'),
              'Save as xlsx',
              color = 'primary', size = 'sm', block = FALSE,
              style = 'stretch'
            )
          )
        ),
        mainPanel = shiny::mainPanel(
          width = 9,
          # gt::gt_output(ns('nfi_table'))
          DT::DTOutput(ns('nfi_table'))
        )
      )
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

    if (any(is.null(data_inputs$viz_shape), is.null(map_inputs$main_data))) {
      return()
    }

    if (data_inputs$viz_shape == 'plot') {
      if (is.null(map_inputs$main_data[['selected']])) {
        return()
      } else {
        map_inputs$main_data[['selected']] %>%
          dplyr::collect()
      }
    } else {
      if (is.null(map_inputs$main_data[['summarised']])) {
        return()
      } else {
        map_inputs$main_data[['summarised']] %>%
          dplyr::ungroup() %>%
          dplyr::collect()
      }
    }
  }))

  # update the column visibility input
  shiny::observeEvent(
    eventExpr = table_data(),
    handlerExpr = {
      col_vis_choices <- names(table_data())

      shinyWidgets::updatePickerInput(
        session = session, 'col_vis_selector',
        label = 'Choose the variables to show',
        selected = col_vis_choices[1:10],
        choices = col_vis_choices
      )
    }
  )

  # apply_table <- shiny::callModule(
  #   mod_applyButton, 'mod_applyButtonInput_table'
  # )

  # reactive to build the DT
  build_table <- shiny::reactive({

    numeric_vars <- table_data() %>%
      dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
      dplyr::select_if(is.numeric) %>%
      names()

   basic_table <- table_data() %>%
      dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
      DT::datatable(
        rownames = FALSE,
        class = 'hover compact order-column stripe nowrap',
        filter = 'bottom',
        options = list(
          pageLength = 20,
          dom = 'tip'
        )
      ) %>%
     DT::formatRound(
       columns = numeric_vars,
       digits = 2
     )
   for (var in numeric_vars) {
     basic_table <- basic_table %>%
       DT::formatStyle(
         columns = var,
         background = DT::styleColorBar(table_data()[[var]],'#3fc380', 90),
         backgroundSize = '98% 88%',
         backgroundRepeat = 'no-repeat',
         backgroundPosition = 'center'
       )
   }
   return(basic_table)
  })

  # reactive to build the gt
  # build_table <- shiny::reactive({
  #   table_data() %>%
  #     dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
  #     gt::gt() %>%
  #     gt::tab_spanner(
  #       label = 'Climatic',
  #       columns = dplyr::vars(dplyr::starts_with('clim_'))
  #     ) %>%
  #     gt::tab_spanner(
  #       label = 'Topo',
  #       columns = dplyr::vars(dplyr::starts_with('topo_'))
  #     ) %>%
  #     gt::tab_spanner(
  #       label = 'Features',
  #       columns = dplyr::vars(dplyr::starts_with('feat_'))
  #     )
  # })

  # build_table <- shiny::eventReactive(
  #   ignoreInit = FALSE,
  #   eventExpr = apply_table$apply,
  #   valueExpr = {
  #     table_data() %>%
  #       dplyr::select(dplyr::one_of(input$col_vis_selector)) %>%
  #       gt::gt() %>%
  #       gt::tab_spanner(
  #         label = 'Climatic',
  #         columns = dplyr::vars(dplyr::starts_with('clim_'))
  #       ) %>%
  #       gt::tab_spanner(
  #         label = 'Topo',
  #         columns = dplyr::vars(dplyr::starts_with('topo_'))
  #       ) %>%
  #       gt::tab_spanner(
  #         label = 'Features',
  #         columns = dplyr::vars(dplyr::starts_with('feat_'))
  #       )
  #   }
  # )

  # table per se.
  # output$nfi_table <- gt::render_gt({
  output$nfi_table <- DT::renderDT({
    build_table()
  })

}