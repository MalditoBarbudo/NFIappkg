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
    shiny::div(
      id = 'tableTabHeader',
      shiny::fluidRow(
        shiny::column(
          3,
          # shiny::br(),
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
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        ),
        shiny::column(
          2, offset = 2, align = 'center',
          shiny::br(),
          # shiny::p('Data info'),
          shinyWidgets::actionBttn(
            ns('show_hri'),
            'Info',
            style = 'material-circle',
            icon = shiny::icon('info-circle'),
            color = 'primary', size = 'sm'
          ),
          shinyWidgets::actionBttn(
            ns('show_glossary'),
            'Glossary',
            style = 'material-circle',
            icon = shiny::icon('question-circle'),
            color = 'primary', size = 'sm'
          )
        ),
        shiny::column(
          2, offset = 3,
          shiny::br(),
          shiny::actionButton(
            ns('save_data_table'), icon = shiny::icon('download'), label = ''
          )


          # shiny::downloadButton(
          #   ns('dwl_csv_button'),
          #   'csv',
          #   class = 'success'
          # ),
          # shiny::downloadButton(
          #   ns('dwl_xlsx_button'),
          #   'xlsx',
          #   class = 'success'
          # )#,
          # shiny::downloadButton(
          #   ns('dwl_sql_query'),
          #   'SQL query',
          #   class = 'success'
          # )
        )
      )
    ),
    shiny::fluidRow(
      # shinyWidgets::addSpinner(DT::DTOutput(ns('nfi_table')), spin = 'dots')
      DT::DTOutput(ns('nfi_table'))
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
#' @param var_thes variables thesaurus df
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  data_inputs, map_inputs, nfidb, var_thes, texts_thes, numerical_thes, lang
) {

  # table data from map_inputs, but only updated when apply button is pressed
  table_data <- shiny::eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = data_inputs$apply_data,
    valueExpr = {
      viz_shape <- shiny::isolate({data_inputs$viz_shape})

      if (any(is.null(viz_shape), is.null(map_inputs$main_data))) {
        return()
      }

      if (viz_shape == 'plot') {
        if (is.null(map_inputs$main_data[['selected']])) {
          return()
        } else {
          # check for dominant
          if (data_inputs$dominant_group == 'none') {
            res <- map_inputs$main_data[['selected']]
          } else {
            res <- map_inputs$main_data[['selected']] %>%
              tidyNFI::nfi_results_summarise(
                polygon_group = 'none', functional_group = 'none',
                diameter_classes = FALSE,
                dominant_group = data_inputs$dominant_group,
                dominant_criteria = data_inputs$dominant_criteria,
                dominant_nfi = data_inputs$dominant_nfi,
                conn = nfidb
              )
          }

        }
      } else {
        if (is.null(map_inputs$main_data[['summarised']])) {
          return()
        } else {
          res <- map_inputs$main_data[['summarised']] %>%
            dplyr::ungroup()
        }
      }
      return(res)
    }
  )

  tables_to_look_at <- shiny::reactive({

    shiny::validate(
      shiny::need(data_inputs$nfi, 'No NFI version selected')
    )

    tables_to_look_at_helper(data_inputs)
  })

  # update the column visibility input
  shiny::observeEvent(
    eventExpr = table_data(),
    handlerExpr = {
      col_vis_choices <- names(table_data())

      if (data_inputs$viz_shape == 'polygon' | shiny::isolate(data_inputs$dominant_group) != 'none') {
        summ <- TRUE
      } else {
        summ <- FALSE
      }

      # selected_choices_sources
      admin_div_sel <- glue::glue("admin_{data_inputs$admin_div}")
      fg_sel <- glue::glue("{data_inputs$functional_group}_id")
      dom_nfi_sel <- switch(data_inputs$dominant_nfi, none = '', nfi2 = '_nfi2', nfi3 = '_nfi3', nfi4 = '_nfi4')
      dominant_sel <- glue::glue("{data_inputs$dominant_criteria}_{data_inputs$dominant_group}_dominant{dom_nfi_sel}")

      selected_choices <- col_vis_choices %>%
        magrittr::extract(. %in% c(
          # plot
          "plot_id",
          # admin
          admin_div_sel,
          # fg and dc id
          fg_sel, "diamclass_id",
          # dominant
          dominant_sel,
          # viz_color
          data_inputs$viz_color,
          glue::glue("{data_inputs$viz_color}{data_inputs$viz_statistic}")
        ))

      shinyWidgets::updatePickerInput(
        session = session, 'col_vis_selector',
        label = text_translate('col_vis_selector_input', lang(), texts_thes),
        choices = var_names_input_builder(
          col_vis_choices, lang(), var_thes, texts_thes, tables_to_look_at(), numerical_thes, summ
        ) %>%
          var_inputs_aggregator(lang(), texts_thes),
        selected = selected_choices
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

    if (shiny::isolate(data_inputs$viz_shape) == 'polygon' | shiny::isolate(data_inputs$dominant_group) != 'none') {
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
        colnames = names(var_names_input_builder(
          names(.), lang(), var_thes, texts_thes, tables_to_look_at(),
          numerical_thes, summ, ordered = FALSE
        )),
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        options = list(
          pageLength = 15,
          dom = 'tip',
          autoWidth = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
            "$(this.api().table().body()).css({'font-family': 'Hacker'});",
            "}"
          )
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

  # modal for saving the data
  shiny::observeEvent(
    eventExpr = input$save_data_table,
    handlerExpr = {

      ns <- session$ns

      shiny::showModal(
        ui = shiny::modalDialog(
          shiny::tagList(

            shiny::fluidRow(
              shiny::column(
                12,
                # format options
                shiny::radioButtons(
                  ns('data_format'), #'Data format',
                  text_translate('data_format', lang(), texts_thes),
                  choices = c('csv', 'xlsx'),
                  selected = 'csv'
                ),
                # length options
                shiny::radioButtons(
                  ns('data_length'), #'All data?',
                  text_translate('data_length', lang(), texts_thes),
                  choices = c('visible', 'all_columns') %>%
                    magrittr::set_names(c(
                      text_translate('visible', lang(), texts_thes),
                      text_translate('all_columns', lang(), texts_thes)
                    )),
                  selected = 'visible', width = '100%'
                )
              )
            )
          ),
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton(text_translate('dismiss', lang(), texts_thes)),
            shiny::downloadButton(
              ns('download_data_with_options'),
              label = text_translate('download', lang(), texts_thes),
              class = 'btn-success'
            )
          )
        )
      )
    }
  )


  # download handlers
  output$download_data_with_options <- shiny::downloadHandler(
    filename = function() {
      glue::glue("NFI_data.{input$data_format}")
    },
    content = function(file) {

      # data length
      if (input$data_length == 'visible') {
        data_res <- table_data() %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))
      } else {
        data_res <- table_data()
      }

      # data format
      if (input$data_format == 'csv') {
        readr::write_csv(data_res, file)
      } else {
        writexl::write_xlsx(data_res, file)
      }
    }
  )

  # output$dwl_csv_button <- shiny::downloadHandler(
  #   filename = function() {
  #     'NFI_data.csv'
  #   },
  #   content = function(file) {
  #     if (isTRUE(data_inputs$diameter_classes)) {
  #       shinyWidgets::sendSweetAlert(
  #         session, 'Note:',
  #         text = 'Saving the data broken down by diameter classes can take some time'
  #       )
  #     }
  #     readr::write_csv(table_data(), file)
  #   }
  # )
  #
  # output$dwl_xlsx_button <- shiny::downloadHandler(
  #   filename = function() {
  #     'NFI_data.xlsx'
  #   },
  #   content = function(file) {
  #     if (isTRUE(data_inputs$diameter_classes)) {
  #       shinyWidgets::sendSweetAlert(
  #         session, 'Note:',
  #         text = 'Saving the data broken down by diameter classes can take some time'
  #       )
  #     }
  #     writexl::write_xlsx(table_data(), file)
  #   }
  # )
  #
  # output$dwl_sql_query <- shiny::downloadHandler(
  #   filename = function() {
  #     'NFI_data_query.sql'
  #   },
  #   content = function(file) {
  #     if (data_inputs$viz_shape == 'plot') {
  #       query <- dbplyr::sql_render(map_inputs$main_data$selected)
  #     } else {
  #       query <- dbplyr::sql_render(map_inputs$main_data$summarised)
  #     }
  #     writeLines(query, con = file)
  #   }
  # )

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

  shiny::observeEvent(
    eventExpr = input$show_glossary,
    handlerExpr = {

      # ns <- session$ns
      shiny::showModal(
        # ui = mod_glossaryUI(ns('mod_glossaryUI'))
        ui = mod_glossaryUI('mod_glossaryUI')
      )
    }
  )
}