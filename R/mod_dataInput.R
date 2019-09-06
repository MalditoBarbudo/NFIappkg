#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi db
#'
#' @export
mod_dataInput <- function(id, nfidb) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::uiOutput(
      ns('mod_data_container')
    )
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb pool object to access the nfi db
#' @param var_thes variables thesaurus df
#' @param lang lang value
#'
#' @param cache_list inMemory cache
#'
#' @export
mod_data <- function(
  input, output, session,
  nfidb, var_thes, texts_thes, numerical_thes, lang,
  cache_list
) {

  # renderUI
  output$mod_data_container <- shiny::renderUI({

    ns <- session$ns

    ## preacalculated choices ####
    nfi_choices <- c(
      # base_data
      'nfi_2', 'nfi_3', 'nfi_4',
      # comparisions
      'nfi_2_nfi_3', 'nfi_3_nfi_4',
      # shrub
      'nfi_2_shrub', 'nfi_3_shrub', 'nfi_4_shrub',
      # regeneration
      'nfi_2_regen', 'nfi_3_regen', 'nfi_4_regen'
    ) %>%
      magrittr::set_names(c(
        text_translate('nfi_2', lang(), texts_thes),
        text_translate('nfi_3', lang(), texts_thes),
        text_translate('nfi_4', lang(), texts_thes),
        text_translate('nfi_2_nfi_3', lang(), texts_thes),
        text_translate('nfi_3_nfi_4', lang(), texts_thes),
        text_translate('nfi_2_shrub', lang(), texts_thes),
        text_translate('nfi_3_shrub', lang(), texts_thes),
        text_translate('nfi_4_shrub', lang(), texts_thes),
        text_translate('nfi_2_regen', lang(), texts_thes),
        text_translate('nfi_3_regen', lang(), texts_thes),
        text_translate('nfi_4_regen', lang(), texts_thes)
      ))

    viz_shape_choices <- c('polygon', 'plot') %>%
      magrittr::set_names(c(
        text_translate('polygon', lang(), texts_thes),
        text_translate('plot', lang(), texts_thes)
      ))

    admin_div_choices <- c(
      'aut_community', 'province', 'vegueria', 'region', 'municipality',
      'natural_interest_area', 'special_protection_natural_area', 'natura_network_2000',
      'file'
    ) %>%
      magrittr::set_names(c(
        text_translate('aut_community', lang(), texts_thes),
        text_translate('province', lang(), texts_thes),
        text_translate('vegueria', lang(), texts_thes),
        text_translate('region', lang(), texts_thes),
        text_translate('municipality', lang(), texts_thes),
        text_translate('natural_interest_area', lang(), texts_thes),
        text_translate('special_protection_natural_area', lang(), texts_thes),
        text_translate('natura_network_2000', lang(), texts_thes),
        text_translate('file', lang(), texts_thes)
      ))

    functional_group_choices <- c('plot', 'species', 'simpspecies', 'genus', 'dec', 'bc') %>%
      magrittr::set_names(c(
        text_translate('fg_plot', lang(), texts_thes),
        text_translate('fg_species', lang(), texts_thes),
        text_translate('fg_simpspecies', lang(), texts_thes),
        text_translate('fg_genus', lang(), texts_thes),
        text_translate('fg_dec', lang(), texts_thes),
        text_translate('fg_bc', lang(), texts_thes)
      ))

    dominant_group_choices <- c('none', 'species', 'simpspecies', 'genus', 'dec', 'bc') %>%
      magrittr::set_names(c(
        text_translate('none', lang(), texts_thes),
        text_translate('species', lang(), texts_thes),
        text_translate('simpspecies', lang(), texts_thes),
        text_translate('genus', lang(), texts_thes),
        text_translate('dec', lang(), texts_thes),
        text_translate('bc', lang(), texts_thes)
      ))

    dominant_criteria_choices <- c('density', 'basal_area') %>%
      magrittr::set_names(c(
        text_translate('dominant_criteria_density', lang(), texts_thes),
        text_translate('dominant_criteria_basal_area', lang(), texts_thes)
      ))

    dominant_nfi_choices <- c('none', 'nfi2', 'nfi3', 'nfi4') %>%
      magrittr::set_names(c(
        text_translate('none', lang(), texts_thes),
        text_translate('nfi2', lang(), texts_thes),
        text_translate('nfi3', lang(), texts_thes),
        text_translate('nfi4', lang(), texts_thes)
      ))

    # absolute panel for all, later on we will be able to hide/show the different
    # parts of the panel
    shiny::absolutePanel(
      # panel settings
      id = 'dataControls', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, width = 650, height = 'auto',
      # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
      # top = 'auto', left = 'auto', right = 100, bottom = 100,
      top = 60, right = 'auto', left = 50, bottom = 'auto',

      # panel contents
      # 1. data selection (div and id is for shinyjs later application)
      shiny::div(
        id = 'dataSel',
        shiny::fluidRow(
          shiny::h4(text_translate('h4_data_selection', lang(), texts_thes)),
          shiny::column(
            6, offset = 3,
            shinyWidgets::radioGroupButtons(
              ns('viz_shape'),
              choices = viz_shape_choices, selected = 'polygon',
              status = 'lfc_radiogroupbuttons',
              size = 'sm', justified = TRUE,
              checkIcon = list(
                yes = shiny::icon("check"),
                no = shiny::icon("times")
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns('nfi'),
              label = text_translate('data_version', lang(), texts_thes),
              choices = nfi_choices,
              selected = 'nfi_4'
            )
          ),
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns('admin_div'), text_translate('divisions', lang(), texts_thes),
              admin_div_choices, selected = 'region'
            )
          )
        ),
        shinyjs::hidden(
          shiny::div(
            id = ns('file_upload_panel'),
            shiny::fluidRow(
              shiny::column(
                7, align = 'center',
                shiny::fileInput(
                  ns('user_file_sel'),
                  text_translate('user_file_sel_label', lang(), texts_thes),
                  accept = c('zip', 'gpkg'),
                  buttonLabel = text_translate(
                    'user_file_sel_buttonLabel', lang(), texts_thes
                  ),
                  placeholder = text_translate(
                    'user_file_sel_placeholder', lang(), texts_thes
                  )
                )
              ),
              shiny::column(
                5, align = 'center',
                shiny::p(text_translate('file_text', lang(), texts_thes))
              )
            )
          )
        ),
        shiny::hr(),
        shiny::fluidRow(
          shiny::h4(text_translate('h4_additional_controls', lang(), texts_thes)),
          shiny::tabsetPanel(
            type = 'pills',
            # 2. data aggregation level (div and id is for shinyjs later
            #    application)
            shiny::tabPanel(
              title = text_translate('data_tab_1', lang(), texts_thes),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  8, offset = 2, align = 'center',
                  shinyWidgets::pickerInput(
                    ns('functional_group'),
                    text_translate('functional_group_input', lang(), texts_thes),
                    choices = functional_group_choices,
                    selected = 'plot', width = '100%'
                  ),
                  shiny::actionButton(
                    ns('advanced_fg_options'),# 'Show/Hide advanced options',
                    text_translate('advanced_fg_options', lang(), texts_thes),
                    class = 'btn-info'
                  )
                )
              ),
              shinyjs::hidden(
                shiny::div(
                  id = ns('advanced_fg_options_container'),
                  shiny::fluidRow(
                    shiny::column(
                      12, align = 'center',
                      shiny::br(),
                      shinyWidgets::awesomeCheckbox(
                        ns('diameter_classes'),
                        label = text_translate(
                          'diameter_classes_input', lang(), texts_thes
                        ),
                        status = 'info'
                      ),
                      shinyjs::hidden(
                        shiny::div(
                          id = ns('shrub_regen_warn'),
                          shiny::br(),
                          text_translate('shrub_regen_warn', lang(), texts_thes)
              #             'When shrub or regeneration tables are selected,
              # breakdown is fixed to species and diameter classes are inactive'
                        )
                      ),
                      # hidden message
                      shinyjs::hidden(
                        shiny::div(
                          id = ns('dominant_warn'),
                          shiny::br(),
                          text_translate('dominant_warn', lang(), texts_thes)
                          # 'When other than total by plot break down is selected,
                          # or diameter classes breakown is active,
                          # dominant functional group grouping is not available'
                        )
                      )
                    )
                  ),
                  shiny::fluidRow(
                    shiny::column(
                      5, offset = 1,
                      # inputs
                      shinyWidgets::awesomeRadio(
                        ns('dominant_group'), label = 'Dominant group to group by',
                        choices = dominant_group_choices,
                        selected = 'none',
                        status = 'info', checkbox = FALSE
                      )
                    ),
                    shiny::column(
                      5, offset = 1,
                      shinyWidgets::awesomeRadio(
                        ns('dominant_criteria'),
                        label = 'Dominant criteria to group by',
                        choices = dominant_criteria_choices,
                        selected = 'density',
                        status = 'info', checkbox = FALSE
                      ),
                      shinyWidgets::awesomeRadio(
                        ns('dominant_nfi'),
                        label = 'Dominant NFI version',
                        choices = dominant_nfi_choices,
                        selected = 'none',
                        status = 'info', checkbox = FALSE
                      )
                    )
                  )
                )
              )
            ),
            # 3. data filtering (this inputs are located in the mod_filter
            # module)
            shiny::tabPanel(
              title = text_translate('data_tab_2', lang(), texts_thes),
              shiny::br(),
              mod_filtersUI(ns('mod_filtersUI'), nfidb, lang(), texts_thes)
            ),
            # 4. Visualization controls (inputs in the mod_viz)
            shiny::tabPanel(
              title = text_translate('data_tab_3', lang(), texts_thes),
              shiny::column(
                12, class = 'center',
                shiny::br(),
                mod_vizInput(ns('mod_vizInput'), nfidb, lang(), texts_thes)
              )
            ),
            # 5. Save (here we call the ui function, but the server function
            # of the module is called on the parent level, in the nfi_app.R
            # file)
            shiny::tabPanel(
              text_translate('data_tab_4', lang(), texts_thes),
              shiny::column(
                12,
                shiny::br(),
                mod_saveMapInput('mod_saveMapInput', lang(), texts_thes)
              )
            )
          )
        ),
        # apply button
        shiny::hr(),
        mod_applyButtonInput(ns('mod_applyButtonInput_data_panel'), lang(), texts_thes)
      )
    )
  })

  # reactive values to return and use in other modules
  data_inputs <- shiny::reactiveValues()

  shiny::observe({
    data_inputs$nfi <- input$nfi
    data_inputs$viz_shape <- input$viz_shape
    data_inputs$admin_div <- input$admin_div
    # data_inputs$protected_areas <- input$protected_areas
    data_inputs$functional_group <- input$functional_group
    data_inputs$diameter_classes <- input$diameter_classes
  })

  # observer to show the file upload panel if needed
  shiny::observe({

    shiny::validate(
      shiny::need(input$admin_div, 'no div')
    )
    admin_div <- input$admin_div

    if (admin_div == 'file') {
      shinyjs::show('file_upload_panel')
    } else {
      shinyjs::hide('file_upload_panel')
    }
  })

  # observer to show the advanced options in the aggregation tab
  shiny::observeEvent(
    ignoreInit = TRUE,
    eventExpr = input$advanced_fg_options,
    handlerExpr = {
      shinyjs::toggle('advanced_fg_options_container')
    }
  )

  # observer to get the file uploaded
  shiny::observe({
    data_inputs$user_file_sel <- input$user_file_sel
  })

  # observer to disable the dominant grouping when other than plot is the
  # functional group, or diameter classes are selected, or other than nfi static
  # versions are selected
  shiny::observe({

    # validation
    shiny::validate(
      shiny::need(input$functional_group, 'no inputs yet'),
      shiny::need(!is.null(input$diameter_classes), 'no inputs yet')
    )

    # disabling and enabling
    fg <- input$functional_group
    dc <- input$diameter_classes
    nfi <- input$nfi


    # remove if plot
    if (fg != 'plot' |
        isTRUE(dc) |
        !(nfi %in% c('nfi_2', 'nfi_3', 'nfi_4', 'nfi_2_nfi_3', 'nfi_3_nfi_4'))) {
      shinyjs::reset('dominant_group')
      shinyjs::hide('dominant_group')
      shinyjs::hide('dominant_criteria')
      shinyjs::hide('dominant_nfi')
      shinyjs::showElement('dominant_warn')
    } else {
      shinyjs::show('dominant_group')
      shinyjs::show('dominant_criteria')
      shinyjs::show('dominant_nfi')
      shinyjs::hideElement('dominant_warn')

      if (nfi %in% c('nfi_2', 'nfi_3', 'nfi_4')) {
        shinyWidgets::updateAwesomeRadio(
          session = session,
          'dominant_nfi', label = 'Dominant NFI version',
          choices = 'none',
          selected = 'none',
          status = 'info', checkbox = FALSE
        )
        shinyjs::hide('dominant_nfi')
      } else {
        shinyjs::enable('dominant_nfi')
        shinyjs::show('dominant_nfi')

        if (nfi == 'nfi_2_nfi_3') {
          dominant_nfi_choices <- c('nfi2', 'nfi3') %>%
            magrittr::set_names(c(
              text_translate('nfi2', lang(), texts_thes),
              text_translate('nfi3', lang(), texts_thes)
            ))
        } else {
          dominant_nfi_choices <- c('nfi3', 'nfi4') %>%
            magrittr::set_names(c(
              text_translate('nfi3', lang(), texts_thes),
              text_translate('nfi4', lang(), texts_thes)
            ))
        }

        shinyWidgets::updateAwesomeRadio(
          session = session,
          'dominant_nfi', label = 'Dominant NFI version',
          choices = dominant_nfi_choices,
          selected = dominant_nfi_choices[1],
          status = 'info', checkbox = FALSE
        )

      }
    }
  })

  # observer to disable the breakdown and diamclass inputs when shrub or regeneration
  # tables are selected
  shiny::observe({

    # validation
    shiny::validate(
      shiny::need(input$nfi, 'no nfi selected')
    )

    # disabling and enabling
    nfi <- input$nfi
    if (stringr::str_detect(nfi, 'shrub|regen')) {
      # shinyjs::hide('functional_group')
      shinyWidgets::updatePickerInput(
        session, 'functional_group',
        text_translate('functional_group_input', lang(), texts_thes),
        choices = 'species' %>%
          magrittr::set_names(text_translate('fg_species', lang(), texts_thes)),
        selected = 'species'
      )
      shinyjs::hide('diameter_classes')
      shinyjs::showElement('shrub_regen_warn')
    } else {
      # shinyjs::show('functional_group')
      functional_group_choices <- c('plot', 'species', 'simpspecies', 'genus', 'dec', 'bc') %>%
        magrittr::set_names(c(
          text_translate('fg_plot', lang(), texts_thes),
          text_translate('fg_species', lang(), texts_thes),
          text_translate('fg_simpspecies', lang(), texts_thes),
          text_translate('fg_genus', lang(), texts_thes),
          text_translate('fg_dec', lang(), texts_thes),
          text_translate('fg_bc', lang(), texts_thes)
        ))

      shinyWidgets::updatePickerInput(
        session, 'functional_group',
        text_translate('functional_group_input', lang(), texts_thes),
        choices = functional_group_choices,
        selected = 'plot'
      )
      shinyjs::show('diameter_classes')
      shinyjs::hideElement('shrub_regen_warn')
    }
  })

  # updating the data inputs
  shiny::observe({
    data_inputs$dominant_group <- input$dominant_group
    data_inputs$dominant_criteria <- input$dominant_criteria
    data_inputs$dominant_nfi <- input$dominant_nfi
  })

  # calling the modules used
  # buttons_reactives <- shiny::callModule(
  #   mod_buttons, 'mod_buttonsInput'
  # )

  apply_data <- shiny::callModule(
    mod_applyButton, 'mod_applyButtonInput_data_panel'
  )

  viz_reactives <- shiny::callModule(
    mod_viz, 'mod_vizInput',
    data_inputs, nfidb, var_thes, texts_thes, numerical_thes, lang
  )

  filters_reactives <- shiny::callModule(
    mod_filters, 'mod_filtersUI',
    nfidb, data_inputs, var_thes, texts_thes, numerical_thes, lang
  )

  # observer to get the filter expressions and the buttons actions
  shiny::observe({

    # filters
    data_inputs$filter_expressions <- filters_reactives$filter_expressions
    data_inputs$filter_vars <- filters_reactives$filter_vars
    data_inputs$otf_filter_inputs <- filters_reactives$otf_filter_inputs
    # apply buttons
    data_inputs$apply_data <- apply_data$apply
    # viz
    data_inputs$viz_color <- viz_reactives$viz_color
    data_inputs$viz_reverse_pal <- viz_reactives$viz_reverse_pal
    data_inputs$viz_size <- viz_reactives$viz_size
    data_inputs$viz_statistic <- viz_reactives$viz_statistic
    data_inputs$viz_functional_group_value <- viz_reactives$viz_functional_group_value
    data_inputs$viz_diamclass <- viz_reactives$viz_diamclass
    data_inputs$viz_pal_config <- viz_reactives$viz_pal_config
  })

  return(data_inputs)
}