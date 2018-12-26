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

  ## preacalculated choices ####
  nfi_choices <- c(
    'NFI v2' = 'nfi_2',
    'NFI v3' = 'nfi_3',
    'NFI v4' = 'nfi_4',
    'NFI comp v2 - v3' = 'nfi_2_nfi_3',
    'NFI comp v3 - v4' = 'nfi_3_nfi_4'
  )

  viz_shape_choices <- c(
    'Polygons' = 'polygon',
    'Plots' = 'plot'
  )

  admin_div_choices <- c(
    'Catalonia' = 'aut_community',
    'Provinces' = 'province',
    'Veguerias' = 'vegueria',
    'Regions' = 'region',
    'Municipalities' = 'municipality',
    'Natural Interest Area' = 'natural_interest_area',
    'Special Protection Natural Area' = 'special_protection_natural_area',
    'Natura Network 2000' = 'natura_network_2000'
  )

  # protected_areas_choices <- c(
  #   'Natural Interest Area' = 'natural_interest_area',
  #   'Special Protection Natural Area' = 'special_protection_natural_area',
  #   'Natura Network 2000' = 'natura_network_2000'
  # )

  functional_group_choices <- c(
    'Total by plot' = 'plot',
    'Breakdown by Species' = 'species',
    'Breakdown by Simplified Species' = 'simpspecies',
    'Breakdown by Genus' = 'genus',
    'Breakdown by Decidious/Esclerophyl/Conifer' = 'dec',
    'Breakdown by Broadleaf/Conifer' = 'bc'
  )

  # UI ####
  shiny::tagList(

    # absolute panel for all, later on we will be able to hide/show the different
    # parts of the panel
    shiny::absolutePanel(
      # panel settings
      id = 'dataControls', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, width = 640, height = 'auto',
      # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
      # top = 'auto', left = 'auto', right = 100, bottom = 100,
      top = 60, right = 'auto', left = 50, bottom = 'auto',

      # panel contents
      # 1. data selection (div and id is for shinyjs later application)
      shiny::div(
        id = 'dataSel',

        shiny::h3('Data'),

        shiny::fluidRow(
          shiny::column(
            4,
            shinyWidgets::pickerInput(
              ns('nfi'),
              label = 'Data version',
              choices = nfi_choices,
              selected = 'NFI_4'
            )
          ),
          shiny::column(
            6, offset = 2,
            shinyWidgets::radioGroupButtons(
              ns('viz_shape'),
              'Visualization shape',
              choices = viz_shape_choices, selected = 'polygon',
              status = 'info', size = 'sm', justified = TRUE,
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
              ns('admin_div'), 'Administrative divisions & Protected areas',
              admin_div_choices, selected = 'region'
            )
          ),
          shiny::column(
            6#,
            # shinyWidgets::pickerInput(
            #   ns('protected_areas'), 'Protected areas',
            #   protected_areas_choices, selected = 'natural_interest_area'
            # )
          )
        ),

        # buttons module
        mod_buttonsInput(ns('mod_buttonsInput'))
      ),

      # 2. data aggregation level (div and id is for shinyjs later application)
      shinyjs::hidden(
        shiny::div(
          id = ns('data_aggregation'),

          # horizontal rule to separate
          shiny::hr(),

          shiny::h4('Breakdown level'),

          shiny::fluidRow(
            shiny::column(
              9,
              shinyWidgets::pickerInput(
                ns('functional_group'), 'Select the breakdown level',
                choices = functional_group_choices,
                selected = 'none', width = '100%'
              ),
              shinyWidgets::awesomeCheckbox(
                ns('diameter_classes'),
                label = 'Extra breakdown by diameter classes?',
                status = 'info'
              )
            )
          )
        )
      ),

      # 3. data filtering (div and id is for shinyjs later application)
      #   (this inputs are located in the mod_filter module
      shinyjs::hidden(
        shiny::div(
          id = ns('data_filters'),
          shiny::hr(),
          shiny::h4('Filter data'),
          mod_filtersUI(ns('mod_filtersUI'), nfidb)

        )
      ),

      # apply button
      shiny::hr(),
      mod_applyButtonInput(ns('mod_applyButtonInput_data_panel'))
    ), # absolute panel end

    ## vizControls ####
    shiny::div(
      id = ns('vizInputs'),
      shiny::absolutePanel(
        id = 'vizControls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, width = 380, height = 'auto',
        top = 400, right = 60, left = 'auto', bottom = 'auto',

        mod_vizInput(ns('mod_vizInput'), nfidb),

        mod_applyButtonInput(ns('mod_applyButtonInput_viz_panel'))
      )
    )
  ) # end of tagList
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb pool object to access the nfi db
#'
#' @export
mod_data <- function(
  input, output, session,
  nfidb
) {

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

  # calling the modules used
  buttons_reactives <- shiny::callModule(
    mod_buttons, 'mod_buttonsInput'
  )

  apply_data <- shiny::callModule(
    mod_applyButton, 'mod_applyButtonInput_data_panel'
  )

  viz_reactives <- shiny::callModule(
    mod_viz, 'mod_vizInput',
    data_inputs, nfidb
  )

  apply_viz <- shiny::callModule(
    mod_applyButton, 'mod_applyButtonInput_viz_panel'
  )

  filters_reactives <- shiny::callModule(
    mod_filters, 'mod_filtersUI',
    nfidb, data_inputs
  )

  # show/hide the panels
  shiny::observeEvent(
    eventExpr = buttons_reactives$show_filter_def,
    handlerExpr = {
      shinyjs::toggleElement(id = 'data_filters')
    }
  )

  shiny::observeEvent(
    eventExpr = buttons_reactives$show_agg,
    handlerExpr = {
      shinyjs::toggleElement(id = 'data_aggregation')
    }
  )

  shiny::observeEvent(
    eventExpr = buttons_reactives$show_viz,
    handlerExpr = {
      shinyjs::toggleElement(id = 'vizInputs')
    }
  )

  # observer to get the filter expressions and the buttons actions
  shiny::observe({
    # browser()
    # filters
    data_inputs$filter_expressions <- filters_reactives$filter_expressions
    data_inputs$filter_vars <- filters_reactives$filter_vars
    data_inputs$otf_filter_inputs <- filters_reactives$otf_filter_inputs
    # apply buttons
    data_inputs$apply_data <- apply_data$apply
    data_inputs$apply_viz <- apply_viz$apply
    # viz
    data_inputs$viz_color <- viz_reactives$viz_color
    data_inputs$viz_reverse_pal <- viz_reactives$viz_reverse_pal
    data_inputs$viz_size <- viz_reactives$viz_size
    data_inputs$viz_statistic <- viz_reactives$viz_statistic
    data_inputs$viz_functional_group_value <- viz_reactives$viz_functional_group_value
    data_inputs$viz_diamclass <- viz_reactives$viz_diamclass
    data_inputs$show_save <- buttons_reactives$show_save
  })

  return(data_inputs)
}