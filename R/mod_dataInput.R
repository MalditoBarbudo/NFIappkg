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
    'NFI v2' = 'NFI_2',
    'NFI v3' = 'NFI_3',
    'NFI v4' = 'NFI_4',
    'NFI comp v2 - v3' = 'COMP_NFI2_NFI3',
    'NFI comp v3 - v4' = 'COMP_NFI3_NFI4'
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
    'Municipalities' = 'municipality'
  )

  protected_areas_choices <- c(
    'Natural Interest Area' = 'natural_interest_area',
    'Special Protection Natural Area' = 'special_protection_natural_area',
    'Natura Network 2000' = 'natura_network_2000'
  )

  functional_group_choices <- c(
    'Total by plot' = 'PLOT',
    'Breakdown by Species' = 'SPECIES',
    'Breakdown by Simplified Species' = 'SIMPSPECIES',
    'Breakdown by Genus' = 'GENUS',
    'Breakdown by Decidious/Esclerophyl/Conifer' = 'DEC',
    'Breakdown by Broadleaf/Conifer' = 'BC'
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
              ns('admin_div'), 'Administrative divisions',
              admin_div_choices, selected = 'region'
            )
          ),
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns('protected_areas'), 'Protected areas',
              protected_areas_choices, selected = 'natural_interest_area'
            )
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
    )#, # absolute panel end

    ## vizControls ####
    # shiny::div(
    #   id = ns('vizInputs'),
    #   mod_vizInput(ns('mod_vizInput'), nfidb)
    # )
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
  data_reactives <- shiny::reactiveValues()

  shiny::observe({
    data_reactives$nfi <- input$nfi
    data_reactives$viz_shape <- input$viz_shape
    data_reactives$admin_div <- input$admin_div
    data_reactives$protected_areas <- input$protected_areas
    data_reactives$functional_group <- input$functional_group
    data_reactives$diameter_classes <- input$diameter_classes
  })

  # calling the modules used
  buttons_reactives <- shiny::callModule(
    mod_buttons, 'mod_buttonsInput'
  )

  apply_data <- shiny::callModule(
    mod_applyButton, 'mod_applyButtonInput_data_panel'
  )

  # apply_viz <- shiny::callModule(
  #   mod_applyButton, 'mod_applyButtonInput_viz_panel'
  # )
  apply_viz <- apply_data
  ## TODO change this when mod viz is done

  filters_reactives <- shiny::callModule(
    mod_filters, 'mod_filtersUI',
    nfidb, data_reactives, apply_data, apply_viz
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

    }
  )

  shiny::observe({
    data_reactives$filter_expressions <- filters_reactives
  })


}