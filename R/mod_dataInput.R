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

  agg_level_choices <- c(
    'Total by plot' = 'plot',
    'Breakdown by Species' = 'species',
    'Breakdown by Simplified Species' = 'simpspecies',
    'Breakdown by Genus' = 'genus',
    'Breakdown by Decidious/Esclerophyl/Conifer' = 'dec',
    'Breakdown by Broadleaf/Conifer' = 'bc'
  )

  admin_div_fil_choices <- dplyr::tbl(nfidb, 'VARIABLES_CATEGORICAL') %>%
    dplyr::filter(var_id == 'admin_region') %>%
    dplyr::pull(var_values)

  protected_areas_fil_choices <- dplyr::tbl(nfidb, 'VARIABLES_CATEGORICAL') %>%
    dplyr::filter(var_id == 'admin_natural_interest_area') %>%
    dplyr::pull(var_values)

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
        mod_buttonsInput(ns('mod_buttonsInput'), nfidb)
      ),

      # 2. data aggregation level (div and id is for shinyjs later application)
      shinyjs::hidden(
        shiny::div(
          id = ns('dataAgg'),

          # horizontal rule to separate
          shiny::hr(),

          shiny::h4('Breakdown level'),

          shiny::fluidRow(
            shiny::column(
              9,
              shinyWidgets::pickerInput(
                ns('agg_level'), 'Select the breakdown level',
                choices = agg_level_choices,
                selected = 'plot', width = '100%'
              ),
              shinyWidgets::awesomeCheckbox(
                ns('diameter_classes'),
                label = 'Extra breakdown by diameter classes?',
                status = 'info'
              )
            )
          )
        )
      )#,

      # 3. data filtering (div and id is for shinyjs later application)
      #   (this inputs are created empty and filled later on in the server based
      #   on the section 1. inputs)
      # shinyjs::hidden(
      #   shiny::div(
      #     id = ns('dataFil'),
      #
      #     # horizontal rule to separate
      #     shiny::hr(),
      #
      #     shiny::h4(label_getter(nfidb, 'esp', 'dataFil_h4_label')),
      #
      #     shiny::fluidRow(
      #       shiny::column(
      #         6,
      #         shinyWidgets::pickerInput(
      #           ns('admin_div_fil'),
      #           label_getter(nfidb, 'esp', 'admin_div_fil_label', 'comarca'),
      #           choices = admin_div_fil_choices,
      #           selected = '', multiple = TRUE, width = '100%',
      #           options = list(
      #             `actions-box` = TRUE,
      #             `deselect-all-text` = 'None selected...',
      #             `select-all-text` = 'All selected',
      #             `selected-text-format` = 'count',
      #             `count-selected-text` = "{0} divisions selected (of {1})"
      #           )
      #         )
      #       ),
      #       shiny::column(
      #         6,
      #         shinyWidgets::pickerInput(
      #           ns('protected_areas_fil'),
      #           label_getter(nfidb, 'esp', 'protected_areas_fil_label', 'proteccio'),
      #           choices = protected_areas_fil_choices,
      #           selected = '', multiple = TRUE, width = '100%',
      #           options = list(
      #             `actions-box` = TRUE,
      #             `deselect-all-text` = 'None selected...',
      #             `select-all-text` = 'All selected',
      #             `selected-text-format` = 'count',
      #             `count-selected-text` = "{0} divisions selected (of {1})"
      #           )
      #         )
      #       )
      #     ),
      #
      #     # hidden div for advanced filters
      #     mod_advancedFiltersUI(ns('mod_advancedFiltersUI'), nfidb),
      #     # shinyjs::hidden(
      #     #   shiny::div(
      #     #     id = ns('advancedFiltersControls'),
      #     #     mod_advancedFiltersUI(ns('mod_advancedFiltersUI'))
      #     #   )
      #     # ),
      #
      #     shiny::fluidRow(
      #       shiny::column(
      #         6, offset = 3,
      #         shinyWidgets::actionBttn(
      #           ns('apply_filters'),
      #           label_getter(nfidb, 'esp', 'apply_filters_label'),
      #           icon = shiny::icon('check'),
      #           style = "stretch",
      #           block = TRUE,
      #           size = 'sm'
      #         )
      #       )
      #     )
      #   )
      # )
    )#, # absolute panel end

    ## vizControls ####
    # shiny::div(
    #   id = ns('vizInputs'),
    #   mod_vizInput(ns('mod_vizInput'), nfidb)
    # )
  ) # end of tagList
}