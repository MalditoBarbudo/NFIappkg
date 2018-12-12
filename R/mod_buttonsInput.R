#' @title mod_buttonsInput and mod_buttons
#'
#' @description A shiny module to create and populate the buttons inputs
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi db
#'
#' @export
mod_buttonsInput <- function(id, nfidb) {

  # ns
  ns <- shiny::NS(id)

  # Buttons
  shiny::fluidRow(
    shiny::column(
      12,
      # title
      shiny::tags$strong('Show/Hide additional controls'),
      # buttons
      shinyWidgets::actionGroupButtons(
        inputIds = c(
          ns('show_agg'), ns('show_filter_def'), ns('show_viz')
        ),
        labels = c(
          'Breakdown level',
          'Filters',
          'Visualization Controls'
        ),
        status = 'success', size = 'sm',
        direction = 'horizontal', fullwidth = TRUE
      )
      # ... other buttons, to develop
    )
  )
}

#' mod_buttons server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#'
#' @rdname mod_buttonsInput
mod_buttons <- function(
  input, output, session
) {

  # reactive values from buttons to return
  mod_buttons_reactives <- shiny::reactiveValues()

  shiny::observe({
    mod_buttons_reactives$show_filter_def <- input$show_filter_def
    mod_buttons_reactives$show_agg <- input$show_agg
    mod_buttons_reactives$show_viz <- input$show_viz
  })

  return(mod_buttons_reactives)
}