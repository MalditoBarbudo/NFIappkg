#' @title mod_applyButtonInput and mod_applyButton
#'
#' @description A shiny module to create and populate the buttons inputs
#'
#' @param id shiny id
#'
#' @export
mod_applyButtonInput <- function(id, lang, texts_thes) {

  # ns
  ns <- shiny::NS(id)

  # button
  shiny::fluidRow(
    shinyWidgets::actionBttn(
      ns('apply'),
      text_translate('apply', lang, texts_thes),
      icon = shiny::icon('check-circle'), style = 'stretch', block = FALSE, size = 'sm'
    )
  )

}

#' mod_applyButton
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#'
#' @rdname mod_applyButtonInput
mod_applyButton <- function(input, output, session) {

  mod_applyButton_reactives <- shiny::reactiveValues()

  shiny::observe({
    mod_applyButton_reactives$apply <- input$apply
  })

  return(mod_applyButton_reactives)
}