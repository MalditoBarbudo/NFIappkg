#' @title mod_saveMapInput and mod_saveMap
#'
#' @description A shiny module to create and populate the buttons inputs
#'
#' @param id shiny id
#'
#' @export
mod_saveMapInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # Buttons
  shiny::fluidRow(
    shiny::column(
      12,
      # title
      shiny::tags$strong('Save the map'),
      shiny::br(),
      # buttons
      shinyWidgets::downloadBttn(
        ns('save_shp'), label = 'Save as shapefile',
        style = 'material-flat', color = 'success', size = 'sm', block = TRUE
      ),
      shiny::br(),
      shinyWidgets::downloadBttn(
        ns('save_wkt'), label = 'Save as wkt',
        style = 'material-flat', color = 'success', size = 'sm', block = TRUE
      )
    )
  )
}

#' mod_saveMap server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param map_inputs Reactive with the map reactives, including map_data
#'
#' @export
#'
#' @rdname mod_saveMapInput
mod_saveMap <- function(
  input, output, session,
  map_inputs
) {

  # shapefile
  output$save_shp <- shiny::downloadHandler(
    filename = function() {
      glue::glue("nfi_map_{Sys.Date()}.zip")
    },
    content = function(file) {
      # a shp are several files, which downloadHandler can not handle, so we
      # zip them all and return the zip
      tmp_dir <- tempdir()
      sf::st_write(
        map_inputs$map_data,
        file.path(tmp_dir, glue::glue("nfi_map_{Sys.Date()}.shp")),
        layer = glue::glue("nfi_map_{Sys.Date()}"),
        delete_layer = TRUE
      )
      shp_files <- list.files(tmp_dir, 'nfi_map_', full.names = TRUE)
      utils::zip(
        file.path(tmp_dir, 'shp_files.zip'),
        shp_files
      )
      file.copy(file.path(tmp_dir, 'shp_files.zip'), file)
      file.remove(file.path(tmp_dir, 'shp_files.zip'), shp_files)
    }
  )

  # wkt
  output$save_wkt <- shiny::downloadHandler(
    filename = function() {
      glue::glue("nfi_map_{Sys.Date()}.csv")
    },
    content = function(file) {
      sf::write_sf(
        map_inputs$map_data, file, delete_layer = TRUE,
        layer_options = "GEOMETRY=AS_WKT"
      )
    }
  )

  # png
  # here we have to work with the easyPrint leaflet plugin
  # shiny::observeEvent(
  #   eventExpr = input$save_png,
  #   handlerExpr = {
  #     session$sendCustomMessage(
  #       'easyPrint_button', glue::glue("nfi_map_{Sys.Date()}")
  #     )
  #   }
  # )
}