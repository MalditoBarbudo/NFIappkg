#' function to launch the nfi app
#'
#' @importFrom magrittr %>%
#'
#' @export
nfi_app <- function() {

  ### DB access ################################################################
  nfidb <- tidyNFI::nfi_connect()

  ## UI ####
  ui <- shiny::tagList(

    shinyjs::useShinyjs(),
    shinyWidgets::chooseSliderSkin(skin = "Flat", color = '#0DB3D4'),

    shiny::navbarPage(
      # opts
      title = 'NFI app',
      id = 'nav',
      collapsible = TRUE,

      # contents
      shiny::tabPanel(
        title = 'Map',

        shiny::div(
          class = "outer",
          shiny::tags$head(
            # custom css
            shiny::includeCSS(
              system.file('resources', 'nfi.css', package = 'NFIappkg')
            ),
            # custom scripts
            ## easyPrint leaflet plugin
            shiny::tags$script(
              src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"
            )
          ),

          ########################################################### debug ####
          shiny::absolutePanel(
            id = 'debug', class = 'panel panel-default', fixed = TRUE,
            draggable = TRUE, width = 640, height = 'auto',
            # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
            # top = 'auto', left = 'auto', right = 100, bottom = 100,
            top = 60, left = 'auto', right = 50, bottom = 'auto',

            shiny::tableOutput('debug1'),
            shiny::textOutput('debug2'),
            shiny::textOutput('debug3')
          ),
          ####################################################### end debug ####

          ## mod_data ####
          # mod_data module, it includes the dataSel, dataFil and dataAgg inputs
          mod_dataInput('mod_dataInput', nfidb),

          # just a call to the returned_data module
          mod_returnedDataOutput('mod_returnedDataOutput'),

          ## mod_map ####
          # mod_map, it includes the map
          # mod_mapUI('mod_mapUI'),

          ## mod_infoPanel ####
          # mod_infoPanel, it includes the map events info panel
          # mod_infopanelUI('mod_infopanelUI'),

          ## cite div ####
          shiny::tags$div(
            id = 'cite',
            glue::glue(
              'Data prepared by the CTFC and CREAF based on the raw NFI data served ',
              'by the MAPA (Spanish government)'
            )
          )
        )
      ),

      # data tab
      shiny::tabPanel(
        'Data',

        shiny::div(
          class = 'inner'#,
          # mod_tableOutput('mod_tableOutput', nfidb)
        )
      ),

      # Alometrias tab
      shiny::tabPanel(
        'Allometries'
      )
    )
  )

  ## SERVER ####
  server <- function(input, output, session) {

    ## module calling ####

    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput',
      nfidb
    )

    # returned data (NON COLLECTED!!!)
    returned_data_reactives <- shiny::callModule(
      mod_returnedData, 'mod_returnedDataOutput',
      data_reactives, nfidb
    )

    # map
    # map_reactives <- shiny::callModule(
    #   mod_map, 'mod_mapUI',
    #   data_reactives, data_reactives$advancedFilters_reactives, nfidb
    # )

    # info panel
    # shiny::callModule(
    #   mod_infopanel, 'mod_infopanelUI',
    #   data_reactives, map_reactives, data_reactives$advancedFilters_reactives, nfidb
    # )

    # table
    # shiny::callModule(
    #   mod_table, 'mod_tableOutput',
    #   data_reactives, data_reactives$advancedFilters_reactives, map_reactives, nfidb
    # )

    ## debug #####
    output$debug1 <- shiny::renderTable({
      foo <- returned_data_reactives$main_data
      foo[['selected']] %>% dplyr::collect()
    })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_draw_all_features
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_draw_new_feature
    # })
  }

  # Run the application
  nfi_app_res <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      shiny::onStop(function() {
        pool::poolClose(nfidb)
      })
    }
  )

  # shiny::runApp(nfi_app)
  return(nfi_app_res)

}