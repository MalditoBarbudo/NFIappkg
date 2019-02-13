#' function to launch the nfi app
#'
#' @importFrom magrittr %>%
#'
#' @export
nfi_app <- function(user = 'guest', password = 'guest') {

  ### DB access ################################################################
  nfidb <- tidyNFI::nfi_connect(user = user, password = password)

  ### Variables names inter ####################################################
  var_thes <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>% dplyr::collect()
  texts_thes <- dplyr::tbl(nfidb, 'TEXTS_THESAURUS') %>% dplyr::collect()
  numerical_thes <- dplyr::tbl(nfidb, 'VARIABLES_NUMERICAL') %>% dplyr::collect()

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'NFIappkg')
  )
  lang_choices <- c('spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  ## UI ####
  ui <- shiny::tagList(

    shinyjs::useShinyjs(),
    shinyWidgets::chooseSliderSkin(skin = "Shiny", color = '#0DB3D4'),
    shinyWidgets::useSweetAlert(),

    navbarPageWithInputs(
      # opts
      title = 'NFI app',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = c('spa', 'eng'),
        selected = 'eng',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2])
          )
        )
      ),

      # contents
      shiny::tabPanel(
        title = 'Map',

        shiny::div(
          class = "outer",
          shiny::tags$head(
            # custom css
            shiny::includeCSS(
              system.file('resources', 'nfi.css', package = 'NFIappkg')
            )
          ),

          ########################################################### debug ####
          # shiny::absolutePanel(
          #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
          #   draggable = TRUE, width = 640, height = 'auto',
          #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
          #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
          #   top = 60, left = 'auto', right = 50, bottom = 'auto',
          #
          #   shiny::textOutput('debug1'),
          #   shiny::textOutput('debug2'),
          #   shiny::textOutput('debug3')
          # ),
          ####################################################### end debug ####

          ## mod_data ####
          # mod_data module, it includes the dataSel, dataFil and dataAgg inputs
          mod_dataInput('mod_dataInput', nfidb),

          ## mod_map ####
          # mod_map, it includes the map
          mod_mapUI('mod_mapUI'),

          ## mod_info ####
          # mod_infoPanel, it includes the map events info panel
          shinyjs::hidden(
            shiny::absolutePanel(
              id = 'infoPanel', class = 'panel panel-default', fixed = TRUE,
              draggable = TRUE, width = 'auto', height = 'auto',
              top = 60, left = '5%', right = '5%', bottom = '2%',

              mod_infoUI('mod_infoUI')
            )
          ),

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
          class = 'inner',
          mod_tableOutput('mod_tableOutput')
        )
      )
    )
  )

  ## SERVER ####
  server <- function(input, output, session) {

    # lang reactive
    lang <- shiny::reactive({
      input$lang
    })

    ## module calling ####

    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput',
      nfidb, var_thes, texts_thes, numerical_thes, lang
    )

    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapUI',
      data_reactives, nfidb, var_thes, texts_thes, numerical_thes, lang
    )

    # info panel
    info_reactives <- shiny::callModule(
      mod_info, 'mod_infoUI',
      map_reactives, data_reactives, nfidb, var_thes, texts_thes, numerical_thes, lang
    )

    # saveMap panel
    shiny::callModule(
      mod_saveMap, 'mod_saveMapInput',
      map_reactives
    )

    # table
    shiny::callModule(
      mod_table, 'mod_tableOutput',
      data_reactives, map_reactives, nfidb, var_thes, texts_thes, numerical_thes, lang
    )

    shiny::observeEvent(
      eventExpr = map_reactives$map_shape_click,
      handlerExpr = {
        shinyjs::showElement('infoPanel')
      }
    )
    shiny::observeEvent(
      eventExpr = info_reactives$close,
      handlerExpr = {
        shinyjs::hideElement('infoPanel')
      }
    )

    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   data_reactives$diameter_classes
    # })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_click
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_shape_click
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