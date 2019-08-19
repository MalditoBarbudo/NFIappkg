#' function to launch the nfi app
#'
#' @importFrom magrittr %>%
#'
#' @export
nfi_app <- function(user = 'guest', password = 'guest', host = NULL, port = NULL, dbname = NULL) {

  ### DB access ################################################################
  nfidb <- tidyNFI::nfi_connect(user = user, password = password, host = host, port = port, dbname = dbname)

  ### Variables names inter ####################################################
  # var_thes <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>% dplyr::collect()
  # texts_thes <- dplyr::tbl(nfidb, tolower('TEXTS_THESAURUS')) %>% dplyr::collect()
  # numerical_thes <- dplyr::tbl(nfidb, 'VARIABLES_NUMERICAL') %>% dplyr::collect()

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'NFIappkg')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
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
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # contents
      shiny::tabPanel(
        title = 'Map',

        shiny::div(
          class = "mapouter",
          shiny::tags$head(
            # custom css
            shiny::includeCSS(
              system.file('resources', 'nfi.css', package = 'NFIappkg')
            ),
            # corporative image css
            shiny::includeCSS(
              system.file('resources', 'corp_image.css', package = 'NFIappkg')
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
          mod_mapUI('mod_mapUI')

          ## mod_info ####
          # mod info now is a modal, and is launched in the server section
        )
      ),

      ## data tab ####
      shiny::tabPanel(
        'Data',

        shiny::div(
          class = 'tableinner',
          mod_tableOutput('mod_tableOutput')
        )
      )
    )
  )

  ## SERVER ####
  server <- function(input, output, session) {

    ## Cache ####
    cache_list <- shiny::memoryCache(
      max_size = 2000 * 1024^2,
      evict = 'fifo'
    )

    cache_list$set("datacached", NULL)
    cache_list$set("nficached", NULL)
    cache_list$set("admindivcached", NULL)
    cache_list$set("functionalgroupcached", NULL)
    cache_list$set("diameterclassescached", NULL)
    cache_list$set("filtervarscached", NULL)
    cache_list$set("filterexpressionscached", NULL)
    cache_list$set("custompolygoncached", NULL)
    cache_list$set("dominantgroupcached", NULL)
    cache_list$set("dominantcriteriacached", NULL)
    cache_list$set("dominantnficached", NULL)

    # lang reactive
    lang <- shiny::reactive({
      input$lang
    })

    ## module calling ####

    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput',
      nfidb, var_thes, texts_thes, numerical_thes, lang,
      cache_list
    )

    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapUI',
      data_reactives, nfidb, var_thes, texts_thes, numerical_thes, lang,
      cache_list
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

    # glossary
    shiny::callModule(
      mod_glossary, 'mod_glossaryUI',
      nfidb, var_thes, numerical_thes, texts_thes, lang, data_reactives
    )

    shiny::observeEvent(
      eventExpr = map_reactives$map_shape_click,
      handlerExpr = {
        shiny::showModal(
          shiny::modalDialog(
            mod_infoUI('mod_infoUI'),
            footer = shiny::modalButton(
              text_translate('dismiss', lang(), texts_thes)
            ),
            size = 'l', easyClose = TRUE
          )
        )
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