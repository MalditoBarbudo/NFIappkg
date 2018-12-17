#' @title mod_mapUI and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#' @param nfidb pool object to access the database
#'
#' @export
mod_mapUI <- function(id, nfidb) {

  # ns
  ns <- shiny::NS(id)

  shiny::tagList(
    # leaflet output
    leaflet::leafletOutput(ns('map'), height = '100%'),

    # mod returned data
    mod_returnedDataOutput('mod_returnedDataOutput')
  )



}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_inputs reactive with the reactive data and the data inputs
#' @param nfidb pool with database connection object
#'
#' @export
#'
#' @rdname mod_mapUI
mod_map <- function(
  input, output, session,
  data_inputs, nfidb
) {

  # basic map,setting the view, zoom and panes to manage the zIndex
  base_map <- shiny::reactive({
    leaflet::leaflet() %>%
      leaflet::setView(0.8, 41.67, zoom = 8) %>%
      leaflet::addMapPane('admin_divs', zIndex = 410) %>%
      leaflet::addMapPane('plots', zIndex = 420) %>%
      # leaflet.extras plugins
      leaflet.extras::addDrawToolbar(
        targetGroup = 'custom_polygon',
        position = 'topleft',
        polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
        markerOptions = FALSE, circleMarkerOptions = FALSE,
        polygonOptions = leaflet.extras::drawPolygonOptions(
          shapeOptions = leaflet.extras::drawShapeOptions()
        ),
        editOptions = leaflet.extras::editToolbarOptions(
          edit = TRUE, remove = TRUE
        ),
        singleFeature = TRUE
      ) %>%
      # raw easy print plugin (js bundle loaded in the nfi_app function)
      htmlwidgets::onRender(
        "function(el, x) {
        L.easyPrint({
        title: '',
        sizeModes: ['A4Landscape', 'A4Portrait'],
        filename: 'NFImap',
        exportOnly: true,
        hideControlContainer: false
        }).addTo(this);
        }"
      )
  })

  # output map
  output$map <- leaflet::renderLeaflet({
    base_map()
  })

  # observer for admin divs polygons. We use this instead of add polygons
  # directly in the map and control them with the default addLayersControl
  # because some ids are identical between polygons layers (i.e. Barcelona in
  # provincies and comarques) which causes some polygons to dissapear after
  # drawing. Also, in this way the app load is faster, but the polygon layer is
  # slower, though. So we control the polygons drawing with a classic
  # input-observer pair, as we do with the parceles circles.
  shiny::observeEvent(
    eventExpr = data_inputs$admin_div,
    handlerExpr = {

      polygon_object <- switch(
        data_inputs$admin_div,
        'aut_community' = 'catalonia_polygons',
        'province' = 'provinces_polygons',
        'vegueria' = 'veguerias_polygons',
        'region' = 'regions_polygons',
        'municipality' = 'municipalities_polygons'
      )

      polygon_group <- switch(
        data_inputs$admin_div,
        'aut_community' = 'aut_communities',
        'province' = 'provinces',
        'vegueria' = 'veguerias',
        'region' = 'regions',
        'municipality' = 'municipalities'
      )

      polygon_labels <- switch(
        data_inputs$admin_div,
        'aut_community' = '~admin_aut_community',
        'province' = '~admin_province',
        'vegueria' = '~admin_vegueria',
        'region' = '~admin_region',
        'municipality' = '~admin_municipality'
      )

      leaflet::leafletProxy('map') %>%
        leaflet::clearGroup('veguerias') %>%
        leaflet::clearGroup('regions') %>%
        leaflet::clearGroup('municipalities') %>%
        leaflet::clearGroup('provinces') %>%
        leaflet::addPolygons(
          data = rlang::eval_tidy(rlang::sym(polygon_object)),
          group = polygon_group,
          label = as.formula(polygon_labels),
          layerId = as.formula(polygon_labels),
          weight = 1, smoothFactor = 1,
          opacity = 1.0, fill = TRUE,
          color = '#6C7A89FF', fillColor = "#CF000F00",
          highlightOptions = leaflet::highlightOptions(
            color = "#CF000F", weight = 2,
            bringToFront = FALSE,
            fill = TRUE, fillColor = "#CF000F00"
          ),
          options = leaflet::pathOptions(
            pane = 'admin_divs'
          )
        )
    }
  )

  # capture the custom polygon (if any) to use it later
  custom_polygon <- shiny::reactive({

    # browser()
    # When removing the features (custom polygon) the input$map_draw_new_feature
    # is not cleared, so is always filtering the sites, even after removing. For
    # that we need to control when the removed feature equals the new, that's it,
    # when we removed the last one
    res <- input$map_draw_all_features
    if (is.null(res) || length(res[['features']]) == 0) {
      return(NULL)
    } else {
      polygon_res <- tidyNFI:::custom_poly_to_sf(
        res[['features']][[1]]
      )
      return(polygon_res)
    }
  })

  # returned data (NON COLLECTED!!!) ## NOT HERE
  returned_data_inputs <- shiny::callModule(
    mod_returnedData, 'mod_returnedDataOutput',
    data_inputs, custom_polygon(), nfidb
  )

  # map_modificated <- shiny::eventReactive(
  #   eventExpr = returned_data_inputs,
  #   valueExpr = {
  #
  #     viz_shape <- data_inputs$viz_shape
  #     admin_div <- glue::glue('admin_{data_inputs$admin_div}')
  #
  #     if (viz_shape == 'polygon') {
  #
  #       map_data <- returned_data_inputs[['summarised']] %>%
  #
  #
  #     }
  #
  #
  #   }
  # )


}