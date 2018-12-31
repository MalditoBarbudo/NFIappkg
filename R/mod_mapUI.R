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

  # output map
  output$map <- leaflet::renderLeaflet({
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
      )
      # raw easy print plugin (js bundle loaded in the nfi_app function)
      # htmlwidgets::onRender(
      #   "function(el, x) {
      #   L.easyPrint({
      #   hidden: true,
      #   sizeModes: ['A4Landscape'],
      #   }).addTo(this);
      #   }"
      # )
      # htmlwidgets::onRender(
      #   "function(el, x) {
      #   var myMap = this;
      #   var printPlugin = L.easyPrint({
      #   sizeModes: ['A4Portrait'],
      #   hidden: true
      #   }).addTo(myMap);
      #   Shiny.addCustomMessageHandler('easyPrint_button', function(message) {
      #     printPlugin.printMap('A4Portrait', message);
      #   });
      #   }"
      # )
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

      # browser()
      polygon_object <- switch(
        data_inputs$admin_div,
        'aut_community' = 'catalonia_polygons',
        'province' = 'provinces_polygons',
        'vegueria' = 'veguerias_polygons',
        'region' = 'regions_polygons',
        'municipality' = 'municipalities_polygons',
        'natural_interest_area' = 'natural_interest_area_polygons',
        'special_protection_natural_area' = 'special_protection_natural_area_polygons',
        'natura_network_2000' = 'natura_network_2000_polygons'
      )

      polygon_group <- switch(
        data_inputs$admin_div,
        'aut_community' = 'aut_communities',
        'province' = 'provinces',
        'vegueria' = 'veguerias',
        'region' = 'regions',
        'municipality' = 'municipalities',
        'natural_interest_area' = 'natural_interest_areas',
        'special_protection_natural_area' = 'special_protection_natural_areas',
        'natura_network_2000' = 'natura_network_2000s'
      )

      polygon_labels <- switch(
        data_inputs$admin_div,
        'aut_community' = '~admin_aut_community',
        'province' = '~admin_province',
        'vegueria' = '~admin_vegueria',
        'region' = '~admin_region',
        'municipality' = '~admin_municipality',
        'natural_interest_area' = '~admin_natural_interest_area',
        'special_protection_natural_area' = '~admin_special_protection_natural_area',
        'natura_network_2000' = '~admin_natura_network_2000'
      )

      leaflet::leafletProxy('map') %>%
        leaflet::clearGroup('veguerias') %>%
        leaflet::clearGroup('regions') %>%
        leaflet::clearGroup('municipalities') %>%
        leaflet::clearGroup('provinces') %>%
        leaflet::clearGroup('aut_communities') %>%
        leaflet::clearGroup('natural_interest_areas') %>%
        leaflet::clearGroup('special_protection_natural_areas') %>%
        leaflet::clearGroup('natura_network_2000s') %>%
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

  map_inputs <- shiny::reactiveValues()
  shiny::observe({
    map_inputs$map_draw_all_features <- input$map_draw_all_features
    map_inputs$map_center <- input$map_center
  })

  # capture the custom polygon (if any) to use it later
  custom_polygon <- shiny::eventReactive(
    # ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = map_inputs$map_draw_all_features,
    valueExpr = {

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
    }
  )

  shiny::observe({
    map_inputs$custom_polygon <- custom_polygon()
  })

  # returned data (NON COLLECTED!!!)
  returned_data_inputs <- shiny::callModule(
    mod_returnedData, 'mod_returnedDataOutput',
    data_inputs, map_inputs, nfidb
  )

  apply_reactives <- shiny::reactive({
    apply_reactives <- list()
    apply_reactives$apply_data <- data_inputs$apply_data
    apply_reactives$apply_viz <- data_inputs$apply_viz
  })

  map_data <- shiny::eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = apply_reactives(),
    valueExpr = {

      # First check if data is null (filters too restrictive returning no data)
      shiny::validate(
        shiny::need(
          returned_data_inputs$main_data[['selected']],
          "No data computed yet, no map data can be generated"
        )
      )

      # start the progress
      shinyWidgets::progressSweetAlert(
        session = session, id = 'map_data_progress',
        title = 'Map data carpentry', value = 25,
        display_pct = TRUE
      )

      # filter by functional group value
      if (data_inputs$functional_group != 'plot') {

        fil_var <- glue::glue("{data_inputs$functional_group}_id")
        fil_val <- data_inputs$viz_functional_group_value

        # when changing to another functional group from plot there is one run without
        # viz_functional_group_value, so we have to skip it
        if (fil_val == '') {
          return()
        } else {
          gf_filter_expr <- rlang::quo(!! rlang::sym(fil_var) == fil_val)
        }
      } else {
        gf_filter_expr <- rlang::quo()
      }

      # filter by diam class value
      if (isTRUE(data_inputs$diameter_classes)) {
        dc_filter_expr <- rlang::quo(diamclass_id == data_inputs$viz_diamclass)
      } else {
        dc_filter_expr <- rlang::quo()
      }

      # all the filter expressions
      filter_exprs <- rlang::quos(!!!gf_filter_expr, !!!dc_filter_expr) %>%
        magrittr::extract(!vapply(., rlang::quo_is_missing, logical(1)))

      # switches (polygons objects, labels and groups)
      join_var <- switch(
        data_inputs$admin_div,
        'aut_community' = 'admin_aut_community',
        'province' = 'admin_province',
        'vegueria' = 'admin_vegueria',
        'region' = 'admin_region',
        'municipality' = 'admin_municipality',
        'natural_interest_area' = 'admin_natural_interest_area',
        'special_protection_natural_area' = 'admin_special_protection_natural_area',
        'natura_network_2000' = 'admin_natura_network_2000'
      )

      polygon_object <- switch(
        data_inputs$admin_div,
        'aut_community' = 'catalonia_polygons',
        'province' = 'provinces_polygons',
        'vegueria' = 'veguerias_polygons',
        'region' = 'regions_polygons',
        'municipality' = 'municipalities_polygons',
        'natural_interest_area' = 'natural_interest_area_polygons',
        'special_protection_natural_area' = 'special_protection_natural_area_polygons',
        'natura_network_2000' = 'natura_network_2000_polygons'
      )

      shinyWidgets::updateProgressBar(
        session = session, id = 'map_data_progress',
        value = 50
      )

      # polygons shape
      if (data_inputs$viz_shape == 'polygon') {
        viz_color <- glue::glue("{data_inputs$viz_color}{data_inputs$viz_statistic}")

        map_data_pre <- returned_data_inputs$main_data[['summarised']] %>%
          dplyr::filter(
            !!! filter_exprs
          ) %>%
          dplyr::select(dplyr::one_of(
            join_var, viz_color, glue::glue("{data_inputs$functional_group}_id"),
            'diamclass_id'
          )) %>%
          dplyr::collect()

        shiny::validate(
          shiny::need(
            nrow(map_data_pre) != 0,
            "No data to map"
          )
        )

        shinyWidgets::updateProgressBar(
          session = session, id = 'map_data_progress',
          value = 75
        )

        map_data <- map_data_pre %>%
          dplyr::full_join(
            rlang::eval_tidy(rlang::sym(polygon_object)), by = join_var
          ) %>%
          sf::st_as_sf()

      } else {
        # plots shape
        # color and size vars
        viz_color <- glue::glue("{data_inputs$viz_color}")
        viz_size <- glue::glue("{data_inputs$viz_size}")

        map_data_pre <- returned_data_inputs$main_data[['selected']] %>%
          dplyr::filter(
            !!! filter_exprs
          ) %>%
          dplyr::select(dplyr::one_of(
            c('plot_id', 'coords_longitude', 'coords_latitude'), viz_color, viz_size
          )) %>%
          dplyr::collect()

        shiny::validate(
          shiny::need(
            nrow(map_data_pre) != 0,
            "No data to map"
          )
        )

        shinyWidgets::updateProgressBar(
          session = session, id = 'map_data_progress',
          value = 75
        )

        map_data <- map_data_pre %>%
          sf::st_as_sf(
            coords = c('coords_longitude', 'coords_latitude'),
            crs = '+proj=longlat +datum=WGS84'
          )
      }

      shinyWidgets::updateProgressBar(
        session = session, id = 'map_data_progress',
        value = 100
      )
      shinyWidgets::closeSweetAlert(session = session)
      return(map_data)
    }
  )

  # draw the new map
  # draw the new map
  shiny::observeEvent(
    eventExpr = map_data(),
    handlerExpr = {

      # browser()
      # map data
      map_data <- map_data()

      # First check if there is data
      shiny::validate(
        shiny::need(
          map_data,
          "No data to map"
        )
      )

      # start the progress
      # shinyWidgets::progressSweetAlert(
      #   session = session, id = 'map_build_progress',
      #   title = 'Building the map', value = 0,
      #   display_pct = TRUE
      # )

      # switches (polygons objects, labels and groups)
      polygon_group <- switch(
        data_inputs$admin_div,
        'aut_community' = 'aut_communities',
        'province' = 'provinces',
        'vegueria' = 'veguerias',
        'region' = 'regions',
        'municipality' = 'municipalities',
        'natural_interest_area' = 'natural_interest_areas',
        'special_protection_natural_area' = 'special_protection_natural_areas',
        'natura_network_2000' = 'natura_network_2000s'
      )

      polygon_labels <- switch(
        data_inputs$admin_div,
        'aut_community' = '~admin_aut_community',
        'province' = '~admin_province',
        'vegueria' = '~admin_vegueria',
        'region' = '~admin_region',
        'municipality' = '~admin_municipality',
        'natural_interest_area' = '~admin_natural_interest_area',
        'special_protection_natural_area' = '~admin_special_protection_natural_area',
        'natura_network_2000' = '~admin_natura_network_2000'
      )

      polygon_object <- switch(
        data_inputs$admin_div,
        'aut_community' = 'catalonia_polygons',
        'province' = 'provinces_polygons',
        'vegueria' = 'veguerias_polygons',
        'region' = 'regions_polygons',
        'municipality' = 'municipalities_polygons',
        'natural_interest_area' = 'natural_interest_area_polygons',
        'special_protection_natural_area' = 'special_protection_natural_area_polygons',
        'natura_network_2000' = 'natura_network_2000_polygons'
      )

      # shinyWidgets::updateProgressBar(
      #   session = session, id = 'map_build_progress',
      #   value = 10
      # )

      # polygons
      if (data_inputs$viz_shape == 'polygon') {

        viz_color <- glue::glue("{data_inputs$viz_color}{data_inputs$viz_statistic}")

        # check if there is color variable
        if (is.null(viz_color) || rlang::is_empty(viz_color)) {
          color_vector <- rep('no_color', nrow(map_data))
        } else {
          color_vector <- map_data %>%
            dplyr::pull(rlang::eval_tidy(rlang::sym(viz_color)))
        }

        if (is.numeric(color_vector)) {
          pal <- leaflet::colorNumeric(
            scales::gradient_n_pal(
              viridis::plasma(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
            ), color_vector, 9, reverse = data_inputs$viz_reverse_pal
          )
        } else {
          pal <- leaflet::colorFactor(
            'plasma', color_vector, reverse = data_inputs$viz_reverse_pal
          )
        }

        # shinyWidgets::updateProgressBar(
        #   session = session, id = 'map_build_progress',
        #   value = 30
        # )

        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('veguerias') %>%
          leaflet::clearGroup('regions') %>%
          leaflet::clearGroup('municipalities') %>%
          leaflet::clearGroup('provinces') %>%
          leaflet::clearGroup('aut_communities') %>%
          leaflet::clearGroup('natural_interest_areas') %>%
          leaflet::clearGroup('special_protection_natural_areas') %>%
          leaflet::clearGroup('natura_network_2000s') %>%
          leaflet::clearGroup('plots') %>%
          leaflet::addPolygons(
            data = map_data,
            group = polygon_group,
            label = as.formula(polygon_labels),
            layerId = as.formula(polygon_labels),
            weight = 1, smoothFactor = 1,
            opacity = 1.0, fill = TRUE,
            color = '#6C7A89FF', fillColor = pal(color_vector),
            fillOpacity = 1,
            highlightOptions = leaflet::highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE
            ),
            options = leaflet::pathOptions(
              pane = 'admin_divs'
            )
          ) %>%
          leaflet::addLegend(
            position = 'topright', pal = pal, values = color_vector, title = viz_color,
            layerId = 'color_legend', opacity = 1
          )

        # shinyWidgets::updateProgressBar(
        #   session = session, id = 'map_build_progress',
        #   value = 75
        # )

      } else {
        # plots

        # color and size vars
        viz_color <- glue::glue("{data_inputs$viz_color}")
        viz_size <- glue::glue("{data_inputs$viz_size}")

        # check if there is color variable
        if (is.null(viz_color) || rlang::is_empty(viz_color)) {
          color_vector <- rep('no_color', nrow(map_data))
        } else {
          color_vector <- map_data %>%
            dplyr::pull(rlang::eval_tidy(rlang::sym(viz_color)))
        }

        # build the color palette
        if (is.numeric(color_vector)) {
          pal <- leaflet::colorNumeric(
            scales::gradient_n_pal(
              viridis::plasma(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
            ), color_vector, 9, reverse = data_inputs$viz_reverse_pal
          )
        } else {
          pal <- leaflet::colorFactor(
            'plasma', color_vector, reverse = data_inputs$viz_reverse_pal
          )
        }

        # check if there is size variable
        if (is.null(viz_size) || rlang::is_empty(viz_size) || viz_size == '') {
          size_vector <- rep(750, nrow(map_data))
        } else {
          if (is.numeric(map_data[[viz_size]])) {
            size_vector <- ((map_data[[viz_size]] / max(map_data[[viz_size]], na.rm = TRUE)) * 1500) + 750
          } else {
            size_vector <- ((as.numeric(as.factor(map_data[[viz_size]])) /
                               max(as.numeric(as.factor(map_data[[viz_size]])), na.rm = TRUE)) * 1500) + 750
          }
        }

        # reduce the size of the nas
        size_vector[is.na(color_vector)] <- 500

        # shinyWidgets::updateProgressBar(
        #   session = session, id = 'map_build_progress',
        #   value = 30
        # )

        # build the map
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('veguerias') %>%
          leaflet::clearGroup('regions') %>%
          leaflet::clearGroup('municipalities') %>%
          leaflet::clearGroup('provinces') %>%
          leaflet::clearGroup('aut_communities') %>%
          leaflet::clearGroup('natural_interest_areas') %>%
          leaflet::clearGroup('special_protection_natural_areas') %>%
          leaflet::clearGroup('natura_network_2000s') %>%
          leaflet::clearGroup('plots') %>%
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
          ) %>%
          leaflet::addCircles(
            data = map_data,
            group = 'plots', label = ~plot_id, layerId = ~plot_id,
            stroke = FALSE, fillOpacity = 0.4, fillColor = pal(color_vector),
            radius = size_vector,
            options = leaflet::pathOptions(pane = 'plots')
          ) %>%
          leaflet::addLegend(
            position = 'topright', pal = pal, values = color_vector, title = viz_color,
            layerId = 'color_legend', opacity = 1
          )

        # shinyWidgets::updateProgressBar(
        #   session = session, id = 'map_build_progress',
        #   value = 75
        # )
      }

      # shinyWidgets::closeSweetAlert(session = session)
    }
  )

  # collect all the inputs and the data to return them
  shiny::observe({
    map_inputs$main_data <- returned_data_inputs$main_data
    map_inputs$map_data <- map_data()
    map_inputs$map_bounds <- input$map_bounds
    map_inputs$map_click <- input$map_click
    map_inputs$map_groups <- input$map_groups
    map_inputs$map_shape_click <- input$map_shape_click
    map_inputs$map_zoom <- input$map_zoom
    map_inputs$map_draw_new_feature <- input$map_draw_new_feature
    map_inputs$map_draw_start <- input$map_draw_start
    map_inputs$map_draw_stop <- input$map_draw_stop
  })

  return(map_inputs)
}