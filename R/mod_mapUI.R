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

  # returned data (NON COLLECTED!!!)
  returned_data_inputs <- shiny::callModule(
    mod_returnedData, 'mod_returnedDataOutput',
    data_inputs, custom_polygon(), nfidb
  )

  apply_reactives <- shiny::reactive({
    apply_reactives <- list()
    apply_reactives$apply_data <- data_inputs$apply_data
    apply_reactives$apply_viz <- data_inputs$apply_viz
  })

  shiny::observeEvent(
    ignoreNULL = FALSE, ignoreInit = TRUE,
    eventExpr = apply_reactives(),
    handlerExpr = {

      # browser()

      # common things to both, polygons and plots

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

      # polygons
      if (data_inputs$viz_shape == 'polygon') {

        polygon_object <- switch(
          data_inputs$admin_div,
          'aut_community' = 'catalonia_polygons',
          'province' = 'provinces_polygons',
          'vegueria' = 'veguerias_polygons',
          'region' = 'regions_polygons',
          'municipality' = 'municipalities_polygons'
        )

        join_var <- switch(
          data_inputs$admin_div,
          'aut_community' = 'admin_aut_community',
          'province' = 'admin_province',
          'vegueria' = 'admin_vegueria',
          'region' = 'admin_region',
          'municipality' = 'admin_municipality'
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

        viz_color <- glue::glue("{data_inputs$viz_color}{data_inputs$viz_statistic}")
        # viz_size <- glue::glue("{data_inputs$viz_size}_{data_inputs$viz_statistic}")

        # # filter by functional group value
        # if (data_inputs$functional_group != 'plot') {
        #
        #   fil_var <- glue::glue("{data_inputs$functional_group}_id")
        #   fil_val <- data_inputs$viz_functional_group_value
        #
        #   # when changing to another functional group from plot there is one run without
        #   # viz_functional_group_value, so we have to skip it
        #   if (fil_val == '') {
        #     return()
        #   } else {
        #     gf_filter_expr <- rlang::quo(!! rlang::sym(fil_var) == fil_val)
        #   }
        # } else {
        #   gf_filter_expr <- rlang::quo()
        # }

        # # filter by diam class value
        # if (isTRUE(data_inputs$diameter_classes)) {
        #   dc_filter_expr <- rlang::quo(diamclass_id == data_inputs$viz_diamclass)
        # } else {
        #   dc_filter_expr <- rlang::quo()
        # }

        # filter_exprs <- rlang::quos(!!!gf_filter_expr, !!!dc_filter_expr) %>%
        #   magrittr::extract(!vapply(., rlang::quo_is_missing, logical(1)))

        map_data_pre <- returned_data_inputs$main_data[['summarised']] %>%
          dplyr::filter(
            !!! filter_exprs
          ) %>%
          dplyr::select(dplyr::one_of(
            join_var, viz_color
          )) %>%
          dplyr::collect()

        if (nrow(map_data_pre) == 0) {
          ## TODO a warning
          return()
        }

        map_data <- map_data_pre %>%
          dplyr::full_join(
            rlang::eval_tidy(rlang::sym(polygon_object)), by = join_var
          ) %>%
          sf::st_as_sf()

        # check if there is color variable
        if (is.null(viz_color) || rlang::is_empty(viz_color)) {
          color_vector <- rep('no_color', nrow(map_data))
        } else {
          color_vector <- map_data %>%
            dplyr::pull(rlang::eval_tidy(rlang::sym(viz_color)))
        }

        if (is.numeric(color_vector)) {
          pal <- leaflet::colorBin(
            'plasma', color_vector, 9, reverse = data_inputs$viz_reverse_pal
          )
        } else {
          pal <- leaflet::colorFactor(
            'plasma', color_vector, reverse = data_inputs$viz_reverse_pal
          )
        }

        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('veguerias') %>%
          leaflet::clearGroup('regions') %>%
          leaflet::clearGroup('municipalities') %>%
          leaflet::clearGroup('provinces') %>%
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
      } else {
        # plots

        # switches
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

        if (nrow(map_data_pre) == 0) {
          ## TODO a warning
          return()
        }

        map_data <- map_data_pre %>%
          sf::st_as_sf(
            coords = c('coords_longitude', 'coords_latitude'),
            crs = '+proj=longlat +datum=WGS84'
          )

        # check if there is color variable
        if (is.null(viz_color) || rlang::is_empty(viz_color)) {
          color_vector <- rep('no_color', nrow(map_data))
        } else {
          color_vector <- map_data %>%
            dplyr::pull(rlang::eval_tidy(rlang::sym(viz_color)))
        }

        # build the color palette
        if (is.numeric(color_vector)) {
          pal <- leaflet::colorBin(
            'plasma', color_vector, 9, reverse = data_inputs$viz_reverse_pal
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
            size_vector <- (map_data[[viz_size]] / max(map_data[[viz_size]], na.rm = TRUE)) * 3000
          } else {
            size_vector <- (as.numeric(as.factor(map_data[[viz_size]])) /
              max(as.numeric(as.factor(map_data[[viz_size]])), na.rm = TRUE)) * 3000
          }
        }

        # reduce the size of the nas
        size_vector[is.na(color_vector)] <- 250

        # build the map
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('veguerias') %>%
          leaflet::clearGroup('regions') %>%
          leaflet::clearGroup('municipalities') %>%
          leaflet::clearGroup('provinces') %>%
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

      }
    }
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